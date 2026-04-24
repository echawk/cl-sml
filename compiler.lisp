(in-package #:cl-sml)

(defun target-sml-package-name ()
  (package-name (ensure-sml-package *sml-package*)))

(defun exception-constructor-type-p (type)
  (or (and (stringp type)
           (string= type "exn"))
      (and (consp type)
           (eq (car type) :fn)
           (equal (third type) "exn"))))

(defun payload-exception-type-p (type)
  (and (consp type)
       (eq (car type) :fn)
       (equal (third type) "exn")))

(defun exception-constructor-info (name &optional local-exceptions)
  (let ((local-entry (assoc name local-exceptions :test #'string=)))
    (cond
      (local-entry
       (list :payloadp (cdr local-entry)))
      (t
       (let ((type (lookup-sml-binding-type name *sml-package*)))
         (when (exception-constructor-type-p type)
           (list :payloadp (payload-exception-type-p type))))))))

(defun compile-type-registration-form (symbol type)
  `(register-sml-binding-type ',symbol ',type))

(defun compile-type-declaim-form (symbol type)
  (let ((cl-type (sml-type->cl-type type)))
    (unless (or (eq cl-type t) (eq cl-type 'function))
      `(declaim (type ,cl-type ,symbol)))))

(defun compile-export-form (symbols)
  (when symbols
    `(export ',symbols ,(target-sml-package-name))))

(defun record-fields-sorted-by-label (fields)
  (sort (copy-list fields) #'string< :key #'first))

(defun pattern-type-bindings (pat type)
  (cond
    ((or (numberp pat) (stringp pat) (characterp pat) (eq pat :wild))
     nil)
    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     (list (cons (sml-symbol (second pat)) type)))
    ((and (listp pat) (member (car pat) '(:pat-ctor :ctor :pat-unit :pat-nil)))
     nil)
    ((and (listp pat) (eq (car pat) :pat-app))
     (pattern-type-bindings (third pat) :unknown))
    ((and (listp pat) (eq (car pat) :pat-cons))
     (let ((element-type (if (and (consp type) (eq (car type) :list))
                             (second type)
                             :unknown))
           (list-type (if (and (consp type) (eq (car type) :list))
                          type
                          '(:list :unknown))))
       (list (cons (sml-symbol (second pat)) element-type)
             (cons (sml-symbol (third pat)) list-type))))
    ((and (listp pat) (eq (car pat) :pat-tuple))
     (loop for subpat in (cdr pat)
           for subtype in (if (and (consp type) (eq (car type) :tuple))
                              (cdr type)
                              (make-list (length (cdr pat)) :initial-element :unknown))
           append (pattern-type-bindings subpat subtype)))
    ((and (listp pat) (eq (car pat) :pat-record))
     (let ((field-types (if (and (consp type) (eq (car type) :record))
                            (cdr type)
                            nil)))
       (mapcan (lambda (field)
                 (pattern-type-bindings
                  (second field)
                  (or (cdr (assoc (first field) field-types :test #'string=))
                      :unknown)))
               (cdr pat))))
    (t
     nil)))

(defun pattern-bound-symbols (pat)
  (cond
    ((or (numberp pat) (stringp pat) (characterp pat) (eq pat :wild))
     nil)
    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     (list (sml-symbol (second pat))))
    ((and (listp pat) (member (car pat) '(:pat-ctor :ctor)))
     nil)
    ((and (listp pat) (eq (car pat) :pat-app))
     (pattern-bound-symbols (third pat)))
    ((and (listp pat) (member (car pat) '(:pat-tuple)))
     (mapcan #'pattern-bound-symbols (cdr pat)))
    ((and (listp pat) (member (car pat) '(:pat-unit :pat-nil)))
     nil)
    ((and (listp pat) (eq (car pat) :pat-cons))
     (list (sml-symbol (second pat))
           (sml-symbol (third pat))))
    ((and (listp pat) (eq (car pat) :pat-record))
     (mapcan (lambda (field)
               (pattern-bound-symbols (second field)))
             (cdr pat)))
    (t
     (error "Unknown pattern for variable extraction: ~A" pat))))

(defun compile-clause-pattern (params)
  (if (= (length params) 1)
      (first params)
      `(:pat-tuple ,@params)))

(defun compile-fn-clauses (clauses &optional local-exceptions)
  (let* ((arity (length (first (first clauses))))
         (tmp-args (loop repeat arity collect (gensym "ARG"))))
    (unless (every (lambda (clause) (= (length (first clause)) arity)) clauses)
      (error "All fun clauses must have the same arity: ~A" clauses))
    (reduce (lambda (arg body) `(lambda (,arg) ,body))
            tmp-args
            :from-end t
            :initial-value
            `(trivia:match ,(if (= arity 1)
                                (first tmp-args)
                                `(list :tuple ,@tmp-args))
               ,@(mapcar (lambda (clause)
                           `(,(compile-pat (compile-clause-pattern (first clause))
                                           local-exceptions)
                             ,(compile-expr (second clause) local-exceptions)))
                         clauses)
               (_ (error "Match failure in function"))))))

(defun compile-local-val-binding (pat expr body &optional local-exceptions)
  (cond
    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     `(let ((,(sml-symbol (second pat)) ,expr))
        ,body))
    ((eq pat :wild)
     (let ((tmp (gensym "IGNORED")))
       `(let ((,tmp ,expr))
          (declare (ignore ,tmp))
          ,body)))
    (t
     (let ((tmp (gensym "MATCHED")))
       `(let ((,tmp ,expr))
          (trivia:match ,tmp
            (,(compile-pat pat local-exceptions) ,body)
            (_ (error "Pattern match failure in val binding"))))))))

(defun extend-local-exceptions (local-exceptions dec)
  (if (eq (car dec) :exception)
      (acons (second dec)
             (not (null (getf (cddr dec) :arg-type)))
             local-exceptions)
      local-exceptions))

(defun compile-local-decls (decs body-asts &optional (local-exceptions nil))
  (if (null decs)
      `(progn ,@(mapcar (lambda (expr)
                          (compile-expr expr local-exceptions))
                        body-asts))
      (let* ((dec (first decs))
             (extended-exceptions (extend-local-exceptions local-exceptions dec))
             (body (compile-local-decls (rest decs) body-asts extended-exceptions)))
        (compile-local-decl dec body local-exceptions))))

(defun compile-local-decl (dec body &optional local-exceptions)
  (cond
    ((eq (car dec) :val)
     (compile-local-val-binding (second dec)
                                (compile-expr (third dec) local-exceptions)
                                body
                                local-exceptions))
    ((eq (car dec) :fun)
     (let ((name (sml-symbol (second dec))))
       `(let ((,name nil))
          (setf ,name ,(compile-fn-clauses (third dec) local-exceptions))
          ,body)))
    ((eq (car dec) :val-rec)
     (let ((name (sml-symbol (second dec))))
       `(let ((,name nil))
          (setf ,name ,(compile-expr (third dec) local-exceptions))
          ,body)))
    ((eq (car dec) :exception)
     (let* ((name (sml-symbol (second dec)))
            (arg-type (getf (cddr dec) :arg-type)))
       (if arg-type
           `(let ((,name (make-sml-exception-function ,(second dec))))
              ,body)
           `(let ((,name (make-sml-exception-constructor ,(second dec))))
              ,body))))
    (t
     (error "Unknown decl in let: ~A" dec))))

(defun compile-top-level-val (pat expr &optional local-exceptions)
  (let* ((expr-type (infer-sml-ast-type expr :package *sml-package*))
         (typed-bindings (pattern-type-bindings pat expr-type)))
    (cond
      ((and (listp pat) (member (car pat) '(:pat-var :var)))
       (let* ((sym (sml-symbol (second pat)))
              (declaim-form (compile-type-declaim-form sym expr-type))
              (compiled-expr (compile-expr expr local-exceptions)))
         `(progn
            ,@(when declaim-form (list declaim-form))
            (defparameter ,sym ,compiled-expr)
            ,(compile-type-registration-form sym expr-type)
            ,(compile-export-form (list sym)))))
      ((eq pat :wild)
       (compile-expr expr local-exceptions))
      (t
       (let ((tmp (gensym "MATCHED"))
             (bound-symbols (remove-duplicates (pattern-bound-symbols pat) :test #'eq)))
         `(let ((,tmp ,(compile-expr expr local-exceptions)))
            (trivia:match ,tmp
              (,(compile-pat pat local-exceptions)
               (progn
                 ,@(mapcar (lambda (sym) `(defparameter ,sym ,sym)) bound-symbols)
                 ,@(mapcar (lambda (binding)
                             (compile-type-registration-form (car binding) (cdr binding)))
                           typed-bindings)
                 ,(compile-export-form bound-symbols)
                 ,tmp))
              (_ (error "Pattern match failure in top-level val")))))))))

(defun compile-exception-ctor-pattern (name)
  (let ((it (gensym "EXN")))
    `(guard1 ,it
             (and (sml-exception-tag-p ,it)
                  (eq (sml-exception-constructor-tag ,it)
                      (sml-exception-constructor-tag ,(sml-symbol name)))))))

(defun compile-exception-app-pattern (name payload local-exceptions)
  (let ((it (gensym "EXN")))
    `(guard1 ,it
             (and (consp ,it)
                  (sml-exception-tag-p (car ,it))
                  (eq (sml-exception-constructor-tag ,it)
                      (sml-exception-constructor-tag ,(sml-symbol name))))
             (cdr ,it) ,(compile-pat payload local-exceptions))))

(defun compile-pat (pat &optional local-exceptions)
  (cond
    ((numberp pat) pat)
    ((stringp pat) pat)
    ((characterp pat) pat)
    ((eq pat :wild) '_)

    ((and (listp pat) (member (car pat) '(:pat-ctor :ctor)))
     (if (exception-constructor-info (second pat) local-exceptions)
         (compile-exception-ctor-pattern (second pat))
         `',(sml-symbol (second pat))))

    ((and (listp pat) (eq (car pat) :pat-app))
     (if (exception-constructor-info (second (second pat)) local-exceptions)
         (compile-exception-app-pattern (second (second pat))
                                        (third pat)
                                        local-exceptions)
         (let ((ctor (sml-symbol (second (second pat))))
               (payload (compile-pat (third pat) local-exceptions)))
           `(cons ',ctor ,payload))))

    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     (sml-symbol (second pat)))

    ((and (listp pat) (eq (car pat) :pat-unit))
     `(list :tuple))

    ((and (listp pat) (eq (car pat) :pat-tuple))
     `(list :tuple ,@(mapcar (lambda (subpat)
                               (compile-pat subpat local-exceptions))
                             (cdr pat))))

    ((and (listp pat) (eq (car pat) :pat-record))
     `(list :record
            ,@(mapcar (lambda (field)
                        `(cons ,(first field)
                               ,(compile-pat (second field) local-exceptions)))
                      (record-fields-sorted-by-label (cdr pat)))))

    ((and (listp pat) (eq (car pat) :pat-nil)) 'nil)
    ((and (listp pat) (eq (car pat) :pat-cons))
     `(cons ,(sml-symbol (second pat))
            ,(sml-symbol (third pat))))
    (t (error "Unknown pattern ~A" pat))))

(defun compile-expr (ast &optional local-exceptions)
  "Compiles an SML expression AST into a Common Lisp form."
  (cond
    ((numberp ast) ast)
    ((stringp ast) ast)
    ((characterp ast) ast)

    ((and (listp ast) (eq (car ast) :var))
     (let* ((name (second ast))
            (mapping (assoc name *sml-env* :test #'string=)))
       (if mapping
           (cdr mapping)
           (sml-symbol name))))

    ((and (listp ast) (eq (car ast) :ctor))
     (sml-symbol (second ast)))

    ((and (listp ast) (eq (car ast) :selector))
     `(lambda (record)
        (sml-record-select record ,(second ast))))

    ((and (listp ast) (eq (car ast) :deref))
     `(funcall #'sml-deref ,(compile-expr (second ast) local-exceptions)))

    ;; Replace the :app block in compile-expr
    ((and (listp ast) (eq (car ast) :app))
     (let ((head (second ast))
           (arg (third ast)))
       (if (and (listp head)
                (eq (car head) :ctor)
                (not (exception-constructor-info (second head) local-exceptions)))
           ;; FIX: Intern constructor as keyword and don't quote it here
           `(cons ',(sml-symbol (second head))
                  ,(compile-expr arg local-exceptions))
           `(funcall ,(compile-expr head local-exceptions)
                     ,(compile-expr arg local-exceptions)))))

    ((and (listp ast) (eq (car ast) :case))
     `(trivia:match ,(compile-expr (second ast) local-exceptions)
        ,@(mapcar (lambda (branch)
                    `(,(compile-pat (first branch) local-exceptions)
                      ,(compile-expr (second branch) local-exceptions)))
                  (cddr ast))))

    ((and (listp ast) (eq (car ast) :handle))
     (let ((branches (third ast))
           (condition-var (gensym "EXN")))
       `(handler-case ,(compile-expr (second ast) local-exceptions)
          (sml-raised-exception (,condition-var)
            (let ((value (sml-exception-value ,condition-var)))
              (trivia:match value
                ,@(mapcar (lambda (branch)
                            `(,(compile-pat (first branch) local-exceptions)
                              ,(compile-expr (second branch) local-exceptions)))
                          branches)
                (_ (sml-raise value))))))))

    ;; --- Control Flow & Short-Circuiting ---
    ((and (listp ast) (eq (car ast) :if))
     `(if ,(compile-expr (second ast) local-exceptions)
          ,(compile-expr (third ast) local-exceptions)
          ,(compile-expr (fourth ast) local-exceptions)))

    ((and (listp ast) (eq (car ast) :raise))
     `(sml-raise ,(compile-expr (second ast) local-exceptions)))

    ((and (listp ast) (eq (car ast) :let))
     (compile-local-decls (second ast) (third ast) local-exceptions))

    ((and (listp ast) (eq (car ast) :andalso))
     `(and ,(compile-expr (second ast) local-exceptions)
           ,(compile-expr (third ast) local-exceptions)))

    ((and (listp ast) (eq (car ast) :orelse))
     `(or ,(compile-expr (second ast) local-exceptions)
          ,(compile-expr (third ast) local-exceptions)))

    ((and (listp ast) (eq (car ast) :seq))
     `(progn ,@(mapcar (lambda (expr)
                         (compile-expr expr local-exceptions))
                       (cdr ast))))

    ;; Add this to compile-expr!
    ((and (listp ast) (eq (car ast) :list))
     `(list ,@(mapcar (lambda (expr)
                        (compile-expr expr local-exceptions))
                      (cdr ast))))

    ((and (listp ast) (eq (car ast) :record))
     `(make-sml-record
       (list ,@(mapcar (lambda (field)
                         `(cons ,(first field)
                                ,(compile-expr (second field) local-exceptions)))
                       (record-fields-sorted-by-label (cdr ast))))))

    ((and (listp ast) (eq (car ast) :unit))
     `(list :tuple))

    ((and (listp ast) (eq (car ast) :tuple))
     `(list :tuple ,@(mapcar (lambda (expr)
                               (compile-expr expr local-exceptions))
                             (cdr ast))))

    ((and (listp ast) (eq (car ast) :fn))
     (let ((clauses (second ast))
           (tmp-arg (gensym "ARG")))
       `(lambda (,tmp-arg)
          (trivia:match ,tmp-arg
            ,@(mapcar (lambda (branch)
                        `(,(compile-pat (first branch) local-exceptions)
                          ,(compile-expr (second branch) local-exceptions)))
                      clauses)
            (_ (error "Match failure in anonymous function"))))))

    (t (error "Unknown AST: ~A" ast))))

(defun compile-decl (ast &optional local-exceptions)
  "Compiles top level declarations."
  (cond
    ((eq (car ast) :val)
     (compile-top-level-val (second ast) (third ast) local-exceptions))
    ((eq (car ast) :val-rec)
     (let ((name (sml-symbol (second ast))))
       `(progn
          (declaim (special ,name))
          (defparameter ,name nil)
          (setf ,name ,(compile-expr (third ast) local-exceptions))
          ,(compile-type-registration-form name (infer-sml-ast-type (third ast) :package *sml-package*))
          ,(compile-export-form (list name)))))
    ((eq (car ast) :fun)
     (let* ((name (sml-symbol (second ast)))
            (fun-type (loop repeat (length (first (first (third ast))))
                            for result = :unknown then `(:fn :unknown ,result)
                            finally (return result))))
       `(progn
          (declaim (special ,name))
          (defparameter ,name ,(compile-fn-clauses (third ast) local-exceptions))
          ,(compile-type-registration-form name fun-type)
          ,(compile-export-form (list name)))))

    ;; Replace the :datatype block in compile-decl
    ((eq (car ast) :datatype)
     (let ((ctors (third ast)))
       `(progn
          ,@(mapcar (lambda (c)
                      (let* ((cname (sml-symbol (second c)))
                             (keyword (sml-symbol (second c)))
                             (has-args (fourth c)))
                        (if has-args
                            ;; If it has args, the name refers to a constructor function
                            `(progn
                               (defun ,cname (payload) (cons ',keyword payload))
                               (defparameter ,cname #',cname)
                               ,(compile-type-registration-form cname `(:fn ,(or (getf (cddr c) :arg-type) :unknown) ,(second ast)))
                               ,(compile-export-form (list cname)))
                            ;; If no args, it's just the keyword constant
                            `(progn
                               (defparameter ,cname ',keyword)
                               ,(compile-type-registration-form cname (second ast))
                               ,(compile-export-form (list cname))))))
                    ctors)
          ,(compile-export-form nil))))
    ((eq (car ast) :exception)
     (let* ((name (sml-symbol (second ast)))
            (arg-type (getf (cddr ast) :arg-type)))
       (if arg-type
           `(progn
              (defparameter ,name (make-sml-exception-function ,(second ast)))
              ,(compile-type-registration-form name `(:fn ,arg-type "exn"))
              ,(compile-export-form (list name)))
           `(progn
              (defparameter ,name (make-sml-exception-constructor ,(second ast)))
              ,(compile-type-registration-form name "exn")
              ,(compile-export-form (list name))))))

    (t (error "Unknown Declaration: ~A" ast))))

(defun compile-program-decls (decs &optional (local-exceptions nil))
  (if (null decs)
      nil
      (let* ((dec (first decs))
             (form (compile-decl dec local-exceptions))
             (extended-exceptions (extend-local-exceptions local-exceptions dec)))
        (cons form
              (compile-program-decls (rest decs) extended-exceptions)))))

(defun compile-program (ast)
  (if (eq (car ast) :program)
      `(progn ,@(compile-program-decls (cdr ast)))
      (error "Not a program AST.")))
