(in-package #:cl-sml)

(defun target-sml-package-name ()
  (package-name (ensure-sml-package *sml-package*)))

(defun compile-export-form (symbols)
  (when symbols
    `(export ',symbols ,(target-sml-package-name))))

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
    (t
     (error "Unknown pattern for variable extraction: ~A" pat))))

(defun compile-clause-pattern (params)
  (if (= (length params) 1)
      (first params)
      `(:pat-tuple ,@params)))

(defun compile-fn-clauses (clauses)
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
                           `(,(compile-pat (compile-clause-pattern (first clause)))
                             ,(compile-expr (second clause))))
                         clauses)
               (_ (error "Match failure in function"))))))

(defun compile-local-val-binding (pat expr body)
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
            (,(compile-pat pat) ,body)
            (_ (error "Pattern match failure in val binding"))))))))

(defun compile-local-decls (decs body)
  (if (null decs)
      body
      (let ((dec (first decs)))
        (compile-local-decl dec (compile-local-decls (rest decs) body)))))

(defun compile-local-decl (dec body)
  (cond
    ((eq (car dec) :val)
     (compile-local-val-binding (second dec) (compile-expr (third dec)) body))
    ((eq (car dec) :fun)
     (let ((name (sml-symbol (second dec))))
       `(let ((,name nil))
          (setf ,name ,(compile-fn-clauses (third dec)))
          ,body)))
    ((eq (car dec) :val-rec)
     (let ((name (sml-symbol (second dec))))
       `(let ((,name nil))
          (setf ,name ,(compile-expr (third dec)))
          ,body)))
    (t
     (error "Unknown decl in let: ~A" dec))))

(defun compile-top-level-val (pat expr)
  (cond
    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     (let ((sym (sml-symbol (second pat))))
       `(progn
          (defparameter ,sym ,expr)
          ,(compile-export-form (list sym)))))
    ((eq pat :wild)
     expr)
    (t
     (let ((tmp (gensym "MATCHED"))
           (bound-symbols (remove-duplicates (pattern-bound-symbols pat) :test #'eq)))
       `(let ((,tmp ,expr))
         (trivia:match ,tmp
            (,(compile-pat pat)
             (progn
               ,@(mapcar (lambda (sym) `(defparameter ,sym ,sym)) bound-symbols)
               ,(compile-export-form bound-symbols)
               ,tmp))
            (_ (error "Pattern match failure in top-level val"))))))))

(defun compile-pat (pat)
  (cond
    ((numberp pat) pat)
    ((stringp pat) pat)
    ((characterp pat) pat)
    ((eq pat :wild) '_)

    ((and (listp pat) (member (car pat) '(:pat-ctor :ctor)))
     ;; We QUOTE the symbol so trivia matches the value, rather than binding a variable
     `',(sml-symbol (second pat)))

    ((and (listp pat) (eq (car pat) :pat-app))
     (let ((ctor (sml-symbol (second (second pat))))
           (payload (compile-pat (third pat))))
       `(cons ',ctor ,payload)))

    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     (sml-symbol (second pat)))

    ((and (listp pat) (eq (car pat) :pat-unit))
     `(list :tuple))

    ((and (listp pat) (eq (car pat) :pat-tuple))
     `(list :tuple ,@(mapcar #'compile-pat (cdr pat))))

    ((and (listp pat) (eq (car pat) :pat-nil)) 'nil)
    ((and (listp pat) (eq (car pat) :pat-cons))
     `(cons ,(sml-symbol (second pat))
            ,(sml-symbol (third pat))))
    (t (error "Unknown pattern ~A" pat))))

(defun compile-expr (ast)
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

    ((and (listp ast) (eq (car ast) :deref))
     `(funcall #'sml-deref ,(compile-expr (second ast))))

    ;; Replace the :app block in compile-expr
    ((and (listp ast) (eq (car ast) :app))
     (let ((head (second ast))
           (arg (third ast)))
       (if (and (listp head) (eq (car head) :ctor))
           ;; FIX: Intern constructor as keyword and don't quote it here
           `(cons ',(sml-symbol (second head))
                  ,(compile-expr arg))
           `(funcall ,(compile-expr head) ,(compile-expr arg)))))

    ((and (listp ast) (eq (car ast) :case))
     `(trivia:match ,(compile-expr (second ast))
        ,@(mapcar (lambda (branch)
                    `(,(compile-pat (first branch)) ,(compile-expr (second branch))))
                  (cddr ast))))

    ;; --- Control Flow & Short-Circuiting ---
    ((and (listp ast) (eq (car ast) :if))
     `(if ,(compile-expr (second ast))
          ,(compile-expr (third ast))
          ,(compile-expr (fourth ast))))

    ((and (listp ast) (eq (car ast) :let))
     (compile-local-decls (second ast)
                          `(progn ,@(mapcar #'compile-expr (third ast)))))

    ((and (listp ast) (eq (car ast) :andalso))
     `(and ,(compile-expr (second ast)) ,(compile-expr (third ast))))

    ((and (listp ast) (eq (car ast) :orelse))
     `(or ,(compile-expr (second ast)) ,(compile-expr (third ast))))

    ((and (listp ast) (eq (car ast) :seq))
     `(progn ,@(mapcar #'compile-expr (cdr ast))))

    ;; Add this to compile-expr!
    ((and (listp ast) (eq (car ast) :list))
     `(list ,@(mapcar #'compile-expr (cdr ast))))

    ((and (listp ast) (eq (car ast) :unit))
     `(list :tuple))

    ((and (listp ast) (eq (car ast) :tuple))
     `(list :tuple ,@(mapcar #'compile-expr (cdr ast))))

    ((and (listp ast) (eq (car ast) :fn))
     (let ((clauses (second ast))
           (tmp-arg (gensym "ARG")))
       `(lambda (,tmp-arg)
          (trivia:match ,tmp-arg
            ,@(mapcar (lambda (branch)
                        `(,(compile-pat (first branch)) ,(compile-expr (second branch))))
                      clauses)
            (_ (error "Match failure in anonymous function"))))))

    (t (error "Unknown AST: ~A" ast))))

(defun compile-decl (ast)
  "Compiles top level declarations."
  (cond
    ((eq (car ast) :val)
     (compile-top-level-val (second ast) (compile-expr (third ast))))
    ((eq (car ast) :val-rec)
     (let ((name (sml-symbol (second ast))))
       `(progn
          (declaim (special ,name))
          (defparameter ,name nil)
          (setf ,name ,(compile-expr (third ast)))
          ,(compile-export-form (list name)))))
    ((eq (car ast) :fun)
     (let ((name (sml-symbol (second ast))))
       `(progn
          (declaim (special ,name))
          (defparameter ,name ,(compile-fn-clauses (third ast)))
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
                               ,(compile-export-form (list cname)))
                            ;; If no args, it's just the keyword constant
                            `(progn
                               (defparameter ,cname ',keyword)
                               ,(compile-export-form (list cname))))))
                    ctors)
          ,(compile-export-form nil))))

    (t (error "Unknown Declaration: ~A" ast))))

(defun compile-program (ast)
  (if (eq (car ast) :program)
      `(progn ,@(mapcar #'compile-decl (cdr ast)))
      (error "Not a program AST.")))
