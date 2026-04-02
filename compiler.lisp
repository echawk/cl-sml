(in-package #:cl-sml)

(defun compile-pat (pat)
  (cond
    ((numberp pat) pat)
    ((stringp pat) pat)
    ((characterp pat) pat)
    ((eq pat :wild) '_)

    ((and (listp pat) (member (car pat) '(:pat-ctor :ctor)))
     ;; We QUOTE the symbol so trivia matches the value, rather than binding a variable
     `',(intern (string-upcase (second pat)) "CL-SML"))

    ((and (listp pat) (eq (car pat) :pat-app))
     (let ((ctor (intern (string-upcase (second (second pat))) "CL-SML"))
           (payload (compile-pat (third pat))))
       `(cons ',ctor ,payload)))

    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     (intern (string-upcase (second pat)) "CL-SML"))

    ((and (listp pat) (eq (car pat) :pat-unit))
     `(list :tuple))

    ((and (listp pat) (eq (car pat) :pat-tuple))
     `(list :tuple ,@(mapcar #'compile-pat (cdr pat))))

    ((and (listp pat) (eq (car pat) :pat-nil)) 'nil)
    ((and (listp pat) (eq (car pat) :pat-cons))
     `(cons ,(intern (string-upcase (second pat)) "CL-SML")
            ,(intern (string-upcase (third pat)) "CL-SML")))
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
           (intern (string-upcase name) "CL-SML"))))

    ((and (listp ast) (eq (car ast) :ctor))
     (intern (string-upcase (second ast)) "CL-SML"))

    ((and (listp ast) (eq (car ast) :deref))
     `(funcall #'sml-deref ,(compile-expr (second ast))))

    ;; Replace the :app block in compile-expr
    ((and (listp ast) (eq (car ast) :app))
     (let ((head (second ast))
           (arg (third ast)))
       (if (and (listp head) (eq (car head) :ctor))
           ;; FIX: Intern constructor as keyword and don't quote it here
           `(cons ',(intern (string-upcase (second head)) "CL-SML")
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

    ;; --- Local Let Bindings ---
    ;; --- Local Let Bindings ---
    ((and (listp ast) (eq (car ast) :let))
     (let ((decs (second ast))
           (body-exprs (third ast)))
       `(let* ,(mapcar (lambda (dec)
                         (cond
                           ((eq (car dec) :val)
                            ;; Force let-bound vals into CL-SML
                            `(,(intern (string-upcase (second dec)) "CL-SML") ,(compile-expr (third dec))))
                           ((eq (car dec) :fun)
                            ;; Force let-bound function names and parameters into CL-SML
                            (let* ((name (intern (string-upcase (second dec)) "CL-SML"))
                                   (params (mapcar (lambda (p) (intern (string-upcase p) "CL-SML")) (third dec)))
                                   (body-expr (compile-expr (fourth dec)))
                                   (curried (reduce (lambda (p b) `(lambda (,p) ,b))
                                                    params :initial-value body-expr :from-end t)))
                              `(,name ,curried)))
                           (t (error "Unknown decl in let: ~A" dec))))
                       decs)
          ;; let* implicitly returns the result of the final expression
          ,@(mapcar #'compile-expr body-exprs))))

    ((and (listp ast) (eq (car ast) :andalso))
     `(and ,(compile-expr (second ast)) ,(compile-expr (third ast))))

    ((and (listp ast) (eq (car ast) :orelse))
     `(or ,(compile-expr (second ast)) ,(compile-expr (third ast))))

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
     `(defparameter ,(intern (string-upcase (second ast)) "CL-SML")
                    ,(compile-expr (third ast))))
    ((eq (car ast) :fun)
     (let* ((name (intern (string-upcase (second ast)) "CL-SML"))
            (params (mapcar (lambda (p) (intern (string-upcase p) "CL-SML")) (third ast)))
            (body (compile-expr (fourth ast)))
            (curried (reduce (lambda (p b) `(lambda (,p) ,b))
                             params :initial-value body :from-end t)))
       ;; FIX: Wrap in progn and declaim so Lisp knows it's global before compiling the body
       `(progn
          (declaim (special ,name))
          (defparameter ,name ,curried))))

    ;; ((eq (car ast) :fun)
    ;;  (let* ((name (intern (string-upcase (second ast)) "CL-SML"))
    ;;         (params (mapcar (lambda (p) (intern (string-upcase p) "CL-SML")) (third ast)))
    ;;         (body (compile-expr (fourth ast)))
    ;;         (curried (reduce (lambda (p b) `(lambda (,p) ,b))
    ;;                          params :initial-value body :from-end t)))
    ;;    `(defparameter ,name ,curried)))

    ;; Replace the :datatype block in compile-decl
    ((eq (car ast) :datatype)
     (let ((ctors (third ast)))
       `(progn
          ,@(mapcar (lambda (c)
                      (let* ((cname (intern (string-upcase (second c)) "CL-SML"))
                             (keyword (intern (string-upcase (second c)) "CL-SML"))
                             (has-args (fourth c)))
                        (if has-args
                            ;; If it has args, the name refers to a constructor function
                            `(progn
                               (defun ,cname (payload) (cons ',keyword payload))
                               (defparameter ,cname #',cname))
                            ;; If no args, it's just the keyword constant
                            `(defparameter ,cname ',keyword))))
                    ctors))))

    (t (error "Unknown Declaration: ~A" ast))))

(defun compile-program (ast)
  (if (eq (car ast) :program)
      `(progn ,@(mapcar #'compile-decl (cdr ast)))
      (error "Not a program AST.")))
