(in-package #:cl-sml)

(defun compile-pat (pat)
  (cond
    ((numberp pat) pat)
    ((eq pat :wild) '_)
    ;; Use keywords for constructors so they match regardless of package
    ((and (listp pat) (member (car pat) '(:pat-ctor :ctor)))
     (intern (string-upcase (second pat)) "KEYWORD"))

    ((and (listp pat) (eq (car pat) :pat-app))
     (let ((ctor (compile-pat (second pat)))
           (payload (compile-pat (third pat))))
       `(cons ,ctor ,payload)))

    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     (intern (string-upcase (second pat))))

    ((and (listp pat) (eq (car pat) :pat-nil)) 'nil)
    ((and (listp pat) (eq (car pat) :pat-cons))
     `(cons ,(intern (string-upcase (second pat)))
            ,(intern (string-upcase (third pat)))))
    (t (error "Unknown pattern ~A" pat))))

(defun compile-expr (ast)
  "Compiles an SML expression AST into a Common Lisp form."
  (cond
    ((numberp ast) ast)

    ((and (listp ast) (eq (car ast) :var))
     (let* ((name (second ast))
            (mapping (assoc name *sml-env* :test #'string=)))
       (if mapping
           (cdr mapping)
           (intern (string-upcase name)))))

    ((and (listp ast) (eq (car ast) :ctor))
     `(make-sml-adt ,(intern (string-upcase (second ast)) "KEYWORD")))

    ;; Replace the :app block in compile-expr
    ((and (listp ast) (eq (car ast) :app))
     (let ((head (second ast))
           (arg (third ast)))
       (if (and (listp head) (eq (car head) :ctor))
           ;; FIX: Intern constructor as keyword and don't quote it here
           `(cons ,(intern (string-upcase (second head)) "KEYWORD")
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
    ((and (listp ast) (eq (car ast) :let))
     (let ((decs (second ast))
           (body-exprs (third ast)))
       `(let* ,(mapcar (lambda (dec)
                         (cond
                           ((eq (car dec) :val)
                            `(,(intern (string-upcase (second dec))) ,(compile-expr (third dec))))
                           ((eq (car dec) :fun)
                            (let* ((name (intern (string-upcase (second dec))))
                                   (params (mapcar (lambda (p) (intern (string-upcase p))) (third dec)))
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

    (t (error "Unknown AST: ~A" ast))))

(defun compile-decl (ast)
  "Compiles top level declarations."
  (cond
    ((eq (car ast) :val)
     `(defparameter ,(intern (string-upcase (second ast)))
                    ,(compile-expr (third ast))))
    ((eq (car ast) :fun)
     (let* ((name (intern (string-upcase (second ast))))
            (params (mapcar (lambda (p) (intern (string-upcase p))) (third ast)))
            (body (compile-expr (fourth ast)))
            (curried (reduce (lambda (p b) `(lambda (,p) ,b))
                             params :initial-value body :from-end t)))
       `(defparameter ,name ,curried)))

    ;; Replace the :datatype block in compile-decl
    ((eq (car ast) :datatype)
     (let ((ctors (third ast)))
       `(progn
          ,@(mapcar (lambda (c)
                      (let* ((cname (intern (string-upcase (second c))))
                             (keyword (intern (string-upcase (second c)) "KEYWORD"))
                             (has-args (fourth c)))
                        (if has-args
                            ;; If it has args, the name refers to a constructor function
                            `(progn
                               (defun ,cname (payload) (cons ,keyword payload))
                               (defparameter ,cname #',cname))
                            ;; If no args, it's just the keyword constant
                            `(defparameter ,cname ,keyword))))
                    ctors))))

    (t (error "Unknown Declaration: ~A" ast))))

(defun compile-program (ast)
  (if (eq (car ast) :program)
      `(progn ,@(mapcar #'compile-decl (cdr ast)))
      (error "Not a program AST.")))
