(in-package #:cl-sml)

(defun repl-it-symbol ()
  (sml-symbol "it"))

(defun trim-repl-input (source)
  (string-trim '(#\Space #\Tab #\Newline #\Return) source))

(defun parse-repl-input (source)
  (let* ((trimmed (trim-repl-input source))
         (length (length trimmed)))
    (cond
      ((zerop length)
       (values nil nil nil))
      ((char= (char trimmed (1- length)) #\;)
       (handler-case
           (values :program (esrap:parse 'sml-program trimmed) nil)
         (error (program-error)
           (let ((expr-source (trim-repl-input (subseq trimmed 0 (1- length)))))
             (handler-case
                 (values :expr (esrap:parse 'sml-expr expr-source) nil)
               (error ()
                 (values nil nil program-error)))))))
      (t
       (handler-case
           (values :expr (esrap:parse 'sml-expr trimmed) nil)
         (error (condition)
           (values nil nil condition)))))))

(defun sml-namespaced-symbol-p (value)
  (and (symbolp value)
       (let ((package (symbol-package value)))
         (and package
              (sml-package-name-p (package-name package))))))

(defun constructor-value-p (value)
  (and (consp value)
       (sml-namespaced-symbol-p (car value))))

(defun sml-type-known-p (type)
  (cond
    ((null type) nil)
    ((eq type :unknown) nil)
    ((stringp type) t)
    ((and (consp type) (eq (car type) :list))
     (sml-type-known-p (second type)))
    ((and (consp type) (eq (car type) :tuple))
     (every #'sml-type-known-p (cdr type)))
    ((and (consp type) (eq (car type) :record))
     (every #'sml-type-known-p (mapcar #'cdr (cdr type))))
    ((and (consp type) (eq (car type) :fn))
     (and (sml-type-known-p (second type))
          (sml-type-known-p (third type))))
    (t nil)))

(defun repl-type-suffix (symbol)
  (let ((type (lookup-sml-binding-type symbol)))
    (if (sml-type-known-p type)
        (format nil " : ~A" (sml-type->string type))
        "")))

(defun sml-value->string (value)
  (cond
    ((functionp value) "<fn>")
    ((and (vectorp value)
          (= (length value) 2)
          (eq (aref value 0) :ref))
     (format nil "ref ~A" (sml-value->string (aref value 1))))
    ((sml-record-p value)
     (format nil "{~{~A~^, ~}}"
             (mapcar (lambda (field)
                       (format nil "~A = ~A"
                               (car field)
                               (sml-value->string (cdr field))))
                     (sml-record-fields value))))
    ((and (consp value) (sml-exception-p value))
     (format nil "~A ~A"
             (sml-exception-name value)
             (sml-value->string (sml-exception-payload value))))
    ((sml-exception-tag-p value)
     (sml-exception-name value))
    ((eq value t) "true")
    ((null value) "[]")
    ((and (listp value) (eq (car value) :tuple))
     (if (null (cdr value))
         "()"
         (format nil "(~{~A~^, ~})"
                 (mapcar #'sml-value->string (cdr value)))))
    ((constructor-value-p value)
     (format nil "~A ~A"
             (symbol-name (car value))
             (sml-value->string (cdr value))))
    ((and (listp value) (not (constructor-value-p value)))
     (format nil "[~{~A~^, ~}]"
             (mapcar #'sml-value->string value)))
    ((characterp value)
     (format nil "#~S" (string value)))
    ((stringp value)
     (prin1-to-string value))
    ((sml-namespaced-symbol-p value)
     (symbol-name value))
    (t
     (princ-to-string value))))

(defun repl-symbol-display-name (symbol)
  (string-downcase (symbol-name symbol)))

(defun repl-report-decl (decl)
  (case (car decl)
    (:val
     (mapcar (lambda (sym)
               (format nil "val ~A = ~A~A"
                       (repl-symbol-display-name sym)
                       (sml-value->string (symbol-value sym))
                       (repl-type-suffix sym)))
             (remove-duplicates (pattern-bound-symbols (second decl)) :test #'eq)))
    (:val-rec
     (let ((sym (sml-symbol (second decl))))
       (list (format nil "val ~A = <fn>~A"
                     (repl-symbol-display-name sym)
                     (repl-type-suffix sym)))))
    (:fun
     (let ((sym (sml-symbol (second decl))))
       (list (format nil "val ~A = <fn>~A"
                     (repl-symbol-display-name sym)
                     (repl-type-suffix sym)))))
    (:datatype
     (list (format nil "datatype ~A"
                   (string-downcase (second decl)))))
    (:exception
     (let ((sym (sml-symbol (second decl))))
       (list (format nil "exception ~A~A"
                     (sml-exception-name (symbol-value sym))
                     (if (getf (cddr decl) :arg-type)
                         (format nil " of ~A" (getf (cddr decl) :arg-type))
                         "")))))
    (otherwise
     (list (format nil "~S" decl)))))

(defun eval-repl-program (ast)
  (eval (compile-program ast))
  (mapcan #'repl-report-decl (cdr ast)))

(defun eval-repl-expression (ast)
  (let* ((type (infer-sml-ast-type ast :package *sml-package*))
         (value (eval (compile-expr ast))))
    (setf (symbol-value (repl-it-symbol)) value)
    (register-sml-binding-type (repl-it-symbol) type)
    (export-sml-symbols (list (repl-it-symbol)))
    (list (format nil "val it = ~A~A"
                  (sml-value->string value)
                  (repl-type-suffix (repl-it-symbol))))))

(defun prompt-string (continuation-p)
  (if continuation-p "= " "- "))

(defun repl (&key (input *standard-input*)
                  (output *standard-output*)
                  (error-output *error-output*)
                  (prompt t)
                  (package "SML-USER"))
  (let ((*sml-package* (ensure-sml-package package))
        (buffer ""))
    (labels ((emit-prompt ()
               (when prompt
                 (write-string (prompt-string (not (string= buffer ""))) output)
                 (finish-output output)))
             (reset-buffer ()
               (setf buffer "")))
      (loop
        (emit-prompt)
        (let ((line (read-line input nil :eof)))
          (when (eql line :eof)
            (return :eof))
          (when (member (string-downcase (trim-repl-input line))
                        '(":quit" ":q" ":exit")
                        :test #'string=)
            (return :quit))
          (setf buffer (concatenate 'string buffer line (string #\Newline)))
          (multiple-value-bind (kind ast error) (parse-repl-input buffer)
            (let ((trimmed-buffer (trim-repl-input buffer)))
            (cond
              (kind
               (handler-case
                   (dolist (line (ecase kind
                                   (:program (eval-repl-program ast))
                                   (:expr (eval-repl-expression ast))))
                     (format output "~A~%" line))
                 (error (condition)
                   (format error-output "Error: ~A~%" condition)))
               (reset-buffer))
              ((or (and (> (length trimmed-buffer) 0)
                        (char= (char trimmed-buffer
                                     (1- (length trimmed-buffer)))
                               #\;))
                   (string= (trim-repl-input line) ""))
               (when error
                 (format error-output "Error: ~A~%" error))
               (reset-buffer))))))))))
