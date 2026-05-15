(in-package #:cl-sml)

(defun read-sml-source (stream)
  (with-output-to-string (out)
    (loop for c = (read-char stream nil :eof)
          until (eql c :eof)
          do (write-char c out))))

(defun sml-file-directory (pathname)
  (make-pathname :directory (pathname-directory (truename pathname))))

(defun sml-declaration-start-line-p (line)
  (some (lambda (keyword)
          (let ((length (length keyword)))
            (and (<= length (length line))
                 (string= keyword line :end2 length)
                 (or (= length (length line))
                     (member (char line length)
                             '(#\Space #\Tab #\; #\= #\: #\()
                             :test #'char=)))))
        '("abstype" "datatype" "end" "exception" "fun" "functor" "in"
          "infix" "infixr" "local" "nonfix" "open" "signature"
          "structure" "type" "val" "withtype")))

(defun sml-type-declaration-start-line-p (line)
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (and (<= 4 (length trimmed))
         (string= "type" trimmed :end2 4)
         (or (= 4 (length trimmed))
             (member (char trimmed 4) '(#\Space #\Tab #\()
                     :test #'char=)))))

(defun normalize-sml-type-declaration-continuations (source)
  (with-output-to-string (out)
    (let ((in-type-declaration nil)
          (at-start t))
      (dolist (line (uiop:split-string source :separator '(#\Newline)))
        (let* ((trimmed-left (string-left-trim '(#\Space #\Tab) line))
               (starts-declaration (sml-declaration-start-line-p trimmed-left)))
          (cond
            ((and in-type-declaration
                  (not starts-declaration)
                  (plusp (length trimmed-left)))
             (write-char #\Space out)
             (write-string trimmed-left out))
            (t
             (unless at-start
               (write-char #\Newline out))
             (write-string line out)
             (setf in-type-declaration
                   (sml-type-declaration-start-line-p line)))))
        (setf at-start nil)))))

(defun compile-sml-program-string (sml-text &key package)
  (let ((*sml-package* (ensure-sml-package (or package (current-sml-package)))))
    (compile-program
     (esrap:parse 'sml-program
                  (normalize-sml-type-declaration-continuations sml-text)))))

(defun compile-sml-declarations-string (sml-text &key package)
  (compile-sml-program-string sml-text :package package))

(defun compile-sml-expression-string (sml-text &key package)
  (let ((*sml-package* (ensure-sml-package (or package (current-sml-package)))))
    (compile-expr (esrap:parse 'sml-expr sml-text))))

(defun compile-sml-file (pathname &key package)
  (with-open-file (stream pathname :direction :input)
    (let* ((target-package (ensure-sml-package (or package (pathname->sml-package-name pathname))))
           (form (compile-sml-program-string (read-sml-source stream) :package target-package)))
      (values form target-package))))

(defun load-sml-file (pathname &key package)
  (multiple-value-bind (form target-package)
      (compile-sml-file pathname :package package)
    (let ((*sml-package* target-package)
          (*sml-current-directory* (sml-file-directory pathname)))
      (values target-package (eval form)))))

(defun read-sml-block (stream char arg)
  (declare (ignore char arg))
  (let ((sml-text
          (with-output-to-string (out)
            (loop for c = (read-char stream nil :eof)
                  when (eql c :eof) do (error "Unterminated SML block")
                  when (char= c #\})
                    do (let ((next (read-char stream nil :eof)))
                         (if (char= next #\#)
                             (return)
                             (progn (write-char c out) (write-char next out))))
                  else do (write-char c out)))))
    (compile-sml-program-string sml-text :package (current-sml-package))))

(defreadtable sml-readtable
  (:merge :standard)
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\{ #'read-sml-block))
