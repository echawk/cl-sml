(in-package #:cl-sml)

(defun read-sml-source (stream)
  (with-output-to-string (out)
    (loop for c = (read-char stream nil :eof)
          until (eql c :eof)
          do (write-char c out))))

(defun compile-sml-program-string (sml-text &key package)
  (let ((*sml-package* (ensure-sml-package (or package (current-sml-package)))))
    (compile-program (esrap:parse 'sml-program sml-text))))

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
    (values target-package (eval form))))

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
