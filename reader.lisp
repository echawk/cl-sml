(in-package #:cl-sml)

(defun read-sml-source (stream)
  (with-output-to-string (out)
    (loop for c = (read-char stream nil :eof)
          until (eql c :eof)
          do (write-char c out))))

(defun compile-sml-program-string (sml-text)
  (compile-program (esrap:parse 'sml-program sml-text)))

(defun compile-sml-file (pathname)
  (with-open-file (stream pathname :direction :input)
    (compile-sml-program-string (read-sml-source stream))))

(defun load-sml-file (pathname)
  (eval (compile-sml-file pathname)))

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
    (compile-sml-program-string sml-text)))

(defreadtable sml-readtable
  (:merge :standard)
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\{ #'read-sml-block))
