(in-package #:cl-sml)

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
    (let ((ast (esrap:parse 'sml-program sml-text)))
      (compile-program ast))))

(defreadtable sml-readtable
  (:merge :standard)
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\{ #'read-sml-block))
