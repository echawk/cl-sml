(defpackage #:cl-sml-repl-tests
  (:use #:cl #:fiveam #:cl-sml))

(in-package #:cl-sml-repl-tests)

(def-suite cl-sml-repl-suite
  :description "REPL tests for cl-sml")

(in-suite cl-sml-repl-suite)

(defun sml-user-symbol-status (name)
  (nth-value 1 (find-symbol (string-upcase name) "SML-USER")))

(defun run-repl-session (input)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((result
            (with-input-from-string (stream input)
              (cl-sml:repl :input stream :output out :error-output err :prompt t))))
      (values result
              (get-output-stream-string out)
              (get-output-stream-string err)))))

(defun run-repl-session-in-package (input package)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((result
            (with-input-from-string (stream input)
              (cl-sml:repl :input stream
                           :output out
                           :error-output err
                           :prompt t
                           :package package))))
      (values result
              (get-output-stream-string out)
              (get-output-stream-string err)))))

(test repl-handles-declarations-and-expressions
  (multiple-value-bind (result output error-output)
      (run-repl-session (format nil "val x = 10;~%x + 5~%:quit~%"))
    (is (eq :quit result))
    (is (search "- " output))
    (is (search "val x = 10" output))
    (is (search "val it = 15" output))
    (is (eq :external (sml-user-symbol-status "x")))
    (is (string= "" error-output))))

(test repl-supports-expression-semicolons-and-multiline-definitions
  (multiple-value-bind (result output error-output)
      (run-repl-session (format nil "fun add a b =~%  a + b;~%add 2 3;~%:quit~%"))
    (is (eq :quit result))
    (is (search "= " output))
    (is (search "val add = <fn>" output))
    (is (search "val it = 5" output))
    (is (string= "" error-output))))

(test repl-recovers-after-errors
  (multiple-value-bind (result output error-output)
      (run-repl-session (format nil "val x = ;~%1 + 2~%:quit~%"))
    (is (eq :quit result))
    (is (search "Error:" error-output))
    (is (search "val it = 3" output))))

(test repl-sees-file-loaded-definitions
  (cl-sml:load-sml-file #P"testdata/sample-program.sml")
  (multiple-value-bind (result output error-output)
      (run-repl-session (format nil "fileFact 4~%:quit~%"))
    (is (eq :quit result))
    (is (search "Error:" error-output))
    (is (not (search "val it = 24" output)))))

(test repl-can-target-file-package
  (multiple-value-bind (package result)
      (cl-sml:load-sml-file #P"testdata/sample-program.sml")
    (declare (ignore result))
    (multiple-value-bind (repl-result output error-output)
        (run-repl-session-in-package (format nil "fileFact 4~%:quit~%") package)
      (is (eq :quit repl-result))
      (is (search "val it = 24" output))
      (is (string= "" error-output)))))

(test repl-returns-eof-on-end-of-input
  (multiple-value-bind (result output error-output)
      (run-repl-session "")
    (is (eq :eof result))
    (is (search "- " output))
    (is (string= "" error-output))))

(fiveam:run! 'cl-sml-repl-suite)
