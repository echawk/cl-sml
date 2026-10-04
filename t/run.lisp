(defpackage #:cl-sml-test-runner
  (:use #:cl)
  (:export #:run-tests))

(in-package #:cl-sml-test-runner)

(defparameter *suites*
  '(cl-sml-tests::cl-sml-parser-suite
    cl-sml-compiler-tests::cl-sml-compiler-suite
    cl-sml-runtime-tests::cl-sml-runtime-suite
    cl-sml-repl-tests::cl-sml-repl-suite
    cl-sml-hamlet-tests::cl-sml-hamlet-suite))

(defun run-tests (&optional (suites *suites*))
  "Run SUITES, reporting each; true when all of them pass."
  (let ((results (mapcar #'fiveam:run! suites)))
    (every #'identity results)))
