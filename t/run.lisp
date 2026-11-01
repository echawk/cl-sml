(defpackage #:cl-sml-test-runner
  (:use #:cl)
  (:export #:run-tests))

(in-package #:cl-sml-test-runner)

(defparameter *suites*
  '(cl-sml-tests::cl-sml-parser-suite
    cl-sml-compiler-tests::cl-sml-compiler-suite
    cl-sml-runtime-tests::cl-sml-runtime-suite
    cl-sml-repl-tests::cl-sml-repl-suite
    cl-sml-inspect-tests::cl-sml-inspect-suite
    cl-sml-semantics-tests::cl-sml-semantics-suite
    cl-sml-reader-tests::cl-sml-reader-suite
    cl-sml-typecheck-tests::cl-sml-typecheck-suite))

(defun hamlet-present-p ()
  (probe-file (asdf:system-relative-pathname "cl-sml" "hamlet/hamlet.sml")))

(defun default-suites ()
  "*SUITES*, and the HaMLet suite (HaMLet as the differential oracle) when
the hamlet/ submodule is present."
  (if (hamlet-present-p)
      (append *suites* '(cl-sml-hamlet-tests::cl-sml-hamlet-suite))
      (progn
        (format t "~&Skipping the HaMLet suite: no hamlet/ (run ~
`git submodule update --init hamlet` to test against HaMLet).~%")
        *suites*)))

(defun run-tests (&optional (suites (default-suites)))
  "Run SUITES, reporting each; true when all of them pass."
  (let ((results (mapcar #'fiveam:run! suites)))
    (every #'identity results)))
