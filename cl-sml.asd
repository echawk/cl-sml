(asdf:defsystem #:cl-sml
  :description "A compiler for SML syntax into Common Lisp"
  :license "See LICENSE"
  :depends-on (#:esrap #:named-readtables #:trivia)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "runtime")
               (:file "static-facts")
               (:file "parser")
               (:file "compiler")
               (:file "reader")
               (:file "type-checker")
               (:file "repl")
               ;; SML source cl-sml loads into HaMLet at run time.
               (:static-file "hamlet-check.sml"))
  :in-order-to ((test-op (test-op #:cl-sml/tests))))

;;; The HaMLet suite loads HaMLet, which needs a deep control stack, e.g.
;;; `sbcl --control-stack-size 1GB`; see test.sh.
(asdf:defsystem #:cl-sml/tests
  :description "Test suites for cl-sml"
  :depends-on (#:cl-sml #:fiveam)
  :pathname "t/"
  :serial t
  :components ((:file "parser-tests")
               (:file "compiler-tests")
               (:file "runtime-tests")
               (:file "repl-tests")
               (:file "hamlet-tests")
               (:file "run"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:cl-sml-test-runner '#:run-tests)
               (error "cl-sml tests failed"))))
