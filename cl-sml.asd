;;; src/basis.lisp compiles the Basis Library's SML sources, src/basis/,
;;; into its FASL; they are inputs of its compilation, so that ASDF
;;; recompiles the basis when one changes.
(defclass cl-sml-basis-file (asdf:cl-source-file) ())

;;; The Lisp generated from SML draws full warnings from the compiler (e.g.
;;; trivia's on patterns it cannot type), which do not make it unusable;
;;; cl-sml's own loading of SML ignores them too.
(defmethod asdf:perform :around ((operation asdf:compile-op)
                                 (component cl-sml-basis-file))
  (let ((asdf:*compile-file-failure-behaviour* :warn))
    (call-next-method)))

(defmethod asdf:input-files ((operation asdf:compile-op)
                             (component cl-sml-basis-file))
  (append (call-next-method)
          (directory (merge-pathnames "basis/*.sml"
                                      (asdf:component-pathname component)))))

(asdf:defsystem #:cl-sml
  :description "A compiler for SML syntax into Common Lisp"
  :license "See LICENSE"
  :depends-on (#:named-readtables #:trivia)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "portability")
               (:file "runtime")
               (:file "static-facts")
               (:file "lexer")
               (:file "parser")
               (:file "compiler")
               (:file "inspect")
               (:file "reader")
               (:file "type-checker")
               (:file "repl")
               (:cl-sml-basis-file "basis")
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
               (:file "inspect-tests")
               (:file "hamlet-tests")
               (:file "semantics-tests")
               (:file "reader-tests")
               (:file "run"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:cl-sml-test-runner '#:run-tests)
               (error "cl-sml tests failed"))))
