;;; src/basis.lisp compiles the Basis Library's SML sources, src/basis/,
;;; into its FASL; they are inputs of its compilation, so that ASDF
;;; recompiles the basis when one changes.
(defclass cl-sml-basis-file (asdf:cl-source-file)
  ((sml-sources :initarg :sml-sources :initform "basis/*.sml"
                :reader cl-sml-basis-file-sml-sources
                :documentation "The SML sources, relative to the file.")))

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
          (directory (merge-pathnames (cl-sml-basis-file-sml-sources component)
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
               (:file "ir")
               (:file "inspect")
               (:file "reader")
               ;; The native checker (docs/DESIGN-TYPECHECKER.md).
               (:module "types"
                :serial t
                :components ((:file "objects")
                             (:file "unify")
                             (:file "env")
                             (:file "print")
                             (:file "initial")
                             (:file "scope-tyvars")
                             (:file "check-pattern")
                             (:file "elab-core")
                             (:file "sig")
                             (:file "elab-module")
                             (:file "defunctorize")
                             (:file "initial-basis")
                             (:file "annotations")
                             (:file "typecheck")))
               (:file "ir-types")
               (:file "type-checker")
               (:file "repl")
               (:cl-sml-basis-file "basis"))
  :in-order-to ((test-op (test-op #:cl-sml/tests))))

;;; HaMLet (https://github.com/rossberg/hamlet, the hamlet/ submodule) as a
;;; static checker: the differential oracle of the native checker, for tests
;;; only.  Loading HaMLet needs a deep control stack, e.g.
;;; `sbcl --control-stack-size 1GB`; see test.sh.
(asdf:defsystem #:cl-sml/hamlet-oracle
  :description "HaMLet as cl-sml's static checker, the native checker's oracle"
  :license "See LICENSE"
  :depends-on (#:cl-sml)
  :pathname "t/hamlet-oracle/"
  :components ((:file "hamlet-oracle")
               ;; SML source the oracle loads into HaMLet at run time.
               (:static-file "hamlet-check.sml")))

;;; The HaMLet suite runs only when the hamlet/ submodule is present.
(asdf:defsystem #:cl-sml/tests
  :description "Test suites for cl-sml"
  :depends-on (#:cl-sml #:cl-sml/hamlet-oracle #:fiveam)
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
               (:file "typecheck-tests")
               (:file "run"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:cl-sml-test-runner '#:run-tests)
               (error "cl-sml tests failed"))))

;;; Concurrent ML (docs/DESIGN-CML.md): the structures CML, SyncVar, Mailbox,
;;; Multicast and RunCML, on OS threads.  Only this system depends on
;;; bordeaux-threads; cl-sml itself does not.  Its basis.lisp compiles the
;;; SML sources of src/cml/ into its FASL, as src/basis.lisp does the Basis's.

(asdf:defsystem #:cl-sml/cml
  :description "Concurrent ML for cl-sml"
  :license "See LICENSE"
  :depends-on (#:cl-sml #:bordeaux-threads)
  :pathname "src/cml/"
  :serial t
  :components ((:file "package")
               (:file "scheduler")
               (:file "core")
               (:file "primitives")
               (:cl-sml-basis-file "basis" :sml-sources "*.sml"))
  :in-order-to ((test-op (test-op #:cl-sml/cml-tests))))

(asdf:defsystem #:cl-sml/cml-tests
  :description "Tests of cl-sml's Concurrent ML"
  :depends-on (#:cl-sml/cml #:fiveam)
  :pathname "t/cml/"
  :components ((:file "cml-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:cl-sml-cml-tests '#:run-cml-tests)
               (error "cl-sml CML tests failed"))))

;;; CML through the Coalton backend.
(asdf:defsystem #:cl-sml/cml-coalton-tests
  :description "Tests of cl-sml's Concurrent ML through the Coalton backend"
  :depends-on (#:cl-sml/cml-tests #:cl-sml/coalton)
  :pathname "t/cml/"
  :components ((:file "cml-coalton-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:cl-sml-cml-tests '#:run-cml-coalton-tests)
               (error "cl-sml CML Coalton tests failed"))))

;;; The optional Coalton backend (docs/IR.md, "The Coalton backend"): SML
;;; programs compiled to Coalton through the typed IR.  Only this system
;;; depends on Coalton; cl-sml itself does not.
(asdf:defsystem #:cl-sml/coalton
  :description "cl-sml's Coalton backend"
  :license "See LICENSE"
  :depends-on (#:cl-sml #:coalton)
  :pathname "src/coalton/"
  :serial t
  :components ((:file "prelude")
               (:file "emit")
               (:file "basis"))
  :in-order-to ((test-op (test-op #:cl-sml/coalton-tests))))

(asdf:defsystem #:cl-sml/coalton-tests
  :description "Tests of cl-sml's Coalton backend"
  :depends-on (#:cl-sml/coalton #:fiveam)
  :pathname "t/"
  :components ((:file "coalton-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:cl-sml-coalton-tests '#:run-coalton-tests)
               (error "cl-sml Coalton backend tests failed"))))
