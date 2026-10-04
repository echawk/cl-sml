;;; Load a corpus of HOL4 sources with HaMLet as the static checker, so that
;;; HaMLet's elaboration drives code generation, then run some of the code.
;;; Run through ./test-hol.

(load "~/.sbclrc")
(ql:quickload :cl-sml)

(defpackage #:cl-sml-hol-checked
  (:use #:cl #:cl-sml))

(in-package #:cl-sml-hol-checked)

(defparameter *package-name* "SML.HOL-CHECKED")

;;; In load order.  testdata/hol-shims/ stands in for Poly/ML structures.
(defparameter *sources*
  '("HOL/src/portableML/Uref.sig"
    "HOL/src/portableML/Uref.sml"
    "testdata/hol-shims/FixedInt.sml"
    "HOL/src/portableML/mosml/PrettyImpl.sml"
    "HOL/src/portableML/quotation_dtype.sml"
    "HOL/src/portableML/HOLquotation.sig"
    "HOL/src/portableML/HOLquotation.sml"
    "HOL/src/portableML/HOLPP.sig"
    "HOL/src/portableML/HOLPP.sml"
    "HOL/src/portableML/UTF8.sig"
    "HOL/src/portableML/UTF8.sml"
    "HOL/src/portableML/poly/Susp.sig"
    "HOL/src/portableML/poly/Susp.sml"
    "HOL/src/portableML/mosml/Exn.sig"
    "HOL/src/portableML/mosml/Exn.sml"
    "HOL/tools-poly/poly/Binarymap.sig"
    "HOL/tools-poly/poly/Binarymap.sml"
    "HOL/tools-poly/poly/Binaryset.sig"
    "HOL/tools-poly/poly/Binaryset.sml"
    "HOL/src/portableML/Redblackset.sig"
    "HOL/src/portableML/Redblackset.sml"
    "HOL/src/portableML/HOLset.sig"
    "HOL/src/portableML/HOLset.sml"
    "HOL/src/portableML/mosml/concurrent/Sref.sig"
    "HOL/src/portableML/mosml/concurrent/Sref.sml"
    "HOL/src/portableML/mosml/concurrent/Lock.sig"
    "HOL/src/portableML/mosml/concurrent/Lock.sml"
    "HOL/src/portableML/mosml/concurrent/RWLock.sig"
    "HOL/src/portableML/mosml/concurrent/RWLock.sml"
    "HOL/src/portableML/seq.sig"
    "HOL/src/portableML/seq.sml"
    "HOL/src/portableML/monads/optmonad.sig"
    "HOL/src/portableML/monads/optmonad.sml"
    "HOL/src/portableML/monads/readermonad.sig"
    "HOL/src/portableML/monads/readermonad.sml"
    "HOL/src/portableML/monads/stmonad.sig"
    "HOL/src/portableML/monads/stmonad.sml"
    "HOL/src/portableML/mosml/Arbnumcore.sig"
    "HOL/src/portableML/mosml/Arbnumcore.sml"
    "HOL/src/portableML/Arbnum.sig"
    "HOL/src/portableML/Arbnum.sml"
    "HOL/src/portableML/mosml/Arbintcore.sig"
    "HOL/src/portableML/mosml/Arbintcore.sml"
    "HOL/src/portableML/Arbint.sig"
    "HOL/src/portableML/Arbint.sml"
    "HOL/src/portableML/PIntMap.sig"
    "HOL/src/portableML/PIntMap.sml"
    "HOL/src/portableML/smpp.sig"
    "HOL/src/portableML/smpp.sml"
    "HOL/src/portableML/ImplicitGraph.sig"
    "HOL/src/portableML/ImplicitGraph.sml"
    "HOL/src/portableML/HOLsexp_dtype.sml"
    "HOL/src/prekernel/Nonce.sig"
    "HOL/src/prekernel/Nonce.sml"
    "HOL/src/0/Subst.sig"
    "HOL/src/0/Subst.sml"))

;;; SML programs run against the loaded corpus: (name source expected).
(defparameter *checks*
  '(("arbnumProduct"
     "val arbnumProduct = Arbnum.toString (Arbnum.* (Arbnum.fromString \"12345678901234567890\", Arbnum.fromInt 1000));"
     "12345678901234567890000")
    ("arbintDifference"
     "val arbintDifference = Arbint.toString (Arbint.- (Arbint.fromInt 3, Arbint.fromInt 10));"
     "-7i")
    ("redblackMembers"
     "val redblackMembers = Redblackset.listItems (Redblackset.addList (Redblackset.empty Int.compare, [5, 1, 3, 1]));"
     (1 3 5))
    ("holsetMember"
     "val holsetMember = HOLset.member (HOLset.addList (HOLset.empty String.compare, [\"a\", \"b\"]), \"b\");"
     t)
    ("binarymapFind"
     "val binarymapFind = Binarymap.find (Binarymap.insert (Binarymap.mkDict Int.compare, 7, \"seven\"), 7);"
     "seven")
    ("utf8Euro"
     "val utf8Euro = map Char.ord (explode (UTF8.chr 0x20AC));"
     (226 130 172))
    ("holppString"
     "val holppString = HOLPP.pp_to_string 70 HOLPP.add_string \"pretty\";"
     "pretty")))

(unless (probe-file #P"HOL/README.md")
  (error "HOL checkout is not available at ./HOL; run `git submodule update --init HOL`."))

(defun quietly (thunk)
  (let ((*standard-output* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (funcall thunk)))

(quietly #'enable-hamlet-type-checker)

;;; User packages have no Basis structures of their own yet (String.compare
;;; and friends); HaMLet's basis sources provide them on top of the run-time
;;; primitives.
(let ((*sml-type-checker* nil))
  (quietly (lambda ()
             (load-sml-file (asdf:system-relative-pathname
                             "cl-sml" "hamlet/basis/all.sml")
                            :package *package-name*))))

(defvar *failures* nil)

(dolist (source *sources*)
  (format t "~&[HOL checked] ~A~%" source)
  (handler-case (quietly (lambda () (load-sml-file source :package *package-name*)))
    (error (condition)
      (format t "  FAILED: ~A~%" condition)
      (push source *failures*))))

(dolist (check *checks*)
  (destructuring-bind (name program expected) check
    (handler-case
        (with-sml-package (*package-name*)
          (quietly (lambda ()
                     (eval (compile-sml-program-string program
                                                       :package *package-name*))))
          (let ((value (sml-value name *package-name*)))
            (format t "~&[HOL checked] ~A = ~S~%" name value)
            (unless (equal value expected)
              (format t "  FAILED: expected ~S~%" expected)
              (push name *failures*))))
      (error (condition)
        (format t "  FAILED: ~A: ~A~%" name condition)
        (push name *failures*)))))

(if *failures*
    (error "HOL checked corpus failures: ~{~A~^, ~}" (reverse *failures*))
    (format t "HOL checked corpus passed (~D source files, ~D checks).~%"
            (length *sources*) (length *checks*)))
