;;; Load a corpus of HOL4 sources with the native static checker (the
;;; default), whose elaboration drives code generation, then run some of it.
;;; Run through scripts/test-hol.

(ql:quickload :cl-sml)

(defpackage #:cl-sml-hol-corpus
  (:use #:cl #:cl-sml))

(in-package #:cl-sml-hol-corpus)

(defparameter *package-name* "SML.HOL-CORPUS")

;;; In load order: the HOL sources (portableML, Holmake's file system layer,
;;; prekernel, src/0) that the checker accepts, each after its dependencies.
;;; t/testdata/hol-shims/ stands in for Poly/ML structures and for HOL's
;;; configuration.  This reaches the kernel: src/0 (KernelTypes, Type,
;;; Term, Net) and src/thm (Compute, Thm), which the checks below exercise.
(defparameter *sources*
  '("t/testdata/hol-shims/FixedInt.sml"
    "HOL/src/portableML/mosml/PrettyImpl.sml"
    "HOL/src/portableML/AList.sig"
    "HOL/src/portableML/DArray.sig"
    "HOL/src/portableML/DArray.sml"
    "HOL/src/portableML/DString.sig"
    "HOL/src/portableML/DString.sml"
    "HOL/src/portableML/FlagDB.sig"
    "HOL/src/portableML/HOLsexp_dtype.sml"
    "HOL/src/portableML/HOLsexp_parser.sig"
    "HOL/src/portableML/ISPEG_dtype.sml"
    "HOL/src/portableML/ImplicitGraph.sig"
    "HOL/src/portableML/PEGParse.sig"
    "HOL/src/portableML/PEGParse.sml"
    "HOL/src/portableML/PIntMap.sig"
    "HOL/src/portableML/PIntMap.sml"
    "HOL/src/portableML/Profile.sig"
    "HOL/src/portableML/Redblackmap.sig"
    "HOL/src/portableML/Redblackset.sig"
    "HOL/src/portableML/Redblackset.sml"
    "HOL/src/portableML/Streams.sig"
    "HOL/src/portableML/UC_ASCII_Encode.sig"
    "HOL/src/portableML/UTF8.sig"
    "HOL/src/portableML/UTF8.sml"
    "HOL/src/portableML/UTF8Set.sig"
    "HOL/src/portableML/UnicodeChars.sig"
    "HOL/src/portableML/UniversalType.sig"
    "HOL/src/portableML/UniversalType.sml"
    "HOL/src/portableML/Uref.sig"
    "HOL/src/portableML/Uref.sml"
    "HOL/src/portableML/locn.sig"
    "HOL/src/portableML/locn.sml"
    "HOL/src/portableML/quotation_dtype.sml"
    "HOL/src/portableML/seq.sig"
    "HOL/src/portableML/seq.sml"
    "HOL/src/portableML/monads/optmonad.sig"
    "HOL/src/portableML/monads/optmonad.sml"
    "HOL/src/portableML/monads/readermonad.sig"
    "HOL/src/portableML/monads/readermonad.sml"
    "HOL/src/portableML/monads/stmonad.sig"
    "HOL/src/portableML/monads/stmonad.sml"
    "HOL/src/portableML/poly/Susp.sig"
    "HOL/src/portableML/poly/Susp.sml"
    "HOL/src/portableML/mosml/Exn.sig"
    "HOL/src/portableML/mosml/Exn.sml"
    "HOL/src/portableML/mosml/Arbnumcore.sig"
    "HOL/src/portableML/mosml/Arbnumcore.sml"
    "HOL/src/portableML/mosml/Arbintcore.sig"
    "HOL/src/portableML/mosml/Arbintcore.sml"
    "t/testdata/hol-shims/MLSYSPortable.sml"
    "t/testdata/hol-shims/Systeml.sml"
    "HOL/src/portableML/mosml/CoreReplVARS.sml"
    "HOL/src/portableML/mosml/Unsynchronized.sig"
    "HOL/src/portableML/mosml/Unsynchronized.sml"
    "HOL/src/portableML/mosml/concurrent/Future.sig"
    "HOL/src/portableML/mosml/concurrent/Future.sml"
    "HOL/src/portableML/mosml/concurrent/Lock.sig"
    "HOL/src/portableML/mosml/concurrent/Lock.sml"
    "HOL/src/portableML/mosml/concurrent/Parmap.sig"
    "HOL/src/portableML/mosml/concurrent/Parmap.sml"
    "HOL/src/portableML/mosml/concurrent/RWLock.sig"
    "HOL/src/portableML/mosml/concurrent/RWLock.sml"
    "HOL/src/portableML/mosml/concurrent/Sref.sig"
    "HOL/src/portableML/mosml/concurrent/Sref.sml"
    "HOL/src/portableML/mosml/concurrent/ThreadLocal.sig"
    "HOL/src/portableML/mosml/concurrent/ThreadLocal.sml"
    "HOL/tools-poly/poly/Binarymap.sig"
    "HOL/tools-poly/poly/Binarymap.sml"
    "HOL/tools-poly/poly/Binaryset.sig"
    "HOL/tools-poly/poly/Binaryset.sml"
    "HOL/tools-poly/poly/Help.sig"
    "HOL/tools-poly/poly/Listsort.sig"
    "HOL/tools-poly/poly/Listsort.sml"
    "HOL/tools-poly/poly/quse.sig"
    "HOL/tools/Holmake/hfs/HFS_NameMunge.sig"
    "HOL/tools/Holmake/hfs/HOLFS_dtype.sml"
    "HOL/tools/Holmake/hfs/HOLFileSys.sig"
    "HOL/tools/Holmake/hfs/holpathdb.sig"
    "HOL/tools/Holmake/poly/HFS_NameMunge.sml"
    "HOL/src/prekernel/Count.sig"
    "HOL/src/prekernel/Feedback_dtype.sml"
    "HOL/src/prekernel/FinalNet-sig.sml"
    "HOL/src/prekernel/Globals.sig"
    "HOL/src/prekernel/KernelSig.sig"
    "HOL/src/prekernel/Lexis.sig"
    "HOL/src/prekernel/Listener.sig"
    "HOL/src/prekernel/Nonce.sig"
    "HOL/src/prekernel/Nonce.sml"
    "HOL/src/prekernel/TraceMode.sig"
    "HOL/src/prekernel/TraceMode.sml"
    "HOL/src/prekernel/stringfindreplace.sig"
    "HOL/src/0/Subst.sig"
    "HOL/src/0/Subst.sml"
    "HOL/src/portableML/HOLPP.sig"
    "HOL/src/portableML/HOLPP.sml"
    "HOL/src/portableML/HOLquotation.sig"
    "HOL/src/portableML/HOLquotation.sml"
    "HOL/src/portableML/HOLset.sig"
    "HOL/src/portableML/HOLset.sml"
    "HOL/src/portableML/HOLsexp.sig"
    "HOL/src/portableML/ISPEGParse.sig"
    "HOL/src/portableML/ISPEGParse.sml"
    "HOL/src/portableML/ImplicitGraph.sml"
    "HOL/src/portableML/OldPP.sig"
    "HOL/src/portableML/OldPP.sml"
    "HOL/src/portableML/smpp.sig"
    "HOL/src/portableML/smpp.sml"
    "HOL/src/portableML/monads/errormonad.sig"
    "HOL/tools/Holmake/hfs/HOLFileSys.sml"
    "HOL/src/prekernel/Dep.sig"
    "HOL/src/prekernel/Feedback.sig"
    "HOL/src/portableML/Arbint.sig"
    "HOL/src/portableML/Arbint.sml"
    "HOL/src/portableML/Arbnum.sig"
    "HOL/src/portableML/Arbnum.sml"
    "HOL/src/portableML/Arbrat.sig"
    "HOL/src/portableML/Arbrat.sml"
    "HOL/src/portableML/Portable.sig"
    "HOL/src/portableML/Portable.sml"
    "HOL/src/portableML/Redblackmap.sml"
    "HOL/src/portableML/Streams.sml"
    "HOL/src/portableML/Table.sml"
    "HOL/src/portableML/UC_ASCII_Encode.sml"
    "HOL/src/portableML/monads/errormonad.sml"
    "HOL/src/portableML/monads/seqmonad.sig"
    "HOL/src/portableML/monads/seqmonad.sml"
    "t/testdata/hol-shims/poly-init.sml"
    "HOL/src/prekernel/Globals.sml"
    "HOL/src/prekernel/Lib.sig"
    "HOL/src/portableML/AList.sml"
    "HOL/src/portableML/Chartab.sml"
    "HOL/src/portableML/Inttab.sml"
    "HOL/src/portableML/Symreltab.sml"
    "HOL/src/portableML/Symtab.sml"
    "HOL/src/portableML/UTF8Set.sml"
    "HOL/src/portableML/UnicodeChars.sml"
    "HOL/src/prekernel/Feedback.sml"
    "HOL/src/prekernel/KernelSig.sml"
    "HOL/src/prekernel/Listener.sml"
    "HOL/src/prekernel/Type_dtype.sml"
    "HOL/src/prekernel/stringfindreplace.sml"
    "HOL/src/portableML/FlagDB.sml"
    "HOL/src/portableML/Profile.sml"
    "HOL/src/prekernel/Coding.sig"
    "HOL/src/prekernel/Coding.sml"
    "HOL/src/prekernel/Context.sig"
    "HOL/src/prekernel/FinalContext-sig.sml"
    "HOL/src/prekernel/KNametab.sml"
    "HOL/src/prekernel/Lib.sml"
    "HOL/src/prekernel/Count.sml"
    "HOL/src/prekernel/Dep.sml"
    "HOL/src/prekernel/FinalTag-sig.sml"
    "HOL/src/prekernel/Tag.sig"
    "HOL/src/prekernel/Tag.sml"
    "HOL/src/prekernel/Lexis.sml"
    "HOL/src/prekernel/Context.sml"
    "HOL/src/prekernel/FinalType-sig.sml"
    "HOL/src/prekernel/FinalTerm-sig.sml"
    "HOL/src/prekernel/FinalThm-sig.sml"
    "HOL/src/0/KernelTypes.sml"
    "HOL/src/0/Type.sig"
    "HOL/src/0/Type.sml"
    "HOL/src/0/Term.sig"
    "HOL/src/0/Term.sml"
    "HOL/src/0/Net.sig"
    "HOL/src/0/Net.sml"
    "HOL/src/thm/Compute.sig"
    "HOL/src/thm/Compute.sml"
    "HOL/src/thm/std-thmsig.ML"
    "HOL/src/thm/std-thm.ML"))

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
    ("optmonadReturn"
     "val optmonadReturn = case optmonad.return 42 \"state\" of SOME (_, n) => n | NONE => 0;"
     42)
    ("holppString"
     "val holppString = HOLPP.pp_to_string 70 HOLPP.add_string \"pretty\";"
     "pretty")
    ;; The kernel: types, terms and a theorem.
    ("funType"
     "val funType = #1 (Type.dest_type (Type.--> (Type.alpha, Type.mk_vartype \"'b\")));"
     "fun")
    ("varName"
     "val varName = #1 (Term.dest_var (Term.mk_var (\"x\", Type.alpha)));"
     "x")
    ("absAconv"
     "val absAconv = let val x = Term.mk_var (\"x\", Type.alpha) val y = Term.mk_var (\"y\", Type.alpha) in Term.aconv (Term.mk_abs (x, x)) (Term.mk_abs (y, y)) end;"
     t)
    ("reflTheorem"
     "val reflTheorem = let val x = Term.mk_var (\"x\", Type.alpha) val th = Thm.REFL x val (l, r) = Term.dest_eq (Thm.concl th) in Int.toString (length (Thm.hyp th)) ^ Bool.toString (Term.aconv l r) ^ Type.dest_vartype (Term.type_of l) end;"
     "0true'a")))

(unless (probe-file #P"HOL/README.md")
  (error "HOL checkout is not available at ./HOL; run `git submodule update --init HOL`."))

(defun quietly (thunk)
  (let ((*standard-output* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (funcall thunk)))


(defvar *failures* nil)

(dolist (source *sources*)
  (format t "~&[HOL corpus] ~A~%" source)
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
            (format t "~&[HOL corpus] ~A = ~S~%" name value)
            (unless (equal value expected)
              (format t "  FAILED: expected ~S~%" expected)
              (push name *failures*))))
      (error (condition)
        (format t "  FAILED: ~A: ~A~%" name condition)
        (push name *failures*)))))

(if *failures*
    (error "HOL corpus failures: ~{~A~^, ~}" (reverse *failures*))
    (format t "HOL corpus passed (~D source files, ~D checks).~%"
            (length *sources*) (length *checks*)))
