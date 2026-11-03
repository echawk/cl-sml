(in-package #:cl-sml)

;;; The SML structures of Concurrent ML, compiled into the Basis's package
;;; when this file is compiled (DEFINE-SML-BASIS-EXTENSION, src/basis.lisp):
;;; loading cl-sml/cml makes CML, SyncVar, Mailbox, Multicast and RunCML
;;; visible to every SML program, and to the native checker.  cl-sml.asd
;;; makes these sources inputs of this file's compilation.

(define-sml-basis-extension
  "src/cml/CML-sig.sml"
  "src/cml/CML.sml"
  "src/cml/SYNC_VAR-sig.sml"
  "src/cml/SyncVar.sml"
  "src/cml/MAILBOX-sig.sml"
  "src/cml/Mailbox.sml"
  "src/cml/MULTICAST-sig.sml"
  "src/cml/Multicast.sml"
  "src/cml/RUN_CML-sig.sml"
  "src/cml/RunCML.sml")
