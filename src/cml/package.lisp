;;; Concurrent ML for cl-sml (docs/DESIGN-CML.md): the optional system
;;; cl-sml/cml.  The Lisp core (this directory's .lisp files) implements
;;; threads, channels, events and RunCML; the SML structures CML, SyncVar,
;;; Mailbox, Multicast and RunCML (this directory's .sml files) extend the
;;; Basis with them.

(defpackage #:cl-sml.cml
  (:use #:cl)
  (:export
   ;; The scheduler protocol (scheduler.lisp).
   #:scheduler
   #:os-thread-scheduler
   #:*scheduler*
   #:call-atomically
   #:make-waiter
   #:block-waiter
   #:wake-waiter
   #:start-thread
   #:current-thread
   #:yield-thread
   #:stop-thread
   ;; Configuration.
   #:seed-choice
   #:*inherited-specials*
   #:*shutdown-grace-seconds*
   #:*report-uncaught-exceptions*))
