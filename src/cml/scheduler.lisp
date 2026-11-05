(in-package #:cl-sml.cml)

;;; The scheduler protocol: everything the event protocol (core.lisp) needs
;;; from the host to run threads and to block and wake them.  The event
;;; protocol is Reppy's single-processor one: polling and committing run in
;;; an atomic region (CALL-ATOMICALLY), and a thread that finds nothing
;;; enabled leaves a waiter on every object it waits for and blocks
;;; (BLOCK-WAITER) until a partner commits it and wakes it (WAKE-WAITER).
;;;
;;; OS-THREAD-SCHEDULER, the only implementation, runs one OS thread per
;;; CML thread (bordeaux-threads); its atomic region is one global lock and
;;; a waiter is a condition variable.  A green-thread scheduler would
;;; implement the same generic functions over continuations on one OS
;;; thread: docs/DESIGN-CML.md, section 6, says what it needs from the
;;; compiler.

(defclass scheduler () ()
  (:documentation "Runs CML threads; see the generic functions below."))

(defgeneric call-atomically (scheduler thunk)
  (:documentation "Call THUNK in the atomic region: no other CML thread
polls, commits or changes CML state meanwhile.  THUNK runs no SML code and
does not call CALL-ATOMICALLY again."))

(defgeneric make-waiter (scheduler)
  (:documentation "A new waiter: what one thread blocks on."))

(defgeneric block-waiter (scheduler waiter deadline)
  (:documentation "Called in the atomic region: leave it, wait until WAITER
is woken or DEADLINE (an internal real time, or NIL for none) passes, and
enter it again.  May return early; the caller checks why it returned."))

(defgeneric wake-waiter (scheduler waiter)
  (:documentation "Called in the atomic region: make the thread blocked on
WAITER return from BLOCK-WAITER."))

(defgeneric start-thread (scheduler thread function)
  (:documentation "Run FUNCTION (of no arguments) in a new thread of
control, in which CURRENT-THREAD returns THREAD.  Called in the atomic
region; FUNCTION may start before it is left.  Returns a handle for
STOP-THREAD."))

(defgeneric current-thread (scheduler)
  (:documentation "The CML thread (core.lisp's THREAD) running the caller, or
NIL outside CML threads."))

(defgeneric yield-thread (scheduler)
  (:documentation "Let other threads run (CML.yield)."))

(defgeneric stop-thread (scheduler handle)
  (:documentation "Stop the thread START-THREAD returned HANDLE for, which
has not ended although RunCML has shut down (it computes, or blocks outside
CML).  Asynchronous."))

;;; One OS thread per CML thread.

(defclass os-thread-scheduler (scheduler)
  ((lock :initform (bt:make-lock "CML") :reader scheduler-lock)
   (stack-size :initarg :stack-size :initform nil :accessor scheduler-stack-size
               :documentation "The control stack size of new threads in
bytes, or NIL for the Lisp's default (SBCL: its --control-stack-size).
Only SBCL honours it."))
  (:documentation "One OS thread per CML thread, under one global lock."))

(defvar *current-thread* nil
  "The CML thread of this OS thread (OS-THREAD-SCHEDULER).")

(defmethod call-atomically ((scheduler os-thread-scheduler) thunk)
  (bt:with-lock-held ((scheduler-lock scheduler))
    (funcall thunk)))

(defmethod make-waiter ((scheduler os-thread-scheduler))
  (bt:make-condition-variable :name "CML waiter"))

(defmethod block-waiter ((scheduler os-thread-scheduler) waiter deadline)
  (if deadline
      (let ((remaining (- deadline (get-internal-real-time))))
        (when (plusp remaining)
          (bt:condition-wait waiter (scheduler-lock scheduler)
                             :timeout (/ remaining
                                         (float internal-time-units-per-second 1d0)))))
      (bt:condition-wait waiter (scheduler-lock scheduler))))

(defmethod wake-waiter ((scheduler os-thread-scheduler) waiter)
  (bt:condition-notify waiter))

(defmethod start-thread ((scheduler os-thread-scheduler) thread function)
  (flet ((run ()
           (let ((*current-thread* thread))
             (funcall function))))
    (cl-sml::call-with-portable-thread-stack-size
     (scheduler-stack-size scheduler)
     (lambda () (bt:make-thread #'run :name "CML thread")))))

(defmethod current-thread ((scheduler os-thread-scheduler))
  *current-thread*)

(defmethod yield-thread ((scheduler os-thread-scheduler))
  (bt:thread-yield))

(defmethod stop-thread ((scheduler os-thread-scheduler) handle)
  (when (and handle (bt:thread-alive-p handle))
    (ignore-errors (bt:destroy-thread handle))))

(defvar *scheduler* (make-instance 'os-thread-scheduler)
  "The scheduler CML runs on.")
