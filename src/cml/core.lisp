(in-package #:cl-sml.cml)

;;; The CML core: threads, events and their synchronisation, channels, the
;;; base events of SyncVar and Mailbox, and RunCML.  The protocol is
;;; Reppy's for one processor (Concurrent Programming in ML, chapter 10):
;;; SYNC forces the guards, then in the atomic region polls every base
;;; event and commits to an enabled one, or leaves a waiter on each and
;;; blocks.  Wrap functions and handlers run after the atomic region, in
;;; the synchronising thread.  Everything here that touches CML state runs
;;; in the atomic region (ATOMICALLY).

(defmacro atomically (&body body)
  `(call-atomically *scheduler* (lambda () ,@body)))

;;; SML values the primitives build or take.

(declaim (inline sml-unit))
(defun sml-unit () (cl-sml:sml-unit))

(defun sml-fail (format-control &rest arguments)
  "Raise the SML exception Fail with a message."
  (cl-sml::sml-raise (funcall (cl-sml::sml-initial-exception "Fail")
                              (apply #'format nil format-control arguments))))

;;; Random choice among enabled events: a xorshift64* generator, the same
;;; on every Lisp, so that SEED-CHOICE makes the choices reproducible (given
;;; the same set of enabled events, which the OS scheduling also decides).

(defvar *choice-state* (logior 1 (ldb (byte 64 0) (+ (get-universal-time)
                                                      (* 7919 (get-internal-real-time))))))

(defun seed-choice (seed)
  "Make the choices among enabled events start again from SEED, an integer."
  (setf *choice-state* (logior 1 (ldb (byte 64 0) (* (1+ (abs seed)) #x9E3779B97F4A7C15))))
  seed)

(defun random-index (n)
  "A pseudo-random integer below N; called in the atomic region."
  (let ((x *choice-state*))
    (setf x (logxor x (ash x -12))
          x (logxor x (ldb (byte 64 0) (ash x 25)))
          x (logxor x (ash x -27))
          *choice-state* x)
    (mod (ash (ldb (byte 64 0) (* x #x2545F4914F6CDD1D)) -32) n)))

;;; Instances of RunCML.doit and their threads.

(defvar *shutdown-grace-seconds* 1
  "How long RunCML.doit waits, after a shutdown or a deadlock, for threads
to end before it stops them (STOP-THREAD).  Blocked threads end at once.")

(defvar *report-uncaught-exceptions* t
  "Whether a thread that ends by an uncaught exception reports it on the
error output, as SML/NJ's and MLton's CML do.")

(defvar *inherited-specials*
  '(*standard-output* *standard-input* *error-output* *trace-output*
    *query-io* *debug-io* *terminal-io* *default-pathname-defaults*
    *package* *readtable* cl-sml::*sml-package* cl-sml::*sml-current-directory*)
  "The special variables whose values a new thread takes from the thread
that spawns it: a new OS thread otherwise sees their global values, not
the bindings of, e.g., the REPL or a test harness.")

(defstruct (instance (:constructor make-instance-state ()))
  "One run of RunCML.doit."
  (live 0)        ; threads that have not ended
  (blocked 0)     ; threads blocked in SYNC with no deadline
  (timed 0)       ; threads blocked in SYNC until a deadline
  (finished nil)  ; set by shutdown or deadlock
  (status 1)      ; doit's result: OS.Process.status (0 success, 1 failure)
  (threads nil)   ; the live threads
  (next-id 0)
  (waiter nil))   ; RunCML.doit's

(defvar *instance* nil "The running instance of RunCML.doit, or NIL.")

(defstruct (thread (:constructor make-thread-state (id instance)))
  "A CML thread, CML.thread_id."
  id
  instance
  (done nil)
  (joiners nil)   ; waiting entries of joinEvt
  (props nil)     ; alist of thread properties
  (waiter nil)
  (handle nil))

(defun this-thread (operation)
  "The calling CML thread; raises Fail outside one."
  (or (current-thread *scheduler*)
      (sml-fail "~A: not called from a CML thread (see RunCML.doit)" operation)))

(defun exit-thread ()
  "End the calling thread (CML.exit); also how shutdown unwinds threads."
  (throw 'thread-exit nil))

(defun check-running (thread)
  "Unwind THREAD if its instance has finished.  In the atomic region."
  (when (instance-finished (thread-instance thread))
    (exit-thread)))

(defun finish-instance (instance status)
  "End INSTANCE (shutdown or deadlock): wake doit and every blocked thread.
In the atomic region."
  (unless (instance-finished instance)
    (setf (instance-finished instance) t
          (instance-status instance) status)
    (wake-waiter *scheduler* (instance-waiter instance))
    (dolist (thread (instance-threads instance))
      (wake-waiter *scheduler* (thread-waiter thread)))))

(defun check-deadlock (instance)
  "Finish INSTANCE with failure when no thread can run and none waits for
a deadline.  In the atomic region."
  (when (and (= (instance-live instance) (instance-blocked instance))
             (zerop (instance-timed instance)))
    (finish-instance instance 1)))

(defun report-uncaught (thread condition)
  (when *report-uncaught-exceptions*
    (ignore-errors
     (format *error-output* "~&CML: thread ~A ended by ~A~%"
             (thread-name thread) condition)
     (finish-output *error-output*))))

(defun thread-name (thread)
  (format nil "[~4,'0D]" (thread-id thread)))

(defun spawn (function)
  "CML.spawn: run the SML function FUNCTION (unit -> unit) in a new thread."
  (let* ((parent (this-thread "CML.spawn"))
         (instance (thread-instance parent))
         (specials *inherited-specials*)
         (values (mapcar #'symbol-value specials))
         (thread nil))
    (atomically
      (check-running parent)
      (setf thread (spawn-thread instance specials values function)))
    thread))

(defun spawn-thread (instance specials values function)
  "A new thread of INSTANCE running FUNCTION with SPECIALS bound to VALUES.
In the atomic region."
  (let ((thread (make-thread-state (instance-next-id instance) instance)))
    (incf (instance-next-id instance))
    (incf (instance-live instance))
    (push thread (instance-threads instance))
    (setf (thread-waiter thread) (make-waiter *scheduler*))
    (setf (thread-handle thread)
          (start-thread *scheduler* thread
                        (lambda ()
                          (progv specials values
                            (run-thread thread function)))))
    thread))

(defun run-thread (thread function)
  (unwind-protect
       (catch 'thread-exit
         (handler-case (cl-sml::with-sml-exceptions-as-conditions
                         (funcall function (sml-unit)))
           (cl-sml::sml-raised-exception (condition)
             (report-uncaught thread condition))
           (serious-condition (condition)
             (report-uncaught thread condition))))
    (atomically (end-thread thread))))

(defun end-thread (thread)
  "THREAD has ended.  In the atomic region."
  (let ((instance (thread-instance thread)))
    (setf (thread-done thread) t)
    (decf (instance-live instance))
    (setf (instance-threads instance) (delete thread (instance-threads instance)))
    (dolist (entry (shiftf (thread-joiners thread) nil))
      (when (entry-live-p entry)
        (commit-entry entry (sml-unit))))
    (cond ((instance-finished instance)
           (when (zerop (instance-live instance))
             (wake-waiter *scheduler* (instance-waiter instance))))
          ((zerop (instance-live instance))
           ;; Every thread ended without RunCML.shutdown.
           (finish-instance instance 1))
          (t (check-deadlock instance)))))

(defun run-cml (function)
  "RunCML.doit: run the SML function FUNCTION (unit -> unit) as the first
thread, until shutdown or deadlock; return the status."
  (install-thread-safety)
  (let ((instance (make-instance-state))
        (specials *inherited-specials*))
    (setf (instance-waiter instance) (make-waiter *scheduler*))
    (atomically
      (when *instance*
        (sml-fail "RunCML.doit: CML is already running"))
      (setf *instance* instance))
    (unwind-protect
         (progn
           (atomically
             (spawn-thread instance specials (mapcar #'symbol-value specials) function)
             (loop until (instance-finished instance)
                   do (block-waiter *scheduler* (instance-waiter instance) nil))
             ;; Let the threads unwind.
             (loop with deadline = (+ (get-internal-real-time)
                                      (* *shutdown-grace-seconds*
                                         internal-time-units-per-second))
                   until (or (zerop (instance-live instance))
                             (>= (get-internal-real-time) deadline))
                   do (block-waiter *scheduler* (instance-waiter instance) deadline)))
           (instance-status instance))
      (let ((handles nil))
        (atomically
          (finish-instance instance (instance-status instance))
          (setf handles (mapcar #'thread-handle (instance-threads instance)))
          (setf *instance* nil))
        (dolist (handle handles)
          (stop-thread *scheduler* handle))))))

(defun shutdown (status)
  "RunCML.shutdown."
  (let ((thread (this-thread "RunCML.shutdown")))
    (atomically (finish-instance (thread-instance thread) status))
    (exit-thread)))

(defun running-p ()
  "RunCML.isRunning."
  (let ((instance *instance*))
    (and instance (not (instance-finished instance)))))

;;; Thread properties (CML.newThreadProp, CML.newThreadFlag).

(defvar *prop-keys* 0)

(defun new-prop-key ()
  (atomically (incf *prop-keys*)))

(defun prop-cell (key)
  (assoc key (thread-props (this-thread "CML thread property"))))

;;; Events.  SML's 'a event is an EVENT: a tree that SYNC forces into
;;; BRANCHes, one per base event.

(defstruct event)
(defstruct (base-event (:include event)))
(defstruct (wrap-event (:include event)) event function)
(defstruct (handler-event (:include event)) event handler)
(defstruct (choose-event (:include event)) events)
(defstruct (guard-event (:include event)) thunk)
(defstruct (nack-event (:include event)) function)

(defstruct branch
  base     ; the BASE-EVENT
  post     ; NIL, or the function of its value that the wraps make
  nacks)   ; the CVARs of the withNacks around it

;;; Base events implement three generic functions, all in the atomic
;;; region but FORCE-BASE.

(defgeneric force-base (base)
  (:documentation "The base event SYNC polls for BASE, made when it is
synchronised on (a time-out starts then).")
  (:method ((base base-event)) base))

(defgeneric poll-base (base)
  (:documentation "Is BASE enabled?"))

(defgeneric commit-base (base)
  (:documentation "Commit to BASE, which POLL-BASE found enabled; return
its value."))

(defgeneric block-base (base entry)
  (:documentation "Leave ENTRY, a waiting ENTRY of a blocked
synchronisation, where whatever enables BASE will find it.")
  (:method ((base base-event) entry) (declare (ignore entry)) nil))

(defgeneric base-deadline (base)
  (:documentation "The internal real time at which BASE becomes enabled by
itself, or NIL.")
  (:method ((base base-event)) nil))

(defun force-event (event nacks)
  "The branches of EVENT, running its guards and withNack functions; the
CVARs of the withNacks go to the cdr of NACKS."
  (etypecase event
    (base-event (list (make-branch :base (force-base event) :post nil :nacks nil)))
    (wrap-event
     (let ((function (wrap-event-function event))
           (branches (force-event (wrap-event-event event) nacks)))
       (dolist (branch branches branches)
         (let ((post (branch-post branch)))
           (setf (branch-post branch)
                 (if post
                     (lambda (value) (funcall function (funcall post value)))
                     function))))))
    (handler-event
     (let ((handler (handler-event-handler event))
           (branches (force-event (handler-event-event event) nacks)))
       (dolist (branch branches branches)
         (let ((post (branch-post branch)))
           (when post
             (setf (branch-post branch)
                   (lambda (value)
                     (handler-case (cl-sml::with-sml-exceptions-as-conditions
                                     (funcall post value))
                       (cl-sml::sml-raised-exception (condition)
                         (funcall handler (cl-sml::sml-exception-value condition)))))))))))
    (choose-event
     (loop for event in (choose-event-events event)
           append (force-event event nacks)))
    (guard-event (force-event (funcall (guard-event-thunk event) (sml-unit)) nacks))
    (nack-event
     (let* ((cvar (make-cvar))
            (branches (force-event (funcall (nack-event-function event)
                                            (make-cvar-event :cvar cvar))
                                   nacks)))
       (push cvar (cdr nacks))
       (dolist (branch branches branches)
         (push cvar (branch-nacks branch)))))))

;;; A blocked synchronisation leaves one ENTRY per branch; a partner that
;;; commits one commits the whole transaction (TXN).  Entries of a
;;; committed transaction stay where they are and are skipped (and dropped)
;;; when found.

(defstruct txn
  thread
  (state :waiting)   ; or :synched
  index value        ; the branch committed and its value
  branches           ; vector of the BRANCHes
  nacks              ; the CVARs of all withNacks of the synchronisation
  deadline)

(defstruct entry txn index value)

(defun entry-live-p (entry)
  (eq (txn-state (entry-txn entry)) :waiting))

(defun set-nacks (nacks chosen)
  "Set the CVARs in NACKS that are not around the branch CHOSEN (NIL: set
all)."
  (dolist (cvar nacks)
    (unless (and chosen (member cvar (branch-nacks chosen)))
      (set-cvar cvar))))

(defun commit-entry (entry value)
  "A partner commits the transaction of ENTRY, a live entry, to its branch,
with VALUE."
  (let* ((txn (entry-txn entry))
         (thread (txn-thread txn))
         (instance (thread-instance thread)))
    (setf (txn-state txn) :synched
          (txn-index txn) (entry-index entry)
          (txn-value txn) value)
    (if (txn-deadline txn)
        (decf (instance-timed instance))
        (decf (instance-blocked instance)))
    (set-nacks (txn-nacks txn) (aref (txn-branches txn) (entry-index entry)))
    (wake-waiter *scheduler* (thread-waiter thread))))

;;; Queues of entries (and of messages), FIFO.

(defstruct (queue (:constructor make-queue ())) (head nil) (tail nil))

(defun enqueue (queue item)
  (let ((cell (list item)))
    (if (queue-head queue)
        (setf (cdr (queue-tail queue)) cell)
        (setf (queue-head queue) cell))
    (setf (queue-tail queue) cell)
    item))

(defun dequeue (queue)
  (let ((cell (queue-head queue)))
    (when cell
      (setf (queue-head queue) (cdr cell))
      (unless (cdr cell) (setf (queue-tail queue) nil))
      (car cell))))

(defun queue-empty-p (queue)
  (null (queue-head queue)))

(defun first-live-entry (queue)
  "The first live entry of QUEUE, dropping the dead ones before it."
  (loop for head = (queue-head queue)
        while head
        do (if (entry-live-p (car head))
               (return (car head))
               (dequeue queue))))

(defun enqueue-entry (queue entry)
  "Add ENTRY to QUEUE, first dropping its dead entries, so that a queue
only ever polled (a recvEvt chosen against a time-out) does not grow."
  (let ((live (remove-if-not #'entry-live-p (queue-head queue))))
    (setf (queue-head queue) live
          (queue-tail queue) (last live)))
  (enqueue queue entry))

;;; SYNC.

(defun sync (event)
  "CML.sync."
  (let ((thread (this-thread "CML.sync"))
        (nacks (list nil)))
    (let* ((branches (coerce (force-event event nacks) 'simple-vector))
           (all-nacks (cdr nacks))
           (index nil)
           (value nil))
      (atomically
        (check-running thread)
        (let ((enabled (loop for i from 0 below (length branches)
                             when (poll-base (branch-base (aref branches i)))
                               collect i)))
          (cond (enabled
                 (setf index (nth (random-index (length enabled)) enabled)
                       value (commit-base (branch-base (aref branches index))))
                 (set-nacks all-nacks (aref branches index)))
                (t
                 (multiple-value-setq (index value)
                   (block-sync thread branches all-nacks))))))
      (let ((post (branch-post (aref branches index))))
        (if post (funcall post value) value)))))

(defun block-sync (thread branches nacks)
  "Block THREAD until a partner commits one of BRANCHES, none of which is
enabled, or a deadline passes.  Return the branch's index and value.  In
the atomic region."
  (let* ((instance (thread-instance thread))
         (deadline (let ((deadlines (remove nil (map 'list (lambda (branch)
                                                             (base-deadline (branch-base branch)))
                                                           branches))))
                     (and deadlines (reduce #'min deadlines))))
         (txn (make-txn :thread thread :branches branches :nacks nacks
                        :deadline deadline)))
    (loop for i from 0 below (length branches)
          do (block-base (branch-base (aref branches i))
                         (make-entry :txn txn :index i)))
    (if deadline
        (incf (instance-timed instance))
        (incf (instance-blocked instance)))
    (check-deadlock instance)
    (loop
      (when (eq (txn-state txn) :synched)
        (return (values (txn-index txn) (txn-value txn))))
      (when (instance-finished instance)
        (setf (txn-state txn) :synched)
        (exit-thread))
      (when (and deadline (>= (get-internal-real-time) deadline))
        ;; The earliest time-out fires.
        (let ((index (position deadline branches
                               :key (lambda (branch) (base-deadline (branch-base branch))))))
          (setf (txn-state txn) :synched)
          (decf (instance-timed instance))
          (set-nacks nacks (aref branches index))
          (return (values index (sml-unit)))))
      (block-waiter *scheduler* (thread-waiter thread) deadline))))

(defun poll-event (event some none)
  "Synchronise on EVENT if one of its base events is enabled now: the SML
SOME of its value, or NONE (CML.recvPoll and the other polls)."
  (let* ((nacks (list nil))
         (branches (coerce (force-event event nacks) 'simple-vector))
         (all-nacks (cdr nacks))
         (index nil)
         (value nil))
    (atomically
      (let ((enabled (loop for i from 0 below (length branches)
                           when (poll-base (branch-base (aref branches i)))
                             collect i)))
        (when enabled
          (setf index (nth (random-index (length enabled)) enabled)
                value (commit-base (branch-base (aref branches index)))))
        (set-nacks all-nacks (and index (aref branches index)))))
    (if index
        (let ((post (branch-post (aref branches index))))
          (funcall some (if post (funcall post value) value)))
        none)))

;;; Base events.

(defstruct (always-event (:include base-event)) value)
(defmethod poll-base ((base always-event)) t)
(defmethod commit-base ((base always-event)) (always-event-value base))

(defstruct (never-event (:include base-event)))
(defmethod poll-base ((base never-event)) nil)

;;; Time-outs: timeOutEvt and atTimeEvt become a DEADLINE-EVENT when
;;; synchronised on.

(defstruct (time-out-event (:include base-event)) nanoseconds)
(defstruct (at-time-event (:include base-event)) nanoseconds)
(defstruct (deadline-event (:include base-event)) deadline)

(defun deadline-after (nanoseconds)
  (+ (get-internal-real-time)
     (max 0 (ceiling (* nanoseconds internal-time-units-per-second) 1000000000))))

(defmethod force-base ((base time-out-event))
  (make-deadline-event :deadline (deadline-after (time-out-event-nanoseconds base))))

(defmethod force-base ((base at-time-event))
  (make-deadline-event
   :deadline (deadline-after (- (at-time-event-nanoseconds base)
                                (cl-sml::sml-time-now (sml-unit))))))

(defmethod poll-base ((base deadline-event))
  (>= (get-internal-real-time) (deadline-event-deadline base)))
(defmethod commit-base ((base deadline-event)) (sml-unit))
(defmethod base-deadline ((base deadline-event)) (deadline-event-deadline base))

;;; joinEvt.

(defstruct (join-event (:include base-event)) thread)
(defmethod poll-base ((base join-event)) (thread-done (join-event-thread base)))
(defmethod commit-base ((base join-event)) (sml-unit))
(defmethod block-base ((base join-event) entry)
  (push entry (thread-joiners (join-event-thread base))))

;;; Condition variables: the nack events of withNack.

(defstruct cvar (set nil) (waiting nil))

(defun set-cvar (cvar)
  (unless (cvar-set cvar)
    (setf (cvar-set cvar) t)
    (dolist (entry (shiftf (cvar-waiting cvar) nil))
      (when (entry-live-p entry)
        (commit-entry entry (sml-unit))))))

(defstruct (cvar-event (:include base-event)) cvar)
(defmethod poll-base ((base cvar-event)) (cvar-set (cvar-event-cvar base)))
(defmethod commit-base ((base cvar-event)) (sml-unit))
(defmethod block-base ((base cvar-event) entry)
  (push entry (cvar-waiting (cvar-event-cvar base))))

;;; Channels.

(defstruct (channel (:constructor make-channel ()))
  (senders (make-queue))     ; entries whose value is the message
  (receivers (make-queue)))

(defstruct (send-event (:include base-event)) channel message)
(defstruct (recv-event (:include base-event)) channel)

(defmethod poll-base ((base send-event))
  (first-live-entry (channel-receivers (send-event-channel base))))

(defmethod commit-base ((base send-event))
  (commit-entry (dequeue (channel-receivers (send-event-channel base)))
                (send-event-message base))
  (sml-unit))

(defmethod block-base ((base send-event) entry)
  (setf (entry-value entry) (send-event-message base))
  (enqueue-entry (channel-senders (send-event-channel base)) entry))

(defmethod poll-base ((base recv-event))
  (first-live-entry (channel-senders (recv-event-channel base))))

(defmethod commit-base ((base recv-event))
  (let ((entry (dequeue (channel-senders (recv-event-channel base)))))
    (commit-entry entry (sml-unit))
    (entry-value entry)))

(defmethod block-base ((base recv-event) entry)
  (enqueue-entry (channel-receivers (recv-event-channel base)) entry))

;;; SyncVar: I-variables (written once) and M-variables.  A reader of an
;;; empty variable waits in READERS (iGet, mGet) or TAKERS (mTake, mSwap,
;;; whose entry's value is the new contents).

(defstruct (syncvar (:constructor make-syncvar ()))
  (full nil) (value nil)
  (readers (make-queue))
  (takers (make-queue)))

(defun syncvar-fill (var value)
  "Put VALUE in VAR, which is empty: wake its readers, then its first taker
(which empties it again, or for mSwap fills it with the new value).  In the
atomic region."
  (loop
    (loop for entry = (first-live-entry (syncvar-readers var))
          while entry
          do (commit-entry (dequeue (syncvar-readers var)) value))
    (let ((taker (first-live-entry (syncvar-takers var))))
      (cond ((null taker)
             (setf (syncvar-full var) t
                   (syncvar-value var) value)
             (return))
            (t (dequeue (syncvar-takers var))
               (commit-entry taker value)
               (if (eq (entry-value taker) '%take)
                   (return)
                   (setf value (entry-value taker))))))))

(defun syncvar-put (var value)
  "iPut and mPut: false when VAR is full (SyncVar.Put)."
  (atomically
    (unless (syncvar-full var)
      (syncvar-fill var value)
      t)))

(defstruct (syncvar-read-event (:include base-event)) var)
(defstruct (syncvar-take-event (:include base-event)) var (new '%take))

(defmethod poll-base ((base syncvar-read-event)) (syncvar-full (syncvar-read-event-var base)))
(defmethod commit-base ((base syncvar-read-event)) (syncvar-value (syncvar-read-event-var base)))
(defmethod block-base ((base syncvar-read-event) entry)
  (enqueue-entry (syncvar-readers (syncvar-read-event-var base)) entry))

(defmethod poll-base ((base syncvar-take-event)) (syncvar-full (syncvar-take-event-var base)))
(defmethod commit-base ((base syncvar-take-event))
  (let* ((var (syncvar-take-event-var base))
         (value (syncvar-value var))
         (new (syncvar-take-event-new base)))
    (setf (syncvar-full var) nil
          (syncvar-value var) nil)
    (unless (eq new '%take)
      (syncvar-fill var new))
    value))
(defmethod block-base ((base syncvar-take-event) entry)
  (setf (entry-value entry) (syncvar-take-event-new base))
  (enqueue-entry (syncvar-takers (syncvar-take-event-var base)) entry))

;;; Mailbox: asynchronous channels with an unbounded buffer.

(defstruct (mailbox (:constructor make-mailbox ()))
  (messages (make-queue))
  (receivers (make-queue)))

(defun mailbox-send (mailbox message)
  (atomically
    (let ((entry (first-live-entry (mailbox-receivers mailbox))))
      (if entry
          (commit-entry (dequeue (mailbox-receivers mailbox)) message)
          (enqueue (mailbox-messages mailbox) message))))
  (sml-unit))

(defstruct (mailbox-recv-event (:include base-event)) mailbox)
(defmethod poll-base ((base mailbox-recv-event))
  (not (queue-empty-p (mailbox-messages (mailbox-recv-event-mailbox base)))))
(defmethod commit-base ((base mailbox-recv-event))
  (dequeue (mailbox-messages (mailbox-recv-event-mailbox base))))
(defmethod block-base ((base mailbox-recv-event) entry)
  (enqueue-entry (mailbox-receivers (mailbox-recv-event-mailbox base)) entry))
