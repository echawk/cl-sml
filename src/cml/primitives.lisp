(in-package #:cl-sml.cml)

;;; The run time's thread safety (docs/DESIGN-CML.md, section 3), installed
;;; when RunCML.doit first runs: the run time's lock and per-stream locks
;;; (cl-sml's *SML-RUNTIME-LOCK-HOOK* and *SML-STREAM-LOCK-HOOK*), and
;;; synchronized versions of the hash tables written at run time.

(defvar *thread-safety-installed* nil)

(defvar *runtime-lock* (bt:make-recursive-lock "cl-sml run time"))

(defvar *stream-locks-lock* (bt:make-lock "cl-sml stream locks"))

(defvar *stream-locks* (cl-sml::make-portable-weak-eq-hash-table)
  "The lock of each stream SML code has used, under *STREAM-LOCKS-LOCK*.")

(defun stream-lock (stream)
  (bt:with-lock-held (*stream-locks-lock*)
    (or (gethash stream *stream-locks*)
        (setf (gethash stream *stream-locks*)
              (bt:make-recursive-lock "cl-sml stream")))))

(defun call-with-stream-lock (stream thunk)
  (bt:with-recursive-lock-held ((stream-lock stream))
    (funcall thunk)))

(defun call-with-runtime-lock (thunk)
  (bt:with-recursive-lock-held (*runtime-lock*)
    (funcall thunk)))

(defun synchronized-copy (table &key weak)
  "A copy of the EQ hash TABLE that threads may read and write at once.
ABCL's hash tables are synchronized already."
  (let ((copy #+sbcl (if weak
                         (make-hash-table :test 'eq :synchronized t :weakness :key)
                         (make-hash-table :test 'eq :synchronized t))
              #+ecl (make-hash-table :test 'eq :synchronized t)
              #-(or sbcl ecl) (progn weak (make-hash-table :test 'eq))))
    (maphash (lambda (key value) (setf (gethash key copy) value)) table)
    copy))

(defun install-thread-safety ()
  "Make the run time safe for several threads; called by RunCML.doit before
it starts the first one."
  (unless *thread-safety-installed*
    (setf cl-sml::*sml-exception-function-tags*
          (synchronized-copy cl-sml::*sml-exception-function-tags*)
          cl-sml::*sml-constructor-function-tags*
          (synchronized-copy cl-sml::*sml-constructor-function-tags* :weak t)
          cl-sml::*sml-runtime-lock-hook* #'call-with-runtime-lock
          cl-sml::*sml-stream-lock-hook* #'call-with-stream-lock
          *thread-safety-installed* t)))

;;; The primitives of the SML structures in this directory: `use{b = NAME}'
;;; there is the function below for NAME.  Tuples arrive as SML tuples,
;;; lists as Lisp lists, SML functions as Lisp functions of one argument.

(defmacro define-primitives (&body definitions)
  `(progn
     ,@(loop for (name lambda-list . body) in definitions
             collect `(setf (gethash ,name cl-sml::*sml-extension-primitives*)
                            (lambda ,lambda-list ,@body)))))

(defmacro with-tuple ((&rest variables) tuple &body body)
  (let ((value (gensym "TUPLE")))
    `(let* ((,value ,tuple)
            ,@(loop for variable in variables
                    for i from 1
                    collect `(,variable (cl-sml:sml-tuple-ref ,value ,i))))
       ,@body)))

(define-primitives
  ("CML.same" (pair) (with-tuple (a b) pair (eq a b)))

  ;; Threads
  ("CML.getTid" (unit) (declare (ignore unit)) (this-thread "CML.getTid"))
  ("CML.tidId" (thread) (thread-id thread))
  ("CML.tidToString" (thread) (thread-name thread))
  ("CML.spawn" (function) (spawn function))
  ("CML.exit" (unit) (declare (ignore unit))
   (this-thread "CML.exit")
   (exit-thread))
  ("CML.joinEvt" (thread) (make-join-event :thread thread))
  ("CML.yield" (unit) (declare (ignore unit))
   (let ((thread (this-thread "CML.yield")))
     (atomically (check-running thread)))
   (yield-thread *scheduler*)
   (sml-unit))
  ("CML.newPropKey" (unit) (declare (ignore unit)) (new-prop-key))
  ("CML.propGet" (pair)
   (with-tuple (key init) pair
     (let ((cell (prop-cell key)))
       (if cell
           (cdr cell)
           (let ((value (funcall init (sml-unit))))
             (push (cons key value) (thread-props (this-thread "CML thread property")))
             value)))))
  ("CML.propPeek" (triple)
   (with-tuple (key some none) triple
     (let ((cell (prop-cell key)))
       (if cell (funcall some (cdr cell)) none))))
  ("CML.propSet" (pair)
   (with-tuple (key value) pair
     (let ((cell (prop-cell key)))
       (if cell
           (setf (cdr cell) value)
           (push (cons key value) (thread-props (this-thread "CML thread property")))))
     (sml-unit)))
  ("CML.propClr" (key)
   (let ((thread (this-thread "CML thread property")))
     (setf (thread-props thread) (remove key (thread-props thread) :key #'car)))
   (sml-unit))

  ;; Events
  ("CML.never" (unit) (declare (ignore unit)) (make-never-event))
  ("CML.alwaysEvt" (value) (make-always-event :value value))
  ("CML.wrap" (pair)
   (with-tuple (event function) pair (make-wrap-event :event event :function function)))
  ("CML.wrapHandler" (pair)
   (with-tuple (event handler) pair (make-handler-event :event event :handler handler)))
  ("CML.guard" (thunk) (make-guard-event :thunk thunk))
  ("CML.withNack" (function) (make-nack-event :function function))
  ("CML.choose" (events) (make-choose-event :events events))
  ("CML.sync" (event) (sync event))
  ("CML.poll" (triple) (with-tuple (event some none) triple (poll-event event some none)))
  ("CML.timeOutEvt" (nanoseconds) (make-time-out-event :nanoseconds nanoseconds))
  ("CML.atTimeEvt" (nanoseconds) (make-at-time-event :nanoseconds nanoseconds))

  ;; Channels
  ("CML.channel" (unit) (declare (ignore unit)) (make-channel))
  ("CML.sendEvt" (pair)
   (with-tuple (channel message) pair (make-send-event :channel channel :message message)))
  ("CML.recvEvt" (channel) (make-recv-event :channel channel))

  ;; SyncVar
  ("SyncVar.new" (unit) (declare (ignore unit)) (make-syncvar))
  ("SyncVar.put" (pair) (with-tuple (var value) pair (syncvar-put var value)))
  ("SyncVar.readEvt" (var) (make-syncvar-read-event :var var))
  ("SyncVar.takeEvt" (var) (make-syncvar-take-event :var var))
  ("SyncVar.swapEvt" (pair)
   (with-tuple (var new) pair (make-syncvar-take-event :var var :new new)))

  ;; Mailbox
  ("Mailbox.new" (unit) (declare (ignore unit)) (make-mailbox))
  ("Mailbox.send" (pair) (with-tuple (mailbox message) pair (mailbox-send mailbox message)))
  ("Mailbox.recvEvt" (mailbox) (make-mailbox-recv-event :mailbox mailbox))

  ;; RunCML
  ("RunCML.isRunning" (unit) (declare (ignore unit)) (running-p))
  ("RunCML.doit" (function) (run-cml function))
  ("RunCML.shutdown" (status) (shutdown status)))
