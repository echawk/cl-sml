;;; Concurrent ML (src/cml, system cl-sml/cml): the conformance list of
;;; docs/DESIGN-CML.md, section 4, and programs ported from MLton's
;;; lib/cml/tests.  Each program in programs/ ends with the output it must
;;; print, in a comment `(* EXPECT ... *)'.  The programs do not depend on
;;; timing beyond lower bounds and generous time-outs; a watchdog shuts
;;; down a program that hangs.

(defpackage #:cl-sml-cml-tests
  (:use #:cl #:fiveam)
  (:export #:run-cml-tests))

(in-package #:cl-sml-cml-tests)

(def-suite cl-sml-cml-suite :description "Concurrent ML for cl-sml")

(in-suite cl-sml-cml-suite)

(defvar *programs*
  (asdf:system-relative-pathname "cl-sml/cml-tests" "t/cml/programs/"))

(defvar *watchdog-seconds* 300
  "How long a program may run before the watchdog shuts its CML down.")

(defvar *counter* 0)

(defun call-with-watchdog (seconds thunk)
  "Call THUNK; if it has not returned after SECONDS, finish the running
CML instance (as a deadlock would), so that RunCML.doit returns."
  (let* ((lock (bt:make-lock))
         (condition (bt:make-condition-variable))
         (done nil)
         (fired nil)
         (watchdog (bt:make-thread
                    (lambda ()
                      (bt:with-lock-held (lock)
                        (loop with deadline = (+ (get-internal-real-time)
                                                 (* seconds internal-time-units-per-second))
                              until (or done (>= (get-internal-real-time) deadline))
                              do (bt:condition-wait condition lock
                                                    :timeout (max 0.01
                                                                  (/ (- deadline (get-internal-real-time))
                                                                     internal-time-units-per-second)))))
                      (unless done
                        (setf fired t)
                        (let ((instance cl-sml.cml::*instance*))
                          (when instance
                            (cl-sml.cml::call-atomically
                             cl-sml.cml:*scheduler*
                             (lambda () (cl-sml.cml::finish-instance instance 1)))))))
                    :name "CML test watchdog")))
    (unwind-protect (funcall thunk)
      (bt:with-lock-held (lock)
        (setf done t)
        (bt:condition-notify condition))
      (bt:join-thread watchdog)
      (when fired
        (fail "watchdog: the program ran longer than ~D s" seconds)))))

(defun run-sml (source)
  "Compile and run SOURCE; return what it printed on the standard and the
error output."
  (let ((error-output (make-string-output-stream))
        (package (format nil "SML.CML-TEST-~D" (incf *counter*))))
    (values
     (with-output-to-string (*standard-output*)
       (let ((*error-output* error-output)
             (cl-sml:*sml-type-checker* :native))
         (call-with-watchdog *watchdog-seconds*
                             (lambda ()
                               (eval (cl-sml:compile-sml-program-string source :package package))))))
     (get-output-stream-string error-output))))

(defun expected-output (source)
  (let* ((marker "(* EXPECT")
         (start (search marker source))
         (end (and start (search "*)" source :start2 start))))
    (and start
         (string-left-trim '(#\Newline) (subseq source (+ start (length marker)) end)))))

(defun program-source (name)
  (uiop:read-file-string (merge-pathnames (format nil "~A.sml" name) *programs*)))

(defvar *ring-threads*
  #+ecl 50 #-ecl 1000
  "The threads of stress.sml's ring.  ECL runs out of memory with a few
hundred threads (its default heap and per-thread stacks).")

(defun adapt-source (name source)
  (if (string= name "stress")
      (let* ((old "val threads = 1000")
             (start (search old source)))
        (concatenate 'string (subseq source 0 start)
                     (format nil "val threads = ~D" *ring-threads*)
                     (subseq source (+ start (length old)))))
      source))

(defmacro test-program (name &body checks)
  "Run program NAME of programs/ and compare its output with its EXPECT
comment; CHECKS may also examine OUTPUT and ERRORS."
  `(test ,(intern (string-upcase (format nil "program-~A" name)))
     (let ((source (adapt-source ,name (program-source ,name))))
       (multiple-value-bind (output errors) (run-sml source)
         (declare (ignorable errors))
         (let ((expected (expected-output source)))
           (when expected
             (is (string= expected output)
                 "~A printed~%~A~%instead of~%~A~%errors:~%~A" ,name output expected errors)))
         ,@checks))))

(test-program "channels")
(test-program "events")
(test-program "time")
(test-program "threads"
  ;; The uncaught exception was reported, and only that one.
  (is (search "Fail" errors))
  (is (search "boom" errors)))
(test-program "syncvar")
(test-program "mailbox-multicast")
(test-program "runcml")
(test-program "mlton-ping-pong")
(test-program "mlton-timeout")
(test-program "mlton-primes-multicast")
(test-program "mlton-exit"
  (is (search "uncaught" errors)))

(test-program "stress"
  (let ((lines (uiop:split-string (string-right-trim '(#\Newline) output)
                                  :separator '(#\Newline)))
        (a (make-string 64 :initial-element #\a))
        (b (make-string 64 :initial-element #\b)))
    (is (string= (format nil "ring: ~D" (* 100 *ring-threads*)) (first lines)))
    (is (string= "success" (car (last lines))))
    (is (= 10000 (count a lines :test #'string=)))
    (is (= 10000 (count b lines :test #'string=)))
    (is (= 20002 (length lines)) "corrupted lines")))

;;; Random choice among enabled events, reproducible with a fixed seed.

(defun choices ()
  (run-sml "val r = ref []
val _ = RunCML.doit (fn () =>
  (List.app (fn _ => r := CML.select (List.tabulate (4, CML.alwaysEvt)) :: !r)
            (List.tabulate (200, fn i => i));
   RunCML.shutdown OS.Process.success), NONE)
val _ = print (String.concat (map Int.toString (!r)))"))

(test random-choice
  (cl-sml.cml:seed-choice 42)
  (let ((first (choices)))
    (cl-sml.cml:seed-choice 42)
    (is (string= first (choices)))
    ;; Every branch is chosen sometimes.
    (is (every (lambda (digit) (find digit first)) "0123"))
    (cl-sml.cml:seed-choice 43)
    (is (string/= first (choices)))))

;;; The run time's thread safety (docs/DESIGN-CML.md, section 3).

(test runtime-thread-safety
  ;; Threads that declare exceptions and make records and tuples of new
  ;; shapes at once.
  (is (string= "ok"
               (run-sml "val done = CML.channel ()
fun work i = let exception E of int
                 val r = {a = i, b = i + 1}
             in (raise E (#a r + #b r)) handle E n => CML.send (done, n) end
val total = ref 0
val _ = RunCML.doit (fn () =>
  (List.app (fn i => ignore (CML.spawn (fn () => work i))) (List.tabulate (50, fn i => i));
   List.app (fn _ => total := !total + CML.recv done) (List.tabulate (50, fn i => i));
   RunCML.shutdown OS.Process.success), NONE)
val _ = print (if !total = 2500 then \"ok\" else \"wrong\")")))
  (is (not (null cl-sml::*sml-runtime-lock-hook*)))
  (is (not (null cl-sml::*sml-stream-lock-hook*)))
  ;; New threads see the spawning thread's bindings of the output streams.
  (is (string= "from a thread"
               (run-sml "val _ = RunCML.doit (fn () =>
  let val ch = CML.channel ()
  in ignore (CML.spawn (fn () => (print \"from a thread\"; CML.send (ch, ()))));
     CML.recv ch; RunCML.shutdown OS.Process.success end, NONE)"))))

(defun run-cml-tests ()
  (run! 'cl-sml-cml-suite))
