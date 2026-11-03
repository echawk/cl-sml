;;; Concurrent ML through the Coalton backend (docs/DESIGN-CML.md,
;;; milestone 7): the programs of cml-tests.lisp, compiled by cl-sml/coalton,
;;; print what they must.  time.sml is left out: the Coalton backend types
;;; an int literal compared with a LargeInt.int (Time.toMilliseconds) as int.

(in-package #:cl-sml-cml-tests)

(def-suite cl-sml-cml-coalton-suite :description "Concurrent ML through Coalton")

(in-suite cl-sml-cml-coalton-suite)

(defun run-sml-via-coalton (source)
  (let ((error-output (make-string-output-stream))
        (package (format nil "SML.CML-COALTON-TEST-~D" (incf *counter*))))
    (values
     (with-output-to-string (*standard-output*)
       (let ((*error-output* error-output)
             (cl-sml:*sml-type-checker* :native))
         (call-with-watchdog *watchdog-seconds*
                             (lambda ()
                               (cl-sml:load-sml-via-coalton source :package-name package)))))
     (get-output-stream-string error-output))))

(test coalton-basis-has-cml
  (is (not (null (cl-sml::ensure-coalton-basis-loaded))))
  (let ((blocked (cl-sml::coalton-registry-blocked cl-sml::*coalton-basis*)))
    (dolist (structure '("CML." "SyncVar." "Mailbox." "Multicast." "RunCML."))
      (is (notany (lambda (entry) (eql 0 (search structure (car entry)))) blocked)
          "~A has blocked declarations" structure))))

(test coalton-programs
  (dolist (name '("channels" "events" "threads" "syncvar" "mailbox-multicast" "runcml"
                  "mlton-ping-pong" "mlton-primes-multicast" "mlton-exit"))
    (let ((source (program-source name)))
      (multiple-value-bind (output errors) (run-sml-via-coalton source)
        (is (string= (expected-output source) output)
            "~A printed~%~A~%errors:~%~A" name output errors)))))

(defun run-cml-coalton-tests ()
  (run! 'cl-sml-cml-coalton-suite))
