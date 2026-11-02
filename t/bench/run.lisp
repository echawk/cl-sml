;;; Micro-benchmarks for the code generator (docs/ROADMAP.md items 13-14).
;;; Usage, from the repository root:
;;;   sbcl --dynamic-space-size 4GB --script t/bench/run.lisp [native|plain] [NAME...]
;;; Each program defines `benchmark : unit -> int'.  Prints the compile
;;; time, the best run time of three, the bytes consed by that run, and the
;;; result (which must not change when the compiler changes).

(load "~/.sbclrc")
(push (truename ".") asdf:*central-registry*)
(ql:quickload :cl-sml :silent t)

(defvar *bench-directory* (merge-pathnames "t/bench/" (truename ".")))

(defvar *bench-expected*
  '(("fib" . 832040) ("tak" . 140) ("real" . 3288753) ("bst" . 460260)
    ("eq" . 1409666) ("word" . 15003428) ("rec" . 1432352292)))

(defvar *bench-names*
  (or (cddr (uiop:raw-command-line-arguments)) (mapcar #'car *bench-expected*)))

(defvar *bench-checker*
  (if (equal (second (uiop:raw-command-line-arguments)) "plain") nil :native))

(defun bench-seconds (start)
  (/ (- (get-internal-real-time) start) (float internal-time-units-per-second 1d0)))

(defun bench-bytes-consed ()
  #+sbcl (sb-ext:get-bytes-consed)
  #-sbcl 0)

;; Elaborate the Basis first, so that the first compile time does not include it.
(let ((cl-sml:*sml-type-checker* *bench-checker*))
  (cl-sml:load-sml-file (merge-pathnames "fib.sml" *bench-directory*)
                        :package "SML.BENCH.WARMUP"))

(dolist (name *bench-names*)
  (let* ((package (format nil "SML.BENCH.~A" (string-upcase name)))
         (cl-sml:*sml-type-checker* *bench-checker*)
         (start (get-internal-real-time)))
    (cl-sml:load-sml-file (merge-pathnames (format nil "~A.sml" name) *bench-directory*)
                          :package package)
    (let ((compile-seconds (bench-seconds start))
          (best nil) (bytes nil) (result nil))
      (dotimes (i 3)
        #+sbcl (sb-ext:gc :full t)
        (let ((bytes0 (bench-bytes-consed))
              (run-start (get-internal-real-time)))
          (setf result (funcall (cl-sml:sml-function "benchmark" package) (cl-sml::sml-unit)))
          (let ((seconds (bench-seconds run-start)))
            (when (or (null best) (< seconds best))
              (setf best seconds
                    bytes (- (bench-bytes-consed) bytes0))))))
      (format t "~8A compile ~5,2Fs  run ~7,3Fs  ~8,1F MB  result ~A~:[  WRONG~;~]~%"
              name compile-seconds best (/ bytes 1d6) result
              (eql result (cdr (assoc name *bench-expected* :test #'string=))))
      (finish-output))))
