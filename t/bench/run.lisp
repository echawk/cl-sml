;;; Micro-benchmarks for the code generator (docs/ROADMAP.md items 13-14).
;;; Usage, from the repository root:
;;;   sbcl --dynamic-space-size 4GB --script t/bench/run.lisp [native|plain|coalton] [NAME...]
;;; (coalton: through the Coalton backend, cl-sml/coalton.)
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

(defvar *bench-coalton* (equal (second (uiop:raw-command-line-arguments)) "coalton"))

(when *bench-coalton*
  (ql:quickload :cl-sml/coalton :silent t)
  (uiop:symbol-call '#:cl-sml '#:load-coalton-basis))

(defun bench-load (pathname package)
  "Compile and run the program; return its `benchmark' as a CL function of
no arguments."
  (if *bench-coalton*
      (let* ((coalton-package (uiop:symbol-call '#:cl-sml '#:load-sml-file-via-coalton
                                                pathname :package-name package))
             (symbol (find-symbol "benchmark" coalton-package))
             (*package* coalton-package))
        (compile nil `(lambda () (,(find-symbol "COALTON" "COALTON")
                                   (,symbol ,(find-symbol "UNIT" "COALTON"))))))
      (progn
        (cl-sml:load-sml-file pathname :package package)
        (let ((function (cl-sml:sml-function "benchmark" package)))
          (lambda () (funcall function (cl-sml::sml-unit)))))))

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
    (let ((benchmark (bench-load (merge-pathnames (format nil "~A.sml" name) *bench-directory*)
                                 package))
          (compile-seconds (bench-seconds start))
          (best nil) (bytes nil) (result nil))
      (dotimes (i 3)
        #+sbcl (sb-ext:gc :full t)
        (let ((bytes0 (bench-bytes-consed))
              (run-start (get-internal-real-time)))
          (setf result (funcall benchmark))
          (let ((seconds (bench-seconds run-start)))
            (when (or (null best) (< seconds best))
              (setf best seconds
                    bytes (- (bench-bytes-consed) bytes0))))))
      (format t "~8A compile ~5,2Fs  run ~7,3Fs  ~8,1F MB  result ~A~:[  WRONG~;~]~%"
              name compile-seconds best (/ bytes 1d6) result
              (eql result (cdr (assoc name *bench-expected* :test #'string=))))
      (finish-output))))
