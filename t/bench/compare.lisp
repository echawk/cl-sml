;;;; compare.lisp -- run one program of t/bench/compare.sh under cl-sml.
;;;;
;;;; Loaded into the image compare.sh builds, one process per run:
;;;;   (cmp:run "WORKDIR/" POLICY)
;;;; compiles WORKDIR/prog.sml with the native checker and *SML-OPTIMIZE-POLICY*
;;;; POLICY, then WORKDIR/timed.sml, which runs and times the program (the
;;;; time goes to stderr as "@@TIME microseconds").  The current directory is
;;;; WORKDIR/run-IMPL/, set by compare.sh.

(defpackage #:cmp (:use #:cl) (:export #:run))
(in-package #:cmp)

(defun run (workdir policy)
  (let ((dir (uiop:ensure-directory-pathname workdir)))
    (setf *default-pathname-defaults* (uiop:getcwd))
    (handler-case
        (let ((cl-sml:*sml-type-checker* :native)
              (cl-sml:*sml-optimize-policy* policy)
              (*compile-verbose* nil) (*compile-print* nil)
              (*load-verbose* nil) (*load-print* nil))
          (handler-bind ((warning #'muffle-warning))
            (cl-sml:load-sml-file (merge-pathnames "prog.sml" dir) :package "SML.CMP")
            (cl-sml:load-sml-file (merge-pathnames "timed.sml" dir) :package "SML.CMP"))
          (finish-output *standard-output*)
          (finish-output *error-output*)
          (uiop:quit 0))
      (serious-condition (c)
        (finish-output *standard-output*)
        (format *error-output* "~&cl-sml: ~A~%" c)
        (finish-output *error-output*)
        (uiop:quit 1)))))
