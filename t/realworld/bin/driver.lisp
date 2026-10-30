;;;; driver.lisp -- run one corpus program under cl-sml and classify the result.
;;;;
;;;; Loaded into cl-sml-rw.core (see build-image.sh), one process per program:
;;;;   (rw:run "WORKDIR" "MODE" TIMEOUT-SECONDS)
;;;; MODE is "plain" (no static checker) or "checked" (HaMLet checker), with an
;;;; optional "-bare" suffix to use a package without HaMLet's basis preloaded.
;;;; Writes WORKDIR/MODE.out (program stdout), MODE.log (compiler noise) and
;;;; MODE.res (one line: CLASS<TAB>PHASE<TAB>compile-s<TAB>run-s<TAB>message)
;;;; plus MODE.bt (condition + cl-sml backtrace frames) on failure.
;;;; Program-written files land in WORKDIR/run-MODE/ (the current directory).

(defpackage #:rw (:use #:cl) (:export #:run))
(in-package #:rw)

(define-condition rw-timeout (condition) ())

(defvar *driver-file* *load-truename*)

(defparameter *phase-frames*
  ;; Frame name (symbol-name, upcased) -> phase, checked innermost first.
  '(("TYPE-CHECK-SML-STRING" . "CHECKER")
    ("HAMLET-TYPE-CHECK-STRING" . "CHECKER")
    ("PARSE-SML-PROGRAM" . "PARSE")
    ("COMPILE-PROGRAM" . "COMPILE")
    ("COMPILE-WITH-HOISTED-SML-FORMS" . "COMPILE")
    ("COMPILE-FILE" . "LISPCOMPILE")
    ("LOAD" . "RUNTIME")))

(defun frame-names (&optional (count 4000))
  (let ((names '()))
    (sb-debug:map-backtrace
     (lambda (frame)
       (push (sb-di:debug-fun-name (sb-di:frame-debug-fun frame)) names))
     :count count)
    (nreverse names)))

(defun name-string (name)
  (cond ((symbolp name) (symbol-name name))
        ((and (consp name) (symbolp (second name))) (symbol-name (second name)))
        (t (princ-to-string name))))

(defun frame-phase (names)
  (dolist (name names nil)
    (let* ((s (name-string name))
           (hit (assoc s *phase-frames* :test #'string=)))
      (when (and hit
                 ;; Only cl-sml's PARSE-SML-PROGRAM counts as the parser.
                 (or (not (string= s "PARSE-SML-PROGRAM"))
                     (and (symbolp name)
                          (string= (package-name (symbol-package name)) "CL-SML"))))
        (return (cdr hit))))))

(defun interesting-frames (names &optional (limit 25))
  (let ((out '()))
    (dolist (name names)
      (let ((pkg (cond ((symbolp name) (symbol-package name))
                       ((and (consp name) (symbolp (second name)))
                        (symbol-package (second name))))))
        (when (and pkg (member (package-name pkg)
                               '("CL-SML") :test #'string=))
          (unless (equal name (first out)) (push name out)))))
    (subseq (nreverse out) 0 (min limit (length out)))))

(defun one-line (string &optional (max 300))
  (let ((s (substitute #\Space #\Tab
                       (substitute #\Space #\Newline (string-trim '(#\Space #\Newline) string)))))
    (if (> (length s) max) (concatenate 'string (subseq s 0 max) "...") s)))

(defun classify-condition (c names)
  (let ((phase (or (frame-phase names) "RUNTIME")))
    (cond
      ((typep c 'rw-timeout) (values "TIMEOUT" phase))
      ((typep c 'cl-sml:sml-static-type-error) (values "CHECKER" "CHECKER"))
      ((typep c 'cl-sml:sml-syntax-error) (values "PARSE" "PARSE"))
      ((typep c 'sb-kernel::control-stack-exhausted) (values "STACK" phase))
      ((typep c 'sb-kernel::heap-exhausted-error) (values "HEAP" phase))
      ((typep c 'storage-condition) (values "HEAP" phase))
      ((search "Failed to compile generated Lisp" (princ-to-string c))
       (values "LISPCOMPILE" "LISPCOMPILE"))
      ((search "compiled with errors" (princ-to-string c))
       (values "LISPCOMPILE" "LISPCOMPILE"))
      ((typep c 'cl-sml::sml-raised-exception) (values "SMLEXN" phase))
      ((string= phase "CHECKER") (values "CHECKER" phase))
      ((string= phase "PARSE") (values "PARSE" phase))
      ((string= phase "COMPILE") (values "COMPILE" phase))
      ((string= phase "LISPCOMPILE") (values "LISPCOMPILE" phase))
      (t (values "RUNTIME" phase)))))

(defun condition-message (c)
  (handler-case
      (cond
        ((typep c 'cl-sml:sml-static-type-error)
         (let ((cause (princ-to-string (cl-sml::sml-static-type-error-cause c))))
           ;; drop the "/path/prog.sml:" prefix HaMLet puts on positions
           (let ((p (search "prog.sml:" cause)))
             (format nil "HaMLet: ~A" (if p (subseq cause (+ p 9)) cause)))))
        ((typep c 'cl-sml::sml-raised-exception)
          (let ((v (cl-sml::sml-exception-value c)))
            (format nil "uncaught SML exception ~A~@[ ~S~]"
                    (ignore-errors (cl-sml::sml-exception-name v))
                    (ignore-errors (cl-sml::sml-exception-payload v)))))
        (t (format nil "~A: ~A" (type-of c) c)))
    (error () (format nil "~A (unprintable)" (type-of c)))))

(defun now () (/ (get-internal-real-time) internal-time-units-per-second))

(defun run (workdir mode timeout)
  (let* ((dir (uiop:ensure-directory-pathname workdir))
         (checked (search "checked" mode))
         (bare (search "bare" mode))
         (package (if bare "SML.RW-BARE" "SML.RW"))
         (rundir (merge-pathnames (format nil "run-~A/" mode) dir))
         (prog (merge-pathnames "prog.sml" dir))
         (driver (merge-pathnames "driver.sml" dir))
         (class "OK") (phase "") (message "") (t-compile nil) (t-run nil)
         (warnings 0) (t0 0) (stage nil))
    (ensure-directories-exist rundir)
    (setf cl-sml:*sml-type-checker* (and checked cl-sml::*hamlet-type-checker*))
    (uiop:chdir rundir)
    (with-open-file (out (merge-pathnames (format nil "~A.out" mode) dir)
                         :direction :output :if-exists :supersede)
      (with-open-file (log (merge-pathnames (format nil "~A.log" mode) dir)
                           :direction :output :if-exists :supersede)
        (let ((*standard-output* out)
              (*error-output* log)
              (*trace-output* log)
              (*default-pathname-defaults* rundir)
              (*compile-verbose* nil) (*compile-print* nil)
              (*load-verbose* nil) (*load-print* nil)
              (timer nil))
          (block attempt
            (handler-bind
                ((warning (lambda (w)
                            (unless (typep w 'style-warning) (incf warnings))
                            (format log "~&;; ~A: ~A~%" (type-of w) w)
                            (muffle-warning w)))
                 ((or rw-timeout serious-condition)
                   (lambda (c)
                     (let ((names (ignore-errors (frame-names))))
                       (multiple-value-setq (class phase) (classify-condition c names))
                       (setf message (condition-message c))
                       (ignore-errors
                        (with-open-file (bt (merge-pathnames (format nil "~A.bt" mode) dir)
                                            :direction :output :if-exists :supersede)
                          (format bt "~A~%~A~%~%cl-sml frames (innermost first):~%~{  ~S~%~}"
                                  class message (interesting-frames names 40)))))
                     (if (eq stage :run)
                         (setf t-run (- (now) t0))
                         (setf t-compile (- (now) t0)))
                     (return-from attempt))))
              (setf timer (sb-ext:make-timer (lambda () (signal 'rw-timeout))
                                             :thread sb-thread:*current-thread*))
              (sb-ext:schedule-timer timer timeout)
              (when (search "shim" mode)
                ;; *-shim modes: portability shims (compat/shim.sml) first;
                ;; their load time is not counted.
                (setf stage :shim)
                (cl-sml:load-sml-file
                 (merge-pathnames "../compat/shim.sml"
                                  (make-pathname :name nil :type nil
                                                 :defaults *driver-file*))
                 :package package))
              (setf t0 (now) stage :compile)
              (cl-sml:load-sml-file prog :package package)
              (finish-output out)
              (setf t-compile (- (now) t0))
              (when (probe-file driver)
                (setf t0 (now) stage :run)
                (cl-sml:load-sml-file driver :package package)
                (setf t-run (- (now) t0)))
              (finish-output out)))
          (when timer (sb-ext:unschedule-timer timer)))))
    (with-open-file (res (merge-pathnames (format nil "~A.res" mode) dir)
                         :direction :output :if-exists :supersede)
      (format res "~A	~A	~@[~,2F~]	~@[~,2F~]	~D	~A~%"
              class phase
              (and (realp t-compile) t-compile)
              (and (realp t-run) t-run)
              warnings
              (one-line message)))
    (finish-output)))
