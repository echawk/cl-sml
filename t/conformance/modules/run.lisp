;;;; Conformance runner for cl-sml: Modules language and Basis Library.
;;;;
;;;; Each test is tests/NAME.sml with expected stdout in tests/NAME.out
;;;; (generated with Poly/ML and cross-checked with MLton, see gen-ref.sh).
;;;; Every test runs in its own fresh SML package with stdout captured.
;;;;
;;;; Configuration (environment variables, normally set by run.sh):
;;;;   CONF_CHECKER  "1" to type-check with HaMLet (cl-sml:enable-hamlet-type-checker)
;;;;   CONF_BASIS    ignored: every SML package sees cl-sml's Basis (kept for
;;;;                 older command lines; it used to preload HaMLet's)
;;;;   CONF_TIMEOUT  seconds per test (default 60)
;;;;   CONF_ISOLATE  "fork" (default for plain mode on SBCL): one forked child per test;
;;;;                 "none": in-process (run.sh restarts the Lisp after a crash and
;;;;                 resumes with CONF_RESUME=1; default with --checker, because
;;;;                 forked children of the large HaMLet image run very slowly)
;;;;   CONF_RESUME   "1": keep results already recorded and skip those tests
;;;;   CONF_TESTS    space-separated test names (default: every tests/*.sml)
;;;;   CONF_TESTDIR  directory holding NAME.sml / NAME.out (default tests/)
;;;;   CONF_RESULTS  directory for NAME.actual / NAME.diag files

(defpackage #:cl-sml-conformance
  (:use #:cl))
(in-package #:cl-sml-conformance)

(defvar *root* (uiop:pathname-directory-pathname
                (or *load-truename* *default-pathname-defaults*)))

(defun env (name default)
  (let ((value (uiop:getenv name)))
    (if (and value (plusp (length value))) value default)))

(defvar *tests* (uiop:ensure-directory-pathname
                 (env "CONF_TESTDIR" (namestring (merge-pathnames "tests/" *root*)))))
(defvar *checker* (string= (env "CONF_CHECKER" "0") "1"))
(defvar *basis-mode* (env "CONF_BASIS" "auto"))
(defvar *timeout* (parse-integer (env "CONF_TIMEOUT" "60")))
(defvar *results*
  (uiop:ensure-directory-pathname
   (env "CONF_RESULTS"
        (namestring (merge-pathnames (if *checker* "results/checker/" "results/plain/")
                                     *root*)))))

(defun test-names ()
  (let ((explicit (env "CONF_TESTS" nil)))
    (if explicit
        (remove "" (uiop:split-string explicit :separator '(#\Space)) :test #'string=)
        (sort (mapcar #'pathname-name
                      (directory (merge-pathnames "*.sml" *tests*)))
              #'string<))))

(defun needs-basis-p (name)
  (cond ((string= *basis-mode* "all") t)
        ((string= *basis-mode* "none") nil)
        (t (char= (char name 0) #\b))))

(defun read-file (path)
  (if (probe-file path) (uiop:read-file-string path) nil))

(defun first-line (string &optional (limit 300))
  (let* ((s (string-trim '(#\Space #\Newline #\Tab) string))
         (nl (position #\Newline s))
         (line (subseq s 0 (or nl (length s)))))
    (if (> (length line) limit) (subseq line 0 limit) line)))

(defmacro with-test-timeout ((seconds) &body body)
  #+sbcl `(sb-ext:with-timeout ,seconds ,@body)
  #-sbcl (declare (ignore seconds))
  #-sbcl `(progn ,@body))

(defun quietly (thunk)
  (let ((*standard-output* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (funcall thunk)))

(defun load-basis (package)
  ;; Every SML package sees cl-sml's Basis; there is nothing to load.
  (declare (ignore package)))

(defun classify (condition)
  (cond
    ((typep condition 'cl-sml:sml-static-type-error) "REJECT")
    ((typep condition (find-symbol "SML-RAISED-EXCEPTION" "CL-SML")) "SMLEXN")
    #+sbcl ((typep condition 'sb-ext:timeout) "TIMEOUT")
    ((typep condition 'storage-condition) "STACK")
    (t "ERROR")))

(defun run-test (name)
  "Run test NAME; return (values status actual diagnostic)."
  (let* ((source (merge-pathnames (format nil "~A.sml" name) *tests*))
         (package (format nil "CONF.~:[PLAIN~;CHECKED~].~A" *checker* (string-upcase name)))
         (out (make-string-output-stream))
         (err (make-string-output-stream))
         (status nil)
         (diag ""))
    (handler-case
        (with-test-timeout (*timeout*)
          (when (needs-basis-p name)
            (load-basis package))
          (let ((*standard-output* out)
                (*error-output* err)
                (*compile-verbose* nil)
                (*compile-print* nil)
                (*load-verbose* nil))
            (cl-sml:load-sml-file source :package package)))
      (serious-condition (c)
        (setf status (classify c)
              diag (format nil "~A: ~A" (type-of c)
                           (handler-case (princ-to-string c)
                             (error () "<unprintable condition>"))))))
    (let* ((actual (get-output-stream-string out))
           (expected (or (read-file (merge-pathnames (format nil "~A.out" name) *tests*)) ""))
           (final (cond ((and (null status) (string= actual expected)) "PASS")
                        ((null status) "FAIL")
                        (t status))))
      (values final actual
              (concatenate 'string diag (string #\Newline)
                           "--- stderr ---" (string #\Newline)
                           (get-output-stream-string err))))))

(defun write-file (path string)
  (with-open-file (s path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string string s)))

(defun result-path (name type)
  (merge-pathnames (format nil "~A.~A" name type) *results*))

;;; Isolation: on SBCL each test runs in a forked child of the process that
;;; loaded cl-sml (and HaMLet) once.  A test that exhausts the heap, overflows
;;; the stack fatally or loops forever then only takes its child down.

#+sbcl (require :sb-posix)

(defun run-test-in-child (name)
  "Run NAME in the current process and record its results in files."
  (multiple-value-bind (status actual diag) (run-test name)
    (write-file (result-path name "actual") actual)
    (write-file (result-path name "diag") diag)
    (write-file (result-path name "status") status)))

#+sbcl
(defun run-test-forked (name)
  (let ((status-file (result-path name "status")))
    (when (probe-file status-file) (delete-file status-file))
    (finish-output *standard-output*)
    (finish-output *error-output*)
    (let ((pid (sb-posix:fork)))
      (cond
        ((zerop pid)
         ;; Child: low-level crash reports go to NAME.crash, not the table.
         (let ((fd (sb-posix:open (namestring (result-path name "crash"))
                                  (logior sb-posix:o-wronly sb-posix:o-creat
                                          sb-posix:o-trunc)
                                  #o644)))
           (sb-posix:dup2 fd 1)
           (sb-posix:dup2 fd 2))
         (handler-case (run-test-in-child name)
           (serious-condition (c)
             (ignore-errors
              (write-file (result-path name "status") "ERROR")
              (write-file (result-path name "diag") (princ-to-string c)))))
         (sb-ext:exit :code 0 :abort t))
        (t
         (let ((deadline (+ (get-internal-real-time)
                            (* (+ *timeout* 15) internal-time-units-per-second)))
               (exited nil))
           (loop
             (multiple-value-bind (done wstatus) (sb-posix:waitpid pid sb-posix:wnohang)
               (declare (ignore wstatus))
               (when (= done pid) (setf exited t) (return)))
             (when (> (get-internal-real-time) deadline)
               (ignore-errors (sb-posix:kill pid sb-posix:sigkill))
               (ignore-errors (sb-posix:waitpid pid 0))
               (return))
             (sleep 0.05))
           (let ((status (read-file status-file))
                 (crash (or (read-file (result-path name "crash")) "")))
             (cond
               (status
                (values status
                        (or (read-file (result-path name "actual")) "")
                        (or (read-file (result-path name "diag")) "")))
               ((not exited)
                (values "TIMEOUT" "" "killed after timeout"))
               (t
                (values "CRASH" ""
                        (format nil "child died: ~A"
                                (let ((lines (remove "" (uiop:split-string crash :separator '(#\Newline))
                                                     :test #'string=)))
                                  (or (find-if (lambda (l) (search "exhausted" l)) lines)
                                      (first lines)
                                      "no output")))))))))))))

(defun run-one (name)
  #+sbcl (if (string= (env "CONF_ISOLATE" "fork") "fork")
             (run-test-forked name)
             (run-test name))
  #-sbcl (run-test name))

(defun current-marker () (merge-pathnames "CURRENT" *results*))

(defun note-crashed-test ()
  "A previous in-process run died while running the test named in CURRENT."
  (let ((name (read-file (current-marker))))
    (when name
      (setf name (string-trim '(#\Space #\Newline) name))
      (write-file (result-path name "status") "CRASH")
      (write-file (result-path name "secs") "0")
      (write-file (result-path name "diag")
                  (format nil "process died while running the test (see run log)~%"))
      (unless (probe-file (result-path name "actual"))
        (write-file (result-path name "actual") ""))
      (delete-file (current-marker)))))

(defun print-summary (names)
  (let ((counts (make-hash-table :test #'equal)))
    (with-open-file (s (merge-pathnames "summary.txt" *results*)
                       :direction :output :if-exists :supersede)
      (dolist (name names)
        (let* ((status (string-trim '(#\Space #\Newline)
                                    (or (read-file (result-path name "status")) "MISSING")))
               (diag (or (read-file (result-path name "diag")) ""))
               (secs (string-trim '(#\Space #\Newline)
                                  (or (read-file (result-path name "secs")) "?")))
               (note (if (member status '("PASS" "FAIL") :test #'string=) "" (first-line diag 160))))
          (incf (gethash status counts 0))
          (format s "~A	~A	~A	~A~%" status name secs note))))
    (format t "~&~%mode: checker=~:[off~;on~] basis=~A~%" *checker* *basis-mode*)
    (maphash (lambda (k v) (format t "~A: ~D~%" k v)) counts)
    (= (gethash "PASS" counts 0) (length names))))

(defun main ()
  "Run every test not yet recorded in *RESULTS* (all of them unless
CONF_RESUME=1), print one line per test and a summary."
  (ensure-directories-exist *results*)
  (let ((resume (string= (env "CONF_RESUME" "0") "1"))
        (names (test-names)))
    (if resume
        (note-crashed-test)
        (dolist (name names)
          (let ((f (result-path name "status"))) (when (probe-file f) (delete-file f)))))
    (when *checker*
      (format t "~&;; enabling HaMLet type checker...~%")
      (quietly #'cl-sml:enable-hamlet-type-checker))
    (dolist (name names)
      (unless (and resume (probe-file (result-path name "status")))
        (write-file (current-marker) name)
        (let ((start (get-internal-real-time)))
          (multiple-value-bind (status actual diag) (run-one name)
            (let ((secs (/ (- (get-internal-real-time) start)
                           internal-time-units-per-second)))
              (write-file (result-path name "actual") actual)
              (write-file (result-path name "diag") diag)
              (write-file (result-path name "secs") (format nil "~,1F" secs))
              (write-file (result-path name "status") status)
              (delete-file (current-marker))
              (format t "~&~7A ~30A ~6,1Fs ~A~%" status name secs
                      (if (member status '("PASS" "FAIL") :test #'string=)
                          ""
                          (first-line diag 160)))
              (finish-output))))))
    (print-summary names)))
