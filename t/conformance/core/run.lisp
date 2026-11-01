;;; SML97 Core conformance runner for cl-sml.
;;;
;;; Each tests/NAME.sml is compiled and run in a fresh SML package with
;;; LOAD-SML-FILE.  Its standard output is compared with tests/NAME.out
;;; (generated from reference implementations by gen-expected.sh).
;;;
;;; Directives, anywhere in a test's source:
;;;   (* @basis *)   the test uses the Basis Library.  Every SML package sees
;;;                  cl-sml's Basis, so the runner does nothing for it (it
;;;                  used to load HaMLet's basis into the test's package).
;;;
;;; Environment variables (run.sh sets them from its flags):
;;;   CONF_CHECKER=1      enable the HaMLet static checker
;;;   CONF_CHECKER=native use the native checker (the default of cl-sml);
;;;                       otherwise no checker (plain mode)
;;;   CONF_TESTS="a b"    run only these tests (default: all)
;;;   CONF_TIMEOUT=60     per-test timeout in seconds (SBCL only)
;;;   CLSML=/path         the cl-sml checkout (default: the current directory)

;;; Load cl-sml first: the code below refers to its symbols.
(let ((root (uiop:getenv "CLSML")))
  (when (and root (plusp (length root)))
    (push (uiop:ensure-directory-pathname root) asdf:*central-registry*)))
(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (ql:quickload '(:cl-sml :cl-sml/hamlet-oracle) :silent t))

(defpackage #:conf-core
  (:use #:cl)
  (:export #:main))

(in-package #:conf-core)

(defparameter *here*
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *default-pathname-defaults*)))

(defun env (name &optional default)
  (let ((value (uiop:getenv name)))
    (if (and value (plusp (length value))) value default)))

(defparameter *checker-p* (equal (env "CONF_CHECKER") "1"))
(defparameter *native-p* (equal (env "CONF_CHECKER") "native"))
(defparameter *timeout* (parse-integer (env "CONF_TIMEOUT" "60")))
(defparameter *mode* (cond (*checker-p* "checked") (*native-p* "native") (t "plain")))

(defun test-names ()
  (let ((only (env "CONF_TESTS")))
    (if only
        (uiop:split-string (string-trim " " only) :separator " ")
        (sort (mapcar #'pathname-name
                      (directory (merge-pathnames "tests/*.sml" *here*)))
              #'string<))))

(defun file-string (pathname)
  (if (probe-file pathname) (uiop:read-file-string pathname) nil))

(defun call-with-timeout (seconds thunk)
  #+sbcl (sb-ext:with-timeout seconds (funcall thunk))
  #-sbcl (progn seconds (funcall thunk)))

(defun first-line (string &optional (max 160))
  "STRING with runs of whitespace collapsed, truncated to MAX characters."
  (let ((collapsed (with-output-to-string (out)
                     (let ((space nil))
                       (loop for ch across (string-trim '(#\Space #\Newline #\Tab) string)
                             do (if (member ch '(#\Space #\Newline #\Tab #\Return))
                                    (setf space t)
                                    (progn (when space (write-char #\Space out))
                                           (setf space nil)
                                           (write-char ch out))))))))
    (subseq collapsed 0 (min max (length collapsed)))))

(defun run-one (name)
  "Run test NAME.  Returns (values status detail actual-output error-text)."
  (let* ((source (merge-pathnames (format nil "tests/~A.sml" name) *here*))
         (expected (file-string (merge-pathnames (format nil "tests/~A.out" name)
                                                 *here*)))
         (package (format nil "SML.CONF-~:@(~A~).~:@(~A~)" *mode* name))
         (out (make-string-output-stream))
         (err (make-string-output-stream))
         (status nil)
         (detail ""))
    (when (find-package package)
      (delete-package package))
    (handler-case
        (let ((*error-output* err)
              (*compile-verbose* nil)
              (*compile-print* nil)
              (*load-verbose* nil))
          (let ((*standard-output* out))
            (handler-bind ((warning #'muffle-warning))
              (call-with-timeout
               *timeout*
               (lambda () (cl-sml:load-sml-file source :package package)))))
          (setf status :ran))
      #+sbcl
      (sb-ext:timeout ()
        (setf status :timeout detail (format nil "timeout after ~Ds" *timeout*)))
      (serious-condition (condition)
        (setf status :error
              detail (first-line (handler-case (princ-to-string condition)
                                   (error () (format nil "~S" (type-of condition)))))))
      )
    (let ((actual (get-output-stream-string out)))
      (cond
        ((null expected) (setf status :no-expected))
        ((and (eq status :ran) (string= actual expected)) (setf status :pass))
        ((eq status :ran)
         (setf status :fail
               detail (let ((mismatch (mismatch actual expected)))
                        (format nil "output differs at line ~D"
                                (1+ (count #\Newline actual :end (min mismatch (length actual)))))))))
      (values status detail actual (get-output-stream-string err)))))

(defun main ()
  (let ((names (test-names))
        (results nil)
        (actual-dir (merge-pathnames (format nil "build/actual/~A/" *mode*) *here*)))
    (ensure-directories-exist actual-dir)
    (format t "~&cl-sml Core conformance (~A mode, ~D tests)~%" *mode* (length names))
    (finish-output)
    (dolist (name names)
      (multiple-value-bind (status detail actual error-text) (run-one name)
        (with-open-file (s (merge-pathnames (format nil "~A.txt" name) actual-dir)
                           :direction :output :if-exists :supersede)
          (write-string actual s))
        (with-open-file (s (merge-pathnames (format nil "~A.err" name) actual-dir)
                           :direction :output :if-exists :supersede)
          (format s "~A~%~A" detail error-text))
        (push (cons name status) results)
        (format t "~&~7A ~30A ~A~%"
                (if (eq status :pass) "PASS" (string-upcase (string status)))
                name (if (eq status :pass) "" detail))
        (finish-output)))
    (let ((passed (count :pass results :key #'cdr)))
      (format t "~&~A mode: ~D/~D passed, ~D failed~%"
              *mode* passed (length results) (- (length results) passed))
      (format t "Actual outputs: ~A~%" (namestring actual-dir))
      (= passed (length results)))))

;;; HaMLet takes a while to load and elaborate its basis; do it once.
(setf cl-sml:*sml-type-checker* (and *native-p* :native))
(when *checker-p*
  (let ((*standard-output* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (cl-sml:enable-hamlet-type-checker)))
