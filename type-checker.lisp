(in-package #:cl-sml)

;;; HaMLet (the `hamlet' git submodule) provides cl-sml's SML97 static
;;; semantics.  A checker is a HaMLet session: each successfully elaborated
;;; program extends the static basis that later programs are checked against.

(defstruct (hamlet-type-checker
            (:constructor %make-hamlet-type-checker (package argument)))
  package
  argument
  ;; HaMLet's rendering of the bindings from the last successful check,
  ;; e.g. "val x : int\n".
  (last-description ""))

(defun default-hamlet-root ()
  (asdf:system-relative-pathname "cl-sml" "hamlet/"))

(defun hamlet-adapter-source ()
  (asdf:system-relative-pathname "cl-sml" "hamlet-check.sml"))

(defun configure-hamlet-basis-path (package basis-path)
  (with-sml-package (package)
    (let ((cell (sml-value "Sml.basisPath")))
      (setf (aref (ensure-sml-ref cell) 1)
            (sml-some-value (namestring (truename basis-path)))))))

(defun call-capturing-hamlet-diagnostics (thunk)
  "Call THUNK with HaMLet's stderr diagnostics captured.
Returns THUNK's value and the captured text."
  (let* ((diagnostics (make-string-output-stream))
         (value (let ((*error-output* diagnostics))
                  (funcall thunk))))
    (values value (get-output-stream-string diagnostics))))

(defun initialize-hamlet-type-checker (package)
  (with-sml-package (package)
    (%make-hamlet-type-checker
     (package-name (ensure-sml-package package))
     ;; Loading the library prints "[loading standard basis library]".
     (call-capturing-hamlet-diagnostics
      (lambda () (call-sml "ClSmlHamlet.initial" (sml-unit)))))))

(defun make-hamlet-type-checker (&key
                                   (package "SML.HAMLET-TYPE-CHECKER")
                                   (hamlet-root (default-hamlet-root))
                                   (load t))
  "Load HaMLet and return a stateful SML97 static elaboration session."
  (let* ((root (uiop:ensure-directory-pathname (truename hamlet-root)))
         (basis-source (merge-pathnames #P"basis/all.sml" root))
         (hamlet-source (merge-pathnames #P"hamlet.sml" root))
         (basis-path (merge-pathnames #P"basis/" root))
         (*sml-type-checker* nil))
    (unless (probe-file hamlet-source)
      (error "HaMLet sources not found at ~A; run `git submodule update --init`."
             root))
    (when load
      (load-sml-file basis-source :package package)
      (load-sml-file hamlet-source :package package)
      (load-sml-file (hamlet-adapter-source) :package package))
    (configure-hamlet-basis-path package basis-path)
    (initialize-hamlet-type-checker package)))

(defvar *hamlet-type-checker* nil
  "The checker installed by ENABLE-HAMLET-TYPE-CHECKER, created on first use.")

(defun enable-hamlet-type-checker (&rest options)
  "Make HaMLet the default static checker for compilation, file loading and
the REPL.  OPTIONS are passed to MAKE-HAMLET-TYPE-CHECKER when the shared
checker has not been created yet."
  (setf *sml-type-checker*
        (or *hamlet-type-checker*
            (setf *hamlet-type-checker*
                  (apply #'make-hamlet-type-checker options)))))

(defun disable-type-checker ()
  (setf *sml-type-checker* nil))

(defun hamlet-type-check-string (checker source &key filename)
  "Elaborate SOURCE with CHECKER and advance its static session on success.
Returns HaMLet's description of the new bindings; signals
SML-STATIC-TYPE-ERROR, leaving the session unchanged, if SOURCE is rejected."
  (let ((package (hamlet-type-checker-package checker))
        (diagnostics (make-string-output-stream)))
    (handler-case
        (with-sml-package (package)
          (let* ((source-pair
                   (list :tuple
                         (if filename
                             (sml-some-value (namestring filename))
                             (sml-none-value))
                         source))
                 (result (let ((*error-output* diagnostics))
                           (call-sml "ClSmlHamlet.elab"
                                     (list :tuple
                                           (hamlet-type-checker-argument checker)
                                           source-pair)))))
            (setf (hamlet-type-checker-argument checker) (sml-tuple-first result)
                  (hamlet-type-checker-last-description checker)
                  (sml-tuple-second result))))
      (sml-raised-exception (cause)
        (let ((message (string-trim '(#\Space #\Newline)
                                    (get-output-stream-string diagnostics))))
          (error 'sml-static-type-error
                 :source source
                 :filename filename
                 :cause (if (string= message "") cause message)))))))

(defmethod type-check-sml-string-using ((checker hamlet-type-checker) source
                                        &key filename)
  (hamlet-type-check-string checker source :filename filename))
