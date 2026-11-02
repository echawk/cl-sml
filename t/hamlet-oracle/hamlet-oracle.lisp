(in-package #:cl-sml)

;;; The HaMLet oracle (system cl-sml/hamlet-oracle): HaMLet
;;; (https://github.com/rossberg/hamlet, pinned at commit 37275b9, the
;;; hamlet/ submodule), compiled by cl-sml and run as a static checker, the
;;; differential oracle of the native checker (src/types/).  Loading this
;;; system defines the :HAMLET value of *SML-TYPE-CHECKER*,
;;; MAKE-HAMLET-TYPE-CHECKER and ENABLE-HAMLET-TYPE-CHECKER; HaMLet itself
;;; is loaded when the first checker is made.  cl-sml proper does not use
;;; HaMLet.

;;;; HaMLet

(defstruct (hamlet-type-checker
            (:constructor %make-hamlet-type-checker (package initial-argument)))
  ;; The package HaMLet itself is loaded into.
  package
  ;; HaMLet's state for a fresh session: the initial (library) basis.
  initial-argument
  ;; SML package name -> HaMLet state of that package's session.
  (sessions (make-hash-table :test #'equal))
  ;; HaMLet's rendering of the bindings from the last successful check,
  ;; e.g. "val x : int\n".
  (last-description "")
  ;; The SML-STATIC-FACTS of the last successful check.
  (last-facts nil))

(defun default-hamlet-root ()
  (asdf:system-relative-pathname "cl-sml" "hamlet/"))

(defun hamlet-adapter-source ()
  (asdf:system-relative-pathname "cl-sml" "t/hamlet-oracle/hamlet-check.sml"))

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

(defun initialize-hamlet-type-checker (package basis-path)
  "A checker whose sessions start from the static basis of the library in
BASIS-PATH, which HaMLet, loaded into PACKAGE, elaborates (and does not
run; see ClSmlHamlet.initialFrom)."
  (with-sml-package (package)
    (multiple-value-bind (initial-argument diagnostics)
        (call-capturing-hamlet-diagnostics
         (lambda ()
           (call-sml "ClSmlHamlet.initialFrom"
                     (mapcar #'namestring (sml-basis-source-files basis-path)))))
      ;; HaMLet reports an error in a basis file and carries on without the
      ;; file, which would leave every program checked against a partial
      ;; basis.
      (let ((errors (string-trim '(#\Space #\Newline) diagnostics)))
        (unless (string= errors "")
          (error "HaMLet could not elaborate the basis:~%~A" errors)))
      (%make-hamlet-type-checker (package-name (ensure-sml-package package))
                                 initial-argument))))

(defun make-hamlet-type-checker (&key
                                   (package "SML.HAMLET-TYPE-CHECKER")
                                   (hamlet-root (default-hamlet-root))
                                   (basis-path (sml-basis-directory))
                                   (load t))
  "Load HaMLet and return a stateful SML97 static elaboration session.
HaMLet runs on cl-sml's basis, which every SML package sees, and it
elaborates the programs it checks against the basis whose sources are in
BASIS-PATH: by default cl-sml's own, so that the static and the dynamic
basis agree."
  (let* ((root (uiop:ensure-directory-pathname (truename hamlet-root)))
         (hamlet-source (merge-pathnames #P"hamlet.sml" root))
         (*sml-type-checker* nil)
         ;; The Lisp generated for HaMLet itself is not worth the memory
         ;; (some 14 MB) its records would keep alive in every image.
         (*record-sml-lisp-source* nil))
    (unless (probe-file hamlet-source)
      (error "HaMLet sources not found at ~A; run `git submodule update --init`."
             root))
    (when load
      (load-sml-file hamlet-source :package package)
      (load-sml-file (hamlet-adapter-source) :package package))
    (initialize-hamlet-type-checker package basis-path)))


(defun enable-hamlet-type-checker (&rest options)
  "Make HaMLet the default static checker for compilation, file loading and
the REPL.  OPTIONS are passed to MAKE-HAMLET-TYPE-CHECKER when the shared
checker has not been created yet."
  (setf *sml-type-checker*
        (or *hamlet-type-checker*
            (setf *hamlet-type-checker*
                  (apply #'make-hamlet-type-checker options)))))

(defun sml-list->list (value)
  "HaMLet facts arrive as an SML list of 5-tuples."
  (mapcar (lambda (tuple)
            (if (sml-tuple-p tuple)
                (sml-tuple->list tuple)
                tuple))
          value))

(defun hamlet-type-checker-argument (checker &optional (key (sml-session-key)))
  "HaMLet's state for the session of the SML package named KEY."
  (or (gethash key (hamlet-type-checker-sessions checker))
      (hamlet-type-checker-initial-argument checker)))

(defun (setf hamlet-type-checker-argument)
    (argument checker &optional (key (sml-session-key)))
  (setf (gethash key (hamlet-type-checker-sessions checker)) argument))

(defun hamlet-type-check-string (checker source &key filename)
  "Elaborate SOURCE with CHECKER and advance its static session on success.
Returns HaMLet's description of the new bindings and the SML-STATIC-FACTS
the code generator uses; signals SML-STATIC-TYPE-ERROR, leaving the session
unchanged, if SOURCE is rejected."
  (let ((package (hamlet-type-checker-package checker))
        ;; The session of the package being compiled, not HaMLet's own.
        (session (sml-session-key))
        (diagnostics (make-string-output-stream)))
    (handler-case
        (with-sml-package (package)
          (let* ((source-pair
                   (sml-tuple
                         (if filename
                             (sml-some-value (namestring filename))
                             (sml-none-value))
                         ;; HaMLet's program grammar wants a final `;`,
                         ;; which source files usually lack.  A redundant
                         ;; one is harmless, and appending keeps positions.
                         (concatenate 'string source (string #\Newline) ";")))
                 (result (let ((*error-output* diagnostics))
                           (call-sml "ClSmlHamlet.elab"
                                     (sml-tuple
                                           (hamlet-type-checker-argument checker session)
                                           source-pair)))))
            (destructuring-bind (argument description facts) (sml-tuple->list result)
              (setf (hamlet-type-checker-argument checker session) argument
                    (hamlet-type-checker-last-description checker) description
                    (hamlet-type-checker-last-facts checker)
                    (make-sml-static-facts source (sml-list->list facts)))
              (values description
                      (hamlet-type-checker-last-facts checker)))))
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


(defmethod checker-last-description ((checker hamlet-type-checker))
  (hamlet-type-checker-last-description checker))

(defmethod checker-sessions ((checker hamlet-type-checker))
  (hamlet-type-checker-sessions checker))
