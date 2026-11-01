(in-package #:cl-sml)

;;; The static checkers of compilation (*SML-TYPE-CHECKER*):
;;;
;;; :NATIVE (the default)  the native checker of src/types/ (package
;;;                        cl-sml.types, DESIGN-TYPECHECKER.md), which
;;;                        elaborates the AST the compiler then compiles,
;;;                        and gives it an annotation table keyed by node;
;;; :HAMLET                HaMLet (the hamlet/ submodule), compiled by
;;;                        cl-sml and loaded on first use; kept as the
;;;                        differential oracle of the native checker;
;;; NIL                    no checking (plain mode): the compiler guesses
;;;                        what it would otherwise learn from a checker.
;;;
;;; Either checker keeps a session per SML package: each successfully
;;; elaborated program extends the static basis that later programs
;;; compiled into the same package are checked against.

;;;; The native checker

(defstruct (native-type-checker (:constructor make-native-type-checker ()))
  ;; SML package name -> the static environment (a CL-SML.TYPES::ENV) of
  ;; that package's session.  A package starts from the Basis.
  (sessions (make-hash-table :test #'equal))
  ;; The printed bindings of the last successful check, as HaMLet prints
  ;; them, e.g. "val x : int\n".
  (last-description "")
  ;; The annotation table of the last successful check.
  (last-annotations nil))

(defvar *native-type-checker* nil
  "The checker :NATIVE stands for, created on first use.")

(defvar *hamlet-type-checker* nil
  "The checker :HAMLET stands for (and ENABLE-HAMLET-TYPE-CHECKER
installs), created on first use.")

(defun native-type-checker ()
  (or *native-type-checker*
      (setf *native-type-checker* (make-native-type-checker))))

(defun resolve-sml-type-checker (checker)
  "The checker object CHECKER, a value of *SML-TYPE-CHECKER*, denotes."
  (case checker
    (:native (native-type-checker))
    (:hamlet (or *hamlet-type-checker*
                 (setf *hamlet-type-checker* (make-hamlet-type-checker))))
    (t checker)))

(defun native-sml-checker-p (checker)
  (native-type-checker-p (resolve-sml-type-checker checker)))

(defun native-session-env (checker &optional (key (hamlet-session-key)))
  (or (gethash key (native-type-checker-sessions checker))
      (cl-sml.types::initial-env)))

(defun report-sml-match-warning (warning filename)
  (format *error-output* "~&~@[~A:~]~A~%" filename warning)
  (muffle-warning warning))

(defun native-check-sml-ast (checker program source &key filename splices)
  "Elaborate PROGRAM, the AST the parser made of SOURCE (its position tables
still bound), in the session of the current SML package, and extend the
session on success.  SPLICES are identifiers bound, for this program only,
to values of unknown type (the splices of #@sml blocks).  Match warnings
go to *ERROR-OUTPUT*.  Returns the description of the new bindings and the
annotation table; signals SML-NATIVE-TYPE-ERROR if PROGRAM is ill-typed."
  (let* ((env (native-session-env checker))
         (cl-sml.types::*annotations* (make-hash-table :test 'eq))
         (cl-sml.types::*warnings* nil)
         (increment
           (handler-bind ((sml-match-warning
                            (lambda (warning) (report-sml-match-warning warning filename))))
             (cl-sml.types::elaborate-ast
              (if splices
                  (cl-sml.types::env-plus-ve
                   env (mapcar (lambda (name)
                                 (cons name (cl-sml.types::make-valstr
                                             (cl-sml.types::mono-scheme
                                              (cl-sml.types::fresh-tvar :unknown t))
                                             :v)))
                               splices))
                  env)
              program source :filename filename)))
         (annotations cl-sml.types::*annotations*))
    (setf (gethash (hamlet-session-key) (native-type-checker-sessions checker))
          (cl-sml.types::env-plus env increment)
          (native-type-checker-last-annotations checker) annotations
          (native-type-checker-last-description checker)
          (cl-sml.types::describe-env increment))
    (values (native-type-checker-last-description checker) annotations)))

(defmethod type-check-sml-string-using ((checker native-type-checker) source
                                        &key filename)
  "Parse SOURCE in the current SML package's parser environment (without
changing it) and check it.  Returns the description of its bindings and
the annotation table."
  (let ((*sml-ast-positions* (make-sml-ast-position-table))
        (*sml-declaration-positions* (make-sml-declaration-position-table))
        (*sml-static-facts* nil))
    (native-check-sml-ast checker
                          (parse-sml-program source :source-name filename)
                          source :filename filename)))

(defun sml-checker-last-description (checker)
  (let ((checker (resolve-sml-type-checker checker)))
    (typecase checker
      (native-type-checker (native-type-checker-last-description checker))
      (hamlet-type-checker (hamlet-type-checker-last-description checker)))))

(defun enable-native-type-checker ()
  "Make the native checker the static checker for compilation, file
loading and the REPL (the default)."
  (setf *sml-type-checker* :native))

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
  (asdf:system-relative-pathname "cl-sml" "src/hamlet-check.sml"))

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

(defun disable-type-checker ()
  "Compile without a static checker (plain mode)."
  (setf *sml-type-checker* nil))

(defun sml-list->list (value)
  "HaMLet facts arrive as an SML list of 5-tuples."
  (mapcar (lambda (tuple)
            (if (and (consp tuple) (eq (car tuple) :tuple))
                (cdr tuple)
                tuple))
          value))

(defun hamlet-session-key ()
  (package-name (ensure-sml-package *sml-package*)))

(defun hamlet-type-checker-argument (checker &optional (key (hamlet-session-key)))
  "HaMLet's state for the session of the SML package named KEY."
  (or (gethash key (hamlet-type-checker-sessions checker))
      (hamlet-type-checker-initial-argument checker)))

(defun (setf hamlet-type-checker-argument)
    (argument checker &optional (key (hamlet-session-key)))
  (setf (gethash key (hamlet-type-checker-sessions checker)) argument))

(defun hamlet-type-check-string (checker source &key filename)
  "Elaborate SOURCE with CHECKER and advance its static session on success.
Returns HaMLet's description of the new bindings and the SML-STATIC-FACTS
the code generator uses; signals SML-STATIC-TYPE-ERROR, leaving the session
unchanged, if SOURCE is rejected."
  (let ((package (hamlet-type-checker-package checker))
        ;; The session of the package being compiled, not HaMLet's own.
        (session (hamlet-session-key))
        (diagnostics (make-string-output-stream)))
    (handler-case
        (with-sml-package (package)
          (let* ((source-pair
                   (list :tuple
                         (if filename
                             (sml-some-value (namestring filename))
                             (sml-none-value))
                         ;; HaMLet's program grammar wants a final `;`,
                         ;; which source files usually lack.  A redundant
                         ;; one is harmless, and appending keeps positions.
                         (concatenate 'string source (string #\Newline) ";")))
                 (result (let ((*error-output* diagnostics))
                           (call-sml "ClSmlHamlet.elab"
                                     (list :tuple
                                           (hamlet-type-checker-argument checker session)
                                           source-pair)))))
            (destructuring-bind (argument description facts) (cdr result)
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

(defun sml-checker-sessions (checker)
  (let ((checker (resolve-sml-type-checker checker)))
    (typecase checker
      (native-type-checker (native-type-checker-sessions checker))
      (hamlet-type-checker (hamlet-type-checker-sessions checker)))))

(defun call-preserving-sml-checker-session (checker thunk &key (always t))
  "Call THUNK, then forget what CHECKER elaborated meanwhile in the session
of the current SML package, e.g. to compile a phrase that is not run.
Unless ALWAYS, forget it only if THUNK exits non-locally (e.g. a REPL
phrase whose evaluation failed, which binds nothing)."
  (let ((sessions (sml-checker-sessions checker)))
    (if sessions
        (let ((key (hamlet-session-key))
              (done nil))
          (multiple-value-bind (argument presentp) (gethash key sessions)
            (unwind-protect (multiple-value-prog1 (funcall thunk)
                              (setf done t))
              (when (or always (not done))
                (if presentp
                    (setf (gethash key sessions) argument)
                    (remhash key sessions))))))
        (funcall thunk))))
