(in-package #:cl-sml)

;;; The static checkers of compilation (*SML-TYPE-CHECKER*):
;;;
;;; :NATIVE (the default)  the native checker of src/types/ (package
;;;                        cl-sml.types, DESIGN-TYPECHECKER.md), which
;;;                        elaborates the AST the compiler then compiles,
;;;                        and gives it an annotation table keyed by node;
;;; :HAMLET                HaMLet, the differential oracle of the native
;;;                        checker: only with the system
;;;                        cl-sml/hamlet-oracle loaded (t/hamlet-oracle/);
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
installs), created on first use by the system cl-sml/hamlet-oracle.")

(defun native-type-checker ()
  (or *native-type-checker*
      (setf *native-type-checker* (make-native-type-checker))))

(defun resolve-sml-type-checker (checker)
  "The checker object CHECKER, a value of *SML-TYPE-CHECKER*, denotes."
  (case checker
    (:native (native-type-checker))
    (:hamlet (or *hamlet-type-checker*
                 (if (fboundp 'make-hamlet-type-checker)
                     (setf *hamlet-type-checker*
                           (funcall 'make-hamlet-type-checker))
                     (error "The :HAMLET checker needs the system ~
cl-sml/hamlet-oracle: (asdf:load-system \"cl-sml/hamlet-oracle\").  It loads ~
HaMLet from the hamlet/ submodule (`git submodule update --init hamlet`)."))))
    (t checker)))

(defun native-sml-checker-p (checker)
  (native-type-checker-p (resolve-sml-type-checker checker)))

(defun native-session-env (checker &optional (key (sml-session-key)))
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
    (setf (gethash (sml-session-key) (native-type-checker-sessions checker))
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

(defgeneric checker-last-description (checker)
  (:method ((checker t)) nil)
  (:method ((checker native-type-checker))
    (native-type-checker-last-description checker)))

(defun sml-checker-last-description (checker)
  (checker-last-description (resolve-sml-type-checker checker)))

(defun enable-native-type-checker ()
  "Make the native checker the static checker for compilation, file
loading and the REPL (the default)."
  (setf *sml-type-checker* :native))

(defun disable-type-checker ()
  "Compile without a static checker (plain mode)."
  (setf *sml-type-checker* nil))

(defun sml-session-key ()
  (package-name (ensure-sml-package *sml-package*)))

(defgeneric checker-sessions (checker)
  (:documentation "CHECKER's sessions: SML package name -> session state.")
  (:method ((checker t)) nil)
  (:method ((checker native-type-checker))
    (native-type-checker-sessions checker)))

(defun sml-checker-sessions (checker)
  (checker-sessions (resolve-sml-type-checker checker)))

(defun call-preserving-sml-checker-session (checker thunk &key (always t))
  "Call THUNK, then forget what CHECKER elaborated meanwhile in the session
of the current SML package, e.g. to compile a phrase that is not run.
Unless ALWAYS, forget it only if THUNK exits non-locally (e.g. a REPL
phrase whose evaluation failed, which binds nothing)."
  (let ((sessions (sml-checker-sessions checker)))
    (if sessions
        (let ((key (sml-session-key))
              (done nil))
          (multiple-value-bind (argument presentp) (gethash key sessions)
            (unwind-protect (multiple-value-prog1 (funcall thunk)
                              (setf done t))
              (when (or always (not done))
                (if presentp
                    (setf (gethash key sessions) argument)
                    (remhash key sessions))))))
        (funcall thunk))))
