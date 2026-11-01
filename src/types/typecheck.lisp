(in-package #:cl-sml.types)

;;; Entry points of the native checker.  Opt-in: the compiler still takes
;;; its static facts from HaMLet (src/type-checker.lisp).

(defun typecheck-sml-string (source &key filename (env (initial-env))
                                         (scopes (initial-scopes)))
  "Elaborate the SML program SOURCE in ENV, by default the initial basis
(the elaborated Basis Library), parsing it in the parser environment
SCOPES.  Returns HaMLet's printed form of the bindings it makes (e.g.
\"val x : int\"), the environment ENV extended with them, the annotation
table (see annotations.lisp) and the match warnings as (line column
message).  Signals SML-NATIVE-TYPE-ERROR, with the source position and the
clashing types, if SOURCE does not elaborate."
  (let* ((*annotations* (make-hash-table :test 'eq))
         (*warnings* nil)
         (increment (elaborate-program env source :filename filename :scopes scopes)))
    (setf *native-annotations* *annotations*)
    (values (describe-env increment)
            (env-plus env increment)
            *annotations*
            (reverse *warnings*))))

(defun typecheck-sml-file (pathname &rest options &key env scopes)
  "TYPECHECK-SML-STRING on the contents of the file PATHNAME."
  (declare (ignore env scopes))
  (apply #'typecheck-sml-string (uiop:read-file-string pathname)
         :filename (namestring pathname) options))
