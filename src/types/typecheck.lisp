(in-package #:cl-sml.types)

;;; Entry points of the native checker.  Opt-in: the compiler still takes
;;; its static facts from HaMLet (src/type-checker.lisp).

(defun typecheck-sml-string (source &key filename (env (initial-env)))
  "Elaborate the SML program SOURCE (Core only, milestone M1) in ENV, by
default the initial environment.  Returns HaMLet's printed form of the
bindings it makes (e.g. \"val x : int\"), the environment ENV extended with
them, the annotation table (see annotations.lisp) and the match warnings
as (line column message).  Signals SML-NATIVE-TYPE-ERROR, with the source
position and the clashing types, if SOURCE does not elaborate."
  (let* ((cl-sml::*sml-ast-positions* (cl-sml::make-sml-ast-position-table))
         (cl-sml::*sml-declaration-positions* (make-hash-table :test 'eq))
         (*source* source)
         (*filename* filename)
         (*annotations* (make-hash-table :test 'eq))
         (*warnings* nil)
         (program (parse-sml-program source
                                     :source-name filename
                                     :scopes (list cl-sml::*sml-builtin-scope*)))
         (increment (elab-top-level-decs (make-context env) (rest program))))
    (setf *native-annotations* *annotations*)
    (values (describe-env increment)
            (env-plus env increment)
            *annotations*
            (reverse *warnings*))))

(defun typecheck-sml-file (pathname &rest options &key env)
  "TYPECHECK-SML-STRING on the contents of the file PATHNAME."
  (declare (ignore env))
  (apply #'typecheck-sml-string (uiop:read-file-string pathname)
         :filename (namestring pathname) options))
