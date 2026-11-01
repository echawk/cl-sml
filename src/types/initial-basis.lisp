(in-package #:cl-sml.types)

;;; The initial static basis of the programs the native checker checks:
;;; cl-sml's Basis Library (src/basis), elaborated from its sources in the
;;; order all.sml uses them, on top of the starting basis of initial.lisp,
;;; just as HaMLet's checker builds it (ClSmlHamlet.initialFrom in
;;; t/hamlet-oracle/hamlet-check.sml).  It is elaborated once, on first use, and kept.

(defvar *basis-env* nil
  "The elaborated Basis Library, an ENV, once INITIAL-ENV has built it.")

(defvar *basis-scopes* nil
  "The parser's identifier status and fixity environment after the Basis.")

(defvar *basis-elaboration-seconds* nil)

(defun elaborate-program (env source &key filename scopes)
  "Parse SOURCE in the parser environment SCOPES and elaborate it in ENV.
Returns the environment it binds and the parser environment it leaves."
  (let ((cl-sml::*sml-ast-positions* (cl-sml::make-sml-ast-position-table))
        (cl-sml::*sml-declaration-positions* (make-hash-table :test 'eq)))
    (multiple-value-bind (program scopes)
        (let ((cl-sml::*sml-static-facts* nil))
          (parse-sml-program source :source-name filename :scopes scopes))
      (values (elaborate-ast env program source :filename filename)
              scopes))))

(defun elaborate-ast (env program source &key filename)
  "Elaborate PROGRAM, the (:program dec...) AST the parser made of SOURCE
(its positions in the parser's tables, which must still be bound), in ENV.
Returns the environment it binds."
  (let ((*source* source)
        (*filename* filename))
    (elab-top-level-decs (make-context env) (rest program))))

(defun basis-structure-tynames (env tycon &rest members)
  "The type names TYCON denotes in the top-level structures of ENV that
also have the values MEMBERS."
  (let ((tynames nil))
    (loop for (nil . sub) in (env-se env)
          do (let ((tystr (cdr (assoc tycon (env-te sub) :test #'string=))))
               (when (and tystr
                          (every (lambda (member) (assoc member (env-ve sub) :test #'string=))
                                 members))
                 (let ((tyname (tyfcn-tyname (tystr-tyfcn tystr))))
                   (when (and tyname (zerop (tyname-arity tyname)))
                     (pushnew tyname tynames))))))
    (nreverse tynames)))

(defvar *instance-names* nil
  "Alist from the type names of the Basis's integer, word and real
structures that are not the built-in ones (IntInf.int, Int32.int,
Word32.word, ...) to the name the code generator knows them by: the
structure's name in lower case, e.g. \"intinf\" or \"word32\".")

(defun basis-instance-names (env)
  (let ((names nil)
        (builtin (list *t-int* *t-word* *t-word8* *t-real* *t-char* *t-string*)))
    (loop for (strid . sub) in (reverse (env-latest (env-se env)))
          do (dolist (tycon '("int" "word" "real"))
               (let ((tystr (cdr (assoc tycon (env-te sub) :test #'string=))))
                 (when tystr
                   (let ((tyname (tyfcn-tyname (tystr-tyfcn tystr))))
                     (when (and tyname (zerop (tyname-arity tyname))
                                (not (member tyname builtin))
                                (not (assoc tyname names)))
                       (push (cons tyname (string-downcase strid)) names)))))))
    names))

(defun tyname-instance-name (tyname)
  "The name of TYNAME as an overloading instance (see *INSTANCE-NAMES*)."
  (or (cdr (assoc tyname *instance-names*))
      (tyname-name tyname)))

(defun elaborate-basis (&optional (files (cl-sml::sml-basis-source-files)))
  "Elaborate the Basis Library sources FILES; set *BASIS-ENV* and
*BASIS-SCOPES*.  Then the integer, word and real types of the Basis's
structures (IntInf.int, Int32.int, Word8.word, ...) join the overloading
classes Int, Word and Real: DESIGN-TYPECHECKER.md, section 4, item 3."
  (let ((start (get-internal-real-time))
        (env (starting-env))
        (scopes (list cl-sml::*sml-builtin-scope*))
        (*native-match-warnings* nil)
        (*native-unchecked-long-ids* nil)
        (*annotations* (make-hash-table :test 'eq))
        (*warnings* nil))
    (dolist (file files)
      (clrhash *annotations*)
      (multiple-value-bind (increment new-scopes)
          (elaborate-program env (uiop:read-file-string file
                                                        :external-format cl-sml::*sml-external-format*)
                             :filename (namestring file) :scopes scopes)
        (setf env (env-plus env increment)
              scopes new-scopes)))
    (setf env (env-clean env))
    (extend-overloading-classes
     :ints (basis-structure-tynames env "int" "toInt" "fromInt")
     :words (basis-structure-tynames env "word" "toInt" "fromInt")
     :reals (basis-structure-tynames env "real" "fromInt" "/"))
    (setf *instance-names* (basis-instance-names env))
    (let ((option (cdr (assoc "option" (env-te env) :test #'string=))))
      (setf *t-option* (and option (tyfcn-tyname (tystr-tyfcn option)))))
    (setf *basis-elaboration-seconds*
          (/ (- (get-internal-real-time) start) internal-time-units-per-second)
          *basis-scopes* scopes
          *basis-env* env)))

(defun initial-env ()
  "The initial static basis: the elaborated Basis Library."
  (or *basis-env*
      (progn (elaborate-basis) *basis-env*)))

(defun initial-scopes ()
  (initial-env)
  *basis-scopes*)
