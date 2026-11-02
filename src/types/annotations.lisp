(in-package #:cl-sml.types)

;;; What the native checker tells later passes: an annotation table from
;;; AST node to static information, with lookups in the style of
;;; src/static-facts.lisp, so that the compiler can later take its facts
;;; from here instead of HaMLet (milestone M3).  Types are stored as
;;; elaborated and read through their links, so they are final once the
;;; enclosing top-level declaration has been elaborated.
;;;
;;; Keys of a node's plist:
;;;   :status       identifier status "v", "c", "c1", "e", "e1" (variable
;;;                 occurrences in expressions and patterns)
;;;   :type         the instantiated type of an identifier occurrence, or
;;;                 the type of a special constant
;;;   :instances    the types instantiating an identifier's type scheme
;;;   :record       the record type of a `#lab' or `{..., ...}' pattern
;;;   :exhaustive, :redundant   match checks of fn, case, handle, fun and
;;;                 val bindings
;;;   :env          the environment a declaration binds
;;;   :members      the environment of a structure binding, after
;;;                 ascription (the code generator's "str" facts)
;;;   :constructors the constructor environment a datatype replication
;;;                 binds ("dtrep")

(defvar *native-annotations* nil
  "The annotation table (an EQ hash table) of the last native check, as
returned by TYPECHECK-SML-STRING.")

(defun native-annotation (node key &optional (table *native-annotations*))
  (and table (getf (gethash node table) key)))

(defun native-node-type (node &optional (table *native-annotations*))
  "The type of identifier occurrence or constant NODE, or NIL."
  (let ((type (native-annotation node :type table)))
    (and type (zonk type))))

(defun native-overloaded-instance (node &optional (table *native-annotations*))
  "The type name (\"int\", \"word\", \"real\", \"char\", \"string\") at which
the overloaded identifier or constant NODE is used, or NIL; the Basis's
other integer, word and real types are named after their structures, e.g.
\"intinf\" or \"word32\" (see *INSTANCE-NAMES*).  Compare
SML-OVERLOADED-INSTANCE."
  (let ((var (find-if #'tvar-overload
                      (or (native-annotation node :instances table)
                          (let ((type (native-annotation node :type table)))
                            (and (tvar-p type) (list type)))))))
    (when var
      (let ((type (repr var)))
        (and (consp type) (eq (car type) :con)
             (tyname-instance-name (second type)))))))

(defun native-equality-type (node &optional (table *native-annotations*))
  "The type compared by an occurrence NODE of = or <>, or NIL."
  (let ((instances (native-annotation node :instances table)))
    (and instances
         (member (second node) '("=" "<>") :test #'equal)
         (zonk (first instances)))))

(defun native-equality-kind (node &optional (table *native-annotations*))
  "How the code generator can compare the values that occurrence NODE of
= or <> compares: :EQL (integers, words, characters), :EQ (references,
and datatypes whose constructors are all nullary, such as bool, whose
values are symbols), :STRING, or NIL for structural equality."
  (let ((type (native-equality-type node table)))
    (when (and (consp type) (eq (car type) :con))
      (let* ((tyname (second type))
             (instance (cdr (assoc tyname *instance-names*)))
             (constructors (tyname-constructors tyname)))
        (cond ((eq tyname *t-string*) :string)
              ((or (member tyname (list *t-int* *t-word* *t-word8* *t-char*))
                   (and instance
                        (or (eql 0 (search "int" instance))
                            (eql 0 (search "word" instance)))))
               :eql)
              ((eq tyname *t-ref*) :eq)
              ((and constructors (notany #'cdr constructors)) :eq))))))

(defun native-node-representation (node &optional (table *native-annotations*))
  "The run-time representation of the value identifier occurrence NODE
binds or refers to, when its type is a built-in base type: :REAL (a
double-float) or NIL."
  (let ((type (native-node-type node table)))
    (and (consp type) (eq (car type) :con)
         (cond ((eq (second type) *t-real*) :real)))))

(defun native-record-row (node &optional (table *native-annotations*))
  "The full row, alist label -> type, of `#lab' or flexible record pattern
NODE, or NIL."
  (let ((type (native-annotation node :record table)))
    (and type (second (zonk type)))))

(defun native-record-labels (node &optional (table *native-annotations*))
  "The labels of the record type of `#lab' or flexible record pattern NODE,
in canonical order, when its row is closed; else NIL."
  (let ((type (native-annotation node :record table)))
    (when type
      (let ((type (zonk type)))
        (and (null (third type))
             (mapcar #'car (second type)))))))

(defun native-match-info (node &optional (table *native-annotations*))
  "Whether the match of NODE (fn, case, handle, fun, val binding) is
exhaustive, and the 0-based indices of its redundant rules."
  (values (native-annotation node :exhaustive table)
          (native-annotation node :redundant table)))

(defun native-node-fact-info (node kind &optional (table *native-annotations*))
  "The information of KIND about NODE in the encoding of
SML-NODE-FACT-INFO: \"id\" and \"pat\" give the identifier status, \"ov\"
the overloaded instance."
  (cond ((member kind '("id" "pat") :test #'string=)
         (native-annotation node :status table))
        ((string= kind "ov") (native-overloaded-instance node table))
        (t nil)))

(defun ve-members (ve prefix)
  (loop for (vid . valstr) in (env-latest ve)
        collect (cons (status-string valstr) (concatenate 'string prefix vid))))

(defun env-members (env &optional (prefix ""))
  "The value members of ENV and of its structures, as (status . longvid)."
  (append (ve-members (env-ve env) prefix)
          (loop for (strid . sub) in (env-latest (env-se env))
                append (env-members sub (concatenate 'string prefix strid ".")))))

(defun native-structure-members (node &optional (table *native-annotations*))
  "The value members (status . longvid) of the structure that binding NODE
declares, after signature matching, or :UNKNOWN."
  (let ((env (native-annotation node :members table)))
    (if env (env-members env) :unknown)))

(defun native-replicated-constructors (node &optional (table *native-annotations*))
  "The constructors (status . vid) datatype replication NODE binds, or NIL."
  (let ((ve (native-annotation node :constructors table)))
    (and ve (ve-members ve ""))))
