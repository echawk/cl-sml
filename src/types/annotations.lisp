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
the overloaded identifier or constant NODE is used, or NIL (compare
SML-OVERLOADED-INSTANCE)."
  (let ((var (find-if #'tvar-overload
                      (or (native-annotation node :instances table)
                          (let ((type (native-annotation node :type table)))
                            (and (tvar-p type) (list type)))))))
    (when var
      (let ((type (repr var)))
        (and (consp type) (eq (car type) :con) (tyname-name (second type)))))))

(defun native-equality-type (node &optional (table *native-annotations*))
  "The type compared by an occurrence NODE of = or <>, or NIL."
  (let ((instances (native-annotation node :instances table)))
    (and instances
         (member (second node) '("=" "<>") :test #'equal)
         (zonk (first instances)))))

(defun native-record-row (node &optional (table *native-annotations*))
  "The full row, alist label -> type, of `#lab' or flexible record pattern
NODE, or NIL."
  (let ((type (native-annotation node :record table)))
    (and type (second (zonk type)))))

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
