(in-package #:cl-sml.types)

;;; Semantic objects of the Core static semantics (Definition section 4.2;
;;; HaMLet elab/StaticObjectsCore, TyName, Type, TypeFcn, TypeScheme and
;;; OverloadingClass).
;;;
;;; Types are lists, except type variables, which are mutable cells:
;;;   (:con TYNAME ARG...)          type name application, t(tau1..tauk)
;;;   (:arrow DOMAIN RANGE)         tau -> tau'
;;;   (:record FIELDS ROW)          {lab1 : tau1, ...}, FIELDS sorted by
;;;                                 label order; ROW is NIL (a closed row) or
;;;                                 a row variable (a flexible record)
;;;   a TVAR                        a type variable, possibly linked
;;;
;;; This is Algorithm J: unification links type variables destructively
;;; (union-find), and each variable carries the let-nesting level where it
;;; was created, so that generalisation quantifies the variables deeper than
;;; the declaration instead of scanning the context for free variables.

(defvar *stamp* 0)

(defun next-stamp ()
  (incf *stamp*))

;;;; Type names [Section 4.1]

(defstruct (tyname (:constructor %make-tyname))
  (name "" :type string)
  (arity 0 :type fixnum)
  ;; Equality attribute: T, NIL, or :REF for a type name that admits
  ;; equality whatever its arguments (ref, Section 4.4).
  (equality t)
  ;; The constructors of a datatype, (name . takes-argument-p), for match
  ;; checking (Section 4.11); NIL for a type with infinitely many values.
  (constructors nil)
  ;; The level at which a local datatype was declared, to detect a type
  ;; name escaping its scope (side condition of rule 4).
  (level 0 :type fixnum)
  (stamp (next-stamp) :type fixnum))

(defmethod print-object ((tyname tyname) stream)
  (print-unreadable-object (tyname stream :type t)
    (format stream "~A/~D" (tyname-name tyname) (tyname-stamp tyname))))

(defun make-tyname (name arity &key (equality t) constructors (level 0))
  (%make-tyname :name name :arity arity :equality equality
                :constructors constructors :level level))

;;;; Overloading classes [Appendix E]
;;; A class is a set of type names with a default.

(defstruct (oclass (:constructor make-oclass (tynames default)))
  tynames
  default)

(defun oclass-union (class1 class2)
  "HaMLet OverloadingClass.union: the default is CLASS2's."
  (make-oclass (union (oclass-tynames class1) (oclass-tynames class2))
               (oclass-default class2)))

(defun oclass-intersection (class1 class2)
  "The intersection of two classes, or NIL when it is empty.  The default
is the one that survives (HaMLet OverloadingClass.intersection)."
  (let ((tynames (intersection (oclass-tynames class1) (oclass-tynames class2))))
    (cond ((null tynames) nil)
          ((eq (oclass-default class1) (oclass-default class2))
           (make-oclass tynames (oclass-default class1)))
          ((member (oclass-default class1) tynames)
           (make-oclass tynames (oclass-default class1)))
          (t (make-oclass tynames (or (find (oclass-default class2) tynames)
                                      (first tynames)))))))

(defun oclass-equality (class)
  "CLASS restricted to the type names admitting equality, or NIL."
  (let ((tynames (remove-if-not #'tyname-equality (oclass-tynames class))))
    (and tynames
         (make-oclass tynames (if (member (oclass-default class) tynames)
                                  (oclass-default class)
                                  (first tynames))))))

;;;; Type variables

(defvar *level* 0
  "The let-nesting level of the phrase being elaborated.")

(defstruct (tvar (:constructor %make-tvar))
  (link nil)                            ; NIL or the type this variable stands for
  (id (next-stamp) :type fixnum)
  (level 0 :type fixnum)
  (equality nil)                        ; ''a: admits equality only
  (overload nil)                        ; NIL or an OCLASS
  (name nil)                            ; the name of an explicit type variable
  (rigid nil)                           ; explicit type variable (Section 4.6)
  (row nil)                             ; the rest of a flexible record
  (unknown nil))                        ; stands for an unchecked long identifier

(defmethod print-object ((tvar tvar) stream)
  (print-unreadable-object (tvar stream :type t)
    (format stream "~@[~A ~]~D@~D~:[~; linked~]"
            (tvar-name tvar) (tvar-id tvar) (tvar-level tvar) (tvar-link tvar))))

(defun fresh-tvar (&key equality overload row unknown (level *level*))
  (%make-tvar :level level :equality equality :overload overload :row row
              :unknown unknown))

(defun make-rigid-tvar (name &key (level *level*))
  "The explicit type variable NAME ('a or ''a)."
  (%make-tvar :level level :name name :rigid t
              :equality (and (> (length name) 1) (char= (char name 1) #\'))))

(defun repr (type)
  "TYPE with links followed (with path compression)."
  (if (and (tvar-p type) (tvar-link type))
      (let ((target (repr (tvar-link type))))
        (setf (tvar-link type) target)
        target)
      type))

;;;; Type constructors

(defun con-type (tyname &rest args)
  (list* :con tyname args))

(defun arrow-type (domain range)
  (list :arrow domain range))

(defun label< (label1 label2)
  "Label order (HaMLet Lab.compare): numerals numerically, else by string."
  (let ((n1 (ignore-errors (parse-integer label1)))
        (n2 (ignore-errors (parse-integer label2))))
    (if (and n1 n2) (< n1 n2) (string< label1 label2))))

(defun sort-fields (fields)
  (sort (copy-list fields) #'label< :key #'car))

(defun record-type (fields &optional row)
  "The record type with FIELDS, an alist (label . type), and ROW."
  (list :record (sort-fields fields) row))

(defun tuple-type (types)
  (record-type (loop for type in types
                     for i from 1
                     collect (cons (princ-to-string i) type))))

(defun unit-type ()
  (record-type nil))

(defun expand-record (type)
  "The fields and open row variable (or NIL) of record TYPE, following row
variables that unification has linked to further fields."
  (let ((fields (second type))
        (row (third type)))
    (loop while row
          do (let ((target (repr row)))
               (cond ((tvar-p target) (setf row target) (return))
                     (t (setf fields (append fields (second target))
                              row (third target))))))
    (values (sort-fields fields) (and row (repr row)))))

;;;; Type schemes [Section 4.5]

(defstruct (scheme (:constructor make-scheme (vars type)))
  vars                                  ; the bound type variables
  type)

(defun mono-scheme (type)
  (make-scheme nil type))

(defun zonk (type)
  "TYPE with every linked variable replaced by what it stands for, and
record rows flattened."
  (let ((type (repr type)))
    (cond ((tvar-p type) type)
          (t (ecase (car type)
               (:con (list* :con (second type) (mapcar #'zonk (cddr type))))
               (:arrow (list :arrow (zonk (second type)) (zonk (third type))))
               (:record (multiple-value-bind (fields row) (expand-record type)
                          (list :record
                                (mapcar (lambda (field)
                                          (cons (car field) (zonk (cdr field))))
                                        fields)
                                row))))))))

(defun substitute-type (type substitution)
  "TYPE with the variables of SUBSTITUTION, an alist from TVARs, replaced."
  (let ((type (repr type)))
    (cond ((tvar-p type)
           (let ((entry (assoc type substitution)))
             (if entry (cdr entry) type)))
          (t (ecase (car type)
               (:con (list* :con (second type)
                            (mapcar (lambda (arg) (substitute-type arg substitution))
                                    (cddr type))))
               (:arrow (list :arrow (substitute-type (second type) substitution)
                             (substitute-type (third type) substitution)))
               (:record (multiple-value-bind (fields row) (expand-record type)
                          (list :record
                                (mapcar (lambda (field)
                                          (cons (car field)
                                                (substitute-type (cdr field) substitution)))
                                        fields)
                                (and row (let ((new (substitute-type row substitution)))
                                           (if (tvar-p new) new nil)))))))))))

(defun instantiate (scheme)
  "A fresh instance of SCHEME, and the fresh variables for its bound ones."
  (if (null (scheme-vars scheme))
      (values (scheme-type scheme) nil)
      (let* ((fresh (mapcar (lambda (var)
                              (fresh-tvar :equality (tvar-equality var)
                                          :overload (tvar-overload var)))
                            (scheme-vars scheme)))
             (substitution (mapcar #'cons (scheme-vars scheme) fresh)))
        (values (substitute-type (scheme-type scheme) substitution) fresh))))

(defun free-tvars (type &optional acc)
  "The unlinked type variables of TYPE (including row variables), in order
of first occurrence, consed onto ACC in reverse."
  (let ((type (repr type)))
    (cond ((tvar-p type) (if (member type acc) acc (cons type acc)))
          (t (ecase (car type)
               (:con (dolist (arg (cddr type) acc)
                       (setf acc (free-tvars arg acc))))
               (:arrow (free-tvars (third type) (free-tvars (second type) acc)))
               (:record (multiple-value-bind (fields row) (expand-record type)
                          (dolist (field fields)
                            (setf acc (free-tvars (cdr field) acc)))
                          (if (and row (not (member row acc))) (cons row acc) acc))))))))

(defun scheme-free-tvars (scheme)
  (set-difference (free-tvars (scheme-type scheme)) (scheme-vars scheme)))

(defun type-tynames (type &optional acc)
  (let ((type (repr type)))
    (cond ((tvar-p type) acc)
          (t (ecase (car type)
               (:con (let ((acc (adjoin (second type) acc)))
                       (dolist (arg (cddr type) acc)
                         (setf acc (type-tynames arg acc)))))
               (:arrow (type-tynames (third type) (type-tynames (second type) acc)))
               (:record (dolist (field (expand-record type) acc)
                          (setf acc (type-tynames (cdr field) acc)))))))))

;;;; Type functions [Section 4.4]
;;; A type function is (PARAMS . BODY): PARAMS are rigid type variables.

(defun make-tyfcn (params body)
  (cons params body))

(defun tyfcn-params (tyfcn) (car tyfcn))
(defun tyfcn-body (tyfcn) (cdr tyfcn))

(defun tyfcn-arity (tyfcn)
  (length (tyfcn-params tyfcn)))

(defun apply-tyfcn (tyfcn args)
  (if (null (tyfcn-params tyfcn))
      (tyfcn-body tyfcn)
      (substitute-type (tyfcn-body tyfcn) (mapcar #'cons (tyfcn-params tyfcn) args))))

(defun tyfcn-tyname (tyfcn)
  "The type name TYFCN is eta-equivalent to, or NIL."
  (let ((body (repr (tyfcn-body tyfcn))))
    (and (consp body)
         (eq (car body) :con)
         (= (length (cddr body)) (length (tyfcn-params tyfcn)))
         (every (lambda (arg param) (eq (repr arg) param))
                (cddr body) (tyfcn-params tyfcn))
         (second body))))
