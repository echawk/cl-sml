(in-package #:cl-sml)

;;; The IR's types from the native checker's (docs/IR.md, "Types").  The
;;; IR's type representation (IR-TYCON and the type lists, src/ir.lisp)
;;; does not refer to the checker's objects: this file is the one place
;;; that converts them.

(defvar *ir-tycons* (make-portable-weak-eq-hash-table)
  "The checker's type names -> their IR-TYCONs, so that each type name has
one.")

(defun ir-tycon-of-tyname (tyname)
  (or (gethash tyname *ir-tycons*)
      (setf (gethash tyname *ir-tycons*)
            (make-ir-tycon :name (cl-sml.types::tyname-name tyname)
                           :arity (cl-sml.types::tyname-arity tyname)
                           :equality (cl-sml.types::tyname-equality tyname)
                           :constructors (cl-sml.types::tyname-constructors tyname)
                           :stamp (cl-sml.types::tyname-stamp tyname)))))

(defun ir-type-from-checker (type)
  "The IR type of the checker's TYPE (final: after the enclosing top-level
declaration has been elaborated).  An overloaded type variable left
unresolved stands for its class's default, as the checker defaults it."
  (let ((type (cl-sml.types::repr type)))
    (cond
      ((null type) nil)
      ((cl-sml.types::tvar-p type)
       (let ((class (cl-sml.types::tvar-overload type)))
         (if class
             (list :con (ir-tycon-of-tyname (cl-sml.types::oclass-default class)))
             (list :tyvar (cl-sml.types::tvar-id type)
                   (cl-sml.types::tvar-name type)
                   (and (cl-sml.types::tvar-equality type) t)))))
      (t
       (ecase (car type)
         (:con (list* :con (ir-tycon-of-tyname (second type))
                      (mapcar #'ir-type-from-checker (cddr type))))
         (:arrow (list :arrow (ir-type-from-checker (second type))
                       (ir-type-from-checker (third type))))
         (:record
          (multiple-value-bind (fields row) (cl-sml.types::expand-record type)
            (list :record
                  (mapcar (lambda (field)
                            (cons (car field) (ir-type-from-checker (cdr field))))
                          fields)
                  (and row t)))))))))

(defun ir-checker-node-type (ast)
  "The IR type the native checker gives expression or pattern AST, or NIL."
  (let ((type (cl-sml.types:native-node-type ast *sml-static-facts*)))
    (and type (ir-type-from-checker type))))

(defun ir-checker-base-type (name)
  "The IR type of the built-in type NAME (\"int\", \"bool\", ...)."
  (let ((tyname (cond ((string= name "int") cl-sml.types::*t-int*)
                      ((string= name "word") cl-sml.types::*t-word*)
                      ((string= name "real") cl-sml.types::*t-real*)
                      ((string= name "string") cl-sml.types::*t-string*)
                      ((string= name "char") cl-sml.types::*t-char*)
                      ((string= name "bool") cl-sml.types::*t-bool*)
                      ((string= name "exn") cl-sml.types::*t-exn*)
                      ((string= name "list") cl-sml.types::*t-list*)
                      ((string= name "ref") cl-sml.types::*t-ref*)
                      ((string= name "vector") cl-sml.types::*t-vector*))))
    (and tyname (list :con (ir-tycon-of-tyname tyname)))))

;;; Declarations (IR-DECLARATIONS-OF): what the checker's environments say
;;; about the types and constructors a declaration binds.

(defun ir-checker-annotation (node key)
  (and (native-sml-static-facts-p)
       (cl-sml.types:native-annotation node key *sml-static-facts*)))

(defun ir-checker-scheme-type (valstr)
  (ir-type-from-checker
   (cl-sml.types::scheme-type (cl-sml.types::valstr-scheme valstr))))

(defun ir-checker-type-bindings (env)
  "The types ENV binds: ((name arity kind params body constructors) ...),
where PARAMS are type variables, BODY the type the name stands for (for a
datatype or an abstract type, (:con tycon . params)), and CONSTRUCTORS
((name type) ...) with each constructor's type (an arrow from its
argument for a constructor with one)."
  (loop for (name . tystr) in (cl-sml.types::env-latest (cl-sml.types::env-te env))
        collect (let* ((tyfcn (cl-sml.types::tystr-tyfcn tystr))
                       (params (cl-sml.types::tyfcn-params tyfcn)))
                  (list name (length params) (cl-sml.types::tystr-kind tystr)
                        (mapcar #'ir-type-from-checker params)
                        (ir-type-from-checker (cl-sml.types::tyfcn-body tyfcn))
                        (loop for (con . valstr)
                                in (cl-sml.types::env-latest (cl-sml.types::tystr-ve tystr))
                              collect (list con (ir-checker-scheme-type valstr)))))))

(defun ir-checker-value-bindings (env)
  "The values ENV binds: ((name status type) ...)."
  (loop for (name . valstr) in (cl-sml.types::env-latest (cl-sml.types::env-ve env))
        collect (list name (cl-sml.types::valstr-status valstr)
                      (ir-checker-scheme-type valstr))))

(defun ir-checker-declaration-env (dec key)
  "The type and value bindings of the environment the checker recorded
under KEY (:ENV, or :MEMBERS for a structure after ascription) on DEC."
  (let ((env (ir-checker-annotation dec key)))
    (and env (cl-sml.types::env-p env)
         (values (ir-checker-type-bindings env) (ir-checker-value-bindings env)))))
