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
                      ((string= name "exn") cl-sml.types::*t-exn*))))
    (and tyname (list :con (ir-tycon-of-tyname tyname)))))
