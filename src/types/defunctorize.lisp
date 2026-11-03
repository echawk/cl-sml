(in-package #:cl-sml.types)

;;; Defunctorisation (docs/IR.md, "Functors in the Coalton backend";
;;; ROADMAP item 21).  With cl-sml:*sml-defunctorize*, each functor
;;; application is replaced, in the AST, by a copy of the functor's body,
;;; which the checker then elaborates like any structure: the copy has
;;; concrete types, its own datatypes and exception constructors (each copy
;;; is elaborated anew, so generativity holds), and the code generators
;;; compile it as an ordinary structure.  `structure S = F (A)' becomes
;;;
;;;   structure F%argK = A : PARAMSIG       (transparent: the realisation)
;;;   structure S = BODY' : RESULTSIG       (or :>, as the functor says)
;;;
;;; where BODY' is the body with the parameter's occurrences renamed to
;;; F%argK.  The body's other free identifiers must keep the meaning they
;;; have where the functor is defined, although the application may be in
;;; the scope of later bindings of the same names.  So, when the functor is
;;; elaborated, every identifier occurrence is recorded with what it
;;; resolves to (a FUNCTOR-SCAN):
;;;
;;; - the parameter: renamed in each copy;
;;; - a binding of the program (not of the Basis) in the definition
;;;   environment: the declaration `structure F%env = struct ... end',
;;;   inserted right after the functor, rebinds it (`val x = x',
;;;   `datatype t = datatype t', `exception E = E', `structure A = A', ...),
;;;   and copies refer to it as F%env.x, which nothing can shadow;
;;; - anything else outside the body (the Basis, signatures, functors):
;;;   checked where each copy is elaborated; a copy that would see another
;;;   binding of the name is an error.
;;;
;;; Names with `%' cannot be written in SML, so the added structures cannot
;;; clash with the program's.  A functor's body itself is not expanded: its
;;; applications of other functors are expanded in each copy.

(defstruct (functor-scan (:constructor make-functor-scan (def-env)))
  def-env                               ; the definition environment
  param                                 ; the parameter's name, or NIL (spec)
  param-env                             ; the parameter signature's env
  ;; (node namespace name class object) for each occurrence, CLASS being
  ;; :param, :param-spec, :capture or :check.
  records)

(defstruct functor-template
  name
  param                                 ; strid, or NIL for funid (spec)
  param-sigexp
  result-sigexp
  opaque
  body                                  ; the body's strexp
  records                               ; see FUNCTOR-SCAN
  env-name)                             ; the F%env structure, or NIL

(defun env-binding (env namespace name)
  (cdr (assoc name (ecase namespace
                     (:vid (env-ve env))
                     (:tycon (env-te env))
                     (:strid (env-se env))
                     (:sigid (env-g env))
                     (:funid (env-f env)))
              :test #'string=)))

(defun record-functor-identifier (scan env node name namespace)
  "Record how identifier NAME, at NODE of the functor body being scanned,
resolves in ENV."
  (multiple-value-bind (namespace name)
      (if (and (member namespace '(:vid :tycon :strid)) (long-name-p name))
          (values :strid (first (split-long-name name)))
          (values namespace name))
    (let* ((object (env-binding env namespace name))
           (param-env (functor-scan-param-env scan))
           (def-env (functor-scan-def-env scan))
           (class
             (cond ((null object) nil)
                   ((and (functor-scan-param scan) (eq namespace :strid)
                         (string= name (functor-scan-param scan))
                         (eq object param-env))
                    :param)
                   ((and param-env (null (functor-scan-param scan))
                         (member namespace '(:vid :tycon :strid))
                         (eq object (env-binding param-env namespace name)))
                    :param-spec)
                   ((not (eq object (env-binding def-env namespace name))) nil)
                   ((and (member namespace '(:vid :tycon :strid))
                         (not (eq object (env-binding (initial-env) namespace name))))
                    :capture)
                   (t :check))))
      (when class
        (push (list node namespace name class object) (functor-scan-records scan))))))

;;;; The definition environment

(defun capture-declaration (scan env-name)
  "The declaration of structure ENV-NAME that rebinds the program's
bindings the functor body refers to, or NIL if there are none.  An
occurrence that cannot be rebound so (a constructor whose datatype has no
name in scope) is checked instead."
  (let ((def-env (functor-scan-def-env scan))
        (seen nil)
        (datatypes nil) (types nil) (exceptions nil) (vals nil) (structures nil))
    (labels ((replicate (tycon)
               (unless (member tycon datatypes :test #'string=)
                 (push tycon datatypes)))
             (capture (record)
               (destructuring-bind (node namespace name class object) record
                 (declare (ignore node class))
                 (ecase namespace
                   (:strid (push name structures))
                   (:tycon
                    (if (tystr-ve object)
                        (replicate name)
                        (push (cons name (tyfcn-arity (tystr-tyfcn object))) types)))
                   (:vid
                    (ecase (valstr-status object)
                      (:v (push name vals))
                      (:e (push name exceptions))
                      (:c (let ((tycon (loop for (tycon . tystr) in (env-latest (env-te def-env))
                                             when (eq (cdr (assoc name (tystr-ve tystr)
                                                                  :test #'string=))
                                                      object)
                                               return tycon)))
                            (if tycon
                                (replicate tycon)
                                (setf (fourth record) :check))))))))))
      (dolist (record (reverse (functor-scan-records scan)))
        (when (eq (fourth record) :capture)
          (let ((key (cons (second record) (third record))))
            (if (member key seen :test #'equal)
                nil
                (progn (push key seen)
                       (capture record))))))
      (let ((decs (append
                   (loop for tycon in (reverse datatypes)
                         collect (list :datatype-replication tycon tycon))
                   (loop for (tycon . arity) in (reverse types)
                         for tyvars = (loop for i from 1 to arity
                                            collect (format nil "'a~D" i))
                         collect (list :type tycon
                                       (list :ty-con tycon (mapcar (lambda (tyvar)
                                                                     (list :ty-var tyvar))
                                                                   tyvars))
                                       :tyvars tyvars))
                   (loop for name in (reverse exceptions)
                         collect (list :exception-alias name name))
                   (loop for name in (reverse vals)
                         collect (list :val (list :pat-var name) (list :var name)))
                   (loop for name in (reverse structures)
                         collect (cl-sml::sml-structure-binding-node
                                  name nil (list :str-id name))))))
        (and decs
             (cl-sml::sml-structure-binding-node env-name nil (list :struct decs)))))))

(defun functor-template (scan node param-sigexp)
  "The template of functor declaration NODE, whose elaboration SCAN
recorded.  The structure of its definition environment is to be declared
next (*PENDING-TOP-LEVEL-DECS*)."
  (let* ((env-name (format nil "~A%env" (second node)))
         (capture (capture-declaration scan env-name)))
    (when capture
      (setf *pending-top-level-decs* (append *pending-top-level-decs* (list capture))))
    (make-functor-template :name (second node)
                           :param (functor-scan-param scan)
                           :param-sigexp param-sigexp
                           :result-sigexp (node-key node :result-sigexp)
                           :opaque (node-key node :opaque)
                           :body (node-key node :strexp)
                           :records (functor-scan-records scan)
                           :env-name (and capture env-name))))

;;;; Copies

(defun copy-functor-ast (tree memo)
  "A copy of TREE (conses copied, sharing kept: MEMO maps each original
cons to its copy), with the source positions of the originals."
  (labels ((copy (x)
             (cond ((atom x) x)
                   ((gethash x memo))
                   (t (let ((new (cons nil nil)))
                        (setf (gethash x memo) new
                              (car new) (copy (car x))
                              (cdr new) (copy (cdr x)))
                        (dolist (table (list cl-sml::*sml-ast-positions*
                                             cl-sml::*sml-declaration-positions*))
                          (when table
                            (multiple-value-bind (position presentp) (gethash x table)
                              (when presentp
                                (setf (gethash new table) position)))))
                        new)))))
    (copy tree)))

(defun rename-identifier (node name new-name)
  "In identifier occurrence NODE, replace the identifier that is NAME, or a
long one that starts with structure NAME, by (NEW-NAME identifier)."
  (flet ((renamed (identifier)
           (if (and (stringp identifier)
                    (or (string= identifier name)
                        (and (> (length identifier) (length name))
                             (string= name identifier :end2 (length name))
                             (char= (char identifier (length name)) #\.))))
               (funcall new-name identifier)
               identifier)))
    (case (car node)
      ((:exception-alias :datatype-replication :spec-datatype-replication)
       (setf (third node) (renamed (third node))))
      (:open (setf (second node) (mapcar #'renamed (second node))))
      (t (let ((new (renamed (second node))))
           (unless (eq new (second node))
             (setf (second node) new)
             (when (and (eq (car node) :pat-var) (long-name-p new))
               (setf (car node) :pat-ctor))))))))

(defun refresh-structure-bindings (tree)
  "Make the structure binding nodes in TREE agree with their :STREXP again
(their other fields repeat parts of it for the compiler)."
  (let ((seen (make-hash-table :test 'eq))
        (bindings nil))
    (labels ((walk (x)
               (when (and (consp x) (not (gethash x seen)))
                 (setf (gethash x seen) t)
                 (when (and (member (car x) '(:structure :structure-alias :structure-app))
                            (stringp (second x))
                            (member :strexp x))
                   (push x bindings))
                 (walk (car x))
                 (walk (cdr x)))))
      (walk tree))
    (dolist (binding bindings)
      (let ((new (cl-sml::sml-structure-binding-node (second binding) nil
                                                     (node-key binding :strexp))))
        (when (and-continuation-p binding)
          (setf new (append new (list :and t))))
        (setf (car binding) (car new)
              (cdr binding) (cdr new))))))

(defun instantiate-functor (template argument)
  "A copy of TEMPLATE's functor applied to the structure expression
ARGUMENT: the declaration of the argument's structure and the body's
structure expression."
  (let* ((parameter (format nil "~A%arg~D" (functor-template-name template)
                            (incf *functor-instances*)))
         (memo (make-hash-table :test 'eq))
         (copy (copy-functor-ast (list (functor-template-param-sigexp template)
                                       (functor-template-result-sigexp template)
                                       (functor-template-body template))
                                 memo)))
    (loop for (node nil name class) in (functor-template-records template)
          for new = (gethash node memo)
          when new
            do (case class
                 (:param (rename-identifier new name
                                            (lambda (id)
                                              (concatenate 'string parameter
                                                           (subseq id (length name))))))
                 (:param-spec (rename-identifier new name
                                                 (lambda (id)
                                                   (format nil "~A.~A" parameter id))))
                 (:capture (rename-identifier new name
                                              (lambda (id)
                                                (format nil "~A.~A"
                                                        (functor-template-env-name template)
                                                        id))))))
    (refresh-structure-bindings copy)
    (destructuring-bind (param-sigexp result-sigexp body) copy
      (values (cl-sml::sml-structure-binding-node
               parameter nil (list :str-constraint argument param-sigexp :opaque nil))
              (if result-sigexp
                  (list :str-constraint body result-sigexp
                        :opaque (functor-template-opaque template))
                  body)))))

(defun check-functor-instance (c template node)
  "Signal an error if an identifier free in TEMPLATE's functor and not
rebound by its F%env structure means something else in context C, where
the copy NODE of its body is elaborated."
  (loop for (nil namespace name class object) in (functor-template-records template)
        when (and (eq class :check)
                  (not (eq (env-binding (context-env c) namespace name) object)))
          do (elab-error node "functor ~A refers to ~(~A~) ~A, which is rebound where ~
it is applied (not supported when functors are specialised)"
                         (functor-template-name template)
                         (ecase namespace
                           (:vid "identifier") (:tycon "type") (:strid "structure")
                           (:sigid "signature") (:funid "functor"))
                         name)))

;;;; Expansion

(defun specialise-strexp (c strexp)
  "STREXP with its functor applications replaced by copies of the
functors' bodies, and the declarations to make before it (the
arguments' structures)."
  (case (car strexp)
    (:str-app
     (let* ((funsig (env-binding (context-env c) :funid (second strexp)))
            (template (and funsig (funsig-template funsig))))
       (if (null template)
           (values nil strexp)
           (multiple-value-bind (argument body) (instantiate-functor template (third strexp))
             (multiple-value-bind (hoisted body) (specialise-strexp c body)
               (push template (gethash body *functor-instance-checks*))
               (values (cons argument hoisted) body))))))
    (:str-constraint
     (multiple-value-bind (hoisted inner) (specialise-strexp c (second strexp))
       (values hoisted
               (if (eq inner (second strexp))
                   strexp
                   (list* :str-constraint inner (cddr strexp))))))
    (:str-let
     ;; The arguments' structures go with the declarations they may use.
     (multiple-value-bind (hoisted inner) (specialise-strexp c (third strexp))
       (values nil
               (if (eq inner (third strexp))
                   strexp
                   (list :str-let (append (second strexp) hoisted) inner)))))
    (t (values nil strexp))))

(defun expand-functor-applications (c cell)
  "Replace the functor applications in the structure bindings of the group
of declarations at CELL of a declaration list (in place) by copies of the
functors' bodies, preceded by their arguments' structures.  Returns true
if the declarations changed."
  (let ((group (dec-group-at cell))
        (hoisted nil)
        (new-group nil)
        (changed nil))
    (dolist (node group)
      (let ((strexp (and (member (car node) '(:structure :structure-alias :structure-app))
                         (node-key node :strexp))))
        (multiple-value-bind (decs new-strexp) (if strexp
                                                   (specialise-strexp c strexp)
                                                   (values nil nil))
          (if (eq new-strexp strexp)
              (push node new-group)
              (let ((new (cl-sml::sml-structure-binding-node (second node) nil new-strexp)))
                (setf changed t
                      hoisted (append hoisted decs))
                (push (if (and-continuation-p node) (append new (list :and t)) new)
                      new-group))))))
    (when changed
      (let ((new (append hoisted (nreverse new-group)))
            (position (and cl-sml::*sml-declaration-positions*
                           (gethash (first group) cl-sml::*sml-declaration-positions*))))
        (when position
          (setf (gethash (first new) cl-sml::*sml-declaration-positions*) position))
        (setf (cdr cell) (append (rest new) (nthcdr (length group) cell))
              (car cell) (first new))))
    changed))
