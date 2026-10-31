(in-package #:cl-sml.types)

;;; Implicit scoping of explicit type variables (Definition section 4.6;
;;; HaMLet elab/ScopeTyVars).  A type variable occurring unguarded in a
;;; value declaration (not inside a nested value declaration) and not
;;; already in scope is bound by that declaration.

(defun ty-tyvars (ty &optional acc)
  "The type variable names of type expression TY."
  (if (atom ty)
      acc
      (ecase (car ty)
        (:ty-var (adjoin (second ty) acc :test #'string=))
        (:ty-con (dolist (arg (third ty) acc) (setf acc (ty-tyvars arg acc))))
        (:ty-tuple (dolist (item (rest ty) acc) (setf acc (ty-tyvars item acc))))
        (:ty-arrow (ty-tyvars (third ty) (ty-tyvars (second ty) acc)))
        (:ty-record (dolist (field (rest ty) acc) (setf acc (ty-tyvars (second field) acc)))))))

(defun pat-unguarded-tyvars (pat &optional acc)
  (if (atom pat)
      acc
      (case (car pat)
        (:pat-typed (ty-tyvars (third pat) (pat-unguarded-tyvars (second pat) acc)))
        (:pat-app (pat-unguarded-tyvars (third pat) acc))
        ((:pat-as :pat-cons)
         (pat-unguarded-tyvars (third pat) (pat-unguarded-tyvars (second pat) acc)))
        (:pat-tuple (dolist (item (rest pat) acc) (setf acc (pat-unguarded-tyvars item acc))))
        (:pat-record (dolist (field (rest pat) acc)
                       (unless (eq field :record-rest)
                         (setf acc (pat-unguarded-tyvars (second field) acc)))))
        (t acc))))

(defun match-unguarded-tyvars (rules acc)
  (dolist (rule rules acc)
    (setf acc (exp-unguarded-tyvars (second rule) (pat-unguarded-tyvars (first rule) acc)))))

(defun exp-unguarded-tyvars (exp &optional acc)
  (if (atom exp)
      acc
      (case (car exp)
        (:typed (ty-tyvars (third exp) (exp-unguarded-tyvars (second exp) acc)))
        ((:app :andalso :orelse :while)
         (exp-unguarded-tyvars (third exp) (exp-unguarded-tyvars (second exp) acc)))
        (:if (exp-unguarded-tyvars (fourth exp)
                                   (exp-unguarded-tyvars (third exp)
                                                         (exp-unguarded-tyvars (second exp) acc))))
        ((:tuple :list :seq) (dolist (item (rest exp) acc)
                               (setf acc (exp-unguarded-tyvars item acc))))
        (:record (dolist (field (rest exp) acc)
                   (setf acc (exp-unguarded-tyvars (second field) acc))))
        (:let (let ((acc (decs-unguarded-tyvars (second exp) acc)))
                (dolist (item (third exp) acc)
                  (setf acc (exp-unguarded-tyvars item acc)))))
        (:case (match-unguarded-tyvars (cddr exp) (exp-unguarded-tyvars (second exp) acc)))
        (:fn (match-unguarded-tyvars (second exp) acc))
        (:handle (match-unguarded-tyvars (third exp) (exp-unguarded-tyvars (second exp) acc)))
        (:raise (exp-unguarded-tyvars (second exp) acc))
        (t acc))))

(defun decs-unguarded-tyvars (decs acc)
  "Declarations other than value declarations contribute the type
variables of exception bindings (ScopeTyVars.unguardedTyVarsDec)."
  (dolist (dec decs acc)
    (case (car dec)
      (:exception (let ((ty (getf (cddr dec) :arg-type)))
                    (when ty (setf acc (ty-tyvars ty acc)))))
      (:local (setf acc (decs-unguarded-tyvars (third dec)
                                               (if (getf (cdddr dec) :abstype)
                                                   acc
                                                   (decs-unguarded-tyvars (second dec) acc))))))))

(defun valbind-unguarded-tyvars (plain recursive)
  "The unguarded type variables of a value declaration: PLAIN bindings
(pat exp) and RECURSIVE bindings (name exp) or (name :clauses clauses)."
  (let ((acc nil))
    (loop for (pat exp) in plain
          do (setf acc (exp-unguarded-tyvars exp (pat-unguarded-tyvars pat acc))))
    (loop for (nil kind body) in recursive
          do (if (eq kind :clauses)
                 (loop for (params body-exp) in body
                       do (dolist (param params)
                            (setf acc (pat-unguarded-tyvars param acc)))
                          (setf acc (exp-unguarded-tyvars body-exp acc)))
                 (setf acc (exp-unguarded-tyvars body acc))))
    (reverse acc)))
