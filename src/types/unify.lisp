(in-package #:cl-sml.types)

;;; Unification (HaMLet elab/Type, Type.unify): union-find over TVARs, with
;;; the occurs check, level adjustment, and merging of the attributes a
;;; variable carries: equality (Section 4.4), overloading class (Appendix
;;; E) and the row of a flexible record (Section 4.11).

(define-condition unify-failure (error)
  ((reason :initarg :reason :reader unify-failure-reason))
  (:report (lambda (condition stream)
             (format stream "unification failed: ~A" (unify-failure-reason condition)))))

(defun unify-fail (reason)
  (error 'unify-failure :reason reason))

;;;; Equality [Section 4.4]

(defun admits-equality-p (type)
  "Does TYPE admit equality, without changing any variable?"
  (let ((type (repr type)))
    (cond ((tvar-p type)
           (or (tvar-equality type)
               (not (tvar-rigid type))))
          (t (ecase (car type)
               (:con (case (tyname-equality (second type))
                       (:ref t)
                       ((nil) nil)
                       (t (every #'admits-equality-p (cddr type)))))
               (:arrow nil)
               (:record (every (lambda (field) (admits-equality-p (cdr field)))
                               (expand-record type))))))))

(defun enforce-equality (type)
  "Make TYPE admit equality, constraining its variables, or fail."
  (let ((type (repr type)))
    (cond ((tvar-p type)
           (cond ((tvar-equality type))
                 ((tvar-rigid type)
                  (unify-fail "type variable does not admit equality"))
                 (t
                  (when (tvar-overload type)
                    (let ((class (oclass-equality (tvar-overload type))))
                      (unless class
                        (unify-fail "type does not admit equality"))
                      (setf (tvar-overload type) class)))
                  (setf (tvar-equality type) t))))
          (t (ecase (car type)
               (:con (case (tyname-equality (second type))
                       (:ref nil)
                       ((nil) (unify-fail "type does not admit equality"))
                       (t (mapc #'enforce-equality (cddr type)))))
               (:arrow (unify-fail "function type does not admit equality"))
               (:record (multiple-value-bind (fields row) (expand-record type)
                          (dolist (field fields)
                            (enforce-equality (cdr field)))
                          (when row
                            (setf (tvar-equality row) t)))))))))

;;;; Occurs check and levels

(defun adjust-levels (var type)
  "Check that VAR does not occur in TYPE, lower the levels of TYPE's
variables to VAR's, and check that no type name in TYPE is more local than
VAR (side condition of rule 4)."
  (let ((level (tvar-level var)))
    (labels ((walk (type)
               (let ((type (repr type)))
                 (cond ((eq type var) (unify-fail "circular type"))
                       ((tvar-p type)
                        (when (> (tvar-level type) level)
                          (setf (tvar-level type) level)))
                       (t (ecase (car type)
                            (:con (when (> (tyname-level (second type)) level)
                                    (unify-fail
                                     (format nil "local type ~A would escape its scope"
                                             (tyname-name (second type)))))
                                  (mapc #'walk (cddr type)))
                            (:arrow (walk (second type)) (walk (third type)))
                            (:record (multiple-value-bind (fields row) (expand-record type)
                                       (dolist (field fields) (walk (cdr field)))
                                       (when row (walk row))))))))))
      (walk type))))

;;;; Binding variables

(defun merge-tvars (var target)
  "Link flexible VAR to variable TARGET, merging VAR's attributes into it."
  (when (tvar-rigid target)
    (when (or (tvar-overload var) (tvar-row var))
      (unify-fail "explicit type variable cannot be an overloaded or record type"))
    (when (and (tvar-equality var) (not (tvar-equality target)))
      (unify-fail "type variable does not admit equality"))
    (setf (tvar-level target) (min (tvar-level target) (tvar-level var)))
    (setf (tvar-link var) target)
    (return-from merge-tvars))
  (let ((overload (cond ((null (tvar-overload var)) (tvar-overload target))
                        ((null (tvar-overload target)) (tvar-overload var))
                        (t (or (oclass-intersection (tvar-overload var)
                                                    (tvar-overload target))
                               (unify-fail "incompatible overloaded types")))))
        (equality (or (tvar-equality var) (tvar-equality target))))
    (when (and equality overload)
      (setf overload (or (oclass-equality overload)
                         (unify-fail "type does not admit equality"))))
    (setf (tvar-overload target) overload
          (tvar-equality target) equality
          (tvar-unknown target) (or (tvar-unknown var) (tvar-unknown target))
          (tvar-level target) (min (tvar-level target) (tvar-level var))
          (tvar-link var) target)))

(defun bind-tvar (var type)
  "Link unlinked flexible VAR to TYPE, which is not a variable."
  (when (tvar-rigid var)
    (unify-fail "explicit type variable cannot be instantiated"))
  (when (tvar-row var)
    (unify-fail "record row mismatch"))
  (when (tvar-overload var)
    (unless (and (eq (car type) :con)
                 (null (cddr type))
                 (member (second type) (oclass-tynames (tvar-overload var))))
      (unify-fail "type is not in the overloading class")))
  (adjust-levels var type)
  (when (tvar-equality var)
    (enforce-equality type))
  (setf (tvar-link var) type))

;;;; Unification

(defun unify (type1 type2)
  "Make TYPE1 and TYPE2 equal, or signal UNIFY-FAILURE."
  (let ((type1 (repr type1))
        (type2 (repr type2)))
    (cond ((eq type1 type2))
          ((and (tvar-p type1) (tvar-p type2))
           (cond ((and (tvar-rigid type1) (tvar-rigid type2))
                  (unify-fail "distinct explicit type variables"))
                 ((tvar-rigid type1) (merge-tvars type2 type1))
                 (t (merge-tvars type1 type2))))
          ((tvar-p type1) (bind-tvar type1 type2))
          ((tvar-p type2) (bind-tvar type2 type1))
          ((not (eq (car type1) (car type2)))
           (unify-fail "type constructor clash"))
          (t (ecase (car type1)
               (:con (unless (eq (second type1) (second type2))
                       (unify-fail "type constructor clash"))
                     (mapc #'unify (cddr type1) (cddr type2)))
               (:arrow (unify (second type1) (second type2))
                       (unify (third type1) (third type2)))
               (:record (unify-records type1 type2)))))))

(defun bind-row (row fields level-row)
  "Link row variable ROW to the further FIELDS and LEVEL-ROW (NIL or a row
variable)."
  (let ((rest (list :record fields level-row)))
    (adjust-levels row rest)
    (when (tvar-equality row)
      (enforce-equality rest))
    (setf (tvar-link row) rest)))

(defun unify-records (type1 type2)
  (multiple-value-bind (fields1 row1) (expand-record type1)
    (multiple-value-bind (fields2 row2) (expand-record type2)
      (let ((extra1 (remove-if (lambda (field) (assoc (car field) fields2 :test #'string=))
                               fields1))
            (extra2 (remove-if (lambda (field) (assoc (car field) fields1 :test #'string=))
                               fields2)))
        (when (or (and extra1 (null row2)) (and extra2 (null row1)))
          (unify-fail "record label mismatch"))
        (cond ((and row1 row2 (eq row1 row2)))
              ((and row1 row2)
               (let ((rest (fresh-tvar :row t
                                       :level (min (tvar-level row1) (tvar-level row2))
                                       :equality (or (tvar-equality row1)
                                                     (tvar-equality row2)))))
                 (bind-row row1 extra2 rest)
                 (bind-row row2 extra1 rest)))
              (row1 (bind-row row1 extra2 nil))
              (row2 (bind-row row2 extra1 nil)))
        (dolist (field fields1)
          (let ((other (assoc (car field) fields2 :test #'string=)))
            (when other
              (unify (cdr field) (cdr other)))))))))
