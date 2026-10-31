(in-package #:cl-sml.types)

;;; Exhaustiveness and redundancy of matches (Definition section 4.11,
;;; items 2 and 3; HaMLet elab/CheckPattern).  Only warnings, never errors.
;;;
;;; Elaboration translates each pattern into a simple pattern:
;;;   :any                          variable or wildcard
;;;   (:con KEY TYNAME ARG)         constructor; KEY is its name, or for an
;;;                                 exception its identity; ARG a simple
;;;                                 pattern or NIL; TYNAME gives the other
;;;                                 constructors (none for exn: infinite)
;;;   (:const VALUE)                special constant (infinitely many)
;;;   (:record TYPE FIELDS)         record; FIELDS alist label -> pattern,
;;;                                 TYPE gives the full row of a `...' pattern
;;; and the check is the usefulness algorithm (Maranget, "Warnings for
;;; pattern matching", 2007): a rule is redundant if it is not useful after
;;; the rules before it, and a match is exhaustive if a wildcard is not
;;; useful after all its rules.

(defun normalize-spat (spat)
  "SPAT with records turned into (:tuple PATTERN...) over the full row."
  (cond ((eq spat :any) :any)
        ((eq (car spat) :record)
         (let ((labels (mapcar #'car (expand-record (second spat)))))
           (list :tuple (mapcar (lambda (label)
                                  (let ((field (assoc label (third spat) :test #'string=)))
                                    (if field (normalize-spat (cdr field)) :any)))
                                labels))))
        ((eq (car spat) :con)
         (list :con (second spat) (third spat)
               (and (fourth spat) (normalize-spat (fourth spat)))))
        (t spat)))

(defun spat-key (spat)
  (ecase (car spat)
    (:con (second spat))
    (:const spat)))

(defun spat-args (spat)
  (ecase (car spat)
    (:con (if (fourth spat) (list (fourth spat)) nil))
    (:const nil)))

(defun anys (n)
  (make-list n :initial-element :any))

(defun specialize (rows key arity)
  (loop for row in rows
        for head = (first row)
        when (eq head :any)
          collect (append (anys arity) (rest row))
        else when (and (member (car head) '(:con :const)) (equal (spat-key head) key))
               collect (append (spat-args head) (rest row))))

(defun specialize-tuple (rows arity)
  (loop for row in rows
        for head = (first row)
        collect (if (eq head :any)
                    (append (anys arity) (rest row))
                    (append (second head) (rest row)))))

(defun default-rows (rows)
  (loop for row in rows
        when (eq (first row) :any)
          collect (rest row)))

(defun useful-p (rows vector)
  "Is pattern VECTOR useful after the pattern vectors ROWS?"
  (if (null vector)
      (null rows)
      (let ((head (first vector)))
        (cond
          ((eq head :any)
           (let* ((heads (remove :any (mapcar #'first rows)))
                  (tuple (find :tuple heads :key #'car)))
             (cond
               (tuple
                (let ((n (length (second tuple))))
                  (useful-p (specialize-tuple rows n) (append (anys n) (rest vector)))))
               ((and heads (eq (car (first heads)) :con)
                     (tyname-constructors (third (first heads)))
                     (let ((all (tyname-constructors (third (first heads)))))
                       (every (lambda (con)
                                (find (car con) heads :key #'second :test #'equal))
                              all)))
                (some (lambda (con)
                        (let ((arity (if (cdr con) 1 0)))
                          (useful-p (specialize rows (car con) arity)
                                    (append (anys arity) (rest vector)))))
                      (tyname-constructors (third (first heads)))))
               (t (useful-p (default-rows rows) (rest vector))))))
          ((eq (car head) :tuple)
           (let ((n (length (second head))))
             (useful-p (specialize-tuple rows n) (append (second head) (rest vector)))))
          (t
           (let ((args (spat-args head)))
             (useful-p (specialize rows (spat-key head) (length args))
                       (append args (rest vector)))))))))

(defun check-match (rows)
  "Check a match whose rules have the simple pattern vectors ROWS.
Returns whether it is exhaustive and the 0-based indices of redundant rules."
  (let ((rows (mapcar (lambda (row) (mapcar #'normalize-spat row)) rows))
        (redundant nil))
    (loop for (row . more) on rows
          for i from 0
          for before = (subseq rows 0 i)
          unless (useful-p before row)
            do (push i redundant))
    (values (not (useful-p rows (anys (length (first rows)))))
            (nreverse redundant))))
