(in-package #:cl-sml)

(defun make-sml-adt (tag &optional payload)
  (cons tag payload))

;; --- Curried Standard Library ---
;; SML functions are auto-curried. Lisp's standard functions are not.
;; We must wrap Lisp's binary operators into curried closures.
(defun sml-+ (a) (lambda (b) (+ a b)))
(defun sml-- (a) (lambda (b) (- a b)))
(defun sml-* (a) (lambda (b) (* a b)))
(defun sml-div (a) (lambda (b) (truncate a b)))
(defun sml-mod (a) (lambda (b) (mod a b)))
(defun sml-/ (a) (lambda (b) (/ a b)))

(defun sml-< (a) (lambda (b) (< a b)))
(defun sml-> (a) (lambda (b) (> a b)))
(defun sml->= (a) (lambda (b) (>= a b)))
(defun sml-<= (a) (lambda (b) (<= a b)))

(defun sml-^ (a) (lambda (b) (concatenate 'string a b)))

(defun sml-andalso (a) (lambda (b) (and a b)))
(defun sml-orelse (a) (lambda (b) (or a b)))
(defun sml-not (v) (not v))

;; (defun sml-if (c) (lambda (t) (lambda (e) (if c t e))))

;; Map SML standard functions to Common Lisp equivalents
(defparameter *sml-env*
  '(("+" . #'sml-+)
    ("-" . #'sml--)
    ("*" . #'sml-*)
    ("div" . #'sml-div)
    ("mod" . #'sml-mod)
    ("^" . #'sml-^)
    ("print" . #'princ)

    (">" . #'sml->)
    ("<" . #'sml-<)
    (">=" . #'sml->=)
    ("<=" . #'sml-<=)

    ("not" . #'sml-not)
    ("true" . t)
    ("false" . nil)))
