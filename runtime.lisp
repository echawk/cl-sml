(in-package #:cl-sml)

;; (defmacro defun-curried (name args &rest body)
;;   "Define a function NAME taking the first ARGs as curried parameters.
;; Assumes ARGS is a plain parameter list (no &optional/&rest/&key).
;; Emits a one-argument function for the first parameter that returns nested
;; lambdas for the remaining parameters and uses BODY as the ultimate body,
;; with the original argument names bound in the innermost lambda."
;;   (unless (and (listp args) (every #'symbolp args))
;;     (error "defun-curried requires a simple list of symbols as ARGS"))
;;   (let* ((first (car args))
;;          (rest-args (cdr args))
;;          ;; build nested lambdas: (lambda (a2) (lambda (a3) ... <inner-body> ...))
;;          (nested (reduce (lambda (arg acc) `(lambda (,arg) ,acc))
;;                          (reverse rest-args)
;;                          :initial-value `(progn ,@body))))
;;     `(defun ,name (,first)
;;        ,(when (and (stringp (car body)) (stringp (caar body)))
;;           ;; if user provided a docstring as first form of body, move it
;;           nil)
;;        ,nested)))

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

(defun sml-cons (a) (lambda (b) (cons a b)))
(defun sml-hd (l) (car l))
(defun sml-tl (l) (cdr l))
(defun sml-length (l) (length l))
(defun sml-null (l) (null l))
(defun sml-rev (l) (reverse l))
(defun sml-map (f) (lambda (l) (mapcar f l)))

(defun foldl (fn init list)
  "Left fold: ((fn (fn init x1) x2) ...)."
  (reduce fn list :from-end nil :initial-value init))

(defun foldr (fn init list)
  "Right fold: (fn x1 (fn x2 ... init))."
  (reduce (lambda (x acc) (funcall fn x acc)) list :from-end t :initial-value init))

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

    ("::" . #'sml-cons)
    ("hd" . #'sml-hd)
    ("tl" . #'sml-tl)
    ("length" . #'sml-length)

    ("not" . #'sml-not)
    ("Math.pi" . pi)
    ("true" . t)
    ("false" . nil)))

