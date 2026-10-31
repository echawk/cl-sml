;;;; Embedded SML in a Lisp file.
;;;;
;;;; Load it with cl-sml loaded:
;;;;   (load (compile-file "examples/embedded-sml.lisp"))
;;;; The SML is compiled when the file is compiled; loading the FASL does
;;;; not compile SML again.

(defpackage #:sml-showcase
  (:use #:cl))
(in-package #:sml-showcase)

;;; SML syntax for the rest of this file.  (named-readtables users can
;;; write (named-readtables:in-readtable cl-sml:sml-readtable) instead.)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (cl-sml:enable-sml-syntax (copy-readtable))))

;;; Declarations define bindings in this package's SML package,
;;; SML.SML-SHOWCASE; in Lisp, a value `name' is the variable |name| there.
#@sml{
  datatype shape = Circle of real | Rect of real * real

  fun area (Circle r) = Math.pi * r * r
    | area (Rect (w, h)) = w * h

  fun fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

  (* "}#" and $x in strings and (* nested *) comments are just SML. *)
  val banner = "SML says }# and $hello"
}#

;;; An expression block is a Lisp expression.
(defun sum-of-fibs (n)
  (loop for i below n sum #@sml{ fib 10 }# ))

;;; Lisp values are spliced in with $name or $(form), lexical variables
;;; included.  An annotation such as ($n : int) documents the type.
(defun fib-of (n)
  #@sml{ fib ($n : int) }#)

(defun scaled-area (k)
  #@sml{ area (Rect ($(* k 2.0d0), 3.0)) }#)

;;; SML functions are Lisp functions: curried, one argument each.
(defun make-adder (k)
  #@sml{ fn x => x + $k }#)

(defun demo ()
  (format t "~&fib 20          = ~A~%" (fib-of 20))
  (format t "sum-of-fibs 3   = ~A~%" (sum-of-fibs 3))
  (format t "scaled-area 1.5 = ~A~%" (scaled-area 1.5d0))
  (format t "adder           = ~A~%" (funcall (make-adder 10) 32))
  (format t "banner          = ~A~%"
          (symbol-value (find-symbol "banner" "SML.SML-SHOWCASE")))
  ;; Lists are Lisp lists; tuples are (:tuple ...).
  (format t "map             = ~A~%"
          #@sml{ List.map (fn x => (x, x * x)) $(list 1 2 3) }#))
