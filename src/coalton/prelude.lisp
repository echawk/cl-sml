;;; The Coalton backend's prelude (docs/IR.md, "The Coalton backend"): the
;;; Coalton types and functions the emitted code uses below the Basis.  The
;;; Basis itself is SML (src/basis), emitted through the backend; what is
;;; here is what cannot be SML: the representation of exceptions, refs with
;;; SML's equality, Int arithmetic that raises Overflow, and the `lisp'
;;; forms that call the run time's primitives.

(defpackage #:cl-sml/coalton-prelude
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:cell #:coalton/cell))
  (:export #:Raw #:Exn #:Ref #:SmlVector #:SmlArray
           #:unsafe-coerce #:ignore-raw
           #:new-ref #:deref #:assign
           #:raise #:SmlException #:exn-of #:exn-make #:exn-is #:exn-payload
           #:new-exn-tag #:initial-exn-tag #:exn-tag-of
           #:while-loop))

(in-package #:cl-sml/coalton-prelude)

(coalton-toplevel
  ;; Any Lisp object: the values the run time's primitives take and return,
  ;; before they are converted to Coalton's representations.
  (repr :native cl:t)
  (define-type Raw)

  ;; SML's exn: the run time's exception values (src/runtime.lisp,
  ;; SML-EXCEPTION-TAG), so that the CL backend, the run time's primitives
  ;; and Coalton code raise and handle the same exceptions.
  (repr :native cl:t)
  (define-type Exn)

  ;; The run time's vectors (simple vectors), for 'a vector.
  (repr :native cl:t)
  (define-type (SmlVector :a))

  (inline)
  (declare unsafe-coerce (:a -> :b))
  (define (unsafe-coerce x)
    (lisp (-> :b) (x) x))

  (inline)
  (declare ignore-raw (Raw -> Unit))
  (define (ignore-raw _x) Unit)

  ;; 'a ref: a Cell whose equality is identity, as SML's is.  (Coalton's
  ;; Cell compares contents.)
  (repr :transparent)
  (define-type (Ref :a) (%Ref (Cell :a)))

  (define-instance (Eq (Ref :a))
    (define (== a b)
      (lisp (-> Boolean) (a b) (cl:eq a b))))

  (inline)
  (declare new-ref (:a -> (Ref :a)))
  (define (new-ref x) (%Ref (cell:new x)))

  (inline)
  (declare deref ((Ref :a) -> :a))
  (define (deref r)
    (match r ((%Ref c) (cell:read c))))

  (inline)
  (declare assign ((Ref :a) * :a -> Unit))
  (define (assign r x)
    (match r ((%Ref c) (progn (cell:write! c x) Unit))))

  ;; Exceptions.  A tag is the run time's SML-EXCEPTION-TAG; an exception
  ;; without a payload is its tag, one with a payload (tag . payload).
  ;; Raising and handling: SML's exceptions are the run time's Lisp
  ;; condition SML-RAISED-EXCEPTION, a native Coalton exception, so that a
  ;; Coalton `catch' handles what the run time's primitives and the CL
  ;; backend raise, and the reverse.
  (repr :native cl-sml::sml-raised-exception)
  (define-exception SmlException)

  (declare raise (Exn -> :a))
  (define (raise e)
    (throw (lisp (-> SmlException) (e)
             (cl:make-condition 'cl-sml::sml-raised-exception :value e))))

  (inline)
  (declare exn-of (SmlException -> Exn))
  (define (exn-of condition)
    (lisp (-> Exn) (condition) (cl-sml::sml-exception-value condition)))

  (declare new-exn-tag (String -> Raw))
  (define (new-exn-tag name)
    (lisp (-> Raw) (name) (cl-sml::make-sml-exception-constructor name)))

  (declare initial-exn-tag (String -> Raw))
  (define (initial-exn-tag name)
    (lisp (-> Raw) (name)
      (cl-sml::sml-exception-constructor-tag (cl-sml::sml-initial-exception name))))

  (declare exn-tag-of (Exn -> Raw))
  (define (exn-tag-of e)
    (lisp (-> Raw) (e) (cl-sml::sml-exception-constructor-tag e)))

  (inline)
  (declare exn-make (Raw * :a -> Exn))
  (define (exn-make tag x)
    (lisp (-> Exn) (tag x) (cl:cons tag x)))

  (inline)
  (declare exn-is (Raw * Exn -> Boolean))
  (define (exn-is tag e)
    (lisp (-> Boolean) (tag e)
      (cl:if (cl:consp e) (cl:eq (cl:car e) tag) (cl:eq e tag))))

  (inline)
  (declare exn-payload (Exn -> :a))
  (define (exn-payload e)
    (lisp (-> :a) (e) (cl:cdr e)))

  (declare while-loop ((Unit -> Boolean) * (Unit -> Unit) -> Unit))
  (define (while-loop test body)
    (if (test Unit)
        (progn (body Unit) (while-loop test body))
        Unit)))

;;; The overloaded operators at each type the checker resolves them to,
;;; e.g. |int+| or |real<|: the CL backend's open code
;;; (COMPILE-OVERLOADED-OPERATION), so that both backends compute the same
;;; results (Int raises Overflow, words wrap, reals follow IEEE).

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defparameter cl-sml::*coalton-overloaded-types*
    '(("int" . IFix) ("word" . UFix) ("word8" . U8) ("real" . F64)
      ("char" . Char) ("string" . String) ("intinf" . Integer)))

  (cl:defparameter cl-sml::*coalton-overloaded-operators*
    '("+" "-" "*" "/" "div" "mod" "~" "abs" "<" ">" "<=" ">=")))

(cl:defmacro define-overloaded-operators ()
  (cl:let ((forms cl:nil))
    (cl:loop
      :for (tyname . type) :in cl-sml::*coalton-overloaded-types*
      :do (cl:dolist (op cl-sml::*coalton-overloaded-operators*)
            (cl:let* ((unary (cl:member op '("~" "abs") :test #'cl:string=))
                      (args (cl:if unary '(a) '(a b)))
                      (form (cl-sml::compile-overloaded-operation op tyname args))
                      (result (cl:if (cl:member op '("<" ">" "<=" ">=") :test #'cl:string=)
                                     'Boolean type))
                      (name (cl:intern (cl:concatenate 'cl:string tyname op))))
              (cl:when form
                (cl:push `(inline) forms)
                (cl:push `(declare ,name ,(cl:if unary
                                                 `(,type -> ,result)
                                                 `(,type * ,type -> ,result)))
                         forms)
                (cl:push `(define (,name ,@args) (lisp (-> ,result) ,args ,form)) forms)
                ))))
    `(coalton-toplevel ,@(cl:nreverse forms))))

(define-overloaded-operators)
