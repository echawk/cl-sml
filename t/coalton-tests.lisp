;;; The Coalton backend (src/coalton, system cl-sml/coalton): programs
;;; compiled through Coalton print what the CL backend's do.

(defpackage #:cl-sml-coalton-tests
  (:use #:cl #:fiveam)
  (:export #:run-coalton-tests))

(in-package #:cl-sml-coalton-tests)

(def-suite cl-sml-coalton-suite
  :description "SML through the Coalton backend, compared with the CL backend")

(in-suite cl-sml-coalton-suite)

(defvar *counter* 0)

(defun cl-output (source)
  "What SOURCE prints compiled by the CL backend (native checker)."
  (let ((package (format nil "SML.COALTON-TEST-CL-~D" (incf *counter*)))
        (cl-sml:*sml-type-checker* :native))
    (with-output-to-string (*standard-output*)
      (eval (cl-sml:compile-sml-program-string source :package package)))))

(defun coalton-output (source)
  "What SOURCE prints compiled by the Coalton backend."
  (let ((cl-sml:*sml-type-checker* :native))
    (with-output-to-string (*standard-output*)
      (cl-sml:load-sml-via-coalton
       source :package-name (format nil "SML.COALTON-TEST-~D" (incf *counter*))))))

(defmacro same-output (source &optional expected)
  `(let ((source ,source))
     (let ((coalton (coalton-output source)))
       (is (string= (cl-output source) coalton))
       ,@(when expected `((is (string= ,expected coalton)))))))

(defun lines (&rest lines)
  (format nil "~{~A~%~}" lines))

(test coalton-basis-loads
  (is (not (null (cl-sml::ensure-coalton-basis-loaded))))
  ;; The core structures are emitted whole.
  (let ((blocked (cl-sml::coalton-registry-blocked cl-sml::*coalton-basis*)))
    (dolist (structure '("List." "Option." "String." "Char." "Int." "General." "Bool."
                         "ListPair." "Real." "TextIO." "Vector." "Array."))
      (is (notany (lambda (entry) (eql 0 (search structure (car entry)))) blocked)
          "~A has blocked declarations" structure))))

(test coalton-core-programs
  (same-output "val _ = print (Int.toString (6 * 7) ^ \"\\n\")" (lines "42"))
  (same-output "fun fact 0 = 1 | fact n = n * fact (n - 1)
val _ = print (Int.toString (fact 20) ^ \"\\n\")")
  (same-output "fun sum (acc, []) = acc | sum (acc, x :: xs) = sum (acc + x, xs)
val _ = print (Int.toString (sum (0, List.tabulate (1000, fn i => i))) ^ \"\\n\")")
  (same-output "fun add x y = x + y  val inc = add 1
val _ = print (String.concatWith \",\" (map (Int.toString o inc) [1, 2, 3]) ^ \"\\n\")")
  (same-output "val r = ref 0
fun loop 0 = () | loop n = (r := !r + n; loop (n - 1))
val _ = loop 10
val _ = print (Int.toString (!r) ^ \"\\n\")
val i = ref 0 val _ = while !i < 3 do (print (Int.toString (!i)); i := !i + 1)
val _ = print \"\\n\"")
  (same-output "val (a, b) = (1, \"two\")
val {x, y = (p, q), ...} = {x = 3.5, y = (#\"c\", [true]), z = ()}
val _ = print (Int.toString a ^ b ^ Real.toString x ^ String.str p ^ Bool.toString (hd q) ^ \"\\n\")")
  (same-output "fun f (l as h :: _) = h + length l | f [] = 0
val _ = print (Int.toString (f [5, 6, 7]) ^ \"\\n\")
val s = case explode \"abc\" of #\"a\" :: rest => implode (rev rest) | _ => \"\"
val _ = print (s ^ \"\\n\")"))

(test coalton-datatypes
  (same-output "datatype 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
fun insert (x, Leaf) = Node (Leaf, x, Leaf)
  | insert (x, t as Node (l, y, r)) =
      if x < y then Node (insert (x, l), y, r)
      else if x > y then Node (l, y, insert (x, r)) else t
fun toList Leaf = [] | toList (Node (l, x, r)) = toList l @ x :: toList r
val t = foldl insert Leaf [5, 3, 8, 1, 4]
val _ = print (String.concatWith \" \" (map Int.toString (toList t)) ^ \"\\n\")
val _ = print (Bool.toString (t = t) ^ Bool.toString (insert (9, t) = t) ^ \"\\n\")")
  (same-output "datatype shape = Circle of real | Rect of {w : real, h : real}
fun area (Circle r) = 3.0 * r * r | area (Rect {w, h}) = w * h
val _ = print (Real.toString (area (Circle 1.0) + area (Rect {w = 2.0, h = 3.0})) ^ \"\\n\")")
  (same-output "structure S = struct
  datatype color = Red | Green | Blue
  fun name Red = \"red\" | name Green = \"green\" | name Blue = \"blue\"
end
val _ = print (S.name S.Green ^ (if S.Red = S.Red then \"!\" else \"?\") ^ \"\\n\")"))

(test coalton-equality
  (same-output "fun member (x, []) = false | member (x, y :: ys) = x = y orelse member (x, ys)
val _ = print (Bool.toString (member (\"b\", [\"a\", \"b\"])) ^ Bool.toString (member ([1], [[2]])) ^ \"\\n\")
val r1 = ref 1 val r2 = ref 1
val _ = print (Bool.toString (r1 = r2) ^ Bool.toString (r1 = r1) ^ Bool.toString ([] = nil) ^ \"\\n\")"
               (lines "truefalse" "falsetruetrue")))

(test coalton-exceptions
  (same-output "exception E of int
exception F
fun f x = if x > 0 then raise E x else raise F
val _ = print ((f 3 handle E n => Int.toString n | F => \"F\") ^ \"\\n\")
val _ = print ((f 0 handle E n => Int.toString n | F => \"F\") ^ \"\\n\")
fun g () = let exception L in (raise L) handle L => \"local\" end
val _ = print (g () ^ \"\\n\")")
  ;; Raised by the run time's primitives and by Int's checks.
  (same-output "val _ = print ((Int.toString (1 div 0)) handle Div => \"Div\\n\")
val _ = print ((Int.toString (valOf Int.maxInt + 1)) handle Overflow => \"Overflow\\n\")
val _ = print ((hd []) handle Empty => \"Empty\\n\")
val _ = print ((String.sub (\"abc\", 5); \"\") handle Subscript => \"Subscript\\n\")
val _ = print ((raise Fail \"boom\") handle Fail m => m ^ \"\\n\")
val _ = print ((case (fn 1 => \"one\") 2 of s => s) handle Match => \"Match\\n\")")
  ;; An uncaught exception is the run time's condition.
  (signals cl-sml::sml-raised-exception
    (cl-sml:load-sml-via-coalton "val _ = raise Fail \"out\"")))

(test coalton-structures-and-local
  (same-output "structure Q :> sig type t val empty : t val push : int * t -> t val sum : t -> int end =
struct type t = int list val empty = [] fun push (x, q) = x :: q fun sum q = foldl op+ 0 q end
local val base = 100 fun helper x = x + base in val v = helper 1 end
val _ = print (Int.toString (Q.sum (Q.push (2, Q.push (3, Q.empty))) + v) ^ \"\\n\")"
               (lines "106")))

(test coalton-rejects-functors
  (signals cl-sml::coalton-unsupported
    (cl-sml:emit-coalton-sml-string
     "functor F (X : sig val x : int end) = struct val y = X.x end
structure A = F (struct val x = 1 end)")))

(test coalton-show
  (let ((text (with-output-to-string (s)
                (cl-sml:show-sml-coalton "fun twice f x = f (f x)" :stream s))))
    (is (search "coalton-toplevel" text))
    (is (search "twice" text))))

(defun run-coalton-tests ()
  (run! 'cl-sml-coalton-suite))
