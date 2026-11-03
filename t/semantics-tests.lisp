(defpackage #:cl-sml-semantics-tests
  (:use #:cl #:fiveam))

(in-package #:cl-sml-semantics-tests)

(def-suite cl-sml-semantics-suite
  :description "SML97 dynamic semantics of compiled programs: static scoping
of rebound names, Match and Bind, records and tuples")

(in-suite cl-sml-semantics-suite)

(defvar *package-counter* 0)

(defun fresh-sml-package ()
  (let ((name (format nil "SML.SEMANTICS-TEST-~D" (incf *package-counter*))))
    (when (find-package name)
      (delete-package name))
    name))

(defun run-sml (&rest sources)
  "Compile and run each of SOURCES, in order and as separate compilation
units, in a fresh SML package.  Returns what they printed and the package."
  (let ((package (fresh-sml-package)))
    (values (with-output-to-string (*standard-output*)
              (dolist (source sources)
                (eval (cl-sml:compile-sml-program-string source
                                                         :package package))))
            package)))

(defun load-sml (source)
  "Like RUN-SML for one SOURCE, but through LOAD-SML-FILE, which compiles the
generated Lisp with COMPILE-FILE."
  (let ((package (fresh-sml-package))
        (pathname (uiop:with-temporary-file (:pathname p :type "sml" :keep t)
                    p)))
    (unwind-protect
         (progn
           (with-open-file (stream pathname :direction :output
                                            :if-exists :supersede)
             (write-string source stream))
           (values (with-output-to-string (*standard-output*)
                     (handler-bind ((warning #'muffle-warning))
                       (cl-sml:load-sml-file pathname :package package)))
                   package))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun lines (&rest lines)
  (format nil "~{~A~%~}" lines))

;;; Modules

(test open-several-structures-later-ones-shadow
  (is (string= (lines "B.c" "A.c")
               (run-sml "structure A = struct val c = \"A.c\" end
structure B = struct val c = \"B.c\" end
open A B
val _ = print (c ^ \"\\n\")
structure C = struct
open B A
val d = c
end
val _ = print (C.d ^ \"\\n\")")))
  (is (string= (lines "A.B.x")
               (run-sml "structure A = struct
val x = \"A.x\"
structure B = struct val x = \"A.B.x\" end
end
open A A.B
val _ = print (x ^ \"\\n\")"))))

(test long-constructor-pattern-rejects-nullary-value
  (is (string= (lines "circle 3" "empty")
               (run-sml "structure Shapes = struct
datatype shape = Circle of int | Empty
end
fun describe (Shapes.Circle r) = \"circle \" ^ Int.toString r
  | describe Shapes.Empty = \"empty\"
val _ = print (describe (Shapes.Circle 3) ^ \"\\n\")
val _ = print (describe Shapes.Empty ^ \"\\n\")"))))

(test nested-structure-may-refer-to-the-outer-one-of-its-name
  (is (string= (lines "2")
               (run-sml "structure U = struct
  structure Array = struct val sub = Array.sub end
end
val a = Array.fromList [1, 2, 3]
val _ = print (Int.toString (U.Array.sub (a, 1)) ^ \"\\n\")"))))

;;; Functors are generative: each application evaluates the body afresh.

(test functor-applied-in-functor-body-to-parameter-substructure
  ;; MkCore's result reaches Grammar through its argument's substructure
  ;; (SML-SUBSTRUCTURE of a name inside a named substructure); from
  ;; mlyacc's mkMakeLrTable.
  (let ((program "signature G = sig datatype term = T of int end
signature IG = sig structure Grammar : G end
signature C = sig structure Grammar : G end
structure IntGrammar : IG = struct structure Grammar = struct datatype term = T of int end end
functor MkCore (structure IntGrammar : IG) : C =
  struct open IntGrammar structure Grammar = Grammar end
functor F (structure IntGrammar : IG) =
  struct
    structure Core = MkCore (structure IntGrammar = IntGrammar)
    structure Grammar = Core.Grammar
  end
structure M = F (structure IntGrammar = IntGrammar)
val _ = case M.Grammar.T 3 of M.Grammar.T n => print (Int.toString n ^ \"\\n\")"))
    (is (string= (lines "3") (run-sml program)))
    (is (string= (lines "3") (load-sml program)))))

(test functor-exceptions-are-generative-and-aliases-keep-identity
  ;; Each application declares new exceptions (rule 54); `exception Y = P.X'
  ;; denotes the argument's exception (rule 31).
  (let ((program "signature S = sig exception X end
functor F () = struct exception E of int end
structure A = F () structure B = F ()
structure C : S = struct exception X end
functor G (P : S) = struct exception Y = P.X fun f () = raise Y end
structure GC = G (C)
val r1 = (raise B.E 1) handle A.E n => n | B.E _ => 2
val r2 = (raise C.X) handle GC.Y => 3 | _ => 0
val r3 = (GC.f ()) handle C.X => 4 | _ => 0
val _ = app (fn n => print (Int.toString n ^ \"\\n\")) [r1, r2, r3]"))
    (is (string= (lines "2" "3" "4") (run-sml program)))
    (is (string= (lines "2" "3" "4") (load-sml program)))))

(test functor-applications-have-state-of-their-own
  (let ((program "functor Counter (X : sig val start : int end) = struct
  val r = ref X.start
  fun next () = (r := !r + 1; !r)
  structure Inner = struct fun get () = !r end
end
structure Z = struct val start = 0 end
structure C1 = Counter (Z)
structure C2 = Counter (struct val start = 100 end)
val _ = print (Int.toString (C1.next ()) ^ \"\\n\")
val _ = print (Int.toString (C1.next ()) ^ \"\\n\")
val _ = print (Int.toString (C2.next ()) ^ \"\\n\")
val _ = print (Int.toString (C1.Inner.get ()) ^ \"\\n\")
val _ = print (Int.toString (!C2.r) ^ \"\\n\")"))
    (is (string= (lines "1" "2" "101" "2" "101") (run-sml program)))
    (is (string= (lines "1" "2" "101" "2" "101") (load-sml program))))
  ;; A functor declared by an earlier compilation unit, applied twice in
  ;; a functor body.
  (multiple-value-bind (output package)
      (run-sml "functor Cell () = struct val r = ref 0 fun bump () = (r := !r + 1; !r) end"
               "functor Pair () = struct structure A = Cell () structure B = Cell () end
structure P = Pair ()
val _ = (P.A.bump (); P.A.bump ())
val _ = print (Int.toString (P.A.bump ()) ^ \" \" ^ Int.toString (P.B.bump ()) ^ \"\\n\")")
    (is (string= (lines "3 1") output))
    (is (= 4 (funcall (cl-sml:sml-value "P.A.bump" package) (cl-sml::sml-unit))))))

(test functor-applications-have-exceptions-of-their-own
  (is (string= (lines "A.E 1" "B.E 2" "G1.X" "Y.E")
               (run-sml "functor F () = struct exception E of int fun raiseIt n = raise E n end
structure A = F ()
structure B = F ()
fun test f = (f (); \"none\")
  handle B.E n => \"B.E \" ^ Int.toString n | A.E n => \"A.E \" ^ Int.toString n
val _ = print (test (fn () => A.raiseIt 1) ^ \"\\n\")
val _ = print (test (fn () => B.raiseIt 2) ^ \"\\n\")
functor G () = struct exception X fun boom () = raise X end
structure G1 = G ()
structure G2 = G ()
val _ = print (((G1.boom (); \"none\") handle G2.X => \"G2.X\" | G1.X => \"G1.X\") ^ \"\\n\")
functor H (X : sig exception E end) = struct fun boom () = raise X.E end
structure Y = struct exception E end
structure HY = H (Y)
val _ = print (((HY.boom (); \"none\") handle Y.E => \"Y.E\") ^ \"\\n\")"))))

(test functor-applications-have-datatype-constructors-of-their-own
  (multiple-value-bind (output package)
      (run-sml "functor F () = struct
  datatype t = C | D of int
  fun isC C = true | isC _ = false
  fun get (D n) = n | get C = 0
end
structure A = F ()
structure B = F ()
open B
val _ = print (Int.toString (A.get (A.D 3) + get (D 4)) ^ \"\\n\")
val _ = print (Bool.toString (A.isC A.C) ^ \" \" ^ Bool.toString (A.C = A.C) ^ \"\\n\")
val _ = print ((case B.D 5 of B.D n => Int.toString n | B.C => \"C\") ^ \"\\n\")")
    (is (string= (lines "7" "true true" "5") output))
    ;; The instances' constructors differ (only an ill-typed program could
    ;; tell them apart).
    (is (not (eq (cl-sml:sml-value "A.C" package) (cl-sml:sml-value "B.C" package))))
    (is (not (funcall (cl-sml:sml-value "A.isC" package)
                      (cl-sml:sml-value "B.C" package))))))

;;; Match and Bind

(test failed-matches-raise-match
  (is (string= (lines "Match" "7" "~1" "match2" "explicit match")
               (run-sml "fun p s = print (s ^ \"\\n\")
val _ = p ((case [1] of [] => \"e\") handle Match => \"Match\")
val _ = p (Int.toString ((fn 0 => 1) 2 handle Match => 7))
fun partial (SOME x) = x
val _ = p (Int.toString (partial NONE handle Match => ~1))
fun f x = case x of [] => \"empty\"
val _ = p ((f [1]) handle Match => \"match2\")
val _ = p ((raise Match) handle Bind => \"wrong\" | Match => \"explicit match\")")))
  ;; A literal case without a default rule.
  (is (string= (lines "match")
               (run-sml "val _ = print (((case 3 of 1 => \"one\" | 2 => \"two\") handle Match => \"match\") ^ \"\\n\")"))))

(test failed-bindings-raise-bind
  (is (string= (lines "1" "0" "bind" "bind2")
               (run-sml "fun p s = print (s ^ \"\\n\")
val _ = p (Int.toString (let val SOME y = (NONE : int option) in 0 end handle Bind => 1))
val _ = p (Int.toString (let val (x :: _) = ([] : int list) in x end handle Bind => 0))
val _ = p ((let val 1 = 2 in \"no\" end) handle Bind => \"bind\")
val _ = p ((let val (a, 0) = (1, 1) in \"no\" end) handle Bind => \"bind2\")")))
  (signals cl-sml::sml-raised-exception
    (run-sml "val [x] = [] : int list"))
  (is (string= "Bind"
               (handler-case (progn (run-sml "val [x] = [] : int list") nil)
                 (cl-sml::sml-raised-exception (condition)
                   (cl-sml::sml-exception-name
                    (cl-sml::sml-exception-value condition)))))))

(test unhandled-exception-propagates-through-handle
  (is (string= (lines "outer")
               (run-sml "exception A
exception B
val _ = print (((raise A) handle B => \"inner\") handle A => \"outer\")
val _ = print \"\\n\""))))

(test primitives-raise-initial-basis-exceptions
  ;; Rebinding the names does not change what the primitives raise.
  (is (string= (lines "initial Div" "initial Match")
               (run-sml "exception InitialDiv = Div
exception InitialMatch = Match
exception Div
exception Match
val _ = print ((Int.toString (1 div 0); \"none\") handle Div => \"new Div\" | InitialDiv => \"initial Div\")
val _ = print \"\\n\"
val _ = print (((fn 0 => \"none\") 1) handle Match => \"new Match\" | InitialMatch => \"initial Match\")
val _ = print \"\\n\""))))

(test repl-reports-uncaught-match-and-continues
  (let ((output (make-string-output-stream))
        (error-output (make-string-output-stream)))
    (cl-sml:repl :input (make-string-input-stream
                         (format nil "(fn 0 => 1) 2;~%val [x] = [] : int list;~%1 + 1;~%:quit~%"))
                 :output output
                 :error-output error-output
                 :prompt nil
                 :package (fresh-sml-package))
    (let ((errors (get-output-stream-string error-output)))
      (is (search "uncaught exception Match" errors))
      (is (search "uncaught exception Bind" errors)))
    (is (search "val it = 2" (get-output-stream-string output)))))

;;; Records and tuples

(test record-fields-are-evaluated-in-source-order
  (is (string= "ba" (run-sml "val r = {b = print \"b\", a = print \"a\"}")))
  (is (string= (lines "abc" "123" "xy")
               (run-sml "fun t s v = (print s; v)
val _ = (t \"a\" 1, t \"b\" 2, t \"c\" 3)
val _ = print \"\\n\"
val _ = {zed = t \"1\" 1, alpha = t \"2\" 2, mid = t \"3\" 3}
val _ = print \"\\n\"
val _ = {2 = t \"x\" 0, 1 = t \"y\" 0}
val _ = print \"\\n\""))))

(test tuples-are-records-with-numeric-labels
  (is (string= (lines "true" "true" "5" "15" "19" "ab" "3" "12" "11" "true" "7")
               (run-sml "fun p n = print (Int.toString n ^ \"\\n\")
fun b x = print ((if x then \"true\" else \"false\") ^ \"\\n\")
val _ = b ({1 = 1, 2 = 2} = (1, 2))
val _ = b ((3, 4) = {2 = 4, 1 = 3})
val _ = p (#1 {1 = 5, 2 = 6})
val {1 = x, 2 = y} = (7, 8)
val _ = p (x + y)
val (q, r) = {1 = 9, 2 = 10}
val _ = p (q + r)
val t = {2 = \"b\", 1 = \"a\"}
val _ = print (#1 t ^ #2 t ^ \"\\n\")
val _ = p (case (1, 2) of {1 = a, 2 = b'} => a + b')
val _ = p (case {1 = 3, 2 = 4} of (a, b') => a * b')
fun fst ({1 = a, ...} : int * int) = a
val _ = p (fst (11, 12))
val _ = b ({} = ())
val _ = p (case () of {} => 7)"))))

(test one-field-numeric-record-is-not-a-tuple
  (is (string= (lines "4")
               (run-sml "val r = {1 = 4}
val _ = print (Int.toString (#1 r) ^ \"\\n\")"))))

;;; Static scoping of top-level rebindings

(test top-level-rebinding-does-not-change-earlier-code
  (is (string= (lines "1" "20" "10" "21" "8" "7" "new" "12")
               (run-sml "fun p n = print (Int.toString n ^ \"\\n\")
val x = 1
fun getx () = x
val x = 2
val _ = p (getx ())
val y = 10
val f = let val y = 20 in fn () => y end
val _ = p (f ())
val _ = p y
fun g z = let val y = z in fn () => y end
val g1 = g 1
val g2 = g 2
val _ = p (g1 () + g2 () * 10)
fun shadow n = let val n = n + 1 in let val n = n * 2 in n end end
val _ = p (shadow 3)
datatype k = K of int
fun unK (K n) = n
val old = K 7
datatype k = K of string
val _ = p (unK old)
val _ = print ((case K \"new\" of K s => s) ^ \"\\n\")
fun h () = 1
val h1 = h
fun h () = 2
val _ = p (h1 () * 10 + h ())"))))

(test rebinding-across-compilation-units
  ;; Each string is compiled and run on its own, like REPL phrases or `use'd
  ;; files loaded into one package.
  (multiple-value-bind (output package)
      (run-sml "val x = 1"
               "fun getx () = x"
               "val x = 2"
               "fun f () = 1 fun g () = f ()"
               "fun f () = 2"
               "val _ = print (Int.toString (getx () * 100 + g () * 10 + f ()) ^ \"\\n\")")
    (is (string= (lines "112") output))
    ;; The package's symbol is the newest binding, for Common Lisp.
    (is (= 2 (cl-sml:sml-value "x" package)))
    (is (= 2 (funcall (cl-sml:sml-function "f" package) (cl-sml::sml-unit))))))

(test simultaneous-val-and
  (is (string= (lines "21" "18" "6" "2")
               (run-sml "fun p n = print (Int.toString n ^ \"\\n\")
val x = 1
val x = 2 and y = x
val _ = p (x * 10 + y)
val a = 5 and b = 6 and c = 7
val _ = p (a + b + c)
val (m, n) = (1, 2) and k = 3
val _ = p (m + n + k)
val f = fn () => x
val x = 100
val _ = p (f ())")))
  (is (string= (lines "21")
               (run-sml "val r = let val x = 1 in let val x = 2 and y = x in x * 10 + y end end
val _ = print (Int.toString r ^ \"\\n\")"))))

(test exception-declarations-are-generative-at-top-level
  (is (string= (lines "old" "new")
               (run-sml "exception E
val old = (fn () => raise E) : unit -> unit
exception E
val _ = print (((old (); \"\") handle E => \"new\" | _ => \"old\") ^ \"\\n\")
val _ = print (((raise E) handle E => \"new\" | _ => \"old\") ^ \"\\n\")")))
  (is (string= (lines "old")
               (run-sml "exception E"
                        "val old = (fn () => raise E) : unit -> unit"
                        "exception E"
                        "val _ = print (((old (); \"\") handle E => \"new\" | _ => \"old\") ^ \"\\n\")"))))

(test structure-rebinding-does-not-change-earlier-code
  (is (string= (lines "1" "10" "20")
               (run-sml "structure S = struct
val x = 1
fun get () = x
end
val g = S.get
structure S = struct
val x = 2
fun get () = x
end
val _ = print (Int.toString (g ()) ^ \"\\n\")
structure T = struct val v = 10 end
fun getV () = T.v
structure T = struct val v = 20 end
val _ = print (Int.toString (getV ()) ^ \"\\n\")
val _ = print (Int.toString T.v ^ \"\\n\")"))))

(test let-binders-are-lexical-after-top-level-binding
  ;; A `use'd file binding a name that an earlier unit bound at top level.
  (is (string= (lines "20" "10")
               (run-sml "val y = 10"
                        "val f = let val y = 20 in fn () => y end
val _ = print (Int.toString (f ()) ^ \"\\n\")
val _ = print (Int.toString y ^ \"\\n\")"))))

(test rebinding-through-load-sml-file
  (is (string= (lines "1" "2")
               (load-sml "val x = 1
fun getx () = x
val x = 2
val _ = print (Int.toString (getx ()) ^ \"\\n\")
val _ = print (Int.toString x ^ \"\\n\")"))))

(test repl-rebinding-is-static
  (let ((output (make-string-output-stream))
        (error-output (make-string-output-stream)))
    (cl-sml:repl :input (make-string-input-stream
                         (format nil "val x = 1;~%fun getx () = x;~%val x = 2;~%getx ();~%x;~%val x : int = raise Fail \"no\";~%x;~%:quit~%"))
                 :output output
                 :error-output error-output
                 :prompt nil
                 :package (fresh-sml-package))
    (let ((text (get-output-stream-string output)))
      (is (search "val it = 1" text))
      (is (= 2 (count-matches "val it = 2" text))))
    (is (search "uncaught exception Fail" (get-output-stream-string error-output)))))

(defun count-matches (needle haystack)
  (loop for start = (search needle haystack) then (search needle haystack :start2 (1+ start))
        while start
        count t))

;;; SML identifiers named like COMMON-LISP symbols

(defparameter *common-lisp-named-identifiers*
  "val T = 1
fun list x = x
val nil' = 2
structure S = struct val car = 3 val NIL = 4 val PI = 5 end
datatype d = CAR | LIST of int
fun f CAR = 0 | f (LIST n) = n
val _ = print (Int.toString (T + list 10 + nil' + S.car + S.NIL + S.PI + f (LIST 100) + f CAR) ^ \"\\n\")
val t = let val T = 7 val NIL = 8 in T + NIL end
val _ = print (Int.toString t ^ \"\\n\")
fun g () = let val T = 5 in T + 1 end
val _ = print (Int.toString (g ()) ^ \"\\n\")
val h = fn x => let datatype t = T of int in case T x of T y => y end
val _ = print (Int.toString (h 42) ^ \"\\n\")
abstype a = A of int with fun mk x = A x fun get (A x) = x end
val _ = print (Int.toString (get (mk 3)) ^ \"\\n\")
functor F () = let datatype t = T in struct val x = T end end
structure F = F ()
val _ = print (if F.x = F.x then \"true\\n\" else \"false\\n\")
exception T
val _ = print ((raise T) handle T => \"T\\n\")")

(test identifiers-named-like-common-lisp-symbols
  (let ((expected (lines "125" "15" "6" "42" "3" "true" "T")))
    (is (string= expected (run-sml *common-lisp-named-identifiers*)))
    (is (string= expected (load-sml *common-lisp-named-identifiers*)))))

(test later-let-bindings-shadow-earlier-ones
  (is (string= (lines "3" "11" "5")
               (run-sml "fun f r = let val {a, b} = r val a = a + b in a end
val _ = print (Int.toString (f {a = 1, b = 2}) ^ \"\\n\")
val g = let val x = 1 local val x = 10 in val y = x + 1 end in y end
val _ = print (Int.toString g ^ \"\\n\")
val h = let local val x = 2 in val x = x + 3 end in x end
val _ = print (Int.toString h ^ \"\\n\")"))))

;;; The Basis Library, which every package sees (src/basis/).

(test basis-is-visible-and-programs-shadow-it-locally
  (is (string= (lines "3" "Empty" "LESS" "2" "1")
               (run-sml "val _ = print (Int.toString (List.length [1, 2, 3]) ^ \"\\n\")
val _ = print ((Int.toString (hd [])) handle Empty => \"Empty\\n\")
val _ = print (case Int.compare (1, 2) of LESS => \"LESS\\n\" | _ => \"other\\n\")
structure List = struct fun length _ = 2 end
val _ = print (Int.toString (List.length [1]) ^ \"\\n\")"
                        "val _ = print (Int.toString (length [1]) ^ \"\\n\")")))
  ;; The program's List did not replace the basis's.
  (is (string= (lines "1")
               (run-sml "val _ = print (Int.toString (List.length [1]) ^ \"\\n\")"))))

(test int-arithmetic-raises-overflow
  (is (string= (lines "Overflow" "Overflow" "Overflow" "Overflow" "Div" "4611686018427387903")
               (run-sml "fun t f = print ((Int.toString (f ()); \"none\\n\") handle Overflow => \"Overflow\\n\" | Div => \"Div\\n\")
val mx = valOf Int.maxInt
val _ = t (fn () => mx + 1)
val _ = t (fn () => valOf Int.minInt - 1)
val _ = t (fn () => mx * 2)
val _ = t (fn () => floor 1E30)
val _ = t (fn () => Int.quot (1, 0))
val _ = print (Int.toString mx ^ \"\\n\")"))))

(defparameter *int-overflow-boundaries*
  "fun t f = print ((Int.toString (f ()) ^ \"\\n\") handle Overflow => \"Overflow\\n\" | Div => \"Div\\n\")
val mx = valOf Int.maxInt
val mn = valOf Int.minInt
fun add (x : int, y) = x + y
fun sub (x : int, y) = x - y
fun mul (x : int, y) = x * y
fun neg (x : int) = ~x
val _ = t (fn () => mx + 1)
val _ = t (fn () => add (mx, 1))
val _ = t (fn () => mn - 1)
val _ = t (fn () => sub (mn, 1))
val _ = t (fn () => ~mn)
val _ = t (fn () => neg mn)
val _ = t (fn () => abs mn)
val _ = t (fn () => mn div ~1)
val _ = t (fn () => Int.quot (mn, ~1))
val _ = t (fn () => mx * 2)
val _ = t (fn () => mul (3037000500, 3037000500))
val _ = t (fn () => mul (mn, ~1))
val _ = t (fn () => mx * ~1)
val _ = t (fn () => mn + 0)
val _ = t (fn () => mx - 0)
val _ = t (fn () => add (mx, mn))
val _ = t (fn () => ~ (mn + 1))
val _ = t (fn () => abs (mn + 1))
val _ = t (fn () => mn div 1)
val _ = t (fn () => mn mod ~1)
val _ = t (fn () => mul (2147483648, 2147483647))
val _ = t (fn () => foldl (op +) 0 [mx, 1])
val _ = t (fn () => foldl (op * ) 1 [mn, ~1])"
  "Int arithmetic at the edges of Int's range: each line is Overflow or
the exact result.")

(defparameter *int-overflow-boundary-results*
  (list "Overflow" "Overflow" "Overflow" "Overflow" "Overflow" "Overflow" "Overflow"
        "Overflow" "Overflow" "Overflow" "Overflow" "Overflow"
        "~4611686018427387903" "~4611686018427387904" "4611686018427387903" "~1"
        "4611686018427387903" "4611686018427387903" "~4611686018427387904" "0"
        "4611686016279904256" "Overflow" "Overflow"))

(test int-overflow-at-the-boundaries
  ;; With the checker, `+' and friends are specialised at int; without it
  ;; (plain mode) the generic operations must raise Overflow just the same.
  (is (string= (apply #'lines *int-overflow-boundary-results*)
               (run-sml *int-overflow-boundaries*)))
  (is (string= (apply #'lines *int-overflow-boundary-results*)
               (let ((cl-sml:*sml-type-checker* nil))
                 (run-sml *int-overflow-boundaries*))))
  (is (string= (apply #'lines *int-overflow-boundary-results*)
               (load-sml *int-overflow-boundaries*))))

(defparameter *specialised-operations*
  "fun say s = print (s ^ \"\\n\")
fun b x = say (Bool.toString x)
val nan = 0.0 / 0.0
val big = 1E308
fun lt (x : real, y) = x < y
fun ge (x : real, y) = x >= y
val _ = b (nan < 1.0 orelse nan >= 1.0 orelse nan > 1.0 orelse nan <= 1.0)
val _ = b (if lt (nan, 1.0) then true else ge (nan, 1.0))
val _ = b (Real.isFinite (big * 10.0))
val _ = say (Real.toString (big * 10.0 - big * 10.0))
val _ = say (Real.toString (~ (big * 10.0)))
val _ = say (Real.toString (abs (~ big * 10.0)))
val _ = say (Real.toString (1.0 / 0.0))
val _ = say (Real.toString (2.5 * 2.0 + 1.0 / 4.0 - 0.25))
val _ = b (big * 10.0 > big)
val w : word = 0wx3FFFFFFFFFFFFFFF
val _ = say (Word.toString (w + 0w1))
val _ = say (Word.toString (w * w))
val _ = say (Word.toString (0w0 - 0w1))
val _ = say (Word8.toString (0w255 + 0w1 : Word8.word))
val _ = say (Word8.toString (0w16 * 0w17 : Word8.word))
val _ = say (Word64.toString (0wxFFFFFFFFFFFFFFFF * 0w3 : Word64.word))
val _ = b (w > 0w1 andalso (0w255 : Word8.word) >= 0w255)
datatype color = Red | Green | Blue
datatype shape = Dot | Circle of int
val r = ref 1
val r' = ref 1
val _ = b (\"abc\" = \"ab\" ^ \"c\" andalso \"abc\" <> \"abd\")
val _ = b (#\"a\" = #\"a\" andalso #\"a\" <> #\"b\")
val _ = b (Blue = Blue andalso Red <> Green)
val _ = b (Circle 1 = Circle 1 andalso Dot <> Circle 2)
val _ = b (r = r andalso r <> r' andalso !r = !r')
val _ = b (0w5 = 0w5 andalso (0w5 : Word8.word) <> 0w6)
val _ = b (IntInf.pow (2, 100) = IntInf.pow (2, 100) andalso IntInf.pow (2, 64) <> 0)
val _ = b (true = true andalso true <> false andalso LESS <> GREATER)
val _ = b ([1, 2] = [1, 2] andalso (1, \"a\") <> (1, \"b\"))
val _ = b (SOME Red = SOME Red andalso NONE <> SOME Blue)"
  "Operations the compiler specialises by type, at their edges.")

(defparameter *specialised-operation-results*
  (list "false" "false" "false" "nan" "~inf" "inf" "inf" "5.0" "true"
        "0" "1" "3FFFFFFFFFFFFFFF" "0" "10" "FFFFFFFFFFFFFFFD" "true"
        "true" "true" "true" "true" "true" "true" "true" "true" "true" "true"))

(test specialised-operations-agree-with-generic-ones
  ;; Reals: no traps, NaN compares false.  Words wrap at their size.
  ;; Equality: EQL, EQ, STRING= or structural, by type (ROADMAP 13, 14).
  (is (string= (apply #'lines *specialised-operation-results*)
               (run-sml *specialised-operations*)))
  (is (string= (apply #'lines *specialised-operation-results*)
               (load-sml *specialised-operations*))))

(test real-to-string-is-gen-12
  (is (string= (lines "1.0" "0.333333333333" "10000000000.0" "1E22" "~2.5" "3.142")
               (run-sml "fun p r = print (r ^ \"\\n\")
val _ = p (Real.toString 1.0)
val _ = p (Real.toString (1.0 / 3.0))
val _ = p (Real.toString 1E10)
val _ = p (Real.toString 1E22)
val _ = p (Real.toString ~2.5)
val _ = p (Real.fmt (StringCvt.FIX (SOME 3)) 3.14159)"))))

(test equality-is-structural-except-for-references
  (is (string= (lines "true" "false" "true")
               (run-sml "fun b x = print ((if x then \"true\" else \"false\") ^ \"\\n\")
val _ = b (vector [1, 2] = vector [1, 2])
val _ = b (Array.fromList [1] = Array.fromList [1])
val a = Array.fromList [1]
val _ = b (a = a)"))))

(test intinf-and-sized-integers
  (is (string= (lines "1267650600228229401496703205376" "Overflow" "FFFFFFFFFFFFFFFF")
               (run-sml "val _ = print (IntInf.toString (IntInf.pow (IntInf.fromInt 2, 100)) ^ \"\\n\")
val _ = print ((Int32.toString (Int32.+ (valOf Int32.maxInt, Int32.fromInt 1))) handle Overflow => \"Overflow\\n\")
val _ = print (Word64.toString (Word64.fromInt ~1) ^ \"\\n\")"))))

(test large-int-is-intinf
  ;; LargeInt = IntInf (as in Poly/ML and MLton): toLarge never overflows.
  (is (string= (lines "true" "4611686018427387904" "Overflow" "18446744073709551615"
                      "4611686018427387904" "1000000000000000000000")
               (run-sml "fun say s = print (s ^ \"\\n\")
val big : LargeInt.int = IntInf.pow (2, 62)
val _ = say (Bool.toString (LargeInt.toString big = IntInf.toString big))
val _ = say (LargeInt.toString (LargeInt.+ (Int.toLarge (valOf Int.maxInt), 1)))
val _ = say (Int.toString (Int.fromLarge big) handle Overflow => \"Overflow\")
val _ = say (IntInf.toString (Word64.toLargeInt (Word64.fromInt ~1)))
val _ = say (IntInf.toString (Real.toLargeInt IEEEReal.TO_NEAREST 4.611686018427387904E18))
val _ = say (IntInf.toString (Time.toNanoseconds (Time.fromSeconds (IntInf.pow (10, 12)))))"))))

(test val-rec-layered-pattern
  ;; `rec' binds every variable of a layered pattern to the function
  ;; (Definition rule 26; HaMLet test valrec).
  (let ((program "val rec c : int -> int as d = fn 0 => 0 | n => d (n - 1) + 2
val _ = print (Int.toString (c 3) ^ \" \" ^ Int.toString (d 4) ^ \"\\n\")"))
    (is (string= (lines "6 8") (run-sml program)))
    (is (string= (lines "6 8") (load-sml program)))))

(test word-scan-prefix-without-digits
  ;; Basis WORD: "0wx" with no hex digit after it scans as 0 (it raised
  ;; Div), and "0wx" is a prefix only in hexadecimal.
  (is (string= (lines "0" "0" "A" "NONE" "B" "0")
               (run-sml "fun say w = print ((case w of NONE => \"NONE\" | SOME w => Word.toString w) ^ \"\\n\")
val _ = app say [Word.fromString \"0wx\", Word.fromString \"0w\", Word.fromString \"0xa\",
                 Word.fromString \"g\",
                 StringCvt.scanString (Word.scan StringCvt.DEC) \" \\t11\",
                 StringCvt.scanString (Word.scan StringCvt.DEC) \"0wx1\"]"))))

(test date-structure
  ;; Date (Basis DATE): calendar, normalisation, fmt, scan, time round trips.
  (is (string= (lines "Wed Mar 08 19:06:45 1995"
                      "1995-03-08 19:06:45 Wed Wednesday Mar March 067 10 10 3 PM 07 95 UTC"
                      "794689605" "Sat Mar 02 01:00:00 2024 61 Sat Mar" "Wed Mar 08 19:06:45 1995"
                      "LESS EQUAL" "true true" "Wed Dec 31 23:59:59 1969" "NONE")
               (run-sml "fun say s = print (s ^ \"\\n\")
val d = Date.fromTimeUniv (Time.fromSeconds 794689605)
val _ = say (Date.toString d)
val _ = say (Date.fmt \"%Y-%m-%d %H:%M:%S %a %A %b %B %j %U %W %w %p %I %y %Z\" d)
val _ = say (IntInf.toString (Time.toSeconds (Date.toTime d)))
val d2 = Date.date {year = 2024, month = Date.Feb, day = 30, hour = 25, minute = 0,
                    second = 0, offset = SOME Time.zeroTime}
val _ = say (Date.toString d2 ^ \" \" ^ Int.toString (Date.yearDay d2) ^ \" \"
             ^ Date.fmt \"%a\" d2 ^ \" \" ^ (if Date.month d2 = Date.Mar then \"Mar\" else \"?\"))
val d3 = valOf (Date.fromString \" Wed Mar 08 19:06:45 1995 rest\")
val _ = say (Date.toString d3)
fun ord LESS = \"LESS\" | ord EQUAL = \"EQUAL\" | ord GREATER = \"GREATER\"
val _ = say (ord (Date.compare (d, d2)) ^ \" \" ^ ord (Date.compare (d, d3)))
val now = Time.fromSeconds (Time.toSeconds (Time.now ()))
val l = Date.fromTimeLocal now
val _ = say (Bool.toString (Date.toTime l = now) ^ \" \"
             ^ Bool.toString (Date.offset l = NONE andalso isSome (Date.isDst l)))
val _ = say (Date.toString (Date.fromTimeUniv (Time.fromSeconds ~1)))
val _ = say (case Date.fromString \"Xyz Mar 08 19:06:45 1995\" of NONE => \"NONE\" | SOME _ => \"SOME\")"))))

;;; The IR simplifier (docs/IR.md) must not change what programs do.

(defun run-sml-both-ways (&rest sources)
  "What SOURCES print with the IR simplifier on, after checking that they
print the same with it off and with the IR off."
  (let ((simplified (apply #'run-sml sources)))
    (is (string= simplified
                 (let ((cl-sml:*sml-ir-simplify* nil)) (apply #'run-sml sources))))
    (is (string= simplified
                 (let ((cl-sml:*sml-ir* nil)) (apply #'run-sml sources))))
    simplified))

(test ir-inlining-keeps-static-scoping
  ;; g inlines f, which is rebound later: g still sees the first f.
  (is (string= "4 30 4"
               (run-sml-both-ways
                "fun f x = x + 1
fun g y = f y
val a = g 3
fun f x = x * 10
val _ = print (Int.toString a ^ \" \" ^ Int.toString (f 3) ^ \" \" ^ Int.toString (g 3))")))
  ;; The same across compilation units.
  (is (string= "4"
               (run-sml-both-ways "fun f x = x + 1  fun g y = f y"
                                  "fun f x = x * 10  val _ = print (Int.toString (g 3))"))))

(test ir-simplifier-keeps-effects-and-exceptions
  (is (string= "ab 3 Overflow Div 7 12 true"
               (run-sml-both-ways
                "fun twice f x = f (f x)
val r = ref \"\"
fun note s = (r := !r ^ s; 1)
val n = (fn (x, y) => x + y) (note \"a\", note \"b\") + 1
val _ = print (!r ^ \" \" ^ Int.toString n ^ \" \")
val _ = print ((Int.toString (valOf Int.maxInt + 1)) handle Overflow => \"Overflow \")
val _ = print ((Int.toString (1 div 0)) handle Div => \"Div \")
val _ = print (Int.toString (let val p = (3, 4) in #1 p + #2 p end) ^ \" \")
val _ = print (Int.toString (twice (fn x => x * 2) 3) ^ \" \")
val _ = print (Bool.toString (not (1 > 2)))"))))

(test ir-opens-basis-primitives
  (is (string= "6 2 0 1 98 c true"
               (run-sml-both-ways
                "val w = Word.xorb (0w5, 0w3)
val s = Word.<< (0w1, 0w1)
val z = Word.>> (0w1, Word.fromInt Word.wordSize)
val o1 = Word.toInt (Word.andb (0w13, 0w1))
val _ = print (Word.toString w ^ \" \" ^ Word.toString s ^ \" \" ^ Word.toString z
               ^ \" \" ^ Int.toString o1 ^ \" \" ^ Int.toString (Char.ord (String.sub (\"abc\", 1)))
               ^ \" \" ^ String.str (Char.chr 99) ^ \" \")
val _ = print (Bool.toString ((String.sub (\"abc\", 5); false) handle Subscript => true))"))))

(test ir-folding-does-not-hide-overflow
  (is (string= "Overflow Overflow 0w3"
               (run-sml-both-ways
                "val _ = print ((Int.toString (4611686018427387903 + 1)) handle Overflow => \"Overflow \")
val _ = print ((Int.toString (~4611686018427387904 - 1 * 1)) handle Overflow => \"Overflow \")
val _ = print (\"0w\" ^ Word.toString (0w4611686018427387903 + 0w4))"))))

;;; Decision trees (ROADMAP item 11) must match as trivia did.

(defun run-sml-with-and-without-trees (&rest sources)
  "What SOURCES print with matches compiled to decision trees, after
checking that they print the same when compiled by trivia."
  (let ((trees (apply #'run-sml sources)))
    (is (string= trees
                 (let ((cl-sml:*sml-decision-trees* nil)) (apply #'run-sml sources))))
    trees))

(test decision-trees-nested-and-overlapping-rules
  (is (string= "R B 3 2 1 0 9 a ok "
               (run-sml-with-and-without-trees
                "datatype color = R | B
datatype tree = E | T of color * tree * int * tree
fun p s = print (s ^ \" \")
fun balance (B, T (R, T (R, a, x, b), y, c), z, d) = T (R, T (B, a, x, b), y, T (B, c, z, d))
  | balance (B, a, x, T (R, b, y, T (R, c, z, d))) = T (R, T (B, a, x, b), y, T (B, c, z, d))
  | balance (color, a, x, b) = T (color, a, x, b)
fun col (T (R, _, _, _)) = \"R\" | col _ = \"B\"
val t = balance (B, T (R, T (R, E, 1, E), 2, E), 3, E)
val _ = p (col t)
val _ = p (case t of T (_, l, _, _) => col l | E => \"E\")
fun f (_, _, 0) = 0 | f (0, _, _) = 1 | f (_, 0, _) = 2 | f _ = 3
val _ = p (Int.toString (f (1, 1, 1)))
val _ = p (Int.toString (f (1, 0, 1)))
val _ = p (Int.toString (f (0, 0, 1)))
val _ = p (Int.toString (f (0, 0, 0)))
fun g (x :: _ :: _, _) = x | g ([x], y) = x + y | g ([], y) = y
val _ = p (Int.toString (g ([4], 5)))
fun h (#\"a\", \"x\") = \"a\" | h (_, \"y\") = \"y\" | h (c, _) = String.str c
val _ = p (h (#\"a\", \"x\"))
fun k {a = 1, b} = b | k {b = \"q\", ...} = \"q\" | k {a, ...} = \"ok\"
val _ = p (k {a = 2, b = \"r\"})"))))

(test decision-trees-exceptions-refs-and-layers
  (is (string= "E1 F 2 G other 5 (1,2) Match Bind Match "
               (run-sml-with-and-without-trees
                "fun p s = print (s ^ \" \")
exception E of int
exception F = E
fun mk () = let exception G in (G, fn G => \"G\" | _ => \"other\") end
val (g1, test1) = mk ()
val (g2, _) = mk ()
fun e (E 1) = \"E1\" | e (F n) = Int.toString n | e _ = \"?\"
val _ = p (e (E 1))
val _ = p ((raise F 3) handle E 1 => \"no\" | F _ => \"F\")
val _ = p (e (F 2))
val _ = p (test1 g1)
val _ = p (test1 g2)
fun r (ref 0) = 0 | r (ref n) = n
val _ = p (Int.toString (r (ref 5)))
fun s (x as (a, b), 0) = x | s (_, n) = (n, n)
val _ = p (case s ((1, 2), 0) of (a, b) => \"(\" ^ Int.toString a ^ \",\" ^ Int.toString b ^ \")\")
val _ = p ((case SOME 3 of NONE => \"none\") handle Match => \"Match\")
val _ = p ((let val [x] = [1, 2] in \"one\" end) handle Bind => \"Bind\")
val _ = p (((fn (\"a\", #\"b\") => 1) (\"a\", #\"c\") ; \"no\") handle Match => \"Match\")"))))

(test decision-trees-local-and-functor-constructors
  (is (string= "a b c d "
               (run-sml-with-and-without-trees
                "fun p s = print (s ^ \" \")
fun f n = let datatype t = A | B of int | C of int * int
              fun g A = \"a\" | g (B 0) = \"b\" | g (B _) = \"x\" | g (C (_, 1)) = \"c\" | g _ = \"y\"
          in g (case n of 0 => A | 1 => B 0 | _ => C (1, 1)) end
val _ = p (f 0)  val _ = p (f 1)  val _ = p (f 2)
functor M (type u) = struct datatype t = X | Y of u * u fun h (Y (_, _)) = \"d\" | h X = \"e\" end
structure N = M (type u = int)
val _ = p (N.h (N.Y (1, 2)))"))))

(test decision-trees-switch-on-self-tail-call-arguments
  ;; The switches test the loop variables of the self-tail calls directly.
  (is (string= "41 3 "
               (run-sml-with-and-without-trees
                "fun p s = print (s ^ \" \")
fun count (0, a) = a | count (1, a) = count (0, a + 1) | count (n, a) = count (n - 1, a + 2)
val _ = p (Int.toString (count (21, 0)))
datatype d = X | Y | Z
fun walk (X, n) = n | walk (Y, n) = walk (X, n + 1) | walk (Z, n) = walk (Y, n + 2)
val _ = p (Int.toString (walk (Z, 0)))"))))
