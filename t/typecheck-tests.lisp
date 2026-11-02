(defpackage #:cl-sml-typecheck-tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:types #:cl-sml.types)))

(in-package #:cl-sml-typecheck-tests)

(def-suite cl-sml-typecheck-suite
  :description "The native type checker (src/types/, milestones M1 and M2
of docs/DESIGN-TYPECHECKER.md): unifier, Core and Modules elaboration rules,
signature matching, the elaborated Basis, errors, overloading, value
restriction, match checks and annotations")

(in-suite cl-sml-typecheck-suite)

;;;; Helpers

(defun check (source &rest options)
  "The printed bindings of SOURCE, with match warnings muffled."
  (handler-bind ((cl-sml:sml-match-warning #'muffle-warning))
    (values (apply #'cl-sml:typecheck-sml-string source options))))

(defun binds (source &rest lines)
  "Does SOURCE elaborate, printing each of LINES?"
  (let ((printed (check source)))
    (every (lambda (line)
             (search (format nil "~A~%" line) printed))
           lines)))

(defun type-error-message (source)
  "The message of the type error SOURCE draws, or NIL if it elaborates."
  (handler-case (progn (check source) nil)
    (cl-sml:sml-native-type-error (condition)
      (cl-sml:sml-native-type-error-message condition))))

(defun rejects (source &optional fragment)
  (let ((message (type-error-message source)))
    (and message (or (null fragment) (search fragment message)) t)))

(defun warnings (source)
  "The match warning messages of SOURCE."
  (let ((messages nil))
    (handler-bind ((cl-sml:sml-match-warning
                     (lambda (warning)
                       (push (cl-sml.types::sml-match-warning-message warning) messages)
                       (muffle-warning warning))))
      (cl-sml:typecheck-sml-string source))
    (nreverse messages)))

;;;; Unifier

(test unify-structures
  (let* ((a (types::fresh-tvar))
         (b (types::fresh-tvar))
         (int (types::int-type))
         (t1 (types::arrow-type a (types::list-type b)))
         (t2 (types::arrow-type int (types::list-type a))))
    (types::unify t1 t2)
    (is (equal "int -> int list" (types:type->string t1)))
    (is (eq (types::repr b) (types::repr a)))))

(test unify-occurs-check
  (let ((a (types::fresh-tvar)))
    (signals types::unify-failure
      (types::unify a (types::list-type a)))))

(test unify-clash
  (signals types::unify-failure
    (types::unify (types::int-type) (types::bool-type))))

(test unify-equality-attribute
  (let ((a (types::fresh-tvar :equality t))
        (b (types::fresh-tvar)))
    (types::unify a (types::list-type b))
    (is (types::tvar-equality (types::repr b)))
    (signals types::unify-failure
      (types::unify b (types::arrow-type (types::int-type) (types::int-type)))))
  (let ((a (types::fresh-tvar :equality t)))
    (signals types::unify-failure
      (types::unify a (types::con-type types::*t-real*))))
  ;; ref admits equality whatever its argument
  (let ((a (types::fresh-tvar :equality t)))
    (finishes (types::unify a (types::con-type types::*t-ref*
                                               (types::con-type types::*t-real*))))))

(test unify-overloading-classes
  (let ((a (types::fresh-tvar :overload types::*class-num*))
        (b (types::fresh-tvar :overload types::*class-realint*)))
    (types::unify a b)
    ;; Num and RealInt meet in RealInt (which the Basis's types extend).
    (is (= (length (types::oclass-tynames types::*class-realint*))
           (length (types::oclass-tynames (types::tvar-overload (types::repr a))))))
    (signals types::unify-failure (types::unify a (types::con-type types::*t-word*)))
    (finishes (types::unify a (types::con-type types::*t-real*))))
  (let ((a (types::fresh-tvar :overload types::*class-num* :equality t)))
    (signals types::unify-failure (types::unify a (types::con-type types::*t-real*)))))

(test unify-levels
  (let* ((types::*level* 1)
         (outer (types::fresh-tvar))
         (inner (let ((types::*level* 3)) (types::fresh-tvar))))
    (types::unify outer (types::list-type inner))
    (is (= 1 (types::tvar-level inner)))))

(test unify-rows
  (let* ((field (types::fresh-tvar))
         (flexible (list :record (list (cons "a" field)) (types::fresh-tvar :row t)))
         (closed (types::record-type (list (cons "a" (types::int-type))
                                           (cons "b" (types::bool-type))))))
    (types::unify flexible closed)
    (is (equal "{a : int, b : bool}" (types:type->string flexible))))
  (signals types::unify-failure
    (types::unify (types::record-type (list (cons "a" (types::int-type))))
                  (types::record-type (list (cons "b" (types::int-type)))))))

(test unify-rigid-variables
  (let ((a (types::make-rigid-tvar "'a"))
        (b (types::make-rigid-tvar "'b")))
    (signals types::unify-failure (types::unify a b))
    (signals types::unify-failure (types::unify a (types::int-type)))
    (let ((flexible (types::fresh-tvar)))
      (types::unify flexible a)
      (is (eq a (types::repr flexible))))))

;;;; Rules: expressions, patterns, declarations

(test elab-constants-and-records
  (is (binds "val x = (1, \"a\", #\"c\", 1.5, 0w2, [true], ())"
             "val x : int * string * char * real * word * bool list * unit"))
  (is (binds "val r = {b = 1, a = true}" "val r : {a : bool, b : int}"))
  (is (binds "val s = #b {a = 1, b = \"x\"}" "val s : string")))

(test elab-functions-and-polymorphism
  (is (binds "fun f x y = (y, x)" "val f : 'a -> 'b -> 'b * 'a"))
  (is (binds "val id = fn x => x val p = (id 1, id true)"
             "val id : 'a -> 'a" "val p : int * bool"))
  (is (binds "fun len [] = 0 | len (_ :: t) = 1 + len t" "val len : 'a list -> int"))
  (is (binds "fun f x = x = x" "val f : ''a -> bool"))
  (is (binds "val rec f = fn 0 => 1 | n => n * f (n - 1)" "val f : int -> int"))
  (is (binds "fun even 0 = true | even n = odd (n - 1) and odd 0 = false | odd n = even (n - 1)"
             "val even : int -> bool" "val odd : int -> bool")))

(test elab-derived-forms
  (is (binds "val x = if 1 < 2 andalso true orelse false then \"a\" else \"b\"" "val x : string"))
  (is (binds "val x = case [1] of [] => 0 | h :: _ => h" "val x : int"))
  (is (binds "val x = (print \"a\"; 3)" "val x : int"))
  (is (binds "val x = let val y = 2 in y * y end" "val x : int"))
  (is (binds "val u = while false do ()" "val u : unit"))
  (is (binds "1 + 1" "val it : int")))

(test elab-patterns
  (is (binds "fun f (x as (a, _)) = (x, a)" "val f : 'a * 'b -> ('a * 'b) * 'a"))
  (is (binds "fun f {a, b = (c : int, _)} = a + c" "val f : {a : int, b : int * 'a} -> int"))
  (is (binds "val SOME x = SOME 3" "val x : int"))
  (is (binds "fun f (ref x) = x" "val f : 'a ref -> 'a"))
  (is (binds "fun f #\"a\" = 1 | f _ = 2" "val f : char -> int")))

(test elab-datatypes
  (is (binds "datatype 'a tree = L | N of 'a tree * 'a * 'a tree
              fun ins x L = N (L, x, L)
                | ins x (t as N (l, y, r)) = if x < y then N (ins x l, y, r) else t"
             "datatype 'a tree = L | N of 'a tree * 'a * 'a tree"
             "val ins : int -> int tree -> int tree"))
  (is (binds "datatype t = A of u and u = B of t | C" "datatype t = A of u" "datatype u = B of t | C"))
  (is (binds "datatype t = A of s withtype s = int * int val x = A (1, 2)"
             "type s = int * int" "val x : t"))
  (is (binds "datatype t = datatype bool" "datatype t = false | true"))
  (is (binds "type 'a pair = 'a * 'a val p : int pair = (1, 2)"
             "type 'a pair = 'a * 'a" "val p : int * int")))

(test elab-equality-of-datatypes
  (is (binds "datatype t = A of int | B val b = A 1 = B" "val b : bool"))
  (is (rejects "datatype t = A of real val b = A 1.0 = A 2.0" "equality"))
  (is (rejects "datatype t = F of int -> int val b = F (fn x => x) = F (fn x => x)" "equality"))
  (is (binds "val b = ref 1.0 = ref 2.0" "val b : bool")))

(test elab-abstype-and-local
  (is (binds "abstype t = T of int with fun mk n = T n fun get (T n) = n end"
             "type t = t" "val get : t -> int" "val mk : int -> t"))
  (is (rejects "abstype t = T of int with fun mk n = T n end val x = T 1" "unknown identifier"))
  (is (rejects "abstype t = T of int with fun mk n = T n end val b = mk 1 = mk 2" "equality"))
  (is (binds "local val a = 1 in val b = a end" "val b : int")))

(test elab-exceptions
  (is (binds "exception E of int exception F = E val e = E 3"
             "exception E of int" "exception F of int" "val e : exn"))
  (is (binds "val x = (raise Fail \"x\") handle Fail s => size s | _ => 0" "val x : int"))
  (is (rejects "exception E val x = E 1")))

;;;; Ill-typed programs

(test error-clash-reports-position-and-types
  (handler-case (progn (check (format nil "val a = 1~%val x = 1 + true")) (fail "accepted"))
    (cl-sml:sml-native-type-error (condition)
      (is (= 2 (cl-sml:sml-native-type-error-line condition)))
      (is (= 2 (length (cl-sml:sml-native-type-error-types condition))))
      (is (search "bool" (second (cl-sml:sml-native-type-error-types condition)))))))

(test error-occurs-check
  (is (rejects "fun f x = f x x" "circular")))

(test error-equality-type
  (is (rejects "fun f (x : real) = x = x" "equality"))
  (is (rejects "val b = (fn x => x) = (fn x => x)" "equality")))

(test error-unknown-identifiers
  (is (rejects "val x = y" "unknown identifier y"))
  (is (rejects "val x : t = 1" "unknown type constructor"))
  (is (rejects "fun f (SOME) = 1" "needs an argument")))

(test error-value-restriction-strict-and-relaxed
  (is (rejects "val r = ref []" "not determined"))
  (is (rejects "val f = (fn x => x) (fn y => y)" "not determined"))
  ;; Resolved later in the program: accepted, as HaMLet does.
  (is (binds "val r = ref [] val _ = r := [1]" "val r : int list ref"))
  ;; Relaxed: covariant variables generalise, invariant (ref) ones do not.
  (let ((cl-sml:*native-relaxed-value-restriction* t))
    (is (binds "val l = (fn x => x) []" "val l : 'a list"))
    (is (binds "fun f () = NONE val x = f ()" "val x : 'a option"))
    (is (rejects "val r = ref []" "not determined"))
    (is (rejects "val f = (fn x => x) (fn y => y)" "not determined")))
  (is (rejects "val l = (fn x => x) []" "not determined")))

(test error-unresolved-flexible-record
  (is (rejects "fun f r = #a r" "flexible record"))
  (is (rejects "fun f {a, ...} = a" "flexible record"))
  (is (binds "fun f (r : {a : int, b : bool}) = #a r" "val f : {a : int, b : bool} -> int"))
  ;; Never resolved in its top-level declaration.
  (is (rejects "val l = [#a, #b] val n = 1" "flexible record"))
  (is (rejects "structure S = struct val f = #a end" "flexible record")))

(test flexible-record-resolved-in-top-level-declaration
  ;; Poly/ML and MLton resolve a flexible record by the end of the
  ;; top-level declaration (HaMLet: the innermost value declaration).
  (is (binds "val x = let fun f {a, ...} = a in f {a = 1, b = 2} end" "val x : int"))
  ;; HOL's Count.sml: a list of selectors resolved by a later binding.
  (is (binds "structure S = struct
                val l = [#ABS, #COMB]
                fun total (r : {ABS : int, COMB : int, VAR : int}) =
                  foldl (fn (f, n) => f r + n) 0 l
              end"
             "    val l : ({ABS : int, COMB : int, VAR : int} -> int) list"))
  ;; A type with an unresolved row is not generalised: both uses of f
  ;; share it, so the field types must agree.
  (is (rejects "val x = let fun f r = #a r in (f {a = 1, b = 2}, f {a = true, b = 2}) end"
               "do not agree"))
  (is (binds "val x = let fun f r = #a r in (f {a = 1, b = 2}, f {a = 3, b = 4}) end"
             "val x : int * int")))

(test error-escaping-local-datatype
  (is (rejects "val x = let datatype t = A in A end" "escapes"))
  (is (rejects "fun f y = let datatype t = A in y = A end" "escape"))
  (is (rejects "val u = let val r = ref nil datatype t = C in r := [C] end" "escape")))

(test error-explicit-type-variables
  (is (binds "fun f (x : 'a) = let val y : 'a = x in y end" "val f : 'a -> 'a"))
  (is (rejects "fun f (x : 'a) = x + 1" "explicit type variable"))
  (is (binds "val f = fn x => let val g = fn (y : 'a) => (x, y) in x end" "val f : 'a -> 'a"))
  (is (rejects "val f = fn x => let val g = fn (y : 'a) => [x, y] in x end" "generalised"))
  (is (rejects "val 'a x = ref (fn (y : 'a) => y)" "generalised"))
  (is (rejects "exception E of 'a" "free type variable"))
  (is (binds "local exception E of 'a in end val x = 1" "val x : int"))
  (is (rejects "val x : 'a = 1" "explicit type variable"))
  (is (rejects "type t = 'a list" "unbound type variable"))
  (is (rejects "val 'a x = let val 'a y = () in () end" "shadows")))

(test error-unknown-structures
  (is (rejects "val x = Nope.toString 1" "unknown structure Nope"))
  (is (rejects "val x = Int.nope" "unknown identifier Int.nope"))
  (is (rejects "open Nope" "unknown structure"))
  (let ((cl-sml:*native-unchecked-long-ids* t))
    (is (binds "val s = Nope.toString 1 val n = size \"ab\"" "val s : ?" "val n : int"))
    (is (binds "val s = Int.toString 1" "val s : string"))))

;;;; Overloading [Appendix E]

(test overloading-defaults
  (is (binds "fun double x = x + x" "val double : int -> int"))
  (is (binds "fun f x = x + 1.0" "val f : real -> real"))
  (is (binds "val w = 0w1 + 0w2 val d = 7 div 2 val l = \"a\" < \"b\""
             "val w : word" "val d : int" "val l : bool"))
  (is (binds "val x = let val g = fn (a, b) => a * b in g (1.5, 2.0) end" "val x : real"))
  (is (rejects "val x = 1.0 div 2.0" "overloaded"))
  (is (rejects "val x = 1 + 1.0")))

;;;; Match checks [Section 4.11]

(test match-warnings
  (is (equal '("match is not exhaustive")
             (warnings "datatype t = A | B val f = fn A => 1")))
  (is (equal '("match rule 2 is redundant")
             (warnings "val f = fn 1 => true | 1 => false | _ => false")))
  (is (equal '("binding is not exhaustive") (warnings "val SOME x = SOME 3")))
  (is (null (warnings "fun f [] = 0 | f [_] = 1 | f (_ :: _ :: _) = 2")))
  (is (null (warnings "fun f (true, _) = 1 | f (false, true) = 2 | f (false, false) = 3")))
  (is (equal '("match is not exhaustive")
             (warnings "fun f (true, _) = 1 | f (false, true) = 2")))
  (is (null (warnings "val x = (raise Fail \"x\") handle Fail _ => 1")))
  (is (equal '("match rule 2 is redundant")
             (warnings "val x = 1 handle Div => 1 | Div => 2")))
  (let ((cl-sml:*native-match-warnings* nil))
    (is (equal "val f : t -> int
" (subseq (cl-sml:typecheck-sml-string "datatype t = A | B val f = fn A => 1") 19)))))

;;;; Annotations

(test annotations-for-later-passes
  (let* ((source "val a = 1 = 2 val b = 2.0 < 3.0 fun f r = #x (r : {x : int, y : bool})")
         (table (nth-value 2 (cl-sml:typecheck-sml-string source)))
         (nodes nil))
    (maphash (lambda (node plist) (push (cons node plist) nodes)) table)
    (flet ((node-named (name)
             (car (find-if (lambda (entry)
                             (and (consp (car entry)) (member (car (car entry)) '(:var :ctor))
                                  (equal (second (car entry)) name)))
                           nodes))))
      (is (equal "int" (types:type->string (types:native-equality-type (node-named "=") table))))
      (is (equal "real" (types:native-overloaded-instance (node-named "<") table)))
      (is (equal "real" (types:native-node-fact-info (node-named "<") "ov" table)))
      (is (equal "v" (types:native-node-fact-info (node-named "<") "id" table)))
      (let ((selector (car (find-if (lambda (entry) (eq (car (car entry)) :selector)) nodes))))
        (is (equal '("x" "y") (mapcar #'car (types:native-record-row selector table))))))))

;;;; Modules [rules 50-89]

(test basis-elaborates
  (let ((env (types::initial-env)))
    (dolist (name '("General" "List" "Option" "String" "Int" "IntInf" "Word8" "Real"
                    "Vector" "Array" "TextIO" "OS" "CommandLine" "Time"))
      (is (types::env-lookup-strid env name) "structure ~A is missing" name))
    (is (binds "val x = List.map (fn x => x + 1) [1, 2] val s = String.concat [\"a\"]
                val p = TextIO.print val m = Option.map"
               "val x : int list" "val s : string" "val p : string -> unit"
               "val m : ('a -> 'b) -> 'a option -> 'b option"))
    ;; The Basis's top-level values (values.sml).
    (is (binds "val a = valOf (SOME 1) val b = rev [true] val c = 3 <> 4"
               "val a : int" "val b : bool list" "val c : bool"))
    (is (< types::*basis-elaboration-seconds* 10))))

(test module-structures-and-long-identifiers
  (is (binds "structure S = struct val x = 1 type t = int datatype d = A | B of t end
              val y = S.x + 1 val z : S.t = 2 val d = S.B 3
              val n = case d of S.A => 0 | S.B k => k"
             "val y : int" "val z : int" "val d : d" "val n : int"))
  (is (binds "structure S = struct structure T = struct val x = \"a\" end end val y = S.T.x"
             "val y : string"))
  (is (binds "structure S = struct local val x = 3 in val y = x end end open S val z = y"
             "val z : int"))
  (is (binds "structure A = List structure B = A val l = B.length [1]" "val l : int"))
  (is (binds "structure S = let val x = 1 in struct val y = x end end val z = S.y"
             "val z : int"))
  (is (rejects "structure S = struct val x = 1 end val y = S.z" "unknown identifier S.z"))
  (is (rejects "val x = S.A" "unknown structure S")))

(test module-signature-matching
  (is (binds "signature S = sig type t val x : t end
              structure X : S = struct type t = int val x = 1 val hidden = 2 end
              val y = X.x + 1"
             "val y : int"))
  ;; Transparent ascription keeps the representation but hides other names.
  (is (rejects "structure X : sig val x : int end = struct val x = 1 val y = 2 end
                val z = X.y" "unknown identifier X.y"))
  (is (rejects "structure S = struct val x = 1 end : sig val y : int end"
               "value y is specified but not declared"))
  (is (rejects "structure S : sig type t end = struct end" "type t is specified but not declared"))
  (is (rejects "structure S : sig structure A : sig end end = struct end"
               "structure A is specified but not declared"))
  (is (rejects "structure S : sig val f : 'a -> 'a end = struct fun f x = x + 1 end"
               "the signature says 'a -> 'a"))
  (is (binds "structure S : sig val f : int -> int end = struct fun f x = x end
              val y = S.f 1"
             "val y : int"))
  (is (rejects "structure S : sig exception E end = struct val E = 1 end" "is a value"))
  (is (rejects "structure S : sig datatype t = A | B end = struct datatype t = A | B | C end"
               "constructors"))
  (is (rejects "structure S : sig type 'a t end = struct type t = int end" "argument"))
  ;; An undetermined type may not become polymorphic (HaMLet undetermined3).
  (is (rejects "structure X : sig val f : 'a -> 'a option end =
                struct fun g () = let val r = ref NONE in fn x => !r before r := SOME x end
                       val f = g () end")))

(test module-opaque-ascription
  ;; Opaque ascription hides the representation ...
  (is (rejects "structure X :> sig type t val x : t end = struct type t = int val x = 1 end
                val y = X.x + 1"))
  (is (binds "structure X :> sig type t val x : t val f : t -> int end =
                struct type t = int val x = 1 fun f x = x end
              val y = X.f X.x"
             "val y : int"))
  ;; ... and equality, unless the signature says eqtype.
  (is (rejects "structure X :> sig type t val x : t end = struct type t = int val x = 1 end
                val b = X.x = X.x" "equality"))
  (is (binds "structure X :> sig eqtype t val x : t end = struct type t = int val x = 1 end
              val b = X.x = X.x"
             "val b : bool"))
  (is (rejects "structure X : sig eqtype t end = struct type t = int -> int end"
               "does not admit equality"))
  ;; Datatypes in an opaque signature are new types with their constructors.
  (is (binds "structure X :> sig datatype t = A | B end = struct datatype t = A | B end
              val y = case X.A of X.A => 1 | X.B => 2"
             "val y : int")))

(test module-where-type-and-sharing
  (is (binds "signature S = sig type t val x : t end
              structure X :> S where type t = int = struct type t = int val x = 1 end
              val y = X.x + 1"
             "val y : int"))
  (is (rejects "signature S = sig type t end where type u = int" "unknown type"))
  (is (rejects "signature S = sig type t = int end where type t = bool" "rigid"))
  (is (rejects "signature S = sig eqtype t end where type t = real" "equality"))
  (is (binds "signature S = sig type t type u sharing type t = u end
              structure X : S = struct type t = int type u = int end"
             "structure X :"))
  (is (rejects "signature S = sig type t type u sharing type t = u end
                structure X : S = struct type t = int type u = bool end"
               "the signature says"))
  (is (rejects "signature S = sig type t = int type u sharing type t = u end" "rigid"))
  ;; Structure sharing (Appendix A) shares the types in common.
  (is (binds "signature S = sig structure A : sig type t val x : t end
                                structure B : sig type t val f : t -> int end
                                sharing A = B end
              functor F (X : S) = struct val y = X.B.f X.A.x end"
             "functor F(Arg :"))
  (is (rejects "functor F (X : sig structure A : sig type t val x : t end
                                   structure B : sig type t val f : t -> int end end) =
                struct val y = X.B.f X.A.x end"))
  (is (rejects "signature S = sig val x : int val x : bool end" "duplicate specification"))
  (is (binds "signature A = sig type t val x : t end
              signature B = sig include A val f : t -> int end
              structure S : B = struct type t = bool val x = true fun f b = if b then 1 else 0 end
              val n = S.f S.x"
             "val n : int")))

(test module-functors
  (is (binds "functor F (X : sig type t val x : t end) = struct val l = [X.x, X.x] end
              structure A = F (struct type t = string val x = \"a\" end)
              val l = A.l"
             "val l : string list"))
  ;; The derived form with a specification as parameter.
  (is (binds "functor F (type t val x : t) = struct val l = [x] end
              structure A = F (type t = int val x = 3) val l = A.l"
             "val l : int list"))
  (is (rejects "functor F (X : sig val x : int end) = struct end structure A = F (struct end)"
               "value x is specified but not declared"))
  (is (rejects "structure A = Nope (struct end)" "unknown functor"))
  ;; Each application generates new types.
  (is (rejects "functor F (X : sig end) = struct datatype t = T val x = T end
                structure A = F (struct end) structure B = F (struct end)
                val l = [A.x, B.x]"))
  (is (binds "functor F (X : sig end) = struct datatype t = T val x = T end
              structure A = F (struct end) val l = [A.x, A.x]"
             "val l : t list"))
  ;; An opaque result signature hides the representation.
  (is (rejects "functor F (X : sig end) :> sig type t val x : t end =
                  struct type t = int val x = 1 end
                structure A = F (struct end) val y = A.x + 1"))
  (is (binds "functor F (X : sig type t end) : sig type u val mk : X.t -> u end =
                struct type u = X.t fun mk x = x end
              structure A = F (struct type t = int end) val y = A.mk 3 + 1"
             "val y : int")))

(test module-escape-and-top-level
  (is (rejects "structure S = struct val x = let datatype t = A in A end end" "escapes"))
  (is (rejects "structure S = struct val r = ref nil end" "not determined"))
  ;; Each `;'-terminated program must determine its types (HaMLet
  ;; undetermined2).
  (is (rejects "val a = ref nil; val _ = a := [1];" "not determined"))
  (is (binds "val a = ref nil val _ = a := [1];" "val a : int list ref")))

(test overloading-basis-types
  (is (binds "val x : IntInf.int = 12345678901234567890 * 2 + 1" "val x : int"))
  (is (binds "val w : Word8.word = 0w255 + 0w1 val m = Word8.andb (w, 0wx0f)"
             "val w : word8" "val m : word8"))
  (is (binds "val y = Int64.+ (1, 2) val z : Word32.word = 0w3 * 0w4"
             "val y : int" "val z : word"))
  (is (binds "val l = IntInf.< (1, 2)" "val l : bool"))
  ;; Defaults stay int, word and real.
  (is (binds "val n = 1 + 2 val w = 0w1 val r = 1.5" "val n : int" "val w : word" "val r : real"))
  (is (rejects "val x : IntInf.int = 1 val y : int = x")))

;;;; M3: the native checker as the compiler's static front end

(defun run-checked (source package)
  "Compile SOURCE in PACKAGE with the default checker and run it."
  (eval (cl-sml:compile-sml-program-string source :package package)))

(defun checked-value (name package)
  (cl-sml:with-sml-package (package) (cl-sml:sml-value name)))

(test checking-is-on-by-default
  (is (eq :native cl-sml:*sml-type-checker*)))

(test compiler-rejects-ill-typed-programs
  (let ((condition (handler-case (progn (run-checked "val n = 1 + 2
                                                      val bad : string = n"
                                                     "SML.M3-REJECT-TEST")
                                        nil)
                     (cl-sml:sml-static-type-error (e) e))))
    (is (typep condition 'cl-sml:sml-native-type-error))
    (is (equal '(2 59) (list (cl-sml:sml-native-type-error-line condition)
                            (cl-sml:sml-native-type-error-column condition))))
    (is (equal '("string" "int") (cl-sml:sml-native-type-error-types condition))))
  ;; Plain mode still compiles it.
  (let ((cl-sml:*sml-type-checker* nil))
    (finishes (cl-sml:compile-sml-program-string "val bad : string = 1 + 2"
                                                 :package "SML.M3-REJECT-TEST"))))

(test load-sml-file-rejects-ill-typed-file
  (let ((file (asdf:system-relative-pathname "cl-sml" "t/testdata/ill-typed.sml")))
    (let ((condition (handler-case (progn (cl-sml:load-sml-file file :package "SML.M3-FILE-TEST")
                                          nil)
                       (cl-sml:sml-static-type-error (e) e))))
      (is (not (null condition)))
      (is (search "ill-typed.sml:3:" (princ-to-string condition))))))

(test repl-rejects-ill-typed-input-and-goes-on
  (let ((output (make-string-output-stream))
        (error-output (make-string-output-stream)))
    (cl-sml:repl :input (make-string-input-stream
                         (format nil "val r : string = 3;~%val ok = 1;~%:quit~%"))
                 :output output :error-output error-output :prompt nil
                 :package "SML.M3-REPL-REJECT-TEST")
    (is (search "type error" (get-output-stream-string error-output)))
    (is (search "val ok = 1 : int" (get-output-stream-string output)))))

(test repl-shadowing-keeps-earlier-code
  (let ((output (make-string-output-stream))
        (error-output (make-string-output-stream)))
    (cl-sml:repl :input (make-string-input-stream
                         (format nil "val x = 1;~%fun getx () = x;~%val x = \"s\";~%getx () + 1;~%x ^ \"t\";~%:quit~%"))
                 :output output :error-output error-output :prompt nil
                 :package "SML.M3-REPL-SHADOW-TEST")
    (let ((text (get-output-stream-string output)))
      (is (search "val x = \"s\" : string" text))
      (is (search "val it = 2 : int" text))
      (is (search "val it = \"st\" : string" text)))
    (is (string= "" (get-output-stream-string error-output)))))

(defun eval-sml-block (string)
  (let ((*readtable* (cl-sml:enable-sml-syntax (copy-readtable nil)))
        (*package* (find-package '#:cl-sml-typecheck-tests)))
    (eval (read-from-string string))))

(test sml-blocks-are-checked
  (signals cl-sml:sml-static-type-error (eval-sml-block "#@sml{ 1 + \"a\" }#"))
  ;; Splices have a type of their own, which the SML determines.
  (is (= 12 (eval-sml-block "(let ((x 3)) #@sml{ ($x : int) * 4 }#)")))
  ;; (SBCL reports an error of the macroexpansion inside LET as its own.)
  (signals error
    (eval-sml-block "(let ((x 3)) #@sml{ ($x : int) ^ \"a\" }#)")))

(test intinf-and-word8-literals-run
  (let ((package "SML.M3-LITERAL-TEST"))
    (run-checked "val big : IntInf.int = 100000000000000000000 * 100000000000000000000
                  val q = big div 3
                  val neg = ~ big
                  val w : Word8.word = 0w250 + 0w10
                  val w2 = Word8.toInt (0wxff * 0w2 : Word8.word)
                  val n = IntInf.toString (IntInf.pow (2, 100))"
                 package)
    (is (= (expt 10 40) (checked-value "big" package)))
    (is (= (floor (expt 10 40) 3) (checked-value "q" package)))
    (is (= (- (expt 10 40)) (checked-value "neg" package)))
    (is (= 4 (checked-value "w" package)))
    (is (= 254 (checked-value "w2" package)))
    (is (string= (princ-to-string (expt 2 100)) (checked-value "n" package))))
  (signals cl-sml:sml-static-type-error
    (run-checked "val w : Word8.word = 0w256" "SML.M3-LITERAL-TEST"))
  (signals cl-sml:sml-static-type-error
    (run-checked "val i = 100000000000000000000" "SML.M3-LITERAL-TEST")))

(test use-sees-what-the-used-file-bound
  ;; Each `;'-terminated program of a file is checked after the one before
  ;; it ran (t/testdata/use-main.sml uses use-child.sml).
  (multiple-value-bind (package)
      (cl-sml:load-sml-file (asdf:system-relative-pathname "cl-sml" "t/testdata/use-main.sml")
                            :package "SML.M3-USE-TEST")
    (is (= 42 (checked-value "used_main" (package-name package))))))
