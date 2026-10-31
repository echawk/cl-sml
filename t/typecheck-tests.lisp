(defpackage #:cl-sml-typecheck-tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:types #:cl-sml.types)))

(in-package #:cl-sml-typecheck-tests)

(def-suite cl-sml-typecheck-suite
  :description "The native type checker (src/types/, milestone M1 of
DESIGN-TYPECHECKER.md): unifier, Core elaboration rules, errors,
overloading, value restriction, match checks and annotations")

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
    (is (= 2 (length (types::oclass-tynames (types::tvar-overload (types::repr a))))))
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
  ;; Resolved by the innermost value declaration, as in HaMLet.
  (is (rejects "val x = let fun f {a, ...} = a in f {a = 1, b = 2} end" "flexible record")))

(test error-escaping-local-datatype
  (is (rejects "val x = let datatype t = A in A end" "escapes"))
  (is (rejects "fun f y = let datatype t = A in y = A end" "escape")))

(test error-explicit-type-variables
  (is (binds "fun f (x : 'a) = let val y : 'a = x in y end" "val f : 'a -> 'a"))
  (is (rejects "fun f (x : 'a) = x + 1" "explicit type variable"))
  (is (binds "val f = fn x => let val g = fn (y : 'a) => (x, y) in x end" "val f : 'a -> 'a"))
  (is (rejects "val f = fn x => let val g = fn (y : 'a) => [x, y] in x end" "generalised"))
  (is (rejects "val 'a x = ref (fn (y : 'a) => y)" "generalised"))
  (is (rejects "exception E of 'a" "unbound type variable")))

(test error-modules-not-supported
  (is (rejects "structure S = struct end" "modules are not supported"))
  (is (rejects "val x = Int.toString 1" "modules are not supported"))
  (let ((cl-sml:*native-unchecked-long-ids* t))
    (is (binds "val s = Int.toString 1 val n = size \"ab\"" "val s : ?" "val n : int"))))

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
