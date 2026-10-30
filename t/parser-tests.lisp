(defpackage #:cl-sml-tests
  (:use #:cl #:fiveam #:cl-sml))

(in-package #:cl-sml-tests)

(def-suite cl-sml-parser-suite
  :description "Tests for the cl-sml lexer and parser")

(in-suite cl-sml-parser-suite)

;;; Each parse starts from the initial basis, so tests do not see each
;;; other's fixity or constructors.

(defun initial-scopes ()
  (list cl-sml::*sml-builtin-scope*))

(defun parse-exp (text)
  (parse-sml-expression text :scopes (initial-scopes)))

(defun parse-pat (text)
  (cl-sml::parse-sml-pattern text :scopes (initial-scopes)))

(defun parse-ty (text)
  (cl-sml::parse-sml-type text))

(defun parse-prog (text)
  (values (parse-sml-program text :scopes (initial-scopes))))

(defun parse-decs (text)
  "The declarations of program TEXT."
  (rest (parse-prog text)))

(defun parse-dec (text)
  (first (parse-decs text)))

(defun token-values (text)
  (loop for token across (cl-sml::lex-sml text)
        until (eq (cl-sml::sml-token-kind token) :eof)
        collect (list (cl-sml::sml-token-kind token) (cl-sml::sml-token-value token))))

(defun syntax-error-of (thunk)
  (handler-case (progn (funcall thunk) nil)
    (sml-syntax-error (condition) condition)))

;;; Lexical structure (Definition chapter 2)

(test lex-integer-and-word-constants
  (is (equal 42 (parse-exp "42")))
  (is (equal -10 (parse-exp "~10")))
  (is (equal 1114111 (parse-exp "0x10FFFF")))
  (is (equal -31 (parse-exp "~0x1F")))
  (is (equal '(:word 12) (parse-exp "0w12")))
  (is (equal '(:word 255) (parse-exp "0wxFF")))
  ;; `~` followed by a digit is part of the constant; otherwise it is the
  ;; negation function.
  (is (equal '(:app (:var "~") (:var "x")) (parse-exp "~ x")))
  (is (equal '(:app (:var "f") -1) (parse-exp "f ~1"))))

(test lex-real-constants
  (is (= 3.8d0 (parse-exp "3.8")))
  (is (= -0.5d0 (parse-exp "~0.5")))
  (is (= 1000 (rational (parse-exp "1E3"))))
  (is (= (expt 10 10) (rational (parse-exp "1e10"))))
  (is (= (expt 10 22) (rational (parse-exp "1E22"))))
  (is (= -2500 (rational (parse-exp "~2.5e3"))))
  ;; The nearest doubles, whatever the host's reader would make of them.
  (is (= 2833419889721787/18889465931478580854784 (rational (parse-exp "1.5E~7"))))
  (is (= 3602879701896397/36028797018963968 (rational (parse-exp "0.1"))))
  (is (= 99999999999999991611392 (rational (parse-exp "1E23"))))
  (is (eql 0.0015d0 (parse-exp "1.5E~3")))
  (signals sml-syntax-error (parse-exp "1E400"))
  ;; `1.` and `.5` are not reals.
  (is (equal '(:app (:var "f") 1) (parse-exp "f 1"))))

(test lex-string-and-character-escapes
  (is (equal "hello" (parse-exp "\"hello\"")))
  (is (char= #\a (parse-exp "#\"a\"")))
  (is (equal (format nil "line~%tab~C" #\Tab) (parse-exp "\"line\\ntab\\t\"")))
  (is (equal "A" (parse-exp "\"\\065\"")))
  (is (equal "A" (parse-exp "\"\\u0041\"")))
  (is (equal (coerce (list (code-char 7) (code-char 8) (code-char 11) (code-char 12)
                           (code-char 13) #\" #\\)
                     'string)
             (parse-exp "\"\\a\\b\\v\\f\\r\\\"\\\\\"")))
  (is (equal (string (code-char 1)) (parse-exp "\"\\^A\"")))
  (is (equal (string (code-char 31)) (parse-exp "\"\\^_\"")))
  ;; A gap \f...f\ is ignored.
  (is (equal "hello" (parse-exp (format nil "\"\\~%   \\hello\""))))
  (is (equal "" (parse-exp (format nil "\"\\~%\\\""))))
  ;; Bytes outside ASCII are taken literally.
  (is (equal (format nil "caf~C" (code-char 233))
             (parse-exp (format nil "\"caf~C\"" (code-char 233)))))
  (signals sml-syntax-error (parse-exp "\"\\q\""))
  (signals sml-syntax-error (parse-exp "\"\\06\""))
  (signals sml-syntax-error (parse-exp (format nil "\"two~%lines\"")))
  (signals sml-syntax-error (parse-exp "#\"ab\"")))

(test lex-identifiers
  (is (equal '((:alnum "x'") (:alnum "vid'1") (:alnum "a_B2") (:tyvar "'a")
               (:tyvar "''eq") (:symbolic ">>=") (:symbolic "::") (:long "Word.~>>")
               (:long "List.@") (:long "A.B.c"))
             (token-values "x' vid'1 a_B2 'a ''eq >>= :: Word.~>> List.@ A.B.c")))
  ;; Reserved symbols are not identifiers, but longer symbolic runs are.
  (is (equal '((:reserved :equals) (:reserved :darrow) (:reserved :arrow)
               (:reserved :seal) (:reserved :colon) (:reserved :bar)
               (:symbolic "==") (:symbolic ":=") (:symbolic "|>") (:reserved :hash))
             (token-values "= => -> :> : | == := |> #")))
  (is (equal '(:tuple (:var "~") (:var "Word.~") (:var "Word8.~") (:var "~"))
             (parse-exp "(~, Word.~, Word8.~, ~)"))))

(test lex-keywords-are-delimited
  (is (equal '(:val (:pat-var "recx") 5) (parse-dec "val recx = 5")))
  (is (equal '(:val (:pat-var "record") 1) (parse-dec "val record = 1")))
  (is (equal '(:val (:pat-var "recorded") 1) (parse-dec "val recorded = 1;")))
  (is (equal '(:app (:var "raiseIt") (:unit)) (parse-exp "raiseIt ()")))
  (is (equal '(:val-rec "f" (:fn (((:pat-var "x") (:var "x")))))
             (parse-dec "val rec f = fn x => x"))))

(test lex-nested-comments
  (is (equal '(:program (:val (:pat-var "x") 10))
             (parse-prog "(* outer (* inner *) *) val x = 10; (**)")))
  (let ((condition (syntax-error-of (lambda () (parse-prog (format nil "val x = 1~%(* open (* *)"))))))
    (is (not (null condition)))
    (is (= 2 (cl-sml::sml-syntax-error-line condition)))
    (is (search "unterminated comment" (princ-to-string condition)))))

;;; Expressions and fixity (Definition section 2.6)

(test parse-applications
  (is (equal '(:app (:var "f") (:var "x")) (parse-exp "f x")))
  (is (equal '(:app (:app (:var "f") (:var "x")) (:var "y")) (parse-exp "f x y"))))

(test parse-infix-applications-as-pairs
  (is (equal '(:app (:var "+") (:tuple (:var "a") (:var "b"))) (parse-exp "a + b")))
  (is (equal '(:app (:var "*") (:tuple (:app (:var "+") (:tuple (:var "a") (:var "b")))
                                       (:var "c")))
             (parse-exp "(a + b) * c")))
  (is (equal '(:app (:var "+") (:tuple (:var "a")
                                       (:app (:var "*") (:tuple (:var "b") (:var "c")))))
             (parse-exp "a + b * c")))
  (is (equal '(:app (:var "-") (:tuple (:app (:var "-") (:tuple (:var "a") (:var "b")))
                                       (:var "c")))
             (parse-exp "a - b - c")))
  (is (equal '(:app (:var "mod") (:tuple (:var "n") 26)) (parse-exp "n mod 26")))
  (is (equal '(:app (:ctor "::") (:tuple 1 (:app (:ctor "::") (:tuple 2 (:ctor "nil")))))
             (parse-exp "1 :: 2 :: nil")))
  (is (equal '(:app (:var "=") (:tuple (:var "a") (:app (:var "@") (:tuple (:var "b") (:var "c")))))
             (parse-exp "a = b @ c")))
  (is (equal '(:app (:var "before") (:tuple (:app (:var ":=") (:tuple (:var "r") 1)) (:var "x")))
             (parse-exp "r := 1 before x")))
  (is (equal '(:app (:var "!") (:var "r")) (parse-exp "! r")))
  (is (equal '(:app (:var "f") (:app (:var "!") (:var "r"))) (parse-exp "f (!r)"))))

(test parse-op-makes-identifiers-nonfix
  (is (equal '(:app (:var "foldl") (:var "+")) (parse-exp "foldl (op +)")))
  (is (equal '(:app (:var "+") (:tuple 5 6)) (parse-exp "op + (5, 6)")))
  (is (equal '(:app (:ctor "::") (:tuple 1 (:list))) (parse-exp "op :: (1, [])"))))

(test parse-user-fixity-declarations
  (let ((decs (parse-decs "infix 7 **; val z = 2 + 3 ** 4")))
    (is (equal '(:infix "infix" 7 ("**")) (first decs)))
    (is (equal '(:val (:pat-var "z")
                 (:app (:var "+") (:tuple 2 (:app (:var "**") (:tuple 3 4)))))
               (second decs))))
  ;; infixr associates to the right.
  (is (equal '(:val (:pat-var "v")
               (:app (:var "+++") (:tuple 1 (:app (:var "+++") (:tuple 2 3)))))
             (second (parse-decs "infixr 5 +++ val v = 1 +++ 2 +++ 3"))))
  ;; Alphanumeric infix identifiers, and the default precedence 0.
  (is (equal '(:val (:pat-var "v")
               (:app (:var "plus2") (:tuple 1 (:app (:var "+") (:tuple 2 3)))))
             (second (parse-decs "infix plus2 val v = 1 plus2 2 + 3"))))
  ;; nonfix
  (is (equal '(:val (:pat-var "q") (:app (:var "+") (:tuple 5 6)))
             (second (parse-decs "nonfix + val q = + (5, 6)"))))
  (is (equal '(:val (:pat-var "plus") 3) (parse-dec "val plus = 3")))
  (is (equal '(:val (:pat-var "sub") 3) (parse-dec "val sub = 3"))))

(test parse-fixity-is-scoped
  ;; A fixity declared in `let' does not leak out of it...
  (let ((decs (parse-decs "val a = let infix 7 xx in 2 xx 3 end val b = xx (2, 3)")))
    (is (equal '(:let ((:infix "infix" 7 ("xx"))) ((:app (:var "xx") (:tuple 2 3))))
               (third (first decs))))
    (is (equal '(:app (:var "xx") (:tuple 2 3)) (third (second decs)))))
  ;; ...nor out of a structure, but `local' exports its body's.
  (is (equal '(:app (:var "yy") (:tuple 1 2))
             (third (second (parse-decs "structure S = struct infix yy end val c = yy (1, 2)")))))
  (is (equal '(:app (:var "zz") (:tuple 1 2))
             (third (second (parse-decs "local in infix zz end val c = 1 zz 2")))))
  ;; Fixity persists from one program to the next in a package.
  (multiple-value-bind (program scopes)
      (parse-sml-program "infix 6 &&" :scopes (initial-scopes))
    (declare (ignore program))
    (is (equal '(:program (:expr (:app (:var "&&") (:tuple 1 2))))
               (parse-sml-program "1 && 2" :scopes scopes)))))

(test parse-mixed-associativity-is-an-error
  (let ((condition (syntax-error-of
                    (lambda ()
                      (parse-prog "infix 5 -- infixr 5 ++ val x = 1 -- 2 ++ 3")))))
    (is (not (null condition)))
    (is (search "associativity" (princ-to-string condition))))
  (signals sml-syntax-error (parse-exp "1 +"))
  (signals sml-syntax-error (parse-exp "+ 1")))

(test parse-boolean-operators-and-raise
  ;; andalso binds tighter than orelse.
  (is (equal '(:orelse (:andalso (:ctor "false") (:ctor "true")) (:ctor "true"))
             (parse-exp "false andalso true orelse true")))
  (is (equal '(:orelse (:var "a") (:andalso (:var "b") (:var "c")))
             (parse-exp "a orelse b andalso c")))
  (is (equal '(:orelse (:var "eq") (:raise (:ctor "Match")))
             (parse-exp "eq orelse raise Match")))
  (is (equal '(:handle (:andalso (:var "a") (:var "b")) (((:pat-ctor "Div" :status "e") (:ctor "false"))))
             (parse-exp "a andalso b handle Div => false")))
  (is (equal '(:raise (:handle (:var "e") (((:pat-var "x") 1))))
             (parse-exp "raise e handle x => 1"))))

(test parse-control-and-sequence-expressions
  (is (equal '(:seq (:app (:var "print") 1) 2) (parse-exp "(print 1; 2)")))
  (is (equal '(:while (:app (:var "not") (:var "done")) (:unit)) (parse-exp "while not done do ()")))
  (is (equal '(:if (:var "c") 1 (:handle 2 ((:wild 3)))) (parse-exp "if c then 1 else 2 handle _ => 3")))
  (is (equal '(:case (:var "opt")
               ((:pat-app (:pat-ctor "SOME" :status "c1") (:pat-var "v"))
                (:app (:app (:var "add") (:var "v")) 1))
               ((:pat-ctor "NONE" :status "c") 0))
             (parse-exp "case opt of SOME v => add v 1 | NONE => 0")))
  (is (equal '(:fn (((:pat-var "x") (:app (:var "+") (:tuple (:var "x") 1)))))
             (parse-exp "fn x => x + 1")))
  (is (equal '(:let ((:val (:pat-var "x") 1)) ((:app (:var "f") (:var "x")) (:var "x")))
             (parse-exp "let val x = 1 in f x; x end"))))

(test parse-tuples-records-lists-and-selectors
  (is (equal '(:tuple 1 2) (parse-exp "(1, 2)")))
  (is (equal '(:unit) (parse-exp "()")))
  (is (equal '(:unit) (parse-exp "{}")))
  (is (equal '(:list 1 2 3) (parse-exp "[1, 2, 3]")))
  (is (equal '(:list) (parse-exp "[]")))
  (is (equal '(:record ("x" 1) ("y" 2)) (parse-exp "{x = 1, y = 2}")))
  (is (equal '(:record ("1" 1) ("2" 2)) (parse-exp "{1 = 1, 2 = 2}")))
  (is (equal '(:app (:selector "x") (:var "point")) (parse-exp "#x point")))
  (is (equal '(:app (:selector "2") (:var "pair")) (parse-exp "#2 pair"))))

;;; Types everywhere

(test parse-types
  (is (equal '(:ty-arrow (:ty-con "int" nil) (:ty-arrow (:ty-var "'a") (:ty-con "bool" nil)))
             (parse-ty "int -> 'a -> bool")))
  (is (equal '(:ty-arrow (:ty-tuple (:ty-con "int" nil) (:ty-con "string" nil)) (:ty-con "unit" nil))
             (parse-ty "int * string -> unit")))
  (is (equal '(:ty-con "list" ((:ty-con "list" ((:ty-con "int" nil)))))
             (parse-ty "int list list")))
  (is (equal '(:ty-con "Map.map" ((:ty-con "int" nil) (:ty-var "'v")))
             (parse-ty "(int, 'v) Map.map")))
  (is (equal '(:ty-record ("a" (:ty-con "int" nil)) ("b" (:ty-con "list" ((:ty-con "t" nil)))))
             (parse-ty "{a : int, b : t list}")))
  (is (equal '(:ty-tuple (:ty-arrow (:ty-con "a" nil) (:ty-con "b" nil)) (:ty-con "c" nil))
             (parse-ty "(a -> b) * c")))
  (is (equal "(int -> 'a) * string list -> {a : int}"
             (cl-sml::sml-type-ast->string (parse-ty "(int -> 'a) * string list -> {a : int}")))))

(test parse-type-constraints-everywhere
  (is (equal '(:fun "g" ((((:pat-as (:pat-typed (:pat-var "y") (:ty-con "int" nil)) 3)) (:var "y"))))
             (parse-dec "fun g (y : int as 3) = y")))
  (is (equal '(:case (:typed 3 (:ty-con "int" nil)) (3 1) (:wild 2))
             (parse-exp "case 3 : int of 3 => 1 | _ => 2")))
  (is (equal '(:let ((:val (:pat-var "x") (:typed (:list) (:ty-con "list" ((:ty-con "int" nil))))))
                    ((:var "x")))
             (parse-exp "let val x = [] : int list in x end")))
  (is (equal '(:let ((:exception "L" :arg-type (:ty-con "string" nil))) (1))
             (parse-exp "let exception L of string in 1 end")))
  (is (equal '(:record ("a" 1) ("b" (:typed "x" (:ty-con "string" nil))))
             (parse-exp "{a = 1, b = \"x\" : string}")))
  (is (equal '(:fn (((:pat-typed (:pat-record ("l" (:pat-var "l")) :record-rest)
                                 (:ty-record ("l" (:ty-con "string" nil)) ("m" (:ty-con "int" nil))))
                     (:var "l"))))
             (parse-exp "fn {l, ...} : {l : string, m : int} => l")))
  (is (equal '(:val (:pat-var "use") (:var "use")
               :type (:ty-arrow (:ty-con "string" nil) (:ty-con "unit" nil)))
             (parse-dec "val use : string -> unit = use")))
  (is (equal '(:fun "f" ((((:pat-var "x")) (:typed (:var "x") (:ty-con "int" nil)))))
             (parse-dec "fun f x : int = x")))
  ;; A type ends where the next declaration starts, even on one line.
  (is (equal '((:type "t" (:ty-con "int" nil)) (:fun "f" ((((:pat-var "x")) (:var "x")))))
             (third (parse-dec "structure S = struct type t = int fun f x = x end")))))

;;; Patterns

(test parse-patterns
  (is (equal '(:pat-cons (:pat-var "x") (:pat-var "xs")) (parse-pat "x :: xs")))
  (is (equal '(:pat-cons (:pat-typed (:pat-var "x") (:ty-con "int" nil)) (:pat-nil))
             (parse-pat "[x : int]")))
  (is (equal '(:pat-app (:pat-ctor "SOME" :status "c1") (:pat-nil)) (parse-pat "SOME []")))
  (is (equal '(:pat-cons (:pat-tuple (:pat-var "x") (:pat-var "y")) (:pat-var "rest"))
             (parse-pat "(x, y) :: rest")))
  (is (equal '(:pat-unit) (parse-pat "()")))
  (is (equal '(:pat-app (:pat-ctor "SOME" :status "c1")
               (:pat-app (:pat-ctor "SOME" :status "c1") (:pat-var "x")))
             (parse-pat "SOME (SOME x)")))
  (is (equal '(:pat-app (:pat-ctor "IO.Io") :wild) (parse-pat "IO.Io _")))
  (is (equal '(:pat-cons (:pat-var "x") (:pat-var "xs")) (parse-pat "op :: (x, xs)")))
  (is (equal '(:pat-as (:pat-var "B") (:pat-tuple (:pat-var "T") (:pat-var "F")))
             (parse-pat "B as (T, F)")))
  (is (equal '(:pat-as (:pat-var "x") (:pat-cons (:pat-var "y") (:pat-var "ys")))
             (parse-pat "x as y :: ys")))
  (is (equal '((:word 255) "s" #\c 3) (list (parse-pat "0wxff") (parse-pat "\"s\"")
                                            (parse-pat "#\"c\"") (parse-pat "3"))))
  (is (equal -1 (parse-pat "~1"))))

(test parse-record-patterns
  (is (equal '(:pat-record ("x" (:pat-var "x")) ("y" (:pat-var "value")))
             (parse-pat "{x, y = value}")))
  (is (equal '(:pat-record ("key" (:pat-var "key")) ("left" (:pat-var "left")) :record-rest)
             (parse-pat "{key, left, ...}")))
  (is (equal '(:pat-record ("a" (:pat-typed (:pat-var "a") (:ty-con "int" nil)))
                           ("b" (:pat-as (:pat-var "b") (:pat-tuple (:pat-var "p") (:pat-var "q")))))
             (parse-pat "{a : int, b as (p, q)}"))))

(test parse-infix-constructor-patterns
  (is (equal '(:pat-app (:pat-ctor ":::" :status "c1")
               (:pat-tuple (:pat-var "x") (:pat-app (:pat-ctor ":::" :status "c1")
                                                    (:pat-tuple (:pat-var "y") (:pat-var "z")))))
             (first (first (first (third (third (parse-decs "datatype t = N | ::: of int * t
                                                       infixr 5 :::
                                                       fun f (x ::: y ::: z) = x")))))))))

;;; Identifier status (Definition section 4.1)

(test parse-identifier-status
  ;; Lowercase constructors are matched, not bound.
  (is (equal '(((:pat-ctor "red" :status "c")) 1)
             (first (third (second (parse-decs "datatype color = red | green
                                                fun name red = 1 | name green = 2"))))))
  ;; Capitalized names are variables unless they are constructors.
  (is (equal '((:pat-var "X")) (first (first (third (parse-dec "fun f X = X"))))))
  (is (equal '(:pat-app (:pat-ctor "oops" :status "e1") (:pat-var "n"))
             (first (first (third (third (second (parse-decs "exception oops of int
                                                               val v = 1 handle oops n => n"))))))))
  (is (equal '(:pat-app (:pat-ctor "Bind" :status "c1") (:pat-var "n"))
             (first (first (first (third (second (parse-decs "datatype d = Bind of int | Other
                                                        fun f (Bind n) = n | f Other = 0"))))))))
  ;; `open' brings a structure's constructors into scope.
  (is (equal '(:pat-ctor "leaf" :status "c")
             (first (first (first (third (third (parse-decs "structure T = struct datatype t = leaf | node end
                                                              open T
                                                              fun f leaf = 0 | f node = 1"))))))))
  (is (equal '(:pat-ctor "T.leaf" :status "c")
             (first (first (first (third (second (parse-decs "structure T = struct datatype t = leaf end
                                                               fun f T.leaf = 0"))))))))
  ;; abstype hides its constructors after `end'.
  (is (equal '((:pat-var "A"))
             (first (first (third (second (parse-decs "abstype t = A with val a = A end
                                                        fun f A = 1"))))))))

;;; fun clause heads (Definition section 2.6, Appendix A)

(test parse-fun-clause-heads
  (is (equal '(:fun "add" ((((:pat-var "a") (:pat-var "b"))
                            (:app (:var "+") (:tuple (:var "a") (:var "b"))))))
             (parse-dec "fun add a b = a + b")))
  ;; An infix head binds a function of a pair.
  (is (equal '(:fun "++" ((((:pat-tuple (:pat-var "x") (:pat-var "y"))) (:var "x"))))
             (second (parse-decs "infix ++ fun x ++ y = x"))))
  (is (equal '(:fun ">-" ((((:pat-tuple (:pat-var "m1") (:pat-var "f")) (:pat-var "env0"))
                           (:var "m1"))))
             (second (parse-decs "infix >- fun (m1 >- f) env0 = m1"))))
  (is (equal '(:fun "@@" ((((:pat-tuple (:pat-nil) (:pat-var "ys"))) (:var "ys"))))
             (second (parse-decs "infix @@ fun [] @@ ys = ys"))))
  (is (equal '(:fun "D" ((((:pat-tuple (:pat-cons (:pat-var "A") (:pat-var "C")) (:pat-var "E")))
                          (:var "E"))))
             (second (parse-decs "infix D fun (A :: C) D E = E"))))
  ;; `~1' is a constant, not an infix function name.
  (is (equal '(:fun "i" (((-1) 0) (((:pat-var "n")) (:var "n"))))
             (parse-dec "fun i ~1 = 0 | i n = n")))
  (is (equal '(:fun "+" ((((:pat-tuple (:pat-var "a") (:pat-var "b"))) (:var "a"))))
             (parse-dec "fun op + (a, b) = a")))
  (is (equal '(:fun "packVar"
               ((((:pat-ctor "NONE" :status "c") (:pat-var "var")) (:var "var"))
                (((:pat-app (:pat-ctor "SOME" :status "c1") (:pat-var "expr")) (:pat-var "var"))
                 (:var "expr"))))
             (parse-dec "fun packVar NONE var = var | packVar (SOME expr) var = expr")))
  (is (equal '(:funs (:fun "even" ((((0)) (:ctor "true"))))
                     (:fun "odd" ((((0)) (:ctor "false")))))
             (parse-dec "fun even 0 = true and odd 0 = false")))
  (signals sml-syntax-error (parse-dec "fun f 0 = 1 | g 1 = 2"))
  (signals sml-syntax-error (parse-dec "fun f 0 = 1 | f 1 2 = 2")))

;;; Core declarations

(test parse-value-declarations
  (is (equal '(:val (:pat-tuple (:pat-var "x") (:pat-var "y")) (:tuple 1 2))
             (parse-dec "val (x, y) = (1, 2)")))
  (is (equal '(:vals (:val (:pat-var "s") (:app (:var "ref") ""))
                     (:val (:pat-var "index") (:app (:var "ref") 0)))
             (parse-dec "val s = ref \"\" and index = ref 0")))
  (is (equal '(:val-recs (:val-rec "f" (:fn (((:pat-var "n") (:app (:var "g") (:var "n"))))))
                         (:val-rec "g" (:fn (((:pat-var "n") (:var "n"))))))
             (parse-dec "val rec f = fn n => g n and g = fn n => n")))
  (is (equal '(:val (:pat-var "id") (:fn (((:pat-var "x") (:var "x")))) :tyvars ("'a"))
             (parse-dec "val 'a id = fn x => x")))
  (is (equal '(:fun "pair" ((((:pat-typed (:pat-var "x") (:ty-var "'a"))
                              (:pat-typed (:pat-var "y") (:ty-var "'b")))
                             (:tuple (:var "x") (:var "y"))))
               :tyvars ("'a" "'b"))
             (parse-dec "fun ('a, 'b) pair (x : 'a) (y : 'b) = (x, y)")))
  (is (equal '(:program (:expr 1) (:expr (:app (:var "+") (:tuple 2 3))))
             (parse-prog "1; 2 + 3;"))))

(test parse-type-and-datatype-declarations
  (is (equal '(:type "unit" (:ty-record)) (parse-dec "type unit = {}")))
  (is (equal '(:type "pair" (:ty-tuple (:ty-var "'a") (:ty-var "'a")) :tyvars ("'a"))
             (parse-dec "type 'a pair = 'a * 'a")))
  (is (equal '(:datatype "option"
               ((:ctor-def "NONE" :has-args nil :arg-type nil)
                (:ctor-def "SOME" :has-args t :arg-type (:ty-var "'a")))
               :datbinds (("'a") "option" ((:ctor-def "NONE" :has-args nil :arg-type nil)
                                           (:ctor-def "SOME" :has-args t :arg-type (:ty-var "'a"))))
               :withtype nil)
             (parse-dec "datatype 'a option = NONE | SOME of 'a")))
  (is (equal '(:datatype-replication "list" "list") (parse-dec "datatype list = datatype list")))
  ;; withtype bindings are kept, and constructor types span lines.
  (let ((dec (parse-dec "datatype a = A of b
                                | B of int
                         and c = C of {x : int,
                                       y : string} list
                         withtype b = a list")))
    (is (equal '("A" "B" "C") (mapcar #'second (third dec))))
    (is (equal '((nil "b" (:ty-con "list" ((:ty-con "a" nil)))))
               (getf (cdddr dec) :withtype)))))

(test parse-exception-local-open-and-abstype
  (is (equal '(:exception "E" :arg-type nil) (parse-dec "exception E")))
  (is (equal '((:exception "E" :arg-type nil) (:exception "F" :arg-type (:ty-con "int" nil) :and t))
             (parse-decs "exception E and F of int")))
  (is (equal '(:exception-alias "Bind" "Bind") (parse-dec "exception Bind = Bind")))
  (is (equal '(:local ((:fun "helper" ((((:pat-var "x")) (:app (:var "+") (:tuple (:var "x") 1))))))
                      ((:val (:pat-var "y") (:app (:var "helper") 2))))
             (parse-dec "local fun helper x = x + 1; in val y = helper 2; end;")))
  (is (equal '((:open ("A" "B.C")) (:val (:pat-var "d") (:var "c")))
             (rest (parse-decs "structure A = struct end open A B.C val d = c"))))
  (is (equal '(:local ((:datatype "t" ((:ctor-def "T" :has-args t :arg-type (:ty-con "int" nil)))
                        :datbinds ((nil "t" ((:ctor-def "T" :has-args t :arg-type (:ty-con "int" nil)))))
                        :withtype nil))
                      ((:fun "mk" ((((:pat-var "n")) (:app (:ctor "T") (:var "n"))))))
                      :abstype t)
             (parse-dec "abstype t = T of int with fun mk n = T n end"))))

;;; Modules (Definition chapter 3, Appendix B)

(test parse-structures
  (is (equal '(:structure "S" ((:val (:pat-var "x") 1)) :strexp (:struct ((:val (:pat-var "x") 1))))
             (parse-dec "structure S = struct val x = 1 end")))
  (let ((dec (parse-dec "structure S :> SIG where type t = int and type u = bool = struct end")))
    (is (eq :structure (first dec)))
    (is (equal '(:sig-where (:sig-where (:sig-id "SIG") nil "t" (:ty-con "int" nil))
                 nil "u" (:ty-con "bool" nil))
               (getf (cdddr dec) :sigexp)))
    (is (eq t (getf (cdddr dec) :opaque))))
  ;; A signature containing `=' does not end the binding early.
  (let ((dec (parse-dec "structure X : sig datatype p = A | B end = struct datatype p = A | B end")))
    (is (eq :structure (first dec)))
    (is (equal '(:sig ((:spec-datatype ((nil "p" ((:ctor-def "A" :has-args nil :arg-type nil)
                                                  (:ctor-def "B" :has-args nil :arg-type nil)))))))
               (getf (cdddr dec) :sigexp))))
  (is (equal '(:structure-alias "A" "B.C" :strexp (:str-id "B.C")) (parse-dec "structure A = B.C")))
  (is (equal '(:structure "S" ((:val (:pat-var "x") 1))
               :sigexp (:sig-id "T") :opaque nil
               :strexp (:str-constraint (:struct ((:val (:pat-var "x") 1))) (:sig-id "T") :opaque nil))
             (parse-dec "structure S = struct val x = 1 end : T")))
  (is (equal '(:structure "S" ((:local ((:val (:pat-var "a") 1)) ((:open ("A")))))
               :strexp (:str-let ((:val (:pat-var "a") 1)) (:str-id "A")))
             (parse-dec "structure S = let val a = 1 in A end")))
  (is (equal '((:structure-alias "A" "X" :strexp (:str-id "X"))
               (:structure-alias "B" "Y" :strexp (:str-id "Y") :and t))
             (parse-decs "structure A = X and B = Y"))))

(test parse-functor-applications
  (is (equal '(:structure-app "A" "F" (:str-id "X") :strexp (:str-app "F" (:str-id "X")))
             (parse-dec "structure A = F (X)")))
  (is (equal '(:str-app "F" (:str-app "G" (:str-id "X")))
             (getf (cddddr (parse-dec "structure A = F (G (X))")) :strexp)))
  (is (equal '(:str-app "F" (:struct ((:val (:pat-var "k") 1))))
             (getf (cddddr (parse-dec "structure A = F (struct val k = 1 end)")) :strexp)))
  ;; funid (strdec) abbreviates funid (struct strdec end).
  (is (equal '(:str-app "F" (:struct ((:structure "S" ((:val (:pat-var "v") 14))
                                       :strexp (:struct ((:val (:pat-var "v") 14)))))))
             (getf (cddddr (parse-dec "structure A = F (structure S = struct val v = 14 end)"))
                   :strexp)))
  (is (equal '(:str-app "Unit" (:struct nil))
             (getf (cddddr (parse-dec "structure U = Unit ()")) :strexp))))

(test parse-signatures-and-functors
  (is (equal '(:signature "S"
               (:sig ((:spec-type nil "t") (:spec-eqtype ("'a") "u")
                      (:spec-type nil "v" (:ty-con "int" nil))
                      (:spec-val "f" (:ty-arrow (:ty-con "t" nil) (:ty-con "t" nil)))
                      (:spec-val "!" (:ty-arrow (:ty-con "ref" ((:ty-var "'a"))) (:ty-var "'a")))
                      (:spec-exception "E" (:ty-con "int" nil))
                      (:spec-structure "Sub" (:sig-id "SUB"))
                      (:spec-include (:sig-id "A")) (:spec-include (:sig-id "B"))
                      (:spec-sharing-type ("t" "Sub.t"))
                      (:spec-sharing ("Sub" "Other")))))
             (parse-dec "signature S = sig
                           type t eqtype 'a u type v = int
                           val f : t -> t val ! : 'a ref -> 'a
                           exception E of int
                           structure Sub : SUB
                           include A B
                           sharing type t = Sub.t
                           sharing Sub = Other
                         end")))
  (is (equal '((:signature "A" (:sig nil)) (:signature "B" (:sig-id "A") :and t))
             (parse-decs "signature A = sig end and B = A")))
  (let ((dec (parse-dec "functor F (X : SIG) :> RES = struct val y = X.x end")))
    (is (equal '(:functor "F" ((:val (:pat-var "y") (:var "X.x")))) (subseq dec 0 3)))
    (is (equal "X" (getf (cdddr dec) :param)))
    (is (equal '(:sig-id "SIG") (getf (cdddr dec) :param-sigexp)))
    (is (equal '(:sig-id "RES") (getf (cdddr dec) :result-sigexp)))
    (is (eq t (getf (cdddr dec) :opaque))))
  ;; funid (spec): the specification's names are in scope in the body.
  (let ((dec (parse-dec "functor Add (val n : int type t) = struct val m = n end")))
    (is (null (getf (cdddr dec) :param)))
    (is (equal '((:spec-val "n" (:ty-con "int" nil)) (:spec-type nil "t"))
               (getf (cdddr dec) :param-specs))))
  ;; A body that is not `struct ... end'.
  (is (equal '((:local ((:val (:pat-var "a") 1)) ((:val (:pat-var "b") 2))))
             (third (parse-dec "functor F () = let val a = 1 in struct val b = 2 end end")))))

;;; Errors

(test parse-errors-report-position-and-expectation
  (let ((condition (syntax-error-of (lambda ()
                                      (parse-sml-program (format nil "val x = 1~%val y = (2, 3"
                                                                 )
                                                         :scopes (initial-scopes)
                                                         :source-name "test.sml")))))
    (is (not (null condition)))
    (is (= 2 (cl-sml::sml-syntax-error-line condition)))
    (is (search "test.sml:2:" (princ-to-string condition)))
    (is (search "expected `)' but found end of input" (princ-to-string condition))))
  (let ((condition (syntax-error-of (lambda () (parse-prog "val = 1")))))
    (is (search "1:5: syntax error: expected a pattern but found `='"
                (princ-to-string condition)))))

;;; Static facts are keyed by the offsets of identifiers and operators.

(test parse-records-identifier-and-operator-positions
  (let* ((cl-sml::*sml-ast-positions* (cl-sml::make-sml-ast-position-table))
         (ast (parse-exp "f (a + b)")))
    (is (= 0 (cl-sml::sml-ast-position (second ast))))
    (is (= 5 (cl-sml::sml-ast-position (second (third ast)))))
    (is (= 3 (cl-sml::sml-ast-position (second (third (third ast))))))))

(test compiler-constructor-consistency
  "Verify that constructors in expressions and patterns use the same package/symbol"
  (let* ((ctor-expr (cl-sml::compile-expr '(:ctor "SML_NONE"))))
    (setf (symbol-value ctor-expr) ctor-expr)
    (cl-sml::register-sml-constructor ctor-expr)
    (let ((ctor-pat (cl-sml::compile-pat '(:pat-ctor "SML_NONE"))))
      (fiveam:is (eq :matched
                     (eval `(trivia:match ',ctor-expr
                              (,ctor-pat :matched)
                              (_ :miss)))))
      (fiveam:is (symbolp ctor-expr)))))
