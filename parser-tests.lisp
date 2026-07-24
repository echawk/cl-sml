(defpackage #:cl-sml-tests
  (:use #:cl #:fiveam #:cl-sml)
  (:import-from #:esrap #:parse))

(in-package #:cl-sml-tests)

;; Create a test suite
(def-suite cl-sml-parser-suite
  :description "Tests for the cl-sml esrap parser")

(in-suite cl-sml-parser-suite)

(test parse-integers
  (is (equal 42 (parse 'cl-sml::sml-int "42")))
  (is (equal -10 (parse 'cl-sml::sml-int "~10"))))

(test parse-real-literals
  (is (= 3.8d0 (parse 'cl-sml::sml-real "3.8")))
  (is (= -0.5d0 (parse 'cl-sml::sml-real "~0.5"))))

(test parse-string-and-char-literals
  (is (equal "hello" (parse 'cl-sml::sml-string "\"hello\"")))
  (is (char= #\a (parse 'cl-sml::sml-char "#\"a\"")))
  (is (equal "line
tab	"
             (parse 'cl-sml::sml-string "\"line\\ntab\\t\"")))
  (is (equal "A" (parse 'cl-sml::sml-string "\"\\065\"")))
  (is (equal "hello" (parse 'cl-sml::sml-string (format nil "\"\\~%\\hello\""))))
  (is (equal "" (parse 'cl-sml::sml-string (format nil "\"\\~%\\\"")))))

(test parse-identifiers
  (is (equal '(:var "x") (parse 'cl-sml::sml-var-or-ctor "x")))
  (is (equal '(:ctor "SOME") (parse 'cl-sml::sml-var-or-ctor "SOME"))))

(test parse-comments
  (is (equal '(:program (:val (:pat-var "x") 10))
             (parse 'cl-sml::sml-program "(* outer (* inner *) *) val x = 10;"))))

(test parse-applications
  ;; FIX: Tests updated to expect proper strict currying
  (is (equal '(:app (:var "f") (:var "x"))
             (parse 'cl-sml::sml-app "f x")))
  (is (equal '(:app (:app (:var "f") (:var "x")) (:var "y"))
             (parse 'cl-sml::sml-app "f x y"))))

(test parse-infix-math
  (is (equal '(:app (:app (:var "+") (:var "a")) (:var "b"))
             (parse 'cl-sml::sml-expr "a + b")))

  (is (equal '(:app (:app (:var "*")
                      (:app (:app (:var "+") (:var "a")) (:var "b")))
               (:var "c"))
             (parse 'cl-sml::sml-expr "(a + b) * c")))

  (is (equal '(:app (:app (:var "+") (:var "a"))
               (:app (:app (:var "*") (:var "b")) (:var "c")))
             (parse 'cl-sml::sml-expr "a + b * c")))
  (is (equal '(:app (:app (:var "mod") (:var "n")) 26)
             (parse 'cl-sml::sml-expr "n mod 26")))
  (is (equal '(:app (:app (:var "div") (:var "n")) 26)
             (parse 'cl-sml::sml-expr "n div 26"))))

(test parse-append-and-assignment
  (is (equal '(:app (:app (:var "@") (:var "xs")) (:var "ys"))
             (parse 'cl-sml::sml-expr "xs @ ys")))
  (is (equal '(:app (:app (:var "@@") (:var "x")) (:ctor "A"))
             (parse 'cl-sml::sml-expr "x@@A")))
  (is (equal '(:app (:app (:var ":=") (:var "r")) 10)
             (parse 'cl-sml::sml-expr "r := 10")))
  (is (equal '(:deref (:var "r"))
             (parse 'cl-sml::sml-expr "! r"))))

(test parse-symbolic-infix-does-not-split-relational-prefix
  (is (equal '(:app (:app (:var ">>=") (:var "x")) (:var "f"))
             (parse 'cl-sml::sml-expr "x >>= f"))))

(test parse-bare-tilde-as-value
  (is (equal '(:tuple (:var "~") (:var "Word.~") (:var "Word8.~") (:var "~"))
             (parse 'cl-sml::sml-expr "(~, Word.~, Word8.~, ~)")))
  (is (equal '(:app (:var "~") 5)
             (parse 'cl-sml::sml-expr "~5")))
  (is (equal '(:app (:var "~") (:var "x"))
             (parse 'cl-sml::sml-expr "~ x"))))

(test parse-sequencing
  (is (equal '(:seq (:app (:var "print") 1) 2)
             (parse 'cl-sml::sml-expr "print 1; 2"))))

(test parse-raise-as-boolean-operand
  (is (equal '(:orelse (:var "eq") (:raise (:ctor "Type")))
             (parse 'cl-sml::sml-expr "eq orelse raise Type")))
  (is (equal '(:andalso (:var "ok") (:raise (:ctor "Fail")))
             (parse 'cl-sml::sml-expr "ok andalso raise Fail"))))

(test parse-while-expression
  (is (equal '(:while (:app (:var "not") (:var "done")) (:unit))
             (parse 'cl-sml::sml-expr "while not done do ()"))))


(test parse-case-statement
  (is (equal '(:case (:var "opt")
               ((:pat-app (:pat-ctor "SOME") (:pat-var "v"))
                (:app (:app (:var "add") (:var "v")) 1))
               ((:pat-ctor "NONE") 0))
             (parse 'cl-sml::sml-expr "case opt of SOME v => add v 1 | NONE => 0"))))

(test parse-declarations
  (is (equal '(:val (:pat-var "x") 10)
             (parse 'cl-sml::sml-val "val x = 10;")))
  (is (equal '(:val-rec "fact"
               (:fn ((0 1)
                     ((:pat-var "n")
                      (:app (:app (:var "*") (:var "n"))
                       (:app (:var "fact")
                        (:app (:app (:var "-") (:var "n")) 1)))))))
             (parse 'cl-sml::sml-val-rec
                    "val rec fact = fn 0 => 1 | n => n * fact (n - 1);")))
  (is (equal '(:val (:pat-tuple (:pat-var "x") (:pat-var "y"))
                    (:tuple 1 2))
             (parse 'cl-sml::sml-val "val (x, y) = (1, 2);")))
  (is (equal '(:vals (:val (:pat-var "s") (:app (:var "ref") ""))
                     (:val (:pat-var "index") (:app (:var "ref") 0)))
             (parse 'cl-sml::sml-val "val s = ref \"\" and index = ref 0")))
  (is (equal '(:fun "add" ((((:pat-var "a") (:pat-var "b"))
                            (:app (:app (:var "+") (:var "a")) (:var "b")))))
             (parse 'cl-sml::sml-fun "fun add a b = a + b;"))))

(test parse-word-infix-fun-declarations
  (is (equal '(:fun "plus" ((((:pat-ctor "E") (:pat-ctor "E'"))
                             (:tuple (:ctor "E") (:ctor "E'")))))
             (parse 'cl-sml::sml-fun "fun E plus E' = (E, E');")))
  (is (equal '(:fun "TEplus" ((((:pat-ctor "TE'") (:pat-app (:pat-ctor "Env") (:pat-tuple (:pat-ctor "SE") (:pat-ctor "TE") (:pat-ctor "VE"))))
                                (:ctor "TE"))))
             (parse 'cl-sml::sml-fun "fun TE' TEplus (Env(SE, TE, VE)) = TE;"))))

(test parse-nullary-constructor-function-parameter
  (is (equal '(:fun "packVar"
               ((((:pat-ctor "NONE") (:pat-var "var")) (:var "var"))
                (((:pat-app (:pat-ctor "SOME") (:pat-var "expr"))
                  (:pat-var "var"))
                 (:var "expr"))))
             (parse 'cl-sml::sml-fun
                    "fun packVar NONE var = var | packVar (SOME expr) var = expr;"))))

(test parse-local-declaration
  (is (equal '(:local
               ((:fun "helper" ((((:pat-var "x"))
                                 (:app (:app (:var "+") (:var "x")) 1)))))
               ((:val (:pat-var "y") (:app (:var "helper") 2))))
             (parse 'cl-sml::sml-local
                    "local fun helper x = x + 1; in val y = helper 2; end;"))))

(test parse-multi-clause-fun
  (is (equal '(:fun "length" ((((:pat-nil)) 0)
                              (((:pat-cons (:pat-var "x") (:pat-var "xs")))
                               (:app (:app (:var "+") 1)
                                (:app (:var "length") (:var "xs"))))))
             (parse 'cl-sml::sml-fun
                    "fun length [] = 0 | length x :: xs = 1 + length xs;")))
  (is (equal '(:fun "swap" ((((:pat-tuple (:pat-var "x") (:pat-var "y")))
                             (:tuple (:var "y") (:var "x")))))
             (parse 'cl-sml::sml-fun "fun swap (x, y) = (y, x);"))))

(test parse-let-expression
  (let ((prog "let
                 val x = 1;
                 fun add_x y = x + y;
               in
                 add_x 10;
                 add_x 20
               end"))
    (is (equal '(:let ((:val (:pat-var "x") 1)
                       (:fun "add_x" ((((:pat-var "y"))
                                       (:app (:app (:var "+") (:var "x")) (:var "y"))))))
                      ((:seq (:app (:var "add_x") 10)
                             (:app (:var "add_x") 20))))
               (parse 'cl-sml::sml-expr prog))))
  (is (equal '(:let ((:open "A"))
                    ((:app (:var "f") (:var "x"))))
             (parse 'cl-sml::sml-expr "let open A in f x end"))))

(test parse-full-program
  (let ((prog "val x = 10; val rec fact = fn 0 => 1 | n => n * fact (n - 1); fun add a b = a + b;"))
    (is (equal '(:program
                 (:val (:pat-var "x") 10)
                 (:val-rec "fact"
                  (:fn ((0 1)
                        ((:pat-var "n")
                         (:app (:app (:var "*") (:var "n"))
                          (:app (:var "fact")
                           (:app (:app (:var "-") (:var "n")) 1)))))))
                 (:fun "add" ((((:pat-var "a") (:pat-var "b"))
                               (:app (:app (:var "+") (:var "a")) (:var "b"))))))
               (parse 'cl-sml::sml-program prog)))))

(test parse-lists
  (is (equal '(:list 1 2 3)
             (parse 'cl-sml::sml-expr "[1, 2, 3]")))
  (is (equal '(:list)
             (parse 'cl-sml::sml-expr "[]"))))

(test parse-tuples-and-unit
  (is (equal '(:tuple 1 2)
             (parse 'cl-sml::sml-expr "(1, 2)")))
  (is (equal '(:tuple (:var "x") (:app (:app (:var "+") (:var "y")) 1))
             (parse 'cl-sml::sml-expr "(x, y + 1)")))
  (is (equal '(:unit)
             (parse 'cl-sml::sml-expr "()"))))

(test parse-list-patterns
  (is (equal '(:pat-cons (:pat-var "x") (:pat-var "xs"))
             (parse 'cl-sml::sml-pat "x :: xs")))
  (is (equal '(:pat-cons (:pat-typed (:pat-var "x") "int") (:pat-nil))
             (parse 'cl-sml::sml-pat "[x : int]")))
  (is (equal '(:pat-app (:pat-ctor "SOME") (:pat-nil))
             (parse 'cl-sml::sml-pat "SOME []")))
  (is (equal '(:pat-cons (:pat-tuple (:pat-var "x") (:pat-var "y"))
                         (:pat-var "rest"))
             (parse 'cl-sml::sml-pat "(x, y) :: rest")))
  (is (equal '(:pat-nil)
             (parse 'cl-sml::sml-pat "[]"))))

(test parse-tuple-patterns-and-unit
  (is (equal '(:pat-tuple (:pat-var "x") (:pat-var "y"))
             (parse 'cl-sml::sml-pat "(x, y)")))
  (is (equal '(:pat-unit)
             (parse 'cl-sml::sml-pat "()"))))

(test parse-datatype
  (is (equal '(:datatype "color" ((:ctor-def "Red" :has-args nil :arg-type nil)
                                  (:ctor-def "Blue" :has-args nil :arg-type nil)))
             (parse 'cl-sml::sml-datatype "datatype color = Red | Blue ;")))
  (is (equal '(:datatype "option" ((:ctor-def "NONE" :has-args nil :arg-type nil)
                                   (:ctor-def "SOME" :has-args t :arg-type "'a")))
             (parse 'cl-sml::sml-datatype "datatype 'a option = NONE | SOME of 'a")))
  (is (equal '(:datatype-replication "list" "list")
             (parse 'cl-sml::sml-datatype-replication "datatype list = datatype list"))))

(test parse-symbolic-datatype-constructor
  (is (equal '(:datatype "phrase" ((:ctor-def "@@" :has-args t :arg-type "'a * 'b annotation")))
             (parse 'cl-sml::sml-datatype "datatype ('a, 'b) phrase = @@ of 'a * 'b annotation"))))

(test parse-mutually-recursive-datatype-group
  (is (equal '(:datatype "a" ((:ctor-def "A" :has-args nil :arg-type nil)
                              (:ctor-def "B" :has-args t :arg-type "int")
                              (:ctor-def "C" :has-args t :arg-type "string")))
             (parse 'cl-sml::sml-datatype
                    "datatype a = A
                     and b = B of int
                     and c = C of string
                     withtype d = int"))))

(test parse-hamlet-basis-declaration-forms
  (is (equal '(:program (:infix "infix" 7 "* / div mod")
                       (:infix "infixr" 5 ":: @"))
             (parse 'cl-sml::sml-program
                    (format nil "(* basis infix *)~%infix  7 * / div mod~%infixr 5 :: @;"))))
  (is (equal '(:type "unit" "{}")
             (parse 'cl-sml::sml-type-decl "type unit = {}")))
  (is (equal '(:exception-alias "Bind" "Bind")
             (parse 'cl-sml::sml-exception-alias "exception Bind = Bind")))
  (is (equal '(:val (:pat-var "use") (:var "use") :type "string -> unit")
             (parse 'cl-sml::sml-val "val use : string -> unit = use")))
  (is (equal '(:program (:expr (:seq 1 (:app (:app (:var "+") 2) 3))))
             (parse 'cl-sml::sml-program "1; 2 + 3;"))))

(test parse-records-and-selectors
  (is (equal '(:record ("x" 1) ("y" 2))
             (parse 'cl-sml::sml-expr "{x = 1, y = 2}")))
  (is (equal '(:app (:selector "x") (:var "point"))
             (parse 'cl-sml::sml-expr "#x point"))))

(test parse-record-patterns
  (is (equal '(:pat-record ("x" (:pat-var "x"))
                           ("y" (:pat-var "value")))
             (parse 'cl-sml::sml-pat "{x, y = value}")))
  (is (equal '(:pat-record ("key" (:pat-var "key"))
                           ("left" (:pat-var "left"))
                           :record-rest)
             (parse 'cl-sml::sml-pat "{key, left, ...}"))))

(test parse-uppercase-as-pattern-alias
  (is (equal '(:pat-as (:pat-var "B")
                    (:pat-tuple (:pat-ctor "T") (:pat-ctor "F")))
             (parse 'cl-sml::sml-pat "B as (T, F)"))))

(test parse-symbolic-infix-pattern
  (is (equal '(:pat-app (:pat-ctor "@@")
                    (:pat-tuple (:pat-var "s") (:pat-var "A")))
             (parse 'cl-sml::sml-pat "s@@A")))
  (is (equal '(:pat-app (:pat-ctor "@@")
                    (:pat-tuple (:pat-app (:pat-ctor "SCONAtExp") (:pat-var "scon"))
                                :wild))
             (parse 'cl-sml::sml-pat "SCONAtExp(scon)@@_"))))

(test parse-exception-declarations-and-handling
  (is (equal '(:exception "E" :arg-type nil)
             (parse 'cl-sml::sml-exception "exception E;")))
  (is (equal '(:exception "FailInt" :arg-type "int")
             (parse 'cl-sml::sml-exception "exception FailInt of int;")))
  (is (equal '(:handle (:raise (:ctor "E"))
               (((:pat-ctor "E") 1) (:wild 0)))
             (parse 'cl-sml::sml-expr "(raise E) handle E => 1 | _ => 0")))
  (is (equal '(:raise (:handle (:ctor "E")
                        (((:pat-ctor "E") 1))))
             (parse 'cl-sml::sml-expr "raise E handle E => 1"))))

(test parse-capitalization-logic
  ;; Check that lowercase is a var and uppercase is a ctor
  (is (equal '(:pat-var "x") (parse 'cl-sml::sml-pat-var-or-ctor "x")))
  (is (equal '(:pat-ctor "None") (parse 'cl-sml::sml-pat-var-or-ctor "None"))))

(test parse-constructor-app-pattern
  (is (equal '(:pat-app (:pat-ctor "SOME") (:pat-var "x"))
             (parse 'cl-sml::sml-pat "SOME x")))
  (is (equal '(:pat-app (:pat-ctor "IO.Io") :wild)
             (parse 'cl-sml::sml-pat "IO.Io _")))
  (is (equal '(:pat-app (:pat-ctor "MlyValue.vid'") (:pat-var "vid'1"))
             (parse 'cl-sml::sml-pat "MlyValue.vid' vid'1"))))

(test parse-shallow-module-functors
  (is (equal '(:program
               (:functor "F" ((:val (:pat-var "x") 1)) :param "X")
               (:structure-app "A" "F" "(structure X = Y)"))
             (parse 'cl-sml::sml-program
                    "functor F(X : S) :> T where type u = X.u = struct val x = 1 end
                     structure A = F(structure X = Y)")))
  (is (equal '(:program
               (:functor "Anonymous"
                         ((:fun "apply"
                                ((((:pat-var "x")) (:app (:var "transform")
                                                        (:var "x"))))))
                         :param nil))
             (parse 'cl-sml::sml-program
                    "functor Anonymous(
                       type item;
                       val transform : item -> item
                     ) = struct
                       fun apply x = transform x
                     end"))))

(test parse-standalone-ctor-branch
      (is (equal '((:pat-ctor "NONE") 0)
                 (parse 'cl-sml::sml-first-branch "NONE => 0"))))


(test parse-multiline-case
  (is (equal '(:case (:var "opt")
                 ((:pat-ctor "NONE") 0)
                 ((:pat-app (:pat-ctor "SOME") (:pat-var "v")) 1)) ;; Change :var to :pat-var
             (parse 'cl-sml::sml-expr "case opt of NONE => 0 | SOME v => 1"))))

(test parse-nested-constructor-pattern
      ;; Checks if SOME (SOME x) works
      (is (equal '(:pat-app (:pat-ctor "SOME") (:pat-app (:pat-ctor "SOME") (:pat-var "x")))
                 (parse 'cl-sml::sml-pat "SOME (SOME x)"))))

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

(test anonymous-function-parsing
  "Test parsing of fn x => x + 1"
  (let ((ast (cl-sml::parse-sml-string "fn x => x + 1")))
    (is (eq (car ast) :fn))
    ;; Fix: expect :PAT-VAR for the parameter and a raw integer 1
    (is (equal (second ast)
               '(((:pat-var "x") (:app (:app (:var "+") (:var "x")) 1)))))))

;; Run the suite!
(fiveam:run! 'cl-sml-parser-suite)
