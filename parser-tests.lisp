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
             (parse 'cl-sml::sml-string "\"line\\ntab\\t\""))))

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
             (parse 'cl-sml::sml-expr "a + b * c"))))

(test parse-append-and-assignment
  (is (equal '(:app (:app (:var "@") (:var "xs")) (:var "ys"))
             (parse 'cl-sml::sml-expr "xs @ ys")))
  (is (equal '(:app (:app (:var ":=") (:var "r")) 10)
             (parse 'cl-sml::sml-expr "r := 10")))
  (is (equal '(:deref (:var "r"))
             (parse 'cl-sml::sml-expr "! r"))))

(test parse-sequencing
  (is (equal '(:seq (:app (:var "print") 1) 2)
             (parse 'cl-sml::sml-expr "print 1; 2"))))


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
  (is (equal '(:fun "add" ((((:pat-var "a") (:pat-var "b"))
                            (:app (:app (:var "+") (:var "a")) (:var "b")))))
             (parse 'cl-sml::sml-fun "fun add a b = a + b;"))))

(test parse-multi-clause-fun
  (is (equal '(:fun "length" ((((:pat-nil)) 0)
                              (((:pat-cons "x" "xs"))
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
               (parse 'cl-sml::sml-expr prog)))))

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
  (is (equal '(:pat-cons "x" "xs")
             (parse 'cl-sml::sml-pat "x :: xs")))
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
             (parse 'cl-sml::sml-datatype "datatype color = Red | Blue ;"))))

(test parse-records-and-selectors
  (is (equal '(:record ("x" 1) ("y" 2))
             (parse 'cl-sml::sml-expr "{x = 1, y = 2}")))
  (is (equal '(:app (:selector "x") (:var "point"))
             (parse 'cl-sml::sml-expr "#x point"))))

(test parse-record-patterns
  (is (equal '(:pat-record ("x" (:pat-var "x"))
                           ("y" (:pat-var "value")))
             (parse 'cl-sml::sml-pat "{x, y = value}"))))

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
             (parse 'cl-sml::sml-pat "SOME x"))))

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
  (let* ((ctor-expr (cl-sml::compile-expr '(:ctor "SML_NONE")))
         (ctor-pat  (cl-sml::compile-pat  '(:pat-ctor "SML_NONE"))))
    ;; These must be identical for trivia:match to work!
    (fiveam:is (equal ctor-expr (second ctor-pat)))
    (fiveam:is (symbolp ctor-expr))))

(test anonymous-function-parsing
  "Test parsing of fn x => x + 1"
  (let ((ast (cl-sml::parse-sml-string "fn x => x + 1")))
    (is (eq (car ast) :fn))
    ;; Fix: expect :PAT-VAR for the parameter and a raw integer 1
    (is (equal (second ast)
               '(((:pat-var "x") (:app (:app (:var "+") (:var "x")) 1)))))))

;; Run the suite!
(fiveam:run! 'cl-sml-parser-suite)
