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

(test parse-identifiers
  (is (equal '(:var "x") (parse 'cl-sml::sml-var-or-ctor "x")))
  (is (equal '(:ctor "SOME") (parse 'cl-sml::sml-var-or-ctor "SOME"))))

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


(test parse-case-statement
  (is (equal '(:case (:var "opt")
               ((:pat-app (:pat-ctor "SOME") (:pat-var "v"))
                (:app (:app (:var "add") (:var "v")) 1))
               ((:pat-ctor "NONE") 0))
             (parse 'cl-sml::sml-expr "case opt of SOME v => add v 1 | NONE => 0"))))

(test parse-declarations
      (is (equal '(:val "x" 10)
                 (parse 'cl-sml::sml-val "val x = 10;")))

      (is (equal '(:fun "add" ("a" "b")
                   (:app (:app (:var "+") (:var "a")) (:var "b")))
                 (parse 'cl-sml::sml-fun "fun add a b = a + b;"))))

(test parse-let-expression
  (let ((prog "let
                 val x = 1;
                 fun add_x y = x + y;
               in
                 add_x 10;
                 add_x 20
               end"))
    (is (equal '(:let ((:val "x" 1)
                       (:fun "add_x" ("y") (:app (:app (:var "+") (:var "x")) (:var "y"))))
                      ((:app (:var "add_x") 10)
                       (:app (:var "add_x") 20)))
               (parse 'cl-sml::sml-expr prog)))))

(test parse-full-program
      (let ((prog "val x = 10; fun add a b = a + b;"))
        (is (equal '(:program
                     (:val "x" 10)
                     (:fun "add" ("a" "b") (:app (:app (:var "+") (:var "a")) (:var "b"))))
                   (parse 'cl-sml::sml-program prog)))))

(test parse-lists
  (is (equal '(:list 1 2 3)
             (parse 'cl-sml::sml-expr "[1, 2, 3]")))
  (is (equal '(:list)
             (parse 'cl-sml::sml-expr "[]"))))

(test parse-list-patterns
  (is (equal '(:pat-cons "x" "xs")
             (parse 'cl-sml::sml-pat "x :: xs")))
  (is (equal '(:pat-nil)
             (parse 'cl-sml::sml-pat "[]"))))

(test parse-datatype
  (is (equal '(:datatype "color" ((:ctor-def "Red" :has-args nil) (:ctor-def "Blue" :has-args nil)))
             (parse 'cl-sml::sml-datatype "datatype color = Red | Blue ;"))))

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
