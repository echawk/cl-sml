(defpackage #:cl-sml-hamlet-tests
  (:use #:cl #:fiveam #:cl-sml))

(in-package #:cl-sml-hamlet-tests)

(def-suite cl-sml-hamlet-suite
  :description "cl-sml compilation checked by HaMLet's static semantics")

(in-suite cl-sml-hamlet-suite)

;;; Building a checker loads HaMLet and elaborates its basis library, so the
;;; whole suite shares one session.
(defvar *checker* (make-hamlet-type-checker))

(defun compile-checked (source package)
  (compile-sml-program-string source :package package :type-checker *checker*))

(test hamlet-accepts-well-typed-programs-and-describes-them
  (compile-checked "val hamletChecked = 1 + 2;" "SML.HAMLET-TEST")
  (is (search "val hamletChecked : int"
              (hamlet-type-checker-last-description *checker*))))

(test hamlet-session-carries-earlier-bindings
  (compile-checked "fun hamletDouble n = n * 2;" "SML.HAMLET-TEST")
  (compile-checked "val hamletDoubled = hamletDouble 21;" "SML.HAMLET-TEST")
  (is (search "val hamletDoubled : int"
              (hamlet-type-checker-last-description *checker*))))

(test hamlet-rejects-ill-typed-programs-with-its-diagnostic
  (let ((condition
          (handler-case
              (progn (compile-checked "val hamletBad : int = \"no\";"
                                      "SML.HAMLET-TEST")
                     nil)
            (sml-static-type-error (e) e))))
    (is (not (null condition)))
    (is (search "type mismatch" (princ-to-string condition))))
  (signals sml-static-type-error
    (compile-checked "val hamletUnbound = notDefinedAnywhere;" "SML.HAMLET-TEST")))

(test hamlet-reports-syntax-errors
  ;; Exercises ML-Yacc's error recovery in the compiled parser.
  (let ((condition
          (handler-case
              (progn (compile-checked "val = ;" "SML.HAMLET-TEST") nil)
            (sml-static-type-error (e) e))))
    (is (not (null condition)))
    (is (search "syntax error" (princ-to-string condition)))
    ;; Token names come from a constructor pattern inside a functor.
    (is (not (search "bogus-term" (princ-to-string condition))))))

(test hamlet-rejected-program-does-not-extend-session
  (signals sml-static-type-error
    (compile-checked "val hamletRejected = 1 + true;" "SML.HAMLET-TEST"))
  (signals sml-static-type-error
    (compile-checked "val hamletAfter = hamletRejected;" "SML.HAMLET-TEST")))

(test hamlet-checks-modules
  (compile-checked "signature HAMLET_S = sig type t val x : t end;
                    structure HamletM :> HAMLET_S = struct type t = int val x = 1 end;"
                   "SML.HAMLET-TEST")
  (signals sml-static-type-error
    (compile-checked "val hamletLeak = HamletM.x + 1;" "SML.HAMLET-TEST")))

(test repl-reports-hamlet-rejections
  (let* ((output (make-string-output-stream))
         (error-output (make-string-output-stream))
         (result (with-sml-type-checker (*checker*)
                   (repl :input (make-string-input-stream
                                 (format nil "val hamletRepl : string = 3;~%:quit~%"))
                         :output output
                         :error-output error-output
                         :prompt nil
                         :package "SML.HAMLET-REPL-TEST"))))
    (is (eq :quit result))
    (is (search "type mismatch" (get-output-stream-string error-output)))
    (is (not (search "hamletRepl = 3" (get-output-stream-string output))))))

(test repl-prefers-hamlet-diagnostic-for-unparseable-phrases
  (let* ((error-output (make-string-output-stream))
         (result (with-sml-type-checker (*checker*)
                   (repl :input (make-string-input-stream
                                 (format nil "val = ;~%:quit~%"))
                         :output (make-broadcast-stream)
                         :error-output error-output
                         :prompt nil
                         :package "SML.HAMLET-REPL-SYNTAX-TEST")))
         (printed (get-output-stream-string error-output)))
    (is (eq :quit result))
    (is (search "syntax error" printed))
    (is (not (search "esrap" (string-downcase printed))))))

(test repl-prints-hamlet-inferred-types
  (let* ((output (make-string-output-stream))
         (result (with-sml-type-checker (*checker*)
                   (repl :input (make-string-input-stream
                                 (format nil "fun hamletTwice f x = f (f x);~%hamletTwice (fn n => n + 1) 0~%:quit~%"))
                         :output output
                         :error-output (make-broadcast-stream)
                         :prompt nil
                         :package "SML.HAMLET-REPL-TYPES-TEST")))
         (printed (get-output-stream-string output)))
    (is (eq :quit result))
    (is (search "val hamletTwice = <fn> : ('a -> 'a) -> 'a -> 'a" printed))
    (is (search "val it = 2 : int" printed))))

(test hamlet-description-parsing-joins-continuation-lines
  (is (equal '(("x" . "int") ("long" . "int -> int -> int"))
             (cl-sml::hamlet-description-value-types
              (format nil "(* int *)~%val x : int~%val long :~%  int ->~%    int ->~%      int~%")))))

;;; HaMLet's elaboration also guides code generation (static-facts.lisp).

(defun checked-value (source name &optional (package "SML.HAMLET-FACTS-TEST"))
  (eval (compile-checked source package))
  (sml-value name package))

(test hamlet-identifier-status-decides-lowercase-constructor-patterns
  (is (= 2 (checked-value "datatype colour = red | green;
                           fun colourCode red = 1 | colourCode green = 2;
                           val greenCode = colourCode green;"
                          "greenCode")))
  (is (= 5 (checked-value "datatype tree = leaf | node of int;
                           fun nodeValue leaf = 0 | nodeValue (node n) = n;
                           val five = nodeValue (node 5);"
                          "five"))))

(test hamlet-identifier-status-decides-capitalized-variables
  (is (= 5 (checked-value "fun succX X = X + 1; val succ4 = succX 4;" "succ4"))))

(test hamlet-identifier-status-finds-constructors-of-same-program-structures
  (is (= 2 (checked-value "structure FactsA = struct datatype t = foo | bar end;
                           fun factsA FactsA.foo = 1 | factsA FactsA.bar = 2;
                           val factsABar = factsA FactsA.bar;"
                          "factsABar")))
  (is (= 2 (checked-value "structure FactsB = struct datatype t = baz | qux end;
                           open FactsB;
                           fun factsB baz = 1 | factsB qux = 2;
                           val factsBQux = factsB qux;"
                          "factsBQux")))
  (is (= 4 (checked-value "structure FactsC = struct exception oops of int
                           end;
                           val factsCaught = (raise FactsC.oops 4)
                                             handle FactsC.oops n => n;"
                          "factsCaught"))))

(fiveam:run! 'cl-sml-hamlet-suite)
