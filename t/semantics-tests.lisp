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
