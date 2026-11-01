(defpackage #:cl-sml-inspect-tests
  (:use #:cl #:fiveam #:cl-sml))

(in-package #:cl-sml-inspect-tests)

(def-suite cl-sml-inspect-suite
  :description "Inspecting the Common Lisp generated for SML")

(in-suite cl-sml-inspect-suite)

(defun load-sml-string (source package &key (source-name "inspect.sml"))
  "Compile and evaluate SOURCE in PACKAGE, as LOAD-SML-FILE does."
  (with-sml-package (package)
    (eval (compile-sml-program-string source
                                      :package package
                                      :source-name source-name))))

(defun definition-of (name package)
  (first (nth-value 1 (sml-lisp-source name :package package))))

(defun mentions-symbol-named-p (name form)
  "Does FORM contain a symbol named NAME?"
  (cond
    ((symbolp form) (string= name (symbol-name form)))
    ((consp form) (or (mentions-symbol-named-p name (car form))
                      (mentions-symbol-named-p name (cdr form))))))

(defun defparameter-of-p (name form)
  "Does FORM define NAME with DEFPARAMETER, in a binding cell |NAME;k|?"
  (and (consp form)
       (or (and (eq (first form) 'defparameter)
                (symbolp (second form))
                (let ((defined (symbol-name (second form))))
                  (and (< (length name) (length defined))
                       (string= name defined :end2 (length name))
                       (char= #\; (char defined (length name))))))
           (some (lambda (subform) (defparameter-of-p name subform))
                 (remove-if-not #'consp form)))))

(test sml-lisp-source-returns-the-form-defining-a-binding
  (let ((package "SML.INSPECT-SOURCE-TEST"))
    (load-sml-string (format nil "fun square x = x * x;~%val nine = square 3;")
                     package)
    (let ((forms (sml-lisp-source "square" :package package)))
      (is (= 1 (length forms)))
      (is (defparameter-of-p "square" (first forms)))
      (is (not (defparameter-of-p "nine" (first forms)))))
    (let ((definition (definition-of "nine" package)))
      (is (string= "inspect.sml" (cl-sml::sml-lisp-definition-source definition)))
      (is (= 2 (cl-sml::sml-lisp-definition-line definition)))
      (is (string= "val" (cl-sml::sml-lisp-definition-keyword definition))))
    (is (null (sml-lisp-source "notCompiledHere" :package package)))))

(test sml-lisp-source-knows-structures-and-their-members
  (let ((package "SML.INSPECT-STRUCTURE-TEST"))
    (load-sml-string (format nil "structure Shapes = struct~%  datatype shape = Circle of int~%  fun area (Circle r) = 3 * r * r~%end;")
                     package)
    (let ((member (definition-of "Shapes.area" package))
          (structure (definition-of "Shapes" package))
          (constructor (definition-of "Shapes.Circle" package)))
      (is (= 3 (cl-sml::sml-lisp-definition-line member)))
      (is (defparameter-of-p "Shapes.area" (cl-sml::sml-lisp-definition-form member)))
      (is (= 1 (cl-sml::sml-lisp-definition-line structure)))
      (is (mentions-symbol-named-p "REGISTER-SML-STRUCTURE-MEMBERS"
                                   (cl-sml::sml-lisp-definition-form structure)))
      (is (string= "constructor" (cl-sml::sml-lisp-definition-keyword constructor)))
      (is (eq :type (cl-sml::sml-lisp-definition-namespace
                     (definition-of "Shapes.shape" package)))))))

(test sml-lisp-source-finds-functor-instance-members-in-the-functor
  (let ((package "SML.INSPECT-FUNCTOR-TEST"))
    (load-sml-string "functor AddY (X : sig val y : int end) = struct fun addY z = z + X.y end;
structure Three = struct val y = 3 end;
structure AddThree = AddY(Three);
val six = AddThree.addY 3;"
                     package)
    (is (= 6 (sml-value "six" package)))
    (is (string= "AddY.addY"
                 (cl-sml::sml-lisp-definition-name
                  (definition-of "AddThree.addY" package))))
    (is (eq :functor (cl-sml::sml-lisp-definition-namespace
                      (definition-of "AddY" package))))))

(test sml-lisp-source-lines-survive-type-declaration-joining
  ;; Continuation lines of type declarations are joined before parsing.
  (let ((package "SML.INSPECT-LINES-TEST"))
    (load-sml-string (format nil "type pair =~%  int~%  * int~%fun swap ((a, b) : pair) = (b, a)")
                     package)
    (is (= 4 (cl-sml::sml-lisp-definition-line (definition-of "swap" package))))))

(test sml-lisp-source-shows-local-bindings-with-their-local
  (let ((package "SML.INSPECT-LOCAL-TEST"))
    (load-sml-string "local val counter = ref 0 in fun next () = (counter := !counter + 1; !counter) end"
                     package)
    (let ((own (first (sml-lisp-source "next" :package package)))
          (enclosing (first (sml-lisp-source "next" :package package :enclosing t))))
      (is (defparameter-of-p "next" own))
      (is (not (eq 'let (first own))))
      (is (eq 'let (first enclosing)))
      (is (defparameter-of-p "next" enclosing)))))

(test sml-lisp-source-includes-hoisted-helpers
  (let ((package "SML.INSPECT-HOISTED-TEST")
        (source (with-output-to-string (stream)
                  (write-string "fun dispatch pair = case pair of " stream)
                  (loop for i below 12
                        do (format stream "~:[~; | ~](~D, x) => x + ~D"
                                   (plusp i) i i))
                  (write-string " | _ => 0" stream))))
    (load-sml-string source package)
    (let ((forms (sml-lisp-source "dispatch" :package package)))
      (is (< 1 (length forms)))
      (is (every (lambda (form) (eq 'defun (first form))) (butlast forms)))
      (is (defparameter-of-p "dispatch" (car (last forms)))))))

(test record-sml-lisp-source-can-be-turned-off
  (let ((package "SML.INSPECT-OPT-OUT-TEST"))
    (let ((*record-sml-lisp-source* nil))
      (load-sml-string "val unrecorded = 1" package))
    (is (= 1 (sml-value "unrecorded" package)))
    (is (null (sml-lisp-source "unrecorded" :package package)))
    (load-sml-string "val recorded = 2" package)
    (is (not (null (sml-lisp-source "recorded" :package package))))
    (clear-sml-lisp-source package)
    (is (null (sml-lisp-source "recorded" :package package)))))

(test show-sml-lisp-prints-readable-code-with-its-location
  (let ((package "SML.INSPECT-SHOW-TEST"))
    (load-sml-string (format nil "val one = 1;~%fun double n = n + n;") package)
    (let ((printed (with-output-to-string (stream)
                     (show-sml-lisp "double" :package package :stream stream))))
      (is (search ";;; fun double -- inspect.sml:2" printed))
      ;; SML names print unqualified, with their case.
      (is (search "(defparameter |double;" printed))
      (is (not (search "SML.INSPECT-SHOW-TEST::" printed))))
    (signals error
      (show-sml-lisp "missing" :package package :stream (make-broadcast-stream)))))

(test sml->lisp-compiles-phrases-without-evaluating-them
  (let ((package "SML.INSPECT-EXPAND-TEST"))
    (let ((form (sml->lisp "val notEvaluated = 41 + 1" :package package)))
      (is (defparameter-of-p "notEvaluated" form)))
    (let ((symbol (find-symbol "notEvaluated" package)))
      (is (or (null symbol) (not (boundp symbol)))))
    (is (null (sml-lisp-source "notEvaluated" :package package)))
    (is (equal '(cl-sml::sml-int-+ 1 2)
               (sml->lisp "1 + 2" :package package)))
    (is (equal '(cl-sml::sml-+ 1 2)
               (sml->lisp "1 + 2" :package package :type-checker nil)))
    (is (equal (sml->lisp "1 + 2" :package package)
               (sml->lisp "1 + 2;" :package package)))
    (let ((printed (with-output-to-string (stream)
                     (print-sml-lisp (sml->lisp "fun inc n = n + 1" :package package)
                                     :package package :stream stream))))
      (is (search "(defparameter |inc;" printed)))))

(test with-sml-phase-timings-reports-each-phase
  (let* ((report (make-string-output-stream))
         (package (with-sml-phase-timings (report)
                    (load-sml-file (asdf:system-relative-pathname
                                    "cl-sml" "t/testdata/sample-program.sml")
                                   :package "SML.INSPECT-TIMING-TEST")))
         (printed (get-output-stream-string report)))
    (is (string= "SML.INSPECT-TIMING-TEST" (package-name package)))
    (dolist (phase '("parse SML" "SML -> Lisp" "compile Lisp"
                     "load compiled Lisp" "total"))
      (is (search phase printed)))))

(defun emitted-text (sml package)
  (uiop:with-temporary-file (:pathname lisp :type "lisp")
    (emit-sml-file sml lisp :package package)
    (uiop:read-file-string lisp)))

(test emitted-artifacts-are-commented-and-deterministic
  (uiop:with-temporary-file (:pathname sml :type "sml")
    (with-open-file (stream sml :direction :output :if-exists :supersede)
      (format stream "val answer = 42;~%~%fun twice f x = f (f x);~%"))
    (let ((text (emitted-text sml "SML.INSPECT-ARTIFACT-TEST")))
      (is (search "(in-package \"SML.INSPECT-ARTIFACT-TEST\")" text))
      (is (search ":1: val answer" text))
      (is (search ":3: fun twice" text))
      ;; Compiling something else in between does not change the artifact.
      (sml->lisp "fun unrelated a b c = (a, b, c)"
                 :package "SML.INSPECT-ARTIFACT-TEST")
      (is (string= text (emitted-text sml "SML.INSPECT-ARTIFACT-TEST"))))))

(test emitted-artifacts-separate-declarations-from-hoisted-helpers
  (let ((text (emitted-text (asdf:system-relative-pathname
                             "cl-sml" "t/testdata/large-dispatch.sml")
                            "SML.INSPECT-HOISTED-ARTIFACT-TEST")))
    (is (search "Helper functions hoisted" text))
    (is (search "large-dispatch.sml:1: fun dispatch" text))
    (is (search "large-dispatch.sml:17: val dispatch_result" text))
    ;; Each declaration is a top-level form of its own.
    (is (< (search "large-dispatch.sml:1: fun dispatch" text)
           (search "(defparameter |dispatch;" text)
           (search "large-dispatch.sml:17: val dispatch_result" text)
           (search "(defparameter |dispatch_result;" text)))))

(test emitted-artifacts-load-into-a-fresh-package
  ;; An SML name that is also a CL symbol name is shadowed in its package;
  ;; the artifact must shadow it too when it creates the package.
  (let ((package "SML.INSPECT-FRESH-ARTIFACT-TEST"))
    (uiop:with-temporary-file (:pathname sml :type "sml")
      (with-open-file (stream sml :direction :output :if-exists :supersede)
        (format stream "fun LIST n = n + n;~%val doubled = LIST 5;~%"))
      (uiop:with-temporary-file (:pathname lisp :type "lisp")
        (emit-sml-file sml lisp :package package)
        (delete-package package)
        (load lisp)
        (is (= 10 (sml-value "doubled" package)))
        (is (functionp (sml-value "LIST" package)))))))
