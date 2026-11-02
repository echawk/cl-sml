(defpackage #:cl-sml-compiler-tests
  (:use #:cl #:fiveam #:cl-sml))

(in-package #:cl-sml-compiler-tests)

(def-suite cl-sml-compiler-suite
  :description "Compiler surface tests for emitted Common Lisp forms")

(in-suite cl-sml-compiler-suite)

(test compile-expression-string-emits-expected-cl
  (is (equal '(cl-sml::sml-+ 1 2)
             (cl-sml:compile-sml-expression-string "1 + 2"
                                                   :package "SML.COMPILER-TEST"))))

(test compile-record-expression-string-emits-record-constructor
  (is (equal '(vector (load-time-value (cl-sml::sml-record-shape '("x" "y")) t) 1 2)
             (cl-sml:compile-sml-expression-string "{y = 2, x = 1}"
                                                   :package "SML.COMPILER-TEST"))))

(test compile-declarations-string-emits-local-helper-shape
  (let* ((form (cl-sml:compile-sml-declarations-string
                "local fun helper x = x + 1; in val y = helper 2; end;"
                :package "SML.COMPILER-TEST"))
         (printed (write-to-string form :pretty nil)))
    (is (eq 'progn (car form)))
    (is (search "LET" printed))
    (is (search "|helper|" printed))
    (is (search "DEFPARAMETER" printed))
    (is (search "|y|" printed))))

(test compile-file-emits-form-and-package-without-evaluation
  (multiple-value-bind (form package)
      ;; The package is named after the path relative to the project.
      (let ((*default-pathname-defaults* (asdf:system-source-directory "cl-sml")))
        (cl-sml:compile-sml-file #P"t/testdata/sample-program.sml"))
    (let ((printed (write-to-string form :pretty nil)))
      (is (eq 'progn (car form)))
      (is (string= "SML.FILE.T.TESTDATA.SAMPLE-PROGRAM" (package-name package)))
      (is (search "|file_result|" printed)))))

(test compile-program-runs-static-checker-before-lowering
  (let ((observed-source nil)
        (observed-filename nil))
    (cl-sml:with-sml-type-checker
        ((lambda (source filename)
           (setf observed-source source
                 observed-filename filename)))
      (cl-sml:compile-sml-program-string
       "val checked = 42;"
       :package "SML.STATIC-CHECK-PHASE-TEST"
       :source-name "checked.sml"))
    (is (string= "val checked = 42;" observed-source))
    (is (string= "checked.sml" observed-filename))))

(test compile-program-stops-when-static-checker-rejects-source
  (signals cl-sml:sml-static-type-error
    (cl-sml:compile-sml-program-string
     "val invalid = 1 + true;"
     :package "SML.STATIC-CHECK-REJECTION-TEST"
     :type-checker
     (lambda (source filename)
       (error 'cl-sml:sml-static-type-error
              :source source
              :filename filename
              :cause "rejected by test checker")))))

(test compile-functor-argument-structure-expressions
  ;; A structure identifier is passed by name; other arguments become
  ;; declarations of an argument module.
  (is (equal "TyName"
             (cl-sml::functor-argument-structure-name '(:str-id "TyName"))))
  (is (null (cl-sml::functor-argument-structure-name
             '(:struct ((:val (:pat-var "compare") (:var "TyName.compare")))))))
  (is (equal '((:val (:pat-var "compare") (:var "TyName.compare")))
             (cl-sml::sml-strexp-body-decs
              '(:struct ((:val (:pat-var "compare") (:var "TyName.compare"))))))))

(test compile-literal-case-uses-native-common-lisp-case
  (let ((form (cl-sml:compile-sml-expression-string
               "case code of 1 => \"one\" | 2 => \"two\" | _ => \"other\""
               :package "SML.COMPILER-TEST")))
    (is (eq 'case (first form)))
    (is (string= "code" (symbol-name (second form))))
    (is (equal '(1) (first (third form))))
    (is (equal '(2) (first (fourth form))))
    (is (eq 'otherwise (first (fifth form))))
    (is (search "INVOKE-SML-CASE-ACTION"
                (write-to-string form :pretty nil)))))

(test compile-self-tail-calls-as-jumps
  (flet ((compiled (source)
           (write-to-string (cl-sml:compile-sml-declarations-string
                             source :package "SML.COMPILER-TEST")
                            :pretty nil)))
    (let ((looping (compiled "fun loopDown 0 acc = acc
                                | loopDown n acc = loopDown (n - 1) (acc + n);"))
          (recursive (compiled "fun recurDown 0 = 0
                                  | recurDown n = 1 + recurDown (n - 1);")))
      (is (search "TAGBODY" looping))
      (is (search "(GO " looping))
      (is (not (search "TAGBODY" recursive))))))

(test tuple-taking-functions-get-a-worker
  ;; ROADMAP item 9: `fun f (x, y)' is a two-argument worker plus a
  ;; wrapper; a call with a tuple expression calls the worker, and a
  ;; self tail call through a tuple is a jump.
  (let* ((package "SML.COMPILER-WORKER-TEST")
         (form (cl-sml:compile-sml-program-string
                "fun sumTo (0, acc) = acc | sumTo (n, acc) = sumTo (n - 1, acc + n);
                 fun swap (a, b) = (b, a);
                 val viaWorker = sumTo (100000, 0);
                 val p = (3, 4);
                 val viaWrapper = sumTo p;
                 val mapped = map swap [(1, 2), (3, 4)];"
                :package package))
         (printed (write-to-string form :pretty nil)))
    (is (search "sumTo;1/2" printed))
    (is (search "TAGBODY" printed))
    (eval form)
    (is (= 5000050000 (cl-sml:sml-value "viaWorker" package)))
    (is (= 10 (cl-sml:sml-value "viaWrapper" package)))
    (is (equal '((2 1) (4 3))
               (mapcar #'cl-sml:sml-tuple->list (cl-sml:sml-value "mapped" package))))))

(test compile-large-tuple-case-hoists-branch-actions
  (let* ((source
           (with-output-to-string (stream)
             (write-string "case pair of " stream)
             (loop for i below 12
                   do (format stream "~:[~; | ~](~D, x) => x + ~D"
                              (plusp i) i i))
             (write-string " | _ => 0" stream)))
         (form (cl-sml:compile-sml-expression-string
                source :package "SML.COMPILER-TEST"))
         (printed (write-to-string form :pretty nil :circle t)))
    (is (eq 'progn (first form)))
    (is (search "DEFUN" printed))
    (is (search "SML-CASE-ACTION" printed))
    (is (search "CASE-VALUE" printed))))

(test emit-sml-file-writes-loadable-common-lisp
  (let ((artifact (asdf:system-relative-pathname "cl-sml" "t/testdata/sample-program.generated.lisp")))
    (unwind-protect
         (multiple-value-bind (output package)
             (cl-sml:emit-sml-file (asdf:system-relative-pathname "cl-sml" "t/testdata/sample-program.sml") artifact
                                   :package "SML.ARTIFACT-TEST")
           (is (probe-file output))
           (is (string= "SML.ARTIFACT-TEST" (package-name package)))
           (let ((text (uiop:read-file-string output)))
             (is (search "Generated by cl-sml" text))
             (is (search "SML.ARTIFACT-TEST" text)))
           (load output)
           (is (= 120 (cl-sml:sml-value "file_result" package))))
      (when (probe-file artifact)
        (delete-file artifact)))))

(test load-sml-file-can-use-interpreted-debug-path
  (multiple-value-bind (package result)
      (cl-sml:load-sml-file (asdf:system-relative-pathname "cl-sml" "t/testdata/sample-program.sml")
                            :package "SML.INTERPRETED-LOAD-TEST"
                            :compile nil)
    (declare (ignore result))
    (is (= 120 (cl-sml:sml-value "file_result" package)))))

(test load-sml-file-uses-compiled-artifact-path-by-default
  (multiple-value-bind (package result)
      (cl-sml:load-sml-file (asdf:system-relative-pathname "cl-sml" "t/testdata/sample-program.sml")
                            :package "SML.COMPILED-LOAD-TEST")
    (is (string= "SML.COMPILED-LOAD-TEST" (package-name package)))
    (is (= 120 (cl-sml:sml-value "file_result" package)))
    (is (not (null result)))))

(test load-sml-file-compiles-hoisted-dispatch-helpers
  (multiple-value-bind (package result)
      (cl-sml:load-sml-file (asdf:system-relative-pathname "cl-sml" "t/testdata/large-dispatch.sml")
                            :package "SML.COMPILED-DISPATCH-TEST")
    (is (not (null result)))
    (is (= 42 (cl-sml:sml-value "dispatch_result" package)))))

(test emitted-functions-declare-the-optimize-policy
  ;; ROADMAP item 6: every generated function declares the policy, and
  ;; the debug policy still runs a deep tail-recursive loop.
  (flet ((declares-p (policy qualities)
           (let ((cl-sml:*sml-optimize-policy* policy))
             (let ((form (cl-sml:compile-sml-program-string
                          "fun f x = x + 1;" :package "SML.POLICY-TEST")))
               (labels ((walk (x)
                          (or (equal x `(declare (optimize ,@qualities)))
                              (and (consp x) (or (walk (car x)) (walk (cdr x)))))))
                 (walk form))))))
    (is (declares-p :default '((speed 1) (safety 1) (debug 1))))
    (is (declares-p :debug '((speed 0) (safety 3) (debug 2))))
    (is (declares-p '((speed 3)) '((speed 3)))))
  (let ((cl-sml:*sml-optimize-policy* :debug)
        (package "SML.POLICY-RUN-TEST"))
    (eval (cl-sml:compile-sml-program-string
           "fun loop (0, acc) = acc | loop (n, acc) = loop (n - 1, acc + 1);
            val r = loop (1000000, 0);"
           :package package))
    (is (eql 1000000 (cl-sml:sml-value "r" package)))))

(test ir-type-lint-accepts-the-benchmarks-and-corpora
  ;; Every IR term lowered from the benchmark programs and from the Core
  ;; and Modules conformance corpora has a type on every node, and its
  ;; types fit together (docs/IR.md, "The type lint").  The programs are
  ;; compiled, not run; a program that does not compile alone is skipped.
  (let ((files (mapcan (lambda (directory)
                         (directory (merge-pathnames
                                     "*.sml" (asdf:system-relative-pathname "cl-sml" directory))))
                       '("t/bench/" "t/conformance/core/tests/"
                         "t/conformance/modules/tests/")))
        (problems nil)
        (compiled 0))
    (dolist (file files)
      (let ((cl-sml:*sml-type-checker* :native)
            (cl-sml::*sml-ir-lint* :collect)
            (cl-sml::*sml-ir-lint-problems* nil)
            (*error-output* (make-broadcast-stream)))
        (handler-case
            (progn
              (handler-bind ((warning #'muffle-warning))
                (cl-sml:compile-sml-file
                 file :package (format nil "SML.IR-LINT-TEST.~:@(~A~)" (pathname-name file))))
              (incf compiled))
          (error () nil))
        (when cl-sml::*sml-ir-lint-problems*
          (push (cons (pathname-name file) cl-sml::*sml-ir-lint-problems*) problems))))
    (is (> compiled 200))
    (is (null problems) "IR type lint problems:~{~%~S~}" problems)))
