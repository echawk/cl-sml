(defpackage #:cl-sml-compiler-tests
  (:use #:cl #:fiveam #:cl-sml))

(in-package #:cl-sml-compiler-tests)

(def-suite cl-sml-compiler-suite
  :description "Compiler surface tests for emitted Common Lisp forms")

(in-suite cl-sml-compiler-suite)

(test compile-expression-string-emits-expected-cl
  (is (equal '(funcall (funcall #'cl-sml::sml-+ 1) 2)
             (cl-sml:compile-sml-expression-string "1 + 2"
                                                   :package "SML.COMPILER-TEST"))))

(test compile-record-expression-string-emits-record-constructor
  (is (equal '(cl-sml::make-sml-record
               (list (cons "x" 1)
                     (cons "y" 2)))
             (cl-sml:compile-sml-expression-string "{y = 2, x = 1}"
                                                   :package "SML.COMPILER-TEST"))))

(test compile-declarations-string-emits-local-helper-shape
  (let* ((form (cl-sml:compile-sml-declarations-string
                "local fun helper x = x + 1; in val y = helper 2; end;"
                :package "SML.COMPILER-TEST"))
         (printed (write-to-string form :pretty nil)))
    (is (eq 'progn (car form)))
    (is (search "LET" printed))
    (is (search "HELPER" printed))
    (is (search "DEFPARAMETER" printed))
    (is (search "Y" printed))))

(test compile-file-emits-form-and-package-without-evaluation
  (multiple-value-bind (form package)
      (cl-sml:compile-sml-file #P"testdata/sample-program.sml")
    (let ((printed (write-to-string form :pretty nil)))
      (is (eq 'progn (car form)))
      (is (string= "SML.FILE.TESTDATA.SAMPLE-PROGRAM" (package-name package)))
      (is (search "FILE_RESULT" printed)))))

(fiveam:run! 'cl-sml-compiler-suite)
