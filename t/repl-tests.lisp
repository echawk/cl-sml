(defpackage #:cl-sml-repl-tests
  (:use #:cl #:fiveam #:cl-sml))

(in-package #:cl-sml-repl-tests)

(def-suite cl-sml-repl-suite
  :description "REPL tests for cl-sml")

(in-suite cl-sml-repl-suite)

(defun sml-user-symbol-status (name)
  (nth-value 1 (find-symbol name "SML-USER")))

(defun run-repl-session (input)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((result
            (with-input-from-string (stream input)
              (cl-sml:repl :input stream :output out :error-output err :prompt t))))
      (values result
              (get-output-stream-string out)
              (get-output-stream-string err)))))

(defun run-repl-session-in-package (input package)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((result
            (with-input-from-string (stream input)
              (cl-sml:repl :input stream
                           :output out
                           :error-output err
                           :prompt t
                           :package package))))
      (values result
              (get-output-stream-string out)
              (get-output-stream-string err)))))

(test repl-handles-declarations-and-expressions
  (multiple-value-bind (result output error-output)
      (run-repl-session (format nil "val x = 10;~%x + 5~%:quit~%"))
    (is (eq :quit result))
    (is (search "- " output))
    (is (search "val x = 10" output))
    (is (search "val it = 15" output))
    (is (eq :external (sml-user-symbol-status "x")))
    (is (string= "" error-output))))

(test repl-supports-expression-semicolons-and-multiline-definitions
  (multiple-value-bind (result output error-output)
      (run-repl-session (format nil "fun add a b =~%  a + b;~%add 2 3;~%:quit~%"))
    (is (eq :quit result))
    (is (search "= " output))
    (is (search "val add = <fn>" output))
    (is (search "val it = 5" output))
    (is (string= "" error-output))))

(test repl-recovers-after-errors
  (multiple-value-bind (result output error-output)
      (run-repl-session (format nil "val x = ;~%1 + 2~%:quit~%"))
    (is (eq :quit result))
    (is (search "Error:" error-output))
    (is (search "val it = 3" output))))

(test repl-runs-static-checker-for-declarations-and-expressions
  (let ((checked nil))
    (cl-sml:with-sml-type-checker
        ((lambda (source filename)
           (declare (ignore filename))
           (push source checked)))
      (multiple-value-bind (result output error-output)
          (run-repl-session (format nil "val checked = 7;~%checked + 1~%:quit~%"))
        (is (eq :quit result))
        (is (search "val it = 8" output))
        (is (string= "" error-output))))
    (is (find "val checked = 7;" checked :test #'string=))
    (is (find "val it = (checked + 1);" checked :test #'string=))))

(test repl-does-not-evaluate-statically-rejected-phrase
  (let ((package "SML.REPL-STATIC-REJECTION-TEST"))
    (cl-sml:with-sml-type-checker
        ((lambda (source filename)
           (declare (ignore filename))
           (error 'cl-sml:sml-static-type-error
                  :source source
                  :cause "rejected by test checker")))
      (multiple-value-bind (result output error-output)
          (run-repl-session-in-package
           (format nil "val should_not_exist = 9;~%:quit~%") package)
        (is (eq :quit result))
        (is (not (search "should_not_exist = 9" output)))
        (is (search "static elaboration failed" error-output))))
    (let ((symbol (find-symbol "SHOULD_NOT_EXIST" package)))
      (is (or (null symbol) (not (boundp symbol)))))))

(test repl-sees-file-loaded-definitions
  (cl-sml:load-sml-file (asdf:system-relative-pathname "cl-sml" "t/testdata/sample-program.sml"))
  (multiple-value-bind (result output error-output)
      (run-repl-session (format nil "fileFact 4~%:quit~%"))
    (is (eq :quit result))
    (is (search "Error:" error-output))
    (is (not (search "val it = 24" output)))))

(test repl-can-target-file-package
  (multiple-value-bind (package result)
      (cl-sml:load-sml-file (asdf:system-relative-pathname "cl-sml" "t/testdata/sample-program.sml"))
    (declare (ignore result))
    (multiple-value-bind (repl-result output error-output)
        (run-repl-session-in-package (format nil "fileFact 4~%:quit~%") package)
      (is (eq :quit repl-result))
      (is (search "val it = 24" output))
      (is (string= "" error-output)))))

(test repl-shows-records-exceptions-and-types
  (multiple-value-bind (result output error-output)
      (run-repl-session
       (format nil "val point = {y = 2, x = 1};~%exception E;~%#x point~%((raise E) handle E => 1)~%:quit~%"))
    (is (eq :quit result))
    (is (search "val point = {x = 1, y = 2} : {x : int, y : int}" output))
    (is (search "exception E" output))
    (is (search "val it = 1 : int" output))
    (is (string= "" error-output))))

(test repl-returns-eof-on-end-of-input
  (multiple-value-bind (result output error-output)
      (run-repl-session "")
    (is (eq :eof result))
    (is (search "- " output))
    (is (string= "" error-output))))

(test repl-reports-module-and-compound-declarations
  (multiple-value-bind (result output error-output)
      (run-repl-session-in-package
       (format nil "structure ReplS = struct val v = 1 end;~%fun replEven 0 = true | replEven n = replOdd (n - 1)~%and replOdd 0 = false | replOdd n = replEven (n - 1);~%local val hidden = 2 in val replShown = hidden end;~%:quit~%")
       "SML.REPL-REPORT-TEST")
    (is (eq :quit result))
    (is (search "structure ReplS" output))
    (is (not (search ":STRUCTURE" output)))
    (is (search "val replEven = <fn>" output))
    (is (search "val replOdd = <fn>" output))
    (is (search "val replShown = 2" output))
    (is (not (search "hidden" output)))
    (is (string= "" error-output))))

;;; REPL commands

(test repl-lisp-command-shows-generated-code-with-location
  (multiple-value-bind (result output error-output)
      (run-repl-session-in-package
       (format nil "val replOne = 1;~%fun replSquare x =~%  x * x;~%:lisp replSquare~%:quit~%")
       "SML.REPL-LISP-COMMAND-TEST")
    (is (eq :quit result))
    (is (search ";;; fun replSquare -- stdIn:2" output))
    (is (search "(defparameter |replSquare;" output))
    (is (string= "" error-output))))

(test repl-ir-command-shows-the-ir
  (multiple-value-bind (result output error-output)
      (run-repl-session-in-package
       (format nil "fun replInc n = n + 1;~%:ir replInc~%:quit~%")
       "SML.REPL-IR-TEST")
    (is (eq :quit result))
    (is (search ";;; fun replInc" output))
    (is (search "(:overloaded \"+\" \"int\")" output))
    (is (string= "" error-output))))

(test repl-lisp-local-command-shows-enclosing-local
  (multiple-value-bind (result output error-output)
      (run-repl-session-in-package
       (format nil "local val step = 2 in fun replNext n = n + step end;~%:lisp-local replNext~%:quit~%")
       "SML.REPL-LISP-LOCAL-TEST")
    (is (eq :quit result))
    (is (search "Shown with its enclosing `local'" output))
    (is (search "(let ((|step|" output))
    (is (string= "" error-output))))

(test repl-expand-command-compiles-without-running
  (multiple-value-bind (result output error-output)
      (run-repl-session-in-package
       (format nil ":expand val replGhost = 7~%replGhost;~%:expand fn n => n + 2~%:quit~%")
       "SML.REPL-EXPAND-TEST")
    (is (eq :quit result))
    (is (search "(defparameter |replGhost;1| 7)" output))
    ;; The checker resolves `+' at int.
    (is (search "(cl-sml::sml-int-+ " output))
    (is (not (search "val it = 7" output)))
    (is (search "Error:" error-output))))

(test repl-show-lisp-echoes-each-phrase
  (multiple-value-bind (result output error-output)
      (run-repl-session-in-package
       (format nil ":show-lisp on~%val replShown = 3;~%:show-lisp off~%val replHidden = 4;~%:quit~%")
       "SML.REPL-SHOW-LISP-TEST")
    (is (eq :quit result))
    (is (search "(defparameter |replShown;1| 3)" output))
    (is (not (search "(defparameter |replHidden;" output)))
    (is (search "val replHidden = 4" output))
    (is (string= "" error-output))))

(test repl-time-command-reports-phases
  (multiple-value-bind (result output error-output)
      (run-repl-session-in-package
       (format nil ":time on~%val replTimed = 5;~%:quit~%")
       "SML.REPL-TIME-TEST")
    (is (eq :quit result))
    (is (search ";; SML -> Lisp" output))
    (is (search ";; total" output))
    (is (search "val replTimed = 5" output))
    (is (string= "" error-output))))

(test repl-disassemble-command
  (multiple-value-bind (result output error-output)
      (run-repl-session-in-package
       (format nil "fun replInc n = n + 1;~%val replNumber = 1;~%:disassemble replInc~%:disassemble replNumber~%:quit~%")
       "SML.REPL-DISASSEMBLE-TEST")
    (is (eq :quit result))
    (is (search "val replInc = <fn>" output))
    ;; SBCL only (hence one check fewer on ECL, 92 against 93): ECL's
    ;; DISASSEMBLE of a function compiled to native code prints no
    ;; disassembly, only a warning that it has no source code for it.
    #+sbcl (is (search "disassembly" output))
    (is (search "replNumber is not a function" error-output))))

(test repl-help-and-unknown-commands
  (multiple-value-bind (result output error-output)
      (run-repl-session (format nil ":help~%:frobnicate~%:lisp~%:quit~%"))
    (is (eq :quit result))
    (dolist (command '(":lisp NAME" ":expand PHRASE" ":show-lisp on|off"
                       ":time on|off" ":disassemble NAME"))
      (is (search command output)))
    (is (search "Unknown command :frobnicate" error-output))
    (is (search "Usage: :lisp NAME" error-output))))

(test repl-commands-are-only-recognized-at-the-primary-prompt
  ;; A continuation line may start with a type constraint.
  (multiple-value-bind (result output error-output)
      (run-repl-session-in-package
       (format nil "val replTyped~%:int = 3;~%:quit~%")
       "SML.REPL-CONTINUATION-TEST")
    (is (eq :quit result))
    (is (search "val replTyped = 3" output))
    (is (string= "" error-output))))

;;; The cl-sml executable's entry point

(defun run-main (&rest arguments)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((status (let ((*standard-output* out)
                        (*error-output* err))
                    (cl-sml:main arguments))))
      (values status
              (get-output-stream-string out)
              (get-output-stream-string err)))))

(test main-emits-lisp-runs-files-and-reports-errors
  (let ((sample (namestring (asdf:system-relative-pathname
                             "cl-sml" "t/testdata/sample-program.sml"))))
    (multiple-value-bind (status output) (run-main "--emit-lisp" sample)
      (is (= 0 status))
      (is (search ";;; Generated by cl-sml" output))
      (is (search "sample-program.sml:4: fun fileFact" output)))
    (multiple-value-bind (status output error-output)
        (run-main "--time" "--run" sample)
      (is (= 0 status))
      ;; The Lisp compiler's report on generated code is not shown.
      (is (not (search "compiling" output)))
      (is (search ";; total" error-output)))
    (multiple-value-bind (status output) (run-main "--help")
      (is (= 0 status))
      (is (search "--emit-lisp" output)))
    (is (= 1 (run-main "--run" "no-such-file.sml")))
    (is (= 2 (run-main "--no-such-option")))))
