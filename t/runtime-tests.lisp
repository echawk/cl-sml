(defpackage #:cl-sml-runtime-tests
  (:use #:cl #:fiveam #:cl-sml)
  (:shadow #:sml-value)
  )

(in-package #:cl-sml-runtime-tests)

(def-suite cl-sml-runtime-suite
  :description "Runtime and integration tests for cl-sml")

(in-suite cl-sml-runtime-suite)

(defparameter *test-sml-package* "SML-USER")

(defun eval-sml-program (source)
  (eval (cl-sml:compile-sml-program-string source :package *test-sml-package*)))

(defun eval-sml-expr (source)
  (let ((cl-sml::*sml-package* (cl-sml::ensure-sml-package *test-sml-package*)))
    (eval (cl-sml::compile-expr (cl-sml:parse-sml-expression source)))))

(defun sml-symbol (name &optional (package *test-sml-package*))
  (find-symbol name package))

(defun sml-value (name &optional (package *test-sml-package*))
  (symbol-value (sml-symbol name package)))

(defun sml-symbol-status (name &optional (package *test-sml-package*))
  (nth-value 1 (find-symbol name package)))

(test runtime-list-and-equality-functions
  (is (equal '(1 2 3 4)
             (cl-sml::sml-@ '(1 2) '(3 4))))
  (is (eql 10 (cl-sml::sml-hd '(10 20 30))))
  (is (equal '(20 30) (cl-sml::sml-tl '(10 20 30))))
  (is (eql 3 (cl-sml::sml-length '(a b c))))
  (is (eq t (cl-sml::sml-null nil)))
  (is (equal '(3 2 1) (cl-sml::sml-rev '(1 2 3))))
  (is (eq t (cl-sml::sml-= '(1 2) '(1 2))))
  (is (eq t (cl-sml::sml-<> '(1 2) '(2 1)))))

(test runtime-pervasive-option-constructors
  ;; Every package sees the basis's option constructors, which the run-time
  ;; primitives also use.
  (let* ((package (cl-sml::ensure-sml-package "SML.OPTION-BASIS-TEST"))
         (basis cl-sml::*sml-basis-package-name*)
         (none (sml-value "NONE" package))
         (some (cl-sml:sml-function "SOME" package))
         (value (funcall some 42)))
    (is (eq none (cl-sml::sml-symbol "NONE" basis)))
    (is (eq (car value) (cl-sml::sml-symbol "SOME" basis)))
    (is (= 42 (cdr value)))
    (is (eq none (cl-sml::sml-none-value)))
    (is (equal value (cl-sml::sml-some-value 42)))
    (is (eq :external
            (nth-value 1 (find-symbol "SOME" basis))))))

(test runtime-higher-order-functions
  (is (equal '(2 3 4)
             (funcall (cl-sml::sml-map (lambda (x) (+ x 1))) '(1 2 3))))
  (is (= 6
         (funcall (funcall (cl-sml::sml-foldl #'cl-sml::sml-op-+) 0) '(1 2 3))))
  (is (= 6
         (funcall (funcall (cl-sml::sml-foldr #'cl-sml::sml-op-+) 0) '(1 2 3))))
  (let ((tuple-step
          (lambda (pair)
            (+ (second pair) (* 10 (third pair))))))
    (is (= 123
           (funcall (funcall (cl-sml::sml-foldl tuple-step) 0) '(1 2 3))))
    (is (= 321
           (funcall (funcall (cl-sml::sml-foldr tuple-step) 0) '(1 2 3)))))
  (is (= 21
         (cl-sml::sml-before (list :tuple 21 99))))
  (is (= 21
         (funcall (cl-sml::sml-o
                   (list :tuple
                         (lambda (x) (+ x 1))
                         (lambda (x) (* x 2))))
                  10)))
  ;; Binary operators are two-argument primitives and pair-taking values.
  (is (= 5 (cl-sml::sml-+ 2 3)))
  (is (= 5 (cl-sml::sml-op-+ (list :tuple 2 3)))))

(test runtime-string-and-char-functions
  (is (string= "abcd" (cl-sml::sml-concat '("ab" "cd"))))
  (is (= 4 (cl-sml::sml-size "test")))
  (is (equal '(#\a #\b) (cl-sml::sml-explode "ab")))
  (is (string= "ab" (cl-sml::sml-implode '(#\a #\b))))
  (is (= 65 (cl-sml::sml-ord #\A)))
  (is (char= #\A (cl-sml::sml-chr 65)))
  (is (string= "Z" (cl-sml::sml-str #\Z)))
  (is (eq t (cl-sml::sml-< #\A #\B)))
  (is (eq t (cl-sml::sml-< "ab" "ac")))
  (is (= 7 (eval-sml-expr "#2 (\"\", 7)"))))

(test runtime-sequence-subscript-raises-sml-exception
  (let* ((package-name "SML.SUBSCRIPT-TEST")
         (cl-sml::*sml-package* (cl-sml::ensure-sml-package package-name))
         (subscript-symbol (cl-sml::sml-symbol "Subscript" package-name)))
    (setf (symbol-value subscript-symbol)
          (cl-sml::make-sml-exception-constructor "Subscript"))
    (handler-case
        (progn
          (funcall (cl-sml::sml-basis-primitive "String.sub")
                   (list :tuple "a" 1))
          (fail "String.sub did not raise Subscript"))
      (cl-sml::sml-raised-exception (condition)
        (is (string= "Subscript"
                     (cl-sml::sml-exception-name
                      (cl-sml::sml-exception-value condition))))))))

(test runtime-math-functions
  (is (= 12 (cl-sml::sml-abs -12)))
  (is (= 3 (cl-sml::sml-floor 3.8)))
  (is (= 4 (cl-sml::sml-ceil 3.2)))
  (is (= 3 (cl-sml::sml-round 3.2)))
  (is (= 3 (cl-sml::sml-trunc 3.8)))
  (is (= 9.0d0 (cl-sml::sml-real 9)))
  (is (< (abs (- (cl-sml::sml-sqrt 9) 3.0d0)) 1d-9))
  (is (< (abs (cl-sml::sml-sin 0)) 1d-9))
  (is (< (abs (- (cl-sml::sml-cos 0) 1.0d0)) 1d-9))
  (is (< (abs (- (cl-sml::sml-exp 0) 1.0d0)) 1d-9))
  (is (< (abs (- (cl-sml::sml-ln 1) 0.0d0)) 1d-9))
  (is (< (abs (- (cl-sml::sml-arctan 1) (/ pi 4))) 1d-6)))

(test runtime-reference-functions
  (let ((cell (cl-sml::sml-ref 10)))
    (is (= 10 (cl-sml::sml-deref cell)))
    (is (equal '(:tuple) (cl-sml::sml-assign cell 42)))
    (is (= 42 (cl-sml::sml-deref cell)))))

(test runtime-text-io-output-primitives
  (let ((stdout (make-string-output-stream))
        (stderr (make-string-output-stream))
        (output-result nil)
        (output1-result nil)
        (flush-result nil))
    (let ((*standard-output* stdout)
          (*error-output* stderr))
      (setf output-result
            (funcall (cl-sml::sml-basis-primitive "TextIO.output")
                     (list :tuple :text-io-stdout "hello"))
            output1-result
            (funcall (cl-sml::sml-basis-primitive "TextIO.output1")
                     (list :tuple :text-io-stderr #\!))
            flush-result
            (funcall (cl-sml::sml-basis-primitive "TextIO.flushOut")
                     :text-io-stdout)))
    (is (equal '(:tuple) output-result))
    (is (equal '(:tuple) output1-result))
    (is (equal '(:tuple) flush-result))
    (is (string= "hello" (get-output-stream-string stdout)))
    (is (string= "!" (get-output-stream-string stderr)))))

(test runtime-text-io-file-and-input-primitives
  (let* ((open-in (cl-sml::sml-basis-primitive "TextIO.openIn"))
         (close-in (cl-sml::sml-basis-primitive "TextIO.closeIn"))
         (input-n (cl-sml::sml-basis-primitive "TextIO.inputN"))
         (input-line (cl-sml::sml-basis-primitive "TextIO.inputLine"))
         (input-all (cl-sml::sml-basis-primitive "TextIO.inputAll"))
         (end-of-stream (cl-sml::sml-basis-primitive "TextIO.endOfStream"))
         (stream (funcall open-in (asdf:system-relative-pathname "cl-sml" "t/testdata/text-io-input.txt"))))
    (unwind-protect
         (progn
           (is (string= "alpha" (funcall input-n (list :tuple stream 5))))
           (is (string= (string #\Newline) (cdr (funcall input-line stream))))
           (is-false (funcall end-of-stream stream))
           (is (string= (format nil "beta~%last~%")
                        (funcall input-all stream)))
           (is-true (funcall end-of-stream stream))
           (is (eq (cl-sml::sml-none-value) (funcall input-line stream))))
      (when (open-stream-p stream)
        (funcall close-in stream))))
  (let* ((pathname (merge-pathnames "cl-sml-text-io-test.txt"
                                    (uiop:temporary-directory)))
         (open-out (cl-sml::sml-basis-primitive "TextIO.openOut"))
         (open-append (cl-sml::sml-basis-primitive "TextIO.openAppend"))
         (close-out (cl-sml::sml-basis-primitive "TextIO.closeOut"))
         (output (cl-sml::sml-basis-primitive "TextIO.output")))
    (unwind-protect
         (progn
           (let ((stream (funcall open-out pathname)))
             (funcall output (list :tuple stream "first"))
             (funcall close-out stream))
           (let ((stream (funcall open-append pathname)))
             (funcall output (list :tuple stream "+second"))
             (funcall close-out stream))
           (with-open-file (stream pathname :direction :input)
             (is (string= "first+second" (cl-sml::read-sml-source stream)))))
      (when (probe-file pathname)
        (delete-file pathname)))))

(test runtime-os-file-sys-current-directory-primitives
  (let ((original *default-pathname-defaults*)
        (get-dir (cl-sml::sml-basis-primitive "OS.FileSys.getDir"))
        (ch-dir (cl-sml::sml-basis-primitive "OS.FileSys.chDir")))
    (unwind-protect
         (progn
           (funcall ch-dir (namestring (asdf:system-relative-pathname "cl-sml" "t/testdata/")))
           (is (uiop:string-suffix-p (funcall get-dir (cl-sml::sml-unit))
                                     "/testdata/"))
           (with-open-file (stream "text-io-input.txt" :direction :input)
             (is (string= "alpha" (read-line stream)))))
      (setf *default-pathname-defaults* original))))

(test integration-while-expression
  (eval-sml-program
   "val while_result =
      let
        val cell = ref 0;
      in
        while !cell < 3 do cell := !cell + 1;
        !cell
      end;")
  (is (= 3 (sml-value "while_result"))))

(test integration-let-preserves-outer-function-lexicals
  (eval-sml-program
   "fun captureOuter set =
      let
        fun read _ = set;
      in
        read 0
      end;
    val captured_outer = captureOuter 42;")
  (is (= 42 (sml-value "captured_outer"))))

(test integration-local-identifiers-are-case-sensitive
  (eval-sml-program
   "val typed_local_result = let val use : int = 3 in use end;
    val local_recursive_result =
      let fun loop 0 = 0 | loop n = 1 + loop (n - 1) in loop 4 end;
    val local_case_result =
      let
        val yybegin = ref 1;
        val YYBEGIN = fn x => yybegin := x;
      in
        YYBEGIN 7;
        !yybegin
      end;")
  (is (= 3 (sml-value "typed_local_result")))
  (is (= 4 (sml-value "local_recursive_result")))
  (is (= 7 (sml-value "local_case_result"))))

(test runtime-record-and-exception-helpers
  (let* ((record (cl-sml::make-sml-record (list (cons "y" 2)
                                              (cons "x" 1))))
         (nullary (cl-sml::make-sml-exception-constructor "E"))
         (payload-ctor (cl-sml::make-sml-exception-function "FailInt"))
         (payload (funcall payload-ctor 7)))
    (is (cl-sml::sml-record-p record))
    (is (= 1 (cl-sml::sml-record-select record "x")))
    (is (equal '(("x" . 1) ("y" . 2))
               (cl-sml::sml-record-fields record)))
    (is (cl-sml::sml-exception-p nullary))
    (is (string= "E" (cl-sml::sml-exception-name nullary)))
    (is (cl-sml::sml-exception-p payload))
    (is (= 7 (cl-sml::sml-exception-payload payload)))))

(test integration-standard-library-usage
  (eval-sml-program
   "val appended = [1, 2] @ [3, 4];
    val same_list = [1, 2] = [1, 2];
    val different_list = [1, 2] <> [2, 1];
    val folded_left = foldl (fn (x, acc) => acc + x) 0 [1, 2, 3, 4];
    val folded_right = foldr (fn (x, acc) => x + acc) 0 [1, 2, 3, 4];
    val folded_op = foldl (op +) 0 [1, 2, 3, 4];
    val reversed = rev [1, 2, 3];
    val mapped = map (fn x => x + 1) [1, 2, 3];
    val composed = (op o (fn x => x + 1, fn x => x * 2)) 10;
    val before_value = op before (7, 99);
    val joined = concat [\"ab\", \"cd\", str #\"e\"];
    val chars = explode \"hi\";
    val rebuilt = implode [#\"h\", #\"i\"];
    val string_size = size \"hello\";
    val ord_value = ord #\"A\";
    val chr_value = chr 66;
    val magnitude = abs ~12;
    val root = sqrt 9;
    val truncated = trunc 3.8;
    val floored = floor 3.8;
    val ceiled = ceil 3.2;
    val rounded = round 3.2;
    val realed = real 7;
    val pairPlus = fn (x, y) => x + y;
    infix %%;
    val op%% = pairPlus;
    val tupled_infix_alias = 2 %% 3;
    val ref_result =
      let
        val cell = ref 10;
      in
        cell := 42;
        !cell
      end;")
  (is (equal '(1 2 3 4) (sml-value "appended")))
  (is (eq t (sml-value "same_list")))
  (is (eq t (sml-value "different_list")))
  (is (= 10 (sml-value "folded_left")))
  (is (= 10 (sml-value "folded_right")))
  (is (= 10 (sml-value "folded_op")))
  (is (equal '(3 2 1) (sml-value "reversed")))
  (is (equal '(2 3 4) (sml-value "mapped")))
  (is (= 21 (sml-value "composed")))
  (is (= 7 (sml-value "before_value")))
  (is (string= "abcde" (sml-value "joined")))
  (is (equal '(#\h #\i) (sml-value "chars")))
  (is (string= "hi" (sml-value "rebuilt")))
  (is (= 5 (sml-value "string_size")))
  (is (= 65 (sml-value "ord_value")))
  (is (char= #\B (sml-value "chr_value")))
  (is (= 12 (sml-value "magnitude")))
  (is (< (abs (- (sml-value "root") 3.0d0)) 1d-9))
  (is (= 3 (sml-value "truncated")))
  (is (= 3 (sml-value "floored")))
  (is (= 4 (sml-value "ceiled")))
  (is (= 3 (sml-value "rounded")))
  (is (= 7.0d0 (sml-value "realed")))
  (is (= 5 (sml-value "tupled_infix_alias")))
  (is (= 42 (sml-value "ref_result"))))

(test integration-infix-fun-supports-prefix-tuple-call
  (let ((*test-sml-package* "SML.INFIX-FUN-PREFIX-TUPLE-TEST"))
    (eval-sml-program
     "infix plus;
      fun (a, b) plus (c, d) = (a + c, b + d);
      fun pickUpper (T, F, G, E) = E;
      val prefix_tuple_sum = op plus ((1, 2), (3, 4));
      val infix_tuple_sum = (1, 2) plus (3, 4);
      val uppercase_tuple_var = pickUpper (1, 2, 3, 4);")
    (is (equal '(:tuple 4 6)
               (sml-value "prefix_tuple_sum" *test-sml-package*)))
    (is (equal '(:tuple 4 6)
               (sml-value "infix_tuple_sum" *test-sml-package*)))
    (is (= 4 (sml-value "uppercase_tuple_var" *test-sml-package*)))))

(test integration-parenthesized-infix-fun-with-trailing-parameter
  (eval-sml-program
   "infix 5 >-;
    fun (left >- right) scale = (left + right) * scale;
    val parenthesized_infix_result = (2 >- 3) 4;")
  (is (= 20
         (sml-value "parenthesized_infix_result" *test-sml-package*))))

(test integration-open-array-style-sub-infix
  (eval-sml-program
   "structure Arrayish = struct
      fun sub (value, index) = value + index
    end;
    structure SubProbe = struct
      open Arrayish
      infix 9 sub
      val selected = 19 sub 1
    end;")
  (is (= 20 (sml-value "SubProbe.selected" *test-sml-package*))))

(test integration-unspaced-symbolic-infix-function
  (eval-sml-program
   "infix 6 @@;
    fun left@@right = left + right;
    val symbolic_result = 19@@23;")
  (is (= 42 (sml-value "symbolic_result" *test-sml-package*))))

(test integration-patterned-values-and-functions
  (eval-sml-program
   "val (x, y) = (10, 20);
    datatype opt = NONE | SOME of int;
    val SOME z = SOME 9;
    fun swap (a, b) = (b, a);
    fun fact 0 = 1
      | fact n = n * fact (n - 1);
    val seq_value =
      let
        val cell = ref 0;
      in
        cell := x + y;
        !cell
      end;
    val swapped = swap (x, y);
    val factorial_5 = fact 5;")
  (is (= 10 (sml-value "x")))
  (is (= 20 (sml-value "y")))
  (is (= 9 (sml-value "z")))
  (is (equal '(:tuple 20 10) (sml-value "swapped")))
  (is (= 30 (sml-value "seq_value")))
  (is (= 120 (sml-value "factorial_5"))))

(test integration-records-exceptions-and-type-metadata
  (eval-sml-program
   "val point = {y = 2, x = 1};
    val x_coord = #x point;
    val {x = rx, y = ry} = point;
    val {x = rest_x, ...} = {x = 4, y = 5};
    fun swap {x, y} = {x = y, y = x};
    val swapped_point = swap point;
    val point_sum = case point of {x, y} => x + y;
    exception E;
    exception FailInt of int;
    val handled_nullary = ((raise E) handle E => 1 | _ => 0);
    val handled_payload = ((raise (FailInt 7)) handle FailInt n => n | _ => 0);
    val local_payload =
      let
        exception Local of int;
      in
        ((raise (Local 9)) handle Local n => n | _ => 0)
      end;")
  (is (equal '(:record ("x" . 1) ("y" . 2))
             (sml-value "point")))
  (is (= 1 (sml-value "x_coord")))
  (is (= 1 (sml-value "rx")))
  (is (= 2 (sml-value "ry")))
  (is (= 4 (sml-value "rest_x")))
  (is (equal '(:record ("x" . 2) ("y" . 1))
             (sml-value "swapped_point")))
  (is (= 3 (sml-value "point_sum")))
  (is (= 1 (sml-value "handled_nullary")))
  (is (= 7 (sml-value "handled_payload")))
  (is (= 9 (sml-value "local_payload")))
  (is (eq :external (sml-symbol-status "E")))
  (is (eq :external (sml-symbol-status "FailInt")))
  (is (equal '(:record ("x" . "int") ("y" . "int"))
             (cl-sml:lookup-sml-binding-type "point" *test-sml-package*)))
  (is (equal '(:fn "int" "exn")
             (cl-sml:lookup-sml-binding-type "FailInt" *test-sml-package*)))
  (is (string= "{x: int, y: int}"
               (cl-sml:sml-type->string
                (cl-sml:lookup-sml-binding-type "point" *test-sml-package*)))))

(test integration-nonmatching-handler-reraises
  (signals cl-sml::sml-raised-exception
    (eval-sml-expr "let exception E; exception F; in ((raise E) handle F => 1) end")))

(test integration-local-declarations
  (let ((*test-sml-package* "SML.LOCAL-DECL-TEST"))
    (is (= 3 (eval-sml-expr "let val x = 1 and y = 2 in x + y end")))
    (is (= 8
           (eval-sml-expr
            "let local val memo = 7 in val f = fn n => memo + n; end in f 1 end")))
    (eval-sml-program
     "local
        fun helper x = x + 1;
        fun T' (a, b) = a + b;
        datatype opt = NONE | SOME of int;
        exception Hidden;
      in
        val y = helper 2;
        val captured = fn n => helper n;
        val upper_fun = T'(4, 5);
        val picked = case SOME 5 of SOME x => x | NONE => 0;
	    val caught = ((raise Hidden) handle Hidden => 1);
	      end;")
	    (is (= 3 (sml-value "y" *test-sml-package*)))
	    (is (= 5 (funcall (sml-value "captured" *test-sml-package*) 4)))
	    (is (= 9 (sml-value "upper_fun" *test-sml-package*)))
    (is (= 5 (sml-value "picked" *test-sml-package*)))
    (is (= 1 (sml-value "caught" *test-sml-package*)))
    (is (eq :external (sml-symbol-status "y" *test-sml-package*)))
    (is (eq :external (sml-symbol-status "picked" *test-sml-package*)))
    (is (eq :external (sml-symbol-status "caught" *test-sml-package*)))
	    (is (not (eq :external (sml-symbol-status "helper" *test-sml-package*))))
	    (is (not (eq :external (sml-symbol-status "Hidden" *test-sml-package*))))
	    (is (not (eq :external (sml-symbol-status "Hidden" *test-sml-package*))))))

(test integration-local-structure-alias-namespace
  (let ((*test-sml-package* "SML.LOCAL-STRUCTURE-ALIAS-TEST"))
    (eval-sml-program
     "structure AliasSource = struct
        val empty = 37;
      end;
      structure AliasUser = struct
        local
          structure F = AliasSource
        in
          val got = F.empty;
        end;
      end;
      val local_alias_result = AliasUser.got;")
    (is (= 37 (sml-value "local_alias_result" *test-sml-package*)))))

(test integration-opened-constructor-uses-canonical-tag
  (let ((*test-sml-package* "SML.OPENED-CONSTRUCTOR-TEST"))
    (eval-sml-program
     "structure CtorSource = struct
        datatype box = Box of int;
      end;")
    (eval-sml-program
     "structure CtorUser = struct
        open CtorSource;
        fun get (Box n) = n;
      end;
      val opened_ctor_result = CtorUser.get (CtorSource.Box 12);")
    (is (= 12 (sml-value "opened_ctor_result" *test-sml-package*)))))

(test integration-local-open-propagates-constructor-patterns
  (let ((*test-sml-package* "SML.LOCAL-OPEN-CONSTRUCTOR-TEST"))
    (eval-sml-program
     "structure TokenSource = struct
        datatype token = TOKEN of int * (int * int * int);
      end;
      val local_open_left =
        let
          open TokenSource
          val token = TOKEN (19, (11, 0, 5));
          val TOKEN (_, (_, left, _)) = token;
        in
          left
        end;")
    (is (= 0 (sml-value "local_open_left" *test-sml-package*)))))

(test integration-sml-identifiers-remain-case-sensitive
  (let ((*test-sml-package* "SML.CASE-SENSITIVE-SYMBOL-TEST"))
    (eval-sml-program
     "structure Names = struct
        val empty = 17;
        exception Empty;
      end;
      val lower_empty = Names.empty;")
    (let ((lower (sml-symbol "Names.empty"))
          (upper (sml-symbol "Names.Empty")))
      (is (not (null lower)))
      (is (not (null upper)))
      (is (not (eq lower upper)))
      (is (= 17 (symbol-value lower)))
      (is (cl-sml::sml-exception-tag-p (symbol-value upper))))))

(test integration-anonymous-functor-argument-declarations
  (let ((*test-sml-package* "SML.ANONYMOUS-FUNCTOR-ARGUMENT-TEST"))
    (eval-sml-program
     "functor ApplyFn(
        type item;
        val transform : item -> item
      ) =
      struct
        val apply = transform;
      end;
      structure Increment = ApplyFn(
        type item = int;
        fun transform x = x + 1
      );
      val anonymous_functor_result = Increment.apply 41;")
    (is (= 42
           (sml-value "anonymous_functor_result" *test-sml-package*)))
    (eval-sml-program
     "functor NestedApplyFn(
        structure Operations : sig
          val transform : int -> int
        end
      ) =
      struct
        structure Exposed = Operations;
        open Exposed;
        val apply = transform;
      end;
      structure Operations =
      struct
        fun transform x = x + 2;
      end;
      structure NestedIncrement = NestedApplyFn(
        structure Operations = Operations
      );
      val nested_functor_result = NestedIncrement.apply 40;")
    (is (= 42
           (sml-value "nested_functor_result" *test-sml-package*)))
    (is (= 42
           (funcall (sml-value "NestedIncrement.Exposed.transform"
                               *test-sml-package*)
                    40)))))

(test integration-functor-and-result-structure-may-share-a-name
  (let ((*test-sml-package* "SML.SAME-NAME-FUNCTOR-TEST"))
    (eval-sml-program
     "functor Wrapper(
        val transform : int -> int
      ) =
      struct
        val apply = transform;
      end;
      structure Outer =
      struct
        structure Wrapper = Wrapper(
          fun transform x = x + 1
        );
        val result = Wrapper.apply 41;
      end;
      val same_name_functor_result = Outer.result;")
    (is (= 42
           (sml-value "same_name_functor_result" *test-sml-package*)))
    (is (= 42
           (funcall (sml-value "Outer.Wrapper.apply" *test-sml-package*) 41)))))

(test integration-functor-retains-deep-anonymous-structure-members
  (let ((*test-sml-package* "SML.DEEP-FUNCTOR-BINDING-TEST"))
    (eval-sml-program
     "functor ReadFn(
        structure ParserData : sig
          structure EC : sig
            val value : int
          end
        end
      ) =
      struct
        fun read () = ParserData.EC.value;
      end;
      structure Source =
      struct
        structure EC =
        struct
          val value = 42;
        end;
      end;
      structure Holder =
      struct
        structure Source = Source;
      end;
      structure Reader = ReadFn(
        structure ParserData = Holder.Source
      );
      val deep_functor_result = Reader.read ();")
    (is (= 42
           (sml-value "deep_functor_result" *test-sml-package*)))))

(test integration-functor-local-infix-shadows-opened-constructor
  (let ((*test-sml-package* "SML.FUNCTOR-INFIX-SHADOW-TEST"))
    (eval-sml-program
     "structure Annotation =
      struct
        datatype phrase = @@ of string * string;
      end;
      functor Make(val dummy : int) =
      struct
        structure Header =
        struct
          open Annotation;
          infix 6 @@;
          fun left@@right = Annotation.@@(left, right);
          val made = \"use\"@@\"location\";
        end;
      end;
      structure Built = Make(val dummy = 0);"
    )
    (is (functionp (sml-value "Built.Header.@@" *test-sml-package*)))
    (is (equal '(:tuple "use" "location")
               (cdr (sml-value "Built.Header.made" *test-sml-package*))))))

(test runtime-projects-nested-structure-members-from-functor-results
  (let* ((package "SML.RUNTIME-NESTED-MODULE-PROJECTION-TEST")
         (source (cl-sml::sml-symbol-in-package-name
                  "Root.Child.value" package)))
    (setf (symbol-value source) 42)
    (cl-sml::register-sml-structure-members package "Root"
                                             '("Child.value"))
    (is (equal '("value")
               (cl-sml::lookup-sml-structure-members package "Root.Child")))
    (cl-sml::alias-sml-structure-alias package "Alias" "Root.Child")
    (is (= 42 (cl-sml:sml-value "Alias.value" package)))))

(test integration-val-rec-and-symbol-export
  (eval-sml-program
   "val rec fact = fn 0 => 1 | n => n * fact (n - 1);
    val rec sumTo = fn 0 => 0 | n => n + sumTo (n - 1);
    fun makeUpper O = O;
    val upper_pattern_value = makeUpper 12;")
  (is (= 120 (funcall (sml-value "fact") 5)))
  (is (= 15 (funcall (sml-value "sumTo") 5)))
  (is (= 12 (sml-value "upper_pattern_value")))
  (is (eq :external (sml-symbol-status "fact")))
  (is (eq :external (sml-symbol-status "sumTo"))))

(test integration-default-user-package-namespace
  (eval-sml-program
   "val namespace_value = 99;
    datatype namespace_option = NamespaceNone | NamespaceSome of int;")
  (is (= 99 (sml-value "namespace_value")))
  (is (eq :external (sml-symbol-status "namespace_value")))
  (is (eq :external (sml-symbol-status "NamespaceNone")))
  (is (eq :external (sml-symbol-status "NamespaceSome")))
  (is (string= "SML-USER" (package-name (symbol-package (sml-symbol "namespace_value"))))))

(test reader-block-targets-current-package-derived-sml-package
  (let* ((host-package (or (find-package "CL-SML-READER-TEMP")
                           (make-package "CL-SML-READER-TEMP" :use '("COMMON-LISP"))))
         (sml-package-name "SML.CL-SML-READER-TEMP"))
    (let ((*package* host-package)
          (*readtable* (named-readtables:find-readtable 'cl-sml:sml-readtable)))
      (eval (read-from-string "#{
        val reader_value = 77;
      }#")))
    (is (= 77 (sml-value "reader_value" sml-package-name)))
    (is (eq :external (sml-symbol-status "reader_value" sml-package-name)))))

(test load-actual-sml-file
  (multiple-value-bind (package result)
      ;; The package is named after the path relative to the project.
      (let ((*default-pathname-defaults* (asdf:system-source-directory "cl-sml")))
        (cl-sml:load-sml-file #P"t/testdata/sample-program.sml"))
    (declare (ignore result))
    (let ((package-name (package-name package)))
      (is (string= "SML.FILE.T.TESTDATA.SAMPLE-PROGRAM" package-name))
      (is (= 11 (sml-value "file_x" package-name)))
      (is (= 31 (sml-value "file_y" package-name)))
      (is (= 120 (sml-value "file_result" package-name)))
      (is (string= "done" (sml-value "file_comment_ok" package-name)))
      (is (eq :external (sml-symbol-status "file_result" package-name))))))

(test load-sml-use-resolves-relative-files
  (multiple-value-bind (package result)
      (cl-sml:load-sml-file (asdf:system-relative-pathname "cl-sml" "t/testdata/use-main.sml")
                            :package "SML.USE-TEST")
    (declare (ignore result))
    (let ((package-name (package-name package)))
      (is (= 41 (sml-value "used_child" package-name)))
      (is (= 42 (sml-value "used_main" package-name))))))

(test load-basis-prefix-files-into-a-user-package
  (let ((package-name "SML.HAMLET-BASIS-SMOKE"))
    (cl-sml:load-sml-file (asdf:system-relative-pathname "cl-sml" "src/basis/infix.sml") :package package-name)
    (cl-sml:load-sml-file (asdf:system-relative-pathname "cl-sml" "src/basis/types.sml") :package package-name)
    (cl-sml:load-sml-file (asdf:system-relative-pathname "cl-sml" "src/basis/exceptions.sml") :package package-name)
    (let ((some-symbol (cl-sml::sml-symbol "SOME" package-name)))
      (is (not (eq some-symbol 'cl:some)))
      (is (eq (symbol-package some-symbol)
              (find-package package-name))))
    (is (eq (sml-value "NONE" package-name)
            (cl-sml::sml-symbol "NONE" package-name)))
    (is (functionp (sml-value "SOME" package-name)))
    (is (eq :external (sml-symbol-status "LESS" package-name)))
    (is (cl-sml::sml-exception-p (sml-value "Bind" package-name)))
    (is (functionp (sml-value "Fail" package-name)))
    (is (equal '(:fn "string" "exn")
               (cl-sml:lookup-sml-binding-type "Fail" package-name)))
    (is (string= "int" (cl-sml::lookup-sml-type-alias "int" package-name)))))

(test common-lisp-sml-interop-api
  (eval-sml-program "val interop_answer = 42; fun interop_add x y = x + y;")
  (setf (cl-sml:sml-value "interop_mutable" *test-sml-package*) 9)
  (is (= 42 (cl-sml:sml-value "interop_answer" *test-sml-package*)))
  (is (functionp (cl-sml:sml-function "interop_add" *test-sml-package*)))
  (is (= 7 (cl-sml:with-sml-package (*test-sml-package*)
             (cl-sml:call-sml "interop_add" 3 4))))
  (is (= 9 (cl-sml:sml-value "interop_mutable" *test-sml-package*))))

(test integration-large-literal-case
  (let ((branches
          (with-output-to-string (stream)
            (loop for i below 180
                  do (format stream "~:[~; | ~]~D => ~D"
                             (plusp i) i (* i i)))
            (write-string " | _ => ~1" stream))))
    (eval-sml-program
     (format nil "fun dispatch n = case n of ~A; val dispatched = dispatch 173;"
             branches))
    (is (= (* 173 173) (sml-value "dispatched")))))

(test integration-large-tuple-dispatch-case
  (let ((branches
          (with-output-to-string (stream)
            (loop for i below 24
                  do (format stream "~:[~; | ~](~D, x) => x + ~D"
                             (plusp i) i i))
            (write-string " | _ => ~1" stream))))
    (eval-sml-program
     (format nil "fun tupleDispatch pair = case pair of ~A;~%~
                  val tupleDispatched = tupleDispatch(17, 25);"
             branches))
    (is (= 42 (sml-value "tupleDispatched")))))

(test fun-clause-result-type-and-capitalized-parameters
  (eval-sml-program
   "fun lex () : int = 7;
    fun cat J (atp as (a, b)) = J + a + b;
    val lexed = lex ();
    val catted = cat 1 (2, 3);")
  (is (= 7 (sml-value "lexed")))
  (is (= 6 (sml-value "catted"))))

(test local-mutually-recursive-fun-group
  (eval-sml-program
   "fun outer n =
      let
        fun even m = if m = 0 then true else odd' (m - 1)
        and odd' m = if m = 0 then false else even (m - 1)
      in even n end;
    val evenTen = outer 10;")
  (is (eq t (sml-value "evenTen"))))

(test op-binary-operators-take-pairs
  (eval-sml-program
   "fun foldr' f b nil = b | foldr' f b (x::r) = f (x, foldr' f b r);
    val consed = foldr' (op ::) [] [1, 2, 3];
    val summed = foldr' (op +) 0 [1, 2, 3];
    val applied = (op +) (1, 2);")
  (is (equal '(1 2 3) (sml-value "consed")))
  (is (= 6 (sml-value "summed")))
  (is (= 3 (sml-value "applied"))))

(test before-binds-looser-than-assignment
  (eval-sml-program
   "val cell = ref 0;
    val beforeResult = (10 before cell := 7);
    val beforeCell = !cell;")
  (is (= 10 (sml-value "beforeResult")))
  (is (= 7 (sml-value "beforeCell"))))

(test symbolic-operators-are-maximal-munch
  (eval-sml-program
   "infixr ^/^
    fun a ^/^ b = a ^ \"/\" ^ b;
    val joined = \"x\" ^/^ \"y\" ^/^ \"z\";")
  (is (equal "x/y/z" (sml-value "joined"))))

(test cons-pattern-with-looser-infix-constructor
  (eval-sml-program
   "infix @@
    datatype 'a ann = @@ of 'a * int;
    fun headOf (x :: r @@ _) = x | headOf _ = 0;
    val headed = headOf ([4, 5] @@ 9);")
  (is (= 4 (sml-value "headed"))))

(test opened-sealed-structure-does-not-leak-hidden-constructors
  (eval-sml-program
   "signature PP = sig type mode val f : int -> int end;
    structure P :> PP = struct
      datatype mode = H | V | F | A
      fun f x = x + 1
    end;
    structure Q = struct
      open P
      fun ppF F = F + 1
    end;
    val unleaked = Q.ppF 41;")
  (is (= 42 (sml-value "unleaked"))))

(test opened-sealed-structure-does-not-leak-hidden-values
  (eval-sml-program
   "structure Hidden = struct fun size x = 99 end;
    signature SHOWN = sig val shown : int -> int end;
    structure Shown : SHOWN = struct
      open Hidden
      fun shown x = x + 1
    end;
    fun size l = 7;
    structure User = struct
      open Shown
      val sized = size [1, 2]
    end;
    val leakChecked = User.sized;")
  (is (= 7 (sml-value "leakChecked"))))

(test constructor-pattern-through-functor-parameter
  (eval-sml-program
   "structure FnTable = struct
      datatype term = T of int
    end;
    signature FN_TOKEN = sig
      structure FnTable : sig datatype term = T of int end
    end;
    structure FnToken = struct
      structure FnTable = FnTable
    end;
    functor ShowFn(structure Tok : FN_TOKEN) = struct
      structure Table = Tok.FnTable
      open Table
      val show = fn (T 0) => \"zero\" | (T 1) => \"one\" | _ => \"other\"
    end;
    structure Shown = ShowFn(structure Tok = FnToken);
    val shownOne = Shown.show (FnTable.T 1);")
  (is (equal "one" (sml-value "shownOne"))))

(test word-literal-negation-is-modular
  (eval-sml-program "val maxWord = ~(0w1); val sixteen = 0w16;")
  (is (= cl-sml::+sml-word-mask+ (sml-value "maxWord")))
  (is (= 16 (sml-value "sixteen"))))

(test real-basis-primitives
  (is (equal "3.14" (cl-sml::sml-real-to-string 3.14d0)))
  (is (equal "10000000000.0" (cl-sml::sml-real-to-string 1d10)))
  (is (equal "1E22" (cl-sml::sml-real-to-string 1d22)))
  (is (equal "0.333333333333" (cl-sml::sml-real-to-string (/ 1d0 3d0))))
  (is (equal "123456789.0" (cl-sml::sml-real-to-string 123456789d0)))
  (is (equal "1E~5" (cl-sml::sml-real-to-string 1d-5)))
  (is (equal "~0.0" (cl-sml::sml-real-to-string -0d0)))
  (is (equal "3.142E0" (cl-sml::sml-real-fmt :sci 3 3.14159d0)))
  (is (equal "3.14" (cl-sml::sml-real-fmt :fix 2 3.14159d0)))
  (is (equal "2" (cl-sml::sml-real-fmt :fix 0 2.5d0)))
  (is (equal "0.314159E1" (cl-sml::sml-real-fmt :exact 0 3.14159d0)))
  (is (equal "~2.5" (cl-sml::sml-real-to-string -2.5d0)))
  (is (equal "0.1" (cl-sml::sml-real-to-string 0.1d0)))
  (is (equal "~0.25" (cl-sml::sml-real-to-string -0.25d0)))
  (is (= 4 (funcall (cl-sml::sml-basis-primitive "Real.round") 3.5d0)))
  (is (= 2 (funcall (cl-sml::sml-basis-primitive "Real.round") 2.5d0))))

;;; Results must not depend on the host Common Lisp.

(test self-tail-calls-do-not-grow-the-stack
  ;; ECL and ABCL do not merge tail calls: without the compiler's loop these
  ;; recursions overflow their stacks.  ABCL interprets EVALed code, expanding
  ;; macros on every call, so the counts are kept small: its interpreter runs
  ;; out of stack at about a thousand nested SML calls.
  (eval-sml-program
   "fun tailCount 0 acc = acc | tailCount n acc = tailCount (n - 1) (acc + 1);
    val tailCounted = tailCount 20000 0;
    exception TailStop of int;
    fun tailHandler n =
      (raise TailStop n) handle TailStop 0 => 0 | TailStop k => tailHandler (k - 1);
    val tailHandled = tailHandler 5000;
    fun tailOrelse n = n = 0 orelse tailOrelse (n - 1);
    val tailOrelseResult = tailOrelse 20000;")
  (is (= 20000 (sml-value "tailCounted")))
  (is (= 0 (sml-value "tailHandled")))
  (is (eq t (sml-value "tailOrelseResult"))))

(test self-tail-calls-in-compiled-files
  (let ((package-name "SML.TAIL-CALL-TEST"))
    (cl-sml:load-sml-file (asdf:system-relative-pathname "cl-sml" "t/testdata/tail-calls.sml")
                          :package package-name)
    (is (= 1000000 (sml-value "counted" package-name)))
    (is (= 500000500000 (sml-value "summed" package-name)))
    (is (string= "done" (sml-value "countedDown" package-name)))))

(test self-tail-calls-keep-partial-applications-and-closures-apart
  (eval-sml-program
   "fun tailFrom 0 acc = acc | tailFrom n acc = tailFrom (n - 1) (acc + 1);
    val tailFrom3 = tailFrom 3;
    val tailPartial = (tailFrom3 0, tailFrom3 10);
    fun tailThunks 0 acc = acc
      | tailThunks n acc = tailThunks (n - 1) ((fn () => n) :: acc);
    val tailThunkValues = map (fn f => f ()) (tailThunks 3 []);
    fun tailShadow 0 = 0
      | tailShadow n = let fun tailShadow m = m * 100 in tailShadow n end;
    val tailShadowed = tailShadow 3;
    fun tailGuarded 0 = 0 | tailGuarded n = tailGuarded (n - 1) handle Div => ~1;
    val tailGuardedResult = tailGuarded 100;")
  (is (equal '(:tuple 3 13) (sml-value "tailPartial")))
  (is (equal '(1 2 3) (sml-value "tailThunkValues")))
  (is (= 300 (sml-value "tailShadowed")))
  (is (= 0 (sml-value "tailGuardedResult"))))

(test word-and-int-limits-do-not-depend-on-the-host
  (flet ((primitive (name &rest args)
           (apply (cl-sml::sml-basis-primitive name) args)))
    (is (= 62 (primitive "Word.wordSize")))
    (is (= (1- (expt 2 62)) (cdr (primitive "Int.maxInt"))))
    (is (= (- (expt 2 62)) (cdr (primitive "Int.minInt"))))
    (is (= (1- (expt 2 62)) (primitive "Word.fromInt" -1)))
    (is (= -1 (primitive "Word.toIntX" (1- (expt 2 62)))))
    (is (= (1- (expt 2 62)) (primitive "Word.notb" 0)))
    (is (= 0 (primitive "Word.<<" (list :tuple 1 62))))
    (is (= (expt 2 61) (primitive "Word.<<" (list :tuple 1 61))))
    (is (= 1 (primitive "Word.>>" (list :tuple (expt 2 61) 61))))
    (is (= (1- (expt 2 62)) (primitive "Word.~>>" (list :tuple (expt 2 61) 61))))
    (is (= -1 (primitive "Word8.toIntX" 255)))
    (is (= (1- (expt 2 62)) (primitive "Word8.toLargeX" 255)))
    (is (= #xfe (primitive "Word8.<<" (list :tuple #xff 1))))
    (is (= #xc0 (primitive "Word8.~>>" (list :tuple #x80 1))))))

(test char-chr-raises-chr-outside-8-bit-codes
  (is (char= (code-char 255) (cl-sml::sml-chr 255)))
  (dolist (code (list -1 256 #x10000))
    (handler-case
        (progn (cl-sml::sml-chr code)
               (fail "chr ~D did not raise Chr" code))
      (cl-sml::sml-raised-exception (condition)
        (is (string= "Chr"
                     (cl-sml::sml-exception-name
                      (cl-sml::sml-exception-value condition))))))))

(test real-arithmetic-is-ieee-on-every-host
  ;; SBCL and ECL trap by default; ABCL traps only in EXP and EXPT.
  (let ((nan (cl-sml::sml-nan))
        (infinity (cl-sml::sml-real-infinity nil))
        (negative-infinity (cl-sml::sml-real-infinity t)))
    (is (cl-sml::sml-real-nan-p (cl-sml::sml-real-quotient 0d0 0d0)))
    (is (eql infinity (cl-sml::sml-real-quotient 1d0 0d0)))
    (is (eql negative-infinity (cl-sml::sml-real-quotient 1d0 -0d0)))
    (is (eql infinity (cl-sml::sml-* most-positive-double-float 2d0)))
    (is (eql negative-infinity
             (cl-sml::sml-- (- most-positive-double-float) most-positive-double-float)))
    (is (cl-sml::sml-real-nan-p (cl-sml::sml-sqrt -1d0)))
    (is (eql negative-infinity (cl-sml::sml-ln 0d0)))
    (is (eql infinity (cl-sml::sml-exp 1000d0)))
    (is (eql infinity (funcall (cl-sml::sml-basis-primitive "Math.pow")
                               (list :tuple 10d0 400d0))))
    (is (eql infinity (cl-sml::sml-real (expt 10 400))))
    ;; Comparisons with a NaN are false.
    (is (null (cl-sml::sml-< nan 1d0)))
    (is (null (cl-sml::sml->= nan 1d0)))
    (is (null (cl-sml::sml-<= 1d0 nan)))
    (is (null (cl-sml::sml-real->= nan 1d0)))
    (eval-sml-program "val nanOrdered = let val n = 0.0 / 0.0
                                        in n < 1.0 orelse n >= 1.0 end;")
    (is (null (sml-value "nanOrdered")))
    (is (null (funcall (cl-sml::sml-basis-primitive "Real.==") (list :tuple nan nan))))
    (is (eq t (funcall (cl-sml::sml-basis-primitive "Real.?=") (list :tuple nan 1d0))))
    (is (string= "nan" (cl-sml::sml-real-to-string nan)))
    (flet ((raised (function value)
             (handler-case (progn (funcall (cl-sml::sml-basis-primitive function) value) nil)
               (cl-sml::sml-raised-exception (condition)
                 (cl-sml::sml-exception-name (cl-sml::sml-exception-value condition))))))
      (is (equal "Domain" (raised "Real.trunc" nan)))
      (is (equal "Overflow" (raised "Real.floor" infinity))))
    (is (string= "~inf" (cl-sml::sml-real-to-string negative-infinity)))))

(test real-to-string-digits-are-exact-and-host-independent
  ;; The digits come from integer arithmetic, not from the host's FORMAT or
  ;; reader, whose float conversions differ between implementations.
  (flet ((digits (q)
           ;; Not literals: ECL's reader is off by an ulp for some of them.
           (multiple-value-list
            (cl-sml::portable-flonum-to-digits
             (if (floatp q) q (cl-sml::portable-rational-to-double q))))))
    (is (equal '(0 "1") (digits 1/10)))
    (is (equal '(0 "3333333333333333") (digits 1/3)))
    (is (equal '(23 "1") (digits (expt 10 22))))
    (is (equal '(24 "1") (digits (expt 10 23))))
    (is (equal '(-323 "5") (digits least-positive-double-float)))
    (is (equal '(-307 "22250738585072014") (digits least-positive-normalized-double-float)))
    (is (equal '(309 "17976931348623157") (digits most-positive-double-float))))
  (is (equal "4.94065645841E~324" (cl-sml::sml-real-to-string least-positive-double-float)))
  ;; Each result lies within half an ulp of the value, so it reads back as
  ;; the same double.
  (let ((state 42)
        (misses 0))
    (dotimes (i 2000)
      (setf state (ldb (byte 64 0) (+ (* state 6364136223846793005) 1442695040888963407)))
      (let* ((exponent (ldb (byte 11 52) state))
             (fraction (ldb (byte 52 0) state)))
        (unless (= exponent 2047)
          (let* ((exact (if (zerop exponent)
                            (* fraction (expt 2 -1074))
                            (* (+ fraction (expt 2 52)) (expt 2 (- exponent 1075)))))
                 (value (coerce exact 'double-float))
                 (ulp (expt 2 (max -1074 (nth-value 1 (integer-decode-float value))))))
            (when (plusp exact)
              (multiple-value-bind (k digits) (cl-sml::portable-flonum-to-digits value)
                (let ((decimal (* (parse-integer digits)
                                  (expt 10 (- k (length digits))))))
                  (unless (<= (abs (- decimal (rational value))) (/ ulp 2))
                    (incf misses)))))))))
    (is (= 0 misses))))

(test rational-to-double-rounds-exactly
  (flet ((exact (q) (rational (cl-sml::portable-rational-to-double q))))
    (is (= 3602879701896397/36028797018963968 (exact 1/10)))
    (is (= 99999999999999991611392 (exact (expt 10 23))))
    (is (= (* 2024 (expt 2 -1074)) (exact (/ 1 (expt 10 320)))))
    (is (= -1/2 (exact -1/2)))
    ;; Ties go to the even neighbour.
    (is (= (expt 2 53) (exact (1+ (expt 2 53)))))
    (is (= (+ (expt 2 53) 4) (exact (+ (expt 2 53) 3))))
    (is (null (cl-sml::portable-rational-to-double (expt 10 400))))
    (is (eql (cl-sml::sml-real-infinity t) (cl-sml::sml-real (- (expt 10 400)))))))
