(defpackage #:cl-sml-runtime-tests
  (:use #:cl #:fiveam #:cl-sml)
  (:shadow #:sml-value)
  (:import-from #:esrap #:parse))

(in-package #:cl-sml-runtime-tests)

(def-suite cl-sml-runtime-suite
  :description "Runtime and integration tests for cl-sml")

(in-suite cl-sml-runtime-suite)

(defparameter *test-sml-package* "SML-USER")

(defun eval-sml-program (source)
  (eval (cl-sml:compile-sml-program-string source :package *test-sml-package*)))

(defun eval-sml-expr (source)
  (let ((cl-sml::*sml-package* (cl-sml::ensure-sml-package *test-sml-package*)))
    (eval (cl-sml::compile-expr (parse 'cl-sml::sml-expr source)))))

(defun sml-symbol (name &optional (package *test-sml-package*))
  (find-symbol (string-upcase name) package))

(defun sml-value (name &optional (package *test-sml-package*))
  (symbol-value (sml-symbol name package)))

(defun sml-symbol-status (name &optional (package *test-sml-package*))
  (nth-value 1 (find-symbol (string-upcase name) package)))

(test runtime-list-and-equality-functions
  (is (equal '(1 2 3 4)
             (funcall (funcall #'cl-sml::sml-@ '(1 2)) '(3 4))))
  (is (eql 10 (cl-sml::sml-hd '(10 20 30))))
  (is (equal '(20 30) (cl-sml::sml-tl '(10 20 30))))
  (is (eql 3 (cl-sml::sml-length '(a b c))))
  (is (eq t (cl-sml::sml-null nil)))
  (is (equal '(3 2 1) (cl-sml::sml-rev '(1 2 3))))
  (is (eq t (funcall (funcall #'cl-sml::sml-= '(1 2)) '(1 2))))
  (is (eq t (funcall (funcall #'cl-sml::sml-<> '(1 2)) '(2 1)))))

(test runtime-higher-order-functions
  (is (equal '(2 3 4)
             (funcall (cl-sml::sml-map (lambda (x) (+ x 1))) '(1 2 3))))
  (is (= 6
         (funcall (funcall (cl-sml::sml-foldl #'cl-sml::sml-+) 0) '(1 2 3))))
  (is (= 6
         (funcall (funcall (cl-sml::sml-foldr #'cl-sml::sml-+) 0) '(1 2 3))))
  (is (= 21
         (funcall (cl-sml::sml-before 21) 99)))
  (is (= 21
         (funcall (funcall (cl-sml::sml-o (lambda (x) (+ x 1)))
                           (lambda (x) (* x 2)))
                  10)))
  (let ((wrapped (cl-sml::sml-tuple-or-curried-binary
                  (lambda (x)
                    (lambda (y)
                      (+ x y))))))
    (is (= 5 (funcall wrapped (list :tuple 2 3))))
    (is (= 5 (funcall (funcall wrapped 2) 3)))))

(test runtime-string-and-char-functions
  (is (string= "abcd" (cl-sml::sml-concat '("ab" "cd"))))
  (is (= 4 (cl-sml::sml-size "test")))
  (is (equal '(#\a #\b) (cl-sml::sml-explode "ab")))
  (is (string= "ab" (cl-sml::sml-implode '(#\a #\b))))
  (is (= 65 (cl-sml::sml-ord #\A)))
  (is (char= #\A (cl-sml::sml-chr 65)))
  (is (string= "Z" (cl-sml::sml-str #\Z)))
  (is (eq t (funcall (funcall #'cl-sml::sml-< #\A) #\B)))
  (is (eq t (funcall (funcall #'cl-sml::sml-< "ab") "ac")))
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
    (is (equal '(:tuple) (funcall (cl-sml::sml-assign cell) 42)))
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
    val folded_left = foldl (fn acc => fn x => acc + x) 0 [1, 2, 3, 4];
    val folded_right = foldr (fn x => fn acc => x + acc) 0 [1, 2, 3, 4];
    val reversed = rev [1, 2, 3];
    val mapped = map (fn x => x + 1) [1, 2, 3];
    val composed = (o (fn x => x + 1) (fn x => x * 2)) 10;
    val before_value = before 7 99;
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
     "fun (a, b) plus (c, d) = (a + c, b + d);
      fun pickUpper (T, F, G, E) = E;
      val prefix_tuple_sum = plus((1, 2), (3, 4));
      val uppercase_tuple_var = pickUpper (1, 2, 3, 4);")
    (is (equal '(:tuple 4 6)
               (sml-value "prefix_tuple_sum" *test-sml-package*)))
    (is (= 4 (sml-value "uppercase_tuple_var" *test-sml-package*)))))

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
    (eval-sml-program
     "local
        fun helper x = x + 1;
        fun T' (a, b) = a + b;
        datatype opt = NONE | SOME of int;
        exception Hidden;
      in
        val y = helper 2;
        val upper_fun = T'(4, 5);
        val picked = case SOME 5 of SOME x => x | NONE => 0;
	    val caught = ((raise Hidden) handle Hidden => 1);
	      end;")
	    (is (= 3 (sml-value "y" *test-sml-package*)))
	    (is (= 9 (sml-value "upper_fun" *test-sml-package*)))
    (is (= 5 (sml-value "picked" *test-sml-package*)))
    (is (= 1 (sml-value "caught" *test-sml-package*)))
    (is (eq :external (sml-symbol-status "y" *test-sml-package*)))
    (is (eq :external (sml-symbol-status "picked" *test-sml-package*)))
    (is (eq :external (sml-symbol-status "caught" *test-sml-package*)))
	    (is (not (eq :external (sml-symbol-status "helper" *test-sml-package*))))
	    (is (not (eq :external (sml-symbol-status "Hidden" *test-sml-package*))))
	    (is (not (eq :external (sml-symbol-status "SOME" *test-sml-package*))))
	    (is (not (eq :external (sml-symbol-status "NONE" *test-sml-package*))))))

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
      (cl-sml:load-sml-file #P"testdata/sample-program.sml")
    (declare (ignore result))
    (let ((package-name (package-name package)))
      (is (string= "SML.FILE.TESTDATA.SAMPLE-PROGRAM" package-name))
      (is (= 11 (sml-value "file_x" package-name)))
      (is (= 31 (sml-value "file_y" package-name)))
      (is (= 120 (sml-value "file_result" package-name)))
      (is (string= "done" (sml-value "file_comment_ok" package-name)))
      (is (eq :external (sml-symbol-status "file_result" package-name))))))

(test load-sml-use-resolves-relative-files
  (multiple-value-bind (package result)
      (cl-sml:load-sml-file #P"testdata/use-main.sml"
                            :package "SML.USE-TEST")
    (declare (ignore result))
    (let ((package-name (package-name package)))
      (is (= 41 (sml-value "used_child" package-name)))
      (is (= 42 (sml-value "used_main" package-name))))))

(test load-hamlet-basis-prefix-files
  (let ((package-name "SML.HAMLET-BASIS-SMOKE"))
    (cl-sml:load-sml-file #P"hamlet/basis/infix.sml" :package package-name)
    (cl-sml:load-sml-file #P"hamlet/basis/types.sml" :package package-name)
    (cl-sml:load-sml-file #P"hamlet/basis/exceptions.sml" :package package-name)
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

(fiveam:run! 'cl-sml-runtime-suite)
