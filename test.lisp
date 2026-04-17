(ql:quickload :cl-sml)
(in-package :cl-user)
(named-readtables:in-readtable cl-sml:sml-readtable)

(defun sml-user-symbol (name)
  (find-symbol (string-upcase name) "SML-USER"))

(defun sml-user-value (name)
  (symbol-value (sml-user-symbol name)))

;; 1. Embed SML code directly in your Lisp file!
(progn
#{
  val x = 10;

  fun add a b = a + b;

  val result = add x 5;

  fun maybeAddfoo opt =
    case opt of
        SOME foo => add foo 1
      | NONE => 0;
}#
)

(progn
#{
  fun isSafe x =
    if x > 0 andalso x < 10 then
        true
    else
        false;
}#
)
;; 2. SML functions compile to Lisp globals and use Funcall
(format t "Result is: ~A~%" (sml-user-value "result")) ;; Prints 15

;; 3. Let's call the SML function `maybeAddfoo` directly from Lisp!
;; We use our exported struct constructor to pass it an SML 'SOME' value.

;; FIXME: FAILS.
;; (let ((my-opt (cl-sml:make-sml-adt 'cl-sml::some 100))) ;; Use :SOME
;;   (format t "maybeAddfoo(SOME 100) = ~A~%"
;;           (funcall cl-sml::maybeAddfoo my-opt)))

;; (format t "maybeAddfoo(NONE) = ~A~%"
;;         (funcall cl-sml::maybeAddfoo 'cl-sml::NONE)) ;; Use :NONE

(let ((my-opt (cons (sml-user-symbol "SOME") 100)))
  (format t "maybeAddfoo(SOME 100) = ~A~%"
          (funcall (sml-user-value "maybeAddfoo") my-opt)))

(format t "maybeAddfoo(NONE) = ~A~%"
        (funcall (sml-user-value "maybeAddfoo") (sml-user-symbol "NONE")))

(format t "isSafe(5) = ~A~%"
           (funcall (sml-user-value "isSafe") 5))

(progn
#{
  val letResult =
    let
      val a = 10;
      val b = 5;
      fun multiply x y = x * y;
    in
      multiply a b
    end;
}#
)

;; Proof that the let block compiled correctly and evaluated!
(format t "letResult = ~A~%" (sml-user-value "letResult")) ;; Should print 50

;; Proof of Lexical Scoping:
;; 'a', 'b', and 'multiply' were strictly local to the `let` block.
;; Trying to access them here would result in an UNBOUND-VARIABLE error!

(progn
#{
  fun sumList lst =
    case lst of
        [] => 0
      | head :: tail => head + sumList tail;

  val my_list = [10, 20, 30];
  val list_total = sumList my_list;
}#
)

(format t "List to sum: ~A~%" (sml-user-value "my_list"))
(format t "sumList([10, 20, 30]) = ~A~%" (sml-user-value "list_total")) ;; Should print 60!

;; We can even call it with a native Lisp list because SML lists ARE Lisp lists!
(format t "sumList( '(1 2 3 4 5) ) = ~A~%"
        (funcall (sml-user-value "sumList") '(1 2 3 4 5))) ;; Should print 15

(progn
#{
  datatype sml_option = SML_NONE | SML_SOME of int;

  fun maybeAdd opt =
    case opt of
        SML_NONE => 0
      | SML_SOME v => v + 1;

  val test_none = maybeAdd SML_NONE;
  val test_some = maybeAdd (SML_SOME 99);
}#
)


(format t "maybeAdd(SML_NONE) = ~A~%" (sml-user-value "test_none"))       ;; Should print 0
(format t "maybeAdd(SML_SOME 99) = ~A~%" (sml-user-value "test_some"))    ;; Should print 100


(progn
#{
  fun smlMap f lst =
    case lst of
        [] => []
      | head :: tail => (f head) :: (smlMap f tail);

  val nums = [1, 2, 3, 4, 5];


  val squaredNums = smlMap (fn x => x * x) nums;
}#
)
  ;; (* We use 'fn' here to square the numbers *)
(format t "Original: ~A~%" (sml-user-value "nums"))
(format t "Squared:  ~A~%" (sml-user-value "squaredNums")) ;; Should print (1 4 9 16 25)

(progn
#{
  val pair = (10, 20);
  val unitValue = ();
  val swapped = (fn (x, y) => (y, x)) pair;
  val tuple_sum =
    case pair of
        (x, y) => x + y;
}#
)

(format t "pair = ~A~%" (sml-user-value "pair"))
(format t "unitValue = ~A~%" (sml-user-value "unitValue"))
(format t "swapped = ~A~%" (sml-user-value "swapped"))
(format t "tuple_sum = ~A~%" (sml-user-value "tuple_sum"))
