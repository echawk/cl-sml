(defpackage #:cl-sml-reader-tests
  (:use #:cl #:fiveam #:cl-sml))

(in-package #:cl-sml-reader-tests)

(def-suite cl-sml-reader-suite
  :description "Embedded SML blocks: #@sml{ ... }#")

(in-suite cl-sml-reader-suite)

(defun read-with-sml (string)
  "Read STRING in this package with SML syntax enabled."
  (let ((*readtable* (enable-sml-syntax (copy-readtable nil)))
        (*package* (find-package '#:cl-sml-reader-tests)))
    (read-from-string string)))

(defun eval-with-sml (string)
  (eval (read-with-sml string)))

(defun sml-name (name)
  "The Lisp variable of the SML binding NAME of this package's blocks."
  (symbol-value (find-symbol name "SML.CL-SML-READER-TESTS")))

(test sharp-brace-is-left-alone
  ;; Only #@sml{ is ours; #{ stays free for other reader macros.
  (signals reader-error (read-with-sml "#{ 1 }#")))

(test expression-block
  (is (= 7 (eval-with-sml "(+ 1 #@sml{ 2 * 3 }#)")))
  (is (equal "ab" (eval-with-sml "#@sml{ \"a\" ^ \"b\" }#")))
  (is (equal '(2 3) (eval-with-sml "#@sml{ map (fn x => x + 1) [1, 2] }#"))))

(test declaration-block
  (eval-with-sml "#@sml{ fun reader_add a b = a + b
                     val reader_x = reader_add 2 3 }#")
  (is (= 5 (sml-name "reader_x")))
  (is (= 7 (funcall (funcall (sml-name "reader_add") 3) 4)))
  ;; Later blocks see the bindings.
  (is (= 10 (eval-with-sml "#@sml{ reader_x * 2 }#"))))

(test declarations-then-expression
  (is (= 42 (eval-with-sml "#@sml{ val reader_y = 40; fun reader_inc n = n + 1; reader_inc (reader_y + 1) }#"))))

(test datatype-across-blocks
  (eval-with-sml "#@sml{ datatype reader_t = RA of int | RB }#")
  (is (= 3 (eval-with-sml "#@sml{ case RA 3 of RA n => n | RB => 0 }#"))))

(test splices
  (is (= 11 (eval-with-sml "(let ((x 10)) #@sml{ $x + 1 }#)")))
  (is (= 12 (eval-with-sml "(let ((x 3)) #@sml{ ($x : int) * $(+ x 1) }#)")))
  (is (equal '(1 4 9) (eval-with-sml "#@sml{ map (fn x => x * x) $(list 1 2 3) }#")))
  ;; Splices are evaluated each time, in the block's lexical environment.
  (let ((f (eval-with-sml "(lambda (k) #@sml{ fn x => x + $k }#)")))
    (is (= 15 (funcall (funcall f 5) 10)))
    (is (= 105 (funcall (funcall f 100) 5)))))

(test strings-and-comments-are-not-split
  (is (equal "}# $x" (eval-with-sml "#@sml{ \"}# $x\" }#")))
  (is (equal "a\"}#" (eval-with-sml "#@sml{ \"a\\\"}#\" }#")))
  (is (= 1 (eval-with-sml "#@sml{ (* }# $x (* nested }# *) *) 1 }#")))
  (is (eql #\} (eval-with-sml "#@sml{ #\"}\" }#"))))

(test dollar-in-sml-identifiers
  (eval-with-sml "#@sml{ infix 6 <$>; fun a <$> b = a - b }#")
  (is (= 1 (eval-with-sml "#@sml{ 3 <$> 2 }#"))))

(test syntax-error-reports-position
  (let ((path (uiop:tmpize-pathname
               (merge-pathnames "cl-sml-reader-error.lisp"
                                (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output :if-exists :supersede)
             (format out "(in-package #:cl-sml-reader-tests)~%~%#@sml{~%  1 +~%}#~%"))
           (let ((message
                   (handler-case
                       (let ((*readtable* (enable-sml-syntax (copy-readtable nil)))
                             (*package* (find-package '#:cl-sml-reader-tests)))
                         (with-open-file (in path)
                           (read in)
                           (macroexpand (read in)))
                         nil)
                     (error (e) (princ-to-string e)))))
             (is (search (format nil "~A:4:" (file-namestring path)) message))))
      (delete-file path))))

(test compile-file-carries-the-block
  (let* ((path (uiop:tmpize-pathname
                (merge-pathnames "cl-sml-reader-file.lisp"
                                 (uiop:temporary-directory))))
         (fasl nil))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output :if-exists :supersede)
             (write-string "(in-package #:cl-sml-reader-tests)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (cl-sml:enable-sml-syntax (copy-readtable))))
#@sml{ datatype reader_u = RU of int | RP of int * int
   fun reader_unwrap (RU n) = n | reader_unwrap (RP (a, b)) = a + b
   fun reader_field ({a, ...} : {a : int, b : bool}) = a }#
(defun reader-file-test (k)
  #@sml{ reader_unwrap (RU ($k : int)) * 2
         + reader_unwrap (RP (1, 2)) - #b {a = 1, b = 3}
         + reader_field {b = true, a = 0} }#)
" out))
           (setf fasl (let ((*standard-output* (make-broadcast-stream))
                            (*error-output* (make-broadcast-stream)))
                        (compile-file path)))
           (is (not (null fasl)))
           (load fasl)
           (is (= 42 (funcall 'reader-file-test 21))))
      (when (and fasl (probe-file fasl)) (delete-file fasl))
      (delete-file path))))

(test tuples-and-records-cross-the-boundary
  ;; Tuples and records built in Lisp go into SML, and SML's come out.
  (is (= 5 (eval-with-sml "#@sml{ (fn (x, y) => x + y) $(cl-sml:sml-tuple 2 3) }#")))
  (is (equal '(1 "a")
             (sml-tuple->list (eval-with-sml "#@sml{ (1, \"a\") }#"))))
  (is (= 7 (eval-with-sml
            "#@sml{ #b ($(cl-sml:make-sml-record '((\"b\" . 7) (\"a\" . 1))) : {a : int, b : int}) }#")))
  (let ((record (eval-with-sml "#@sml{ {name = \"x\", size = 2} }#")))
    (is (sml-record-p record))
    (is (not (sml-tuple-p record)))
    (is (equal "x" (sml-record-ref record "name")))
    (is (equal '("name" "size") (sml-record-labels record)))
    (is (equal '(("name" . "x") ("size" . 2)) (sml-record-fields record))))
  (let ((pair (sml-tuple 10 20)))
    (is (sml-tuple-p pair))
    (is (= 20 (sml-tuple-ref pair 2)))
    (is (= 10 (sml-record-ref pair 1)))
    (is (= 10 (svref pair 1)))
    (is (eq (sml-unit) (sml-tuple)))
    (is (equal '(10 20) (sml-tuple->list (list->sml-tuple '(10 20))))))
  ;; `{1 = a, 2 = b}' is the pair `(a, b)'.
  (is (eq t (eval-with-sml "#@sml{ {1 = 3, 2 = 4} = (3, 4) }#")))
  ;; Constructed values.
  (let ((value (eval-with-sml "#@sml{ SOME (1, 2) }#")))
    (is (string= "SOME" (symbol-name (sml-constructed-tag value))))
    (is (equal '(1 2) (sml-tuple->list (sml-constructed-argument value))))))
