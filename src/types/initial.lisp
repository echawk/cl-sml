(in-package #:cl-sml.types)

;;; The initial static environment (HaMLet elab/InitialStaticEnv and
;;; lib/StaticLibrary): Appendix C of the Definition, the overloaded
;;; identifiers of Appendix E, and the values the Basis Library binds at
;;; top level (option, order, General, List, String, ...), written by hand.
;;; Milestone M2 replaces this with the elaborated Basis (src/basis).

(defvar *t-bool* (make-tyname "bool" 0 :constructors '(("false") ("true"))))
(defvar *t-int* (make-tyname "int" 0))
(defvar *t-word* (make-tyname "word" 0))
(defvar *t-real* (make-tyname "real" 0 :equality nil))
(defvar *t-string* (make-tyname "string" 0))
(defvar *t-char* (make-tyname "char" 0))
(defvar *t-list* (make-tyname "list" 1 :constructors '(("::" . t) ("nil"))))
(defvar *t-ref* (make-tyname "ref" 1 :equality :ref :constructors '(("ref" . t))))
(defvar *t-exn* (make-tyname "exn" 0 :equality nil))
(defvar *t-option* (make-tyname "option" 1 :constructors '(("NONE") ("SOME" . t))))
(defvar *t-order* (make-tyname "order" 0 :constructors '(("EQUAL") ("GREATER") ("LESS"))))
(defvar *t-vector* (make-tyname "vector" 1))

;;; Overloading classes [Appendix E.1; HaMLet StaticLibrary].
(defvar *class-int* (make-oclass (list *t-int*) *t-int*))
(defvar *class-real* (make-oclass (list *t-real*) *t-real*))
(defvar *class-word* (make-oclass (list *t-word*) *t-word*))
(defvar *class-string* (make-oclass (list *t-string*) *t-string*))
(defvar *class-char* (make-oclass (list *t-char*) *t-char*))
(defvar *class-wordint* (oclass-union *class-word* *class-int*))
(defvar *class-realint* (oclass-union *class-real* *class-int*))
(defvar *class-num* (oclass-union *class-word* *class-realint*))
(defvar *class-txt* (oclass-union *class-string* *class-char*))
(defvar *class-numtxt* (oclass-union *class-txt* *class-num*))

(defun bool-type () (con-type *t-bool*))
(defun int-type () (con-type *t-int*))
(defun exn-type () (con-type *t-exn*))
(defun list-type (type) (con-type *t-list* type))

;;; Types of the initial environment are written as s-expressions:
;;; a keyword :a, :b, ... is a type variable (:eqa is ''a), a symbol names
;;; a type constructor, (-> a b), (* a b ...) and (con arg...).

(defun initial-tycon (symbol)
  (ecase symbol
    (bool *t-bool*) (int *t-int*) (word *t-word*) (real *t-real*)
    (string *t-string*) (char *t-char*) (list *t-list*) (ref *t-ref*)
    (exn *t-exn*) (option *t-option*) (order *t-order*) (vector *t-vector*)))

(defun spec-type (spec vars)
  "The type SPEC denotes; VARS is an alist keyword -> TVAR, extended."
  (cond ((keywordp spec)
         (or (cdr (assoc spec (car vars)))
             (let ((var (make-rigid-tvar (if (eq spec :eqa)
                                             "''a"
                                             (format nil "'~(~A~)" spec))
                                         :level 0)))
               (push (cons spec var) (car vars))
               var)))
        ((eq spec 'unit) (unit-type))
        ((symbolp spec) (con-type (initial-tycon spec)))
        ((eq (first spec) '->)
         (arrow-type (spec-type (second spec) vars) (spec-type (third spec) vars)))
        ((eq (first spec) '*)
         (tuple-type (mapcar (lambda (s) (spec-type s vars)) (rest spec))))
        (t (apply #'con-type (initial-tycon (first spec))
                  (mapcar (lambda (s) (spec-type s vars)) (rest spec))))))

(defun spec-scheme (spec)
  (let* ((vars (list nil))
         (type (spec-type spec vars)))
    (make-scheme (mapcar #'cdr (sort (car vars) #'string< :key #'car)) type)))

(defun overloaded-scheme (class make-type)
  (let ((var (fresh-tvar :overload class :level 0)))
    (make-scheme (list var) (funcall make-type var))))

(defparameter *initial-values*
  '(;; Appendix C
    ("=" (-> (* :eqa :eqa) bool))
    (":=" (-> (* (ref :a) :a) unit))
    ;; The Basis Library's top-level values
    ("<>" (-> (* :eqa :eqa) bool))
    ("!" (-> (ref :a) :a))
    ("o" (-> (* (-> :b :c) (-> :a :b)) (-> :a :c)))
    ("before" (-> (* :a unit) :a))
    ("ignore" (-> :a unit))
    ("exnName" (-> exn string))
    ("exnMessage" (-> exn string))
    ("getOpt" (-> (* (option :a) :a) :a))
    ("isSome" (-> (option :a) bool))
    ("valOf" (-> (option :a) :a))
    ("not" (-> bool bool))
    ("real" (-> int real))
    ("trunc" (-> real int))
    ("floor" (-> real int))
    ("ceil" (-> real int))
    ("round" (-> real int))
    ("ord" (-> char int))
    ("chr" (-> int char))
    ("size" (-> string int))
    ("str" (-> char string))
    ("concat" (-> (list string) string))
    ("implode" (-> (list char) string))
    ("explode" (-> string (list char)))
    ("substring" (-> (* string int int) string))
    ("^" (-> (* string string) string))
    ("null" (-> (list :a) bool))
    ("hd" (-> (list :a) :a))
    ("tl" (-> (list :a) (list :a)))
    ("length" (-> (list :a) int))
    ("rev" (-> (list :a) (list :a)))
    ("@" (-> (* (list :a) (list :a)) (list :a)))
    ("app" (-> (-> :a unit) (-> (list :a) unit)))
    ("map" (-> (-> :a :b) (-> (list :a) (list :b))))
    ("foldr" (-> (-> (* :a :b) :b) (-> :b (-> (list :a) :b))))
    ("foldl" (-> (-> (* :a :b) :b) (-> :b (-> (list :a) :b))))
    ("print" (-> string unit))
    ("vector" (-> (list :a) (vector :a)))
    ("use" (-> string unit))))

(defparameter *initial-exceptions*
  '(("Bind") ("Match") ("Chr") ("Div") ("Domain") ("Overflow") ("Size")
    ("Subscript") ("Empty") ("Option") ("Span") ("Fail" . string)))

(defun datatype-tystr (tyname constructors)
  "The TYSTR of initial datatype TYNAME; CONSTRUCTORS are (name spec)."
  (let* ((vars (list nil))
         (params (loop for i below (tyname-arity tyname)
                       collect (spec-type (nth i '(:a :b)) vars)))
         (result (apply #'con-type tyname params))
         (ve (loop for (name spec) in constructors
                   collect (cons name
                                 (make-valstr
                                  (make-scheme params
                                               (if spec
                                                   (arrow-type (spec-type spec vars) result)
                                                   result))
                                  :c)))))
    (make-tystr (make-tyfcn params result) ve :datatype)))

(defun make-initial-env ()
  (let* ((datatypes
           (list (cons "bool" (datatype-tystr *t-bool* '(("true") ("false"))))
                 (cons "list" (datatype-tystr *t-list* '(("nil") ("::" (* :a (list :a))))))
                 (cons "ref" (datatype-tystr *t-ref* '(("ref" :a))))
                 (cons "option" (datatype-tystr *t-option* '(("NONE") ("SOME" :a))))
                 (cons "order" (datatype-tystr *t-order* '(("LESS") ("EQUAL") ("GREATER"))))))
         (te (append
              datatypes
              (list (cons "unit" (make-tystr (make-tyfcn nil (unit-type)) nil))
                    (cons "vector" (let ((a (make-rigid-tvar "'a" :level 0)))
                                     (make-tystr (make-tyfcn (list a) (con-type *t-vector* a))
                                                 nil :abstype))))
              (loop for tyname in (list *t-int* *t-word* *t-real* *t-string* *t-char* *t-exn*)
                    collect (cons (tyname-name tyname)
                                  (make-tystr (make-tyfcn nil (con-type tyname)) nil
                                              :abstype)))))
         (constructors (loop for (nil . tystr) in datatypes
                             append (tystr-ve tystr)))
         (exceptions (loop for (name . arg) in *initial-exceptions*
                           collect (cons name
                                         (make-valstr
                                          (mono-scheme (if arg
                                                           (arrow-type (spec-type arg (list nil))
                                                                       (exn-type))
                                                           (exn-type)))
                                          :e))))
         (overloaded
           (flet ((unary (class)
                    (overloaded-scheme class (lambda (a) (arrow-type a a))))
                  (binary (class)
                    (overloaded-scheme class (lambda (a) (arrow-type (tuple-type (list a a)) a))))
                  (relation (class)
                    (overloaded-scheme class (lambda (a) (arrow-type (tuple-type (list a a))
                                                                     (bool-type))))))
             (mapcar (lambda (entry)
                       (cons (first entry) (make-valstr (second entry) :v)))
                     (list (list "abs" (unary *class-realint*))
                           (list "~" (unary *class-num*))
                           (list "+" (binary *class-num*))
                           (list "-" (binary *class-num*))
                           (list "*" (binary *class-num*))
                           (list "div" (binary *class-wordint*))
                           (list "mod" (binary *class-wordint*))
                           (list "/" (binary *class-real*))
                           (list "<" (relation *class-numtxt*))
                           (list ">" (relation *class-numtxt*))
                           (list "<=" (relation *class-numtxt*))
                           (list ">=" (relation *class-numtxt*))))))
         (values (loop for (name spec) in *initial-values*
                       collect (cons name (make-valstr (spec-scheme spec) :v)))))
    (make-env (append constructors exceptions overloaded values) te)))

(defvar *initial-env* nil)

(defun initial-env ()
  (or *initial-env* (setf *initial-env* (make-initial-env))))
