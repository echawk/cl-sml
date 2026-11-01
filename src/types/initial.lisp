(in-package #:cl-sml.types)

;;; The starting static basis: what the Definition fixes (Appendix C, the
;;; initial basis, and the overloaded identifiers of Appendix E) plus the
;;; few primitives HaMLet's lib/StaticLibrary adds and our Basis sources
;;; build on (the type vector, the structures Word8 and IO with their type
;;; word and exception Io, and `use').  The Basis Library proper is
;;; elaborated from its sources (src/basis) on top of this, in basis.lisp
;;; of this directory.

(defvar *t-bool* (make-tyname "bool" 0 :constructors '(("false") ("true"))))
(defvar *t-int* (make-tyname "int" 0))
(defvar *t-word* (make-tyname "word" 0))
(defvar *t-real* (make-tyname "real" 0 :equality nil))
(defvar *t-string* (make-tyname "string" 0))
(defvar *t-char* (make-tyname "char" 0))
(defvar *t-list* (make-tyname "list" 1 :constructors '(("::" . t) ("nil"))))
(defvar *t-ref* (make-tyname "ref" 1 :equality :ref :constructors '(("ref" . t))))
(defvar *t-exn* (make-tyname "exn" 0 :equality nil))
(defvar *t-vector* (make-tyname "vector" 1))
(defvar *t-word8* (make-tyname "word8" 0))  ; as HaMLet names it
;; Set to the Basis's option once it is elaborated (for the relaxed value
;; restriction, which counts option as covariant).
(defvar *t-option* nil)

;;; Overloading classes [Appendix E.1; HaMLet StaticLibrary].  The basic
;;; classes start as HaMLet's; once the Basis is elaborated, the integer,
;;; word and real types of its structures join them (EXTEND-OVERLOADING-
;;; CLASSES), and the unions are recomputed in place, because the schemes
;;; of the overloaded identifiers refer to these objects.
(defvar *class-int* (make-oclass (list *t-int*) *t-int*))
(defvar *class-real* (make-oclass (list *t-real*) *t-real*))
(defvar *class-word* (make-oclass (list *t-word* *t-word8*) *t-word*))
(defvar *class-string* (make-oclass (list *t-string*) *t-string*))
(defvar *class-char* (make-oclass (list *t-char*) *t-char*))
(defvar *class-wordint* (oclass-union *class-word* *class-int*))
(defvar *class-realint* (oclass-union *class-real* *class-int*))
(defvar *class-num* (oclass-union *class-word* *class-realint*))
(defvar *class-txt* (oclass-union *class-string* *class-char*))
(defvar *class-numtxt* (oclass-union *class-txt* *class-num*))

(defun recompute-overloading-unions ()
  (flet ((set-union (class class1 class2)
           (setf (oclass-tynames class)
                 (union (oclass-tynames class1) (oclass-tynames class2)))))
    (set-union *class-wordint* *class-word* *class-int*)
    (set-union *class-realint* *class-real* *class-int*)
    (set-union *class-num* *class-word* *class-realint*)
    (set-union *class-txt* *class-string* *class-char*)
    (set-union *class-numtxt* *class-txt* *class-num*)))

(defun extend-overloading-classes (&key ints words reals)
  "Add the type names INTS, WORDS and REALS to the classes Int, Word and
Real (and so to their unions).  The defaults stay int, word and real."
  (flet ((extend (class tynames)
           (setf (oclass-tynames class) (union (oclass-tynames class) tynames))))
    (extend *class-int* ints)
    (extend *class-word* words)
    (extend *class-real* reals))
  (recompute-overloading-unions))

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
    (exn *t-exn*) (vector *t-vector*)))

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
    ;; HaMLet's StaticLibrary: use{b = "name"} is a primitive of the run
    ;; time (src/runtime.lisp), whose type the Basis states.
    ("use" (-> :a :b))))

(defparameter *initial-exceptions*
  '(;; Appendix C
    ("Bind") ("Match")
    ;; HaMLet's StaticLibrary
    ("Chr") ("Div") ("Domain") ("Overflow") ("Size") ("Subscript")))

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

(defun make-starting-env ()
  (let* ((datatypes
           (list (cons "bool" (datatype-tystr *t-bool* '(("true") ("false"))))
                 (cons "list" (datatype-tystr *t-list* '(("nil") ("::" (* :a (list :a))))))
                 (cons "ref" (datatype-tystr *t-ref* '(("ref" :a))))))
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
                       collect (cons name (make-valstr (spec-scheme spec) :v))))
         (se (list (cons "Word8"
                         (make-env nil (list (cons "word" (make-tystr (make-tyfcn nil (con-type *t-word8*))
                                                                      nil :abstype)))))
                   (cons "IO"
                         (make-env (list (cons "Io"
                                               (make-valstr
                                                (mono-scheme
                                                 (arrow-type (record-type
                                                              (list (cons "name" (con-type *t-string*))
                                                                    (cons "function" (con-type *t-string*))
                                                                    (cons "cause" (exn-type))))
                                                             (exn-type)))
                                                :e))))))))
    (make-env (append constructors exceptions overloaded values) te se)))

(defvar *starting-env* nil)

(defun starting-env ()
  "The static basis the Basis Library is elaborated in."
  (or *starting-env* (setf *starting-env* (make-starting-env))))
