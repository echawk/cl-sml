(in-package #:cl-sml)

;;; A typed intermediate language between the checked AST and Common Lisp
;;; (docs/IR.md, ROADMAP items 19, 12 and 11).
;;;
;;; COMPILE-EXPR lowers an SML expression to an IR term (IR-LOWER),
;;; simplifies it (IR-SIMPLIFY) and emits CL (IR-EMIT); the clauses of a
;;; `fun' go the same way (IR-COMPILE-FN-CLAUSES).  The direct code
;;; generator, COMPILE-EXPR-DIRECT, is used only when *SML-IR* is false.
;;;
;;; A term is a list (KIND TYPE . FIELDS).  TYPE is the node's SML type as
;;; an IR type (below), or NIL when unknown (plain mode, which has no
;;; checker).  Variables are uninterned symbols; every binder binds a
;;; symbol of its own.  The kinds and their fields:
;;;
;;;   (:const ty value)          a literal: integer, word, char, string,
;;;                              real, or T / NIL (true, false, nil)
;;;   (:unit ty)                 ()
;;;   (:var ty symbol)           a lexical variable
;;;   (:ref ty form)             a global binding: FORM reads its cell (or
;;;                              is a built-in function, e.g. #'sml-not)
;;;   (:prim ty op args)         a primitive operation, resolved by type:
;;;                              op is (:overloaded name tyname),
;;;                              (:equality test negated), (:binary fn),
;;;                              (:word-binary fn), (:basis name), (:not),
;;;                              (:deref) or (:list)
;;;   (:tuple ty args)           an n-tuple (a vector, ROADMAP 10)
;;;   (:record ty labels args)   a record, LABELS in canonical order
;;;   (:select ty index arg)     field INDEX (1-based) of a tuple/record
;;;   (:select-label ty label arg)  field LABEL, layout unknown
;;;   (:con ty repr tag args)    a constructor applied: REPR :cons (TAG
;;;                              is a form), :boxed, :flat (ARGS are the
;;;                              fields) or :flat-construct
;;;   (:app ty f arg)            unknown call of a one-argument function
;;;   (:call ty worker args cell)  known call of the global function
;;;                              WORKER (the tupled worker of CELL)
;;;   (:fn ty params body name)  a function of (length PARAMS) arguments;
;;;                              TY is its SML type (a tuple argument for
;;;                              several PARAMS)
;;;   (:let ty symbol value body)
;;;   (:letrec ty ((symbol value) ...) body)
;;;   (:seq ty items)
;;;   (:if ty test then else)
;;;   (:while ty test body)
;;;   (:raise ty exn)
;;;   (:handle ty body ((pattern body) ...))
;;;   (:match ty scrutinees ((patterns body) ...) failure)  each rule has
;;;                              one pattern per scrutinee; FAILURE is
;;;                              (exception-name where offset), raised
;;;                              when no rule matches
;;;   (:loop ty tag variables inits body)  binds VARIABLES to INITS around
;;;                              BODY, which a :tail-call may restart
;;;   (:tail-call ty (tag . variables) args)  a self tail call (ROADMAP 8)
;;;   (:decl ty declaration body)  a local datatype or exception (see
;;;                              IR-EMIT-DECLARATION), or (:module form),
;;;                              a structure or functor declared in a
;;;                              `let', compiled as a declaration
;;;
;;; Patterns are IR data too, (KIND TYPE . FIELDS):
;;;
;;;   (:p-wild ty)  (:p-var ty symbol repr)  (:p-const ty value)
;;;   (:p-tuple ty patterns)  (:p-record ty width ((index label pattern) ...))
;;;   (:p-record-lookup ty ((label pattern) ...))   layout unknown
;;;   (:p-as ty symbol pattern)  (:p-cons ty head tail)  (:p-ref ty pattern)
;;;   (:p-exn ty name form pattern-or-nil)    FORM evaluates to the exception
;;;   (:p-con ty name how data pattern-or-nil)  a datatype constructor;
;;;                              HOW and DATA say how its values are
;;;                              recognized (IR-EMIT-CONSTRUCTOR-PATTERN)
;;;
;;; IR types:
;;;
;;;   (:con tycon arg ...)       TYCON an IR-TYCON
;;;   (:tyvar id name equality)  a type variable
;;;   (:arrow domain range)
;;;   (:record ((label . type) ...) open)  labels in canonical order;
;;;                              tuples have labels 1..n, unit none

(defvar *sml-ir-inline-size* 40
  "The largest IR term (in nodes) of a function inlined at its calls.")

(defvar *sml-ir-simplify-rounds* 3
  "How many times the simplifier runs over a term at most.")

(defvar *sml-ir-lint*
  (let ((setting (uiop:getenv "CL_SML_IR_LINT")))
    (cond ((member setting '(nil "" "0") :test #'equal) nil)
          ((string-equal setting "error") :error)
          (t :collect)))
  "When true, each term is checked by IR-LINT after lowering and after
simplification: :ERROR signals the problems found, any other true value
collects them in *SML-IR-LINT-PROBLEMS*.  The environment variable
CL_SML_IR_LINT (\"error\" or \"collect\") sets it when cl-sml is loaded.")

(defvar *sml-ir-lint-problems* nil)

(defun ir (kind type &rest fields) (list* kind type fields))
(declaim (inline ir-kind ir-type))
(defun ir-kind (node) (first node))
(defun ir-type (node) (second node))

(defun ir-variable-p (form)
  (and form (symbolp form) (null (symbol-package form))))

;;; Types

(defstruct ir-tycon
  "A type constructor: NAME, ARITY, EQUALITY (T, NIL or :REF, which admits
equality whatever its arguments), the CONSTRUCTORS of a datatype as
(name . takes-argument-p), and the checker's STAMP (unique in a session)."
  name (arity 0) (equality t) constructors (stamp 0))

(defmethod make-load-form ((tycon ir-tycon) &optional environment)
  (make-load-form-saving-slots tycon :environment environment))

(defun ir-node-type (ast)
  "The IR type the native checker gives expression or pattern AST, or NIL."
  (and (consp ast)
       (native-sml-static-facts-p)
       (funcall 'ir-checker-node-type ast)))

(defun ir-base-type (name)
  (and (native-sml-static-facts-p)
       (funcall 'ir-checker-base-type name)))

(defun ir-constant-type (value)
  (cond ((integerp value) (ir-base-type "int"))
        ((stringp value) (ir-base-type "string"))
        ((characterp value) (ir-base-type "char"))
        ((floatp value) (ir-base-type "real"))))

(defun ir-arrow-type (domain range)
  (and domain range (list :arrow domain range)))

(defun ir-tuple-type (types)
  (and (every #'identity types)
       (list :record (loop for type in types
                           for i from 1
                           collect (cons (princ-to-string i) type))
             nil)))

(defun ir-record-type (labels types)
  (and (every #'identity types)
       (list :record (mapcar #'cons labels types) nil)))

(defun ir-unit-type-p (type)
  (and (consp type) (eq (first type) :record) (null (second type))))

(defun ir-arrow-domain (type)
  (and (consp type) (eq (first type) :arrow) (second type)))

(defun ir-arrow-range (type)
  (and (consp type) (eq (first type) :arrow) (third type)))

(defun ir-field-type (type label)
  (and (consp type) (eq (first type) :record)
       (cdr (assoc label (second type) :test #'string=))))

(defun ir-tuple-field-types (type n)
  (loop for i from 1 to n collect (ir-field-type type (princ-to-string i))))

(defun ir-type-named-p (type name)
  (and (consp type) (eq (first type) :con)
       (string= (ir-tycon-name (second type)) name)))

(defun ir-list-element-type (type)
  (and (ir-type-named-p type "list") (third type)))

(defvar *ir-abstraction-tolerant* nil
  "True when an abstract type may meet its representation (in simplified
terms, see IR-LINT-TERM).")

(defun ir-type-compatible-p (a b)
  "Can types A and B be the same type?  Unknown types (NIL) and type
variables are compatible with any type: the terms of inlined polymorphic
functions keep their own type variables."
  (cond ((or (null a) (null b)) t)
        ((or (eq (first a) :tyvar) (eq (first b) :tyvar)) t)
        ((and *ir-abstraction-tolerant*
              (or (eq (first a) :con) (eq (first b) :con))
              (not (and (eq (first a) :con) (eq (first b) :con)
                        (string= (ir-tycon-name (second a)) (ir-tycon-name (second b))))))
         t)
        ((not (eq (first a) (first b))) nil)
        (t (ecase (first a)
             (:con (and (string= (ir-tycon-name (second a)) (ir-tycon-name (second b)))
                        (= (length a) (length b))
                        (every #'ir-type-compatible-p (cddr a) (cddr b))))
             (:arrow (and (ir-type-compatible-p (second a) (second b))
                          (ir-type-compatible-p (third a) (third b))))
             (:record
              (or (third a) (third b)
                  (and (= (length (second a)) (length (second b)))
                       (every (lambda (fa fb)
                                (and (string= (car fa) (car fb))
                                     (ir-type-compatible-p (cdr fa) (cdr fb))))
                              (second a) (second b)))))))))

(defun ir-type-string (type)
  "TYPE in SML syntax."
  (labels ((tuple-p (type)
             (and (eq (first type) :record) (second type) (null (third type))
                  (loop for (label) in (second type)
                        for i from 1
                        always (string= label (princ-to-string i)))
                  (/= (length (second type)) 1)))
           (atomic (type)
             (let ((string (show type)))
               (if (or (eq (first type) :arrow) (tuple-p type))
                   (format nil "(~A)" string)
                   string)))
           (show (type)
             (cond
               ((null type) "?")
               (t (ecase (first type)
                    (:con (let ((name (ir-tycon-name (second type)))
                                (args (cddr type)))
                            (cond ((null args) name)
                                  ((null (rest args)) (format nil "~A ~A" (atomic (first args)) name))
                                  (t (format nil "(~{~A~^, ~}) ~A" (mapcar #'show args) name)))))
                    (:tyvar (format nil "~:[~;'~]'~A" (fourth type)
                                    (or (and (third type) (string-left-trim "'" (third type)))
                                        (format nil "t~D" (second type)))))
                    (:arrow (format nil "~A -> ~A" (atomic (second type)) (show (third type))))
                    (:record
                     (cond ((null (second type)) (if (third type) "{...}" "unit"))
                           ((tuple-p type)
                            (format nil "~{~A~^ * ~}"
                                    (mapcar (lambda (field) (atomic (cdr field))) (second type))))
                           (t (format nil "{~{~A~^, ~}~:[~;, ...~]}"
                                      (mapcar (lambda (field)
                                                (format nil "~A: ~A" (car field) (show (cdr field))))
                                              (second type))
                                      (third type))))))))))
    (show type)))

;;; Lowering

(defvar *ir-template-mode* nil
  "True while a function is lowered for inlining: a construct an inline
template cannot hold throws to IR-NO-TEMPLATE.")

(defvar *ir-tail-call-type* nil
  "The result type of the function whose self tail calls are being
lowered.")

(defun ir-failure (exception-name where ast)
  "The failure of a match (see COMPILE-MATCH-FAILURE), as data."
  (list exception-name where (first-positioned-sml-node ast)))

(defun ir-lower-tuple-binary-args (arg)
  "The two operand ASTs of an infix application's argument ARG, or NIL."
  (and (consp arg) (eq (car arg) :tuple) (= (length arg) 3)
       (list (second arg) (third arg))))

(defun ir-lower-overloaded (ast local-exceptions lexical-env type)
  "Like COMPILE-OVERLOADED-APPLICATION, as a (:prim ...) node, or NIL."
  (destructuring-bind (head arg) (cdr ast)
    (multiple-value-bind (name tyname) (overloaded-identifier-instance head)
      (when name
        (let ((args (cond ((and (eq (car head) :var)
                                (member name '("~" "abs") :test #'string=))
                           (list arg))
                          ((ir-lower-tuple-binary-args arg)))))
          (when (and args
                     (compile-overloaded-operation
                      name tyname (mapcar (lambda (a) (declare (ignore a)) (gensym))
                                          args)))
            (ir :prim type (list :overloaded name tyname)
                (mapcar (lambda (a) (ir-lower a local-exceptions lexical-env)) args))))))))

(defun ir-lower-equality (ast local-exceptions lexical-env type)
  "Like COMPILE-EQUALITY-APPLICATION, or NIL."
  (when (and (consp (second ast)) (eq (car (second ast)) :var)
             (member (second (second ast)) '("=" "<>") :test #'equal)
             (ir-lower-tuple-binary-args (third ast))
             (builtin-binary-primitive (second ast) lexical-env))
    (let ((test (case (sml-equality-kind (second ast))
                  (:eql 'eql) (:eq 'eq) (:string 'string=))))
      (when test
        (ir :prim type (list :equality test (string= (second (second ast)) "<>"))
            (mapcar (lambda (a) (ir-lower a local-exceptions lexical-env))
                    (ir-lower-tuple-binary-args (third ast))))))))

(defun ir-lower-binary-primitive (ast local-exceptions lexical-env type)
  "Like COMPILE-BINARY-PRIMITIVE-APPLICATION, or NIL."
  (let ((operands (ir-lower-tuple-binary-args (third ast))))
    (when operands
      (let ((primitive (builtin-binary-primitive (second ast) lexical-env)))
        (when primitive
          (let ((args (mapcar (lambda (a) (ir-lower a local-exceptions lexical-env))
                              operands))
                (word-operation (cdr (assoc primitive '((sml-+ . +) (sml-- . -)
                                                        (sml-* . *))))))
            (if (and word-operation
                     (some (lambda (x) (and (consp x) (eq (car x) :word))) operands))
                (ir :prim type (list :word-binary word-operation) args)
                (ir :prim type (list :binary primitive) args))))))))

(defun ir-lower-constructor-application (head arg local-exceptions lexical-env type)
  "The application of constructor HEAD to ARG, as in COMPILE-EXPR-DIRECT."
  (let ((tag (lexical-constructor-tag-form (second head) lexical-env)))
    (if tag
        (ir :con type :cons tag (list (ir-lower arg local-exceptions lexical-env)))
        (let* ((symbol (constructor-symbol-for-name (second head) lexical-env))
               (arity (constructor-arity-at-compile-time symbol)))
          (cond
            ((null arity)
             (ir :app type (ir-lower head local-exceptions lexical-env)
                 (ir-lower arg local-exceptions lexical-env)))
            ((eql arity 0)
             (ir :con type :boxed symbol (list (ir-lower arg local-exceptions lexical-env))))
            ((and (consp arg) (eq (car arg) :tuple) (= (length (cdr arg)) arity))
             (ir :con type :flat symbol
                 (mapcar (lambda (item) (ir-lower item local-exceptions lexical-env))
                         (cdr arg))))
            (t (ir :con type :flat-construct symbol
                   (list (ir-lower arg local-exceptions lexical-env)))))))))

(defun ir-lower-tupled-call (head arg local-exceptions lexical-env type)
  "Like COMPILE-TUPLED-CALL, or NIL."
  (when (and (consp head) (eq (car head) :var)
             (consp arg) (eq (car arg) :tuple))
    (let* ((symbol (resolved-sml-symbol (second head) lexical-env))
           (worker (and (symbolp symbol) (symbol-package symbol)
                        (get symbol 'sml-tupled-worker))))
      (when (and worker (= (cdr worker) (length (cdr arg))))
        (ir :call type (car worker)
            (mapcar (lambda (item) (ir-lower item local-exceptions lexical-env))
                    (cdr arg))
            symbol)))))

;;; Patterns, lowered as COMPILE-PAT compiles them: every decision
;;; COMPILE-PAT takes is taken here and recorded in the pattern, which
;;; IR-EMIT-PATTERN turns into the same trivia pattern.

(defun ir-lower-pattern (pat local-exceptions lexical-env &optional expected)
  "The IR pattern of pattern AST PAT, whose value has type EXPECTED (when
the checker gave PAT no type of its own)."
  (let ((type (if (and (consp pat) (eq (car pat) :pat-cons))
                  ;; The checker records the type of `::' on the node.
                  (or (ir-arrow-range (ir-node-type pat)) expected)
                  (or (ir-node-type pat) expected))))
    (flet ((sub (pat &optional expected)
             (ir-lower-pattern pat local-exceptions lexical-env expected)))
      (cond
        ((or (numberp pat) (stringp pat) (characterp pat))
         (ir :p-const (or type (ir-constant-type pat)) pat))
        ((eq pat :wild) (ir :p-wild type))
        ((not (consp pat)) (error "Unknown pattern ~A" pat))
        (t
         (case (car pat)
           (:word (ir :p-const type (second pat)))
           ((:pat-ctor :ctor)
            (ir-lower-constructor-pattern pat local-exceptions lexical-env type))
           (:pat-typed (sub (second pat) type))
           (:pat-as
            (let ((alias (sub (second pat) type)))
              (unless (eq (ir-kind alias) :p-var)
                (error "Unknown layered pattern ~A" pat))
              (ir :p-as type (third alias) (sub (third pat) type))))
           (:pat-app
            (let* ((ctor-node (second pat))
                   (arg-type (ir-arrow-domain (ir-node-type ctor-node))))
              (cond
                ((string= (second ctor-node) "ref")
                 (ir :p-ref type (sub (third pat) (and (ir-type-named-p type "ref")
                                                       (third type)))))
                ((pattern-node-exception-p ctor-node local-exceptions)
                 (ir :p-exn type (second ctor-node)
                     (resolved-sml-symbol (second ctor-node) lexical-env)
                     (sub (third pat) arg-type)))
                (t (ir-lower-constructor-app-pattern pat local-exceptions lexical-env
                                                     type arg-type)))))
           ((:pat-var :var)
            (ir :p-var type
                (or (lexical-symbol-for-name (second pat) lexical-env)
                    (target-sml-symbol (second pat)))
                (sml-node-representation pat)))
           (:pat-unit (ir :p-wild type))
           (:pat-tuple
            (let ((types (ir-tuple-field-types type (length (cdr pat)))))
              (ir :p-tuple type (mapcar #'sub (cdr pat) types))))
           (:pat-record
            (multiple-value-bind (tuple-fields tuplep) (tuple-record-fields (cdr pat))
              (if tuplep
                  (if tuple-fields
                      (ir :p-tuple type
                          (mapcar (lambda (field)
                                    (sub (second field) (ir-field-type type (first field))))
                                  tuple-fields))
                      (ir :p-wild type))
                  (ir-lower-record-pattern pat local-exceptions lexical-env type))))
           (:pat-nil (ir :p-const type nil))
           (:pat-cons
            (ir :p-cons type (sub (second pat) (ir-list-element-type type))
                (sub (third pat) type)))
           (otherwise (error "Unknown pattern ~A" pat))))))))

(defun ir-lower-record-pattern (pat local-exceptions lexical-env type)
  "Like COMPILE-RECORD-PATTERN."
  (let* ((fields (cdr pat))
         (sorted (record-fields-sorted-by-label fields))
         (labels (if (member :record-rest fields)
                     (sml-node-record-labels pat)
                     (mapcar #'first sorted))))
    (flet ((sub (field)
             (ir-lower-pattern (second field) local-exceptions lexical-env
                               (ir-field-type type (first field)))))
      (if labels
          (ir :p-record type (length labels)
              (mapcar (lambda (field)
                        (list (1+ (position (first field) labels :test #'string=))
                              (first field)
                              (sub field)))
                      sorted))
          (ir :p-record-lookup type
              (mapcar (lambda (field) (list (first field) (sub field))) sorted))))))

(defun ir-lower-constructor-pattern (pat local-exceptions lexical-env type)
  "A nullary constructor (or a variable) PAT, as COMPILE-PAT decides."
  (let ((name (second pat)))
    (cond
      ((string= name "true") (ir :p-const type t))
      ((member name '("false" "nil") :test #'string=) (ir :p-const type nil))
      ((pattern-node-exception-p pat local-exceptions)
       (ir :p-exn type name (resolved-sml-symbol name lexical-env) nil))
      (t
       (let* ((static-ctor-p (sml-status-constructor-p (pattern-node-status pat)))
              (lexical-ctor (or (lexical-symbol-for-name name lexical-env)
                                (functor-scope-long-name-variable name)))
              (known-ctor (known-constructor-symbol-for-name name))
              (global-ctor (or known-ctor (resolved-sml-symbol name lexical-env))))
         (flet ((con (how data) (ir :p-con type name how data nil)))
           (cond
             ((and lexical-ctor
                   (not static-ctor-p)
                   (not known-ctor)
                   (not (functor-scope-constructor-tag lexical-ctor))
                   (not (sml-constructor-symbol-p global-ctor)))
              ;; Not a constructor: a variable.
              (ir :p-var type lexical-ctor nil))
             (lexical-ctor (con :eql-form lexical-ctor))
             ((and known-ctor
                   (eq global-ctor known-ctor)
                   (or (not (boundp known-ctor))
                       (eq (symbol-value known-ctor) known-ctor)))
              ;; A nullary constructor's value is its tag.
              (con :eq-tag known-ctor))
             (known-ctor (con :bound-eql global-ctor))
             ((sml-constructor-symbol-p global-ctor) (con :value-eql global-ctor))
             (static-ctor-p
              ;; Not registered yet, e.g. a constructor of a structure
              ;; declared earlier in the same program.
              (con :tag-p global-ctor))
             (t (ir :p-var type global-ctor nil)))))))))

(defun ir-lower-constructor-app-pattern (pat local-exceptions lexical-env type arg-type)
  "Constructor application pattern PAT, as COMPILE-PAT decides."
  (let* ((name (second (second pat)))
         (tag (lexical-constructor-tag-form name lexical-env))
         (ctor (constructor-symbol-for-name name lexical-env))
         (payload (ir-lower-pattern (third pat) local-exceptions lexical-env arg-type))
         (known (or (known-constructor-symbol-for-name name)
                    (sml-constructor-symbol-p ctor)))
         (arity (and known (constructor-arity-at-compile-time ctor))))
    (flet ((con (how data) (ir :p-con type name how data payload)))
      (cond
        ;; A constructor of the functor body being compiled.
        (tag (con :functor-tag tag))
        ((eql arity 0) (con :boxed ctor))
        (arity (con :flat (cons ctor arity)))
        ;; Its representation is not known here.
        (known (con :unknown-repr ctor))
        ;; A variable holding the constructor, e.g. a member of a
        ;; functor's argument.
        ((null (symbol-package ctor)) (con :variable ctor))
        ;; Resolved at run time (a member of a functor instance).
        (t (con :runtime ctor))))))

(defun ir-pattern-binders (pattern)
  "The variables PATTERN binds (uninterned symbols)."
  (let ((binders nil))
    (labels ((walk (p)
               (case (ir-kind p)
                 (:p-var (when (ir-variable-p (third p)) (push (third p) binders)))
                 (:p-as (when (ir-variable-p (third p)) (push (third p) binders))
                  (walk (fourth p)))
                 (:p-tuple (mapc #'walk (third p)))
                 (:p-record (dolist (field (fourth p)) (walk (third field))))
                 (:p-record-lookup (dolist (field (third p)) (walk (second field))))
                 (:p-cons (walk (third p)) (walk (fourth p)))
                 (:p-ref (walk (third p)))
                 (:p-exn (when (fifth p) (walk (fifth p))))
                 (:p-con (when (sixth p) (walk (sixth p)))))))
      (walk pattern))
    (nreverse binders)))

(defun ir-pattern-references (pattern)
  "The CL forms PATTERN evaluates (exception and constructor values)."
  (let ((forms nil))
    (labels ((walk (p)
               (case (ir-kind p)
                 (:p-as (walk (fourth p)))
                 (:p-tuple (mapc #'walk (third p)))
                 (:p-record (dolist (field (fourth p)) (walk (third field))))
                 (:p-record-lookup (dolist (field (third p)) (walk (second field))))
                 (:p-cons (walk (third p)) (walk (fourth p)))
                 (:p-ref (walk (third p)))
                 (:p-exn (push (fourth p) forms) (when (fifth p) (walk (fifth p))))
                 (:p-con (push (fifth p) forms) (when (sixth p) (walk (sixth p)))))))
      (walk pattern))
    forms))

(defun ir-lower-match-rules (rules local-exceptions lexical-env &optional scrutinee-type)
  "The ((pattern) body) rules of a one-scrutinee match."
  (mapcar (lambda (rule)
            (let* ((var-map (pattern-variable-map (first rule) local-exceptions))
                   (branch-env (append var-map lexical-env)))
              (list (list (ir-lower-pattern (first rule) local-exceptions branch-env
                                            scrutinee-type))
                    (ir-lower (second rule) local-exceptions branch-env))))
          rules))

(defun ir-lower-record (fields local-exceptions lexical-env type)
  "Like COMPILE-RECORD-EXPRESSION: fields are evaluated in source order."
  (multiple-value-bind (tuple-fields tuplep) (tuple-record-fields fields)
    (let* ((layout (if tuplep tuple-fields (record-fields-sorted-by-label fields)))
           (labels (mapcar #'first layout))
           (temporaries
             (unless (or (equal layout fields)
                         (<= (count-if-not #'effect-free-sml-expression-p
                                           fields :key #'second)
                             1))
               (mapcar (lambda (field) (gensym (first field))) fields)))
           (lowered (mapcar (lambda (field)
                              (ir-lower (second field) local-exceptions lexical-env))
                            fields))
           (field-nodes
             (mapcar (lambda (field)
                       (let ((node (nth (position field fields) lowered)))
                         (if temporaries
                             (ir :var (ir-type node) (nth (position field fields) temporaries))
                             node)))
                     layout))
           (type (or type (ir-record-type labels (mapcar #'ir-type field-nodes))))
           (node (if (loop for label in labels
                           for i from 1
                           always (string= label (princ-to-string i)))
                     (ir :tuple type field-nodes)
                     (ir :record type labels field-nodes))))
      (if temporaries
          (reduce (lambda (pair body) (ir :let type (car pair) (cdr pair) body))
                  (mapcar #'cons temporaries lowered)
                  :from-end t :initial-value node)
          node))))

;;; `let'

(defun ir-lower-let (ast local-exceptions lexical-env type)
  "Like COMPILE-LOCAL-DECLS."
  (let* ((decs (second ast))
         (body-asts (third ast))
         (module-bindings (declarations-local-module-member-bindings decs))
         (structure-prefixes (module-local-structure-prefixes
                              decs (or *sml-module-prefix* ""))))
    (let ((*sml-local-structure-members* (append module-bindings
                                                 *sml-local-structure-members*))
          (*sml-local-structure-prefixes* (append structure-prefixes
                                                  *sml-local-structure-prefixes*)))
      (ir-lower-declarations
       decs local-exceptions lexical-env
       (lambda (local-exceptions lexical-env)
         (let ((items (mapcar (lambda (e) (ir-lower e local-exceptions lexical-env))
                              body-asts)))
           (if (rest items) (ir :seq type items) (first items))))))))

(defun ir-lower-declarations (decs local-exceptions lexical-env body-function)
  "The local declarations DECS around the term BODY-FUNCTION returns, given
the exceptions and lexical environment after DECS (like
COMPILE-LOCAL-DECLS-INTO-BODY)."
  (if (null decs)
      (funcall body-function local-exceptions lexical-env)
      (let* ((dec (first decs))
             (extended-exceptions (extend-local-exceptions local-exceptions dec))
             (dec-bindings (if (eq (car dec) :open)
                               (open-shadowing-lexical-bindings dec lexical-env)
                               (local-declaration-lexical-bindings dec)))
             (open-symbol-bindings (local-open-symbol-bindings dec))
             (open-constructor-bindings (local-open-constructor-bindings dec)))
        (ir-lower-declaration
         dec local-exceptions lexical-env
         (lambda ()
           (let ((*sml-binding-symbol-env*
                   (append open-symbol-bindings *sml-binding-symbol-env*))
                 (*sml-constructor-symbol-env*
                   (append (local-datatype-constructor-bindings dec)
                           open-constructor-bindings
                           *sml-constructor-symbol-env*))
                 (*sml-local-constructor-arities*
                   (append (local-datatype-constructor-arities dec)
                           *sml-local-constructor-arities*)))
             (ir-lower-declarations (rest decs) extended-exceptions
                                    (append dec-bindings lexical-env)
                                    body-function)))))))

(defun ir-lower-val-binding (pat value bindings local-exceptions lexical-env body)
  "PAT bound to the term VALUE around the term BODY (like
COMPILE-LOCAL-VAL-BINDING)."
  (let ((type (ir-type body)))
    (cond
      ((and (listp pat) (member (car pat) '(:pat-var :var)))
       (ir :let type (local-binding-symbol (second pat) bindings) value body))
      ((eq pat :wild)
       (ir :let type (gensym "IGNORED") value body))
      (t
       (let ((tmp (gensym "MATCHED")))
         (ir :let type tmp value
             (ir :match type (list (ir :var (ir-type value) tmp))
                 (list (list (list (let ((*sml-module-prefix* nil))
                                     (ir-lower-pattern pat local-exceptions
                                                       (append bindings lexical-env)
                                                       (ir-type value))))
                             body))
                 (ir-failure "Bind" "val" pat))))))))

(defvar *ir-nested-structures* (make-portable-weak-eq-hash-table)
  "A structure declared by `let' or `local' (its declaration's AST) -> the
IR declarations recorded for it, while a program's are (*SML-IR-PROGRAM*).")

(defun ir-lower-declaration (dec local-exceptions lexical-env body-thunk)
  "DEC, a declaration of a `let' or `local', around the term BODY-THUNK
returns, as COMPILE-LOCAL-DECL compiles it."
  (let ((bindings (local-declaration-lexical-bindings dec)))
    (ecase (car dec)
      ((:type :infix :datatype-replication :expr :signature :open)
       (funcall body-thunk))
      (:val
       (let ((value (ir-lower (third dec) local-exceptions lexical-env)))
         (ir-lower-val-binding (second dec) value bindings local-exceptions lexical-env
                               (funcall body-thunk))))
      (:vals
       ;; `val p1 = e1 and p2 = e2' evaluates every expression before
       ;; binding anything, like `val (p1, p2) = (e1, e2)'; `val rec'
       ;; bindings joined to them come last.
       (let* ((vals (remove-if-not (lambda (val) (eq (car val) :val)) (cdr dec)))
              (recs (remove :val (cdr dec) :key #'car))
              (values (mapcar (lambda (val) (ir-lower (third val) local-exceptions lexical-env))
                              vals))
              (body (if recs
                        (ir-lower-declarations recs local-exceptions lexical-env
                                               (lambda (local-exceptions lexical-env)
                                                 (declare (ignore local-exceptions lexical-env))
                                                 (funcall body-thunk)))
                        (funcall body-thunk))))
         (ir-lower-val-binding `(:pat-tuple ,@(mapcar #'second vals))
                               (ir :tuple (ir-tuple-type (mapcar #'ir-type values)) values)
                               bindings local-exceptions lexical-env body)))
      ((:fun :funs)
       (let* ((funs (if (eq (car dec) :fun) (list dec) (cdr dec)))
              (env (append bindings lexical-env))
              (functions
                (mapcar (lambda (fun-dec)
                          (list (local-binding-symbol (second fun-dec) bindings)
                                (ir-lower-local-fun fun-dec local-exceptions env)))
                        funs))
              (body (funcall body-thunk)))
         (ir :letrec (ir-type body) functions body)))
      ((:val-rec :val-recs)
       (let* ((recs (if (eq (car dec) :val-rec) (list dec) (cdr dec)))
              (env (append bindings lexical-env))
              (values (mapcar (lambda (rec)
                                (list (local-binding-symbol (second rec) bindings)
                                      (ir-lower (third rec) local-exceptions env)))
                              recs))
              (body (funcall body-thunk))
              (letrec (ir :letrec (ir-type body) values body))
              (failure (val-recs-bind-failure recs)))
         ;; A binding that rebinds a constructor raises Bind first.
         (if failure
             (ir :decl (ir-type body) (list :module failure nil) letrec)
             letrec)))
      (:datatype
       (let ((body (funcall body-thunk)))
         (ir :decl (ir-type body)
             (list :datatype
                   (mapcar (lambda (ctor)
                             (list (local-binding-symbol (second ctor) bindings)
                                   (local-datatype-constructor-tag ctor)
                                   (and (fourth ctor) t)
                                   (second ctor)))
                           (third dec))
                   ;; For the Coalton backend, which lifts the datatypes.
                   (and *sml-ir-program* (ir-datatype-entries dec)))
             body)))
      (:exception
       (let ((body (funcall body-thunk)))
         (ir :decl (ir-type body)
             (list :exception (local-binding-symbol (second dec) bindings) (second dec)
                   (and (getf (cddr dec) :arg-type) t))
             body)))
      (:exception-alias
       (let ((target (resolved-sml-symbol (third dec) lexical-env))
             ;; Also a member of a structure the functor body names.
             (lexicalp (or (and (lexical-symbol-for-name (third dec) lexical-env) t)
                           (and (functor-scope-long-name-variable (third dec)) t)))
             (body (funcall body-thunk)))
         (ir :decl (ir-type body)
             (list :exception-alias (local-binding-symbol (second dec) bindings)
                   (second dec) target lexicalp)
             body)))
      ((:structure :structure-alias :structure-app :functor)
       ;; A structure declared by `let' or `local' is a module like any
       ;; other, compiled as a declaration, whose members are then made
       ;; lexical (LEXICAL-STRUCTURE-FORM); the scope of its name is the
       ;; compiler's business.
       (when *ir-template-mode*
         (throw 'ir-no-template nil))
       (let ((form (ir-compile-nested-structure
                    dec (lambda () (compile-decl dec local-exceptions lexical-env))))
             (body (funcall body-thunk)))
         (ir :decl (ir-type body)
             (list :module form (gethash dec *ir-nested-structures*))
             body)))
      (:local
       (let* ((outer (list *sml-binding-symbol-env* *sml-constructor-symbol-env*
                           *sml-local-constructor-arities* *sml-local-structure-members*
                           *sml-local-structure-prefixes*))
              (local-decs (second dec))
              (body-decs (third dec))
              (module-bindings (declarations-local-module-member-bindings local-decs))
              (structure-prefixes (module-local-structure-prefixes
                                   local-decs (or *sml-module-prefix* "")))
              (open-symbol-bindings (declarations-local-open-symbol-bindings local-decs))
              (open-constructor-bindings
                (declarations-local-open-constructor-bindings local-decs)))
         (let ((*sml-local-structure-members* (append module-bindings
                                                      *sml-local-structure-members*))
               (*sml-local-structure-prefixes* (append structure-prefixes
                                                       *sml-local-structure-prefixes*)))
           (ir-lower-declarations
            local-decs local-exceptions lexical-env
            (lambda (inner-exceptions inner-env)
              (let ((*sml-binding-symbol-env*
                      (append open-symbol-bindings *sml-binding-symbol-env*))
                    (*sml-constructor-symbol-env*
                      (append open-constructor-bindings *sml-constructor-symbol-env*)))
                (ir-lower-declarations
                 body-decs inner-exceptions inner-env
                 (lambda (local-exceptions lexical-env)
                   (declare (ignore local-exceptions lexical-env))
                   ;; What follows `local ... in ... end' is lowered in
                   ;; the environment around it.
                   (destructuring-bind (*sml-binding-symbol-env*
                                        *sml-constructor-symbol-env*
                                        *sml-local-constructor-arities*
                                        *sml-local-structure-members*
                                        *sml-local-structure-prefixes*)
                       outer
                     (funcall body-thunk)))))))))))))

(defun ir-lower-local-fun (fun-dec local-exceptions lexical-env)
  "The term of the function a `fun' of a `let' or `local' declares: like a
top-level one's worker (COMPILE-TUPLED-TOP-LEVEL-FUN), a function of the
fields of its argument when its clauses all take a tuple of the same
width (the :fn then has several parameters; IR-EMIT-LOCAL-FUNCTIONS)."
  (let ((arity (and *sml-local-workers* *sml-tupled-workers*
                    (tupled-fun-arity (third fun-dec)))))
    (if arity
        (let ((*ir-fun-type-node* (third fun-dec)))
          (ir-lower-fn-clauses (mapcar (lambda (clause)
                                         (cons (tuple-pattern-subpatterns (first (first clause)))
                                               (rest clause)))
                                       (third fun-dec))
                               local-exceptions (second fun-dec) lexical-env t))
        (ir-lower-fn-clauses (third fun-dec) local-exceptions (second fun-dec) lexical-env))))

;;; Functions of clauses (`fun', and `fn' through IR-LOWER)

(defun ir-lower-fn-clauses (clauses local-exceptions function-name outer-lexical-env
                            &optional tupled)
  "The term of the curried function CLAUSES define, or, when TUPLED, of
the function of N arguments taking the fields of the tuple each clause's
one pattern matched (CLAUSES then have N patterns, the tuple's; see
TUPLED-FUN-ARITY); as
COMPILE-FN-CLAUSES-OF-ARITY compiles it."
  (let* ((arity (length (first (first clauses))))
         (fun-type (or (ir-node-type *ir-fun-type-node*)
                       (ir-node-type clauses)))
         (*ir-fun-type-node* nil)
         (param-types (if tupled
                          (ir-tuple-field-types (ir-arrow-domain fun-type) arity)
                          (loop repeat arity
                                for type = fun-type then (ir-arrow-range type)
                                collect (ir-arrow-domain type))))
         (result-type (if tupled
                          (ir-arrow-range fun-type)
                          (loop repeat arity
                                for type = fun-type then (ir-arrow-range type)
                                finally (return (ir-arrow-range type)))))
         (tmp-args (loop repeat arity collect (gensym "ARG")))
         ;; Fresh variables for the loop: the curried parameters are closed
         ;; over by partial applications, so they must not be assigned.
         (loop-args (loop repeat arity collect (gensym "LOOP-ARG")))
         ;; Not a gensym: ABCL's pretty printer loses the identity of an
         ;; uninterned TAGBODY tag in emitted artifacts.  An inner
         ;; function's loop shadows the tag, and only its own body jumps to
         ;; it.
         (loop-tag '%self-tail-call)
         (looping nil)
         (bodies
           (mapcar (lambda (clause)
                     (if (and (stringp function-name)
                              (notany (lambda (pat)
                                        (member function-name (pattern-bound-names pat)
                                                :test #'equal))
                                      (first clause)))
                         (multiple-value-bind (body found)
                             (mark-sml-self-tail-calls (second clause) function-name
                                                       arity (cons loop-tag loop-args)
                                                       tupled)
                           (when found (setf looping t))
                           body)
                         (second clause)))
                   clauses))
         (args (if looping loop-args tmp-args))
         (*ir-tail-call-type* result-type)
         (match
           (ir :match result-type
               (mapcar (lambda (arg type) (ir :var type arg)) args param-types)
               (mapcar (lambda (clause body)
                         (let* ((pat (compile-clause-pattern (first clause)))
                                (var-map (pattern-variable-map pat local-exceptions))
                                (branch-env (append var-map outer-lexical-env)))
                           (list (mapcar (lambda (param type)
                                           (ir-lower-pattern param local-exceptions
                                                             branch-env type))
                                         (first clause) param-types)
                                 (ir-lower body local-exceptions branch-env))))
                       clauses bodies)
               (ir-failure "Match" (or function-name "fn") (first (first clauses)))))
         (body (if looping
                   (ir :loop result-type loop-tag loop-args
                       (mapcar (lambda (arg type) (ir :var type arg)) tmp-args param-types)
                       match)
                   match)))
    (if tupled
        (ir :fn fun-type tmp-args body function-name)
        (let ((types (loop repeat arity
                           for type = fun-type then (ir-arrow-range type)
                           collect type)))
          (reduce (lambda (pair body)
                    (ir :fn (cdr pair) (list (car pair)) body function-name))
                  (mapcar #'cons tmp-args types)
                  :from-end t :initial-value body)))))

(defun ir-lower-name (ast name lexical-env)
  "A variable or constructor occurrence NAME."
  (let ((form (resolved-sml-symbol name lexical-env)))
    (if (ir-variable-p form)
        (ir :var (ir-node-type ast) form)
        (ir :ref (ir-node-type ast) form))))

(defun ir-lower-selector-function (ast type)
  "`#lab' as a function value."
  (let ((record (gensym "RECORD")))
    (ir :fn type (list record)
        (ir-select-node ast (ir :var (ir-arrow-domain type) record) (ir-arrow-range type))
        nil)))

(defun ir-select-node (selector record type)
  "`#lab' (SELECTOR) of the term RECORD, like COMPILE-SELECTION."
  (let* ((label (second selector))
         (labels (sml-node-record-labels selector))
         (index (and labels (position label labels :test #'string=))))
    (if index
        (ir :select type (1+ index) record)
        (ir :select-label type label record))))

(defun ir-lower-overloaded-value (name tyname type)
  "Overloaded identifier NAME at TYNAME as a function value (see
COMPILE-OVERLOADED-IDENTIFIER-VALUE), or NIL."
  (if (member name '("~" "abs") :test #'string=)
      (let ((x (gensym "X")))
        (and (compile-overloaded-operation name tyname (list x))
             (ir :fn type (list x)
                 (ir :prim (ir-arrow-range type) (list :overloaded name tyname)
                     (list (ir :var (ir-arrow-domain type) x)))
                 nil)))
      (let ((pair (gensym "PAIR"))
            (domain (ir-arrow-domain type)))
        (and (compile-overloaded-operation name tyname (list (gensym) (gensym)))
             (ir :fn type (list pair)
                 (ir :prim (ir-arrow-range type) (list :overloaded name tyname)
                     (list (ir :select (ir-field-type domain "1") 1 (ir :var domain pair))
                           (ir :select (ir-field-type domain "2") 2 (ir :var domain pair))))
                 nil)))))

(defun ir-fill-type (node)
  "NODE, given the type of its body or rules when it has none (a node the
checker did not see, e.g. one rebuilt by the compiler)."
  (unless (ir-type node)
    (let ((type (case (ir-kind node)
                  (:match (some (lambda (rule) (ir-type (second rule))) (fourth node)))
                  (:handle (ir-type (third node)))
                  (:seq (ir-type (car (last (third node)))))
                  (:if (or (ir-type (fourth node)) (ir-type (fifth node))))
                  (:let (ir-type (fifth node)))
                  ((:letrec :decl) (ir-type (fourth node))))))
      (when type
        (setf (second node) type))))
  node)

(defun ir-lower (ast local-exceptions lexical-env)
  "The IR term of expression AST (see COMPILE-EXPR-DIRECT, which this
follows case by case)."
  (ir-fill-type (ir-lower-1 ast local-exceptions lexical-env)))

(defun ir-lower-1 (ast local-exceptions lexical-env)
  (let ((type (ir-node-type ast)))
    (flet ((sub (ast) (ir-lower ast local-exceptions lexical-env)))
      (cond
        ((or (numberp ast) (stringp ast) (characterp ast))
         (ir :const (or type (ir-constant-type ast)) ast))
        ((atom ast) (error "Unknown AST: ~A" ast))
        (t
         (case (car ast)
           (:var
            ;; `op +' and the like as values.
            (multiple-value-bind (name tyname) (overloaded-identifier-instance ast)
              (or (and name (ir-lower-overloaded-value name tyname type))
                  (ir-lower-name ast (second ast) lexical-env))))
           (:op-var
            (let ((var `(:var ,(second ast))))
              (multiple-value-bind (name tyname) (overloaded-identifier-instance ast)
                (or (and name (ir-lower-overloaded-value name tyname type))
                    (let ((node (ir-lower var local-exceptions lexical-env)))
                      (setf (second node) (or (second node) type))
                      node)))))
           (:word (ir :const (or type (ir-base-type "word")) (second ast)))
           (:ctor
            (cond ((string= (second ast) "true") (ir :const type t))
                  ((member (second ast) '("false" "nil") :test #'string=) (ir :const type nil))
                  (t (ir-lower-name ast (second ast) lexical-env))))
           (:typed (let ((node (sub (second ast))))
                     (if (and type (null (ir-type node)))
                         (list* (ir-kind node) type (cddr node))
                         node)))
           (:selector (ir-lower-selector-function ast type))
           (:deref (ir :prim type '(:deref) (list (sub (second ast)))))
           (:infix-app
            (let ((left (sub (third ast)))
                  (right (sub (fourth ast))))
              (ir :app type (ir-lower `(:var ,(second ast)) local-exceptions lexical-env)
                  (ir :tuple (ir-tuple-type (list (ir-type left) (ir-type right)))
                      (list left right)))))
           (:app (ir-lower-application ast local-exceptions lexical-env type))
           (:case
            (let ((scrutinee (sub (second ast))))
              (ir :match type (list scrutinee)
                  (ir-lower-match-rules (cddr ast) local-exceptions lexical-env
                                        (ir-type scrutinee))
                  (ir-failure "Match" "case" (first (first (cddr ast)))))))
           (:handle
            (ir :handle type (sub (second ast))
                (mapcar (lambda (rule) (list (first (first rule)) (second rule)))
                        (ir-lower-match-rules (third ast) local-exceptions lexical-env
                                              (ir-base-type "exn")))))
           (:if (ir :if type (sub (second ast)) (sub (third ast)) (sub (fourth ast))))
           (:while (ir :while type (sub (second ast)) (sub (third ast))))
           (:raise (ir :raise type (sub (second ast))))
           (:let (ir-lower-let ast local-exceptions lexical-env type))
           (:andalso (ir :if type (sub (second ast)) (sub (third ast)) (ir :const type nil)))
           (:orelse (ir :if type (sub (second ast)) (ir :const type t) (sub (third ast))))
           (:seq (ir :seq type (mapcar #'sub (cdr ast))))
           (:list (ir :prim type '(:list) (mapcar #'sub (cdr ast))))
           (:record (ir-lower-record (cdr ast) local-exceptions lexical-env type))
           (:unit (ir :unit (or type (ir-tuple-type nil))))
           (:tuple (let ((items (mapcar #'sub (cdr ast))))
                     (ir :tuple (or type (ir-tuple-type (mapcar #'ir-type items))) items)))
           (:self-tail-call
            (when *ir-template-mode*
              (throw 'ir-no-template nil))
            (ir :tail-call (or type *ir-tail-call-type*) (second ast)
                (mapcar #'sub (cddr ast))))
           (:fn
            (let ((clauses (second ast))
                  (arg (gensym "ARG"))
                  (domain (ir-arrow-domain type)))
              (ir :fn type (list arg)
                  (ir :match (ir-arrow-range type) (list (ir :var domain arg))
                      (ir-lower-match-rules clauses local-exceptions lexical-env domain)
                      (ir-failure "Match" "fn" (first (first clauses))))
                  nil)))
           (otherwise (error "Unknown AST: ~A" ast))))))))

(defun ir-lower-application (ast local-exceptions lexical-env type)
  (destructuring-bind (head arg) (cdr ast)
    (cond
      ;; `~' applied to a word literal is modular negation.
      ((and (equal head '(:var "~")) (consp arg) (eq (car arg) :word))
       (ir :const (or type (ir-base-type "word")) (logand +sml-word-mask+ (- (second arg)))))
      ((ir-lower-overloaded ast local-exceptions lexical-env type))
      ((ir-lower-equality ast local-exceptions lexical-env type))
      ((ir-lower-binary-primitive ast local-exceptions lexical-env type))
      ((and (consp head) (eq (car head) :selector))
       (ir-select-node head (ir-lower arg local-exceptions lexical-env) type))
      ((and (consp head) (eq (car head) :ctor)
            (not (exception-constructor-info (second head) local-exceptions))
            (or (lexical-constructor-tag-form (second head) lexical-env)
                (known-data-constructor-application-p (second head) lexical-env)))
       (ir-lower-constructor-application head arg local-exceptions lexical-env type))
      ((ir-lower-tupled-call head arg local-exceptions lexical-env type))
      (t (ir :app type (ir-lower head local-exceptions lexical-env)
             (ir-lower arg local-exceptions lexical-env))))))

;;; Emission

(defvar *ir-emit-scope* nil
  "The variables in scope where the node being emitted will be: those of
the lexical environment the term was compiled in, and those its binders
bind around the node.  A hoisted case action takes the ones it uses.")

(defmacro with-ir-scope ((symbols) &body body)
  `(let ((*ir-emit-scope* (append ,symbols *ir-emit-scope*)))
     ,@body))

(defun ir-emit-list (nodes) (mapcar #'ir-emit nodes))

(defun ir-emit-pattern (pattern)
  "The trivia pattern of IR PATTERN (as COMPILE-PAT compiles its AST)."
  (ecase (ir-kind pattern)
    (:p-wild '_)
    (:p-var (third pattern))
    (:p-const (third pattern))
    (:p-as (let ((alias (third pattern)))
             `(guard1 ,alias t ,alias ,(ir-emit-pattern (fourth pattern)))))
    (:p-tuple (tuple-pattern-form (mapcar #'ir-emit-pattern (third pattern))))
    (:p-record
     (indexed-record-pattern (mapcar (lambda (field)
                                       (cons (first field) (ir-emit-pattern (third field))))
                                     (fourth pattern))
                             (third pattern)))
    (:p-record-lookup
     (let ((record (gensym "RECORD")))
       ;; SML-RECORD-FIELD is NIL, not an error, on a value without the
       ;; field (see INDEXED-RECORD-PATTERN).
       `(guard1 ,record t
                ,@(mapcan (lambda (field)
                            `((sml-record-field ,record ,(first field))
                              ,(ir-emit-pattern (second field))))
                          (third pattern)))))
    (:p-cons `(cons ,(ir-emit-pattern (third pattern)) ,(ir-emit-pattern (fourth pattern))))
    (:p-ref (let ((it (gensym "REF")))
              `(guard1 ,it
                       (and (vectorp ,it)
                            (= (length ,it) 2)
                            (eq (aref ,it 0) :ref))
                       (aref ,it 1) ,(ir-emit-pattern (third pattern)))))
    (:p-exn (destructuring-bind (name form payload) (cddr pattern)
              (declare (ignore name))
              (let ((it (gensym "EXN")))
                (if payload
                    `(guard1 ,it
                             (and (consp ,it)
                                  (sml-exception-tag-p (car ,it))
                                  (eq (sml-exception-constructor-tag ,it)
                                      (sml-exception-constructor-tag ,form)))
                             (cdr ,it) ,(ir-emit-pattern payload))
                    `(guard1 ,it
                             (and (sml-exception-tag-p ,it)
                                  (eq (sml-exception-constructor-tag ,it)
                                      (sml-exception-constructor-tag ,form))))))))
    (:p-con (ir-emit-constructor-pattern pattern))))

(defun ir-emit-constructor-pattern (pattern)
  (destructuring-bind (name how data payload) (cddr pattern)
    (declare (ignore name))
    (let ((it (gensym (if payload "CTOR-VALUE" "CTOR"))))
      (ecase how
        ;; Nullary constructors.
        (:eql-form `(guard1 ,it (eql ,it ,data)))
        (:eq-tag `(guard1 ,it (eq ,it ',data)))
        (:bound-eql `(guard1 ,it (and (boundp ',data) (eql ,it (symbol-value ',data)))))
        (:value-eql `(guard1 ,it (eql ,it (symbol-value ',data))))
        (:tag-p `(guard1 ,it (sml-constructor-tag-p ,it ',data)))
        ;; Constructors applied to PAYLOAD.
        (:functor-tag `(guard1 ,it (and (consp ,it) (eq (car ,it) ,data))
                               (cdr ,it) ,(ir-emit-pattern payload)))
        (:boxed `(cons ',data ,(ir-emit-pattern payload)))
        (:flat
         (destructuring-bind (tag . arity) data
           ;; The tag is tested as a subpattern, after the type, so that
           ;; trivia can infer the types of the tests when it reorders
           ;; clauses.
           (let ((tag-pattern (let ((tag-var (gensym "TAG")))
                                `(guard1 ,tag-var (eq ,tag-var ',tag)))))
             (if (and (eq (ir-kind payload) :p-tuple) (= (length (third payload)) arity))
                 `(guard1 ,it ,(vector-width-test it (1+ arity))
                          (svref ,it 0) ,tag-pattern
                          ,@(loop for subpattern in (third payload)
                                  for i from 1
                                  for compiled = (ir-emit-pattern subpattern)
                                  unless (eq compiled '_)
                                    append `((svref ,it ,i) ,compiled)))
                 `(guard1 ,it ,(vector-width-test it (1+ arity))
                          (svref ,it 0) ,tag-pattern
                          (sml-flat-constructed-argument ,it)
                          ,(ir-emit-pattern payload))))))
        (:unknown-repr `(guard1 ,it (eq (sml-constructed-tag ,it) ',data)
                                (sml-constructed-argument ,it) ,(ir-emit-pattern payload)))
        (:variable `(guard1 ,it (eq (sml-constructed-tag ,it)
                                    (sml-constructor-value-tag ,data))
                            (sml-constructed-argument ,it) ,(ir-emit-pattern payload)))
        ;; The value may be a nullary constructor (a symbol) of the same
        ;; datatype, so test for a cons rather than assert one.
        (:runtime `(guard1 ,it (let ((tag (sml-constructed-tag ,it)))
                                 (and tag (sml-constructor-tag-p tag ',data)))
                           (sml-constructed-argument ,it) ,(ir-emit-pattern payload)))))))

(defun ir-irrefutable-bindings (pattern form)
  "The LET* bindings that match PATTERN against FORM (a variable or an
access to one), or :REFUTABLE when PATTERN may fail (ROADMAP item 4).
Variables of real type are bound as double-floats, which SBCL can keep
unboxed."
  (case (ir-kind pattern)
    (:p-wild nil)
    (:p-var (if (ir-variable-p (third pattern))
                (list (list (third pattern)
                            (if (eq (fourth pattern) :real) `(sml-double ,form) form)))
                :refutable))
    ((:p-tuple :p-record)
     (let ((record (gensym "RECORD"))
           (bindings nil))
       (loop for (index . subpattern)
               in (if (eq (ir-kind pattern) :p-tuple)
                      (loop for subpattern in (third pattern)
                            for i from 1
                            collect (cons i subpattern))
                      (mapcar (lambda (field) (cons (first field) (third field)))
                              (fourth pattern)))
             do (let ((inner (ir-irrefutable-bindings subpattern `(svref ,record ,index))))
                  (when (eq inner :refutable)
                    (return-from ir-irrefutable-bindings :refutable))
                  (setf bindings (append bindings inner))))
       (and bindings (cons (list record form) bindings))))
    (t :refutable)))

(defun ir-literal-key-p (pattern)
  (and (eq (ir-kind pattern) :p-const)
       (not (stringp (third pattern)))))

(defun ir-literal-match-p (scrutinees rules)
  "Is the match a `case' on literals (LITERAL-CASE-BRANCHES-P)?"
  (and (= (length scrutinees) 1)
       rules
       (loop for (patterns) in rules
             for pattern = (first patterns)
             do (cond ((eq (ir-kind pattern) :p-wild) (return t))
                      ((not (ir-literal-key-p pattern)) (return nil)))
             finally (return t))))

(defun ir-dispatch-key (pattern)
  "The integer that the first field of tuple PATTERN must be, or NIL."
  (and (eq (ir-kind pattern) :p-tuple)
       (let ((first (first (third pattern))))
         (and (eq (ir-kind first) :p-const) (integerp (third first))
              (third first)))))

(defun ir-dispatch-match-p (scrutinees rules)
  "Is the match a large dispatch on the first field of a tuple
(TUPLE-DISPATCH-CASE-BRANCHES-P)?"
  (and (= (length scrutinees) 1)
       (>= (length rules) 12)
       (eq (ir-kind (first (first (car (last rules))))) :p-wild)
       (every (lambda (rule) (ir-dispatch-key (first (first rule)))) (butlast rules))))

(defun ir-emit-rule-body (patterns body)
  (with-ir-scope ((mapcan #'ir-pattern-binders patterns))
    (ir-emit body)))

(defun ir-emit-failure (failure)
  `(sml-raise-match-failure ,@failure))

(defun ir-emit-match (node)
  (destructuring-bind (scrutinees rules failure) (cddr node)
    (cond
      ;; One rule whose patterns cannot fail: bind without a match.
      ((and (= (length rules) 1)
            (let ((patterns (first (first rules))))
              (notany (lambda (pattern)
                        (eq (ir-irrefutable-bindings pattern 'x) :refutable))
                      patterns)))
       (destructuring-bind (patterns body) (first rules)
         (let* ((temporaries nil)
                (bindings
                  (loop for scrutinee in scrutinees
                        for pattern in patterns
                        append (let ((form (if (eq (ir-kind scrutinee) :var)
                                               (third scrutinee)
                                               (let ((temporary (gensym "VALUE")))
                                                 (push (list temporary (ir-emit scrutinee))
                                                       temporaries)
                                                 temporary))))
                                 (ir-irrefutable-bindings pattern form))))
                (bindings (append (nreverse temporaries) bindings)))
           `(let* ,bindings
              (declare (ignorable ,@(mapcar #'first bindings)))
              ,(ir-emit-rule-body patterns body)))))
      ((ir-dispatch-match-p scrutinees rules) (ir-emit-dispatch-match node))
      ((and *sml-decision-trees*
            (ir-emit-decision-tree (ir-emit-list scrutinees) rules (ir-emit-failure failure))))
      ((ir-literal-match-p scrutinees rules) (ir-emit-literal-match node))
      ((= (length scrutinees) 1)
       `(trivia:match ,(ir-emit (first scrutinees))
          ,@(mapcar (lambda (rule)
                      (destructuring-bind (patterns body) rule
                        (list (ir-emit-pattern (first patterns))
                              (ir-emit-rule-body patterns body))))
                    rules)
          (_ ,(ir-emit-failure failure))))
      (t
       ;; Curried arguments are matched as multiple values, without
       ;; allocating a tuple of them.
       `(trivia:match* ,(ir-emit-list scrutinees)
          ,@(mapcar (lambda (rule)
                      (destructuring-bind (patterns body) rule
                        (list (mapcar #'ir-emit-pattern patterns)
                              (ir-emit-rule-body patterns body))))
                    rules)
          (,(make-list (length scrutinees) :initial-element '_)
           ,(ir-emit-failure failure)))))))

(defun ir-emit-literal-match (node)
  "A `case' on literals as a CL CASE (COMPILE-LITERAL-CASE)."
  (destructuring-bind (scrutinees rules failure) (cddr node)
    (let ((clauses nil)
          (has-default nil))
      (dolist (rule rules)
        (destructuring-bind ((pattern) body) rule
          (if (eq (ir-kind pattern) :p-wild)
              (progn
                (push `(otherwise ,(compile-case-action-thunk (ir-emit body))) clauses)
                (setf has-default t)
                (return))
              (push `((,(third pattern)) ,(compile-case-action-thunk (ir-emit body)))
                    clauses))))
      `(case ,(ir-emit (first scrutinees))
         ,@(nreverse clauses)
         ,@(unless has-default
             `((otherwise ,(ir-emit-failure failure))))))))

(defun ir-hoisted-parameters (form)
  "The parameters of a function hoisted out of the term being emitted,
whose body is FORM (see HOISTED-FUNCTION-PARAMETERS)."
  (hoisted-function-parameters (mapcar (lambda (symbol) (cons nil symbol)) *ir-emit-scope*)
                               form))

(defun ir-emit-dispatch-match (node)
  "A large match on the first field of a tuple, as a CL CASE on that field
whose groups of rules are hoisted functions (COMPILE-TUPLE-DISPATCH-CASE)."
  (destructuring-bind (scrutinees rules failure) (cddr node)
    (declare (ignore failure))
    (let* ((value (gensym "CASE-VALUE"))
           (fallback (gensym "CASE-FALLBACK"))
           (default-rule (car (last rules)))
           (groups nil))
      (dolist (rule (butlast rules))
        (let* ((key (ir-dispatch-key (first (first rule))))
               (group (assoc key groups)))
          (if group
              (setf (cdr group) (append (cdr group) (list rule)))
              (setf groups (append groups (list (list key rule)))))))
      `(let ((,value ,(ir-emit (first scrutinees)))
             (,fallback (lambda () ,(ir-emit (second default-rule)))))
         (case (svref ,value 1)
           ,@(mapcar (lambda (group)
                       `((,(first group))
                         ,(with-ir-scope ((list value fallback))
                            (ir-emit-dispatch-group (rest group) value fallback))))
                     groups)
           (otherwise (funcall ,fallback)))))))

(defun ir-emit-dispatch-group (rules value fallback)
  (let* (;; Hoisted functions are global: they see the local functions in
         ;; scope as variables, which they take as parameters.
         (*ir-local-functions* (if *sml-hoisting-enabled* nil *ir-local-functions*))
         (clauses
          (mapcar (lambda (rule)
                    (destructuring-bind ((pattern) body) rule
                      (list (ir-emit-pattern pattern)
                            (with-ir-scope ((ir-pattern-binders pattern))
                              (let ((form (ir-emit body)))
                                (if *sml-hoisting-enabled*
                                    (let ((name (next-hoisted-sml-function-name))
                                          (parameters (ir-hoisted-parameters form)))
                                      (push `(defun ,name ,parameters
                                               ,@(portable-tail-call-declarations)
                                               ,form)
                                            *sml-hoisted-forms*)
                                      `(,name ,@parameters))
                                    form))))))
                  rules)))
    (if *sml-hoisting-enabled*
        (let* ((name (next-hoisted-sml-function-name))
               (dispatch-argument (gensym "DISPATCH-VALUE"))
               (fallback-argument (gensym "DISPATCH-FALLBACK"))
               (parameters (remove-if (lambda (symbol) (member symbol (list value fallback)))
                                      (ir-hoisted-parameters clauses))))
          (push `(defun ,name (,dispatch-argument ,fallback-argument ,@parameters)
                   (trivia:match ,dispatch-argument
                     ,@clauses
                     (_ (funcall ,fallback-argument))))
                *sml-hoisted-forms*)
          `(,name ,value ,fallback ,@parameters))
        `(trivia:match ,value
           ,@clauses
           (_ (funcall ,fallback))))))

;;; Decision trees (ROADMAP item 11; Maranget, "Compiling pattern matching
;;; to good decision trees", ML Workshop 2008).  A match is a matrix of
;;; rows (the patterns of a rule, the variables they have bound so far, the
;;; rule) over columns (the CL forms of the subterms still to be tested),
;;; split on one column at a time, so that each subterm is tested once.
;;; Tuples, records and refs are taken apart, not tested.  Constants and
;;; the constructors whose tags are known at compile time (nullary, boxed
;;; and flat ones, `::' and `nil') exclude each other: a CASE (or an IF) on
;;; the value or its tag has a branch for each of them, which keeps the
;;; rows that may match it.  An exception constructor, or a constructor
;;; known only at run time (of a local datatype, a functor body or a
;;; functor's argument), may have the same value under another name, so it
;;; is tested alone, and in the branch where it matches, the rows of the
;;; others keep their column, to be tested later.  Rules reached from
;;; several leaves become local functions (FLET), unless they are small.

(defvar *sml-decision-trees* t
  "When true, matches are compiled to decision trees (IR-EMIT-DECISION-TREE);
when NIL, rule by rule by trivia, as before.")

(defvar *ir-shared-action-size* 8
  "Rules whose bodies are larger than this (in IR nodes) and are reached
from several leaves of a decision tree are local functions.")

(defvar *dt-tests* 0
  "The number of tests of the decision tree being built.")

(defvar *dt-test-limit* 0
  "The most tests a decision tree may have; beyond it, the match is
compiled by trivia instead.")

(defvar *dt-leaf-marker* nil
  "The car of the leaves of the tree being built, (marker rule bindings).")

(defvar *dt-leaf-counts* nil
  "For each rule, how many leaves of the tree being built reach it.")

(defvar *dt-failure* nil
  "The form of the tree being built that raises when no rule matches.")

(defparameter +dt-wild+ (ir :p-wild nil))

(defstruct (dt-row (:constructor make-dt-row (patterns bindings rule)))
  "A row of a clause matrix: PATTERNS, one per column, none of them a
variable or layered; the BINDINGS ((symbol representation form) ...) of
the variables matched so far, and the index of its RULE."
  patterns bindings rule)

(defun dt-wild-p (pattern) (eq (ir-kind pattern) :p-wild))

(defun dt-peel (pattern form bindings)
  "PATTERN, matched against FORM, without its variable or layers, which are
added to BINDINGS; returns the rest and the bindings."
  (loop
    (case (ir-kind pattern)
      (:p-var (return (values +dt-wild+
                              (cons (list (third pattern) (fourth pattern) form) bindings))))
      (:p-as (push (list (third pattern) nil form) bindings)
       (setf pattern (fourth pattern)))
      (t (return (values pattern bindings))))))

(defun dt-row (patterns forms bindings rule)
  (let ((peeled nil))
    (loop for pattern in patterns
          for form in forms
          do (multiple-value-bind (rest more) (dt-peel pattern form bindings)
               (push rest peeled)
               (setf bindings more)))
    (make-dt-row (nreverse peeled) bindings rule)))

(defun dt-replace (list index items)
  "LIST with its element at INDEX replaced by the elements ITEMS."
  (append (subseq list 0 index) items (nthcdr (1+ index) list)))

(defun dt-product-p (pattern)
  (member (ir-kind pattern) '(:p-tuple :p-record :p-record-lookup :p-ref)))

(defun dt-product-fields (pattern form)
  "The fields ((key form pattern) ...) of tuple, record or ref PATTERN
matched against FORM."
  (ecase (ir-kind pattern)
    (:p-tuple (loop for subpattern in (third pattern)
                    for i from 1
                    collect (list i `(svref ,form ,i) subpattern)))
    (:p-record (mapcar (lambda (field)
                         (list (first field) `(svref ,form ,(first field)) (third field)))
                       (fourth pattern)))
    (:p-record-lookup (mapcar (lambda (field)
                                (list (first field) `(sml-record-field ,form ,(first field))
                                      (second field)))
                              (third pattern)))
    (:p-ref (list (list :ref `(svref ,form 1) (third pattern))))))

(defun dt-static-key (pattern)
  "The key of a constant or of a constructor whose tag is known, or NIL."
  (case (ir-kind pattern)
    (:p-const (list :const (third pattern)))
    (:p-cons '(:cons))
    (:p-con (destructuring-bind (how data) (subseq pattern 3 5)
              (case how
                (:eq-tag (list :tag data 0))
                (:boxed (list :tag data :boxed))
                (:flat (list :tag (car data) :flat (cdr data))))))))

(defun dt-key (pattern)
  "The key of the test that refutable PATTERN makes."
  (or (dt-static-key pattern)
      (case (ir-kind pattern)
        (:p-exn (list :exn (fourth pattern)))
        (:p-con (list :con (fourth pattern) (fifth pattern)))
        (otherwise (throw 'dt-give-up nil)))))

(defun dt-payload (pattern)
  "The pattern of the argument of constructor PATTERN, or NIL."
  (case (ir-kind pattern)
    (:p-exn (fifth pattern))
    (:p-con (sixth pattern))))

(defmacro ir-known (type form)
  "FORM, whose value is known to be of TYPE."
  #+sbcl `(sb-ext:truly-the ,type ,form)
  #-sbcl `(the ,type ,form))

(defun dt-subforms (key value payloadp)
  "The forms of the subterms of VALUE, a variable, when it matches KEY."
  (ecase (first key)
    (:const nil)
    (:cons (list `(car ,value) `(cdr ,value)))
    (:tag (ecase (third key)
            ((0) nil)
            (:boxed (list `(cdr ,value)))
            ;; A simple-vector, known from the test of the tag, or from
            ;; the type when no tag is tested; SVREF still checks the index.
            (:flat (loop for i from 1 to (fourth key)
                         collect `(svref (ir-known simple-vector ,value) ,i)))))
    (:exn (and payloadp (list `(cdr ,value))))
    (:con (and payloadp
               (list (if (eq (second key) :functor-tag)
                         `(cdr ,value)
                         `(sml-constructed-argument ,value)))))))

(defun dt-subpatterns (key pattern value width)
  "The WIDTH patterns of the subterms that PATTERN, of KEY, matches against
VALUE, and the variables bound to the whole argument of a flat
constructor."
  (case (first key)
    (:cons (list (third pattern) (fourth pattern)))
    (:tag
     (if (eq (third key) :flat)
         ;; The argument of a flat constructor is its slots; a variable for
         ;; all of it gets a copy.
         (multiple-value-bind (rest bindings)
             (dt-peel (sixth pattern) `(sml-flat-constructed-argument ,value) nil)
           (values (cond ((dt-wild-p rest) (make-list width :initial-element +dt-wild+))
                         ((and (dt-product-p rest) (not (eq (ir-kind rest) :p-ref)))
                          (let ((fields (dt-product-fields rest value)))
                            (loop for i from 1 to width
                                  collect (or (third (assoc i fields)) +dt-wild+))))
                         (t (throw 'dt-give-up nil)))
                   bindings))
         (and (sixth pattern) (list (sixth pattern)))))
    (t (let ((payload (dt-payload pattern)))
         (cond ((zerop width) nil)
               (payload (list payload))
               (t (list +dt-wild+)))))))

(defun dt-specialize (forms rows column key subforms keep-column)
  "The matrix of the rows of ROWS that may match KEY in COLUMN, whose
subterms there are SUBFORMS; when KEEP-COLUMN, the column stays, for the
rows whose tests there do not exclude KEY."
  (let* ((value (nth column forms))
         (width (length subforms))
         (wilds (make-list width :initial-element +dt-wild+))
         (new-forms (dt-replace forms column
                                (append subforms (and keep-column (list value))))))
    (values new-forms
            (loop for row in rows
                  for pattern = (nth column (dt-row-patterns row))
                  for row-key = (and (not (dt-wild-p pattern)) (dt-key pattern))
                  ;; Two static keys exclude each other; a dynamic key
                  ;; may denote any constructor.
                  unless (and row-key
                              (dt-static-key pattern)
                              (member (first key) '(:const :cons :tag))
                              (not (equal row-key key)))
                    collect (multiple-value-bind (subpatterns bindings)
                                (if (equal row-key key)
                                    (dt-subpatterns key pattern value width)
                                    wilds)
                              (dt-row (dt-replace (dt-row-patterns row) column
                                                  (append subpatterns
                                                          (and keep-column
                                                               (list (if (equal row-key key)
                                                                         +dt-wild+
                                                                         pattern)))))
                                      new-forms
                                      (append bindings (dt-row-bindings row))
                                      (dt-row-rule row)))))))

(defun dt-expand-product (forms rows column)
  "The matrix with COLUMN, of tuples, records or refs, replaced by the
columns of their fields that some row tests or binds."
  (let ((form (nth column forms))
        (keys nil))
    (dolist (row rows)
      (let ((pattern (nth column (dt-row-patterns row))))
        (unless (dt-wild-p pattern)
          (unless (dt-product-p pattern)
            (throw 'dt-give-up nil))
          (loop for (key subform subpattern) in (dt-product-fields pattern form)
                unless (or (dt-wild-p subpattern) (assoc key keys :test #'equal))
                  do (push (list key subform) keys)))))
    (setf keys (nreverse keys))
    (let ((new-forms (dt-replace forms column (mapcar #'second keys))))
      (values new-forms
              (mapcar (lambda (row)
                        (let* ((pattern (nth column (dt-row-patterns row)))
                               (fields (and (not (dt-wild-p pattern))
                                            (dt-product-fields pattern form))))
                          (dt-row (dt-replace (dt-row-patterns row) column
                                              (mapcar (lambda (key)
                                                        (or (third (assoc (first key) fields
                                                                          :test #'equal))
                                                            +dt-wild+))
                                                      keys))
                                  new-forms (dt-row-bindings row) (dt-row-rule row))))
                      rows)))))

(defun dt-choose-column (rows)
  "The column to test: among those where the first row is refutable, the
one with the longest prefix of rows refutable there (Maranget's
heuristic p, after f), the leftmost of equals."
  (let ((best nil) (best-score -1))
    (dotimes (column (length (dt-row-patterns (first rows))) best)
      (unless (dt-wild-p (nth column (dt-row-patterns (first rows))))
        (let ((score (loop for row in rows
                           while (not (dt-wild-p (nth column (dt-row-patterns row))))
                           count t)))
          (when (> score best-score)
            (setf best column best-score score)))))))

(defun dt-compile (forms rows)
  "The CL form of the decision tree of the matrix (FORMS, ROWS), whose
leaves are (*DT-LEAF-MARKER* rule bindings)."
  (when (null rows)
    (return-from dt-compile *dt-failure*))
  (let ((product (position-if (lambda (column)
                                (some (lambda (row)
                                        (dt-product-p (nth column (dt-row-patterns row))))
                                      rows))
                              (loop for i below (length forms) collect i))))
    (when product
      (return-from dt-compile
        (multiple-value-call #'dt-compile (dt-expand-product forms rows product)))))
  (let ((first-row (first rows)))
    (when (every #'dt-wild-p (dt-row-patterns first-row))
      (incf (aref *dt-leaf-counts* (dt-row-rule first-row)))
      (return-from dt-compile
        (list *dt-leaf-marker* (dt-row-rule first-row) (dt-row-bindings first-row)))))
  (when (> (incf *dt-tests*) *dt-test-limit*)
    (throw 'dt-give-up nil))
  (let* ((column (dt-choose-column rows))
         (form (nth column forms)))
    (if (symbolp form)
        (dt-split forms rows column)
        ;; Test a variable, not an access.
        (let ((value (gensym "V")))
          `(let ((,value ,form))
             ,(dt-split (dt-replace forms column (list value)) rows column))))))

(defun dt-split (forms rows column)
  (let* ((value (nth column forms))
         (patterns (mapcar (lambda (row) (nth column (dt-row-patterns row))) rows))
         (head (first patterns)))
    (if (dt-static-key head)
        (dt-switch forms rows column)
        ;; A test of one exception or constructor, which another may equal.
        (let* ((key (dt-key head))
               (payloadp (some (lambda (pattern)
                                 (and (not (dt-wild-p pattern))
                                      (equal (dt-key pattern) key)
                                      (dt-payload pattern)))
                               patterns))
               (subforms (dt-subforms key value payloadp)))
          `(if ,(dt-dynamic-test key value)
               ,(multiple-value-call #'dt-compile
                  (dt-specialize forms rows column key subforms t))
               ,(dt-compile forms
                            (remove-if (lambda (row)
                                         (let ((pattern (nth column (dt-row-patterns row))))
                                           (and (not (dt-wild-p pattern))
                                                (equal (dt-key pattern) key))))
                                       rows)))))))

(defun dt-dynamic-test (key value)
  "The test that VALUE matches KEY, of an exception or of a constructor
whose tag is not known at compile time (see IR-EMIT-CONSTRUCTOR-PATTERN)."
  (if (eq (first key) :exn)
      `(eq (if (consp ,value) (car ,value) ,value)
           (sml-exception-constructor-tag ,(second key)))
      (destructuring-bind (how data) (rest key)
        (ecase how
          (:eql-form `(eql ,value ,data))
          (:bound-eql `(and (boundp ',data) (eql ,value (symbol-value ',data))))
          (:value-eql `(eql ,value (symbol-value ',data)))
          (:tag-p `(sml-constructor-tag-p ,value ',data))
          (:functor-tag `(and (consp ,value) (eq (car ,value) ,data)))
          (:unknown-repr `(eq (sml-constructed-tag ,value) ',data))
          (:variable `(eq (sml-constructed-tag ,value) (sml-constructor-value-tag ,data)))
          (:runtime `(let ((tag (sml-constructed-tag ,value)))
                       (and tag (sml-constructor-tag-p tag ',data))))))))

(defun dt-complete-p (keys patterns)
  "Do the static KEYS cover every value of the type of PATTERNS?"
  (let ((type (ir-type (find-if-not #'dt-wild-p patterns))))
    (case (if (member '(:cons) keys :test #'equal) :cons (first (first keys)))
      (:cons (= (length keys) 2))
      (:const (and (= (length keys) 2)
                   (member '(:const t) keys :test #'equal)
                   (member '(:const nil) keys :test #'equal)))
      (:tag (and (consp type) (eq (first type) :con)
                 (= (length keys)
                    (length (ir-tycon-constructors (second type)))))))))

(defun dt-switch (forms rows column)
  "A CASE (or IF) on the static keys of COLUMN."
  (let* ((value (nth column forms))
         (patterns (mapcar (lambda (row) (nth column (dt-row-patterns row))) rows))
         (keys (remove-duplicates (remove nil (mapcar (lambda (pattern)
                                                        (and (not (dt-wild-p pattern))
                                                             (dt-static-key pattern)))
                                                      patterns))
                                  :test #'equal :from-end t))
         (dynamicp (some (lambda (pattern)
                           (and (not (dt-wild-p pattern)) (not (dt-static-key pattern))))
                         patterns))
         (branches (mapcar (lambda (key)
                             (cons key
                                   (multiple-value-call #'dt-compile
                                     (dt-specialize forms rows column key
                                                    (dt-subforms key value t)
                                                    dynamicp))))
                           keys))
         (default (unless (dt-complete-p keys patterns)
                    (dt-compile forms
                                (remove-if (lambda (row)
                                             (dt-static-key (nth column (dt-row-patterns row))))
                                           rows)))))
    (dt-switch-form value branches default)))

(defun dt-string-switch (value branches default)
  "Choose among BRANCHES ((key . form) ...), whose keys are strings, as
trivia does: by type and length once, then by characters (SML strings are
simple)."
  (let ((block (gensym "STRING"))
        (lengths (remove-duplicates (mapcar (lambda (branch) (length (second (car branch))))
                                            branches))))
    `(block ,block
       (when (simple-string-p ,value)
         (case (length ,value)
           ,@(mapcar
              (lambda (length)
                `((,length)
                  (cond
                    ,@(loop for ((nil string) . form) in branches
                            when (= (length string) length)
                              collect `(,(if (<= length 16)
                                             `(and ,@(loop for char across string
                                                           for i from 0
                                                           collect `(char= (schar ,value ,i) ,char)))
                                             `(string= ,value ,string))
                                        (return-from ,block ,form))))))
              lengths)))
       ,default)))

(defun dt-case (form test branches default)
  "Choose among BRANCHES ((key . form) ...), whose keys are constants or
tags compared by TEST, by FORM, or DEFAULT (NIL when the keys are all
those of the type).  SBCL compiles a CASE on a few sparse keys to a
perfect hash, slower than comparing them in turn, as trivia did."
  (flet ((no-jump-tables (form)
           #+sbcl `(locally (declare (optimize (sb-c:jump-table 0))) ,form)
           #-sbcl form)
         (jump-tables (form)
           #+sbcl `(locally (declare (optimize (sb-c:jump-table 1))) ,form)
           #-sbcl form))
    (if (> (length branches) 12)
        `(case ,form
           ,@(loop for ((key . branch) . more) on branches
                   collect (if (and (null more) (null default))
                               `(otherwise ,branch)
                               `((,(second key)) ,branch)))
           ,@(and default `((otherwise ,default))))
        ;; FORM, when a variable, is not rebound: it may be one that a
        ;; self-tail call in a branch assigns.
        (let* ((value (if (symbolp form) form (gensym "KEY")))
               (choice (no-jump-tables
                        `(cond ,@(loop for ((key . branch) . more) on branches
                                       collect (if (and (null more) (null default))
                                                   `(t ,(jump-tables branch))
                                                   `((,test ,value ',(second key))
                                                     ,(jump-tables branch))))
                               ,@(and default `((t ,(jump-tables default))))))))
          (if (eq value form)
              choice
              `(let ((,value ,form)) ,choice))))))

(defun dt-switch-form (value branches default)
  "The CL form choosing among BRANCHES ((key . form) ...) by the key of
VALUE, or DEFAULT (NIL when the keys are all of the type's)."
  (flet ((branch (key) (cdr (assoc key branches :test #'equal))))
    (let ((keys (mapcar #'car branches)))
      (cond
        ((and (null default) (null (rest branches))) (cdr (first branches)))
        ((member '(:cons) keys :test #'equal)
         `(if (consp ,value) ,(branch '(:cons)) ,(or (branch '(:const nil)) default)))
        ((eq (first (first keys)) :const)
         (cond ((null default) `(if ,value ,(branch '(:const t)) ,(branch '(:const nil))))
               ((stringp (second (first keys))) (dt-string-switch value branches default))
               (t (dt-case value 'eql branches default))))
        ((every (lambda (key) (eql (third key) 0)) keys)
         (dt-case value 'eq branches default))
        ((null (rest branches))
         (let ((tag (second (first keys))))
           `(if ,(ecase (third (first keys))
                   (:boxed `(and (consp ,value) (eq (car ,value) ',tag)))
                   (:flat `(and (simple-vector-p ,value) (eq (svref ,value 0) ',tag))))
                ,(cdr (first branches))
                ,default)))
        ((and (null default) (= (length branches) 2))
         (destructuring-bind ((key-1 . form-1) (key-2 . form-2)) branches
           (let ((kinds (list (third key-1) (third key-2))))
             (cond ((eql (third key-1) 0) `(if (eq ,value ',(second key-1)) ,form-1 ,form-2))
                   ((eql (third key-2) 0) `(if (eq ,value ',(second key-2)) ,form-2 ,form-1))
                   ((equal kinds '(:flat :flat))
                    `(if (eq (svref ,value 0) ',(second key-1)) ,form-1 ,form-2))
                   ((equal kinds '(:boxed :boxed))
                    `(if (eq (car ,value) ',(second key-1)) ,form-1 ,form-2))
                   ((eq (third key-1) :boxed) `(if (consp ,value) ,form-1 ,form-2))
                   (t `(if (consp ,value) ,form-2 ,form-1))))))
        (t
         (let ((kinds (remove-duplicates (mapcar #'third keys))))
           ;; The tag of a value of a constructor of another kind than
           ;; those of KEYS is the value, which is no key.
           (dt-case (cond ((and (null default) (equal kinds '(:flat))) `(svref ,value 0))
                          ((and (null default) (equal kinds '(:boxed))) `(car ,value))
                          (t `(cond ,@(and (member :boxed kinds)
                                           `(((consp ,value) (car ,value))))
                                    ,@(and (member :flat kinds)
                                           `(((simple-vector-p ,value) (svref ,value 0))))
                                    (t ,value))))
                    'eq branches default)))))))

(defun ir-emit-decision-tree (scrutinees rules failure)
  "The CL form matching the values of the CL forms SCRUTINEES against RULES
((patterns body) ...), FAILURE when none matches, as a decision tree; NIL
when the tree would be too large."
  (let* ((temporaries nil)
         (forms (mapcar (lambda (form)
                          (if (and form (symbolp form))
                              form
                              (let ((temporary (gensym "VALUE")))
                                (push (list temporary form) temporaries)
                                temporary)))
                        scrutinees))
         (*dt-tests* 0)
         (*dt-test-limit* (+ 64 (* 16 (length rules))))
         (*dt-leaf-marker* (gensym "LEAF"))
         (*dt-leaf-counts* (make-array (length rules) :initial-element 0))
         (*dt-failure* failure)
         (tree (catch 'dt-give-up
                 (dt-compile forms
                             (loop for (patterns) in rules
                                   for i from 0
                                   collect (dt-row patterns forms nil i))))))
    (when tree
      (let ((actions (make-array (length rules) :initial-element nil))
            (functions nil))
        (labels ((action (rule bindings)
                   ;; (name parameters . form) of RULE, made at its first leaf.
                   (or (aref actions rule)
                       (destructuring-bind (patterns body) (nth rule rules)
                         (setf (aref actions rule)
                               (list* (and (> (aref *dt-leaf-counts* rule) 1)
                                           (> (ir-size body) *ir-shared-action-size*)
                                           (gensym "RULE"))
                                      (mapcar #'first bindings)
                                      (ir-emit-rule-body patterns body))))))
                 (value (binding)
                   (destructuring-bind (symbol representation form) binding
                     (declare (ignore symbol))
                     (if (eq representation :real) `(sml-double ,form) form)))
                 (leaf (rule bindings)
                   (destructuring-bind (name parameters . form) (action rule bindings)
                     (cond
                       (name `(,name ,@(mapcar (lambda (parameter)
                                                 (value (assoc parameter bindings)))
                                               parameters)))
                       (bindings `(let ,(mapcar (lambda (binding)
                                                  (list (first binding) (value binding)))
                                                bindings)
                                    (declare (ignorable ,@(mapcar #'first bindings)))
                                    ,form))
                       (t form))))
                 (fill-leaves (form)
                   (cond ((atom form) form)
                         ((eq (first form) *dt-leaf-marker*) (leaf (second form) (third form)))
                         (t (let ((head (fill-leaves (car form)))
                                  (tail (fill-leaves (cdr form))))
                              (if (and (eq head (car form)) (eq tail (cdr form)))
                                  form
                                  (cons head tail)))))))
          (let ((body (fill-leaves tree)))
            (loop for entry across actions
                  when (and entry (first entry))
                    do (destructuring-bind (name parameters . form) entry
                         (push `(,name ,parameters
                                       (declare (ignorable ,@parameters))
                                       ,form)
                               functions)))
            (let ((body (if functions `(flet ,(nreverse functions) ,body) body)))
              (if temporaries
                  `(let ,(nreverse temporaries) ,body)
                  body))))))))

(defun ir-emit-declaration (declaration body)
  "A local declaration around the CL form BODY (COMPILE-LOCAL-DECL)."
  (ecase (first declaration)
    (:datatype
     ;; ((symbol tag takes-argument-p name) ...)
     (let ((bindings (mapcar (lambda (ctor)
                               (destructuring-bind (symbol tag argumentp name) ctor
                                 (declare (ignore name))
                                 (list symbol (if argumentp
                                                  `(lambda (payload) (cons ',tag payload))
                                                  `',tag))))
                             (second declaration))))
       `(let ,bindings
          (declare (ignorable ,@(mapcar #'first bindings)))
          ,body)))
    (:exception
     (destructuring-bind (symbol name argumentp) (rest declaration)
       `(let ((,symbol ,(if argumentp
                            `(make-sml-exception-function ,name)
                            `(make-sml-exception-constructor ,name))))
          (declare (ignorable ,symbol))
          ,body)))
    (:exception-alias
     (destructuring-bind (symbol name target lexicalp) (rest declaration)
       `(let ((,symbol ,(if lexicalp
                            target
                            `(if (boundp ',target)
                                 (symbol-value ',target)
                                 (make-sml-exception-constructor ,name)))))
          (declare (ignorable ,symbol))
          ,body)))
    (:module (emit-lexical-structure (second declaration) body))))

(defun ir-declaration-binders (declaration)
  (case (first declaration)
    (:datatype (mapcar #'first (second declaration)))
    ((:exception :exception-alias) (list (second declaration)))
    (:module (lexical-structure-cells (second declaration)))))

;;; Representation types: what the code may declare about the values of an
;;; IR type.  Reals are always double-floats; on SBCL, Int.int is exactly
;;; the fixnums.  Only the Basis's own types qualify (compared by stamp, so
;;; a program's `datatype real' does not), and only in typed terms.

(defun ir-base-type-p (type name)
  (and (consp type) (eq (first type) :con)
       (let ((base (ir-base-type name)))
         (and base
              (eql (ir-tycon-stamp (second type)) (ir-tycon-stamp (second base)))
              (string= (ir-tycon-name (second type)) (ir-tycon-name (second base)))))))

(defun ir-representation-type (type)
  "The CL type of every value of IR type TYPE that is worth declaring, or NIL."
  (cond ((null type) nil)
        ((ir-base-type-p type "real") 'double-float)
        #+sbcl ((ir-base-type-p type "int") 'fixnum)))

(defun ir-type-declarations (variables types)
  "Declarations of VARIABLES, of IR types TYPES, for the CL compiler."
  (let ((declarations (loop for variable in variables
                            for type in types
                            for cl-type = (ir-representation-type type)
                            when cl-type collect `(type ,cl-type ,variable))))
    (and declarations `((declare ,@declarations)))))

;;; Local functions (*SML-LOCAL-WORKERS*): the functions of a `letrec' are
;;; CL local functions (LABELS), so that their calls are known calls the CL
;;; compiler can make directly, with unboxed arguments.  A function of
;;; several parameters (a tuple's fields, IR-LOWER-LOCAL-FUN) or of curried
;;; parameters (`fun f a b', a chain of :fn) takes them all at once; its
;;; applications to a tuple, or to all its curried arguments, call it
;;; directly, and its uses as a value are a wrapper of one argument.  An
;;; entry of *IR-LOCAL-FUNCTIONS* is (symbol kind arity wrapper), KIND
;;; :TUPLED, :CURRIED or :PLAIN (one argument; the wrapper is the function).

(defvar *ir-local-functions* nil
  "The local functions in scope where the node being emitted will be.")

(defun ir-local-function (symbol)
  (assoc symbol *ir-local-functions* :test #'eq))

(defun ir-curried-depth (fn)
  "The number of curried parameters of the chain of :fn nodes FN."
  (loop for node = fn then (fourth node)
        while (and (eq (ir-kind node) :fn) (= (length (third node)) 1))
        count t))

(defun ir-local-function-entry (binding)
  (destructuring-bind (symbol fn) binding
    (let ((params (third fn)))
      (cond ((> (length params) 1)
             (list symbol :tupled (length params) (gensym "WRAPPER")))
            ((> (ir-curried-depth fn) 1)
             (list symbol :curried (ir-curried-depth fn) (gensym "WRAPPER")))
            (t (list symbol :plain 1 symbol))))))

(defun ir-local-function-definitions (binding entry)
  "The LABELS definitions of the function of BINDING: the worker, and its
wrapper unless it is the worker."
  (destructuring-bind (symbol kind arity wrapper) entry
    (let* ((fn (second binding))
           (chain (loop for node = fn then (fourth node)
                        repeat (if (eq kind :curried) arity 1)
                        collect node))
           (params (mapcan (lambda (node) (copy-list (third node))) chain))
           (types (if (eq kind :tupled)
                      (ir-tuple-field-types (ir-arrow-domain (ir-type fn)) arity)
                      (mapcar (lambda (node) (ir-arrow-domain (ir-type node))) chain)))
           (body (fourth (car (last chain))))
           (worker `(,symbol ,params
                             ,@(portable-tail-call-declarations)
                             ,@(ir-type-declarations params types)
                             ,(ir-bind-local-function-variables
                               (with-ir-scope (params) (ir-emit body))))))
      (ecase kind
        (:plain (list worker))
        (:tupled
         (let ((tuple (gensym "TUPLE")))
           (list worker
                 `(,wrapper (,tuple)
                            (,symbol ,@(loop for i from 1 to arity
                                             collect `(svref ,tuple ,i)))))))
        (:curried
         (let ((args (loop repeat arity collect (gensym "ARG"))))
           (list worker
                 `(,wrapper (,(first args))
                            ,(reduce (lambda (arg body) `(lambda (,arg) ,body))
                                     (rest args)
                                     :from-end t
                                     :initial-value `(,symbol ,@args))))))))))

(defun ir-mentions-symbol-p (form symbol &key (call-position t))
  "Does SYMBOL occur in the CL form FORM (when not CALL-POSITION, other than
as the operator of a form; declarations are not looked at)?"
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((walk (x)
               (cond ((eq x symbol) t)
                     ((and (consp x) (not (gethash x seen)) (not (eq (car x) 'declare)))
                      (setf (gethash x seen) t)
                      (or (if (eq (car x) symbol) call-position (walk (car x)))
                          (loop for tail = (cdr x) then (cdr tail)
                                while (consp tail)
                                thereis (walk (car tail))))))))
      (walk form))))

(defun ir-bind-local-function-variables (form)
  "FORM, with the local functions in scope that it refers to as variables
bound to their wrappers.  Code compiled apart from the term (a structure
declared in a `let', a hoisted match) refers to the function by its
variable."
  (let ((bindings (loop for (symbol nil nil wrapper) in *ir-local-functions*
                        when (ir-mentions-symbol-p form symbol :call-position nil)
                          collect `(,symbol #',wrapper))))
    (if bindings
        `(let ,bindings
           (declare (ignorable ,@(mapcar #'first bindings)))
           ,form)
        form)))

(defun ir-emit-local-functions (bindings body)
  "A `letrec' of functions BINDINGS around BODY, as LABELS."
  (let* ((entries (mapcar #'ir-local-function-entry bindings))
         (*ir-local-functions* (append entries *ir-local-functions*)))
    ;; In scope as variables too, for hoisted code (IR-EMIT-DISPATCH-GROUP).
    (with-ir-scope ((mapcar #'first bindings))
      (let ((definitions (loop for binding in bindings
                               for entry in entries
                               append (ir-local-function-definitions binding entry))))
        `(labels ,definitions
           (declare (ignorable ,@(mapcar (lambda (definition) `#',(first definition))
                                         definitions)))
           ,(ir-bind-local-function-variables (ir-emit body)))))))

(defun ir-emit-application (node)
  "The CL form of the application NODE: a direct call when it applies a
local function to all of its arguments."
  (let ((args nil) (head node))
    (loop while (eq (ir-kind head) :app)
          do (push (fourth head) args)
             (setf head (third head)))
    (let ((entry (and (eq (ir-kind head) :var) (ir-local-function (third head)))))
      (flet ((apply-rest (call rest)
               (reduce (lambda (form arg) `(funcall ,form ,(ir-emit arg)))
                       rest :initial-value call)))
        (if (null entry)
            `(funcall ,(ir-emit (third node)) ,(ir-emit (fourth node)))
            (destructuring-bind (symbol kind arity wrapper) entry
              (declare (ignore wrapper))
              (ecase kind
                (:plain (apply-rest `(,symbol ,(ir-emit (first args))) (rest args)))
                (:tupled
                 (let ((arg (first args)))
                   (apply-rest
                    (if (and (eq (ir-kind arg) :tuple) (= (length (third arg)) arity))
                        `(,symbol ,@(ir-emit-list (third arg)))
                        (let ((tuple (gensym "TUPLE")))
                          `(let ((,tuple ,(ir-emit arg)))
                             (,symbol ,@(loop for i from 1 to arity
                                              collect `(svref ,tuple ,i))))))
                    (rest args))))
                (:curried
                 (if (>= (length args) arity)
                     (apply-rest `(,symbol ,@(ir-emit-list (subseq args 0 arity)))
                                 (nthcdr arity args))
                     `(funcall ,(ir-emit (third node)) ,(ir-emit (fourth node))))))))))))

(defun ir-emit (node)
  "The CL form of IR term NODE."
  (ecase (ir-kind node)
    (:const (third node))
    (:unit '(sml-unit))
    (:var (let ((entry (ir-local-function (third node))))
            (if entry `#',(fourth entry) (third node))))
    (:ref (third node))
    (:prim (ir-emit-primitive (third node) (ir-emit-list (fourth node))))
    (:tuple `(sml-tuple ,@(ir-emit-list (third node))))
    (:record (record-construction-form (third node) (ir-emit-list (fourth node))))
    (:select `(svref ,(ir-emit (fourth node)) ,(third node)))
    (:select-label `(sml-record-ref ,(ir-emit (fourth node)) ,(third node)))
    (:con (destructuring-bind (repr tag args) (cddr node)
            (let ((args (ir-emit-list args)))
              (ecase repr
                (:cons `(cons ,tag ,@args))
                (:boxed `(cons ',tag ,@args))
                (:flat `(vector ',tag ,@args))
                (:flat-construct `(sml-construct-flat ',tag ,@args))))))
    (:app (ir-emit-application node))
    (:call `(,(third node) ,@(ir-emit-list (fourth node))))
    (:fn (destructuring-bind (params body name) (cddr node)
           (let ((body (with-ir-scope (params) (ir-emit body))))
             (if (= (length params) 1)
                 (sml-lambda-form name (first params) body)
                 `(lambda ,params ,@(portable-tail-call-declarations) ,body)))))
    (:let (destructuring-bind (symbol value body) (cddr node)
            `(let ((,symbol ,(ir-emit value)))
               (declare (ignorable ,symbol))
               ,(with-ir-scope ((list symbol)) (ir-emit body)))))
    (:letrec (destructuring-bind (bindings body) (cddr node)
               (if (and *sml-local-workers*
                        (every (lambda (binding) (eq (ir-kind (second binding)) :fn))
                               bindings))
                   (ir-emit-local-functions bindings body)
                   (with-ir-scope ((mapcar #'first bindings))
                     `(let ,(mapcar (lambda (binding) `(,(first binding) nil)) bindings)
                        ,@(mapcar (lambda (binding)
                                    `(setf ,(first binding) ,(ir-emit (second binding))))
                                  bindings)
                        ,(ir-emit body))))))
    (:seq `(progn ,@(ir-emit-list (third node))))
    (:if `(if ,(ir-emit (third node)) ,(ir-emit (fourth node)) ,(ir-emit (fifth node))))
    (:while `(progn (loop while ,(ir-emit (third node)) do ,(ir-emit (fourth node)))
                    (sml-unit)))
    (:raise `(sml-raise ,(ir-emit (third node))))
    (:handle
     (let ((condition-var (gensym "EXN")))
       `(handler-case ,(ir-emit (third node))
          (sml-raised-exception (,condition-var)
            (let ((value (sml-exception-value ,condition-var)))
              ,(or (and *sml-decision-trees*
                        (let ((value (gensym "VALUE")))
                          (let ((tree (ir-emit-decision-tree
                                       (list value)
                                       (mapcar (lambda (rule)
                                                 (list (list (first rule)) (second rule)))
                                               (fourth node))
                                       `(sml-raise ,value))))
                            (and tree `(let ((,value value)) ,tree)))))
                   `(trivia:match value
                      ,@(mapcar (lambda (rule)
                                  (list (ir-emit-pattern (first rule))
                                        (ir-emit-rule-body (list (first rule)) (second rule))))
                                (fourth node))
                      (_ (sml-raise value)))))))))
    (:match (ir-emit-match node))
    (:loop (destructuring-bind (tag variables inits body) (cddr node)
             (let ((block (gensym "SELF")))
               `(let ,(mapcar #'list variables (ir-emit-list inits))
                  ,@(ir-type-declarations variables (mapcar #'ir-type inits))
                  (block ,block
                    (tagbody
                       ,tag
                       (return-from ,block
                         ,(with-ir-scope (variables) (ir-emit body)))))))))
    (:tail-call
     (destructuring-bind (tag &rest variables) (third node)
       `(progn
          (psetq ,@(mapcan #'list variables (ir-emit-list (fourth node))))
          (go ,tag))))
    (:decl (destructuring-bind (declaration body) (cddr node)
             (ir-emit-declaration declaration
                                  (with-ir-scope ((ir-declaration-binders declaration))
                                    (ir-emit body)))))))

;;; Basis primitives: the Basis binds them with `use{b = "Word.xorb"}' (see
;;; SML-BASIS-PRIMITIVE), which compiles to a closure taking a tuple.  A
;;; cell bound so records the primitive's name (IR-NOTE-BASIS-PRIMITIVE),
;;; and the simplifier opens the applications of the ones below.

(defun ir-word-structure-bits (name)
  "The size of the words of the structure of primitive NAME, e.g. 62 for
\"Word.xorb\", and the operation's name, or NIL."
  (let* ((dot (position #\. name :from-end t))
         (word (and dot (assoc (subseq name 0 dot) *sml-word-structures*
                               :test #'string=))))
    (and word (values (cdr word) (subseq name (1+ dot))))))

(defparameter *ir-basis-primitive-arities*
  '(("String.sub" . 2) ("String.size" . 1) ("Char.ord" . 1) ("Char.chr" . 1))
  "Primitives besides the word structures' (IR-WORD-OPERATION-ARITY):
name -> number of arguments (2 means a pair).")

(defun ir-word-operation-arity (operation)
  (cond ((member operation '("xorb" "orb" "andb" "<<" ">>" "~>>") :test #'string=) 2)
        ((member operation '("notb" "toInt" "fromInt") :test #'string=) 1)))

(defun ir-basis-primitive-arity (name)
  "The number of arguments of the Basis primitive NAME the IR opens, or NIL."
  (multiple-value-bind (bits operation) (ir-word-structure-bits name)
    (if bits
        (ir-word-operation-arity operation)
        (cdr (assoc name *ir-basis-primitive-arities* :test #'string=)))))

(defun ir-pure-basis-primitive-p (name)
  "Can primitive NAME neither raise nor have an effect?"
  (multiple-value-bind (bits operation) (ir-word-structure-bits name)
    (if bits
        (member operation '("xorb" "orb" "andb" "<<" ">>" "~>>" "notb" "fromInt")
                :test #'string=)
        (member name '("String.size" "Char.ord") :test #'string=))))

(defun ir-emit-basis-primitive (name args)
  (multiple-value-bind (bits operation) (ir-word-structure-bits name)
    (if bits
        (flet ((w (form) `(sml-word-operand ,bits ,form)))
          (let ((operation (intern operation :keyword)))
            (case operation
              ((:|xorb| :|orb| :|andb|)
               `(,(ecase operation (:|xorb| 'logxor) (:|orb| 'logior) (:|andb| 'logand))
                 ,(w (first args)) ,(w (second args))))
              (:|notb| `(ldb (byte ,bits 0) (lognot ,(w (first args)))))
              (:|fromInt| `(ldb (byte ,bits 0) ,(first args)))
              (:|toInt| `(sml-check-int ,(w (first args))))
              ((:|<<| :|>>|)
               (let ((word (gensym "W")) (count (gensym "N")))
                 `(let ((,word ,(w (first args)))
                        (,count (sml-word-operand ,+sml-word-size+ ,(second args))))
                    (if (< ,count ,bits)
                        ,(if (eq operation :|<<|)
                             `(ldb (byte ,bits 0)
                                   (ash ,word (the (integer 0 ,(1- bits)) ,count)))
                             `(ash ,word (- (the (integer 0 ,(1- bits)) ,count))))
                        0))))
              (:|~>>| `(sml-word-arithmetic-shift-right
                        ,bits ,(w (first args))
                        (sml-word-operand ,+sml-word-size+ ,(second args)))))))
        (cond
          ((string= name "String.sub") `(sml-sequence-sub ,@args))
          ((string= name "String.size") `(sml-size ,@args))
          ((string= name "Char.ord") `(sml-ord ,@args))
          ((string= name "Char.chr") `(sml-chr ,@args))))))

(defun ir-emit-primitive (op args)
  (ecase (first op)
    (:overloaded (compile-overloaded-operation (second op) (third op) args))
    (:equality
     (destructuring-bind (test negated) (rest op)
       (let ((form `(,test ,@args)))
         (cond (negated `(not ,form))
               ((eq test 'string=) `(if ,form t nil))
               (t form)))))
    (:binary `(,(second op) ,@args))
    (:word-binary `(ldb (byte +sml-word-size+ 0) (,(second op) ,@args)))
    (:basis (ir-emit-basis-primitive (second op) args))
    (:not `(not ,@args))
    (:deref `(funcall #'sml-deref ,@args))
    (:list `(list ,@args))))

(defun sml-basis-primitive-use-name (ast)
  "NAME when expression AST is `use{b = NAME}' (with a type, perhaps)."
  (loop while (and (consp ast) (eq (car ast) :typed))
        do (setf ast (second ast)))
  (and (consp ast) (eq (car ast) :app)
       (equal (second ast) '(:var "use"))
       (consp (third ast)) (eq (car (third ast)) :record)
       (= (length (third ast)) 2)
       (equal (first (second (third ast))) "b")
       (stringp (second (second (third ast))))
       (second (second (third ast)))))

(defun ir-note-basis-primitive (cell expr)
  "Forms recording that CELL, bound to the value of EXPR, holds a Basis
primitive the IR opens (and recording it now, for the code compiled next)."
  (let ((name (sml-basis-primitive-use-name expr)))
    (cond
      ((and name (ir-basis-primitive-arity name))
       (setf (get cell 'sml-ir-primitive) name)
       `((setf (get ',cell 'sml-ir-primitive) ,name)))
      (t (remprop cell 'sml-ir-primitive)
         nil))))


;;; Walking terms

;; IR-MAP rebuilds a node with each IR term directly inside it replaced.
(defun ir-map (function node)
  (flet ((f (x) (funcall function x))
         (fs (xs) (mapcar function xs))
         (rules (rs) (mapcar (lambda (r) (list (first r) (funcall function (second r)))) rs)))
    (destructuring-bind (kind type &rest fields) node
      (list* kind type
             (ecase kind
               ((:const :unit :var :ref) fields)
               (:prim (list (first fields) (fs (second fields))))
               (:tuple (list (fs (first fields))))
               (:record (list (first fields) (fs (second fields))))
               ((:select :select-label) (list (first fields) (f (second fields))))
               (:con (list (first fields) (second fields) (fs (third fields))))
               (:app (list (f (first fields)) (f (second fields))))
               (:call (list (first fields) (fs (second fields)) (third fields)))
               (:fn (list (first fields) (f (second fields)) (third fields)))
               (:let (list (first fields) (f (second fields)) (f (third fields))))
               (:letrec (list (rules (first fields)) (f (second fields))))
               (:seq (list (fs (first fields))))
               ((:if :while :raise) (fs fields))
               (:handle (list (f (first fields)) (rules (second fields))))
               (:match (list (fs (first fields)) (rules (second fields)) (third fields)))
               (:loop (list (first fields) (second fields) (fs (third fields))
                            (f (fourth fields))))
               (:tail-call (list (first fields) (fs (second fields))))
               (:decl (list (first fields) (f (second fields)))))))))

(defun ir-walk-children (function node)
  "Call FUNCTION on each IR term directly inside NODE."
  (case (ir-kind node)
    ((:const :unit :var :ref) nil)
    (:prim (mapc function (fourth node)))
    (:tuple (mapc function (third node)))
    (:record (mapc function (fourth node)))
    ((:select :select-label) (funcall function (fourth node)))
    (:con (mapc function (fifth node)))
    (:app (funcall function (third node)) (funcall function (fourth node)))
    (:call (mapc function (fourth node)))
    (:fn (funcall function (fourth node)))
    (:let (funcall function (fourth node)) (funcall function (fifth node)))
    (:letrec (dolist (b (third node)) (funcall function (second b)))
     (funcall function (fourth node)))
    (:seq (mapc function (third node)))
    (:if (funcall function (third node)) (funcall function (fourth node))
     (funcall function (fifth node)))
    (:while (funcall function (third node)) (funcall function (fourth node)))
    (:raise (funcall function (third node)))
    (:handle (funcall function (third node))
     (dolist (rule (fourth node)) (funcall function (second rule))))
    (:match (mapc function (third node))
     (dolist (rule (fourth node)) (funcall function (second rule))))
    (:loop (mapc function (fifth node)) (funcall function (sixth node)))
    (:tail-call (mapc function (fourth node)))
    (:decl (funcall function (fourth node)))))

(defun ir-rule-patterns (node rule)
  "The patterns of RULE, a rule of match or handle NODE."
  (if (eq (ir-kind node) :match) (first rule) (list (first rule))))

(defun ir-declaration-references (declaration)
  "The CL forms DECLARATION evaluates that may refer to variables."
  (case (first declaration)
    (:exception-alias (list (fourth declaration)))
    (:module (list (second declaration)))))

(defun ir-map-binders (function node)
  "Call FUNCTION on each symbol a binder inside NODE binds: parameters, let,
letrec and loop variables, local declarations and pattern variables."
  (labels ((walk (node)
             (case (ir-kind node)
               (:fn (mapc function (third node)))
               (:let (funcall function (third node)))
               (:letrec (dolist (binding (third node)) (funcall function (first binding))))
               (:loop (mapc function (fourth node)))
               (:decl (mapc function (ir-declaration-binders (third node))))
               ((:match :handle)
                (dolist (rule (fourth node))
                  (dolist (pattern (ir-rule-patterns node rule))
                    (mapc function (ir-pattern-binders pattern))))))
             (ir-walk-children #'walk node)))
    (walk node)))

(defun ir-size (node)
  "The number of nodes of term NODE (patterns are not counted)."
  (let ((size 1))
    (ir-walk-children (lambda (child) (incf size (ir-size child))) node)
    (when (and (eq (ir-kind node) :decl) (eq (first (third node)) :module))
      (incf size 1000))
    size))

(defun ir-scan-variables (form function)
  "Call FUNCTION on each variable (uninterned symbol) in CL FORM."
  (cond ((ir-variable-p form) (funcall function form))
        ((consp form)
         (ir-scan-variables (car form) function)
         (ir-scan-variables (cdr form) function))))

(defun ir-free-variables (node)
  "The uninterned symbols NODE uses without binding them."
  (let ((bound (make-hash-table :test #'eq))
        (free nil))
    (labels ((use (symbol)
               (unless (gethash symbol bound) (pushnew symbol free)))
             (walk (node)
               (case (ir-kind node)
                 (:var (use (third node)))
                 ((:match :handle)
                  (dolist (rule (fourth node))
                    (dolist (pattern (ir-rule-patterns node rule))
                      (dolist (form (ir-pattern-references pattern))
                        (ir-scan-variables form #'use)))))
                 (:decl (dolist (form (ir-declaration-references (third node)))
                          (ir-scan-variables form #'use)))
                 (:tail-call (ir-scan-variables (third node) #'use)))
               (ir-walk-children #'walk node)))
      (ir-map-binders (lambda (symbol) (setf (gethash symbol bound) t)) node)
      (walk node))
    free))

(defun ir-mentions-p (node symbol)
  (cond ((eq node symbol) t)
        ((consp node) (or (ir-mentions-p (car node) symbol)
                          (ir-mentions-p (cdr node) symbol)))))

;;; Inline templates of top-level functions.  A `fun' of one clause whose
;;; parameters are variables (or one tuple of variables, for a function
;;; with a tupled worker), that is not recursive and whose body the IR
;;; covers entirely, records its IR on its cell, which is bound once and
;;; never assigned; the simplifier then inlines its calls.

(defun ir-variable-pattern-name (pat)
  (loop while (and (consp pat) (eq (car pat) :pat-typed))
        do (setf pat (second pat)))
  (cond ((and (consp pat) (member (car pat) '(:pat-var :var))) (second pat))
        ((eq pat :wild) :wild)))

(defun ir-note-inline-template (ast cell worker-arity local-exceptions lexical-env)
  "Forms recording the inline template of the function `fun' AST binds to
CELL, if it has one (and recording it now)."
  (remprop cell 'sml-ir-inline)
  (let ((clauses (third ast)))
    (when (and *sml-ir* *sml-ir-simplify*
               (null lexical-env) (null *functor-scope-variables*)
               (null (rest clauses)))
      (let* ((params (if worker-arity
                         (tuple-pattern-subpatterns (first (first (first clauses))))
                         (first (first clauses))))
             (names (mapcar #'ir-variable-pattern-name params)))
        (when (and params (every #'identity names)
                   (let ((vars (remove :wild names)))
                     (= (length vars) (length (remove-duplicates vars :test #'equal)))))
          (let* ((symbols (mapcar (lambda (name)
                                    (gensym (if (stringp name) (string-upcase name) "IGNORED")))
                                  names))
                 (env (loop for name in names
                            for symbol in symbols
                            unless (eq name :wild) collect (cons name symbol)))
                 (fun-type (ir-node-type clauses))
                 (body (catch 'ir-no-template
                         (let ((*ir-template-mode* t)
                               (*sml-hoisting-enabled* nil)
                               (*sml-binding-symbol-env*
                                 (acons (second ast) cell *sml-binding-symbol-env*)))
                           (ir-lower (second (first clauses)) local-exceptions env)))))
            (when body
              (let* ((body (ir-simplify body))
                     (template
                       (if worker-arity
                           (ir :fn fun-type symbols body nil)
                           (let ((types (loop repeat (length symbols)
                                              for type = fun-type then (ir-arrow-range type)
                                              collect type)))
                             (reduce (lambda (pair body)
                                       (ir :fn (cdr pair) (list (car pair)) body nil))
                                     (mapcar #'cons symbols types)
                                     :from-end t :initial-value body)))))
                (when (and (<= (ir-size template) *sml-ir-inline-size*)
                           (not (ir-mentions-p template cell))
                           (null (ir-free-variables template)))
                  (setf template (ir-portable-types template))
                  (setf (get cell 'sml-ir-inline) template)
                  `((setf (get ',cell 'sml-ir-inline) ',template)))))))))))

(defun ir-portable-types (term)
  "TERM with its types made independent of the session, so that it can be
written to a file the same way each time: type variables are numbered in
order of appearance and type constructors lose their stamps."
  (let ((tyvars (make-hash-table))
        (tycons (make-hash-table :test #'eq)))
    (labels ((copy (x)
               (cond ((ir-tycon-p x)
                      (or (gethash x tycons)
                          (setf (gethash x tycons)
                                (let ((copy (copy-ir-tycon x)))
                                  (setf (ir-tycon-stamp copy) 0)
                                  copy))))
                     ((and (consp x) (eq (first x) :tyvar) (integerp (second x)))
                      (list* :tyvar
                             (or (gethash (second x) tyvars)
                                 (setf (gethash (second x) tyvars)
                                       (hash-table-count tyvars)))
                             (copy (cddr x))))
                     ((consp x) (cons (copy (car x)) (copy (cdr x))))
                     (t x))))
      (copy term))))

;;; Copying with fresh variables, for inlining.

(defun ir-instantiate (template)
  "A copy of TEMPLATE in which every variable it binds is replaced by a
fresh one; its free variables stay."
  (let ((renaming (make-hash-table :test #'eq)))
    (ir-map-binders (lambda (symbol)
                      (setf (gethash symbol renaming)
                            (gensym (string-right-trim "0123456789" (symbol-name symbol)))))
                    template)
    (labels ((copy (x)
               (cond ((ir-variable-p x) (gethash x renaming x))
                     ((consp x) (cons (copy (car x)) (copy (cdr x))))
                     (t x))))
      (copy template))))

;;; Simplification

(defvar *ir-census* nil
  "Uninterned symbol -> number of uses in the term being simplified.")

(defvar *ir-opaque* nil
  "Uninterned symbols used in CL forms (pattern and declaration forms) or
assigned (self tail calls): they are neither substituted nor dropped.")

(defun ir-take-census (node)
  (let ((census (make-hash-table :test #'eq))
        (opaque (make-hash-table :test #'eq)))
    (flet ((scan (form)
             (ir-scan-variables form (lambda (symbol)
                                       (incf (gethash symbol census 0))
                                       (setf (gethash symbol opaque) t)))))
      (labels ((walk (node)
                 (case (ir-kind node)
                   (:var (incf (gethash (third node) census 0)))
                   (:tail-call (scan (third node)))
                   ((:match :handle)
                    (dolist (rule (fourth node))
                      (dolist (pattern (ir-rule-patterns node rule))
                        (mapc #'scan (ir-pattern-references pattern)))))
                   (:decl (mapc #'scan (ir-declaration-references (third node)))))
                 (ir-walk-children #'walk node)))
        ;; Every variable bound in NODE has an entry, so that one missing
        ;; is one made since (see IR-USES).
        (ir-map-binders (lambda (symbol)
                          (unless (nth-value 1 (gethash symbol census))
                            (setf (gethash symbol census) 0)))
                        node)
        (walk node)))
    (values census opaque)))

(defun ir-uses (symbol)
  "The number of uses of SYMBOL, or MOST-POSITIVE-FIXNUM for a symbol made
since the census (by inlining), whose uses are not counted."
  (gethash symbol *ir-census* most-positive-fixnum))
(defun ir-opaque-p (symbol) (gethash symbol *ir-opaque*))

(defun ir-trivial-p (node)
  "Can NODE be copied to each use of a variable bound to it?"
  (or (member (ir-kind node) '(:const :unit))
      (and (eq (ir-kind node) :var) (not (ir-opaque-p (third node))))))

(defun ir-pure-primitive-p (op)
  (case (first op)
    ((:equality :not :list) t)
    (:basis (ir-pure-basis-primitive-p (second op)))
    (:binary (eq (second op) 'cons))
    (:word-binary t)
    (:overloaded
     (destructuring-bind (name tyname) (rest op)
       (or (member name '("<" ">" "<=" ">=") :test #'string=)
           (and (or (sml-word-type-bits tyname) (string= tyname "real"))
                (member name '("+" "-" "*" "~" "abs") :test #'string=))
           (and (string= tyname "real") (string= name "/")))))))

(defun ir-pure-p (node)
  "Is evaluating NODE free of effects, and can it not raise?"
  (case (ir-kind node)
    ((:const :unit :var :ref :fn) t)
    ((:tuple) (every #'ir-pure-p (third node)))
    ((:record :prim)
     (and (or (eq (ir-kind node) :record) (ir-pure-primitive-p (third node)))
          (every #'ir-pure-p (fourth node))))
    (:select (ir-pure-p (fourth node)))
    (:con (every #'ir-pure-p (fifth node)))
    (:let (and (ir-pure-p (fourth node)) (ir-pure-p (fifth node))))
    (:if (and (ir-pure-p (third node)) (ir-pure-p (fourth node)) (ir-pure-p (fifth node))))
    (:seq (every #'ir-pure-p (third node)))))

(defun ir-constant-value (node)
  "The value of NODE when it is a literal, and true; else NIL and NIL."
  (if (eq (ir-kind node) :const)
      (values (third node) t)
      (values nil nil)))

(defun ir-fold-primitive (op args type)
  "The constant term OP gives on constant ARGS, or NIL.  Never folds an
operation that raises (Overflow, Div, ...) on these arguments."
  (let ((values (mapcar (lambda (arg) (multiple-value-list (ir-constant-value arg))) args)))
    (unless (every #'second values)
      (return-from ir-fold-primitive nil))
    (let ((values (mapcar #'first values)))
      (flet ((const (value) (ir :const type value)))
        (case (first op)
          (:not (const (not (first values))))
          (:equality
           (destructuring-bind (test negated) (rest op)
             (let ((equal (if (eq test 'string=)
                              (and (stringp (first values)) (stringp (second values))
                                   (string= (first values) (second values)))
                              (eql (first values) (second values)))))
               (when (or (not (eq test 'string=)) (every #'stringp values))
                 (const (if negated (not equal) equal))))))
          (:basis
           (when (and (ir-pure-basis-primitive-p (second op))
                      (every #'integerp values))
             (let ((function (sml-basis-primitive (second op))))
               (const (if (rest values)
                          (funcall function (sml-tuple (first values) (second values)))
                          (funcall function (first values)))))))
          (:overloaded
           (destructuring-bind (name tyname) (rest op)
             (let ((bits (sml-word-type-bits tyname))
                   (comparison (cdr (assoc name '(("<" . <) (">" . >) ("<=" . <=) (">=" . >=))
                                           :test #'string=))))
               (cond
                 ((and comparison (every #'integerp values)
                       (or bits (string= tyname "int")))
                  (const (and (apply comparison values) t)))
                 ((and comparison (every #'characterp values))
                  (const (and (apply (ecase comparison (< #'char<) (> #'char>)
                                       (<= #'char<=) (>= #'char>=))
                                     values)
                              t)))
                 ((not (every #'integerp values)) nil)
                 (bits
                  (let ((result (cond ((rest values)
                                       (let ((f (cdr (assoc name '(("+" . +) ("-" . -) ("*" . *))
                                                            :test #'string=))))
                                         (and f (funcall f (first values) (second values)))))
                                      ((string= name "~") (- (first values))))))
                    (and result (const (ldb (byte bits 0) result)))))
                 ((string= tyname "int")
                  (let ((result (cond ((rest values)
                                       (let ((f (cdr (assoc name '(("+" . +) ("-" . -) ("*" . *))
                                                            :test #'string=))))
                                         (and f (funcall f (first values) (second values)))))
                                      ((string= name "~") (- (first values)))
                                      ((string= name "abs") (abs (first values))))))
                    (and result (<= +sml-min-int+ result +sml-max-int+)
                         (const result))))))))
          (otherwise nil))))))

(defun ir-known-function (node env)
  "The function term NODE denotes when the simplifier knows it, and whether
it was a variable bound to it (to be copied)."
  (case (ir-kind node)
    (:fn (values node nil))
    (:var (let ((known (gethash (third node) env)))
            (and known (eq (ir-kind known) :fn) (values known t))))
    (:ref (let ((form (third node)))
            (and (symbolp form) (symbol-package form)
                 (let ((template (get form 'sml-ir-inline)))
                   (and template (= (length (third template)) 1)
                        (values template t))))))))

(defun ir-beta (fn args)
  "The application of function term FN to the terms ARGS, as lets."
  (let ((body (fourth fn)))
    (reduce (lambda (pair body) (ir :let (ir-type body) (car pair) (cdr pair) body))
            (mapcar #'cons (third fn) args)
            :from-end t :initial-value body)))

(defun ir-simplify-node (node env)
  "One simplification of NODE.  ENV maps let-bound variables to known
values (tuples, constructors, functions) or to a term to substitute."
  (macrolet ((simp (x) `(ir-simplify-node ,x env)))
    (case (ir-kind node)
      ((:const :unit :ref) node)
      (:var
       (let ((known (gethash (third node) env)))
         (if (and known (eq (car known) :substitute))
             (cdr known)
             node)))
      (:prim
       (let* ((op (third node))
              (args (mapcar (lambda (a) (simp a)) (fourth node))))
         (or (ir-fold-primitive op args (ir-type node))
             (ir :prim (ir-type node) op args))))
      (:tuple (ir :tuple (ir-type node) (mapcar (lambda (a) (simp a)) (third node))))
      (:record (ir :record (ir-type node) (third node)
                   (mapcar (lambda (a) (simp a)) (fourth node))))
      (:con (destructuring-bind (repr tag args) (cddr node)
              (ir :con (ir-type node) repr tag (mapcar (lambda (a) (simp a)) args))))
      (:select
       (let* ((index (third node))
              (record (simp (fourth node)))
              (known (if (eq (ir-kind record) :var)
                         (gethash (third record) env)
                         record)))
         (cond
           ((and known (eq (ir-kind known) :tuple)
                 (<= index (length (third known)))
                 (eq (ir-kind record) :var)
                 (ir-trivial-p (nth (1- index) (third known))))
            (nth (1- index) (third known)))
           ((and (eq (ir-kind record) :tuple)
                 (<= index (length (third record)))
                 (every #'ir-pure-p (third record)))
            (nth (1- index) (third record)))
           (t (ir :select (ir-type node) index record)))))
      (:select-label (ir :select-label (ir-type node) (third node) (simp (fourth node))))
      (:app (ir-simplify-application node env))
      (:call (ir-simplify-call node env))
      (:fn (destructuring-bind (params body name) (cddr node)
             (ir :fn (ir-type node) params (simp body) name)))
      (:let (ir-simplify-let node env))
      (:letrec
       (destructuring-bind (bindings body) (cddr node)
         (let ((bindings (remove-if (lambda (binding)
                                      (and (zerop (ir-uses (first binding)))
                                           (not (ir-opaque-p (first binding)))
                                           (eq (ir-kind (second binding)) :fn)))
                                    bindings)))
           (if bindings
               (ir :letrec (ir-type node)
                   (mapcar (lambda (binding) (list (first binding) (simp (second binding))))
                           bindings)
                   (simp body))
               (simp body)))))
      (:seq
       (let* ((items (mapcan (lambda (item)
                               (let ((item (simp item)))
                                 (if (eq (ir-kind item) :seq)
                                     (copy-list (third item))
                                     (list item))))
                             (third node)))
              (items (append (remove-if #'ir-pure-p (butlast items)) (last items))))
         (if (rest items) (ir :seq (ir-type node) items) (first items))))
      (:if
       (let ((test (simp (third node))))
         (multiple-value-bind (value constantp) (ir-constant-value test)
           (cond
             (constantp (simp (if value (fourth node) (fifth node))))
             ((and (eq (ir-kind test) :prim) (equal (third test) '(:not)))
              (ir :if (ir-type node) (first (fourth test))
                  (simp (fifth node)) (simp (fourth node))))
             (t (ir :if (ir-type node) test (simp (fourth node)) (simp (fifth node))))))))
      (:while (ir :while (ir-type node) (simp (third node)) (simp (fourth node))))
      (:raise (ir :raise (ir-type node) (simp (third node))))
      (:handle (ir :handle (ir-type node) (simp (third node))
                   (mapcar (lambda (rule) (list (first rule) (simp (second rule))))
                           (fourth node))))
      (:match (ir-simplify-match node env))
      (:loop (destructuring-bind (tag variables inits body) (cddr node)
               (ir :loop (ir-type node) tag variables
                   (mapcar (lambda (a) (simp a)) inits) (simp body))))
      (:tail-call (ir :tail-call (ir-type node) (third node)
                      (mapcar (lambda (a) (simp a)) (fourth node))))
      (:decl (ir :decl (ir-type node) (third node) (simp (fourth node)))))))

(defun ir-simplify-let (node env)
  (destructuring-bind (symbol value body) (cddr node)
    (let ((value (ir-simplify-node value env)))
      (cond
        ;; let x = (let y = a in b) in c  =>  let y = a in let x = b in c
        ((eq (ir-kind value) :let)
         (destructuring-bind (inner-symbol inner-value inner-body) (cddr value)
           (ir :let (ir-type node) inner-symbol inner-value
               (ir-simplify-let (ir :let (ir-type node) symbol inner-body body) env))))
        ((ir-opaque-p symbol)
         (ir :let (ir-type node) symbol value (ir-simplify-node body env)))
        ;; let x = (a, b) in c  =>  let t1 = a in let t2 = b in let x = (t1, t2)
        ;; in c, so that x is known (and destructuring it costs nothing).
        ((and (eq (ir-kind value) :tuple)
              (notevery #'ir-trivial-p (third value)))
         (let* ((temporaries (mapcar (lambda (arg)
                                       (if (ir-trivial-p arg)
                                           arg
                                           (ir :var (ir-type arg) (gensym "F"))))
                                     (third value)))
                (inner (ir :let (ir-type node) symbol (ir :tuple (ir-type value) temporaries)
                           body)))
           (ir-simplify-node
            (reduce (lambda (pair body)
                      (if (eq (car pair) (cdr pair))
                          body
                          (ir :let (ir-type body) (third (car pair)) (cdr pair) body)))
                    (mapcar #'cons temporaries (third value))
                    :from-end t :initial-value inner)
            env)))
        ((and (zerop (ir-uses symbol)) (ir-pure-p value))
         (ir-simplify-node body env))
        ((zerop (ir-uses symbol))
         ;; Evaluated for its effect.
         (let ((body (ir-simplify-node body env)))
           (if (ir-pure-p body)
               (ir :seq (ir-type node) (list value body))
               (ir :let (ir-type node) symbol value body))))
        ((ir-trivial-p value)
         (setf (gethash symbol env) (cons :substitute value))
         (ir-simplify-node body env))
        (t
         (when (or (and (member (ir-kind value) '(:tuple :con))
                        (every #'ir-trivial-p (if (eq (ir-kind value) :tuple)
                                                  (third value)
                                                  (fifth value))))
                   (and (eq (ir-kind value) :fn)
                        (or (= (ir-uses symbol) 1)
                            (<= (ir-size value) *sml-ir-inline-size*))))
           (setf (gethash symbol env) value))
         (ir :let (ir-type node) symbol value (ir-simplify-node body env)))))))

(defun ir-simplify-application (node env)
  (let ((function (ir-simplify-node (third node) env))
        (arg (ir-simplify-node (fourth node) env)))
    (cond
      ;; (let x = a in f) b  =>  let x = a in f b
      ((eq (ir-kind function) :let)
       (destructuring-bind (symbol value body) (cddr function)
         (ir :let (ir-type node) symbol value
             (ir-simplify-application (ir :app (ir-type node) body arg) env))))
      ;; not e
      ((and (eq (ir-kind function) :ref) (equal (third function) '(function sml-not)))
       (or (ir-fold-primitive '(:not) (list arg) (ir-type node))
           (ir :prim (ir-type node) '(:not) (list arg))))
      ;; (f o g) x  =>  f (g x), when f and g need not be evaluated first.
      ((and (eq (ir-kind function) :prim)
            (equal (third function) '(:binary sml-compose))
            (every #'ir-trivial-p (fourth function)))
       (destructuring-bind (f g) (fourth function)
         (ir-simplify-application
          (ir :app (ir-type node) f (ir :app (ir-arrow-range (ir-type g)) g arg)) env)))
      ;; A Basis primitive the IR opens.
      ((and (eq (ir-kind function) :ref)
            (symbolp (third function)) (symbol-package (third function))
            (get (third function) 'sml-ir-primitive)
            (let ((arity (ir-basis-primitive-arity (get (third function) 'sml-ir-primitive))))
              (or (eql arity 1)
                  (and (eql arity 2) (eq (ir-kind arg) :tuple) (= (length (third arg)) 2)))))
       (let* ((name (get (third function) 'sml-ir-primitive))
              (args (if (eql (ir-basis-primitive-arity name) 1) (list arg) (third arg))))
         (or (ir-fold-primitive (list :basis name) args (ir-type node))
             (ir :prim (ir-type node) (list :basis name) args))))
      (t
       (multiple-value-bind (known copy) (ir-known-function function env)
         (if (and known (= (length (third known)) 1)
                  (or (not copy) (<= (ir-size known) *sml-ir-inline-size*)))
             (ir-simplify-node (ir-beta (if copy (ir-instantiate known) known) (list arg))
                               env)
             (ir :app (ir-type node) function arg)))))))

(defun ir-simplify-call (node env)
  (destructuring-bind (worker args cell) (cddr node)
    (let ((args (mapcar (lambda (a) (ir-simplify-node a env)) args))
          (template (and (symbolp cell) (get cell 'sml-ir-inline))))
      (if (and template (= (length (third template)) (length args)))
          (ir-simplify-node (ir-beta (ir-instantiate template) args) env)
          (ir :call (ir-type node) worker args cell)))))

;;; Matches whose outcome is known (case-of-known-constructor)

(defun ir-known-value (node env)
  "NODE, or the tuple or constructed value the variable NODE is bound to,
when the simplifier knows it."
  (if (eq (ir-kind node) :var)
      (let ((known (gethash (third node) env)))
        (if (and known (consp known) (member (ir-kind known) '(:tuple :con)))
            known
            node))
      node))

(defun ir-static-match (pattern value env)
  "Whether PATTERN matches the term VALUE (trivial, or a known tuple or
constructed value): :YES, with the bindings ((symbol . term) ...) it makes,
:NO, or :UNKNOWN."
  (let ((value (ir-known-value value env)))
    (flet ((all (patterns values)
             (let ((bindings nil))
               (loop for pattern in patterns
                     for value in values
                     do (multiple-value-bind (outcome more)
                            (ir-static-match pattern value env)
                          (case outcome
                            (:no (return-from ir-static-match :no))
                            (:unknown (return-from ir-static-match :unknown))
                            (t (setf bindings (append bindings more))))))
               (values :yes bindings))))
      (case (ir-kind pattern)
        (:p-wild (values :yes nil))
        (:p-var (if (and (ir-variable-p (third pattern)) (ir-trivial-p value))
                    (values :yes (list (cons (third pattern) value)))
                    :unknown))
        (:p-as (if (and (ir-variable-p (third pattern)) (ir-trivial-p value))
                   (multiple-value-bind (outcome bindings)
                       (ir-static-match (fourth pattern) value env)
                     (if (eq outcome :yes)
                         (values :yes (acons (third pattern) value bindings))
                         outcome))
                   :unknown))
        (:p-const (multiple-value-bind (constant constantp) (ir-constant-value value)
                    (cond ((not constantp) :unknown)
                          ((if (stringp (third pattern))
                               (and (stringp constant) (string= constant (third pattern)))
                               (eql constant (third pattern)))
                           (values :yes nil))
                          (t :no))))
        ((:p-tuple :p-record)
         (if (eq (ir-kind value) :tuple)
             (let ((fields (third value)))
               (if (eq (ir-kind pattern) :p-tuple)
                   (if (= (length fields) (length (third pattern)))
                       (all (third pattern) fields)
                       :unknown)
                   (all (mapcar #'third (fourth pattern))
                        (mapcar (lambda (field) (nth (1- (first field)) fields))
                                (fourth pattern)))))
             :unknown))
        (:p-con
         (destructuring-bind (name how data payload) (cddr pattern)
           (declare (ignore name))
           (cond
             ((not (and (eq (ir-kind value) :con)
                        (member (third value) '(:boxed :flat))))
              :unknown)
             ;; A constructed value never is a nullary constructor.
             ((eq how :eq-tag) :no)
             ((not (member how '(:boxed :flat))) :unknown)
             ((not (eq (fourth value) (if (eq how :flat) (car data) data))) :no)
             ((and (eq how :boxed) (eq (third value) :boxed))
              (ir-static-match payload (first (fifth value)) env))
             ((and (eq how :flat) (eq (third value) :flat)
                   (eq (ir-kind payload) :p-tuple)
                   (= (length (third payload)) (length (fifth value))))
              (all (third payload) (fifth value)))
             (t :unknown))))
        (otherwise :unknown)))))

(defun ir-static-scrutinee-p (node env)
  (or (ir-trivial-p node)
      (let ((known (ir-known-value node env)))
        (and (not (eq known node))
             (every #'ir-trivial-p (if (eq (ir-kind known) :tuple)
                                       (third known)
                                       (fifth known)))))
      (and (member (ir-kind node) '(:tuple :con))
           (every #'ir-trivial-p (if (eq (ir-kind node) :tuple) (third node) (fifth node))))))

(defun ir-bind (bindings body)
  (reduce (lambda (binding body) (ir :let (ir-type body) (car binding) (cdr binding) body))
          bindings :from-end t :initial-value body))

(defun ir-simplify-match (node env)
  (destructuring-bind (scrutinees rules failure) (cddr node)
    (let* ((type (ir-type node))
           (scrutinees (mapcar (lambda (s) (ir-simplify-node s env)) scrutinees))
           (first-patterns (first (first rules))))
      (cond
        ;; The first rule's patterns are variables or wildcards.
        ((and rules
              (every (lambda (pattern)
                       (or (eq (ir-kind pattern) :p-wild)
                           (and (eq (ir-kind pattern) :p-var)
                                (ir-variable-p (third pattern)))))
                     first-patterns))
         (ir-simplify-node
          (reduce (lambda (pair body)
                    (destructuring-bind (pattern . scrutinee) pair
                      (if (eq (ir-kind pattern) :p-var)
                          (ir :let type (third pattern) scrutinee body)
                          (if (ir-pure-p scrutinee)
                              body
                              (ir :seq type (list scrutinee body))))))
                  (mapcar #'cons first-patterns scrutinees)
                  :from-end t :initial-value (second (first rules)))
          env))
        ;; Every scrutinee is known: the rules that cannot match are
        ;; dropped, and the first that must match is taken.
        ((every (lambda (s) (ir-static-scrutinee-p s env)) scrutinees)
         (let ((remaining rules))
           (loop
             (when (null remaining) (return))
             (let ((outcome :yes) (bindings nil))
               (loop for pattern in (first (first remaining))
                     for scrutinee in scrutinees
                     do (multiple-value-bind (one more) (ir-static-match pattern scrutinee env)
                          (case one
                            (:no (setf outcome :no) (return))
                            (:unknown (setf outcome :unknown))
                            (t (setf bindings (append bindings more))))))
               (case outcome
                 (:no (pop remaining))
                 (:yes (return-from ir-simplify-match
                         (ir-simplify-node (ir-bind bindings (second (first remaining))) env)))
                 (t (return)))))
           (ir-simplify-rules node type scrutinees remaining failure env)))
        ;; A boolean match.
        ((and (= (length rules) 2) (= (length scrutinees) 1)
              (eq (ir-kind (first first-patterns)) :p-const)
              (member (third (first first-patterns)) '(t nil))
              (let ((second-pattern (first (first (second rules)))))
                (or (eq (ir-kind second-pattern) :p-wild)
                    (and (eq (ir-kind second-pattern) :p-const)
                         (member (third second-pattern) '(t nil))
                         (not (eq (third second-pattern) (third (first first-patterns))))))))
         (let ((then (second (first rules)))
               (else (second (second rules))))
           (ir-simplify-node (if (third (first first-patterns))
                                 (ir :if type (first scrutinees) then else)
                                 (ir :if type (first scrutinees) else then))
                             env)))
        (t (ir-simplify-rules node type scrutinees rules failure env))))))

(defun ir-simplify-rules (node type scrutinees rules failure env)
  (declare (ignore node))
  (ir :match type scrutinees
      (mapcar (lambda (rule) (list (first rule) (ir-simplify-node (second rule) env)))
              rules)
      failure))

(defun ir-simplify (node)
  "NODE simplified (see *SML-IR-SIMPLIFY*)."
  (loop repeat *sml-ir-simplify-rounds*
        do (let ((before node))
             (multiple-value-bind (*ir-census* *ir-opaque*) (ir-take-census node)
               (setf node (ir-simplify-node node (make-hash-table :test #'eq))))
             (when (equal before node)
               (return))))
  node)

(defun ir-trivial-term-p (node)
  "Is NODE too small for simplification to change?"
  (member (ir-kind node) '(:const :unit :var :ref)))

;;; The type lint (*SML-IR-LINT*)

(defun ir-lint (node &optional (require-types t))
  "Problems with the types of term NODE, as strings: a node without a type
(in native mode, when REQUIRE-TYPES), or types that do not fit together
(application, conditional, let, match and function bodies)."
  (let ((problems nil)
        ;; Plain mode has no checker, hence no types.
        (typed (and require-types (native-sml-static-facts-p))))
    (labels ((problem (node format &rest args)
               (push (format nil "~S node: ~?" (ir-kind node) format args) problems))
             (same (node what a b)
               (unless (ir-type-compatible-p a b)
                 (problem node "~A: ~A against ~A" what (ir-type-string a) (ir-type-string b))))
             (pattern (p)
               (unless (or (ir-type p) (not typed))
                 (problem p "pattern without a type"))
               (case (ir-kind p)
                 (:p-as (same p "alias" (ir-type p) (ir-type (fourth p))) (pattern (fourth p)))
                 (:p-tuple (same p "tuple pattern" (ir-type p)
                                 (ir-tuple-type (mapcar #'ir-type (third p))))
                  (mapc #'pattern (third p)))
                 (:p-record (dolist (f (fourth p))
                              (same p "record field" (ir-field-type (ir-type p) (second f))
                                    (ir-type (third f)))
                              (pattern (third f))))
                 (:p-record-lookup (dolist (f (third p)) (pattern (second f))))
                 (:p-cons (same p "list pattern" (ir-type p) (ir-type (fourth p)))
                  (pattern (third p)) (pattern (fourth p)))
                 (:p-ref (pattern (third p)))
                 (:p-exn (when (fifth p) (pattern (fifth p))))
                 (:p-con (when (sixth p) (pattern (sixth p))))))
             (walk (node)
               (let ((type (ir-type node)))
                 (unless (or type (not typed))
                   (problem node "no type: ~S"
                            (let ((*print-length* 4) (*print-level* 3))
                              (princ-to-string (ir-display node)))))
                 (case (ir-kind node)
                   (:app (let ((f (ir-type (third node))))
                           (when f
                             (if (eq (first f) :arrow)
                                 (progn
                                   (same node "argument" (second f) (ir-type (fourth node)))
                                   (same node "result" (third f) type))
                                 (unless (eq (first f) :tyvar)
                                   (problem node "applies a ~A" (ir-type-string f)))))))
                   (:if (same node "test" (ir-type (third node)) (ir-base-type "bool"))
                    (same node "then" type (ir-type (fourth node)))
                    (same node "else" type (ir-type (fifth node))))
                   (:while (same node "test" (ir-type (third node)) (ir-base-type "bool")))
                   ((:let :decl) (same node "body" type (ir-type (fifth* node))))
                   (:letrec (same node "body" type (ir-type (fourth node))))
                   (:seq (same node "last item" type (ir-type (car (last (third node))))))
                   (:loop (same node "body" type (ir-type (sixth node))))
                   (:tuple (same node "fields" type (ir-tuple-type (mapcar #'ir-type (third node)))))
                   (:select (same node "field" type
                                  (ir-field-type (ir-type (fourth node))
                                                 (princ-to-string (third node)))))
                   (:raise (same node "exception" (ir-type (third node)) (ir-base-type "exn")))
                   ;; TYPE has a tuple argument for several parameters.
                   (:fn (same node "body" (ir-arrow-range type) (ir-type (fourth node))))
                   (:handle (same node "body" type (ir-type (third node)))
                    (dolist (rule (fourth node))
                      (same node "handler" type (ir-type (second rule)))
                      (same node "handler pattern" (ir-type (first rule)) (ir-base-type "exn"))
                      (pattern (first rule))))
                   (:match
                    (dolist (rule (fourth node))
                      (same node "rule" type (ir-type (second rule)))
                      (loop for p in (first rule)
                            for scrutinee in (third node)
                            do (same node "pattern" (ir-type p) (ir-type scrutinee))
                               (pattern p))))))
               (ir-walk-children #'walk node))
             (fifth* (node) (if (eq (ir-kind node) :let) (fifth node) (fourth node))))
      (walk node))
    (nreverse problems)))

(defun ir-lint-term (node what)
  "Lint NODE (see *SML-IR-LINT*).  Only a lowered term must have a type on
every node: the simplifier inlines templates of the Basis, which is
compiled without the checker.  In a simplified term, a type constructor
may meet another: a function inlined from a structure ascribed a
signature opaquely sees the representation of an abstract type."
  (let ((problems (let ((*ir-abstraction-tolerant* (not (equal what "lowered"))))
                    (ir-lint node (equal what "lowered")))))
    (when problems
      (if (eq *sml-ir-lint* :error)
          (error "IR type lint (~A term):~{~%  ~A~}~%in ~S" what problems (ir-display node))
          (setf *sml-ir-lint-problems*
                (append *sml-ir-lint-problems*
                        (mapcar (lambda (problem) (format nil "~A: ~A" what problem))
                                problems)))))))

;;; Compiling

(defun ir-compile-term (lowered lexical-env)
  "The CL form of the term LOWERED, simplified, compiled where LEXICAL-ENV
is in scope."
  (let ((simplified (if (and *sml-ir-simplify* (not (ir-trivial-term-p lowered)))
                        (ir-simplify lowered)
                        lowered)))
    (when *sml-ir-lint*
      (ir-lint-term lowered "lowered")
      (unless (eq simplified lowered)
        (ir-lint-term simplified "simplified")))
    ;; A program's IR declarations (*SML-IR-PROGRAM*) need every term,
    ;; trivial ones included (`val z = 3').
    (when (and *sml-ir-log* (or *sml-ir-program* (not (ir-trivial-term-p lowered))))
      (push (cons lowered simplified) (car *sml-ir-log*)))
    (let ((*ir-emit-scope* (lexical-environment-symbols lexical-env)))
      (ir-emit simplified))))

(defun ir-compile-expression (ast local-exceptions lexical-env)
  "The CL form of expression AST, compiled through the IR."
  (ir-compile-term (ir-lower ast local-exceptions lexical-env) lexical-env))

(defun ir-compile-fn-clauses (clauses local-exceptions function-name outer-lexical-env
                              &optional tupled)
  "The CL form of the function CLAUSES define (COMPILE-FN-CLAUSES-OF-ARITY),
compiled through the IR."
  (ir-compile-term (ir-lower-fn-clauses clauses local-exceptions function-name
                                        outer-lexical-env tupled)
                   outer-lexical-env))

;;; Showing the IR

(defun ir-display (node)
  "NODE as a form to print: each type, when known, follows its node's
fields as :type \"...\"; patterns are shown the same way."
  (cond
    ((and (consp node) (keywordp (first node)) (consp (rest node))
          (or (null (second node)) (and (consp (second node))
                                        (member (first (second node))
                                                '(:con :tyvar :arrow :record)))))
     (let ((type (and (second node) (ir-type-string (second node)))))
       `(,(first node) ,@(mapcar #'ir-display (cddr node)) ,@(and type `(:type ,type)))))
    ((consp node) (cons (ir-display (car node)) (ir-display (cdr node))))
    (t node)))

;;; Declarations (docs/IR.md, "Modules").  While *SML-IR-PROGRAM* holds a
;;; list, each top-level or structure-level declaration compiled adds its
;;; IR declarations to it: structures are flattened, their members named
;;; by their paths.  A functor is recorded with its AST, and each
;;; application with the functor and its argument, so that a whole-program
;;; pass can specialize the applications.

(defvar *sml-ir-program* nil
  "When a cons, the IR declarations of the program being compiled are
pushed onto its car.")

(defvar *ir-val-patterns* nil
  "While a declaration is recorded (*SML-IR-PROGRAM*), a cons whose car
collects (IR-PATTERN TERM-INDEX ((VARIABLE . CELL) ...)) for each top-level
`val' binding whose pattern is not a variable, TERM-INDEX being the
position of its expression among the declaration's terms.")

(defun ir-declaration-path (name)
  (current-qualified-sml-name name))

(defun ir-declarations-of (dec terms)
  "The IR declarations of top-level or structure-level declaration DEC,
for which the IR terms TERMS ((lowered . simplified) ...) were compiled."
  (let ((simplified (mapcar #'cdr terms)))
    (flet ((types (key) (nth-value 0 (funcall 'ir-checker-declaration-env dec key)))
           (values* (key) (nth-value 1 (funcall 'ir-checker-declaration-env dec key))))
      (case (car dec)
        ((:val :vals :val-rec :val-recs :fun :funs :expr)
         (list (list* :val
                     (mapcar (lambda (binding)
                               (destructuring-bind (name status type) binding
                                 (declare (ignore status))
                                 (list (ir-declaration-path name)
                                       (ignore-errors (current-binding-symbol name))
                                       type)))
                             (values* :env))
                     simplified
                     ;; A binding `val pat = e' whose pattern is not a
                     ;; variable: its IR pattern, and its variables' cells.
                     (and (car *ir-val-patterns*)
                          (list :patterns (reverse (car *ir-val-patterns*)))))))
        (:datatype
         (list (list :datatype (ir-datatype-entries dec) simplified)))
        (:datatype-replication
         (list (list :datatype-replication (ir-declaration-path (second dec)) (third dec))))
        (:type
         (list (list :type (mapcar (lambda (binding)
                                     (destructuring-bind (name arity kind params body ctors) binding
                                       (declare (ignore arity kind ctors))
                                       (list (ir-declaration-path name) params body)))
                                   (types :env)))))
        ((:exception :exception-alias)
         (list (list (car dec) (ir-declaration-path (second dec))
                     (and (eq (car dec) :exception-alias) (third dec))
                     (third (first (values* :env)))
                     ;; The exception's cell.
                     (ignore-errors (current-binding-symbol (second dec))))))
        (:structure
         (let ((sigexp (getf (cdddr dec) :sigexp)))
           (list (list :structure (ir-declaration-path (second dec))
                       :signature sigexp
                       :opaque (getf (cdddr dec) :opaque)
                       ;; The types as the signature exports them: an
                       ;; abstract type's body is a type constructor of its
                       ;; own, without constructors.
                       :types (mapcar (lambda (binding)
                                        (destructuring-bind (name arity kind params body ctors)
                                            binding
                                          (list name arity kind params body
                                                (mapcar #'first ctors))))
                                      (types :members))))))
        (:structure-alias
         (list (list :structure-alias (ir-declaration-path (second dec)) (third dec))))
        (:structure-app
         (list (list :functor-application (ir-declaration-path (second dec))
                     (third dec) (fourth dec)
                     :signature (getf (cddddr dec) :sigexp)
                     :opaque (getf (cddddr dec) :opaque))))
        (:functor
         (list (list :functor (second dec) :ast dec)))
        (:signature (list (list :signature (second dec) (third dec))))
        (:open (list (list :open (second dec))))
        ;; `local A in B end': B's declarations are recorded on their own;
        ;; A's bindings are lexical variables of the program.
        (:local
         (list (list :local
                     (mapcar (lambda (local-dec)
                               (list (car local-dec)
                                     (and (consp local-dec)
                                          (reverse (local-declaration-lexical-bindings local-dec)))
                                     (and (eq (car local-dec) :exception)
                                          (getf (cddr local-dec) :arg-type)
                                          t)
                                     ;; A datatype's types; a structure's
                                     ;; declarations.
                                     (case (car local-dec)
                                       (:datatype (ir-datatype-entries local-dec))
                                       ((:structure :structure-alias :structure-app :functor)
                                        (gethash local-dec *ir-nested-structures*)))))
                             (second dec))
                     simplified)))))))

(defun ir-datatype-entries (dec)
  "The datatypes of declaration DEC, as a :datatype declaration lists them:
((path arity params ((con-path type flat-arity) ...)) ...)."
  (mapcar (lambda (binding)
            (destructuring-bind (name arity kind params body ctors) binding
              (declare (ignore kind body))
              (list (ir-declaration-path name) arity params
                    (mapcar (lambda (ctor)
                              (let ((def (find (first ctor) (third dec)
                                               :key #'second :test #'equal)))
                                (list (ir-declaration-path (first ctor))
                                      (second ctor)
                                      (and def (datatype-constructor-flat-arity def)))))
                            ctors))))
          (nth-value 0 (ir-checker-declaration-env dec :env))))

(defun ir-compile-nested-structure (dec thunk)
  "The form THUNK compiles for DEC, a structure (or functor) declared by
`let' or `local' inside an expression, as a LEXICAL-STRUCTURE-FORM (or by a
top-level `local', as it is).  Its IR
declarations are kept for DEC rather than added to the program's: they
belong to the enclosing declaration, and the compiler compiles a `let' or
`local' body before its declarations."
  (flet ((compile-form ()
           (if *sml-top-level-local*
               (funcall thunk)
               ;; Nothing is hoisted out of it, and its functions have no
               ;; global workers: its members become lexical variables
               ;; (LEXICAL-STRUCTURE-FORM), which a global function would
               ;; not see.
               (let ((*sml-hoisting-enabled* nil)
                     (*sml-tupled-workers* nil))
                 (lexical-structure-form dec (funcall thunk))))))
    (if *sml-ir-program*
        (let ((declarations (list nil)))
          (prog1 (let ((*sml-ir-program* declarations))
                   (compile-form))
            (setf (gethash dec *ir-nested-structures*) (reverse (car declarations)))))
        (compile-form))))

;;; A structure declared inside an expression (by `let', or by `local' in a
;;; `let') is evaluated each time the expression is (Definition, rules 107
;;; and 156 ff.), so its members must not be shared between evaluations.
;;; The structure is compiled as at top level, binding each member in a
;;; cell with DEFPARAMETER, and then those cells are made lexical variables
;;; around the structure and the scope of its name: the form
;;; (lexical-structure (cell ...) members form) that LEXICAL-STRUCTURE-FORM
;;; returns stands for FORM until EMIT-LEXICAL-STRUCTURE puts the scope in
;;; it.  Top-level structures are not affected.

(defun lexical-structure-form (dec form)
  "FORM, compiled for DEC, a structure declared inside an expression, marked
with the cells it binds and the members it publishes ((qualified-name
. cell) ...), for EMIT-LEXICAL-STRUCTURE."
  (let ((cells nil)
        (members nil))
    (labels ((walk (x)
               (when (consp x)
                 (case (car x)
                   (quote nil)
                   (defparameter (pushnew (second x) cells))
                   (publish-sml-binding
                    (when (and (consp (second x)) (consp (third x)))
                      (push (cons (symbol-name (second (second x))) (second (third x)))
                            members))))
                 (unless (eq (car x) 'quote)
                   (loop for tail on x
                         do (walk (car tail))
                         while (consp (cdr tail)))))))
      (walk form))
    (if (and cells (member (car dec) '(:structure :structure-alias :structure-app)))
        `(lexical-structure ,(nreverse cells) ,members ,form)
        form)))

(defun lexical-structure-cells (form)
  "The cells FORM, a LEXICAL-STRUCTURE-FORM, makes lexical variables."
  (and (consp form) (eq (car form) 'lexical-structure) (second form)))

(defun emit-lexical-structure (form body)
  "The CL form for FORM, a LEXICAL-STRUCTURE-FORM, followed by BODY, the
form of the scope of the structure's name: each cell is renamed to a
fresh lexical variable, bound around both."
  (if (not (and (consp form) (eq (car form) 'lexical-structure)))
      `(progn ,form ,body)
      (destructuring-bind (cells members structure-form) (rest form)
        (let* ((variables (mapcar (lambda (cell) (cons cell (make-symbol (symbol-name cell))))
                                  cells))
               (members (remove-if-not (lambda (member) (assoc (cdr member) variables))
                                       members)))
          (labels ((variable (symbol) (cdr (assoc symbol variables)))
                   (quoted-cell (x)
                     (and (consp x) (eq (car x) 'quote) (variable (second x))))
                   (structure-value (package name)
                     ;; The members bound lexically, then the rest of the
                     ;; structure (constructors) as bound at top level.
                     (let ((prefix (format nil "~A." name)))
                       `(make-sml-structure
                         (list ,@(loop for (qualified . cell) in members
                                       when (string-prefix-p prefix qualified)
                                         collect `(cons ,(subseq qualified (length prefix))
                                                        ,(variable cell))))
                         (list (cons "" (sml-global-structure ,package ,name))))))
                   (rewrite (x)
                     (cond ((symbolp x) (or (variable x) x))
                           ((atom x) x)
                           ((eq (car x) 'quote) x)
                           ((and (eq (car x) 'defparameter) (variable (second x)))
                            `(setq ,(variable (second x)) ,(rewrite (third x))))
                           ((and (eq (car x) 'declaim) (consp (second x))
                                 (eq (car (second x)) 'special))
                            (let ((specials (remove-if #'variable (cdr (second x)))))
                              (and specials `(declaim (special ,@specials)))))
                           ((and (eq (car x) 'publish-sml-binding) (quoted-cell (third x)))
                            nil)
                           ;; An exception alias of a member (COMPILE-LOCAL-DECL).
                           ((and (eq (car x) 'boundp) (quoted-cell (second x)))
                            t)
                           ((and (eq (car x) 'symbol-value) (quoted-cell (second x))))
                           ((and (member (car x) '(register-sml-structure-members
                                                   sml-global-structure))
                                 (stringp (third x))
                                 (some (lambda (member)
                                         (string-prefix-p (format nil "~A." (third x))
                                                          (car member)))
                                       members))
                            (if (eq (car x) 'sml-global-structure)
                                (structure-value (second x) (third x))
                                nil))
                           (t (let ((result (loop for tail on x
                                                  collect (rewrite (car tail))
                                                  while (consp (cdr tail)))))
                                (let ((end (cdr (last x))))
                                  (when end (setf (cdr (last result)) (rewrite end))))
                                result)))))
            `(let ,(mapcar #'cdr variables)
               (declare (ignorable ,@(mapcar #'cdr variables)))
               ,(rewrite structure-form)
               ,(rewrite body)))))))

(defvar *ir-program-mark* :none
  "The declarations recorded before the declaration being compiled.")

(defun ir-note-declaration (dec terms)
  "Record the IR declarations of DEC (see *SML-IR-PROGRAM*).  Those of a
`local' come before the declarations of its body, which were recorded
first."
  (when (and *sml-ir-program* (null *functor-scope-variables*))
    (let ((declarations (reverse (ir-declarations-of dec terms))))
      (if (and (eq (car dec) :local) (not (eq *ir-program-mark* :none))
               (tailp *ir-program-mark* (car *sml-ir-program*)))
          (setf (car *sml-ir-program*)
                (append (ldiff (car *sml-ir-program*) *ir-program-mark*)
                        declarations
                        *ir-program-mark*))
          (setf (car *sml-ir-program*) (append declarations (car *sml-ir-program*)))))))

(defun sml-ir-program (source &key (package "SML.IR-PROGRAM"))
  "The IR declarations of the SML program SOURCE, compiled (not run) in
PACKAGE, in order: a structure's members come before the structure."
  (let ((*sml-ir-program* (list nil)))
    (compile-sml-program-string source :package package)
    (reverse (car *sml-ir-program*))))
