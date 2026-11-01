(in-package #:cl-sml.types)

;;; Elaboration of the Core (Definition sections 4.6-4.11, rules 1-55;
;;; HaMLet elab/ElabCore and Clos).  The input is the parser's AST
;;; (src/parser.lisp), whose derived forms (Appendix A: tuples, lists,
;;; `#lab', `case', `if', `andalso', `while', sequences, `fun', ...) are
;;; elaborated directly as their expansions would be, so that errors point
;;; at the code the user wrote.  Each case names its rule.
;;;
;;; Notes (choices the Definition leaves open, compare HaMLet's ElabCore):
;;; - Overloaded identifiers and literals are resolved, by defaulting if
;;;   need be, at the end of the enclosing top-level declaration (HaMLet:
;;;   the innermost value declaration).  Overloaded type variables are never
;;;   generalised.
;;; - A flexible record must be resolved by the end of the innermost
;;;   enclosing value declaration (as HaMLet).
;;; - `let' raises the level, so that the level of a type name tells the
;;;   scope of a local datatype; a variable of an outer level cannot be
;;;   bound to a type containing it (side condition of rule 4).

(defvar *native-unchecked-long-ids* nil
  "When true, a long identifier into an unknown structure elaborates to a
fresh type printed `?' instead of being an error, and opening an unknown
structure binds nothing.  This lets the checker look at the rest of a
program that uses structures it does not know.")

(defvar *native-relaxed-value-restriction* nil
  "When true, an expansive value binding still generalises the type
variables that occur only covariantly in its type (Garrigue's relaxed value
restriction, as in OCaml), not just non-expansive ones (SML '97, section
4.7).  Datatypes other than list, option and vector count as invariant.")

(defvar *native-match-warnings* t
  "When true, non-exhaustive and redundant matches are signalled as
SML-MATCH-WARNINGs (Definition section 4.11).")

;;;; Conditions

(define-condition sml-native-type-error (sml-static-type-error)
  ((message :initarg :message :reader sml-native-type-error-message)
   (line :initarg :line :initform nil :reader sml-native-type-error-line)
   (column :initarg :column :initform nil :reader sml-native-type-error-column)
   ;; The two types that failed to unify, as strings, or NIL.
   (types :initarg :types :initform nil :reader sml-native-type-error-types))
  (:report (lambda (condition stream)
             (format stream "~@[~A:~]~@[~D:~]~@[~D: ~]type error: ~A~{~%  ~A~}"
                     (cl-sml::sml-static-type-error-filename condition)
                     (sml-native-type-error-line condition)
                     (sml-native-type-error-column condition)
                     (sml-native-type-error-message condition)
                     (let ((types (sml-native-type-error-types condition)))
                       (and types
                            (list (format nil "expected: ~A" (first types))
                                  (format nil "found:    ~A" (second types)))))))))

(define-condition sml-match-warning (warning)
  ((message :initarg :message :reader sml-match-warning-message)
   (line :initarg :line :initform nil :reader sml-match-warning-line)
   (column :initarg :column :initform nil :reader sml-match-warning-column))
  (:report (lambda (condition stream)
             (format stream "~@[~D:~]~@[~D: ~]warning: ~A"
                     (sml-match-warning-line condition)
                     (sml-match-warning-column condition)
                     (sml-match-warning-message condition)))))

;;;; State of one elaboration

(defvar *source* nil "The text being checked, for positions.")
(defvar *filename* nil)

(defvar *annotations* (make-hash-table :test 'eq)
  "AST node -> plist of what elaboration learned about it.")

(defvar *overloaded* nil
  "The overloaded type variables of the current top-level declaration, for
defaulting (Appendix E).")

(defvar *deferred-records* nil
  "(node . record type) of the flexible records of the current value
declaration (Section 4.11, item 1).")

(defvar *deferred-matches* nil
  "(kind node rows) of the matches of the current value declaration, to
check once its types are resolved (Section 4.11, items 2 and 3).")

(defvar *saw-unknown* nil
  "True once the current top-level declaration used an unchecked long
identifier.")

(defvar *warnings* nil)

(defvar *position-fallback* nil
  "The declaration being elaborated, whose position an error reports when
the parser recorded none for the offending phrase.")

(defun annotate (node key value)
  (when (or (consp node) (stringp node))
    (setf (getf (gethash node *annotations*) key) value)))

;;;; Positions and errors

(defun node-offset (node)
  "The source offset where NODE, or the first of its parts the parser
recorded a position for, starts."
  (labels ((walk (x depth)
             (cond ((> depth 12) nil)
                   ((or (consp x) (stringp x))
                    (or (and cl-sml::*sml-ast-positions*
                             (gethash x cl-sml::*sml-ast-positions*))
                        (and cl-sml::*sml-declaration-positions*
                             (gethash x cl-sml::*sml-declaration-positions*))
                        (and (consp x)
                             (loop for tail on x
                                   while (consp tail)
                                   thereis (walk (car tail) (1+ depth)))))))))
    (walk node 0)))

(defun node-line-column (node)
  (let ((offset (or (node-offset node) (node-offset *position-fallback*))))
    (if (and offset *source*)
        (cl-sml::sml-offset-line-column *source* offset)
        (values nil nil))))

(defun elab-error (node control &rest arguments)
  (multiple-value-bind (line column) (node-line-column node)
    (let ((message (apply #'format nil control arguments)))
      (error 'sml-native-type-error
             :source *source* :filename *filename* :cause message
             :message message :line line :column column))))

(defun clash (node message expected found failure)
  (let ((namer (make-type-namer)))
    (let ((expected (type->string expected namer))
          (found (type->string found namer)))
      (multiple-value-bind (line column) (node-line-column node)
        (let ((message (format nil "~A (~A)" message (unify-failure-reason failure))))
          (error 'sml-native-type-error
                 :source *source* :filename *filename* :cause message
                 :message message :line line :column column
                 :types (list expected found)))))))

(defmacro with-unification ((node message expected found) &body body)
  "Run BODY, reporting a failed unification as a type error at NODE that
shows the types EXPECTED and FOUND."
  (let ((failure (gensym "FAILURE")))
    `(handler-case (progn ,@body)
       (unify-failure (,failure)
         (clash ,node ,message ,expected ,found ,failure)))))

(defun unify-at (node message expected found)
  (with-unification (node message expected found)
    (unify expected found)))

(defun match-warning (node control &rest arguments)
  (let ((message (apply #'format nil control arguments)))
    (multiple-value-bind (line column) (node-line-column node)
      (push (list line column message) *warnings*)
      (when *native-match-warnings*
        (warn 'sml-match-warning :message message :line line :column column)))))

;;;; Identifiers

(defun long-name-p (name)
  (and (stringp name) (position #\. name) (> (length name) 1)
       ;; `...' and symbolic identifiers made of dots are not long.
       (alpha-char-p (char name 0))))

(defun long-strid-env (c name node)
  "The environment of the structure that long identifier NAME is in, and
NAME's last part.  An unknown structure is an error, or, when
*NATIVE-UNCHECKED-LONG-IDS* is true, gives NIL: the identifier is then
unchecked."
  (multiple-value-bind (strids last) (split-long-name name)
    (multiple-value-bind (env missing) (env-lookup-strids (context-env c) strids)
      (cond (env (values env last))
            (*native-unchecked-long-ids* (setf *saw-unknown* t) (values nil last))
            (t (elab-error node "unknown structure ~A in ~A" missing name))))))

(defun lookup-long-vid (c name node)
  "The VALSTR of identifier NAME, or NIL for an unchecked long identifier."
  (if (long-name-p name)
      (multiple-value-bind (env last) (long-strid-env c name node)
        (and env
             (or (cdr (assoc last (env-ve env) :test #'string=))
                 (elab-error node "unknown identifier ~A" name))))
      (or (lookup-vid c name)
          (elab-error node "unknown identifier ~A" name))))

(defun find-long-vid (c name)
  "The VALSTR of identifier NAME, or NIL; never an error."
  (if (long-name-p name)
      (multiple-value-bind (strids last) (split-long-name name)
        (let ((env (env-lookup-strids (context-env c) strids)))
          (and env (cdr (assoc last (env-ve env) :test #'string=)))))
      (lookup-vid c name)))

(defun lookup-long-tycon (c name node)
  "The TYSTR of type constructor NAME, or NIL when unchecked."
  (if (long-name-p name)
      (multiple-value-bind (env last) (long-strid-env c name node)
        (and env
             (or (cdr (assoc last (env-te env) :test #'string=))
                 (elab-error node "unknown type constructor ~A" name))))
      (or (lookup-tycon c name)
          (elab-error node "unknown type constructor ~A" name))))

(defun lookup-long-strid (c name node)
  "The environment of structure NAME, or NIL when unchecked."
  (multiple-value-bind (strids last) (split-long-name name)
    (multiple-value-bind (env missing)
        (env-lookup-strids (context-env c) (append strids (list last)))
      (cond (env env)
            (*native-unchecked-long-ids* (setf *saw-unknown* t) nil)
            (t (elab-error node "unknown structure ~A~@[ in ~A~]" missing
                           (and (string/= missing name) name)))))))

(defun unknown-type ()
  (fresh-tvar :unknown t))

(defun note-overloaded (node vars)
  (dolist (var vars)
    (when (tvar-overload var)
      (push (cons node var) *overloaded*))))

(defun literal-type (class node)
  "Rule 1 / Appendix E.1: a special constant of overloading CLASS."
  (let ((var (fresh-tvar :overload class)))
    (push (cons node var) *overloaded*)
    (annotate node :type var)
    var))

;;;; Types [rules 44-49]

(defun elab-ty (c ty)
  (ecase (car ty)
    (:ty-var                            ; [Rule 44]
     (or (lookup-tyvar c (second ty))
         (elab-error ty "unbound type variable ~A" (second ty))))
    (:ty-record                         ; [Rules 45, 49]
     (let ((fields (loop for (label field-ty) in (rest ty)
                         collect (cons label (elab-ty c field-ty)))))
       (check-distinct-labels ty (mapcar #'car fields))
       (record-type fields)))
    (:ty-tuple (tuple-type (mapcar (lambda (item) (elab-ty c item)) (rest ty))))
    (:ty-con                            ; [Rule 46]
     (let* ((args (mapcar (lambda (arg) (elab-ty c arg)) (third ty)))
            (name (second ty))
            (tystr (lookup-long-tycon c name ty)))
       (if (null tystr)
           (unknown-type)
           (progn
             (unless (= (length args) (tyfcn-arity (tystr-tyfcn tystr)))
               (elab-error ty "type constructor ~A takes ~D argument~:P, not ~D"
                           name (tyfcn-arity (tystr-tyfcn tystr)) (length args)))
             (apply-tyfcn (tystr-tyfcn tystr) args)))))
    (:ty-arrow                          ; [Rule 47]
     (arrow-type (elab-ty c (second ty)) (elab-ty c (third ty))))))

(defun check-distinct-labels (node labels)
  (loop for (label . more) on labels
        when (member label more :test #'string=)
          do (elab-error node "duplicate label ~A" label)))

;;;; Expressions [rules 1-12]

(defun elab-exp (c exp)
  (let ((type (elab-exp-1 c exp)))
    type))

(defun elab-exps-record (c node items)
  "A record expression with (label exp) ITEMS [rules 3, 6]."
  (check-distinct-labels node (mapcar #'first items))
  (record-type (loop for (label item) in items
                     collect (cons label (elab-exp c item)))))

(defun elab-exp-1 (c exp)
  (cond
    ((integerp exp) (literal-type *class-int* exp)) ; [Rule 1]
    ((floatp exp) (literal-type *class-real* exp))
    ((stringp exp) (literal-type *class-string* exp))
    ((characterp exp) (literal-type *class-char* exp))
    (t
     (ecase (car exp)
       (:word (literal-type *class-word* exp))
       ((:var :ctor)                    ; [Rule 2]
        (let* ((name (second exp))
               (valstr (lookup-long-vid c name exp)))
          (if (null valstr)
              (let ((type (unknown-type)))
                (annotate exp :type type)
                type)
              (multiple-value-bind (type fresh) (instantiate (valstr-scheme valstr))
                (note-overloaded exp fresh)
                (annotate exp :status (status-string valstr))
                (annotate exp :type type)
                (annotate exp :instances fresh)
                type))))
       (:app                            ; [Rule 8]
        (let* ((function (elab-exp c (second exp)))
               (argument (elab-exp c (third exp)))
               (result (fresh-tvar))
               (shape (repr function)))
          (if (and (consp shape) (eq (car shape) :arrow))
              (progn
                (unify-at exp "operator and operand do not agree" (second shape) argument)
                (unify (third shape) result))
              (unify-at exp "applied expression is not a function"
                        function (arrow-type argument result)))
          result))
       (:unit (unit-type))
       (:tuple (elab-exps-record c exp (loop for item in (rest exp)
                                              for i from 1
                                              collect (list (princ-to-string i) item))))
       (:record (elab-exps-record c exp (rest exp)))
       (:selector                       ; Appendix A: fn {lab=vid,...} => vid
        (let* ((field (fresh-tvar))
               (record (list :record (list (cons (second exp) field)) (fresh-tvar :row t))))
          (push (cons exp record) *deferred-records*)
          (annotate exp :record record)
          (arrow-type record field)))
       (:list                           ; Appendix A: exp1 :: ... :: nil
        (let ((element (fresh-tvar)))
          (dolist (item (rest exp))
            (let ((type (elab-exp c item)))
              (unify-at item "list elements have different types" element type)))
          (list-type element)))
       (:seq                            ; Appendix A: case exp1 of _ => ...
        (let ((type nil))
          (dolist (item (rest exp) type)
            (setf type (elab-exp c item)))))
       (:let (elab-let c exp (second exp) (third exp))) ; [Rule 4]
       (:typed                          ; [Rule 9]
        (let ((type (elab-exp c (second exp)))
              (annotation (elab-ty c (third exp))))
          (unify-at exp "expression does not match its type annotation" annotation type)
          type))
       ((:andalso :orelse)              ; Appendix A: if-then-else
        (dolist (item (rest exp) (bool-type))
          (unify-at item (format nil "operand of ~(~A~) is not a bool" (car exp))
                    (bool-type) (elab-exp c item))))
       (:if                             ; Appendix A: case exp of true => | false =>
        (unify-at (second exp) "condition of if is not a bool"
                  (bool-type) (elab-exp c (second exp)))
        (let ((then (elab-exp c (third exp)))
              (else (elab-exp c (fourth exp))))
          (unify-at (fourth exp) "branches of if have different types" then else)
          then))
       (:while                          ; Appendix A
        (unify-at (second exp) "condition of while is not a bool"
                  (bool-type) (elab-exp c (second exp)))
        (elab-exp c (third exp))
        (unit-type))
       (:case                           ; Appendix A: (fn match) exp
        (let* ((subject (elab-exp c (second exp)))
               (match (elab-match c exp (cddr exp) :case)))
          (unify-at exp "case object and patterns have different types"
                    (second match) subject)
          (third match)))
       (:fn (elab-match c exp (second exp) :fn)) ; [Rule 12]
       (:handle                         ; [Rule 10]
        (let ((type (elab-exp c (second exp)))
              (match (elab-match c exp (third exp) :handle)))
          (unify-at exp "handler does not take an exception" (exn-type) (second match))
          (unify-at exp "handler and expression have different types" type (third match))
          type))
       (:raise                          ; [Rule 11]
        (unify-at exp "raised expression is not an exception"
                  (exn-type) (elab-exp c (second exp)))
        (fresh-tvar))))))

(defun elab-let (c node decs body)
  "[Rule 4] let dec in exp1; ...; expn end."
  (let ((outer *level*)
        (type nil))
    (let ((*level* (1+ outer)))
      (let* ((env (elab-decs c decs))
             (inner (context-plus-env c env)))
        (dolist (item body)
          (setf type (elab-exp inner item)))))
    (let ((escaping (find-if (lambda (tyname) (> (tyname-level tyname) outer))
                             (type-tynames type))))
      (when escaping
        (elab-error node "local type ~A escapes its scope in let expression"
                    (tyname-name escaping))))
    type))

;;;; Matches [rules 13-14]

(defun elab-match (c node rules kind)
  "The type of match RULES ((pat exp)...), whose patterns are recorded for
the checks of section 4.11 as a match of KIND (:fn, :case, :handle)."
  (let ((type nil)
        (rows nil))
    (loop for (pat exp) in rules
          do (multiple-value-bind (ve pat-type spat) (elab-pat c pat)
               (let* ((exp-type (elab-exp (context-plus-ve c ve) exp))
                      (rule-type (arrow-type pat-type exp-type)))
                 (push (list spat) rows)
                 (if type
                     (unify-at pat "match rules have different types" type rule-type)
                     (setf type rule-type)))))
    (push (list kind node (nreverse rows)) *deferred-matches*)
    (repr type)))

;;;; Patterns [rules 32-43]

(defun merge-ve (node ve1 ve2)
  "VE1 and VE2 of two parts of one pattern, which must bind distinct
variables (Section 2.9)."
  (dolist (entry ve2)
    (when (assoc (car entry) ve1 :test #'string=)
      (elab-error node "duplicate variable ~A in pattern" (car entry))))
  (append ve1 ve2))

(defun constructor-spat (valstr result-type name arg-spat)
  (if (eq (valstr-status valstr) :e)
      (list :con (valstr-id valstr) *t-exn* arg-spat)
      (let ((result (repr result-type)))
        (list :con name (if (and (consp result) (eq (car result) :con))
                            (second result)
                            *t-exn*)
              arg-spat))))

(defun elab-pat (c pat)
  "Returns the VE (alist name -> VALSTR) and type of PAT, and its simple
pattern for match checking."
  (cond
    ((integerp pat) (values nil (literal-type *class-int* pat) (list :const pat))) ; [Rule 33]
    ((floatp pat) (elab-error pat "real constant in pattern"))
    ((stringp pat) (values nil (literal-type *class-string* pat) (list :const pat)))
    ((characterp pat) (values nil (literal-type *class-char* pat) (list :const pat)))
    ((eq pat :wild) (values nil (fresh-tvar) :any)) ; [Rule 32]
    (t
     (ecase (car pat)
       (:word (values nil (literal-type *class-word* pat) (list :const (list :word (second pat)))))
       ((:pat-var :pat-ctor) (elab-pat-id c pat))
       (:pat-app (elab-pat-constructor c pat (second (second pat)) (second pat) (third pat)))
       (:pat-cons
        (elab-pat-constructor c pat "::" pat (list :pat-tuple (second pat) (third pat))))
       (:pat-nil (elab-pat-id c (list :pat-ctor "nil")))
       (:pat-unit (let ((type (unit-type))) (values nil type (list :record type nil))))
       (:pat-tuple
        (elab-pat-record c pat (loop for item in (rest pat)
                                     for i from 1
                                     collect (list (princ-to-string i) item))
                         nil))
       (:pat-record                     ; [Rules 36, 38, 39]
        (elab-pat-record c pat (remove :record-rest (rest pat))
                         (member :record-rest (rest pat))))
       (:pat-typed                      ; [Rule 42]
        (multiple-value-bind (ve type spat) (elab-pat c (second pat))
          (unify-at pat "pattern does not match its type annotation"
                    (elab-ty c (third pat)) type)
          (values ve type spat)))
       (:pat-as (elab-pat-layered c pat)))))) ; [Rule 43]

(defun elab-pat-id (c pat)
  "[Rules 34, 35] An identifier: a variable unless it is bound as a
constructor or exception constructor."
  (let* ((name (second pat))
         (long (long-name-p name))
         (valstr (if long (lookup-long-vid c name pat) (lookup-vid c name))))
    (cond
      ((and long (null valstr))         ; unchecked
       (let ((type (unknown-type)))
         (values nil type (list :con name *t-exn* nil))))
      ((and long (eq (valstr-status valstr) :v))
       (elab-error pat "~A is not a constructor" name))
      ((and valstr (not (eq (valstr-status valstr) :v))) ; [Rule 35]
       (multiple-value-bind (type fresh) (instantiate (valstr-scheme valstr))
         (note-overloaded pat fresh)
         (when (eq (car (repr type)) :arrow)
           (elab-error pat "constructor ~A needs an argument in pattern" name))
         (annotate pat :status (status-string valstr))
         (annotate pat :type type)
         (values nil type (constructor-spat valstr type name nil))))
      (t                                ; [Rule 34]
       (let ((type (fresh-tvar)))
         (annotate pat :status "v")
         (annotate pat :type type)
         (values (list (cons name (make-valstr (mono-scheme type) :v))) type :any))))))

(defun elab-pat-constructor (c pat name ctor-node arg-pat)
  "[Rule 41] longvid atpat."
  (let ((valstr (if (string= name "::")
                    (lookup-vid (make-context (starting-env)) "::")
                    (lookup-long-vid c name ctor-node))))
    (multiple-value-bind (arg-ve arg-type arg-spat) (elab-pat c arg-pat)
      (cond
        ((null valstr)
         (values arg-ve (unknown-type) (list :con name *t-exn* arg-spat)))
        ((eq (valstr-status valstr) :v)
         (elab-error ctor-node "~A is not a constructor" name))
        (t
         (multiple-value-bind (type fresh) (instantiate (valstr-scheme valstr))
           (note-overloaded ctor-node fresh)
           (let ((type (repr type)))
             (unless (eq (car type) :arrow)
               (elab-error ctor-node "constructor ~A takes no argument" name))
             (annotate ctor-node :status (status-string valstr))
             (annotate ctor-node :type type)
             (unify-at pat (format nil "argument of constructor ~A has the wrong type" name)
                       (second type) arg-type)
             (values arg-ve (third type)
                     (constructor-spat valstr (third type) name arg-spat)))))))))

(defun elab-pat-record (c pat items flexible)
  (let ((ve nil)
        (fields nil))
    (check-distinct-labels pat (mapcar #'first items))
    (loop for (label item) in items
          do (multiple-value-bind (item-ve item-type item-spat) (elab-pat c item)
               (setf ve (merge-ve item ve item-ve))
               (push (list* label item-type item-spat) fields)))
    (setf fields (nreverse fields))
    (let ((type (list :record
                      (sort-fields (mapcar (lambda (field) (cons (first field) (second field)))
                                           fields))
                      (and flexible (fresh-tvar :row t)))))
      (when flexible
        (push (cons pat type) *deferred-records*)
        (annotate pat :record type))
      (values ve type
              (list :record type (mapcar (lambda (field) (cons (first field) (cddr field)))
                                         fields))))))

(defun elab-pat-layered (c pat)
  "[Rule 43] <op> vid <: ty> as pat."
  (let* ((var-pat (second pat))
         (ty (and (eq (car var-pat) :pat-typed) (third var-pat)))
         (var-node (if ty (second var-pat) var-pat))
         (name (second var-node))
         (valstr (lookup-vid c name)))
    (when (and valstr (not (eq (valstr-status valstr) :v)))
      (elab-error var-node "constructor ~A cannot be bound by a layered pattern" name))
    (multiple-value-bind (ve type spat) (elab-pat c (third pat))
      (when ty
        (unify-at pat "pattern does not match its type annotation" (elab-ty c ty) type))
      (annotate var-node :status "v")
      (annotate var-node :type type)
      (values (merge-ve pat (list (cons name (make-valstr (mono-scheme type) :v))) ve)
              type spat))))

;;;; Non-expansive expressions [Section 4.7]

(defun constructor-exp-p (c exp)
  (let ((exp (if (and (consp exp) (eq (car exp) :typed)) (second exp) exp)))
    (and (consp exp)
         (member (car exp) '(:var :ctor))
         (not (string= (second exp) "ref"))
         (let ((valstr (find-long-vid c (second exp))))
           (and valstr (member (valstr-status valstr) '(:c :e)))))))

(defun nonexpansive-p (c exp)
  (cond
    ((atom exp) t)
    (t (case (car exp)
         ((:var :ctor :fn :selector :unit :word) t)
         ((:tuple :list) (every (lambda (item) (nonexpansive-p c item)) (rest exp)))
         (:record (every (lambda (field) (nonexpansive-p c (second field))) (rest exp)))
         (:typed (nonexpansive-p c (second exp)))
         (:app (and (constructor-exp-p c (second exp))
                    (nonexpansive-p c (third exp))))
         (t nil)))))

;;;; Closure [Section 4.8] and the relaxed value restriction

(defun covariant-tvars (type)
  "The variables of TYPE that occur only in covariant positions."
  (let ((polarity nil))
    (labels ((walk (type sign)
               (let ((type (repr type)))
                 (cond ((tvar-p type)
                        (let ((entry (assoc type polarity)))
                          (if entry
                              (unless (eq (cdr entry) sign) (setf (cdr entry) :bad))
                              (push (cons type sign) polarity))))
                       (t (ecase (car type)
                            (:con (let ((sign (if (member (second type)
                                                          (list *t-list* *t-option* *t-vector*))
                                                  sign
                                                  :inv)))
                                    (dolist (arg (cddr type))
                                      (walk arg sign))))
                            (:arrow (walk (second type) (if (eq sign :pos) :neg :inv))
                                    (walk (third type) sign))
                            (:record (dolist (field (expand-record type))
                                       (walk (cdr field) sign)))))))))
      (walk type :pos)
      (loop for (var . sign) in polarity
            when (eq sign :pos) collect var))))

(defun generalizable-tvars (type outer-level nonexpansive)
  "The variables of TYPE to quantify at a value binding of level
OUTER-LEVEL: rigid ones by name, then by creation order (as HaMLet's Clos
and TypeScheme.normalise order them)."
  (let* ((candidates (remove-if (lambda (var)
                                  (or (<= (tvar-level var) outer-level)
                                      (tvar-overload var)
                                      (tvar-row var)
                                      (tvar-unknown var)))
                                (reverse (free-tvars type))))
         (vars (cond (nonexpansive candidates)
                     (*native-relaxed-value-restriction*
                      (intersection candidates (covariant-tvars type)))
                     (t nil))))
    (append (sort (remove-if-not #'tvar-rigid vars) #'string< :key #'tvar-name)
            (sort (remove-if #'tvar-rigid vars) #'< :key #'tvar-id))))

;;;; Value declarations [rules 15, 25, 26]

(defun fn-exp-p (exp)
  (and (consp exp)
       (or (eq (car exp) :fn)
           (and (eq (car exp) :typed) (fn-exp-p (second exp))))))

(defun elab-fun-clauses (c name clauses node)
  "Appendix A: fun f atpat11 ... atpat1n = exp1 | ... is
val rec f = fn x1 => ... fn xn => case (x1, ..., xn) of (atpat11, ...) => exp1 | ..."
  (let* ((arity (length (first (first clauses))))
         (params (loop repeat arity collect (fresh-tvar)))
         (result (fresh-tvar))
         (type (reduce #'arrow-type params :from-end t :initial-value result))
         (rows nil))
    (loop for (pats body) in clauses
          do (let ((ve nil)
                   (types nil)
                   (spats nil))
               (dolist (pat pats)
                 (multiple-value-bind (pat-ve pat-type spat) (elab-pat c pat)
                   (setf ve (merge-ve pat ve pat-ve))
                   (push pat-type types)
                   (push spat spats)))
               (loop for param in params
                     for pat in pats
                     for pat-type in (reverse types)
                     do (unify-at pat (format nil "clauses of ~A have different argument types" name)
                                  param pat-type))
               (unify-at body (format nil "clauses of ~A have different result types" name)
                         result (elab-exp (context-plus-ve c ve) body))
               (push (nreverse spats) rows)))
    (push (list :fun node (nreverse rows)) *deferred-matches*)
    type))

(defun val-dec-parts (node)
  "NODE, a value or function declaration, as explicit type variables, plain
bindings ((pat exp)...) and recursive bindings ((name :exp exp ty node) or
(name :clauses clauses nil node))."
  (let ((plain nil) (recursive nil) (explicit nil))
    (labels ((walk (node)
               (ecase (car node)
                 (:val (destructuring-bind (pat exp &key type tyvars and &allow-other-keys) (rest node)
                         (declare (ignore and))
                         (when tyvars (setf explicit tyvars))
                         (push (list (if type (list :pat-typed pat type) pat) exp) plain)))
                 (:val-rec (destructuring-bind (name exp &key type tyvars and &allow-other-keys) (rest node)
                             (declare (ignore and))
                             (when tyvars (setf explicit tyvars))
                             (push (list name :exp exp type node) recursive)))
                 (:fun (destructuring-bind (name clauses &key tyvars and &allow-other-keys) (rest node)
                         (declare (ignore and))
                         (when tyvars (setf explicit tyvars))
                         (push (list name :clauses clauses nil node) recursive)))
                 ((:vals :val-recs :funs) (mapc #'walk (rest node)))
                 (:expr (push (list (list :pat-var "it") (second node)) plain)))))
      (walk node))
    (values explicit (nreverse plain) (nreverse recursive))))

(defun elab-val-dec (c node)
  "[Rule 15] val tyvarseq valbind, and its derived forms."
  (multiple-value-bind (explicit plain recursive) (val-dec-parts node)
    (dolist (name explicit)
      (when (lookup-tyvar c name)
        (elab-error node "type variable ~A shadows one already in scope" name)))
    (let* ((outer *level*)
           (*level* (1+ outer))
           (implicit (remove-if (lambda (name) (lookup-tyvar c name))
                                (valbind-unguarded-tyvars plain recursive)))
           (names (remove-duplicates (append explicit implicit) :test #'string= :from-end t))
           (u (mapcar (lambda (name) (cons name (make-rigid-tvar name))) names))
           (c1 (context-plus-u c u))
           (*deferred-records* nil)
           (*deferred-matches* nil)
           (bindings nil))              ; (name type nonexpansive-p)
      ;; [Rule 25]
      (loop for (pat exp) in plain
            do (multiple-value-bind (ve pat-type spat) (elab-pat c1 pat)
                 (let ((exp-type (elab-exp c1 exp)))
                   (unify-at pat "pattern and expression of val have different types"
                             pat-type exp-type)
                   (push (list :binding pat (list (list spat))) *deferred-matches*)
                   (let ((nonexpansive (nonexpansive-p c exp)))
                     (loop for (name . valstr) in ve
                           do (push (list name (scheme-type (valstr-scheme valstr)) nonexpansive)
                                    bindings))))))
      ;; [Rule 26]
      (when recursive
        (let* ((rec-ve (loop for (name) in recursive
                             collect (cons name (make-valstr (mono-scheme (fresh-tvar)) :v))))
               (c2 (context-plus-ve c1 rec-ve)))
          (loop for (name kind body ty rec-node) in recursive
                for (nil . valstr) in rec-ve
                do (let ((existing (lookup-vid c name)))
                     (when (and existing (not (eq (valstr-status existing) :v)))
                       (elab-error rec-node "val rec cannot rebind constructor ~A" name)))
                   (let ((type (if (eq kind :clauses)
                                   (elab-fun-clauses c2 name body rec-node)
                                   (progn
                                     (unless (fn-exp-p body)
                                       (elab-error rec-node "the right-hand side of val rec must be fn"))
                                     (elab-exp c2 body)))))
                     (when ty
                       (unify-at rec-node "val rec does not match its type annotation"
                                 (elab-ty c2 ty) type))
                     (unify-at rec-node (format nil "recursive uses of ~A have a different type" name)
                               (scheme-type (valstr-scheme valstr)) type)
                     (push (list name (scheme-type (valstr-scheme valstr)) t) bindings)))))
      (setf bindings (nreverse bindings))
      (loop for (name . more) on (mapcar #'first bindings)
            when (member name more :test #'string=)
              do (elab-error node "~A is bound twice in one value declaration" name))
      ;; Further restriction [Section 4.11, item 1]: flexible records.
      (loop for (record-node . type) in (reverse *deferred-records*)
            when (nth-value 1 (expand-record type))
              do (elab-error record-node "unresolved flexible record type ~A"
                             (type->string type)))
      ;; Closure [Section 4.8]
      (let ((ve (loop for (name type nonexpansive) in bindings
                      collect (let ((type (zonk type)))
                                (cons name (make-valstr
                                            (make-scheme (generalizable-tvars type outer nonexpansive)
                                                         type)
                                            :v))))))
        ;; Explicit type variables must be generalised [rule 15 side condition].
        (loop for (name . var) in u
              when (and (not (tvar-link var))
                        (some (lambda (entry)
                                (member var (scheme-free-tvars (valstr-scheme (cdr entry)))))
                              ve))
                do (elab-error node "explicit type variable ~A cannot be generalised" name))
        ;; Further restriction [Section 4.11, items 2 and 3]: matches.
        (check-deferred-matches)
        (annotate node :env ve)
        (make-env ve nil)))))

(defun check-deferred-matches ()
  (loop for (kind node rows) in (reverse *deferred-matches*)
        do (multiple-value-bind (exhaustive redundant) (check-match rows)
             (annotate node :exhaustive exhaustive)
             (annotate node :redundant redundant)
             (when (and (not exhaustive) (not (eq kind :handle)))
               (match-warning node (if (eq kind :binding)
                                       "binding is not exhaustive"
                                       "match is not exhaustive")))
             (dolist (index redundant)
               (match-warning node "match rule ~D is redundant" (1+ index))))))

;;;; Type and datatype declarations [rules 16-19, 27-29]

(defun elab-typbinds (c bindings)
  "[Rule 27] Type bindings ((tyvars name ty)...), simultaneous."
  (check-distinct-labels bindings (mapcar #'second bindings))
  (loop for (tyvars name ty) in bindings
        collect (let* ((params (mapcar #'make-rigid-tvar tyvars))
                       (inner (make-context (context-env c)
                                            (mapcar #'cons tyvars params))))
                  (cons name (make-tystr (make-tyfcn params (elab-ty inner ty)) nil :type)))))

(defun datatype-admits-equality-p (type)
  "Does TYPE admit equality, assuming its type variables do (Section 4.9)?"
  (let ((type (repr type)))
    (cond ((tvar-p type) t)
          (t (ecase (car type)
               (:con (case (tyname-equality (second type))
                       (:ref t)
                       ((nil) nil)
                       (t (every #'datatype-admits-equality-p (cddr type)))))
               (:arrow nil)
               (:record (every (lambda (field) (datatype-admits-equality-p (cdr field)))
                               (expand-record type))))))))

(defun elab-datatype (c node)
  "[Rules 17, 28, 29] datatype datbind <withtype typbind>.  Returns the
environment and the new type names."
  (destructuring-bind (&key datbinds withtype &allow-other-keys) (cdddr node)
    (check-distinct-labels node (append (mapcar #'second datbinds) (mapcar #'second withtype)))
    (check-distinct-labels node (loop for (nil nil ctors) in datbinds
                                      append (mapcar #'second ctors)))
    (let* ((entries
             ;; [Rule 28, part 1]: the type names, assumed to admit equality.
             (loop for (tyvars name ctors) in datbinds
                   collect (let ((params (mapcar #'make-rigid-tvar tyvars)))
                             (list name params
                                   (make-tyname name (length params)
                                                :level *level*
                                                :constructors
                                                (mapcar (lambda (ctor)
                                                          (cons (second ctor)
                                                                (and (getf (cddr ctor) :has-args) t)))
                                                        ctors))
                                   ctors))))
           (te-rec (loop for (name params tyname) in entries
                         collect (cons name (make-tystr (make-tyfcn params
                                                                    (apply #'con-type tyname params))
                                                        nil :datatype))))
           (c1 (context-plus-te c te-rec))
           ;; Appendix A: withtype abbreviations are visible to the constructors.
           (te-with (elab-typbinds c1 withtype))
           (c2 (context-plus-te c1 te-with))
           (ve nil)
           (te nil))
      ;; [Rules 28 (part 2), 29]
      (loop for (name params tyname ctors) in entries
            do (let* ((inner (make-context (context-env c2)
                                           (mapcar (lambda (param) (cons (tvar-name param) param))
                                                   params)))
                      (result (apply #'con-type tyname params))
                      (con-ve (loop for ctor in ctors
                                    for arg-ty = (getf (cddr ctor) :arg-type)
                                    collect (cons (second ctor)
                                                  (make-valstr
                                                   (make-scheme params
                                                                (if arg-ty
                                                                    (arrow-type (elab-ty inner arg-ty) result)
                                                                    result))
                                                   :c)))))
                 (setf ve (append ve con-ve))
                 (push (cons name (make-tystr (make-tyfcn params result) con-ve :datatype)) te)))
      ;; Maximise equality [Section 4.9].
      (loop with changed = t
            while changed
            do (setf changed nil)
               (loop for (nil nil tyname) in entries
                     for (nil . tystr) in (reverse te)
                     when (and (tyname-equality tyname)
                               (notevery (lambda (entry)
                                           (let ((arg (constructor-argument (cdr entry))))
                                             (or (null arg) (datatype-admits-equality-p arg))))
                                         (tystr-ve tystr)))
                       do (setf (tyname-equality tyname) nil
                                changed t)))
      (values (make-env ve (append (nreverse te) te-with))
              (mapcar #'third entries)))))

(defun abstract-env (env)
  "[Rule 19] Abs(TE, E): the datatypes of ENV lose their constructors and
equality."
  (make-env nil
            (loop for (name . tystr) in (env-te env)
                  collect (cons name
                                (if (eq (tystr-kind tystr) :datatype)
                                    (let ((tyname (tyfcn-tyname (tystr-tyfcn tystr))))
                                      (when tyname (setf (tyname-equality tyname) nil))
                                      (make-tystr (tystr-tyfcn tystr) nil :abstype))
                                    tystr)))))

;;;; Exception declarations [rules 20, 30, 31]

(defun elab-exbinds (c nodes)
  "Type variables not in scope may occur in an exception declaration (as in
HaMLet); they must not reach the top level (rule 87)."
  (check-distinct-labels (first nodes) (mapcar #'second nodes))
  (let ((free (remove-if (lambda (name) (lookup-tyvar c name))
                         (loop for node in nodes
                               for ty = (and (eq (car node) :exception)
                                             (getf (cddr node) :arg-type))
                               when ty append (ty-tyvars ty)))))
    (setf c (context-plus-u c (mapcar (lambda (name) (cons name (make-rigid-tvar name)))
                                      (remove-duplicates free :test #'string=)))))
  (loop for node in nodes
        collect (cons (second node)
                      (ecase (car node)
                        (:exception     ; [Rule 30]
                         (let ((ty (getf (cddr node) :arg-type)))
                           (make-valstr (mono-scheme (if ty
                                                         (arrow-type (elab-ty c ty) (exn-type))
                                                         (exn-type)))
                                        :e (gensym "EXN"))))
                        (:exception-alias ; [Rule 31]
                         (let ((valstr (lookup-long-vid c (third node) node)))
                           (cond ((null valstr)
                                  (make-valstr (mono-scheme (exn-type)) :e (gensym "EXN")))
                                 ((eq (valstr-status valstr) :e) valstr)
                                 (t (elab-error node "~A is not an exception" (third node))))))))))

;;;; Declarations [rules 15-24]

(defun and-continuation-p (dec)
  (let ((tail (member :and (cdr dec))))
    (and tail (second tail))))

(defun elab-dec (c group)
  "Elaborate GROUP, a declaration (with the bindings `and' joins to it)."
  (let* ((node (first group))
         (*position-fallback* (if (node-offset node) node *position-fallback*))
         (env
           (ecase (car node)
             ((:val :vals :val-rec :val-recs :fun :funs :expr) (elab-val-dec c node))
             (:type (make-env nil (elab-typbinds c (mapcar (lambda (dec)
                                                             (list (getf (cdddr dec) :tyvars)
                                                                   (second dec) (third dec)))
                                                           group))))
             (:datatype (values (elab-datatype c node)))
             (:datatype-replication     ; [Rule 18]
              (let ((name (third node)))
                (let ((tystr (or (lookup-long-tycon c name node)
                                 (elab-error node "cannot replicate unchecked type ~A" name))))
                  (make-env (tystr-ve tystr)
                            (list (cons (second node)
                                        (make-tystr (tystr-tyfcn tystr) (tystr-ve tystr)
                                                    (tystr-kind tystr))))))))
             ((:exception :exception-alias) (make-env (elab-exbinds c group) nil))
             (:local
              (if (getf (cdddr node) :abstype)
                  ;; [Rule 19] abstype datbind with dec end
                  (let* ((e1 (elab-decs c (second node)))
                         (e2 (elab-decs (context-plus-env c e1) (third node))))
                    (env-plus (abstract-env e1) e2))
                  ;; [Rule 21]
                  (let ((e1 (elab-decs c (second node))))
                    (elab-decs (context-plus-env c e1) (third node)))))
             (:open                     ; [Rule 22]
              (let ((env (make-env)))
                (dolist (name (second node) env)
                  (let ((opened (lookup-long-strid c name node)))
                    (when opened
                      (setf env (env-plus env (make-env (env-ve opened) (env-te opened)
                                                        (env-se opened)))))))))
             (:infix (make-env))        ; fixity is the parser's business
             ((:structure :structure-alias :structure-app) (elab-strbinds c group))
             (:signature (elab-sigbinds c group))
             (:functor (elab-funbinds c group)))))
    (annotate node :env env)
    env))

(defun dec-groups (decs)
  "DECS split into groups of bindings joined by `and'."
  (let ((groups nil))
    (dolist (dec decs (nreverse groups))
      (if (and groups (and-continuation-p dec))
          (setf (first groups) (append (first groups) (list dec)))
          (push (list dec) groups)))))

(defun elab-decs (c decs)
  "[Rules 23, 24] A declaration sequence; returns its environment."
  (let ((env (make-env)))
    (dolist (group (dec-groups decs) env)
      (setf env (env-plus env (elab-dec (context-plus-env c env) group))))))

;;;; Top-level declarations

(defun resolve-overloading ()
  "Default the overloaded type variables still unresolved [Appendix E]."
  (loop for (nil . var) in (reverse *overloaded*)
        do (let ((var (repr var)))
             (when (and (tvar-p var) (tvar-overload var))
               (setf (tvar-link var) (con-type (oclass-default (tvar-overload var))))))))

(defun check-top-level-env (node env)
  "No undetermined or explicit type variable may remain free on top level
(HaMLet: rule 87 and the remark after it)."
  (loop for (name . valstr) in (env-ve env)
        do (dolist (var (scheme-free-tvars (valstr-scheme valstr)))
             (cond ((tvar-unknown var))
                   (*saw-unknown* (setf (tvar-unknown var) t))
                   ((tvar-rigid var)
                    (elab-error node "free type variable ~A in the type of ~A on top level"
                                (tvar-name var) name))
                   (t (elab-error node "the type of ~A, ~A, is not determined on top level ~
                                        (value restriction)"
                                  name (scheme->string (valstr-scheme valstr))))))))

(defun elab-top-level-decs (c decs)
  "Elaborate the top-level declarations DECS in context C, resolving
overloading at the end of each.  Returns the environment they bind.  As in
HaMLet, the types of the bindings must be determined by the end of the
program (a `;'-terminated top-level declaration), not of each declaration."
  (let ((env (make-env))
        (saw-unknown nil))
    (dolist (group (dec-groups decs))
      (let* ((*overloaded* nil)
             (*saw-unknown* nil)
             (*level* 0)
             (increment (elab-dec (context-plus-env c env) group)))
        (resolve-overloading)
        (when *saw-unknown* (setf saw-unknown t))
        (setf env (env-plus env increment))))
    (let ((*saw-unknown* saw-unknown))
      (check-top-level-env (first (last decs)) (make-env (env-latest (env-ve env)))))
    env))
