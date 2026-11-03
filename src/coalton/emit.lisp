(in-package #:cl-sml)

;;; The Coalton backend (docs/IR.md, "The Coalton backend"): Coalton source
;;; forms from a program's IR declarations (SML-IR-PROGRAM).  Each top-level
;;; declaration becomes one COALTON-TOPLEVEL form, so that the program's
;;; effects happen in order; Coalton type-checks what is emitted again.
;;;
;;; Representations:
;;;   int -> IFix, with the CL backend's Overflow checks (|int+| ...)
;;;   word -> UFix (62 bits, as the CL backend's), word8 -> U8, real -> F64
;;;   string, char, bool, unit, list, option, order -> String, Char,
;;;     Boolean, Unit, List, Optional, Ord
;;;   pairs .. 5-tuples -> Tuple .. Tuple5; wider tuples and records -> a
;;;     generated type per shape (field types as parameters)
;;;   datatypes -> define-type (Eq derived when SML's equality admits it)
;;;   'a ref -> Ref (a Cell compared by identity)
;;;   exn -> Exn, the run time's exception values; raise and handle are the
;;;     Lisp condition SML-RAISED-EXCEPTION, a native Coalton exception
;;;   functions with a tuple parameter at top level -> an N-argument worker
;;;     (A * B -> C) and a wrapper taking the tuple; curried ones curried.

(define-condition coalton-unsupported (error)
  ((message :initarg :message :reader coalton-unsupported-message))
  (:report (lambda (condition stream)
             (format stream "Coalton backend: ~A" (coalton-unsupported-message condition)))))

(defun coalton-unsupported (format-control &rest args)
  (error 'coalton-unsupported :message (apply #'format nil format-control args)))

(defun co (name)
  "The Coalton symbol NAME, as the prelude's package sees it (Coalton's
syntax, its prelude, or the backend's prelude)."
  (let ((name (if (symbolp name) (symbol-name name) name)))
    (or (find-symbol name "CL-SML/COALTON-PRELUDE")
        (find-symbol (string-upcase name) "CL-SML/COALTON-PRELUDE")
        (error "No Coalton symbol ~A" name))))

(defun co-op (name)
  "The prelude's operator NAME, e.g. |int+|."
  (or (find-symbol name "CL-SML/COALTON-PRELUDE")
      (coalton-unsupported "no operator ~A" name)))

;;; The Coalton Basis: what emitting the Basis bound (src/coalton/basis.lisp).

(defstruct coalton-registry
  package
  (values (make-hash-table :test #'equal))      ; path -> symbol (latest binding)
  (workers (make-hash-table :test #'equal))     ; path -> worker symbol
  (types (make-hash-table :test #'equal))       ; path -> type entry
  (exceptions (make-hash-table :test #'equal))  ; path -> tag symbol
  (blocked nil))                                ; ((path . reason) ...)

(defvar *coalton-basis* nil
  "The COALTON-REGISTRY of the Basis emitted to Coalton, once built.")

;;; A type entry: a datatype or abstract type emitted to Coalton.
(defstruct coalton-type
  symbol
  params              ; IR type variables
  constructors        ; ((name symbol nfields) ...)
  alias)              ; for a type only abbreviated: its body (an IR type)

;;; Emission state.

(defvar *ce-package*)
(defvar *ce-basis-mode* nil
  "True while the Basis itself is emitted.")
(defvar *ce-globals*)       ; cell -> symbol
(defvar *ce-workers*)       ; cell -> worker symbol
(defvar *ce-types*)         ; stamp -> coalton-type
(defvar *ce-exn-tags*)      ; cell -> tag symbol
(defvar *ce-names*)         ; names used in the package
(defvar *ce-vars*)          ; IR variable -> symbol
(defvar *ce-shapes*)        ; record shapes used: lists of labels
(defvar *ce-hoisted*)       ; forms to emit before the current declaration
(defvar *ce-path-count*)    ; path -> bindings so far
(defvar *ce-type-paths*)    ; path of a type or datatype -> coalton-type
(defvar *ce-paths*)         ; path -> symbol of its latest binding
(defvar *ce-local-globals*) ; variable of a top-level `local' -> symbol
(defvar *ce-binding-tyvars* nil
  "The type variables of the type of the top-level binding being emitted:
others are not generalized, and SML defaults them (to unit).")

(defun make-ce-state ()
  (list (cons '*ce-globals* (make-hash-table :test #'eq))
        (cons '*ce-workers* (make-hash-table :test #'eq))
        (cons '*ce-types* (make-hash-table :test #'eql))
        (cons '*ce-exn-tags* (make-hash-table :test #'eq))
        (cons '*ce-names* (make-hash-table :test #'equal))
        (cons '*ce-path-count* (make-hash-table :test #'equal))
        (cons '*ce-type-paths* (make-hash-table :test #'equal))
        (cons '*ce-paths* (make-hash-table :test #'equal))
        (cons '*ce-local-globals* (make-hash-table :test #'eq))))

(defun ce-intern (name)
  "The symbol NAME of the package being emitted, its own (shadowing any
symbol it inherits): Coalton defines only the current package's symbols."
  (multiple-value-bind (symbol status) (find-symbol name *ce-package*)
    (if (member status '(:internal :external))
        symbol
        (progn (shadow (list name) *ce-package*)
               (find-symbol name *ce-package*)))))

(defun ce-fresh-name (base)
  "A new name in the package: BASE, or BASE%2, BASE%3, ..."
  (loop for i from 1
        for name = (if (= i 1) base (format nil "~A%~D" base i))
        unless (gethash name *ce-names*)
          do (setf (gethash name *ce-names*) t)
             (return (ce-intern name))))

(defun cell-path (cell)
  "The path of the binding CELL holds, e.g. \"List.map\" for |List.map;1|."
  (let* ((name (symbol-name cell))
         (semicolon (position #\; name :from-end t)))
    (if semicolon (subseq name 0 semicolon) name)))

(defun ce-global-name (cell &optional (path (cell-path cell)))
  (or (gethash cell *ce-globals*)
      (setf (gethash path *ce-paths*)
            (setf (gethash cell *ce-globals*) (ce-fresh-name path)))))

(defun ce-var (variable)
  "The local symbol of IR VARIABLE."
  (or (gethash variable *ce-vars*)
      (gethash variable *ce-local-globals*)
      (setf (gethash variable *ce-vars*)
            (let ((base (string-downcase (string-right-trim "0123456789" (symbol-name variable)))))
              (make-symbol-in-ce-package
               (format nil "~A/~D" (if (string= base "") "v" base)
                       (hash-table-count *ce-vars*)))))))

(defun make-symbol-in-ce-package (name)
  (ce-intern name))

(defun ce-temp (base)
  (make-symbol-in-ce-package (format nil "~A/~D" base (incf (gethash :temp *ce-names* 0)))))

;;; Types

(defparameter *ce-builtin-types*
  '(("int" . "IFIX") ("word" . "UFIX") ("word8" . "U8") ("real" . "F64")
    ("char" . "CHAR") ("string" . "STRING") ("bool" . "BOOLEAN") ("exn" . "EXN")
    ("list" . "LIST") ("option" . "OPTIONAL") ("order" . "ORDERING") ("ref" . "REF")
    ("vector" . "SmlVector") ("cl_object" . "Raw") ("large_int" . "INTEGER")))

(defun ce-tuple-labels-p (fields)
  (and (/= (length fields) 1)
       (loop for (label) in fields
             for i from 1
             always (string= label (princ-to-string i)))))

(defun ce-tuple-type-symbol (n)
  "Coalton's tuple type (and constructor) of N fields, 2 <= N <= 5."
  (case n
    (2 (co "TUPLE")) (3 (co "TUPLE3")) (4 (co "TUPLE4")) (5 (co "TUPLE5"))))

(defun ce-shape-symbol (labels)
  "The type and constructor of the records with LABELS (tuples wider than 5
included), defined in the prelude's package (ENSURE-COALTON-SHAPE-TYPES)."
  (pushnew labels *ce-shapes* :test #'equal)
  (coalton-shape-symbol labels))

(defun coalton-shape-symbol (labels)
  (intern (format nil "Record{~{~A~^,~}}" labels) "CL-SML/COALTON-PRELUDE"))

(defun ce-record-constructor (labels)
  "The constructor of records (or tuples) with LABELS."
  (if (and (ce-tuple-labels-p (mapcar #'list labels)) (<= 2 (length labels) 5))
      (ce-tuple-type-symbol (length labels))
      (ce-shape-symbol labels)))

(defun ce-tyvar (type tyvars)
  "The Coalton type variable of IR type variable TYPE; TYVARS is a cons
whose car is an alist (id . keyword), extended as needed."
  (let ((id (second type)))
    (or (cdr (assoc id (car tyvars)))
        (let ((keyword (intern (if (< (length (car tyvars)) 26)
                                   (string (code-char (+ (char-code #\A) (length (car tyvars)))))
                                   (format nil "T~D" (length (car tyvars))))
                               :keyword)))
          (push (cons id keyword) (car tyvars))
          keyword))))

(defvar *coalton-checker-basis-type-paths* nil
  "The native checker's Basis: the stamps of its type names -> the paths
of the types that are them (\"Substring.substring\", ...), the first one
for each.")

(defun coalton-checker-basis-type-paths ()
  (or *coalton-checker-basis-type-paths*
      (let ((table (make-hash-table)))
        (labels ((walk (env prefix)
                   (loop for (name . tystr) in (cl-sml.types::env-latest (cl-sml.types::env-te env))
                         for tyname = (ignore-errors
                                       (cl-sml.types::tyfcn-tyname (cl-sml.types::tystr-tyfcn tystr)))
                         when tyname
                           do (let ((stamp (cl-sml.types::tyname-stamp tyname)))
                                (unless (gethash stamp table)
                                  (setf (gethash stamp table) (concatenate 'string prefix name)))))
                   (loop for (strid . sub) in (cl-sml.types::env-latest (cl-sml.types::env-se env))
                         when (cl-sml.types::env-p sub)
                           do (walk sub (concatenate 'string prefix strid ".")))))
          (walk (cl-sml.types::initial-env) ""))
        (setf *coalton-checker-basis-type-paths* table))))

(defun ce-lookup-type (tycon arity)
  "The COALTON-TYPE of TYCON (an IR-TYCON) declared by the program or the
Basis, or NIL.  An abstract type (opaque ascription) is the structure's
type of that name: in the Basis, linked when the structure is declared;
in a program, through the path the native checker's Basis binds it at."
  (declare (ignore arity))
  (or (gethash (ir-tycon-stamp tycon) *ce-types*)
      (and *coalton-basis* (not *ce-basis-mode*)
           (let ((path (gethash (ir-tycon-stamp tycon) (coalton-checker-basis-type-paths))))
             (and path (gethash path (coalton-registry-types *coalton-basis*)))))))

(defun ce-structure-declaration (declaration)
  "Link the abstract types the structure exports to its own types."
  (destructuring-bind (path &key types &allow-other-keys) (rest declaration)
    (dolist (type types)
      (destructuring-bind (name arity kind params body constructors) type
        (declare (ignore arity kind params constructors))
        (let ((entry (gethash (format nil "~A.~A" path name) *ce-type-paths*)))
          (when (and entry (eq (first body) :con)
                     (not (gethash (ir-tycon-stamp (second body)) *ce-types*))
                     ;; Not a type the structure only abbreviates.
                     (not (and (coalton-type-alias entry)
                               (eq (first (coalton-type-alias entry)) :con)
                               (eql (ir-tycon-stamp (second (coalton-type-alias entry)))
                                    (ir-tycon-stamp (second body))))))
            (setf (gethash (ir-tycon-stamp (second body)) *ce-types*) entry))))))
  nil)

(defun ce-type-declaration (declaration)
  "Record the abbreviations DECLARATION declares, for the abstract types
that hide them."
  (dolist (binding (second declaration))
    (destructuring-bind (path params body) binding
      (setf (gethash path *ce-type-paths*)
            (make-coalton-type :params params :alias body))))
  nil)

(defun ce-type (type &optional (tyvars (list nil)))
  "The Coalton type of IR TYPE."
  (ecase (first type)
    (:tyvar (ce-tyvar type tyvars))
    (:arrow (list (ce-type (second type) tyvars) (co "->") (ce-type (third type) tyvars)))
    (:record
     (let ((fields (second type)))
       (when (third type)
         (coalton-unsupported "a record type with an unresolved row"))
       (cond ((null fields) (co "UNIT"))
             (t (cons (ce-record-constructor (mapcar #'car fields))
                      (mapcar (lambda (field) (ce-type (cdr field) tyvars)) fields))))))
    (:con
     (let* ((tycon (second type))
            (args (cddr type))
            (entry (ce-lookup-type tycon (length args)))
            (builtin (cdr (assoc (ir-tycon-name tycon) *ce-builtin-types* :test #'string=))))
       (cond
         ((and entry (coalton-type-alias entry)
               (not (and (eq (first (coalton-type-alias entry)) :con)
                         (eql (ir-tycon-stamp (second (coalton-type-alias entry)))
                              (ir-tycon-stamp tycon)))))
          (ce-type (ce-substitute-type (coalton-type-alias entry)
                                       (mapcar #'second (coalton-type-params entry))
                                       args)
                   tyvars))
         ((and entry (coalton-type-symbol entry))
          (let ((symbol (coalton-type-symbol entry)))
            (if args (cons symbol (mapcar (lambda (a) (ce-type a tyvars)) args)) symbol)))
         (builtin
          (let ((symbol (co builtin)))
            (if args (cons symbol (mapcar (lambda (a) (ce-type a tyvars)) args)) symbol)))
         (t (coalton-unsupported "no Coalton type for ~A" (ir-type-string type))))))))

(defun ce-substitute-type (type ids args)
  (cond ((and (eq (first type) :tyvar) (member (second type) ids))
         (nth (position (second type) ids) args))
        ((eq (first type) :tyvar) type)
        ((eq (first type) :con) (list* :con (second type)
                                       (mapcar (lambda (a) (ce-substitute-type a ids args))
                                               (cddr type))))
        ((eq (first type) :arrow) (list :arrow (ce-substitute-type (second type) ids args)
                                        (ce-substitute-type (third type) ids args)))
        (t (list :record (mapcar (lambda (f) (cons (car f) (ce-substitute-type (cdr f) ids args)))
                                 (second type))
                 (third type)))))

(defun ce-type-tyvars (type)
  "The type variables of IR TYPE: ((id . equality) ...)."
  (let ((result nil))
    (labels ((walk (type)
               (case (first type)
                 (:tyvar (pushnew (cons (second type) (fourth type)) result :key #'car))
                 (:con (mapc #'walk (cddr type)))
                 (:arrow (walk (second type)) (walk (third type)))
                 (:record (mapc (lambda (f) (walk (cdr f))) (second type))))))
      (walk type))
    (nreverse result)))

(defun ce-scheme (type &optional worker-arity)
  "The Coalton type of a declaration of IR TYPE, with an Eq constraint for
each equality type variable; with WORKER-ARITY, the type of the N-argument
worker of a function whose parameter is a tuple."
  (let* ((tyvars (list nil))
         (body (if worker-arity
                   (let ((fields (second (second type))))
                     (append (loop for (field . rest) on fields
                                   collect (ce-type (cdr field) tyvars)
                                   when rest collect (co "*"))
                             (list (co "->") (ce-type (third type) tyvars))))
                   (ce-type type tyvars)))
         (constraints (loop for (id . equality) in (ce-type-tyvars type)
                            when equality
                              collect (list (co "EQ") (cdr (assoc id (car tyvars)))))))
    (if constraints
        `(,@(if (rest constraints) (list constraints) (first constraints))
          ,(co "=>")
          ,@(if (and (consp body) (or worker-arity (eq (second body) (co "->"))))
                body
                (list body)))
        body)))

;;; Constructors

(defun ce-type-name-of (type)
  (and (eq (first type) :con) (ir-tycon-name (second type))))

(defun ce-constructor-short-name (name)
  (let* ((name (if (symbolp name) (symbol-name name) name))
         (semicolon (position #\; name)))
    (when semicolon (setf name (subseq name 0 semicolon)))
    (let ((dot (position #\. name :from-end t)))
      (if (and dot (< (1+ dot) (length name))) (subseq name (1+ dot)) name))))

(defun ce-constructor (type name)
  "(symbol nfields) of constructor NAME of the datatype TYPE (the type of
the constructed value)."
  (let* ((name (ce-constructor-short-name name))
         (tyname (ce-type-name-of type))
         (builtin (and tyname
                       (cdr (assoc (list tyname name)
                                   '((("option" "SOME") "SOME" 1) (("option" "NONE") "NONE" 0)
                                     (("list" "::") "CONS" 2) (("list" "nil") "NIL" 0)
                                     (("bool" "true") "TRUE" 0) (("bool" "false") "FALSE" 0)
                                     (("order" "LESS") "LT" 0) (("order" "EQUAL") "EQ" 0)
                                     (("order" "GREATER") "GT" 0)
                                     (("ref" "ref") "new-ref" 1))
                                   :test #'equal)))))
    (cond
      (builtin (list (co (first builtin)) (second builtin)))
      ((null tyname) (coalton-unsupported "constructor ~A of unknown type" name))
      (t (let* ((entry (ce-lookup-type (second type) (length (cddr type))))
                (ctor (and entry (assoc name (coalton-type-constructors entry) :test #'string=))))
           (unless ctor
             (coalton-unsupported "no Coalton constructor ~A of ~A" name (ir-type-string type)))
           (rest ctor))))))

(defun ce-constructor-fields (arg-type)
  "The number of fields of a constructor whose argument has ARG-TYPE: a
tuple's fields are the constructor's."
  (if (and (eq (first arg-type) :record) (ce-tuple-labels-p (second arg-type))
           (second arg-type))
      (length (second arg-type))
      1))

;;; Exceptions

(defun ce-exception-tag (cell &optional name)
  "The form of the tag of the exception CELL holds."
  (or (gethash cell *ce-exn-tags*)
      (and *coalton-basis*
           (let ((path (cell-path cell)))
             (or (gethash path (coalton-registry-exceptions *coalton-basis*))
                 (gethash (format nil "General.~A" path)
                          (coalton-registry-exceptions *coalton-basis*)))))
      (and (not (eq (symbol-package cell) *ce-package*))
           (sml-initial-exception-name-p (or name (cell-path cell)))
           (list (co "initial-exn-tag") (or name (cell-path cell))))
      (coalton-unsupported "unknown exception ~A" cell)))

(defun sml-initial-exception-name-p (name)
  (member name '("Bind" "Chr" "Div" "Domain" "Match" "Overflow" "Size" "Subscript"
                 "Empty" "Fail" "Span" "Option" "Io")
          :test #'string=))

;;; Expressions

(defun ce-unit-type-p (type)
  (and (eq (first type) :record) (null (second type))))

(defun ce-readable-char-p (char)
  "Can CHAR be written #\\c, as Coalton reads characters?"
  (and (graphic-char-p char) (< (char-code char) 127)))

(defun ce-enumeration-constructors (type)
  "The constructors of TYPE's datatype when none takes an argument."
  (and (eq (first type) :con)
       (let ((constructors (ir-tycon-constructors (second type))))
         (and constructors
              (notany #'cdr constructors)
              (mapcar #'car constructors)))))

(defun ce-default-tyvars (type)
  "TYPE with its type variables that the binding being emitted does not
generalize replaced by unit, or NIL when there are none."
  (let ((free (remove-if (lambda (v) (member (car v) *ce-binding-tyvars*))
                         (ce-type-tyvars type))))
    (and free
         (labels ((walk (type)
                    (case (first type)
                      (:tyvar (if (assoc (second type) free) '(:record nil nil) type))
                      (:con (list* :con (second type) (mapcar #'walk (cddr type))))
                      (:arrow (list :arrow (walk (second type)) (walk (third type))))
                      (:record (list :record (mapcar (lambda (f) (cons (car f) (walk (cdr f))))
                                                     (second type))
                                     (third type)))
                      (t type))))
           (walk type)))))

(defun ce-defaulted (form type)
  "FORM, of IR TYPE, with the type variables SML defaults made unit."
  (let ((defaulted (and type (or (ce-default-tyvars type) type))))
    ;; A ground type is stated too: Coalton cannot tell the type of `[]'
    ;; that SML's `([] : int list) = []' compares.
    (if (and defaulted (null (ce-type-tyvars defaulted)))
        (handler-case `(,(co "THE") ,(ce-type defaulted) ,form)
          (coalton-unsupported () form))
        form)))

(defun ce-literal (value type)
  (let ((name (ce-type-name-of type)))
    (cond ((and (string= (or name "") "bool")) (co (if value "TRUE" "FALSE")))
          ((and (null value) (string= (or name "") "list")) (co "NIL"))
          ((floatp value) (coerce value 'double-float))
          ((and (characterp value) (not (ce-readable-char-p value)))
           (ce-lisp (co "CHAR") () `(code-char ,(char-code value))))
          ((and (integerp value) (member name '("int" "word" "word8") :test #'equal))
           (list (co "THE") (ce-type type) value))
          (t value))))

(defparameter *ce-builtin-functions*
  ;; The run time's functions the compiler refers to directly (*SML-ENV*),
  ;; and the Basis paths that bind them.
  '((sml-print . "print") (sml-int-to-string . "Int.toString") (sml-map . "map")
    (sml-app . "app") (sml-foldl . "foldl") (sml-foldr . "foldr") (sml-hd . "hd")
    (sml-tl . "tl") (sml-length . "length") (sml-null . "null") (sml-rev . "rev")
    (sml-concat . "concat") (sml-size . "size") (sml-explode . "explode")
    (sml-implode . "implode") (sml-ord . "ord") (sml-chr . "chr") (sml-str . "str")
    (sml-floor . "floor") (sml-ceil . "ceil") (sml-round . "round") (sml-trunc . "trunc")
    (sml-sqrt . "Math.sqrt") (sml-sin . "Math.sin") (sml-cos . "Math.cos")
    (sml-arctan . "Math.atan") (sml-exp . "Math.exp") (sml-ln . "Math.ln")
    (sml-real . "real") (sml-o . "General.o") (sml-before . "General.before")
    (sml-ignore . "General.ignore") (sml-not . "not") (sml-op-^ . "^") (sml-op-@ . "@")))

(defun ce-basis-value (path)
  (let ((registry *coalton-basis*))
    (or (and *ce-basis-mode*
             (or (gethash path *ce-paths*)
                 (gethash (format nil "General.~A" path) *ce-paths*)))
        (and registry
             (or (gethash path (coalton-registry-values registry))
                 (gethash (format nil "General.~A" path) (coalton-registry-values registry))))
        (coalton-unsupported "~A is not in the Coalton Basis~@[ (~A)~]" path
                             (and registry
                                  (cdr (assoc path (coalton-registry-blocked registry)
                                              :test #'string=)))))))

(defun ce-overloaded-function (name type)
  "The function value of overloaded operator NAME at its pair TYPE."
  (let* ((domain (second type))
         (tyname (if (eq (first domain) :record)
                     (ce-type-name-of (cdr (first (second domain))))
                     (ce-type-name-of domain)))
         (a (ce-temp "a")) (b (ce-temp "b")))
    (if (eq (first domain) :record)
        `(,(co "FN") (,a) (,(co "MATCH") ,a ((,(co "TUPLE") ,a ,b) (,(co-op (format nil "~A~A" tyname name)) ,a ,b))))
        (co-op (format nil "~A~A" tyname name)))))

(defun ce-ref (node)
  (let ((form (third node))
        (type (second node)))
    (cond
      ((and (symbolp form) (ce-constructor-reference-p form type))
       (ce-constructor-value form type))
      ((symbolp form)
       (or (gethash form *ce-globals*)
           (and (not (eq (symbol-package form) *ce-package*))
                (ce-basis-value (cell-path form)))
           (coalton-unsupported "unknown global ~A" form)))
      ((and (consp form) (eq (first form) 'function))
       (let* ((fn (second form))
              (name (symbol-name fn)))
         (cond
           ((eq fn 'sml-ref) (co "new-ref"))
           ((eq fn 'sml-deref) (co "deref"))
           ((eq fn 'sml-op-assign)
            (let ((p (ce-temp "p")) (r (ce-temp "r")) (v (ce-temp "v")))
              `(,(co "FN") (,p) (,(co "MATCH") ,p ((,(co "TUPLE") ,r ,v) (,(co "assign") ,r ,v))))))
           ((member fn '(sml-op-+ sml-op-- sml-op-* sml-op-/ sml-op-div sml-op-mod
                         sml-op-< sml-op-> sml-op-<= sml-op->=))
            (ce-overloaded-function (subseq name 7) type))
           ((member fn '(sml-~ sml-abs))
            (ce-overloaded-function (subseq name 4) type))
           ((member fn '(sml-op-= sml-op-<>))
            (let ((p (ce-temp "p")) (a (ce-temp "a")) (b (ce-temp "b")))
              `(,(co "FN") (,p) (,(co "MATCH") ,p ((,(co "TUPLE") ,a ,b)
                                                   (,(co (if (eq fn 'sml-op-=) "==" "/=")) ,a ,b))))))
           ((eq fn 'sml-op-cons)
            (let ((p (ce-temp "p")) (a (ce-temp "a")) (b (ce-temp "b")))
              `(,(co "FN") (,p) (,(co "MATCH") ,p ((,(co "TUPLE") ,a ,b) (,(co "CONS") ,a ,b))))))
           (t (let ((path (cdr (assoc fn *ce-builtin-functions*))))
                (if path
                    (ce-basis-value path)
                    (coalton-unsupported "built-in ~A" fn)))))))
      ((eq form 'pi) (ce-basis-value "Math.pi"))
      (t (coalton-unsupported "reference ~S" form)))))

(defun ce-datatype-result (type)
  (if (eq (first type) :arrow) (third type) type))

(defun ce-constructor-reference-p (cell type)
  "Does CELL, referred to at TYPE, hold a constructor of TYPE's datatype?"
  (let ((result (ce-datatype-result type)))
    (and (eq (first result) :con)
         (not (gethash cell *ce-globals*))
         (assoc (ce-constructor-short-name cell) (ir-tycon-constructors (second result))
                :test #'string=))))

(defun ce-constructor-value (cell type)
  (destructuring-bind (symbol nfields) (ce-constructor (ce-datatype-result type) cell)
    (cond ((<= nfields 1) symbol)
          (t (let ((tuple (ce-temp "args"))
                   (fields (loop repeat nfields collect (ce-temp "f"))))
               `(,(co "FN") (,tuple)
                 (,(co "MATCH") ,tuple
                  ((,(ce-record-constructor (loop for i from 1 to nfields collect (princ-to-string i)))
                    ,@fields)
                   (,symbol ,@fields)))))))))

(defun ce-list (nodes) (mapcar #'ce nodes))

(defun ce-app-head (node)
  "Application NODE: the emitted function and argument."
  (list (ce (third node)) (ce (fourth node))))

(defun ce-primitive-use-p (node)
  "Is NODE `use{b = NAME}' in the Basis (a primitive)?  Returns NAME."
  (and *ce-basis-mode*
       (eq (ir-kind node) :app)
       (eq (ir-kind (third node)) :ref)
       (symbolp (third (third node)))
       (string= (cell-path (third (third node))) "use")
       (eq (ir-kind (fourth node)) :record)
       (equal (third (fourth node)) '("b"))
       (eq (ir-kind (first (fourth (fourth node)))) :const)
       (third (first (fourth (fourth node))))))

(defun ce (node)
  "The Coalton form of IR term NODE."
  (let ((type (second node)))
    (ecase (ir-kind node)
      (:const (ce-literal (third node) type))
      (:unit (co "UNIT"))
      (:var (ce-var (third node)))
      (:ref (ce-ref node))
      (:prim (ce-primitive (third node) (fourth node) type))
      (:tuple (let ((args (ce-list (third node))))
                (cons (ce-record-constructor (loop for i from 1 to (length args)
                                                   collect (princ-to-string i)))
                      args)))
      (:record (cons (ce-record-constructor (third node)) (ce-list (fourth node))))
      (:select (ce-select (third node) (fourth node)))
      (:select-label
       (let* ((arg-type (second (fourth node)))
              (labels (and (eq (first arg-type) :record) (mapcar #'car (second arg-type)))))
         (unless (member (third node) labels :test #'equal)
           (coalton-unsupported "#~A of a record of unknown labels" (third node)))
         (ce-select (1+ (position (third node) labels :test #'equal)) (fourth node))))
      (:con (ce-construct type (fourth node) (fifth node)))
      (:app
       (let ((name (ce-primitive-use-p node)))
         (if name
             (ce-basis-primitive name type)
             (ce-list (cddr node)))))
      (:call (destructuring-bind (worker args cell) (cddr node)
               (declare (ignore worker))
               (cons (or (gethash cell *ce-workers*)
                         (and *coalton-basis* (not (eq (symbol-package cell) *ce-package*))
                              (gethash (cell-path cell) (coalton-registry-workers *coalton-basis*)))
                         (coalton-unsupported "no worker for ~A" cell))
                     (ce-list args))))
      (:fn (destructuring-bind (params body name) (cddr node)
             (declare (ignore name))
             (ce-fn params body type)))
      (:let (destructuring-bind (variable value body) (cddr node)
              `(,(co "LET") ((,(ce-var variable) ,(ce value))) ,(ce body))))
      (:letrec (destructuring-bind (bindings body) (cddr node)
                 `(,(co "LET") ,(mapcar (lambda (b) (list (ce-var (first b)) (ce (second b))))
                                        bindings)
                   ,(ce body))))
      (:seq `(,(co "PROGN") ,@(ce-list (third node))))
      (:if `(,(co "IF") ,(ce (third node)) ,(ce (fourth node)) ,(ce (fifth node))))
      (:while (let ((loop (ce-temp "while")))
                `(,(co "REC") ,loop ()
                  (,(co "IF") ,(ce (third node))
                   (,(co "PROGN") ,(ce (fourth node)) (,loop))
                   ,(co "UNIT")))))
      (:raise `(,(co "raise") ,(ce (third node))))
      (:handle (ce-handle (third node) (fourth node)))
      (:match (destructuring-bind (scrutinees rules failure) (cddr node)
                (ce-match (ce-list scrutinees) (mapcar #'second scrutinees)
                          (mapcar (lambda (rule) (list (first rule) (second rule))) rules)
                          (ce-match-failure failure))))
      (:loop (destructuring-bind (tag variables inits body) (cddr node)
               (let ((name (ce-temp "loop")))
                 (setf (gethash (cons tag variables) *ce-vars*) name)
                 (let ((body (ce body))
                       (vars (mapcar #'ce-var variables))
                       (inits (ce-list inits)))
                   ;; Coalton's rec wants its calls in tail position of its
                   ;; body itself, not in a function inside it (a match's
                   ;; fall-through thunks): then a local function.
                   (if (ce-called-inside-fn-p name body)
                       `(,(co "LET") ((,name (,(co "FN") ,vars ,body)))
                         (,name ,@inits))
                       `(,(co "REC") ,name ,(mapcar #'list vars inits) ,body))))))
      (:tail-call (destructuring-bind ((tag &rest variables) args) (cddr node)
                    (cons (or (gethash (cons tag variables) *ce-vars*)
                              (coalton-unsupported "tail call outside its loop"))
                          (ce-list args))))
      (:decl (destructuring-bind (declaration body) (cddr node)
               (ce-local-declaration declaration body))))))

(defun ce-called-inside-fn-p (name form &optional inside)
  (cond ((atom form) nil)
        ((and inside (eq (first form) name)) t)
        ((eq (first form) (co "FN"))
         (some (lambda (f) (ce-called-inside-fn-p name f t)) (cddr form)))
        (t (some (lambda (f) (ce-called-inside-fn-p name f inside)) form))))

(defun ce-select (index arg)
  (let* ((type (second arg))
         (fields (if (eq (first type) :record) (second type)
                     (coalton-unsupported "#~A of a non-record" index)))
         (x (ce-temp "x")))
    `(,(co "MATCH") ,(ce arg)
      ((,(ce-record-constructor (mapcar #'car fields))
        ,@(loop for i from 1 to (length fields) collect (if (= i index) x (co "_"))))
       ,x))))

(defun ce-construct (type tag args)
  (destructuring-bind (symbol nfields) (ce-constructor type tag)
    (let ((args (ce-list args)))
      (cond ((= nfields 0) symbol)
            ((= nfields (length args)) (cons symbol args))
            ((= (length args) 1)
             (let ((fields (loop repeat nfields collect (ce-temp "f"))))
               `(,(co "MATCH") ,(first args)
                 ((,(ce-record-constructor (loop for i from 1 to nfields collect (princ-to-string i)))
                   ,@fields)
                  (,symbol ,@fields)))))
            (t (coalton-unsupported "constructor ~A applied to ~D values" tag (length args)))))))

(defun ce-fn (params body type)
  (if (= (length params) 1)
      `(,(co "FN") (,(ce-var (first params))) ,(ce body))
      (let ((tuple (ce-temp "args")))
        `(,(co "FN") (,tuple)
          (,(co "MATCH") ,tuple
           ((,(ce-record-constructor (mapcar #'car (second (second type))))
             ,@(mapcar #'ce-var params))
            ,(ce body)))))))

(defun ce-local-declaration (declaration body)
  (ecase (first declaration)
    (:exception
     (destructuring-bind (symbol name argp) (rest declaration)
       (let ((tag (ce-temp (format nil "~A%tag" name)))
             (x (ce-temp "x")))
         (setf (gethash symbol *ce-exn-tags*) tag)
         `(,(co "LET") ((,tag (,(co "new-exn-tag") ,name))
                        (,(ce-var symbol)
                         ,(if argp
                              `(,(co "FN") (,x) (,(co "exn-make") ,tag ,x))
                              `(,(co "THE") ,(co "Exn") (,(co "unsafe-coerce") ,tag)))))
           ,(ce body)))))
    (:exception-alias
     (destructuring-bind (symbol name target lexicalp) (rest declaration)
       (declare (ignore name))
       (setf (gethash symbol *ce-exn-tags*) (ce-exception-tag target))
       `(,(co "LET") ((,(ce-var symbol)
                       ,(if lexicalp
                            (ce-var target)
                            (ce-ref (list :ref nil target)))))
         ,(ce body))))
    (:datatype (coalton-unsupported "a datatype declared in an expression"))
    (:module (coalton-unsupported "a structure declared in an expression"))))

;;; Primitive operations

(defun ce-primitive (op args type)
  (let ((forms (ce-list args)))
    (ecase (first op)
      (:overloaded
       (destructuring-bind (name tyname) (rest op)
         (cons (co-op (format nil "~A~A" tyname name)) forms)))
      (:equality
       (destructuring-bind (test negated) (rest op)
         (declare (ignore test))
         (cons (co (if negated "/=" "=="))
               (cons (ce-defaulted (first forms) (second (first args))) (rest forms)))))
      (:not (cons (co "NOT") forms))
      (:deref (cons (co "deref") forms))
      (:list `(,(co "MAKE-LIST") ,@forms))
      (:binary
       (let ((fn (second op)))
         (case fn
           (sml-assign (cons (co "assign") forms))
           ((sml-^ sml-@) (cons (co "<>") forms))
           (cons (cons (co "CONS") forms))
           ((sml-= sml-<>)
            (list* (co (if (eq fn 'sml-=) "==" "/="))
                   (ce-defaulted (first forms) (second (first args)))
                   (rest forms)))
           (sml-compose
            (let ((x (ce-temp "x")))
              `(,(co "FN") (,x) (,(first forms) (,(second forms) ,x)))))
           (sml-before-2
            (let ((x (ce-temp "x")))
              `(,(co "LET") ((,x ,(first forms))) (,(co "PROGN") ,(second forms) ,x))))
           (t (coalton-unsupported "binary primitive ~A at ~A" fn (ir-type-string type))))))
      ((:word-binary :basis)
       (coalton-unsupported "primitive ~S (simplified IR)" op)))))

;;; Patterns.  A pattern becomes a Coalton pattern plus residual steps
;;; Coalton's match cannot express: (:bind var form), (:test form), and
;;; (:sub form pattern) (match FORM against the IR pattern too).

(defun ce-pattern (pattern)
  "Coalton pattern of IR PATTERN and its residual steps."
  (let ((residuals nil))
    (labels
        ((fresh (base) (ce-temp base))
         (p (pattern)
           (let ((type (second pattern)))
             (ecase (ir-kind pattern)
               (:p-wild (co "_"))
               (:p-var (ce-var (third pattern)))
               (:p-const
                (let ((value (third pattern))
                      (name (ce-type-name-of type)))
                  (cond ((equal name "bool")
                         (let ((v (fresh "b")))
                           (push (list :test (if value v (list (co "NOT") v))) residuals)
                           v))
                        ((and (null value) (equal name "list")) (list (co "NIL")))
                        ((and (characterp value) (not (ce-readable-char-p value)))
                         (let ((v (fresh "c")))
                           (push (list :test (list (co "==") v (ce-literal value type))) residuals)
                           v))
                        (t value))))
               (:p-as
                (let ((v (ce-var (third pattern)))
                      (inner (fourth pattern)))
                  (case (ir-kind inner)
                    (:p-wild v)
                    (:p-var (push (list :bind (ce-var (third inner)) v) residuals) v)
                    ;; Match the whole as V, then V against the inner pattern.
                    (t (push (list :sub v inner) residuals) v))))
               (:p-tuple
                (let ((subs (third pattern)))
                  (cons (ce-record-constructor (loop for i from 1 to (length subs)
                                                     collect (princ-to-string i)))
                        (mapcar #'p subs))))
               ((:p-record :p-record-lookup)
                (let* ((labels (if (eq (first type) :record) (mapcar #'car (second type))
                                   (coalton-unsupported "record pattern of unknown type")))
                       (fields (if (eq (ir-kind pattern) :p-record)
                                   (mapcar (lambda (f) (cons (second f) (third f))) (fourth pattern))
                                   (mapcar (lambda (f) (cons (first f) (second f))) (third pattern)))))
                  (cons (ce-record-constructor labels)
                        (mapcar (lambda (label)
                                  (let ((field (assoc label fields :test #'equal)))
                                    (if field (p (cdr field)) (co "_"))))
                                labels))))
               (:p-cons (list (co "CONS") (p (third pattern)) (p (fourth pattern))))
               (:p-ref
                (let ((r (fresh "r")))
                  (push (list :sub (list (co "deref") r) (third pattern)) residuals)
                  r))
               (:p-exn
                (destructuring-bind (name form payload) (cddr pattern)
                  (let ((e (fresh "exn")))
                    (push (list :test (list (co "exn-is") (ce-exception-tag form name) e)) residuals)
                    (when payload
                      (push (list :sub (list (co "exn-payload") e) payload) residuals))
                    e)))
               (:p-con
                (destructuring-bind (name how data payload) (cddr pattern)
                  (declare (ignore how data))
                  (destructuring-bind (symbol nfields) (ce-constructor type name)
                    (cond
                      ((= nfields 0) (list symbol))
                      ((= nfields 1) (list symbol (p payload)))
                      ((and (eq (ir-kind payload) :p-tuple) (= (length (third payload)) nfields))
                       (cons symbol (mapcar #'p (third payload))))
                      ((eq (ir-kind payload) :p-wild)
                       (cons symbol (loop repeat nfields collect (co "_"))))
                      (t
                       (let ((fields (loop repeat nfields collect (fresh "f"))))
                         (push (list :sub (cons (ce-record-constructor
                                                 (loop for i from 1 to nfields
                                                       collect (princ-to-string i)))
                                                fields)
                                     payload)
                               residuals)
                         (cons symbol fields))))))))))
         (pop-and-sub (var inner)
           (push (list :sub var inner) residuals)))
      (let ((coalton-pattern (p pattern)))
        (values coalton-pattern (reverse residuals))))))

(defun ce-residual-failable-p (residuals)
  (some (lambda (r) (member (first r) '(:test :sub))) residuals))

(defun ce-apply-residuals (residuals body fail)
  "BODY under the residual steps, calling FAIL (a form) when one fails."
  (if (null residuals)
      body
      (destructuring-bind (kind a &optional b) (first residuals)
        (let ((rest (ce-apply-residuals (rest residuals) body fail)))
          (ecase kind
            (:bind `(,(co "LET") ((,a ,b)) ,rest))
            (:test `(,(co "IF") ,a ,rest ,fail))
            (:sub
             (multiple-value-bind (cp more) (ce-pattern b)
               (let ((inner (ce-apply-residuals more rest fail)))
                 (if (and (symbolp cp) (not (ce-residual-failable-p more)))
                     (if (eq cp (co "_"))
                         `(,(co "PROGN") ,inner)
                         `(,(co "LET") ((,cp ,a)) ,inner))
                     `(,(co "MATCH") ,a (,cp ,inner) (,(co "_") ,fail)))))))))))

(defun ce-irrefutable-symbol-p (cp)
  (symbolp cp))

(defun ce-match (scrutinees scrutinee-types rules failure)
  "The Coalton form matching the SCRUTINEES forms against RULES ((patterns
body) ...), FAILURE when none matches."
  (declare (ignore scrutinee-types))
  (let* ((k (length scrutinees))
         (scrutinee (if (= k 1)
                        (first scrutinees)
                        (cons (ce-record-constructor (loop for i from 1 to k collect (princ-to-string i)))
                              scrutinees)))
         (translated
           (mapcar (lambda (rule)
                     (destructuring-bind (patterns body) rule
                       (let ((all-residuals nil)
                             (cps nil))
                         (dolist (pattern patterns)
                           (multiple-value-bind (cp residuals) (ce-pattern pattern)
                             (push cp cps)
                             (setf all-residuals (append all-residuals residuals))))
                         (setf cps (nreverse cps))
                         (list (if (= k 1)
                                   (first cps)
                                   (cons (ce-record-constructor
                                          (loop for i from 1 to k collect (princ-to-string i)))
                                         cps))
                               all-residuals
                               body))))
                   rules)))
    (cond
      ;; One rule binding variables only.
      ((and (= (length translated) 1)
            (ce-irrefutable-symbol-p (first (first translated)))
            (not (ce-residual-failable-p (second (first translated)))))
       (destructuring-bind (cp residuals body) (first translated)
         (let ((inner (ce-apply-residuals residuals (ce body) failure)))
           (if (eq cp (co "_"))
               `(,(co "PROGN") ,scrutinee ,inner)
               `(,(co "LET") ((,cp ,scrutinee)) ,inner)))))
      ((notany (lambda (tr) (ce-residual-failable-p (second tr))) translated)
       `(,(co "MATCH") ,scrutinee
         ,@(mapcar (lambda (tr)
                     (destructuring-bind (cp residuals body) tr
                       (list cp (ce-apply-residuals residuals (ce body) failure))))
                   translated)
         ,@(unless (ce-irrefutable-symbol-p (first (car (last translated))))
             `((,(co "_") ,failure)))))
      (t
       (let ((s (ce-temp "s")))
         `(,(co "LET") ((,s ,scrutinee))
           ,(ce-match-chain s translated failure)))))))

(defun ce-match-chain (s translated failure)
  (if (null translated)
      failure
      (destructuring-bind (cp residuals body) (first translated)
        (let* ((rest (ce-match-chain s (rest translated) failure))
               (k (ce-temp "next"))
               (fail `(,k))
               (inner (ce-apply-residuals residuals (ce body) fail)))
          `(,(co "LET") ((,k (,(co "FN") () ,rest)))
            ,(if (eq cp (co "_"))
                 inner
                 `(,(co "MATCH") ,s (,cp ,inner) (,(co "_") ,fail))))))))

(defun ce-match-failure (failure)
  (destructuring-bind (exception &optional where position) failure
    `(,(co "LISP") (,(co "->") :a) ()
      (sml-raise-match-failure ,exception ,where ,position))))

(defun ce-handle (body rules)
  (let* ((condition (ce-temp "c"))
         (e (ce-temp "e")))
    `(,(co "CATCH") ,(ce body)
      ((,(co "THE") ,(co "SmlException") ,condition)
       (,(co "LET") ((,e (,(co "exn-of") ,condition)))
        ,(ce-match (list e) nil
                   (mapcar (lambda (rule) (list (list (first rule)) (second rule))) rules)
                   `(,(co "raise") ,e)))))))

;;; The Basis's primitives (`use{b = NAME}'): the run time's function, its
;;; arguments and results converted between Coalton's representations and
;;; the CL backend's by type.

(defun ce-expand-alias (type)
  "TYPE, or the type an abstract type of the program or Basis stands for."
  (if (eq (first type) :con)
      (let ((entry (ignore-errors (ce-lookup-type (second type) (length (cddr type))))))
        (if (and entry (coalton-type-p entry) (coalton-type-alias entry)
                 (not (and (eq (first (coalton-type-alias entry)) :con)
                           (eql (ir-tycon-stamp (second (coalton-type-alias entry)))
                                (ir-tycon-stamp (second type))))))
            (ce-expand-alias (ce-substitute-type (coalton-type-alias entry)
                                                 (mapcar #'second (coalton-type-params entry))
                                                 (cddr type)))
            type))
      type))

(defun ce-identity-representation-p (type)
  "Do Coalton and the CL backend represent values of TYPE alike?"
  (setf type (ce-expand-alias type))
  (case (first type)
    (:tyvar t)
    (:con (let ((name (ir-tycon-name (second type))))
            (or (member name '("int" "word" "word8" "real" "char" "string" "bool" "exn"
                               "cl_object" "large_int")
                        :test #'string=)
                (and (member name '("list" "vector") :test #'string=)
                     (every #'ce-identity-representation-p (cddr type))))))
    (t nil)))

(defun ce-lisp (type vars &rest body)
  `(,(co "LISP") (,(co "->") ,type) ,vars ,@body))

(defun ce-to-cl (type form)
  "Coalton FORM of IR TYPE as the CL backend represents it (a Raw)."
  (let ((raw (co "Raw")))
    (cond
      ((ce-identity-representation-p type) (list (co "unsafe-coerce") form))
      ((ce-unit-type-p type)
       (let ((x (ce-temp "u")))
         `(,(co "LET") ((,x ,form)) ,(ce-lisp raw (list x) '*sml-unit*))))
      ((eq (first type) :record)
       (let* ((fields (second type))
              (vars (loop repeat (length fields) collect (ce-temp "x")))
              (raws (loop repeat (length fields) collect (ce-temp "r"))))
         `(,(co "MATCH") ,form
           ((,(ce-record-constructor (mapcar #'car fields)) ,@vars)
            (,(co "LET") ,(mapcar (lambda (r v f) (list r (ce-to-cl (cdr f) v))) raws vars fields)
             ,(ce-lisp raw raws
                       `(vector (sml-record-shape ',(mapcar #'car fields)) ,@raws)))))))
      ((equal (ce-type-name-of type) "option")
       (let ((x (ce-temp "x")) (r (ce-temp "r")))
         `(,(co "MATCH") ,form
           ((,(co "SOME") ,x) (,(co "LET") ((,r ,(ce-to-cl (third type) x)))
                               ,(ce-lisp raw (list r) `(sml-some-value ,r))))
           ((,(co "NONE")) ,(ce-lisp raw () `(sml-symbol "NONE" *sml-basis-package-name*))))))
      ((equal (ce-type-name-of type) "list")
       (let ((x (ce-temp "x")))
         (list (co "unsafe-coerce")
               `(,(co "MAP") (,(co "FN") (,x) ,(ce-to-cl (third type) x)) ,form))))
      ((ce-enumeration-constructors type)
       ;; The CL backend's nullary constructors are symbols; the run time's
       ;; primitives look at their names.
       `(,(co "MATCH") ,form
         ,@(mapcar (lambda (name)
                     (list (list (first (ce-constructor type name)))
                           (ce-lisp raw () `(quote ,(ce-cl-constructor-symbol type name)))))
                   (ce-enumeration-constructors type))))
      (t (coalton-unsupported "a primitive's argument of type ~A" (ir-type-string type))))))

(defun ce-cl-constructor-symbol (type name)
  (if (equal (ce-type-name-of type) "order")
      (sml-symbol name *sml-basis-package-name*)
      (intern name :keyword)))

(defun ce-from-cl (type form)
  "The Coalton value of IR TYPE that the CL backend's FORM (a Raw) is."
  (let ((raw (co "Raw")))
    (cond
      ((ce-identity-representation-p type) (list (co "unsafe-coerce") form))
      ((ce-unit-type-p type) `(,(co "ignore-raw") ,form))
      ((eq (first type) :arrow)
       (let ((f (ce-temp "f")) (x (ce-temp "x")) (r (ce-temp "r")))
         `(,(co "LET") ((,f ,form))
           (,(co "FN") (,x)
            (,(co "LET") ((,r ,(ce-to-cl (second type) x)))
             ,(ce-from-cl (third type) (ce-lisp raw (list f r) `(funcall ,f ,r))))))))
      ((eq (first type) :record)
       (let ((r (ce-temp "r")))
         `(,(co "LET") ((,r ,form))
           (,(ce-record-constructor (mapcar #'car (second type)))
            ,@(loop for field in (second type)
                    for i from 1
                    collect (ce-from-cl (cdr field) (ce-lisp raw (list r) `(svref ,r ,i))))))))
      ((equal (ce-type-name-of type) "option")
       (let ((r (ce-temp "r")))
         `(,(co "LET") ((,r ,form))
           (,(co "IF") ,(ce-lisp (co "BOOLEAN") (list r) `(consp ,r))
            (,(co "SOME") ,(ce-from-cl (third type) (ce-lisp raw (list r) `(cdr ,r))))
            ,(co "NONE")))))
      ((equal (ce-type-name-of type) "list")
       (let ((x (ce-temp "x")))
         `(,(co "MAP") (,(co "FN") (,x) ,(ce-from-cl (third type) x))
           (,(co "unsafe-coerce") ,form))))
      ((ce-enumeration-constructors type)
       (let ((r (ce-temp "r"))
             (names (ce-enumeration-constructors type)))
         `(,(co "LET") ((,r ,form))
           ,(reduce (lambda (name else)
                      `(,(co "IF") ,(ce-lisp (co "BOOLEAN") (list r)
                                             `(string= (sml-unqualified-name ,r) ,name))
                        ,(first (ce-constructor type name))
                        ,else))
                    (butlast names)
                    :from-end t
                    :initial-value (first (ce-constructor type (car (last names))))))))
      (t (coalton-unsupported "a primitive's result of type ~A" (ir-type-string type))))))

(defun ce-basis-primitive (name type)
  (unless (sml-basis-primitive name)
    (coalton-unsupported "no run-time primitive ~A" name))
  (ce-from-cl type (ce-lisp (co "Raw") () `(load-time-value (sml-basis-primitive ,name) t))))

;;; Declarations

(defun ce-toplevel (&rest forms)
  `(coalton:coalton-toplevel ,@forms))

(defun ce-declare (symbol type &optional worker-arity)
  "A Coalton declare of SYMBOL at IR TYPE, or NIL if TYPE has no Coalton
counterpart."
  (handler-case (list (list (co "DECLARE") symbol (ce-scheme type worker-arity)))
    (coalton-unsupported () nil)))

(defun ce-define-function (symbol cell type term)
  "The definitions of a function: its worker and wrapper when it takes a
tuple's fields."
  (let ((params (third term))
        (body (fourth term)))
    (if (= (length params) 1)
        `(,@(ce-declare symbol type)
          (,(co "DEFINE") (,symbol ,(ce-var (first params))) ,(ce body)))
        (let* ((worker (or (gethash cell *ce-workers*)
                           (setf (gethash cell *ce-workers*)
                                 (ce-fresh-name (format nil "~A%w" (symbol-name symbol))))))
               (vars (mapcar #'ce-var params))
               (tuple (ce-temp "args")))
          `(,@(ce-declare worker type (length params))
            (,(co "DEFINE") (,worker ,@vars) ,(ce body))
            ,@(ce-declare symbol type)
            (,(co "DEFINE") (,symbol ,tuple)
             (,(co "MATCH") ,tuple
              ((,(ce-record-constructor (mapcar #'car (second (second type)))) ,@vars)
               (,worker ,@vars)))))))))

(defun ce-val-declaration (declaration)
  (destructuring-bind (bindings terms &key patterns) (rest declaration)
    (let ((forms nil)
          (remaining (copy-list bindings)))
      ;; Name every binding first: a `fun ... and ...' group is recursive.
      (dolist (binding bindings)
        (destructuring-bind (path cell type) binding
          (declare (ignore type))
          (when cell (ce-global-name cell path))))
      ;; And the workers of the functions taking a tuple's fields.
      (dolist (term terms)
        (when (and (eq (ir-kind term) :fn) (> (length (third term)) 1) (fifth term))
          (let ((binding (find-if (lambda (b) (string= (ce-constructor-short-name (first b))
                                                       (fifth term)))
                                  bindings)))
            (when (and binding (second binding))
              (let ((cell (second binding)))
                (unless (gethash cell *ce-workers*)
                  (setf (gethash cell *ce-workers*)
                        (ce-fresh-name (format nil "~A%w" (symbol-name (gethash cell *ce-globals*)))))))))))
      (loop for term in terms
            for index from 0
            for pattern = (find index patterns :key #'second)
            do (cond
                 (pattern
                  (destructuring-bind (ir-pattern index cells) pattern
                    (declare (ignore index))
                    (let ((temp (ce-fresh-name (format nil "%val~D" (hash-table-count *ce-globals*)))))
                      (push `(,(co "DEFINE") ,temp ,(let ((*ce-binding-tyvars* nil))
                                                       (ce-defaulted (ce term) (second term))))
                            forms)
                      (loop for (variable . cell) in cells
                            for binding = (find cell bindings :key #'second)
                            do (setf remaining (remove binding remaining))
                               (let ((symbol (ce-global-name cell)))
                                 (dolist (d (and binding (ce-declare symbol (third binding))))
                                   (push d forms))
                                 (push `(,(co "DEFINE") ,symbol
                                         ,(ce-match (list temp) nil
                                                    (list (list (list ir-pattern)
                                                                (list :var nil variable)))
                                                    (ce-match-failure '("Bind" "val"))))
                                       forms))))))
                 (t
                  (let* ((fn-name (and (eq (ir-kind term) :fn) (fifth term)))
                         (binding (or (and fn-name
                                           (find-if (lambda (b)
                                                      (string= (ce-constructor-short-name (first b))
                                                               fn-name))
                                                    remaining))
                                      (and (= (length terms) (length bindings)) (first remaining))
                                      (and (= (length remaining) 1) (= (length terms) 1)
                                           (first remaining)))))
                    (cond
                      ((null binding)
                       (push `(,(co "DEFINE")
                               ,(ce-fresh-name (format nil "%val~D" (hash-table-count *ce-globals*)))
                               ,(let ((*ce-binding-tyvars* nil))
                                  (ce-defaulted (ce term) (second term))))
                             forms))
                      (t
                       (setf remaining (remove binding remaining))
                       (destructuring-bind (path cell type) binding
                         (declare (ignore path))
                         (let ((symbol (ce-global-name cell))
                               (*ce-binding-tyvars* (mapcar #'car (ce-type-tyvars type))))
                           (if (eq (ir-kind term) :fn)
                               (dolist (form (ce-define-function symbol cell type term))
                                 (push form forms))
                               (progn
                                 (dolist (d (ce-declare symbol type)) (push d forms))
                                 (push `(,(co "DEFINE") ,symbol ,(ce term)) forms)))))))))))
      (when remaining
        (coalton-unsupported "bindings without terms: ~{~A~^, ~}" (mapcar #'first remaining)))
      (list (apply #'ce-toplevel (nreverse forms))))))

(defun ce-local-declaration-bindings (declaration)
  "The top-level definitions of the declarations of `local A in B end' (A):
its variables become globals."
  (destructuring-bind (decs terms) (rest declaration)
    (let ((bindings nil)
          (exceptions nil))
      (loop for (kind pairs argp) in decs
            do (case kind
                 ((:val :vals :val-rec :val-recs :fun :funs)
                  (setf bindings (append bindings pairs)))
                 ((:open :infix :infixr :nonfix))
                 (:exception (push (list (car (first pairs)) (cdr (first pairs)) argp)
                                   exceptions))
                 (t (coalton-unsupported "a ~(~A~) in a top-level local" kind))))
      (dolist (pair bindings)
        (setf (gethash (cdr pair) *ce-local-globals*)
              (ce-fresh-name (format nil "~A%local" (car pair)))))
      (let ((forms nil)
            (remaining (copy-list bindings)))
        (dolist (exception (reverse exceptions))
          (destructuring-bind (name variable argp) exception
            (let ((tag (ce-fresh-name (format nil "~A%local%tag" name)))
                  (symbol (ce-fresh-name (format nil "~A%local" name)))
                  (x (ce-temp "x")))
              (setf (gethash variable *ce-exn-tags*) tag
                    (gethash variable *ce-local-globals*) symbol)
              (push `(,(co "DECLARE") ,tag ,(co "Raw")) forms)
              (push `(,(co "DEFINE") ,tag (,(co "new-exn-tag") ,name)) forms)
              (push (if argp
                        `(,(co "DEFINE") (,symbol ,x) (,(co "exn-make") ,tag ,x))
                        `(,(co "DEFINE") ,symbol (,(co "THE") ,(co "Exn") (,(co "unsafe-coerce") ,tag))))
                    forms))))
        (dolist (term terms)
          (let* ((fn-name (and (eq (ir-kind term) :fn) (fifth term)))
                 (pair (or (and fn-name (find fn-name remaining :key #'car :test #'string=))
                           (and (= (length terms) (length bindings)) (first remaining))
                           (coalton-unsupported "a top-level local's bindings"))))
            (setf remaining (remove pair remaining))
            (let ((symbol (gethash (cdr pair) *ce-local-globals*)))
              (if (eq (ir-kind term) :fn)
                  (dolist (form (ce-define-function symbol (cdr pair) (second term) term))
                    (push form forms))
                  (push `(,(co "DEFINE") ,symbol ,(ce term)) forms)))))
        (list (apply #'ce-toplevel (nreverse forms)))))))

(defun ce-datatype-declaration (declaration)
  (let ((entries nil))
    ;; Register every type first: datatypes in a group refer to each other.
    (dolist (datatype (second declaration))
      (destructuring-bind (path arity params constructors) datatype
        (declare (ignore arity))
        (let* ((first-type (second (first constructors)))
               (result (if (eq (first first-type) :arrow) (third first-type) first-type))
               (tycon (second result))
               (name (ir-tycon-name tycon))
               (builtin (and *ce-basis-mode*
                             (member name '("option" "order" "list" "bool" "ref" "cl_object" "large_int")
                                     :test #'string=))))
          (unless builtin
            (let ((entry (make-coalton-type
                          :symbol (ce-fresh-name path)
                          :params params
                          :constructors
                          (mapcar (lambda (constructor)
                                    (destructuring-bind (con-path type flat) constructor
                                      (declare (ignore flat))
                                      (list (ce-constructor-short-name con-path)
                                            (ce-fresh-name con-path)
                                            (if (eq (first type) :arrow)
                                                (ce-constructor-fields (second type))
                                                0))))
                                  constructors))))
              (setf (gethash (ir-tycon-stamp tycon) *ce-types*) entry
                    (gethash path *ce-type-paths*) entry)
              (push (list entry tycon constructors) entries))))))
    (when entries
      (list
       (apply #'ce-toplevel
              (loop for (entry tycon constructors) in (nreverse entries)
                    for tyvars = (list nil)
                    for params = (mapcar (lambda (p) (ce-tyvar p tyvars)) (coalton-type-params entry))
                    append `(,@(when (ir-tycon-equality tycon) `((,(co "DERIVE") ,(co "EQ"))))
                             (,(co "DEFINE-TYPE") ,(if params
                                                       (cons (coalton-type-symbol entry) params)
                                                       (coalton-type-symbol entry))
                              ,@(loop for (nil symbol nfields) in (coalton-type-constructors entry)
                                      for (nil type) in constructors
                                      collect (if (= nfields 0)
                                                  symbol
                                                  (let ((arg (second type)))
                                                    (cons symbol
                                                          (if (> nfields 1)
                                                              (mapcar (lambda (f) (ce-type (cdr f) tyvars))
                                                                      (second arg))
                                                              (list (ce-type arg tyvars)))))))))))))))

(defun ce-exception-declaration (declaration)
  (destructuring-bind (kind path target type &optional cell) declaration
    (let* ((name (ce-constructor-short-name path))
           (tag (ce-fresh-name (format nil "~A%tag" path)))
           (symbol (if cell (ce-global-name cell path) (ce-fresh-name path)))
           (tag-form
             (if (eq kind :exception)
                 (if (and *ce-basis-mode* (sml-initial-exception-name-p name))
                     (list (co "initial-exn-tag") name)
                     (list (co "new-exn-tag") name))
                 (let ((target-cell (and (stringp target)
                                         (gethash target *ce-path-exceptions*))))
                   (cond (target-cell (gethash target-cell *ce-exn-tags*))
                         ((and *coalton-basis*
                               (gethash target (coalton-registry-exceptions *coalton-basis*))))
                         ((sml-initial-exception-name-p (ce-constructor-short-name target))
                          (list (co "initial-exn-tag") (ce-constructor-short-name target)))
                         (t (coalton-unsupported "unknown exception ~A" target)))))))
      (when cell
        (setf (gethash cell *ce-exn-tags*) tag
              (gethash path *ce-path-exceptions*) cell))
      (list (ce-toplevel
             `(,(co "DECLARE") ,tag ,(co "Raw"))
             `(,(co "DEFINE") ,tag ,tag-form)
             (if (eq (first type) :arrow)
                 (let ((x (ce-temp "x")))
                   `(,(co "DEFINE") (,symbol ,x) (,(co "exn-make") ,tag ,x)))
                 `(,(co "DEFINE") ,symbol (,(co "unsafe-coerce") ,tag)))
             `(,(co "DECLARE") ,symbol ,(ce-scheme type)))))))

(defvar *ce-path-exceptions*)

(defun ce-declaration (declaration)
  "The forms of IR DECLARATION (a list)."
  (let ((*ce-vars* (make-hash-table :test #'equal)))
    (ecase (first declaration)
      (:val (ce-val-declaration declaration))
      (:datatype (ce-datatype-declaration declaration))
      ((:exception :exception-alias) (ce-exception-declaration declaration))
      (:type (ce-type-declaration declaration))
      (:local (ce-local-declaration-bindings declaration))
      (:structure (ce-structure-declaration declaration))
      ((:structure-alias :signature :open :datatype-replication) nil)
      ((:functor :functor-application)
       (coalton-unsupported "functors are not supported by the Coalton backend (~A ~A)"
                            (first declaration) (second declaration))))))

;;; Record shapes: generated types, defined in the prelude's package.

(defvar *coalton-shapes-defined* (make-hash-table :test #'equal))

(defun coalton-shape-definition (labels)
  (let* ((symbol (coalton-shape-symbol labels))
         (params (loop for i from 1 to (length labels) collect (intern (format nil "T~D" i) :keyword))))
    `((,(co "DERIVE") ,(co "EQ"))
      (,(co "DEFINE-TYPE") (,symbol ,@params) (,symbol ,@params)))))

(defun ensure-coalton-shape-types (shapes)
  "Define the record types of SHAPES (lists of labels) not yet defined."
  (let ((new (remove-if (lambda (labels) (gethash labels *coalton-shapes-defined*)) shapes)))
    (when new
      (let ((*package* (find-package "CL-SML/COALTON-PRELUDE")))
        (eval `(coalton:coalton-toplevel ,@(mapcan #'coalton-shape-definition new))))
      (dolist (labels new)
        (setf (gethash labels *coalton-shapes-defined*) t)))))

;;; Programs

(defun coalton-program-package (name)
  (let ((package (find-package name)))
    (when package (delete-package package))
    (make-package name :use '("COALTON" "COALTON-PRELUDE" "CL-SML/COALTON-PRELUDE"))))

(defun ensure-coalton-basis-loaded ()
  (unless *coalton-basis*
    (funcall 'load-coalton-basis))
  *coalton-basis*)

(defun emit-coalton-declarations (declarations package &key (on-error :error))
  "The Coalton forms of IR DECLARATIONS, in PACKAGE (a package object).
Returns a list of (declaration forms) or, with ON-ERROR :COLLECT, of
(declaration nil condition) for a declaration the backend cannot emit."
  (let ((*ce-package* package))
    (loop for declaration in declarations
          collect (let ((*ce-shapes* nil))
                    (handler-case
                        (let ((forms (ce-declaration declaration)))
                          (list declaration
                                (if *ce-shapes*
                                    (cons `(ensure-coalton-shape-types ',*ce-shapes*) forms)
                                    forms)))
                      (coalton-unsupported (condition)
                        (if (eq on-error :collect)
                            (list declaration nil condition)
                            (error condition))))))))

(defmacro with-coalton-emission ((&key basis) &body body)
  `(progv (mapcar #'car (make-ce-state)) (mapcar #'cdr (make-ce-state))
     (let ((*ce-basis-mode* ,basis)
           (*ce-path-exceptions* (make-hash-table :test #'equal)))
       ,@body)))

(defun sml-program-ir-for-coalton (source package-name)
  (let ((*sml-ir-simplify* nil)
        (*sml-tupled-workers* t))
    (sml-ir-program source :package package-name)))

(defvar *coalton-program-counter* 0)

(defun emit-coalton-sml-string (source &key (package-name (format nil "SML.COALTON.P~D" (incf *coalton-program-counter*))))
  "The Coalton forms of the SML program SOURCE (core language and
structures; no functors), in a fresh package PACKAGE-NAME.  Returns the
forms and the package."
  (ensure-coalton-basis-loaded)
  (let* ((declarations (sml-program-ir-for-coalton source (format nil "~A.IR" package-name)))
         (package (coalton-program-package package-name)))
    (with-coalton-emission ()
      (values (loop for (nil forms) in (emit-coalton-declarations declarations package)
                    append forms)
              package))))

(defun emit-coalton-sml-file (pathname &rest args)
  (apply #'emit-coalton-sml-string
         (uiop:read-file-string pathname :external-format *sml-external-format*)
         args))

(defun load-coalton-forms (forms package)
  "Compile and run FORMS (Coalton's warnings muffled)."
  (let ((*package* package))
    (handler-bind ((warning #'muffle-warning))
      (dolist (form forms)
        ;; Expanding COALTON-TOPLEVEL type-checks; its errors are signaled
        ;; here rather than turned into compiler warnings.
        (eval (macroexpand form))))))

(defun load-sml-via-coalton (source &rest args)
  "Compile the SML program SOURCE to Coalton and run it.  Returns the
program's package."
  (multiple-value-bind (forms package) (apply #'emit-coalton-sml-string source args)
    (load-coalton-forms forms package)
    package))

(defun load-sml-file-via-coalton (pathname &rest args)
  (apply #'load-sml-via-coalton
         (uiop:read-file-string pathname :external-format *sml-external-format*)
         args))

(defun show-sml-coalton (source &key (stream *standard-output*))
  "Print the Coalton forms the backend emits for the SML program SOURCE."
  (multiple-value-bind (forms package) (emit-coalton-sml-string source)
    (let ((*package* package)
          (*print-case* :downcase)
          (*print-right-margin* 100))
      (dolist (form forms)
        (pprint form stream)
        (terpri stream)))
    (values)))

(export '(emit-coalton-sml-string emit-coalton-sml-file load-sml-via-coalton
          load-sml-file-via-coalton show-sml-coalton))
