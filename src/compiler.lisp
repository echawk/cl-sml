(in-package #:cl-sml)

(defun target-sml-package-name ()
  (package-name (ensure-sml-package *sml-package*)))

(defvar *sml-module-prefix* nil)

(defvar *sml-binding-symbol-env* nil)

(defvar *sml-local-structure-prefixes* nil)

(defvar *sml-local-structure-members* nil)

(defvar *sml-debug-applications* nil)

(defvar *functor-scope-structures* nil
  "While a functor body is compiled: structure name -> descriptor, the
newest first.")

(defvar *functor-scope-variables* nil
  "While a functor body is compiled: an EQ hash table of the variables the
body binds, which hoisted functions (see COMPILE-HOISTED-CASE-ACTION) take
as parameters.")

(defvar *sml-constructor-symbol-env* nil)

(defvar *sml-hoisted-forms* nil)

(defvar *sml-hoisting-enabled* nil)

(defvar *sml-hoisted-name-prefix* nil)

(defvar *sml-hoisted-form-counter* 0)

(defun compile-with-hoisted-sml-forms (compiler-thunk &key identity)
  (let ((*sml-hoisted-forms* nil)
        (*sml-hoisting-enabled* t)
        (*sml-hoisted-name-prefix*
          (format nil "%CL-SML-CASE-ACTION-~36R-"
                  (sxhash (or identity (gensym "SML-COMPILATION-")))))
        (*sml-hoisted-form-counter* 0))
    (let ((form (funcall compiler-thunk)))
      (if *sml-hoisted-forms*
          `(progn ,@(nreverse *sml-hoisted-forms*) ,form)
          form))))

(defun next-hoisted-sml-function-name ()
  (intern (format nil "~A~D" *sml-hoisted-name-prefix*
                  (incf *sml-hoisted-form-counter*))
          (ensure-sml-package *sml-package*)))

;;; Signatures.  Without the static checker, a structure ascribed a
;;; signature exports only the values the signature specifies, computed from
;;; the specification AST (including `include'd specifications).  Hidden
;;; members (helpers, constructors of abstract types, names brought in by
;;; `open' in the body) would otherwise leak into every scope that later
;;; `open's the structure.

(defvar *sml-signature-expressions* (make-hash-table :test #'equal)
  "(package-name signature-name) -> the signature expression of the
signatures compiled so far.")

(defun note-signature-declaration (dec)
  "Record a signature as soon as its declaration is seen, so that member
lists computed ahead of compilation can already honour it."
  (when (and (consp dec) (eq (car dec) :signature))
    (setf (gethash (list (target-sml-package-name) (second dec))
                   *sml-signature-expressions*)
          (third dec))))

(defun sml-signature-exports (sigexp &optional (depth 0))
  "The value members signature expression SIGEXP specifies, as names such as
\"x\" or \"S.y\" (\"S.\" stands for every member of a substructure S whose
signature is unknown), and whether the list is complete."
  (if (> depth 100)
      (values nil nil)
      (ecase (car sigexp)
        (:sig-id
         (let ((found (or (gethash (list (target-sml-package-name) (second sigexp))
                                   *sml-signature-expressions*)
                          ;; A signature of the basis.
                          (gethash (list *sml-basis-package-name* (second sigexp))
                                   *sml-signature-expressions*))))
           (if found
               (sml-signature-exports found (1+ depth))
               (values nil nil))))
        (:sig-where (sml-signature-exports (second sigexp) depth))
        (:sig
         (let ((names nil)
               (complete t))
           (dolist (spec (second sigexp) (values names complete))
             (case (car spec)
               ((:spec-val :spec-exception) (push (second spec) names))
               (:spec-datatype
                (dolist (datbind (second spec))
                  (dolist (ctor (third datbind))
                    (push (second ctor) names))))
               ;; The replicated constructors are not known here.
               (:spec-datatype-replication (setf complete nil))
               (:spec-structure
                (multiple-value-bind (members members-complete)
                    (sml-signature-exports (third spec) (1+ depth))
                  (if members-complete
                      (dolist (member members)
                        (push (qualify-sml-name (second spec) member) names))
                      (push (format nil "~A." (second spec)) names))))
               (:spec-include
                (multiple-value-bind (members members-complete)
                    (sml-signature-exports (second spec) (1+ depth))
                  (setf names (append members names))
                  (unless members-complete
                    (setf complete nil)))))))))))

(defun restrict-structure-members-to-signature (members sigexp)
  "MEMBERS of a structure ascribed SIGEXP, restricted to those it specifies."
  (multiple-value-bind (exports complete) (and sigexp (sml-signature-exports sigexp))
    (if (not complete)
        members
        (remove-if-not
         (lambda (member)
           (or (member member exports :test #'string=)
               (some (lambda (export)
                       (and (char= (char export (1- (length export))) #\.)
                            (string-prefix-p export member)))
                     exports)))
         members))))

(defun static-structure-members (dec)
  "The value members of structure declaration DEC according to the static
checker (signature matching included), or :UNKNOWN."
  (if (native-sml-static-facts-p)
      (let ((members (funcall 'cl-sml.types::native-structure-members
                              dec *sml-static-facts*)))
        (if (eq members :unknown) :unknown (mapcar #'cdr members)))
      (let ((fact (first (sml-static-facts-at (sml-ast-position dec) "str"
                                              (second dec)))))
        (if fact
            (mapcar #'cdr (parse-sml-structure-members (third fact)))
            :unknown))))

(defun replicated-constructors (dec)
  "The (status . name) constructors that datatype replication DEC binds,
according to the static checker, or else the parser, or NIL when unknown."
  (let ((fact (first (sml-static-facts-at (sml-ast-position dec) "dtrep"
                                          (second dec)))))
    (cond (fact (parse-sml-structure-members (third fact)))
          ((and (native-sml-static-facts-p)
                (funcall 'cl-sml.types::native-replicated-constructors
                         dec *sml-static-facts*)))
          (t (getf (cdddr dec) :constructors)))))

(defun compile-datatype-replication-constructors (dec)
  "Bind the constructors DEC = (:datatype-replication name source) brings
into scope to those of the replicated datatype."
  (let* ((source-type (string-trim '(#\Space #\Tab #\Newline #\;) (third dec)))
         (dot (position #\. source-type :from-end t))
         (source-prefix (and dot (subseq source-type 0 dot))))
    (loop for (nil . name) in (replicated-constructors dec)
          ;; A built-in such as `true' or `ref', compiled to a value.
          for builtin = (and (null source-prefix)
                             (not (compile-time-sml-env-symbol-shadowed-p name))
                             (assoc name *sml-env* :test #'string=))
          for source-name = (if source-prefix
                                (qualify-sml-name source-prefix name)
                                name)
          for target = (target-sml-symbol name)
          for source = (cond (builtin (cdr builtin))
                             ((let ((resolved (resolved-sml-symbol source-name)))
                                (and (not (eq resolved target)) resolved)))
                             ;; The name resolves to the binding being made;
                             ;; the replicated datatype may be the basis's.
                             (t (sml-visible-symbol source-name)))
          unless (and (not builtin) (eq source target))
            collect (if (and (symbolp source) (not builtin))
                        `(alias-sml-constructor ',target ',source)
                        `(progn (defparameter ,target ,source)
                                ,(compile-export-form (list target)))))))

(defun compile-time-sml-env-symbol-shadowed-p (name)
  "Is built-in NAME rebound by a declaration of the program being compiled?"
  (assoc name *sml-binding-symbol-env* :test #'string=))

(defun structure-declaration-members (dec &optional local-structures)
  "The members a (:structure name decs [:sigexp S]) declaration makes visible."
  (let ((members (declarations-bound-names (third dec) local-structures))
        (static-members (static-structure-members dec)))
    (if (eq static-members :unknown)
        (restrict-structure-members-to-signature members (getf (cdddr dec) :sigexp))
        ;; Exactly the members the elaborated (and possibly ascribed)
        ;; structure has.
        (remove-if-not (lambda (name)
                         (member name static-members :test #'string=))
                       members))))

(defun qualify-sml-name (prefix name)
  (if (and prefix (not (string= prefix "")))
      (format nil "~A.~A" prefix name)
      name))

(defun current-qualified-sml-name (name)
  (qualify-sml-name *sml-module-prefix* name))

(defun target-sml-symbol (name)
  ;; Declarations shadow opened names; only reference resolution consults the
  ;; binding environment.
  (sml-symbol (current-qualified-sml-name name)))

(defun sml-lexical-symbol (name)
  "A fresh variable for the let-bound SML identifier NAME.  The package's
symbol for NAME may have been proclaimed special by a top-level binding,
or be a COMMON-LISP symbol such as T, so it must not be bound by LET."
  (make-symbol name))

(defun resolve-structure-prefix (name)
  (or (cdr (assoc name *sml-local-structure-prefixes* :test #'string=))
      (let ((dot (and (stringp name) (position #\. name))))
        (when dot
          (let* ((head (subseq name 0 dot))
                 (resolved-head
                   (cdr (assoc head *sml-local-structure-prefixes*
                               :test #'string=))))
            (when resolved-head
              (qualify-sml-name resolved-head (subseq name (1+ dot)))))))
      name))

(defun resolve-functor-name (name)
  "Resolve NAME in the functor namespace, independently of structures."
  name)

(defun sml-long-name-p (name)
  (and (stringp name)
       (position #\. name)))

(defun resolve-sml-long-name (name)
  (let ((dot (position #\. name)))
    (if dot
        (qualify-sml-name (resolve-structure-prefix (subseq name 0 dot))
                          (subseq name (1+ dot)))
        (current-qualified-sml-name name))))

(defun compile-time-sml-env-symbol (name)
  (or (cdr (assoc name *sml-binding-symbol-env* :test #'string=))
      (let ((mapping (assoc name *sml-env* :test #'string=)))
        (and mapping (cdr mapping)))))

(defun resolved-sml-symbol (name &optional lexical-env)
  ;; A built-in is still found before a binding of its name made by an
  ;; earlier compilation unit (a known gap: HaMLet's Basis rebinds many of
  ;; them in SML).
  (or (lexical-symbol-for-name name lexical-env)
      ;; A member of a structure of the functor body being compiled.
      (functor-scope-long-name-symbol name)
      (compile-time-sml-env-symbol name)
      (let ((resolved-name (if (sml-long-name-p name)
                               (resolve-sml-long-name name)
                               name)))
        (or (compile-time-sml-env-symbol resolved-name)
            (sml-bound-symbol resolved-name)))))

;;; Binding cells (see PUBLISH-SML-BINDING).  A program refers to the
;;; bindings it makes itself through *SML-BINDING-SYMBOL-ENV* and
;;; *SML-UNIT-CELLS*, and to those of earlier compilation units in its
;;; package through the cells their symbols record.  The bindings of a
;;; functor body are lexical (see COMPILE-FUNCTOR-DECLARATION).

(defvar *sml-unit-cells* nil
  "While a program is compiled: qualified SML name -> the cell of the
binding of that name the program made last, or :PLAIN when the program last
bound it to the package's symbol itself (a datatype constructor, a member
of a functor instance bound at run time).")

(defun sml-binding-cell-symbol-p (symbol)
  "Is SYMBOL a binding cell, |name;k|, rather than a package's symbol for an
SML name (which cannot contain `;')?"
  (and (symbolp symbol)
       (find #\; (symbol-name symbol))
       t))

(defun sml-cell-name-symbol (cell)
  "The package's symbol for the name whose binding CELL holds."
  (let ((name (symbol-name cell)))
    (sml-symbol (subseq name 0 (position #\; name :from-end t))
                (symbol-package cell))))

(defun sml-bound-symbol (qualified-name)
  "The symbol holding the binding of QUALIFIED-NAME in scope: the cell of
its newest binding, or the package's symbol for it."
  (let* ((symbol (sml-symbol qualified-name))
         (unit-cell (and *sml-unit-cells*
                         (gethash qualified-name *sml-unit-cells*)))
         ;; A name the package does not bind denotes the basis's binding.
         (basis-symbol (and (not unit-cell)
                            (not (sml-name-bound-p symbol))
                            (sml-basis-symbol (symbol-name symbol)
                                              (symbol-package symbol)))))
    (cond
      ((eq unit-cell :plain) symbol)
      (unit-cell)
      ((sml-binding-cell symbol))
      (basis-symbol (or (sml-binding-cell basis-symbol) basis-symbol))
      (t symbol))))

(defun current-binding-symbol (name)
  "The symbol holding the binding of NAME just declared in the current
structure (or at top level)."
  (sml-bound-symbol (current-qualified-sml-name name)))

(defvar *sml-unit-cell-counts* nil
  "While a program is compiled: qualified SML name -> the highest K of the
cells |name;K| the program made for it.")

(defvar *sml-reserved-cells* nil
  "While programs are loaded (compiled and run) in a nest, e.g. a file that
`use's another: an EQ hash table of the cells made for programs that have
not run yet, which the nested ones must not reuse.")

(defmacro with-sml-cell-reservations (&body body)
  "Run BODY, which compiles and runs a program, with the cells of programs
it compiles reserved until it returns."
  `(let ((*sml-reserved-cells* (or *sml-reserved-cells*
                                   (make-hash-table :test #'eq))))
     ,@body))

(defun new-sml-binding-cell (name)
  "A fresh cell for a binding of NAME declared in the current structure (or
at top level), which the rest of the program refers to.  Cells are numbered
after those published by the programs that ran before (and those of this
program), so that compiling a program twice without running it gives the
same code."
  (let* ((symbol (target-sml-symbol name))
         (qualified (current-qualified-sml-name name))
         (start (max (published-sml-cell-count symbol)
                     (if *sml-unit-cell-counts*
                         (gethash qualified *sml-unit-cell-counts* 0)
                         0)))
         (cell (loop for k from (1+ start)
                     for cell = (intern (format nil "~A;~D" (symbol-name symbol) k)
                                        (symbol-package symbol))
                     unless (and *sml-reserved-cells*
                                 (gethash cell *sml-reserved-cells*))
                       return cell)))
    (when *sml-reserved-cells*
      (setf (gethash cell *sml-reserved-cells*) t))
    (when *sml-unit-cells*
      (setf (gethash qualified *sml-unit-cells*) cell
            (gethash qualified *sml-unit-cell-counts*)
            (sml-cell-index cell symbol)))
    cell))

(defun note-sml-plain-binding (qualified-name)
  "The program binds QUALIFIED-NAME to the package's symbol itself."
  (when *sml-unit-cells*
    (setf (gethash qualified-name *sml-unit-cells*) :plain)))

(defun compile-binding-publication (cell type)
  "Forms that make CELL, of TYPE, the newest binding of its name (or
register CELL's type, when it is the package's symbol itself)."
  (if (sml-binding-cell-symbol-p cell)
      `((publish-sml-binding ',(sml-cell-name-symbol cell) ',cell ',type))
      `(,(compile-type-registration-form cell type)
        ,(compile-export-form (list cell)))))

(defun module-member-symbol-env (module-name members)
  (mapcar (lambda (member)
            (cons member (sml-symbol (qualify-sml-name module-name member))))
          members))

(defun resolve-structure-prefix-from (name prefixes)
  (or (cdr (assoc name prefixes :test #'string=))
      (resolve-structure-prefix name)))

(defun functor-argument-structure-name (argument)
  "The structure that ARGUMENT, a functor's argument structure expression,
names, if it is a (long) structure identifier."
  (let ((core (strip-sml-strexp-constraints argument)))
    (and (eq (car core) :str-id) (second core))))

(defun functor-argument-module-name (target-name)
  "The module that holds the argument structure of the functor application
bound to TARGET-NAME, when the argument is not a structure identifier."
  (format nil "~A.%ARG" target-name))

(defun module-local-structure-prefixes (decs module-name)
  (let ((prefixes nil))
    (dolist (dec decs (nreverse prefixes))
      (case (car dec)
        ((:structure :structure-app)
         (push (cons (second dec)
                     (qualify-sml-name module-name (second dec)))
               prefixes))
        (:structure-alias
         (push (cons (second dec)
                     (resolve-structure-prefix-from (third dec) prefixes))
               prefixes))))))

(defun lookup-local-structure-members (name local-structures)
  (or (cdr (assoc name local-structures :test #'string=))
      (cdr (assoc name *sml-local-structure-members* :test #'string=))
      (let ((dot (and (stringp name) (position #\. name))))
        (when dot
          (let* ((root (subseq name 0 dot))
                 (nested-prefix (format nil "~A." (subseq name (1+ dot))))
                 (root-members
                   (or (cdr (assoc root local-structures :test #'string=))
                       (cdr (assoc root *sml-local-structure-members*
                                   :test #'string=)))))
            (remove-duplicates
             (loop for member in root-members
                   when (string-prefix-p nested-prefix member)
                     collect (subseq member (length nested-prefix)))
             :test #'string=))))))

(defun remove-sml-binding-symbols (names env)
  (remove-if (lambda (entry)
               (member (car entry) names :test #'string=))
             env))

(defvar *sml-local-constructor-arities* nil
  "Alist from the tags of the datatype constructors declared by the `let' and
`local' declarations being compiled to their representation (always 0: the
values of local constructors are (tag . argument)).  Their tags are the
package's symbols of their names, which a top-level constructor may share.")

(defun datatype-constructor-flat-arity (ctor)
  "N > 1 when the values of constructor CTOR, a (:ctor-def name ...) node of
a top-level or structure-level datatype, have the flat representation of an
N-tuple argument (`C of t1 * ... * tN'), else 0.  See SML-CONSTRUCTED-TAG."
  (let ((type (getf (cddr ctor) :arg-type)))
    (if (and (fourth ctor) (consp type) (eq (car type) :ty-tuple)
             (>= (length (rest type)) 2))
        (length (rest type))
        0)))

(defun constructor-arity-at-compile-time (symbol)
  "How the values of the constructor with tag SYMBOL are represented, when
the compiler knows it (SML-CONSTRUCTOR-ARITY), else NIL."
  (when (and symbol (symbolp symbol) (symbol-package symbol))
    (let ((local (assoc symbol *sml-local-constructor-arities*)))
      (if local
          (cdr local)
          (sml-constructor-arity symbol)))))

(defun pattern-tuple-subpatterns (pat arity)
  "The ARITY subpatterns of PAT when it is a tuple pattern of that many."
  (loop while (and (consp pat) (eq (car pat) :pat-typed))
        do (setf pat (second pat)))
  (when (and (consp pat) (eq (car pat) :pat-record))
    (multiple-value-bind (fields tuplep) (tuple-record-fields (cdr pat))
      (when tuplep
        (setf pat `(:pat-tuple ,@(mapcar #'second fields))))))
  (and (consp pat) (eq (car pat) :pat-tuple)
       (= (length (cdr pat)) arity)
       (cdr pat)))

(defun compile-flat-constructor-pattern (tag arity payload local-exceptions lexical-env)
  "A pattern on the flat values of the constructor with tag TAG and ARITY
fields, whose argument must match PAYLOAD, a pattern AST."
  ;; The tag is tested as a subpattern, after the type, so that trivia can
  ;; infer the types of the tests when it reorders clauses.
  (let* ((it (gensym "CTOR-VALUE"))
         (tag-pattern (let ((tag-var (gensym "TAG")))
                        `(guard1 ,tag-var (eq ,tag-var ',tag))))
         (subpatterns (pattern-tuple-subpatterns payload arity)))
    (if subpatterns
        `(guard1 ,it (typep ,it '(simple-vector ,(1+ arity)))
                 (svref ,it 0) ,tag-pattern
                 ,@(loop for subpattern in subpatterns
                         for i from 1
                         for compiled = (compile-pat subpattern local-exceptions lexical-env)
                         unless (eq compiled '_)
                           append `((svref ,it ,i) ,compiled)))
        `(guard1 ,it (typep ,it '(simple-vector ,(1+ arity)))
                 (svref ,it 0) ,tag-pattern
                 (sml-flat-constructed-argument ,it)
                 ,(compile-pat payload local-exceptions lexical-env)))))

(defun compile-constructor-application (symbol arg local-exceptions lexical-env)
  "The value of constructor SYMBOL (a tag) applied to the expression ARG,
or NIL when the representation of its values is not known."
  (let ((arity (constructor-arity-at-compile-time symbol)))
    (cond
      ((null arity) nil)
      ((eql arity 0)
       `(cons ',symbol ,(compile-expr arg local-exceptions lexical-env)))
      ((and (consp arg) (eq (car arg) :tuple) (= (length (cdr arg)) arity))
       `(vector ',symbol ,@(mapcar (lambda (item)
                                     (compile-expr item local-exceptions lexical-env))
                                   (cdr arg))))
      (t
       `(sml-construct-flat ',symbol
                            ,(compile-expr arg local-exceptions lexical-env))))))

(defun known-constructor-symbol-for-name (name)
  (cdr (assoc name *sml-constructor-symbol-env* :test #'string=)))

(defun canonical-sml-constructor-symbol (symbol)
  (or (sml-constructor-canonical-symbol symbol)
      symbol))

(defun constructor-symbol-for-name (name &optional lexical-env)
  (or (known-constructor-symbol-for-name name)
      (canonical-sml-constructor-symbol (resolved-sml-symbol name lexical-env))))

(defun known-data-constructor-application-p (name lexical-env)
  (let ((lexical (lexical-symbol-for-name name lexical-env))
        (known (known-constructor-symbol-for-name name)))
    (or known
        (and (not lexical)
             (sml-constructor-symbol-p (resolved-sml-symbol name lexical-env))))))

(defun pattern-constructor-name-p (name &optional local-exceptions lexical-env)
  (or (member name '("true" "false" "nil") :test #'string=)
      (exception-constructor-info name local-exceptions)
      (known-constructor-symbol-for-name name)
      (lexical-constructor-tag-form name lexical-env)
      (sml-constructor-symbol-p (resolved-sml-symbol name lexical-env))))

(defun pattern-node-status (pat)
  "The identifier status of pattern node PAT: the static checker's if it has
one, else the parser's (the :STATUS of a :pat-ctor node), or NIL."
  (or (sml-node-fact-info pat "pat")
      (and (consp pat) (getf (cddr pat) :status))))

(defun pattern-node-constructor-p (pat &optional local-exceptions lexical-env)
  (let ((status (pattern-node-status pat)))
    (if status
        (sml-status-constructor-p status)
        (pattern-constructor-name-p (second pat) local-exceptions lexical-env))))

(defun pattern-node-exception-p (pat &optional local-exceptions)
  (let ((status (pattern-node-status pat)))
    (if status
        (sml-status-exception-p status)
        (exception-constructor-info (second pat) local-exceptions))))

(defun exception-constructor-type-p (type)
  (or (and (stringp type)
           (string= type "exn"))
      (and (consp type)
           (eq (car type) :fn)
           (equal (third type) "exn"))))

(defun payload-exception-type-p (type)
  (and (consp type)
       (eq (car type) :fn)
       (equal (third type) "exn")))

(defun exception-constructor-info (name &optional local-exceptions)
  (let ((local-entry (assoc name local-exceptions :test #'string=)))
    (cond
      (local-entry
       (list :payloadp (cdr local-entry)))
      ((functor-scope-exception-info name))
      (t
       (let ((type (lookup-sml-binding-type (resolved-sml-symbol name) *sml-package*)))
         (when (exception-constructor-type-p type)
           (list :payloadp (payload-exception-type-p type))))))))

(defun compile-type-registration-form (symbol type)
  `(register-sml-binding-type ',symbol ',type))

(defun compile-type-alias-form (name target)
  "Register type NAME as TARGET, a type AST or a type constructor name."
  `(register-sml-type-alias ,(target-sml-package-name)
                            ,(current-qualified-sml-name name)
                            ,(if (consp target) (sml-type-ast->string target) target)))

(defun compile-type-declaim-form (symbol type)
  (declare (ignore symbol type))
  nil)

(defun compile-export-form (symbols)
  (when symbols
    `(export ',symbols ,(target-sml-package-name))))

(defun ensure-pattern-ast (pat)
  (if (stringp pat)
      `(:pat-var ,pat)
      pat))

(defun lexical-symbol-for-name (name lexical-env)
  (cdr (assoc name lexical-env :test #'string=)))

(defun pattern-variable-map (pat &optional local-exceptions)
  (mapcar (lambda (name)
            (cons name (gensym (string-upcase name))))
          (remove-duplicates (pattern-bound-names pat local-exceptions)
                             :test #'string=)))

(defun pattern-bound-names (pat &optional local-exceptions lexical-env)
  (cond
    ((or (numberp pat) (stringp pat) (characterp pat) (eq pat :wild))
     nil)
    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     (list (second pat)))
    ((and (listp pat) (eq (car pat) :pat-typed))
     (pattern-bound-names (second pat) local-exceptions lexical-env))
    ((and (listp pat) (eq (car pat) :pat-as))
     (append (pattern-bound-names (second pat) local-exceptions lexical-env)
             (pattern-bound-names (third pat) local-exceptions lexical-env)))
    ((and (listp pat) (member (car pat) '(:pat-ctor :ctor)))
     (unless (pattern-node-constructor-p pat local-exceptions lexical-env)
       (list (second pat))))
    ((and (listp pat) (member (car pat) '(:pat-unit :pat-nil)))
     nil)
    ((and (listp pat) (eq (car pat) :pat-app))
     (pattern-bound-names (third pat) local-exceptions lexical-env))
    ((and (listp pat) (eq (car pat) :pat-cons))
     (append (pattern-bound-names (second pat) local-exceptions lexical-env)
             (pattern-bound-names (third pat) local-exceptions lexical-env)))
    ((and (listp pat) (eq (car pat) :pat-tuple))
     (mapcan (lambda (subpat)
               (pattern-bound-names subpat local-exceptions lexical-env))
             (cdr pat)))
    ((and (listp pat) (eq (car pat) :pat-record))
     (mapcan (lambda (field)
               (pattern-bound-names (second field) local-exceptions lexical-env))
             (record-fields-sorted-by-label (cdr pat))))
    (t nil)))

(defun declaration-direct-bound-names (dec)
  (case (car dec)
    (:val (pattern-bound-names (second dec)))
    (:vals (mapcan #'declaration-direct-bound-names (cdr dec)))
    (:val-rec (list (second dec)))
    (:fun (list (second dec)))
    (:funs (mapcan #'declaration-direct-bound-names (cdr dec)))
    (:val-recs (mapcan #'declaration-direct-bound-names (cdr dec)))
    (:datatype (mapcar #'second (third dec)))
    (:datatype-replication (mapcar #'cdr (replicated-constructors dec)))
    (:exception (list (second dec)))
    (:exception-alias (list (second dec)))
    (:local (declarations-bound-names (third dec)))
    (otherwise nil)))

(defun split-sml-module-names (names)
  "The structure names of an `open' declaration: a list, or a string of names
separated by blanks."
  (if (listp names)
      names
      (split-sml-module-names-text names)))

(defun split-sml-module-names-text (names)
  (let ((current (make-string-output-stream))
        (result nil))
    (labels ((emit ()
               (let ((name (get-output-stream-string current)))
                 (unless (string= name "")
                   (push name result))
                 (setf current (make-string-output-stream)))))
      (loop for ch across names
            do (if (member ch '(#\Space #\Tab #\Newline #\Return #\;))
                   (emit)
                   (write-char ch current)))
      (emit)
      (nreverse result))))

(defun lookup-sml-module-members-for-compiler (name local-structures)
  (let ((resolved-name (resolve-structure-prefix name)))
    (or (lookup-local-structure-members name local-structures)
        (lookup-local-structure-members resolved-name local-structures)
        (lookup-sml-structure-members (target-sml-package-name) name)
        (unless (string= resolved-name name)
          (lookup-sml-structure-members (target-sml-package-name) resolved-name)))))

(defun qualify-structure-member-names (structure-name members)
  (mapcar (lambda (member)
            (qualify-sml-name structure-name member))
          members))

(defun declarations-bound-names (decs &optional local-structures)
  (let ((members nil)
        (structures local-structures))
    (dolist (dec decs)
      (note-signature-declaration dec)
      (case (car dec)
        (:open
         (dolist (name (split-sml-module-names (second dec)))
           (setf members
                 (append (lookup-sml-module-members-for-compiler name structures)
                         members))))
        (:structure
         (let ((structure-members (structure-declaration-members dec structures)))
           (push (cons (second dec) structure-members) structures)
           (setf members
                 (append (qualify-structure-member-names
                          (second dec) structure-members)
                         members))))
        (:structure-app
         (let ((structure-members (structure-application-members dec)))
           (push (cons (second dec) structure-members) structures)
           (setf members
                 (append (qualify-structure-member-names
                          (second dec) structure-members)
                         members))))
        (:structure-alias
         (let ((structure-members
                 (lookup-sml-module-members-for-compiler
                  (third dec) structures)))
           (push (cons (second dec) structure-members) structures)
           (setf members
                 (append (qualify-structure-member-names
                          (second dec) structure-members)
                         members))))
        (otherwise
         (setf members (append (declaration-direct-bound-names dec) members)))))
    (remove-duplicates (nreverse members) :test #'string=)))

(defun compile-structure-alias-form (structure-name member-name)
  (let ((source (sml-visible-symbol member-name))
        (alias (sml-symbol (format nil "~A.~A" structure-name member-name))))
    `(progn
       (when (boundp ',source)
         (defparameter ,alias (symbol-value ',source))
         (let ((type (lookup-sml-binding-type ',source)))
           (when type
             (register-sml-binding-type ',alias type)))
         ,(compile-export-form (list alias))))))

(defun compile-qualified-structure-alias-form (target-structure source-structure member-name)
  (let ((source (sml-visible-symbol (format nil "~A.~A" source-structure member-name)))
        (alias (sml-symbol (format nil "~A.~A" target-structure member-name))))
    `(progn
       (when (boundp ',source)
         (defparameter ,alias (symbol-value ',source))
         (let ((type (lookup-sml-binding-type ',source)))
           (when type
             (register-sml-binding-type ',alias type)))
         ,(compile-export-form (list alias))))))

(defun compile-open-form (names)
  (let ((forms nil))
    (dolist (name (split-sml-module-names names))
      (let* ((source (resolve-structure-prefix name))
             (members (lookup-sml-module-members-for-compiler name *sml-local-structure-members*)))
        (dolist (member members)
          (note-sml-alias-binding (current-qualified-sml-name member)
                                  (qualify-sml-name source member))
          (push `(alias-sml-module-member-to-name
                  ,(target-sml-package-name)
                  ,(current-qualified-sml-name member)
                  ,source
                  ,member)
                forms))))
    `(progn ,@(nreverse forms))))

(defun note-sml-alias-binding (qualified-name source-name)
  "The program binds QUALIFIED-NAME to what SOURCE-NAME denotes (`open',
structure aliases)."
  (when *sml-unit-cells*
    (let ((source (sml-bound-symbol source-name)))
      (setf (gethash qualified-name *sml-unit-cells*)
            (if (sml-binding-cell-symbol-p source) source :plain)))))

(defun note-sml-instance-bindings (structure-name)
  "The program binds the members of structure STRUCTURE-NAME, a functor
instance, to the package's symbols, except those it then binds in cells of
their own (see BIND-SML-FUNCTOR-INSTANCE)."
  (when *sml-unit-cells*
    (let ((prefix (format nil "~A." structure-name)))
      (maphash (lambda (name cell)
                 (declare (ignore cell))
                 (when (string-prefix-p prefix name)
                   (setf (gethash name *sml-unit-cells*) :plain)))
               *sml-unit-cells*))))

(defun exception-alias-type (target)
  (or (lookup-sml-binding-type target *sml-package*) "exn"))

(defun compile-top-level-exception-alias (name target &optional lexical-env)
  (let* ((target-symbol (resolved-sml-symbol target lexical-env))
         (symbol (new-sml-binding-cell name)))
    `(progn
       (defparameter ,symbol
         ,(if (lexical-symbol-for-name target lexical-env)
              target-symbol
              `(if (boundp ',target-symbol)
                   (symbol-value ',target-symbol)
                   (make-sml-exception-constructor ,name))))
       ,@(compile-binding-publication symbol (exception-alias-type target)))))

(defun record-fields-sorted-by-label (fields)
  "FIELDS without :RECORD-REST, in canonical label order (the order of a
record's fields in its vector, SML-LABEL<)."
  (sort (remove :record-rest (copy-list fields))
        #'sml-label<
        :key #'first))

(defun record-construction-form (labels forms)
  "A form for the record with LABELS, in canonical order, and field values
FORMS: a vector with the record's shape in slot 0."
  (if (loop for label in labels
            for i from 1
            always (string= label (princ-to-string i)))
      `(sml-tuple ,@forms)
      `(vector (load-time-value (sml-record-shape ',labels) t) ,@forms)))

(defun indexed-record-pattern (index-patterns width)
  "A trivia pattern matching a record of WIDTH fields whose field at each
index of INDEX-PATTERNS, an alist from vector indices to compiled patterns,
matches the pattern.  The record's type is tested although a well-typed
value always has it: trivia may try a subpattern before the tests of the
patterns around it (e.g. the argument of a constructor before its tag), so
every access must be safe on any value."
  (let ((record (gensym "RECORD")))
    `(guard1 ,record (typep ,record '(simple-vector ,(1+ width)))
             ,@(loop for (index . pattern) in index-patterns
                     unless (eq pattern '_)
                       append `((svref ,record ,index) ,pattern)))))

(defun tuple-pattern-form (subpatterns)
  (indexed-record-pattern (loop for pattern in subpatterns
                                for i from 1
                                collect (cons i pattern))
                          (length subpatterns)))

(defun tuple-record-fields (fields)
  "When the labels of record FIELDS are exactly 1, ..., n for an n other than
1, the record is the n-tuple (the unit for n = 0) and this returns FIELDS in
label order; otherwise NIL and false."
  (let ((n (length fields)))
    (if (and (/= n 1)
             (not (member :record-rest fields))
             (loop for i from 1 to n
                   always (assoc (princ-to-string i) fields :test #'string=)))
        (values (loop for i from 1 to n
                      collect (assoc (princ-to-string i) fields :test #'string=))
                t)
        (values nil nil))))

(defun effect-free-sml-expression-p (ast)
  "Is evaluating AST free of effects, so that it may be moved?"
  (or (atom ast)
      (member (car ast) '(:var :op-var :ctor :word :unit :selector :fn))))

(defun compile-record-expression (fields local-exceptions lexical-env)
  "A record (or tuple) built from FIELDS, whose expressions are evaluated in
source order although the value is laid out in label order."
  (multiple-value-bind (tuple-fields tuplep) (tuple-record-fields fields)
    (let* ((layout (if tuplep tuple-fields (record-fields-sorted-by-label fields)))
           (temporaries
             ;; Only needed when evaluating in layout order could reorder
             ;; effects.
             (unless (or (equal layout fields)
                         (<= (count-if-not #'effect-free-sml-expression-p
                                           fields :key #'second)
                             1))
               (mapcar (lambda (field)
                         (list (gensym (first field))
                               (compile-expr (second field)
                                             local-exceptions lexical-env)))
                       fields)))
           (field-forms
             (mapcar (lambda (field)
                       (if temporaries
                           (first (nth (position field fields) temporaries))
                           (compile-expr (second field)
                                         local-exceptions lexical-env)))
                     layout))
           (form (record-construction-form (mapcar #'first layout) field-forms)))
      (if temporaries
          `(let* ,temporaries ,form)
          form))))

(defun pattern-type-bindings (pat type)
  (cond
    ((or (numberp pat) (stringp pat) (characterp pat) (eq pat :wild))
     nil)
    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     (list (cons (target-sml-symbol (second pat)) type)))
    ((and (listp pat) (eq (car pat) :pat-typed))
     (pattern-type-bindings (second pat) (sml-type-ast->binding-type (third pat))))
    ((and (listp pat) (eq (car pat) :pat-as))
     (append (pattern-type-bindings (second pat) type)
             (pattern-type-bindings (third pat) type)))
    ((and (listp pat) (member (car pat) '(:pat-ctor :ctor)))
     (unless (pattern-node-constructor-p pat)
       (list (cons (second pat) type))))
    ((and (listp pat) (member (car pat) '(:pat-unit :pat-nil)))
     nil)
    ((and (listp pat) (eq (car pat) :pat-app))
     (pattern-type-bindings (third pat) :unknown))
    ((and (listp pat) (eq (car pat) :pat-cons))
     (let ((element-type (if (and (consp type) (eq (car type) :list))
                             (second type)
                             :unknown))
           (list-type (if (and (consp type) (eq (car type) :list))
                          type
                          '(:list :unknown))))
       (append (pattern-type-bindings (second pat) element-type)
               (pattern-type-bindings (third pat) list-type))))
    ((and (listp pat) (eq (car pat) :pat-tuple))
     (loop for subpat in (cdr pat)
           for subtype in (if (and (consp type) (eq (car type) :tuple))
                              (cdr type)
                              (make-list (length (cdr pat)) :initial-element :unknown))
           append (pattern-type-bindings subpat subtype)))
    ((and (listp pat) (eq (car pat) :pat-record))
     (let ((field-types (if (and (consp type) (eq (car type) :record))
                            (cdr type)
                            nil)))
       (mapcan (lambda (field)
                 (pattern-type-bindings
                  (second field)
                  (or (cdr (assoc (first field) field-types :test #'string=))
                      :unknown)))
               (record-fields-sorted-by-label (cdr pat)))))
    (t
     nil)))

(defun pattern-bound-symbols (pat)
  (cond
    ((or (numberp pat) (stringp pat) (characterp pat) (eq pat :wild))
     nil)
    ((and (listp pat) (eq (car pat) :word))
     nil)
    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     (list (target-sml-symbol (second pat))))
    ((and (listp pat) (eq (car pat) :pat-typed))
     (pattern-bound-symbols (second pat)))
    ((and (listp pat) (eq (car pat) :pat-as))
     (append (pattern-bound-symbols (second pat))
             (pattern-bound-symbols (third pat))))
    ((and (listp pat) (member (car pat) '(:pat-ctor :ctor)))
     (unless (pattern-node-constructor-p pat)
       (list (target-sml-symbol (second pat)))))
    ((and (listp pat) (eq (car pat) :pat-app))
     (pattern-bound-symbols (third pat)))
    ((and (listp pat) (member (car pat) '(:pat-tuple)))
     (mapcan #'pattern-bound-symbols (cdr pat)))
    ((and (listp pat) (member (car pat) '(:pat-unit :pat-nil)))
     nil)
    ((and (listp pat) (eq (car pat) :pat-cons))
     (mapcan #'pattern-bound-symbols
             (list (second pat) (third pat))))
    ((and (listp pat) (eq (car pat) :pat-record))
     (mapcan (lambda (field)
               (pattern-bound-symbols (second field)))
             (record-fields-sorted-by-label (cdr pat))))
    (t
     (error "Unknown pattern for variable extraction: ~A" pat))))

(defun compile-clause-pattern (params)
  (if (= (length params) 1)
      (first params)
      `(:pat-tuple ,@params)))

(defun literal-case-pattern-value (pat)
  "Return PAT's EQL-comparable value and whether PAT is a literal case key."
  (cond
    ((or (integerp pat) (characterp pat))
     (values pat t))
    ((and (listp pat) (eq (car pat) :word))
     (values (second pat) t))
    ((and (listp pat) (eq (car pat) :pat-typed))
     (literal-case-pattern-value (second pat)))
    ((and (listp pat) (member (car pat) '(:pat-ctor :ctor) :test #'eq))
     (cond
       ((string= (second pat) "true") (values t t))
       ((string= (second pat) "false") (values nil t))
       (t (values nil nil))))
    (t
     (values nil nil))))

(defun literal-case-branches-p (branches)
  (every (lambda (branch)
           (or (eq (first branch) :wild)
               (nth-value 1 (literal-case-pattern-value (first branch)))))
         branches))

(defun compile-case-action-thunk (form)
  `(invoke-sml-case-action (lambda () ,form)))

(defun compile-literal-case (test branches local-exceptions lexical-env)
  (let ((clauses nil)
        (has-default nil))
    (dolist (branch branches)
      (if (eq (first branch) :wild)
          (progn
            (push `(otherwise
                     ,(compile-case-action-thunk
                       (compile-expr (second branch) local-exceptions lexical-env)))
                  clauses)
            (setf has-default t)
            (return))
          (multiple-value-bind (value literalp)
              (literal-case-pattern-value (first branch))
            (declare (ignore literalp))
            (push `((,value)
                    ,(compile-case-action-thunk
                      (compile-expr (second branch) local-exceptions lexical-env)))
                  clauses))))
    `(case ,(compile-expr test local-exceptions lexical-env)
       ,@(nreverse clauses)
       ,@(unless has-default
           `((otherwise ,(compile-match-failure "Match" "case"
                                                (first (first branches)))))))))

(defun tuple-literal-discriminant (pat)
  (when (and (listp pat) (eq (car pat) :pat-typed))
    (setf pat (second pat)))
  (when (and (listp pat)
             (eq (car pat) :pat-tuple)
             (integerp (second pat)))
    (values (second pat) t)))

(defun tuple-dispatch-case-branches-p (branches)
  (and (>= (length branches) 12)
       (eq (first (car (last branches))) :wild)
       (every (lambda (branch)
                (nth-value 1 (tuple-literal-discriminant (first branch))))
              (butlast branches))))

(defun group-tuple-dispatch-branches (branches)
  (let ((groups nil))
    (dolist (branch branches groups)
      (multiple-value-bind (key presentp)
          (tuple-literal-discriminant (first branch))
        (declare (ignore presentp))
        (let ((group (assoc key groups)))
          (if group
              (setf (cdr group) (append (cdr group) (list branch)))
              (setf groups (append groups (list (list key branch))))))))))

(defun lexical-environment-symbols (lexical-env)
  (remove-duplicates
   (remove-if-not #'symbolp (mapcar #'cdr lexical-env))
   :test #'eq))

(defun hoisted-function-parameters (lexical-env form)
  "The parameters of a hoisted function whose body FORM was compiled in
LEXICAL-ENV: its variables, and those of the functor body being compiled
that FORM refers to."
  (let ((parameters (lexical-environment-symbols lexical-env)))
    (if *functor-scope-variables*
        (let ((seen (make-hash-table :test #'eq))
              (extra nil))
          (labels ((walk (x)
                     (cond ((symbolp x)
                            (when (and (gethash x *functor-scope-variables*)
                                       (not (gethash x seen))
                                       (not (member x parameters)))
                              (setf (gethash x seen) t)
                              (push x extra)))
                           ((and (consp x) (not (gethash x seen)))
                            (setf (gethash x seen) t)
                            (walk (car x))
                            (walk (cdr x))))))
            (walk form))
          (append parameters (nreverse extra)))
        parameters)))

(defun compile-hoisted-case-action (ast local-exceptions branch-env)
  (let ((body (compile-expr ast local-exceptions branch-env)))
    (if *sml-hoisting-enabled*
        (let ((name (next-hoisted-sml-function-name))
              (parameters (hoisted-function-parameters branch-env body)))
          (push `(defun ,name ,parameters ,@(portable-tail-call-declarations) ,body)
                *sml-hoisted-forms*)
          `(,name ,@parameters))
        body)))

(defun compile-trivia-case-clause (branch local-exceptions lexical-env
                                   &key hoist-action)
  (let* ((var-map (pattern-variable-map (first branch) local-exceptions))
         (branch-env (append var-map lexical-env)))
    `(,(compile-pat (first branch) local-exceptions branch-env)
      ,(if hoist-action
           (compile-hoisted-case-action (second branch)
                                        local-exceptions branch-env)
           (compile-expr (second branch) local-exceptions branch-env)))))

(defun compile-tuple-dispatch-group (group value fallback
                                     local-exceptions lexical-env)
  (let ((clauses
          (mapcar (lambda (branch)
                    (compile-trivia-case-clause
                     branch local-exceptions lexical-env
                     :hoist-action *sml-hoisting-enabled*))
                  (rest group))))
    (if *sml-hoisting-enabled*
        (let ((name (next-hoisted-sml-function-name))
              (dispatch-argument (gensym "DISPATCH-VALUE"))
              (fallback-argument (gensym "DISPATCH-FALLBACK"))
              (parameters (hoisted-function-parameters lexical-env clauses)))
          (push `(defun ,name (,dispatch-argument ,fallback-argument ,@parameters)
                   (trivia:match ,dispatch-argument
                     ,@clauses
                     (_ (funcall ,fallback-argument))))
                *sml-hoisted-forms*)
          `(,name ,value ,fallback ,@parameters))
        `(trivia:match ,value
           ,@clauses
           (_ (funcall ,fallback))))))

(defun compile-tuple-dispatch-case (test branches local-exceptions lexical-env)
  (let* ((value (gensym "CASE-VALUE"))
         (fallback (gensym "CASE-FALLBACK"))
         (default-branch (car (last branches)))
         (groups (group-tuple-dispatch-branches (butlast branches))))
    `(let ((,value ,(compile-expr test local-exceptions lexical-env))
           (,fallback
             (lambda ()
               ,(compile-expr (second default-branch)
                              local-exceptions lexical-env))))
       (case (svref ,value 1)
         ,@(mapcar
            (lambda (group)
              `((,(first group))
                ,(compile-tuple-dispatch-group
                  group value fallback local-exceptions lexical-env)))
            groups)
         (otherwise (funcall ,fallback))))))

(defun sml-lambda-form (name arg body)
  "A one-argument lambda; named on SBCL so that backtraces show SML names."
  (portable-sml-lambda-form name arg body))

;;; Self tail calls.  SML programs loop by tail recursion, and only SBCL
;;; merges tail calls; on ECL and ABCL every iteration would take a stack
;;; frame.  So a function's calls to itself with all of its curried
;;; arguments, in tail position, are compiled as assignments to the
;;; function's parameters and a jump back to its pattern match.

(defun sml-application-spine (ast)
  "The head and the arguments of AST, a curried application `f a1 ... an`."
  (let ((args nil))
    (loop while (and (consp ast) (eq (car ast) :app))
          do (push (third ast) args)
             (setf ast (second ast)))
    (values ast args)))

(defun sml-declarations-may-bind-name-p (decs name)
  "Could DECS bind the value identifier NAME?  Declarations whose bindings
are not known here (`open`, structures) are assumed to."
  (some (lambda (dec)
          (case (car dec)
            ((:val :vals :val-rec :val-recs :fun :funs :datatype :datatype-replication
              :exception :exception-alias)
             (member name (declaration-direct-bound-names dec) :test #'equal))
            ((:type :infix :expr :signature) nil)
            (:local (or (sml-declarations-may-bind-name-p (second dec) name)
                        (sml-declarations-may-bind-name-p (third dec) name)))
            (otherwise t)))
        decs))

(defun mark-sml-self-tail-calls (ast name arity target)
  "AST with each call of function NAME with ARITY arguments in tail position
replaced by (:self-tail-call TARGET . arguments).  A second value is true
when there was such a call.  Scopes that may rebind NAME are left alone, as
are function bodies and case actions that are hoisted out of the function."
  (let ((found nil))
    (labels ((binds-name-p (pat)
               (member name (pattern-bound-names pat) :test #'equal))
             (branch (branch)
               (if (binds-name-p (first branch))
                   branch
                   `(,(first branch) ,(tail (second branch)) ,@(cddr branch))))
             (tail (ast)
               (if (atom ast)
                   ast
                   (case (car ast)
                     (:app
                      (multiple-value-bind (head args) (sml-application-spine ast)
                        (if (and (consp head)
                                 (eq (car head) :var)
                                 (equal (second head) name)
                                 (= (length args) arity))
                            (progn (setf found t)
                                   `(:self-tail-call ,target ,@args))
                            ast)))
                     (:if `(:if ,(second ast) ,(tail (third ast)) ,(tail (fourth ast))))
                     ((:andalso :orelse)
                      `(,(car ast) ,(second ast) ,(tail (third ast))))
                     (:typed `(:typed ,(tail (second ast)) ,@(cddr ast)))
                     (:seq `(:seq ,@(butlast (cdr ast)) ,(tail (car (last ast)))))
                     (:let
                      (if (sml-declarations-may-bind-name-p (second ast) name)
                          ast
                          `(:let ,(second ast)
                                 (,@(butlast (third ast)) ,(tail (car (last (third ast)))))
                                 ,@(cdddr ast))))
                     (:case
                      (if (tuple-dispatch-case-branches-p (cddr ast))
                          ast
                          `(:case ,(second ast) ,@(mapcar #'branch (cddr ast)))))
                     ;; The handled expression is not in tail position, but
                     ;; the handlers run after it has been unwound.
                     (:handle
                      `(:handle ,(second ast) ,(mapcar #'branch (third ast))
                                ,@(cdddr ast)))
                     (otherwise ast)))))
      (values (tail ast) found))))

(defun compile-self-tail-call (target args local-exceptions lexical-env)
  (destructuring-bind (tag &rest variables) target
    `(progn
       (psetq ,@(mapcan (lambda (variable arg)
                          (list variable
                                (compile-expr arg local-exceptions lexical-env)))
                        variables args))
       (go ,tag))))

(defun compile-fn-clauses (clauses &optional local-exceptions function-name outer-lexical-env)
  (let ((arity (length (first (first clauses)))))
    (unless (every (lambda (clause) (= (length (first clause)) arity)) clauses)
      (error "All fun clauses must have the same arity: ~A" clauses))
    (compile-fn-clauses-of-arity clauses arity local-exceptions function-name
                                 outer-lexical-env)))

;;; A function of one clause whose patterns are variables, wildcards and
;;; tuples of them cannot fail to match a well-typed argument (ROADMAP item
;;; 4): its parameters are bound with LET*, without a match or a tuple of
;;; the curried arguments.

(defun irrefutable-trivia-pattern-bindings (pattern form)
  "The LET* bindings that match the compiled PATTERN against FORM, a
variable or an access to one, or :REFUTABLE when PATTERN may fail."
  (cond
    ((eq pattern '_) nil)
    ((and (symbolp pattern) pattern (null (symbol-package pattern)))
     (list (list pattern form)))
    ((and (consp pattern) (eq (first pattern) 'guard1)
          (symbolp (second pattern))
          (let ((test (third pattern)))
            (and (consp test) (eq (first test) 'typep)
                 (eq (second test) (second pattern))))
          (loop for (generator) on (cdddr pattern) by #'cddr
                always (and (consp generator) (eq (first generator) 'svref)
                            (eq (second generator) (second pattern)))))
     ;; A record or tuple pattern (INDEXED-RECORD-PATTERN).
     (let ((record (second pattern))
           (bindings nil))
       (loop for (generator subpattern) on (cdddr pattern) by #'cddr
             do (let ((inner (irrefutable-trivia-pattern-bindings
                              subpattern generator)))
                  (when (eq inner :refutable)
                    (return-from irrefutable-trivia-pattern-bindings :refutable))
                  (setf bindings (append bindings inner))))
       (and bindings (cons (list record form) bindings))))
    (t :refutable)))

(defun pattern-variable-nodes (tree)
  "The :PAT-VAR nodes in pattern TREE."
  (cond ((atom tree) nil)
        ((eq (car tree) :pat-var) (list tree))
        (t (loop for sub in tree append (pattern-variable-nodes sub)))))

(defun compile-irrefutable-clause (params body args local-exceptions outer-lexical-env)
  "BODY with PARAMS, the patterns of a function's only clause, bound to the
variables ARGS, or NIL when a pattern is refutable."
  (let* ((var-map (pattern-variable-map (compile-clause-pattern params) local-exceptions))
         (branch-env (append var-map outer-lexical-env))
         (bindings
           (loop for param in params
                 for arg in args
                 for inner = (irrefutable-trivia-pattern-bindings
                              (compile-pat param local-exceptions branch-env) arg)
                 when (eq inner :refutable)
                   do (return-from compile-irrefutable-clause nil)
                 append inner)))
    ;; Reals are bound as double-floats, so that SBCL keeps them unboxed.
    (let ((reals (loop for node in (pattern-variable-nodes params)
                       when (eq (sml-node-representation node) :real)
                         collect (cdr (assoc (second node) var-map :test #'string=)))))
      (setf bindings (mapcar (lambda (binding)
                               (if (member (first binding) reals)
                                   `(,(first binding) (sml-double ,(second binding)))
                                   binding))
                             bindings)))
    `(let* ,bindings
       (declare (ignorable ,@(mapcar #'first bindings)))
       ,(compile-expr body local-exceptions branch-env))))

(defun compile-fn-clauses-of-arity (clauses arity local-exceptions function-name
                                    outer-lexical-env)
  (let* ((tmp-args (loop repeat arity collect (gensym "ARG")))
         ;; Fresh variables for the loop: the curried parameters are closed
         ;; over by partial applications, so they must not be assigned.
         (loop-args (loop repeat arity collect (gensym "LOOP-ARG")))
         ;; Not a gensym: ABCL's pretty printer loses the identity of an
         ;; uninterned TAGBODY tag in emitted artifacts.  An inner function's
         ;; loop shadows the tag, and only its own body jumps to it.
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
                                                       arity (cons loop-tag loop-args))
                           (when found (setf looping t))
                           body)
                         (second clause)))
                   clauses))
         (args (if looping loop-args tmp-args))
         (match
           (or
            (and (null (rest clauses))
                 (compile-irrefutable-clause (first (first clauses)) (first bodies)
                                             args local-exceptions outer-lexical-env))
            ;; Curried arguments are matched as multiple values, without
            ;; allocating a tuple of them.
            `(,@(if (= arity 1) `(trivia:match ,(first args)) `(trivia:match* ,args))
              ,@(mapcar (lambda (clause body)
                          (let* ((pat (compile-clause-pattern (first clause)))
                                 (var-map (pattern-variable-map pat local-exceptions))
                                 (branch-env (append var-map outer-lexical-env)))
                            `(,(if (= arity 1)
                                   (compile-pat pat local-exceptions branch-env)
                                   (mapcar (lambda (param)
                                             (compile-pat param local-exceptions branch-env))
                                           (first clause)))
                              ,(compile-expr body
                                             local-exceptions
                                             branch-env))))
                        clauses bodies)
              (,(if (= arity 1) '_ (make-list arity :initial-element '_))
               ,(compile-match-failure "Match" (or function-name "fn")
                                       (first (first clauses))))))))
    (reduce (lambda (arg body) (sml-lambda-form function-name arg body))
            tmp-args
            :from-end t
            :initial-value
            (if looping
                (let ((block (gensym "SELF")))
                  `(let ,(mapcar #'list loop-args tmp-args)
                     (block ,block
                       (tagbody
                          ,loop-tag
                          (return-from ,block ,match)))))
                match))))

(defun first-positioned-sml-node (tree)
  "The source offset of the first node of AST TREE whose position is known."
  (or (sml-ast-position tree)
      (and (consp tree)
           (some #'first-positioned-sml-node tree))))

(defun compile-match-failure (exception-name where ast)
  "The failure branch of a match: raise Match or Bind (EXCEPTION-NAME).  WHERE
and the position of AST, the match's first pattern, are diagnostics."
  `(sml-raise-match-failure ,exception-name ,where
                            ,(first-positioned-sml-node ast)))

(defun compile-local-val-binding (pat expr body bindings
                                  &optional local-exceptions lexical-env)
  "Bind PAT to the value of EXPR around BODY.  BINDINGS maps the variables of
PAT to their symbols (see LOCAL-DECLARATION-LEXICAL-BINDINGS)."
  (cond
    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     `(let ((,(local-binding-symbol (second pat) bindings)
              ,expr))
        ,body))
    ((eq pat :wild)
     (let ((tmp (gensym "IGNORED")))
       `(let ((,tmp ,expr))
          (declare (ignore ,tmp))
          ,body)))
    (t
     (let ((tmp (gensym "MATCHED"))
           (binding-env (append bindings lexical-env)))
       `(let ((,tmp ,expr))
          (trivia:match ,tmp
            (,(let ((*sml-module-prefix* nil))
                (compile-pat pat local-exceptions binding-env))
             ,body)
            (_ ,(compile-match-failure "Bind" "val" pat))))))))

(defun constructor-arity-forms (symbol arity)
  "Record now, for the code compiled next, and when the compiled code is
loaded, that the values of constructor SYMBOL have representation ARITY."
  (setf (get symbol 'sml-constructor-arity) arity)
  `((setf (get ',symbol 'sml-constructor-arity) ,arity)))

(defun local-datatype-constructor-arities (dec)
  (when (eq (car dec) :datatype)
    (mapcar (lambda (ctor) (cons (local-datatype-constructor-tag ctor) 0))
            (third dec))))

(defun local-datatype-constructor-tag (ctor)
  "The symbol that tags the values of local datatype constructor CTOR."
  (sml-symbol (second ctor)))

(defun compile-local-datatype-bindings (ctors body bindings)
  (let ((bindings
          (mapcar (lambda (ctor)
                    (let ((tag (local-datatype-constructor-tag ctor)))
                      (list (local-binding-symbol (second ctor) bindings)
                            (if (fourth ctor)
                                `(lambda (payload)
                                   (cons ',tag payload))
                                `',tag))))
                  ctors)))
    `(let ,bindings
       (declare (ignorable ,@(mapcar #'first bindings)))
       ,body)))

(defun declaration-exposed-exceptions (dec)
  (cond
    ((eq (car dec) :exception)
     (list (cons (second dec)
                 (not (null (getf (cddr dec) :arg-type))))))
    ((eq (car dec) :exception-alias)
     (let ((type (exception-alias-type (third dec))))
       (list (cons (second dec)
                   (payload-exception-type-p type)))))
    ((eq (car dec) :local)
     (declarations-exposed-exceptions (third dec)))
    (t
     nil)))

(defun declarations-exposed-exceptions (decs)
  (mapcan #'declaration-exposed-exceptions decs))

(defun extend-local-exceptions (local-exceptions dec)
  (append (declaration-exposed-exceptions dec)
          local-exceptions))

(defvar *sml-local-declaration-bindings* (make-portable-weak-eq-hash-table)
  "Local declaration -> its lexical bindings, so that the binding and every
use of a let-bound variable get the same fresh symbol, however many times
the compiler asks.")

(defun local-declaration-lexical-bindings (dec)
  "An alist from the identifiers local declaration DEC binds to the fresh
variables that hold them."
  (when (consp dec)
    (multiple-value-bind (bindings presentp)
        (gethash dec *sml-local-declaration-bindings*)
      (if presentp
          bindings
          (setf (gethash dec *sml-local-declaration-bindings*)
                (compute-local-declaration-lexical-bindings dec))))))

(defun compute-local-declaration-lexical-bindings (dec)
  (flet ((fresh (name) (cons name (sml-lexical-symbol name)))
         (nested (decs) (declarations-lexical-bindings decs)))
    (case (car dec)
      (:val (mapcar #'fresh (pattern-bound-names (second dec))))
      ((:vals :funs :val-recs) (nested (cdr dec)))
      ((:val-rec :fun :exception :exception-alias)
       (list (fresh (second dec))))
      (:datatype (mapcar (lambda (ctor) (fresh (second ctor))) (third dec)))
      (:local (nested (third dec)))
      (otherwise nil))))

(defun declarations-lexical-bindings (decs)
  "The lexical bindings of the local declarations DECS, as one fresh alist
(the memoized lists must not be destructively joined), those of later
declarations first: they shadow the earlier ones."
  (loop for dec in (reverse decs)
        append (local-declaration-lexical-bindings dec)))

(defun local-binding-symbol (name bindings)
  "The symbol BINDINGS (from LOCAL-DECLARATION-LEXICAL-BINDINGS) gives NAME."
  (or (cdr (assoc name bindings :test #'string=))
      (error "No local binding for ~A" name)))

(defun local-datatype-constructor-bindings (dec)
  "Constructor-name -> tag bindings for a local datatype declaration DEC."
  (when (eq (car dec) :datatype)
    (mapcar (lambda (ctor)
              (cons (second ctor) (local-datatype-constructor-tag ctor)))
            (third dec))))

(defun local-open-symbol-bindings (dec)
  (when (eq (car dec) :open)
    (declaration-binding-symbol-bindings dec)))

(defun local-open-constructor-bindings (dec)
  (when (eq (car dec) :open)
    (declaration-constructor-symbol-bindings dec)))

(defun declarations-local-open-symbol-bindings (decs)
  (mapcan #'local-open-symbol-bindings decs))

(defun declarations-local-open-constructor-bindings (decs)
  "The constructors local DECS bring into scope by `open' and by datatype
declarations, those of later declarations first."
  (loop for dec in (reverse decs)
        append (local-datatype-constructor-bindings dec)
        append (local-open-constructor-bindings dec)))

(defun open-shadowing-lexical-bindings (dec lexical-env)
  "The bindings of `open' declaration DEC (see LOCAL-OPEN-SYMBOL-BINDINGS) of
names LEXICAL-ENV binds, which they shadow."
  (remove-if-not (lambda (binding) (lexical-symbol-for-name (car binding) lexical-env))
                 (local-open-symbol-bindings dec)))

(defun declarations-body-lexical-env (decs lexical-env)
  "The lexical environment after the local declarations DECS."
  (let ((env lexical-env))
    (dolist (dec decs env)
      (setf env (append (if (eq (car dec) :open)
                            (open-shadowing-lexical-bindings dec env)
                            (local-declaration-lexical-bindings dec))
                        env)))))

(defun compile-local-decls-into-body (decs body &optional (local-exceptions nil) lexical-env)
  (if (null decs)
      body
      (let* ((dec (first decs))
             (extended-exceptions (extend-local-exceptions local-exceptions dec))
             (dec-bindings (if (eq (car dec) :open)
                               (open-shadowing-lexical-bindings dec lexical-env)
                               (local-declaration-lexical-bindings dec)))
             (open-symbol-bindings (local-open-symbol-bindings dec))
             (open-constructor-bindings (local-open-constructor-bindings dec))
             (wrapped-body
               (let ((*sml-binding-symbol-env*
                       (append open-symbol-bindings *sml-binding-symbol-env*))
                     (*sml-constructor-symbol-env*
                       (append (local-datatype-constructor-bindings dec)
                               open-constructor-bindings
                               *sml-constructor-symbol-env*))
                     (*sml-local-constructor-arities*
                       (append (local-datatype-constructor-arities dec)
                               *sml-local-constructor-arities*)))
                 (compile-local-decls-into-body
                  (rest decs)
                  body
                  extended-exceptions
                  (append dec-bindings lexical-env)))))
        (compile-local-decl dec wrapped-body local-exceptions lexical-env))))

(defun compile-local-decls (decs body-asts &optional (local-exceptions nil) lexical-env)
  (let* ((module-bindings (declarations-local-module-member-bindings decs))
         (structure-prefixes (module-local-structure-prefixes
                              decs
                              (or *sml-module-prefix* "")))
         (body-env (declarations-body-lexical-env decs lexical-env))
         (body-exceptions (append (declarations-exposed-exceptions decs)
                                  local-exceptions))
         (open-symbol-bindings (declarations-local-open-symbol-bindings decs))
         (open-constructor-bindings
           (declarations-local-open-constructor-bindings decs)))
    (let ((*sml-local-structure-members* (append module-bindings
                                                 *sml-local-structure-members*))
          (*sml-local-structure-prefixes* (append structure-prefixes
                                                 *sml-local-structure-prefixes*)))
      (let ((compiled-body
              (let ((*sml-binding-symbol-env*
                      (append open-symbol-bindings *sml-binding-symbol-env*))
                    (*sml-constructor-symbol-env*
                      (append open-constructor-bindings *sml-constructor-symbol-env*))
                    (*sml-local-constructor-arities*
                      (append (mapcan #'local-datatype-constructor-arities decs)
                              *sml-local-constructor-arities*)))
                `(progn ,@(mapcar (lambda (expr)
                                    (compile-expr expr body-exceptions body-env))
                                  body-asts)))))
        (compile-local-decls-into-body
         decs compiled-body local-exceptions lexical-env)))))

(defun compile-program-decls-body (decs &optional (local-exceptions nil) lexical-env)
  `(progn ,@(compile-program-decls decs local-exceptions lexical-env)))

(defun compile-local-decl (dec body &optional local-exceptions lexical-env)
  (cond
    ((eq (car dec) :val)
     (compile-local-val-binding (second dec)
                                (compile-expr (third dec) local-exceptions lexical-env)
                                body
                                (local-declaration-lexical-bindings dec)
                                local-exceptions
                                lexical-env))
    ((eq (car dec) :vals)
     ;; `val p1 = e1 and p2 = e2` evaluates every expression before binding
     ;; anything, like `val (p1, p2) = (e1, e2)`.  `val rec` bindings joined
     ;; to them come last.
     (let ((vals (remove-if-not (lambda (val) (eq (car val) :val)) (cdr dec)))
           (recs (remove :val (cdr dec) :key #'car)))
       (compile-local-val-binding
        `(:pat-tuple ,@(mapcar #'second vals))
        `(sml-tuple ,@(mapcar (lambda (val-dec)
                                (compile-expr (third val-dec)
                                              local-exceptions lexical-env))
                              vals))
        (if recs
            (compile-local-decls-into-body recs body local-exceptions lexical-env)
            body)
        (local-declaration-lexical-bindings dec)
        local-exceptions
        lexical-env)))
	    ((eq (car dec) :fun)
	     (let ((name (local-binding-symbol
                         (second dec) (local-declaration-lexical-bindings dec))))
	       `(let ((,name nil))
	          (setf ,name ,(compile-fn-clauses (third dec)
	                                            local-exceptions
	                                            (second dec)
	                                            (acons (second dec)
	                                                   name
	                                                   lexical-env)))
	          ,body)))
    ((eq (car dec) :funs)
     ;; `fun f ... and g ...` is mutually recursive: bind every name before
     ;; any of the closures is created.
     (let* ((funs (cdr dec))
            (bindings (local-declaration-lexical-bindings dec))
            (extended-env (append bindings lexical-env)))
       `(let ,(mapcar (lambda (binding) `(,(cdr binding) nil)) bindings)
          ,@(mapcar (lambda (fun-dec)
                      `(setf ,(cdr (assoc (second fun-dec) bindings :test #'equal))
                             ,(compile-fn-clauses (third fun-dec)
                                                   local-exceptions
                                                   (second fun-dec)
                                                   extended-env)))
                    funs)
          ,body)))
    ((eq (car dec) :val-rec)
     (let ((name (local-binding-symbol
                  (second dec) (local-declaration-lexical-bindings dec))))
       `(let ((,name nil))
          (setf ,name ,(compile-expr (third dec)
                                     local-exceptions
                                     (acons (second dec) name lexical-env)))
          ,body)))
    ((eq (car dec) :val-recs)
     ;; Like `fun ... and ...': bind every name before the closures.
     (let* ((bindings (local-declaration-lexical-bindings dec))
            (extended-env (append bindings lexical-env)))
       `(let ,(mapcar (lambda (binding) `(,(cdr binding) nil)) bindings)
          ,@(mapcar (lambda (val-rec)
                      `(setf ,(cdr (assoc (second val-rec) bindings :test #'equal))
                             ,(compile-expr (third val-rec) local-exceptions extended-env)))
                    (cdr dec))
          ,body)))
    ((eq (car dec) :datatype)
     (compile-local-datatype-bindings (third dec) body
                                      (local-declaration-lexical-bindings dec)))
    ((member (car dec) '(:type :infix :datatype-replication :expr :signature :open))
     body)
    ;; A structure declared by `local' or `let' is a module like any other;
    ;; the scope of its name is the compiler's business.
    ((member (car dec) '(:structure :structure-alias :structure-app :functor))
     `(progn ,(compile-decl dec local-exceptions lexical-env)
             ,body))
    ((eq (car dec) :exception)
     (let* ((name (local-binding-symbol
                   (second dec) (local-declaration-lexical-bindings dec)))
            (arg-type (getf (cddr dec) :arg-type)))
       (if arg-type
           `(let ((,name (make-sml-exception-function ,(second dec))))
              (declare (ignorable ,name))
              ,body)
           `(let ((,name (make-sml-exception-constructor ,(second dec))))
              (declare (ignorable ,name))
              ,body))))
    ((eq (car dec) :exception-alias)
     (let ((name (local-binding-symbol
                  (second dec) (local-declaration-lexical-bindings dec)))
           (target (resolved-sml-symbol (third dec) lexical-env)))
       `(let ((,name ,(if (lexical-symbol-for-name (third dec) lexical-env)
                          target
                          `(if (boundp ',target)
                               (symbol-value ',target)
                               (make-sml-exception-constructor ,(second dec))))))
          (declare (ignorable ,name))
          ,body)))
    ((eq (car dec) :local)
     (let* ((local-decs (second dec))
            (body-decs (third dec))
            (inner-exceptions (append (declarations-exposed-exceptions local-decs)
                                      local-exceptions))
            (module-bindings (declarations-local-module-member-bindings local-decs))
            (structure-prefixes (module-local-structure-prefixes
                                 local-decs
                                 (or *sml-module-prefix* "")))
            (local-bindings (declarations-lexical-bindings local-decs))
            (open-symbol-bindings
              (declarations-local-open-symbol-bindings local-decs))
            (open-constructor-bindings
              (declarations-local-open-constructor-bindings local-decs))
            (inner-body
              (let ((*sml-local-structure-members* (append module-bindings
                                                           *sml-local-structure-members*))
                    (*sml-local-structure-prefixes* (append structure-prefixes
                                                           *sml-local-structure-prefixes*))
                    (*sml-binding-symbol-env*
                      (append open-symbol-bindings *sml-binding-symbol-env*))
                    (*sml-constructor-symbol-env*
                      (append open-constructor-bindings
                              *sml-constructor-symbol-env*)))
                (compile-local-decls-into-body
                 body-decs
                 body
                 inner-exceptions
                 ;; Each body declaration's bindings come into scope
                 ;; after it.
                 (append local-bindings lexical-env)))))
       (let ((*sml-local-structure-members* (append module-bindings
                                                    *sml-local-structure-members*))
             (*sml-local-structure-prefixes* (append structure-prefixes
                                                    *sml-local-structure-prefixes*)))
         (compile-local-decls-into-body local-decs inner-body local-exceptions lexical-env))))
    (t
     (error "Unknown decl in let: ~A" dec))))

(defun compile-top-level-val (pat expr &optional local-exceptions declared-type lexical-env)
  ;; Each expression is compiled before the cells of the names it binds
  ;; are made: it refers to the bindings in scope before the declaration.
  (let* ((expr-type (or declared-type
                        (infer-sml-ast-type expr :package *sml-package*)))
         (typed-bindings (pattern-type-bindings pat expr-type)))
    (cond
      ((and (listp pat) (member (car pat) '(:pat-var :var)))
       (let* ((compiled-expr (compile-expr expr local-exceptions lexical-env))
              (cell (new-sml-binding-cell (second pat))))
         `(progn
            (defparameter ,cell ,compiled-expr)
            ,@(compile-binding-publication cell expr-type))))
      ((eq pat :wild)
       (compile-expr expr local-exceptions lexical-env))
      (t
       (let* ((tmp (gensym "MATCHED"))
              (var-map (pattern-variable-map pat local-exceptions))
              (compiled-expr (compile-expr expr local-exceptions lexical-env))
              (compiled-pat (compile-pat pat local-exceptions
                                         (append var-map lexical-env)))
              (cells (mapcar (lambda (entry) (new-sml-binding-cell (car entry)))
                             var-map)))
         `(let ((,tmp ,compiled-expr))
            (trivia:match ,tmp
              (,compiled-pat
               ,@(mapcar (lambda (entry cell) `(defparameter ,cell ,(cdr entry)))
                         var-map cells)
               ,@(mapcan (lambda (entry cell)
                           (compile-binding-publication
                            cell
                            (or (cdr (assoc (target-sml-symbol (car entry))
                                            typed-bindings))
                                (cdr (assoc (car entry) typed-bindings
                                            :test #'equal))
                                :unknown)))
                         var-map cells)
               ,tmp)
              (_ ,(compile-match-failure "Bind" "val" pat)))))))))

(defun compile-exception-ctor-pattern (name &optional lexical-env)
  (let ((it (gensym "EXN")))
    `(guard1 ,it
             (and (sml-exception-tag-p ,it)
                  (eq (sml-exception-constructor-tag ,it)
                      (sml-exception-constructor-tag
                       ,(resolved-sml-symbol name lexical-env)))))))

(defun compile-exception-app-pattern (name payload local-exceptions &optional lexical-env)
  (let ((it (gensym "EXN")))
    `(guard1 ,it
             (and (consp ,it)
                  (sml-exception-tag-p (car ,it))
                  (eq (sml-exception-constructor-tag ,it)
                      (sml-exception-constructor-tag
                       ,(resolved-sml-symbol name lexical-env))))
             (cdr ,it) ,(compile-pat payload local-exceptions lexical-env))))

(defun compile-ref-pattern (payload local-exceptions &optional lexical-env)
  (let ((it (gensym "REF")))
    `(guard1 ,it
             (and (vectorp ,it)
                  (= (length ,it) 2)
                  (eq (aref ,it 0) :ref))
             (aref ,it 1) ,(compile-pat payload local-exceptions lexical-env))))

(defun compile-record-pattern (fields local-exceptions lexical-env &optional node)
  "A pattern on the record FIELDS.  The fields are at fixed indices when
the record's labels are known: always for a pattern without `...', and for
a flexible pattern NODE when the static checker gives its record type.
Otherwise the fields are found by label (SML-RECORD-REF)."
  (let* ((sorted (record-fields-sorted-by-label fields))
         (labels (if (member :record-rest fields)
                     (sml-node-record-labels node)
                     (mapcar #'first sorted))))
    (if labels
        (indexed-record-pattern
         (mapcar (lambda (field)
                   (cons (1+ (position (first field) labels :test #'string=))
                         (compile-pat (second field) local-exceptions lexical-env)))
                 sorted)
         (length labels))
        (let ((record (gensym "RECORD")))
          ;; SML-RECORD-FIELD is NIL, not an error, on a value without the
          ;; field (see INDEXED-RECORD-PATTERN).
          `(guard1 ,record t
                   ,@(mapcan (lambda (field)
                               `((sml-record-field ,record ,(first field))
                                 ,(compile-pat (second field)
                                               local-exceptions
                                               lexical-env)))
                             sorted))))))

(defun compile-pat (pat &optional local-exceptions lexical-env)
  (cond
    ((numberp pat) pat)
    ((and (listp pat) (eq (car pat) :word)) (second pat))
    ((stringp pat) pat)
    ((characterp pat) pat)
    ((eq pat :wild) '_)

    ((and (listp pat) (member (car pat) '(:pat-ctor :ctor)))
     (cond
       ((string= (second pat) "true") t)
       ((string= (second pat) "false") nil)
       ((string= (second pat) "nil") nil)
       ((pattern-node-exception-p pat local-exceptions)
        (compile-exception-ctor-pattern (second pat) lexical-env))
       (t
        (let* ((name (second pat))
               (static-ctor-p (sml-status-constructor-p (pattern-node-status pat)))
               (it (gensym "CTOR"))
               (lexical-ctor (or (lexical-symbol-for-name name lexical-env)
                                 (functor-scope-long-name-variable name)))
               (known-ctor (known-constructor-symbol-for-name name))
               (global-ctor (or known-ctor
                                (resolved-sml-symbol name lexical-env))))
	          (cond
	            ((and lexical-ctor
	                  (not static-ctor-p)
	                  (not known-ctor)
	                  (not (functor-scope-constructor-tag lexical-ctor))
	                  (not (sml-constructor-symbol-p global-ctor)))
	             lexical-ctor)
	            (lexical-ctor
	             `(guard1 ,it (eql ,it ,lexical-ctor)))
            ((and known-ctor
                  (eq global-ctor known-ctor)
                  (or (not (boundp known-ctor))
                      (eq (symbol-value known-ctor) known-ctor)))
             ;; A nullary constructor's value is its tag, the symbol a
             ;; datatype declaration binds to itself.
             `(guard1 ,it (eq ,it ',known-ctor)))
            (known-ctor
             `(guard1 ,it
                      (and (boundp ',global-ctor)
                           (eql ,it (symbol-value ',global-ctor)))))
            ((sml-constructor-symbol-p global-ctor)
             `(guard1 ,it (eql ,it (symbol-value ',global-ctor))))
            (static-ctor-p
             ;; A constructor not registered yet, e.g. one of a structure
             ;; defined earlier in this same program.
             `(guard1 ,it (sml-constructor-tag-p ,it ',global-ctor)))
            (t
             global-ctor))))))

    ((and (listp pat) (eq (car pat) :pat-typed))
     (compile-pat (second pat) local-exceptions lexical-env))

    ((and (listp pat) (eq (car pat) :pat-as))
     (let ((alias (compile-pat (second pat) local-exceptions lexical-env)))
       `(guard1 ,alias t ,alias ,(compile-pat (third pat)
                                              local-exceptions
                                              lexical-env))))

    ((and (listp pat) (eq (car pat) :pat-app))
     (if (string= (second (second pat)) "ref")
         (compile-ref-pattern (third pat) local-exceptions lexical-env)
         (if (pattern-node-exception-p (second pat) local-exceptions)
         (compile-exception-app-pattern (second (second pat))
                                        (third pat)
                                        local-exceptions
                                        lexical-env)
	         (let* ((name (second (second pat)))
                        (tag (lexical-constructor-tag-form name lexical-env))
                        (ctor (constructor-symbol-for-name name lexical-env))
	                (payload (compile-pat (third pat) local-exceptions lexical-env)))
	           (cond
                     (tag
                      ;; A constructor of the functor body being compiled.
                      (let ((it (gensym "CTOR-VALUE")))
                        `(guard1 ,it
                                 (and (consp ,it) (eq (car ,it) ,tag))
                                 (cdr ,it) ,payload)))
                     ((and (or (known-constructor-symbol-for-name name)
                               (sml-constructor-symbol-p ctor))
                           (eql (constructor-arity-at-compile-time ctor) 0))
	              `(cons ',ctor ,payload))
                     ((and (or (known-constructor-symbol-for-name name)
                               (sml-constructor-symbol-p ctor))
                           (constructor-arity-at-compile-time ctor))
                      (compile-flat-constructor-pattern
                       ctor (constructor-arity-at-compile-time ctor)
                       (third pat) local-exceptions lexical-env))
                     ((or (known-constructor-symbol-for-name name)
                          (sml-constructor-symbol-p ctor))
                      ;; Its representation is not known here.
                      (let ((it (gensym "CTOR-VALUE")))
                        `(guard1 ,it
                                 (eq (sml-constructed-tag ,it) ',ctor)
                                 (sml-constructed-argument ,it) ,payload)))
                     ((null (symbol-package ctor))
                      ;; A variable holding the constructor, e.g. a member
                      ;; of a functor's argument.
                      (let ((it (gensym "CTOR-VALUE")))
                        `(guard1 ,it
                                 (eq (sml-constructed-tag ,it)
                                     (sml-constructor-value-tag ,ctor))
                                 (sml-constructed-argument ,it) ,payload)))
                     (t
                       ;; Not resolvable now (e.g. a member of a functor
                       ;; instance bound at run time): compare against the
                       ;; run-time constructor.
                       ;; The value may be a nullary constructor (a symbol)
                       ;; of the same datatype, so test for a cons rather
                       ;; than assert one.
                       (let ((it (gensym "CTOR-VALUE")))
                         `(guard1 ,it
                                  (let ((tag (sml-constructed-tag ,it)))
                                    (and tag (sml-constructor-tag-p tag ',ctor)))
                                  (sml-constructed-argument ,it) ,payload))))))))

    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     (or (lexical-symbol-for-name (second pat) lexical-env)
         (target-sml-symbol (second pat))))

    ((and (listp pat) (eq (car pat) :pat-unit))
     ;; The unit is the only value of its type.
     '_)

    ((and (listp pat) (eq (car pat) :pat-tuple))
     (tuple-pattern-form (mapcar (lambda (subpat)
                                   (compile-pat subpat local-exceptions lexical-env))
                                 (cdr pat))))

    ((and (listp pat) (eq (car pat) :pat-record))
     (multiple-value-bind (tuple-fields tuplep) (tuple-record-fields (cdr pat))
       (if tuplep
           ;; {1 = p1, ..., n = pn} is the tuple pattern (p1, ..., pn).
           (compile-pat (if tuple-fields
                            `(:pat-tuple ,@(mapcar #'second tuple-fields))
                            '(:pat-unit))
                        local-exceptions lexical-env)
           (compile-record-pattern (cdr pat) local-exceptions lexical-env pat))))

    ((and (listp pat) (eq (car pat) :pat-nil)) 'nil)
    ((and (listp pat) (eq (car pat) :pat-cons))
     `(cons ,(compile-pat (second pat) local-exceptions lexical-env)
            ,(compile-pat (third pat) local-exceptions lexical-env)))
    (t (error "Unknown pattern ~A" pat))))

(defun overloaded-identifier-instance (node)
  "When NODE is an occurrence of an overloaded identifier (`+`, `<`, `abs`,
...) whose type the static checker resolved, return its name and the type
name it is used at (\"int\", \"word\", \"word8\", \"real\", \"char\" or
\"string\")."
  (when (and (consp node) (member (car node) '(:var :op-var)))
    (let ((tyname (sml-overloaded-instance node)))
      (when tyname
        (values (second node) tyname)))))

(defparameter *specialized-overloading-instances*
  '("int" "intinf" "word" "word8" "word16" "word32" "word64" "real" "char" "string")
  "The instances COMPILE-OVERLOADED-OPERATION specializes.  The other
integer types (Int32.int, ...) take the generic operations, which check
Int's range.")

(defun masked-real-form-p (form)
  (and (consp form)
       (eq (car form) 'with-portable-float-traps-masked)
       (= (length form) 2)))

(defun unmasked-real-form (form)
  "FORM without an outermost WITH-PORTABLE-FLOAT-TRAPS-MASKED: real
operations nested in one another need only the outermost one, which every
caller of this puts around the result."
  (if (masked-real-form-p form) (second form) form))

(defun compile-overloaded-operation (name tyname args)
  "Specialized CL for overloaded identifier NAME used at type TYNAME and
applied to the compiled ARGS (one for unary, two for binary operators).
Returns NIL for combinations without a specialization.  At \"intinf\"
(IntInf.int) the operations are CL's, which never overflow.  Int
operations raise Overflow (SML-INT-+), real ones are double-float
arithmetic with IEEE results (no traps), and word ones wrap at the word
type's size; the operands are declared (SML-DOUBLE, SML-WORD-OPERAND) so
that SBCL compiles them as machine arithmetic."
  (unless (member tyname *specialized-overloading-instances* :test #'string=)
    (return-from compile-overloaded-operation nil))
  (let* ((original-args args)
         (bits (sml-word-type-bits tyname))
         (int-p (string= tyname "int"))
         (real-p (string= tyname "real"))
         (comparison (cdr (assoc name '(("<" . <) (">" . >)
                                        ("<=" . <=) (">=" . >=))
                                 :test #'string=)))
         (args (cond (real-p (mapcar (lambda (arg) `(sml-double ,(unmasked-real-form arg)))
                                     args))
                     (bits (mapcar (lambda (arg) `(sml-word-operand ,bits ,arg)) args))
                     (t args))))
    (flet ((wrap (form)
             (cond (bits `(ldb (byte ,bits 0) ,form))
                   ;; IEEE results (infinities, NaNs) rather than traps.
                   (real-p `(with-portable-float-traps-masked ,form))
                   (t form)))
           (remask (form)
             ;; Real operands lost their own masking above (one is enough
             ;; around nested operations): restore it around the whole.
             (if (and form real-p
                      (not (masked-real-form-p form))
                      (some #'masked-real-form-p original-args))
                 `(with-portable-float-traps-masked ,form)
                 form)))
     (remask
      (cond
        ((= (length args) 1)
         (let ((x (first args)))
           (cond
             ;; Int arithmetic raises Overflow (see SML-INT-+).
             ((and int-p (string= name "~")) `(sml-int-negate ,x))
             ((and int-p (string= name "abs")) `(sml-int-abs ,x))
             ;; Negation and abs of a double never trap.
             ((and real-p (string= name "~")) `(- ,x))
             ((string= name "~") (wrap `(- ,x)))
             ((and (string= name "abs") (not bits)) `(abs ,x)))))
        ((/= (length args) 2) nil)
        (comparison
         (destructuring-bind (x y) args
           (cond
             ((string= tyname "char")
              `(,(cdr (assoc comparison '((< . char<) (> . char>)
                                          (<= . char<=) (>= . char>=))))
                ,x ,y))
             ((string= tyname "string")
              `(if (,(cdr (assoc comparison '((< . string<) (> . string>)
                                              (<= . string<=) (>= . string>=))))
                    ,x ,y)
                   t nil))
             (real-p `(sml-real-comparison ,comparison ,x ,y))
             (int-p `(sml-int-comparison ,comparison ,x ,y))
             (t `(,comparison ,x ,y)))))
        ((and int-p (member name '("+" "-" "*") :test #'string=))
         `(,(cdr (assoc name '(("+" . sml-int-+) ("-" . sml-int--) ("*" . sml-int-*))
                        :test #'string=))
           ,@args))
        ((member name '("+" "-" "*") :test #'string=)
         (destructuring-bind (x y) args
           (wrap `(,(intern name :cl) ,x ,y))))
        ((and real-p (string= name "/"))
         (destructuring-bind (x y) args
           (wrap `(/ ,x ,y))))
        ((string= name "div") `(,(if int-p 'sml-int-div 'sml-integer-div) ,@args))
        ((string= name "mod") `(sml-int-mod ,@args)))))))

(defun compile-overloaded-identifier-value (name tyname)
  "The function value of overloaded identifier NAME at TYNAME, or NIL.  A
binary operator takes a pair, e.g. `op +' in `foldl (op +) 0 xs'."
  (let ((x (gensym "X"))
        (y (gensym "Y"))
        (pair (gensym "PAIR")))
    (if (member name '("~" "abs") :test #'string=)
        (let ((body (compile-overloaded-operation name tyname (list x))))
          (and body `(lambda (,x) ,body)))
        (let ((body (compile-overloaded-operation name tyname (list x y))))
          (and body `(lambda (,pair)
                       (let ((,x (sml-tuple-first ,pair))
                             (,y (sml-tuple-second ,pair)))
                         ,body)))))))

(defun compile-overloaded-application (ast local-exceptions lexical-env)
  "Specialized code for AST, an application of an overloaded identifier, or
NIL when AST is not one or the static checker did not resolve it."
  (when (and (consp ast) (eq (car ast) :app))
    (destructuring-bind (head arg) (cdr ast)
      (flet ((compile-args (name tyname &rest asts)
               ;; Only compile the operands once a specialization exists:
               ;; compiling can hoist helper definitions.
               (if (compile-overloaded-operation
                    name tyname (mapcar (lambda (a) (declare (ignore a)) (gensym))
                                        asts))
                   (mapcar (lambda (a) (compile-expr a local-exceptions lexical-env))
                           asts)
                   (return-from compile-overloaded-application nil))))
        (multiple-value-bind (name tyname) (overloaded-identifier-instance head)
          (when name
            (cond
              ;; Unary application.
              ((and (eq (car head) :var)
                    (member name '("~" "abs") :test #'string=))
               (compile-overloaded-operation name tyname (compile-args name tyname arg)))
              ;; An infix application `x + y' is `op + (x, y)'.
              ((and (consp arg) (eq (car arg) :tuple) (= (length arg) 3))
               (compile-overloaded-operation
                name tyname (compile-args name tyname (second arg) (third arg)))))))))))

(defun builtin-binary-primitive (head lexical-env)
  "When HEAD is the unshadowed built-in binary operator, e.g. `+' or `::',
the two-argument function to call for its application to a pair."
  (when (and (consp head) (member (car head) '(:var :ctor)) (stringp (second head)))
    (let ((primitive (assoc (second head) *sml-binary-primitives* :test #'string=)))
      (when (and primitive
                 (equal (resolved-sml-symbol (second head) lexical-env)
                        (cdr (assoc (second head) *sml-env* :test #'string=))))
        (cdr primitive)))))

(defun compile-equality-application (ast local-exceptions lexical-env)
  "`x = y' or `x <> y' at a type the static checker resolved to one whose
values compare with EQL, EQ or STRING= (SML-EQUALITY-KIND), or NIL."
  (when (and (consp ast) (eq (car ast) :app)
             (consp (second ast)) (eq (car (second ast)) :var)
             (member (second (second ast)) '("=" "<>") :test #'equal)
             (consp (third ast)) (eq (car (third ast)) :tuple)
             (= (length (third ast)) 3)
             (builtin-binary-primitive (second ast) lexical-env))
    (let ((test (case (sml-equality-kind (second ast))
                  (:eql 'eql) (:eq 'eq) (:string 'string=))))
      (when test
        (let ((form `(,test ,(compile-expr (second (third ast)) local-exceptions lexical-env)
                            ,(compile-expr (third (third ast)) local-exceptions lexical-env))))
          (if (string= (second (second ast)) "<>")
              `(not ,form)
              ;; STRING= returns a generalized boolean.
              (if (eq test 'string=) `(if ,form t nil) form)))))))

(defun compile-binary-primitive-application (ast local-exceptions lexical-env)
  "A direct two-argument call for `x op y' (or `op op (x, y)') when OP is an
unshadowed built-in operator: no pair is allocated.  NIL otherwise."
  (when (and (consp ast) (eq (car ast) :app)
             (consp (third ast)) (eq (car (third ast)) :tuple)
             (= (length (third ast)) 3))
    (let ((primitive (builtin-binary-primitive (second ast) lexical-env))
          (x (second (third ast)))
          (y (third (third ast))))
      (when primitive
        (let ((args (list (compile-expr x local-exceptions lexical-env)
                          (compile-expr y local-exceptions lexical-env)))
              (word-operation (cdr (assoc primitive '((sml-+ . +) (sml-- . -) (sml-* . *))))))
          (if (and word-operation
                   (or (and (consp x) (eq (car x) :word))
                       (and (consp y) (eq (car y) :word))))
              ;; Without the checker's resolution, an operand that is a
              ;; word literal still makes this word arithmetic.
              `(ldb (byte +sml-word-size+ 0) (,word-operation ,@args))
              `(,primitive ,@args)))))))

(defun compile-selection (form selector)
  "A form selecting the field of SELECTOR, a (:selector label) node, from the
record FORM evaluates to: SVREF at the field's index when the static checker
gives the record's labels, else a lookup by label."
  (let* ((label (second selector))
         (labels (sml-node-record-labels selector))
         (index (and labels (position label labels :test #'string=))))
    (if index
        `(svref ,form ,(1+ index))
        `(sml-record-ref ,form ,label))))

(defun compile-expr (ast &optional local-exceptions lexical-env)
  "Compiles an SML expression AST into a Common Lisp form."
  (cond
    ((numberp ast) ast)
    ((stringp ast) ast)
    ((characterp ast) ast)

    ((and (listp ast) (eq (car ast) :var))
     (multiple-value-bind (name tyname) (overloaded-identifier-instance ast)
       (or (and name (compile-overloaded-identifier-value name tyname))
           (resolved-sml-symbol (second ast) lexical-env))))

    ((and (listp ast) (eq (car ast) :word))
     (second ast))

    ((and (listp ast) (eq (car ast) :op-var))
     (compile-expr `(:var ,(second ast)) local-exceptions lexical-env))

    ((and (listp ast) (eq (car ast) :ctor))
     (cond
       ((string= (second ast) "true") t)
       ((string= (second ast) "false") nil)
       ((string= (second ast) "nil") nil)
       (t (resolved-sml-symbol (second ast) lexical-env))))

    ((and (listp ast) (eq (car ast) :typed))
     (compile-expr (second ast) local-exceptions lexical-env))

    ((and (listp ast) (eq (car ast) :selector))
     `(lambda (record) ,(compile-selection 'record ast)))

    ((and (listp ast) (eq (car ast) :deref))
     `(funcall #'sml-deref ,(compile-expr (second ast) local-exceptions lexical-env)))

    ((and (listp ast) (eq (car ast) :infix-app))
     `(funcall ,(compile-expr `(:var ,(second ast)) local-exceptions lexical-env)
               (sml-tuple
                     ,(compile-expr (third ast) local-exceptions lexical-env)
                     ,(compile-expr (fourth ast) local-exceptions lexical-env))))

    ;; `~` applied to a word literal is modular negation.
    ((and (listp ast) (eq (car ast) :app)
          (equal (second ast) '(:var "~"))
          (consp (third ast))
          (eq (car (third ast)) :word))
     (logand +sml-word-mask+ (- (second (third ast)))))

    ((compile-overloaded-application ast local-exceptions lexical-env))
    ((compile-equality-application ast local-exceptions lexical-env))
    ((compile-binary-primitive-application ast local-exceptions lexical-env))

    ;; `#lab e'
    ((and (listp ast) (eq (car ast) :app)
          (consp (second ast)) (eq (car (second ast)) :selector))
     (compile-selection (compile-expr (third ast) local-exceptions lexical-env)
                        (second ast)))

    ;; Replace the :app block in compile-expr
    ((and (listp ast) (eq (car ast) :app))
     (let ((head (second ast))
           (arg (third ast)))
	       (if (and (listp head)
	                (eq (car head) :ctor)
	                (not (exception-constructor-info (second head) local-exceptions))
	                (or (lexical-constructor-tag-form (second head) lexical-env)
                            (known-data-constructor-application-p (second head) lexical-env)))
           (let ((tag (lexical-constructor-tag-form (second head) lexical-env)))
             (if tag
                 ;; A constructor of the functor body being compiled.
                 `(cons ,tag ,(compile-expr arg local-exceptions lexical-env))
                 (or (compile-constructor-application
                      (constructor-symbol-for-name (second head) lexical-env)
                      arg local-exceptions lexical-env)
                     `(funcall ,(compile-expr head local-exceptions lexical-env)
                               ,(compile-expr arg local-exceptions lexical-env)))))
           (let ((compiled-head
                   (compile-expr head local-exceptions lexical-env))
                 (compiled-arg
                   (compile-expr arg local-exceptions lexical-env)))
             (if *sml-debug-applications*
                 `(sml-debug-funcall ,compiled-head ,compiled-arg ',head)
                 `(funcall ,compiled-head ,compiled-arg))))))

    ((and (listp ast) (eq (car ast) :case))
     (let ((branches (cddr ast)))
       (cond
         ((literal-case-branches-p branches)
          (compile-literal-case (second ast) branches local-exceptions lexical-env))
         ((tuple-dispatch-case-branches-p branches)
          (compile-tuple-dispatch-case (second ast) branches
                                       local-exceptions lexical-env))
         (t
          `(trivia:match ,(compile-expr (second ast) local-exceptions lexical-env)
             ,@(mapcar (lambda (branch)
                         (compile-trivia-case-clause
                          branch local-exceptions lexical-env))
                       branches)
             (_ ,(compile-match-failure "Match" "case" (first (first branches)))))))))

    ((and (listp ast) (eq (car ast) :handle))
     (let ((branches (third ast))
           (condition-var (gensym "EXN")))
       `(handler-case ,(compile-expr (second ast) local-exceptions lexical-env)
          (sml-raised-exception (,condition-var)
            (let ((value (sml-exception-value ,condition-var)))
              (trivia:match value
                ,@(mapcar (lambda (branch)
	                            (let* ((var-map (pattern-variable-map (first branch)
	                                                                  local-exceptions))
                                   (branch-env (append var-map lexical-env)))
                              `(,(compile-pat (first branch) local-exceptions branch-env)
                                ,(compile-expr (second branch) local-exceptions branch-env))))
                          branches)
                (_ (sml-raise value))))))))

    ;; --- Control Flow & Short-Circuiting ---
    ((and (listp ast) (eq (car ast) :if))
     `(if ,(compile-expr (second ast) local-exceptions lexical-env)
          ,(compile-expr (third ast) local-exceptions lexical-env)
          ,(compile-expr (fourth ast) local-exceptions lexical-env)))

    ((and (listp ast) (eq (car ast) :while))
     `(progn
        (loop while ,(compile-expr (second ast) local-exceptions lexical-env)
              do ,(compile-expr (third ast) local-exceptions lexical-env))
        (sml-unit)))

    ((and (listp ast) (eq (car ast) :raise))
     `(sml-raise ,(compile-expr (second ast) local-exceptions lexical-env)))

    ((and (listp ast) (eq (car ast) :let))
     (compile-local-decls (second ast) (third ast) local-exceptions lexical-env))

    ((and (listp ast) (eq (car ast) :andalso))
     `(and ,(compile-expr (second ast) local-exceptions lexical-env)
           ,(compile-expr (third ast) local-exceptions lexical-env)))

    ((and (listp ast) (eq (car ast) :orelse))
     `(or ,(compile-expr (second ast) local-exceptions lexical-env)
          ,(compile-expr (third ast) local-exceptions lexical-env)))

    ((and (listp ast) (eq (car ast) :seq))
     `(progn ,@(mapcar (lambda (expr)
                         (compile-expr expr local-exceptions lexical-env))
                       (cdr ast))))

    ;; Add this to compile-expr!
    ((and (listp ast) (eq (car ast) :list))
     `(list ,@(mapcar (lambda (expr)
                        (compile-expr expr local-exceptions lexical-env))
                      (cdr ast))))

    ((and (listp ast) (eq (car ast) :record))
     (compile-record-expression (cdr ast) local-exceptions lexical-env))

    ((and (listp ast) (eq (car ast) :unit))
     `(sml-unit))

    ((and (listp ast) (eq (car ast) :tuple))
     `(sml-tuple ,@(mapcar (lambda (expr)
                             (compile-expr expr local-exceptions lexical-env))
                           (cdr ast))))

    ((and (listp ast) (eq (car ast) :self-tail-call))
     (compile-self-tail-call (second ast) (cddr ast) local-exceptions lexical-env))

    ((and (listp ast) (eq (car ast) :fn))
     (let ((clauses (second ast))
           (tmp-arg (gensym "ARG")))
       (sml-lambda-form
        nil tmp-arg
        `(trivia:match ,tmp-arg
           ,@(mapcar (lambda (branch)
                       (let* ((var-map (pattern-variable-map (first branch)
                                                             local-exceptions))
                              (branch-env (append var-map lexical-env)))
                         `(,(compile-pat (first branch) local-exceptions branch-env)
                           ,(compile-expr (second branch) local-exceptions branch-env))))
                     clauses)
           (_ ,(compile-match-failure "Match" "fn" (first (first clauses))))))))

    (t (error "Unknown AST: ~A" ast))))

(defun compile-top-level-val-rec (ast cell local-exceptions lexical-env)
  "Forms that set CELL, already bound, to the value of `val rec' AST."
  `((setf ,cell ,(let ((*sml-binding-symbol-env*
                         (acons (second ast) cell *sml-binding-symbol-env*)))
                   (compile-expr (third ast)
                                 local-exceptions
                                 (acons (second ast) cell lexical-env))))
    ,@(compile-binding-publication
       cell (infer-sml-ast-type (third ast) :package *sml-package*))))

(defun compile-top-level-fun (ast cell local-exceptions lexical-env)
  "Bind CELL to the function `fun' AST declares."
  (let ((fun-type (loop repeat (length (first (first (third ast))))
                        for result = :unknown then `(:fn :unknown ,result)
                        finally (return result))))
    `(progn
       (declaim (special ,cell))
       (defparameter ,cell ,(let ((*sml-binding-symbol-env*
                                    (acons (second ast) cell *sml-binding-symbol-env*)))
                              (compile-fn-clauses (third ast)
                                                  local-exceptions
                                                  (second ast)
                                                  (acons (second ast)
                                                         cell
                                                         lexical-env))))
       ,@(compile-binding-publication cell fun-type))))

(defun compile-decl (ast &optional local-exceptions lexical-env)
  "Compiles top level declarations."
  (cond
    ((eq (car ast) :val)
     (compile-top-level-val (second ast) (third ast) local-exceptions
                            (sml-type-ast->binding-type (getf (cdddr ast) :type))
                            lexical-env))
    ((eq (car ast) :vals)
     ;; `val p1 = e1 and p2 = e2` evaluates every expression before binding
     ;; anything, like `val (p1, p2) = (e1, e2)`.  `val rec` bindings joined
     ;; to them come last.
     (let ((vals (remove-if-not (lambda (val) (eq (car val) :val)) (cdr ast)))
           (recs (remove :val (cdr ast) :key #'car)))
       `(progn
          ,(compile-top-level-val
            `(:pat-tuple ,@(mapcar #'second vals))
            `(:tuple ,@(mapcar #'third vals))
            local-exceptions
            `(:tuple ,@(mapcar (lambda (val-dec)
                                 (or (sml-type-ast->binding-type
                                      (getf (cdddr val-dec) :type))
                                     (infer-sml-ast-type (third val-dec)
                                                         :package *sml-package*)))
                               vals))
            lexical-env)
          ,@(mapcar (lambda (rec) (compile-decl rec local-exceptions lexical-env))
                    recs))))
    ((eq (car ast) :val-rec)
     (let ((cell (new-sml-binding-cell (second ast))))
       `(progn
          (defparameter ,cell nil)
          ,@(compile-top-level-val-rec ast cell local-exceptions lexical-env))))
    ((eq (car ast) :val-recs)
     ;; `val rec f = ... and g = ...': every name is bound in every body.
     (let* ((cells (mapcar (lambda (val-rec) (new-sml-binding-cell (second val-rec)))
                           (cdr ast)))
            (*sml-binding-symbol-env*
              (append (mapcar (lambda (val-rec cell) (cons (second val-rec) cell))
                              (cdr ast) cells)
                      *sml-binding-symbol-env*)))
       `(progn
          ,@(mapcar (lambda (cell) `(defparameter ,cell nil)) cells)
          ,@(mapcan (lambda (val-rec cell)
                      (compile-top-level-val-rec val-rec cell
                                                 local-exceptions lexical-env))
                    (cdr ast) cells))))
    ((eq (car ast) :fun)
     (compile-top-level-fun ast (new-sml-binding-cell (second ast))
                            local-exceptions lexical-env))
    ((eq (car ast) :funs)
     ;; `fun f ... and g ...': every name is bound in every body.
     (let* ((cells (mapcar (lambda (fun-dec) (new-sml-binding-cell (second fun-dec)))
                           (cdr ast)))
            (*sml-binding-symbol-env*
              (append (mapcar (lambda (fun-dec cell) (cons (second fun-dec) cell))
                              (cdr ast) cells)
                      *sml-binding-symbol-env*)))
       `(progn ,@(mapcar (lambda (fun-dec cell)
                           (compile-top-level-fun fun-dec cell
                                                  local-exceptions lexical-env))
                         (cdr ast) cells))))

    ;; Replace the :datatype block in compile-decl
    ((eq (car ast) :datatype)
     (let ((ctors (third ast)))
       (dolist (c ctors)
         (note-sml-plain-binding (current-qualified-sml-name (second c))))
       `(progn
          ,@(mapcar (lambda (c)
                      (let* ((cname (target-sml-symbol (second c)))
                             (keyword (target-sml-symbol (second c)))
                             (has-args (fourth c)))
                        (if has-args
                            ;; If it has args, the name refers to a constructor function
	                            `(progn
                                       ,@(constructor-arity-forms cname (datatype-constructor-flat-arity c))
	                               (defun ,cname (payload)
                                         ,(if (> (datatype-constructor-flat-arity c) 0)
                                              `(sml-construct-flat ',keyword payload)
                                              `(cons ',keyword payload)))
	                               (defparameter ,cname #',cname)
	                               (register-sml-constructor ',cname)
	                               ,(compile-type-registration-form
                                         cname
                                         `(:fn ,(or (sml-type-ast->binding-type
                                                     (getf (cddr c) :arg-type))
                                                    :unknown)
                                               ,(second ast)))
	                               ,(compile-export-form (list cname)))
	                            ;; If no args, it's just the keyword constant
	                            `(progn
                                       ,@(constructor-arity-forms cname 0)
	                               (defparameter ,cname ',keyword)
	                               (register-sml-constructor ',cname)
	                               ,(compile-type-registration-form cname (second ast))
	                               ,(compile-export-form (list cname))))))
                    ctors)
          ,@(mapcar (lambda (binding)
                      (compile-type-alias-form (second binding) (third binding)))
                    (getf (cdddr ast) :withtype))
          ,(compile-export-form nil))))
    ((eq (car ast) :datatype-replication)
     (dolist (name (declaration-direct-bound-names ast))
       (note-sml-plain-binding (current-qualified-sml-name name)))
     `(progn
        ,(compile-type-alias-form (second ast) (third ast))
        ,@(compile-datatype-replication-constructors ast)))
    ((eq (car ast) :type)
     `(progn
        ,(compile-type-alias-form (second ast) (third ast))))
    ((eq (car ast) :infix)
     `(progn))
    ((eq (car ast) :signature)
     (note-signature-declaration ast)
     `(progn))
    ((eq (car ast) :open)
     (compile-open-form (second ast)))
    ((eq (car ast) :structure-alias)
     (dolist (member (lookup-sml-module-members-for-compiler
                      (third ast) *sml-local-structure-members*))
       (note-sml-alias-binding
        (qualify-sml-name (current-qualified-sml-name (second ast)) member)
        (qualify-sml-name (resolve-structure-prefix (third ast)) member)))
     `(alias-sml-structure-alias
       ,(target-sml-package-name)
       ,(current-qualified-sml-name (second ast))
       ,(resolve-structure-prefix (third ast))))
    ((eq (car ast) :structure-app)
     (compile-structure-application ast local-exceptions lexical-env))
    ((eq (car ast) :functor)
     (compile-functor-declaration ast local-exceptions lexical-env))
    ((eq (car ast) :structure)
     (let* ((module-name (current-qualified-sml-name (second ast)))
            (members (structure-declaration-members ast))
            ;; A substructure's name denotes it after its declaration (see
            ;; COMPILE-PROGRAM-DECLS); before, e.g. in its own body, it
            ;; denotes the structure the name had outside.
            (forms (let ((*sml-module-prefix* module-name))
                     (compile-program-decls (third ast)
                                            local-exceptions
                                            lexical-env))))
       `(progn
          ,@forms
          (register-sml-structure-members ,(target-sml-package-name) ,module-name ',members))))
    ((eq (car ast) :expr)
     (compile-expr (second ast) local-exceptions))
    ((eq (car ast) :exception)
     (let* ((name (new-sml-binding-cell (second ast)))
            (arg-type (getf (cddr ast) :arg-type)))
       (if arg-type
           `(progn
              (defparameter ,name (make-sml-exception-function ,(second ast)))
              ,@(compile-binding-publication
                 name `(:fn ,(sml-type-ast->binding-type arg-type) "exn")))
           `(progn
              (defparameter ,name (make-sml-exception-constructor ,(second ast)))
              ,@(compile-binding-publication name "exn")))))
    ((eq (car ast) :exception-alias)
     (compile-top-level-exception-alias (second ast) (third ast) lexical-env))
    ((eq (car ast) :local)
     (let* ((local-decs (second ast))
            (body-decs (third ast))
            (inner-exceptions (append (declarations-exposed-exceptions local-decs)
                                      local-exceptions))
            (module-bindings (declarations-local-module-member-bindings local-decs))
            (local-symbol-bindings
              (declarations-lexical-bindings local-decs))
            (open-symbol-bindings
              (declarations-local-open-symbol-bindings local-decs))
            (open-constructor-bindings
              (declarations-local-open-constructor-bindings local-decs))
            (structure-prefixes (module-local-structure-prefixes
                                 local-decs
                                 (or *sml-module-prefix* ""))))
       (let ((*sml-local-structure-members* (append module-bindings
                                                    *sml-local-structure-members*))
             (*sml-local-structure-prefixes* (append structure-prefixes
                                                    *sml-local-structure-prefixes*)))
         (let ((compiled-body
                 (let ((*sml-binding-symbol-env*
                         (append local-symbol-bindings open-symbol-bindings
                                 *sml-binding-symbol-env*))
                       (*sml-constructor-symbol-env*
                         (append open-constructor-bindings
                                 *sml-constructor-symbol-env*)))
                   (compile-program-decls-body
                    body-decs
                    inner-exceptions
                    (append local-symbol-bindings lexical-env)))))
           (compile-local-decls-into-body
            local-decs
            compiled-body
            local-exceptions
            lexical-env)))))

    (t (error "Unknown Declaration: ~A" ast))))

;;; Functors.  A functor is compiled to a function of the value of its
;;; argument structure (an SML-STRUCTURE) and of the qualified name of the
;;; structure being bound, which returns the value of that structure.  The
;;; body is one lexical scope, like the declarations of a `let': each
;;; application evaluates it afresh, so each instance has refs, exceptions
;;; and datatype constructors of its own, and the closures it makes see that
;;; instance only.  An application at top level binds each member of the
;;; instance in a cell of its own (COMPILE-STRUCTURE-APPLICATION).
;;;
;;; In the body, unqualified names are bound in the lexical environment, and
;;; a structure name denotes a descriptor:
;;;   (:lexical members substructures opened)  a structure of the body:
;;;       MEMBERS maps names to variables (newest first), SUBSTRUCTURES
;;;       names to descriptors, and OPENED lists the descriptors of the
;;;       :value and :global structures it opened, whose members are known
;;;       at run time
;;;   (:value functor-value path)  substructure PATH ("" for the structure
;;;       itself) of a structure known by its value at run time, the
;;;       functor's argument or a functor application in the body; its
;;;       members are bound to variables when the body refers to them
;;;   (:global name)  the structure NAME bound at top level

(defvar *sml-unit-functors* nil
  "While a program is compiled: qualified functor name -> the member specs
(see FUNCTOR-SCOPE-MEMBER-SPECS) of the functors it declared.")

(defun compile-time-functor-member-specs (name)
  (multiple-value-bind (specs presentp)
      (if *sml-unit-functors*
          (gethash name *sml-unit-functors*)
          (values nil nil))
    (if presentp
        specs
        (lookup-sml-functor-member-specs (target-sml-package-name) name))))

(defun structure-application-member-specs (dec)
  "The member specs of the structure (:structure-app name funid strexp
[:sigexp S]) declaration DEC binds, as far as they are known."
  (let ((specs (compile-time-functor-member-specs (resolve-functor-name (third dec))))
        (names (functor-scope-signature-names (getf (cddddr dec) :sigexp) dec)))
    (if (eq names :all)
        specs
        (remove-if-not (lambda (spec) (member (first spec) names :test #'string=))
                       specs))))

(defun structure-application-members (dec)
  (mapcar #'first (structure-application-member-specs dec)))

(defvar *functor-scope-constructors* nil
  "While a functor body is compiled: the variable holding a datatype
constructor -> a form for the tag of its values.")

(defvar *functor-scope-exceptions* nil
  "While a functor body is compiled: the variable holding an exception
constructor -> whether it takes an argument.")

(defvar *functor-scope-types* nil
  "While a functor body is compiled: variable -> binding type.")

(defvar *functor-scope-instance-name* nil
  "While a functor body is compiled: the variable holding the qualified name
of the instance being built.")

(defstruct (functor-value (:constructor make-functor-value (variable)))
  ;; The variable holding the structure value at run time.
  variable
  ;; Qualified member name -> the variable holding the member, for the
  ;; members the body refers to, and their names, the newest first.
  (table (make-hash-table :test #'equal))
  (order nil))

(defun note-functor-scope-variable (variable)
  (when *functor-scope-variables*
    (setf (gethash variable *functor-scope-variables*) t))
  variable)

(defun functor-value-member-variable (value name)
  (or (gethash name (functor-value-table value))
      (let ((variable (note-functor-scope-variable (make-symbol name))))
        (push name (functor-value-order value))
        (setf (gethash name (functor-value-table value)) variable))))

(defun functor-value-bindings (value)
  "LET* bindings of the members of VALUE that the body refers to."
  (loop for name in (reverse (functor-value-order value))
        collect `(,(gethash name (functor-value-table value))
                  (sml-structure-member-or-unresolved
                   ,(functor-value-variable value) ,name))))

(defun functor-value-known-members (value path)
  "The names, relative to substructure PATH, of the members of VALUE known
so far."
  (let ((prefix (if (string= path "") "" (format nil "~A." path))))
    (loop for name in (reverse (functor-value-order value))
          when (and (string-prefix-p prefix name)
                    (> (length name) (length prefix)))
            collect (subseq name (length prefix)))))

(defun functor-scope-constructor-tag (variable)
  (and variable (cdr (assoc variable *functor-scope-constructors*))))

(defun functor-scope-variable-type (variable)
  (or (cdr (assoc variable *functor-scope-types*)) :unknown))

(defun functor-scope-structure (name)
  "The descriptor of the structure that long structure identifier NAME
denotes in the functor body being compiled, or NIL when it denotes a
structure bound at top level that the body does not name otherwise."
  (when *functor-scope-structures*
    (let* ((parts (multiple-value-bind (qualifiers last) (split-sml-long-name name)
                    (append qualifiers (list last))))
           (descriptor (cdr (assoc (first parts) *functor-scope-structures*
                                   :test #'string=))))
      (when descriptor
        (dolist (part (rest parts) descriptor)
          (setf descriptor (functor-scope-substructure descriptor part))
          (unless descriptor
            (return nil)))))))

(defun functor-scope-opened-structure (opened name substructurep)
  "The structure among OPENED (descriptors, the newest first) whose member
NAME (a substructure when SUBSTRUCTUREP) a structure that opened them has."
  (flet ((has-member-p (descriptor)
           (ecase (first descriptor)
             (:global
              (let ((members (lookup-sml-module-members-for-compiler
                              (second descriptor) nil)))
                (if substructurep
                    (let ((prefix (format nil "~A." name)))
                      (some (lambda (member) (string-prefix-p prefix member)) members))
                    (member name members :test #'string=))))
             (:value
              (let ((members (functor-value-known-members (second descriptor)
                                                          (third descriptor))))
                (if substructurep
                    (let ((prefix (format nil "~A." name)))
                      (some (lambda (member) (string-prefix-p prefix member)) members))
                    (member name members :test #'string=))))
             (:lexical
              (assoc name (if substructurep (third descriptor) (second descriptor))
                     :test #'string=)))))
    (or (find-if #'has-member-p opened)
        ;; Members of a structure known by its value need not be known yet.
        (find :value opened :key #'first))))

(defun functor-scope-substructure (descriptor name)
  (ecase (first descriptor)
    (:lexical
     (or (cdr (assoc name (third descriptor) :test #'string=))
         (let ((opened (functor-scope-opened-structure (fourth descriptor) name t)))
           (and opened (functor-scope-substructure opened name)))))
    (:value
     (list :value (second descriptor) (qualify-sml-name (third descriptor) name)))
    (:global
     (list :global (qualify-sml-name (second descriptor) name)))))

(defun global-sml-member-symbol (qualified-name)
  (or (compile-time-sml-env-symbol qualified-name)
      (sml-bound-symbol qualified-name)))

(defun functor-scope-member (descriptor name)
  "The variable (or the symbol, for a structure bound at top level) that
holds member NAME of the structure DESCRIPTOR describes, or NIL."
  (ecase (first descriptor)
    (:lexical
     (or (cdr (assoc name (second descriptor) :test #'string=))
         (let ((opened (functor-scope-opened-structure (fourth descriptor) name nil)))
           (and opened (functor-scope-member opened name)))))
    (:value
     (functor-value-member-variable (second descriptor)
                                    (qualify-sml-name (third descriptor) name)))
    (:global
     (global-sml-member-symbol (qualify-sml-name (second descriptor) name)))))

(defun functor-scope-long-name-symbol (name)
  "When long NAME denotes a member of a structure that the functor body
being compiled names, the variable or symbol that holds it."
  (when (and *functor-scope-structures* (sml-long-name-p name))
    (let* ((dot (position #\. name :from-end t))
           (descriptor (functor-scope-structure (subseq name 0 dot))))
      (when descriptor
        (functor-scope-member descriptor (subseq name (1+ dot)))))))

(defun functor-scope-long-name-variable (name)
  "The variable holding long NAME's binding when the functor body being
compiled binds it."
  (let ((symbol (functor-scope-long-name-symbol name)))
    (and symbol (null (symbol-package symbol)) symbol)))

(defun lexical-constructor-tag-form (name lexical-env)
  "A form for the tag of the values of constructor NAME when it is bound in
the functor body being compiled."
  (functor-scope-constructor-tag
   (or (lexical-symbol-for-name name lexical-env)
       (functor-scope-long-name-variable name))))

(defun functor-scope-exception-info (name)
  (let ((entry (assoc (functor-scope-long-name-variable name) *functor-scope-exceptions*)))
    (and entry (list :payloadp (cdr entry)))))

(defun functor-scope-structure-form (descriptor)
  "A form for the value of the structure DESCRIPTOR describes."
  (ecase (first descriptor)
    (:lexical
     `(make-sml-structure
       (list ,@(loop for (name . variable) in (remove-duplicates (second descriptor)
                                                                 :key #'car
                                                                 :test #'string=
                                                                 :from-end t)
                     collect `(cons ,name ,variable)))
       (list ,@(loop for (name . substructure) in (remove-duplicates (third descriptor)
                                                                     :key #'car
                                                                     :test #'string=
                                                                     :from-end t)
                     collect `(cons ,name ,(functor-scope-structure-form substructure)))
             ,@(loop for opened in (fourth descriptor)
                     collect `(cons "" ,(functor-scope-structure-form opened))))))
    (:value
     (let ((variable (functor-value-variable (second descriptor)))
           (path (third descriptor)))
       (if (string= path "")
           variable
           `(sml-substructure ,variable ,path))))
    (:global
     `(sml-global-structure ,(target-sml-package-name) ,(second descriptor)))))

(defun restrict-structure-value-form (form names)
  "A form for the value of FORM, a structure, restricted to the members
NAMES (or FORM when NAMES is :ALL)."
  (if (eq names :all)
      form
      `(restrict-sml-structure ,form ',names)))

(defun structure-value-form (name)
  "A form for the value of the structure that long identifier NAME denotes."
  (let ((descriptor (functor-scope-structure name)))
    (if descriptor
        (functor-scope-structure-form descriptor)
        `(sml-global-structure ,(target-sml-package-name)
                               ,(resolve-structure-prefix name)))))

(defun functor-scope-member-specs (descriptor &optional (prefix ""))
  "The (name type constructorp) of the members of the structure DESCRIPTOR
describes, as far as they are known at compile time, the names qualified by
PREFIX."
  (remove-duplicates
   (ecase (first descriptor)
     (:lexical
      (append
       (loop for (name . variable) in (second descriptor)
             collect (list (qualify-sml-name prefix name)
                           (if (symbol-package variable)
                               (or (lookup-sml-binding-type variable) :unknown)
                               (functor-scope-variable-type variable))
                           (if (symbol-package variable)
                               (and (sml-constructor-symbol-p variable) t)
                               (and (symbolp (functor-scope-constructor-tag variable))
                                    (functor-scope-constructor-tag variable)
                                    t))))
       (loop for (name . substructure) in (third descriptor)
             append (functor-scope-member-specs substructure
                                                (qualify-sml-name prefix name)))
       (loop for opened in (fourth descriptor)
             append (functor-scope-member-specs opened prefix))))
     (:value
      (loop for name in (functor-value-known-members (second descriptor) (third descriptor))
            for variable = (functor-scope-member descriptor name)
            collect (list (qualify-sml-name prefix name)
                          (functor-scope-variable-type variable)
                          nil)))
     (:global
      (loop for name in (lookup-sml-module-members-for-compiler (second descriptor) nil)
            for symbol = (global-sml-member-symbol
                          (qualify-sml-name (second descriptor) name))
            collect (list (qualify-sml-name prefix name)
                          (or (lookup-sml-binding-type symbol) :unknown)
                          (and (sml-constructor-symbol-p symbol) t)))))
   :key #'first :test #'string= :from-end t))

(defun restrict-functor-scope-structure (descriptor names)
  "The :lexical DESCRIPTOR restricted to NAMES, the members a signature
specifies (see SML-SIGNATURE-EXPORTS), or DESCRIPTOR when NAMES is :ALL.
Members of opened structures that NAMES lists become members of their own."
  (if (eq names :all)
      descriptor
      (destructuring-bind (members substructures opened) (rest descriptor)
        (flet ((member-name-p (name)
                 (member name names :test #'string=))
               (substructure-name-p (name)
                 (let ((prefix (format nil "~A." name)))
                   (some (lambda (export) (string-prefix-p prefix export)) names))))
          (let ((kept-members (remove-if-not #'member-name-p members :key #'car))
                (kept-substructures (remove-if-not #'substructure-name-p substructures
                                                   :key #'car)))
            (when opened
              (dolist (name names)
                (let ((dot (position #\. name)))
                  (if dot
                      (let ((head (subseq name 0 dot)))
                        (unless (assoc head kept-substructures :test #'string=)
                          (let ((substructure (functor-scope-substructure descriptor head)))
                            (when substructure
                              (push (cons head substructure) kept-substructures)))))
                      (unless (assoc name kept-members :test #'string=)
                        (let ((variable (functor-scope-member descriptor name)))
                          (when variable
                            (push (cons name variable) kept-members))))))))
            (list :lexical kept-members kept-substructures nil))))))

(defun functor-scope-signature-names (sigexp &optional dec)
  "The members a structure ascribed SIGEXP (or, when the static checker
ran, the structure declared by DEC) has, or :ALL when that is not known."
  (let ((static (if dec (static-structure-members dec) :unknown)))
    (if (not (eq static :unknown))
        static
        (multiple-value-bind (exports complete) (and sigexp (sml-signature-exports sigexp))
          (if complete exports :all)))))

;;; The scope of the declarations of a functor body.

(defstruct (functor-scope (:copier copy-functor-scope))
  ;; The lexical environment and local exceptions (see COMPILE-LOCAL-DECL).
  env
  exceptions
  ;; Values of *SML-BINDING-SYMBOL-ENV*, *SML-CONSTRUCTOR-SYMBOL-ENV* and
  ;; *FUNCTOR-SCOPE-STRUCTURES* for the declarations.
  symbol-env
  constructor-env
  structures
  ;; The members, substructures and opened structures (see the :LEXICAL
  ;; descriptor) of the structure whose declarations are compiled.
  members
  substructures
  opened
  ;; That structure's qualified name relative to the functor body, or NIL,
  ;; and its qualified name for SML-LISP-SOURCE.
  path
  prefix)

(defmacro updated-functor-scope (scope &rest changes)
  (let ((new (gensym "SCOPE")))
    `(let ((,new (copy-functor-scope ,scope)))
       ,@(loop for (key value) on changes by #'cddr
               collect `(setf (,(intern (concatenate 'string "FUNCTOR-SCOPE-"
                                                     (symbol-name key))
                                        '#:cl-sml)
                               ,new)
                              ,value))
       ,new)))

(defmacro with-functor-scope ((scope) &body body)
  (let ((s (gensym "SCOPE")))
    `(let* ((,s ,scope)
            (*sml-binding-symbol-env* (functor-scope-symbol-env ,s))
            (*sml-constructor-symbol-env* (functor-scope-constructor-env ,s))
            (*functor-scope-structures* (functor-scope-structures ,s))
            (*sml-module-prefix* (functor-scope-prefix ,s)))
       ,@body)))

(defun compile-functor-scope-decls (decs scope continuation)
  "Compile declarations DECS of a functor body in SCOPE around the form
CONTINUATION returns for the scope after them."
  (if (null decs)
      (funcall continuation scope)
      (compile-functor-scope-dec
       (first decs) scope
       (lambda (scope)
         (compile-functor-scope-decls (rest decs) scope continuation)))))

(defun functor-scope-binding-type (dec name)
  (case (car dec)
    (:val (if (and (consp (second dec))
                   (member (car (second dec)) '(:pat-var :var))
                   (string= name (second (second dec))))
              (or (sml-type-ast->binding-type (getf (cdddr dec) :type))
                  (infer-sml-ast-type (third dec) :package *sml-package*))
              :unknown))
    (:fun (loop repeat (length (first (first (third dec))))
                for result = :unknown then `(:fn :unknown ,result)
                finally (return result)))
    (:exception (let ((arg-type (getf (cddr dec) :arg-type)))
                  (if arg-type
                      `(:fn ,(sml-type-ast->binding-type arg-type) "exn")
                      "exn")))
    (:exception-alias
     (let ((target (resolved-sml-symbol (third dec))))
       (if (null (symbol-package target))
           (functor-scope-variable-type target)
           (exception-alias-type target))))
    ((:vals :funs :val-recs)
     (dolist (sub (cdr dec) :unknown)
       (when (member name (declaration-direct-bound-names sub) :test #'string=)
         (return (functor-scope-binding-type sub name)))))
    (otherwise :unknown)))

(defun note-functor-scope-bindings (dec bindings)
  "Record the types of the variables BINDINGS for DEC, and its exceptions."
  (loop for (name . variable) in bindings
        for type = (functor-scope-binding-type dec name)
        do (note-functor-scope-variable variable)
           (push (cons variable type) *functor-scope-types*)
           (when (exception-constructor-type-p type)
             (push (cons variable (payload-exception-type-p type))
                   *functor-scope-exceptions*))))

(defun compile-functor-scope-dec (dec scope continuation)
  (case (car dec)
    ((:val :vals :val-rec :val-recs :fun :funs :exception :exception-alias)
     (let* ((bindings (local-declaration-lexical-bindings dec))
            (rest (make-symbol "REST"))
            (form (with-functor-scope (scope)
                    (note-functor-scope-bindings dec bindings)
                    (note-compiled-sml-declaration
                     dec
                     (compile-local-decl dec rest (functor-scope-exceptions scope)
                                         (functor-scope-env scope))))))
       (subst (funcall continuation
                       (updated-functor-scope
                        scope
                        :env (append bindings (functor-scope-env scope))
                        :members (append bindings (functor-scope-members scope))
                        :exceptions (extend-local-exceptions
                                     (functor-scope-exceptions scope) dec)))
              rest form)))
    (:datatype (compile-functor-scope-datatype dec scope continuation))
    (:datatype-replication
     (compile-functor-scope-datatype-replication dec scope continuation))
    (:type
     `(progn ,(with-functor-scope (scope)
                (note-compiled-sml-declaration
                 dec (compile-type-alias-form (second dec) (third dec))))
             ,(funcall continuation scope)))
    (:open
     (let ((new scope))
       (dolist (name (split-sml-module-names (second dec)))
         (setf new (functor-scope-open new name)))
       (funcall continuation new)))
    (:local (compile-functor-scope-local dec scope continuation))
    (:structure
     (let ((name (second dec)))
       (compile-functor-scope-decls
        (third dec)
        (updated-functor-scope scope
                               :members nil :substructures nil :opened nil
                               :path (qualify-sml-name (functor-scope-path scope) name)
                               :prefix (qualify-sml-name (functor-scope-prefix scope) name))
        (lambda (inner)
          (funcall continuation
                   (functor-scope-bind-structure
                    scope name
                    (restrict-functor-scope-structure
                     (list :lexical (functor-scope-members inner)
                           (functor-scope-substructures inner)
                           (functor-scope-opened inner))
                     (with-functor-scope (scope)
                       (functor-scope-signature-names (getf (cdddr dec) :sigexp)
                                                      dec)))))))))
    (:structure-alias
     (funcall continuation
              (functor-scope-bind-structure
               scope (second dec)
               (with-functor-scope (scope)
                 (or (functor-scope-structure (third dec))
                     (list :global (resolve-structure-prefix (third dec))))))))
    (:structure-app (compile-functor-scope-application dec scope continuation))
    ((:infix :signature) (funcall continuation scope))
    (:expr
     `(progn ,(with-functor-scope (scope)
                (compile-expr (second dec) (functor-scope-exceptions scope)
                              (functor-scope-env scope)))
             ,(funcall continuation scope)))
    (t (error "Unknown declaration in a functor body: ~A" dec))))

(defun functor-scope-bind-structure (scope name descriptor)
  (updated-functor-scope
   scope
   :structures (acons name descriptor (functor-scope-structures scope))
   :substructures (acons name descriptor (functor-scope-substructures scope))))

(defun functor-scope-bind-value (scope name variable)
  (updated-functor-scope
   scope
   :env (acons name variable (functor-scope-env scope))
   :members (acons name variable (functor-scope-members scope))))

(defun compile-functor-scope-datatype (dec scope continuation)
  "Each instance has constructors of its own, whose values are tagged by
symbols named after the instance (SML-INSTANCE-CONSTRUCTOR-TAG)."
  (let ((bindings (local-declaration-lexical-bindings dec))
        (rest (make-symbol "REST"))
        (let-bindings nil))
    (dolist (ctor (third dec))
      (let* ((name (second ctor))
             (variable (note-functor-scope-variable (local-binding-symbol name bindings)))
             (tag `(sml-instance-constructor-tag
                    ,(target-sml-package-name) ,*functor-scope-instance-name*
                    ,(qualify-sml-name (functor-scope-path scope) name))))
        (if (fourth ctor)
            (let ((tag-variable (note-functor-scope-variable
                                 (make-symbol (format nil "~A-TAG" name)))))
              (push `(,tag-variable ,tag) let-bindings)
              (push `(,variable (make-sml-instance-constructor ,tag-variable)) let-bindings)
              (push (cons variable tag-variable) *functor-scope-constructors*)
              (push (cons variable
                          `(:fn ,(or (sml-type-ast->binding-type (getf (cddr ctor) :arg-type))
                                     :unknown)
                                ,(second dec)))
                    *functor-scope-types*))
            (progn
              (push `(,variable ,tag) let-bindings)
              (push (cons variable variable) *functor-scope-constructors*)
              (push (cons variable (second dec)) *functor-scope-types*)))))
    (let ((form (with-functor-scope (scope)
                  (note-compiled-sml-declaration
                   dec
                   `(let* ,(reverse let-bindings)
                      (declare (ignorable ,@(mapcar #'first let-bindings)))
                      ,@(mapcar (lambda (binding)
                                  (compile-type-alias-form (second binding) (third binding)))
                                (getf (cdddr dec) :withtype))
                      ,rest)))))
      (subst (funcall continuation
                      (updated-functor-scope
                       scope
                       :env (append bindings (functor-scope-env scope))
                       :members (append bindings (functor-scope-members scope))))
             rest form))))

(defun compile-functor-scope-datatype-replication (dec scope continuation)
  (let* ((source-type (string-trim '(#\Space #\Tab #\Newline #\;) (third dec)))
         (dot (position #\. source-type :from-end t))
         (source-prefix (and dot (subseq source-type 0 dot)))
         (new scope))
    (loop for (nil . name) in (replicated-constructors dec)
          for source-name = (qualify-sml-name source-prefix name)
          for symbol = (with-functor-scope (new)
                         (resolved-sml-symbol source-name (functor-scope-env new)))
          do (setf new
                   (if (null (symbol-package symbol))
                       (functor-scope-bind-value new name symbol)
                       (let ((canonical (sml-constructor-canonical-symbol symbol)))
                         (updated-functor-scope
                          new
                          :symbol-env (acons name symbol (functor-scope-symbol-env new))
                          :constructor-env (if canonical
                                               (acons name canonical
                                                      (functor-scope-constructor-env new))
                                               (functor-scope-constructor-env new))
                          :members (acons name symbol (functor-scope-members new)))))))
    `(progn ,(with-functor-scope (scope)
               (compile-type-alias-form (second dec) (third dec)))
            ,(funcall continuation new))))

(defun functor-scope-open (scope name)
  "SCOPE after `open NAME'."
  (functor-scope-open-structure
   scope
   (with-functor-scope (scope)
     (or (functor-scope-structure name)
         (list :global (resolve-structure-prefix name))))))

(defun functor-scope-open-structure (scope descriptor)
  "SCOPE after opening the structure DESCRIPTOR describes."
  (ecase (first descriptor)
      (:lexical
       (destructuring-bind (members substructures opened) (rest descriptor)
         (let ((new scope))
           ;; What the structure opened is visible again, then its own
           ;; members.
           (dolist (structure (reverse opened))
             (setf new (functor-scope-open-structure new structure)))
           (updated-functor-scope
            new
            :env (append members (functor-scope-env new))
            :members (append members (functor-scope-members new))
            :structures (append substructures (functor-scope-structures new))
            :substructures (append substructures (functor-scope-substructures new))))))
      (:value
       (let ((new (updated-functor-scope
                   scope :opened (cons descriptor (functor-scope-opened scope))))
             (heads nil))
         (dolist (member (functor-value-known-members (second descriptor) (third descriptor))
                         new)
           (let ((dot (position #\. member)))
             (if dot
                 (let ((head (subseq member 0 dot)))
                   (unless (member head heads :test #'string=)
                     (push head heads)
                     (setf new (functor-scope-bind-structure
                                new head (functor-scope-substructure descriptor head)))))
                 (setf new (functor-scope-bind-value
                            new member (functor-scope-member descriptor member))))))))
      (:global
       (let* ((global-name (second descriptor))
              (open-dec `(:open (,global-name)))
              (symbol-bindings (with-functor-scope (scope)
                                 (declaration-binding-symbol-bindings open-dec)))
              (constructor-bindings (with-functor-scope (scope)
                                      (declaration-constructor-symbol-bindings open-dec))))
         (updated-functor-scope
          scope
          ;; A name bound in the body before is shadowed.
          :env (append (remove-if-not (lambda (binding)
                                        (assoc (car binding) (functor-scope-env scope)
                                               :test #'string=))
                                      symbol-bindings)
                       (functor-scope-env scope))
          :symbol-env (append symbol-bindings (functor-scope-symbol-env scope))
          :constructor-env (append constructor-bindings
                                   (functor-scope-constructor-env scope))
          :opened (cons descriptor (functor-scope-opened scope)))))))

(defun compile-functor-scope-local (dec scope continuation)
  "`local DECS1 in DECS2 end': the bindings of DECS1 are visible in DECS2
only."
  (flet ((hide (final middle outer)
           ;; The bindings made since OUTER, except those made before MIDDLE.
           (append (ldiff final middle) outer)))
    (compile-functor-scope-decls
     (second dec)
     (updated-functor-scope scope :members nil :substructures nil :opened nil)
     (lambda (inner)
       (compile-functor-scope-decls
        (third dec)
        (updated-functor-scope inner
                               :members (functor-scope-members scope)
                               :substructures (functor-scope-substructures scope)
                               :opened (functor-scope-opened scope))
        (lambda (body)
          (funcall continuation
                   (updated-functor-scope
                    body
                    :env (hide (functor-scope-env body) (functor-scope-env inner)
                               (functor-scope-env scope))
                    :exceptions (hide (functor-scope-exceptions body)
                                      (functor-scope-exceptions inner)
                                      (functor-scope-exceptions scope))
                    :symbol-env (hide (functor-scope-symbol-env body)
                                      (functor-scope-symbol-env inner)
                                      (functor-scope-symbol-env scope))
                    :constructor-env (hide (functor-scope-constructor-env body)
                                           (functor-scope-constructor-env inner)
                                           (functor-scope-constructor-env scope))
                    :structures (hide (functor-scope-structures body)
                                      (functor-scope-structures inner)
                                      (functor-scope-structures scope))))))))))

(defun compile-functor-scope-application (dec scope continuation)
  "`structure S = F (A)' in a functor body: S is the value the functor
returns, and its members are bound when the body refers to them."
  (let ((argument (strip-sml-strexp-constraints (fourth dec))))
    (if (not (eq (car argument) :str-id))
        ;; The argument is a structure of its own.
        (compile-functor-scope-dec
         `(:local (,(sml-structure-binding-node "%ARG" nil (fourth dec)))
                  ((:structure-app ,(second dec) ,(third dec) (:str-id "%ARG")
                                   ,@(cddddr dec))))
         scope continuation)
        (let* ((name (second dec))
               (functor-name (resolve-functor-name (third dec)))
               (value (make-functor-value
                       (note-functor-scope-variable (make-symbol name))))
               (argument-form (with-functor-scope (scope)
                                (structure-value-form (second argument)))))
          (dolist (spec (structure-application-member-specs dec))
            (let ((variable (functor-value-member-variable value (first spec))))
              (push (cons variable (second spec)) *functor-scope-types*)
              (when (third spec)
                (push (cons variable `(sml-constructor-value-tag ,variable))
                      *functor-scope-constructors*))))
          (let ((body (funcall continuation
                               (functor-scope-bind-structure scope name
                                                             (list :value value "")))))
            `(let* ((,(functor-value-variable value)
                      ,(restrict-structure-value-form
                        `(apply-sml-functor
                          ,(target-sml-package-name) ,functor-name ,argument-form
                          (format nil "~A.~A" ,*functor-scope-instance-name*
                                  ,(qualify-sml-name (functor-scope-path scope) name)))
                        (with-functor-scope (scope)
                          (functor-scope-signature-names (getf (cddddr dec) :sigexp) dec))))
                    ,@(functor-value-bindings value))
               (declare (ignorable ,(functor-value-variable value)
                                   ,@(mapcar #'first (functor-value-bindings value))))
               ,body))))))

(defun functor-parameter-scope (ast argument lexical-env local-exceptions)
  "The environment, local exceptions and structures of the body of functor
AST whose argument structure is ARGUMENT, a FUNCTOR-VALUE.  The parameter's
members its signature specifies are bound from the start."
  (let* ((param-name (getf (cdddr ast) :param))
         (param-specs (getf (cdddr ast) :param-specs))
         (sigexp (if param-name
                     (getf (cdddr ast) :param-sigexp)
                     `(:sig ,param-specs)))
         (names (and sigexp (sml-signature-exports sigexp)))
         (env lexical-env)
         (exceptions local-exceptions)
         (structures nil))
    (dolist (name names)
      (unless (char= (char name (1- (length name))) #\.)
        (let ((variable (functor-value-member-variable argument name)))
          (when (and (null param-name) (not (position #\. name)))
            (push (cons name variable) env)))))
    (if param-name
        (push (cons param-name (list :value argument "")) structures)
        (dolist (name names)
          (let ((dot (position #\. name)))
            (when dot
              (let ((head (subseq name 0 dot)))
                (unless (assoc head structures :test #'string=)
                  (push (cons head (list :value argument head)) structures)))))))
    ;; The exceptions `funid (spec)' specifies are in scope unqualified.
    (dolist (spec param-specs)
      (when (eq (car spec) :spec-exception)
        (let ((payloadp (and (third spec) t))
              (variable (functor-value-member-variable argument (second spec))))
          (push (cons (second spec) payloadp) exceptions)
          (push (cons variable payloadp) *functor-scope-exceptions*))))
    (values env exceptions structures)))

(defun compile-functor-declaration (ast local-exceptions lexical-env)
  "`functor F (X : S) = strexp': register F's function from the value of
its argument to the value of the structure it builds (see above)."
  (let* ((functor-name (current-qualified-sml-name (second ast)))
         (argument (make-functor-value (make-symbol "ARGUMENT")))
         (instance-name (make-symbol "INSTANCE-NAME"))
         (*functor-scope-constructors* nil)
         (*functor-scope-exceptions* nil)
         (*functor-scope-types* nil)
         (*functor-scope-variables* (make-hash-table :test #'eq))
         (*functor-scope-instance-name* instance-name)
         (specs nil))
    (multiple-value-bind (env exceptions structures)
        (functor-parameter-scope ast argument lexical-env local-exceptions)
      (let* ((scope (make-functor-scope
                     :env env
                     :exceptions exceptions
                     :symbol-env *sml-binding-symbol-env*
                     :constructor-env *sml-constructor-symbol-env*
                     :structures structures
                     :prefix functor-name))
             (body (compile-functor-scope-decls
                    (third ast) scope
                    (lambda (final)
                      (let ((descriptor
                              (restrict-functor-scope-structure
                               (list :lexical (functor-scope-members final)
                                     (functor-scope-substructures final)
                                     (functor-scope-opened final))
                               (with-functor-scope (final)
                                 (functor-scope-signature-names
                                  (getf (cdddr ast) :result-sigexp))))))
                        (setf specs (with-functor-scope (final)
                                      (functor-scope-member-specs descriptor)))
                        (functor-scope-structure-form descriptor)))))
             (argument-bindings (functor-value-bindings argument)))
        (when *sml-unit-functors*
          (setf (gethash functor-name *sml-unit-functors*) specs))
        `(progn
           (register-sml-functor-members ,(target-sml-package-name) ,functor-name ',specs)
           (register-sml-functor-instantiator
            ,(target-sml-package-name)
            ,functor-name
            (lambda (,(functor-value-variable argument) ,instance-name)
              (declare (ignorable ,(functor-value-variable argument) ,instance-name))
              (let* ,argument-bindings
                (declare (ignorable ,@(mapcar #'first argument-bindings)))
                ,body))))))))

(defun compile-structure-application (ast local-exceptions lexical-env)
  "`structure S = F (A)' outside functor bodies: each member of the instance
the functor builds is bound in a fresh cell, except constructors, which are
bound to the package's symbols, and the members known only at run time."
  (let* ((argument-strexp (strip-sml-strexp-constraints (fourth ast)))
         (structure-name (second ast))
         (target-name (current-qualified-sml-name structure-name))
         (functor-name (resolve-functor-name (third ast)))
         ;; A functor application or `let' argument is bound to a structure
         ;; of its own first, as in `F (G (X))'.
         (nested-name (and (member (car argument-strexp) '(:str-app :str-let))
                           (format nil "~A.%ARG" structure-name)))
         (nested-form (and nested-name
                           (compile-decl (sml-structure-binding-node
                                          nested-name nil argument-strexp)
                                         local-exceptions lexical-env)))
         (argument (and (not nested-name)
                        (functor-argument-structure-name argument-strexp)))
         ;; A `struct' argument is compiled as a module of its own.
         (argument-decs (and (not argument) (not nested-name)
                             (sml-strexp-body-decs argument-strexp)))
         (argument-module (and argument-decs
                               (functor-argument-module-name target-name)))
         (argument-members (and argument-decs
                                (declarations-bound-names argument-decs)))
         (argument-forms
           (and argument-decs
                (let ((*sml-module-prefix* argument-module))
                  (compile-program-decls argument-decs local-exceptions lexical-env))))
         (argument-form
           (cond (argument-module
                  `(sml-global-structure ,(target-sml-package-name) ,argument-module))
                 (nested-name
                  `(sml-global-structure ,(target-sml-package-name)
                                         ,(current-qualified-sml-name nested-name)))
                 (t (structure-value-form argument))))
         (specs (structure-application-member-specs ast))
         (exports (functor-scope-signature-names (getf (cddddr ast) :sigexp) ast))
         (value-specs (remove-if #'third specs))
         (instance (make-symbol "INSTANCE")))
    (note-sml-instance-bindings target-name)
    (dolist (spec specs)
      (when (third spec)
        (note-sml-plain-binding (qualify-sml-name target-name (first spec)))))
    (let ((cells (mapcar (lambda (spec)
                           (new-sml-binding-cell
                            (qualify-sml-name structure-name (first spec))))
                         value-specs)))
      `(progn
         ,@(and nested-form (list nested-form))
         ,@argument-forms
         ,@(when argument-module
             `((register-sml-structure-members ,(target-sml-package-name)
                                               ,argument-module
                                               ',argument-members)))
         ,@(when cells
             `((declaim (special ,@cells))))
         (let ((,instance ,(restrict-structure-value-form
                            `(apply-sml-functor ,(target-sml-package-name) ,functor-name
                                                ,argument-form ,target-name)
                            exports)))
           ,@(loop for spec in value-specs
                   for cell in cells
                   collect `(defparameter ,cell
                              (sml-structure-member-or-unresolved ,instance ,(first spec)))
                   append (compile-binding-publication cell (second spec)))
           (bind-sml-functor-instance ,(target-sml-package-name) ,target-name
                                      ,functor-name ,instance
                                      ',(mapcar #'first value-specs)))))))

(defun declaration-local-module-member-bindings (dec)
  (let ((name (and (consp dec) (second dec))))
    (case (and (consp dec) (car dec))
      (:structure
       (let* ((full-name (current-qualified-sml-name name))
              (members (structure-declaration-members dec)))
         (list (cons name members)
               (cons full-name members))))
      (:structure-app
       (let ((full-name (current-qualified-sml-name name))
             (members (structure-application-members dec)))
         (list (cons name members)
               (cons full-name members))))
      (:structure-alias
       (let* ((full-name (current-qualified-sml-name name))
              (source-name (resolve-structure-prefix (third dec)))
              (members (lookup-sml-module-members-for-compiler
                        source-name
                        *sml-local-structure-members*)))
         (list (cons name members)
               (cons full-name members))))
      (otherwise
       nil))))

(defun declarations-local-module-member-bindings (decs)
  (let ((bindings nil)
        (prefixes nil))
    (dolist (dec decs bindings)
      (note-signature-declaration dec)
      (let ((*sml-local-structure-members* (append bindings
                                                   *sml-local-structure-members*))
            (*sml-local-structure-prefixes* (append prefixes
                                                    *sml-local-structure-prefixes*)))
        (setf bindings
              (append (declaration-local-module-member-bindings dec)
                      bindings))
        (setf prefixes
              (append (module-local-structure-prefixes
                       (list dec)
                       (or *sml-module-prefix* ""))
                      prefixes))))))

(defun declaration-binding-symbol-bindings (dec)
  (case (and (consp dec) (car dec))
    (:open
     ;; `open A B` opens B after A, so B's members shadow A's: they are
     ;; pushed last and found first.
     (let ((bindings nil))
       (dolist (name (split-sml-module-names (second dec)) bindings)
         (let ((descriptor (functor-scope-structure name)))
           (case (first descriptor)
             ;; A structure of the functor body being compiled.
             (:lexical
              (dolist (member (reverse (second descriptor)))
                (push member bindings)))
             (:value
              (dolist (member (functor-value-known-members (second descriptor)
                                                           (third descriptor)))
                (push (cons member (functor-scope-member descriptor member))
                      bindings)))
             (t
              (let* ((global-name (if descriptor (second descriptor) name))
                     (source (resolve-structure-prefix global-name)))
                (dolist (member (lookup-sml-module-members-for-compiler
                                 global-name
                                 *sml-local-structure-members*))
                  (push (cons member
                              (sml-bound-symbol (qualify-sml-name source member)))
                        bindings)))))))))
    (:local
     (mapcar (lambda (name)
               (cons name (current-binding-symbol name)))
             (declarations-bound-names (third dec))))
    (otherwise
     (mapcar (lambda (name)
               (cons name (current-binding-symbol name)))
             (declaration-direct-bound-names dec)))))

(defun declaration-constructor-symbol-bindings (dec)
  (case (and (consp dec) (car dec))
    (:datatype
     (mapcar (lambda (ctor)
               (cons (second ctor) (target-sml-symbol (second ctor))))
             (third dec)))
    (:open
     ;; As for values, later structures shadow earlier ones.
     (let ((bindings nil))
       (dolist (name (split-sml-module-names (second dec)) bindings)
         (let ((source (resolve-structure-prefix name)))
           (dolist (member (lookup-sml-module-members-for-compiler
                            name
                            *sml-local-structure-members*))
             (let* ((source-symbol (sml-visible-symbol (qualify-sml-name source member)))
                    (canonical (sml-constructor-canonical-symbol source-symbol)))
               (when canonical
                 (push (cons member canonical) bindings))))))))
    (:local
     (mapcan #'declaration-constructor-symbol-bindings (third dec)))
    (otherwise
     nil)))

(defvar *sml-declaration-compiled-hook* nil
  "When non-NIL, a function called with each declaration-level AST node and
the CL form compiled for it, while the declaration's compile-time
environment (e.g. *SML-MODULE-PREFIX*) is still in effect.")

(defun note-compiled-sml-declaration (dec form)
  (when *sml-declaration-compiled-hook*
    (funcall *sml-declaration-compiled-hook* dec form))
  form)

(defun compile-program-decls (decs &optional (local-exceptions nil) lexical-env)
  (if (null decs)
      nil
      (let* ((dec (first decs))
             (form (note-compiled-sml-declaration
                    dec (compile-decl dec local-exceptions lexical-env)))
             (extended-exceptions (extend-local-exceptions local-exceptions dec))
             (module-bindings (declaration-local-module-member-bindings dec))
             (symbol-bindings (declaration-binding-symbol-bindings dec))
             (constructor-bindings (declaration-constructor-symbol-bindings dec))
             (structure-prefixes (and *sml-module-prefix*
                                      (module-local-structure-prefixes
                                       (list dec) *sml-module-prefix*))))
        (cons form
              (let ((*sml-local-structure-members* (append module-bindings
                                                           *sml-local-structure-members*))
                    (*sml-local-structure-prefixes* (append structure-prefixes
                                                           *sml-local-structure-prefixes*))
                    (*sml-binding-symbol-env* (append symbol-bindings
                                                     *sml-binding-symbol-env*))
                    (*sml-constructor-symbol-env* (append constructor-bindings
                                                          *sml-constructor-symbol-env*)))
                (compile-program-decls (rest decs)
                                       extended-exceptions
                                       lexical-env))))))

(defun compile-program (ast)
  (if (eq (car ast) :program)
      (let ((*sml-unit-cells* (make-hash-table :test #'equal))
            (*sml-unit-cell-counts* (make-hash-table :test #'equal))
            (*sml-unit-functors* (make-hash-table :test #'equal)))
        `(progn ,@(compile-program-decls (cdr ast))))
      (error "Not a program AST.")))
