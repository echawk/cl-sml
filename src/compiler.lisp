(in-package #:cl-sml)

(defun target-sml-package-name ()
  (package-name (ensure-sml-package *sml-package*)))

(defvar *sml-module-prefix* nil)

(defvar *sml-binding-symbol-env* nil)

(defvar *sml-local-structure-prefixes* nil)

(defvar *sml-local-structure-members* nil)

(defvar *sml-compiling-functor* nil)
(defvar *sml-debug-applications* nil)

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
         (let ((found (gethash (list (target-sml-package-name) (second sigexp))
                               *sml-signature-expressions*)))
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
  (let ((fact (first (sml-static-facts-at (sml-ast-position dec) "str"
                                          (second dec)))))
    (if fact
        (mapcar #'cdr (parse-sml-structure-members (third fact)))
        :unknown)))

(defun replicated-constructors (dec)
  "The (status . name) constructors that datatype replication DEC binds,
according to the static checker, or else the parser, or NIL when unknown."
  (let ((fact (first (sml-static-facts-at (sml-ast-position dec) "dtrep"
                                          (second dec)))))
    (if fact
        (parse-sml-structure-members (third fact))
        (getf (cdddr dec) :constructors))))

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
          for source = (if builtin
                           (cdr builtin)
                           (resolved-sml-symbol
                            (if source-prefix
                                (qualify-sml-name source-prefix name)
                                name)))
          for target = (target-sml-symbol name)
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
  (or (lexical-symbol-for-name name lexical-env)
      (compile-time-sml-env-symbol name)
      (let ((resolved-name (if (sml-long-name-p name)
                               (resolve-sml-long-name name)
                               name)))
        (or (compile-time-sml-env-symbol resolved-name)
            (sml-symbol resolved-name)))))

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

(defun compile-functor-member-initializer (name form)
  (if *sml-compiling-functor*
      `(handler-case ,form
         (unbound-variable (condition)
           (make-sml-unresolved-functor-member ,name condition))
         (undefined-function (condition)
           (make-sml-unresolved-functor-member ,name condition)))
      form))

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
        (lookup-sml-functor-members (target-sml-package-name) name)
        (unless (string= resolved-name name)
          (or (lookup-sml-structure-members (target-sml-package-name) resolved-name)
              (lookup-sml-functor-members (target-sml-package-name) resolved-name))))))

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
         (let ((structure-members
                 (lookup-sml-functor-members (target-sml-package-name) (third dec))))
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
  (let ((source (sml-symbol member-name))
        (alias (sml-symbol (format nil "~A.~A" structure-name member-name))))
    `(progn
       (when (boundp ',source)
         (defparameter ,alias (symbol-value ',source))
         (let ((type (lookup-sml-binding-type ',source)))
           (when type
             (register-sml-binding-type ',alias type)))
         ,(compile-export-form (list alias))))))

(defun compile-qualified-structure-alias-form (target-structure source-structure member-name)
  (let ((source (sml-symbol (format nil "~A.~A" source-structure member-name)))
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
          (push `(alias-sml-module-member-to-name
                  ,(target-sml-package-name)
                  ,(current-qualified-sml-name member)
                  ,source
                  ,member)
                forms))))
    `(progn ,@(nreverse forms))))

(defun exception-alias-type (target)
  (or (lookup-sml-binding-type target *sml-package*) "exn"))

(defun compile-top-level-exception-alias (name target)
  (let ((symbol (target-sml-symbol name))
        (target-symbol (resolved-sml-symbol target)))
    `(progn
       (defparameter ,symbol
         (if (boundp ',target-symbol)
             (symbol-value ',target-symbol)
             (make-sml-exception-constructor ,name)))
       ,(compile-type-registration-form symbol (exception-alias-type target))
       ,(compile-export-form (list symbol)))))

(defun record-fields-sorted-by-label (fields)
  (sort (remove :record-rest (copy-list fields))
        #'string<
        :key #'first))

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
           (form (if tuplep
                     `(list :tuple ,@field-forms)
                     `(make-sml-record
                       (list ,@(mapcar (lambda (field form)
                                         `(cons ,(first field) ,form))
                                       layout field-forms))))))
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

(defun compile-hoisted-case-action (ast local-exceptions branch-env)
  (let ((body (compile-expr ast local-exceptions branch-env)))
    (if *sml-hoisting-enabled*
        (let ((name (next-hoisted-sml-function-name))
              (parameters (lexical-environment-symbols branch-env)))
          (push `(defun ,name ,parameters ,body) *sml-hoisted-forms*)
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
              (parameters (lexical-environment-symbols lexical-env)))
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
       (case (second ,value)
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
         (scrutinee (if (= arity 1) (first args) `(list :tuple ,@args)))
         (match
           `(trivia:match ,scrutinee
              ,@(mapcar (lambda (clause body)
                          (let* ((pat (compile-clause-pattern (first clause)))
                                 (var-map (pattern-variable-map pat local-exceptions))
                                 (branch-env (append var-map outer-lexical-env)))
                            `(,(compile-pat pat local-exceptions branch-env)
                              ,(compile-expr body
                                             local-exceptions
                                             branch-env))))
                        clauses bodies)
              (_ ,(compile-match-failure "Match" (or function-name "fn")
                                         (first (first clauses)))))))
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
         (nested (decs)
           (loop for nested-dec in decs
                 append (local-declaration-lexical-bindings nested-dec))))
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
(the memoized lists must not be destructively joined)."
  (loop for dec in decs
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

(defun compile-local-decls-into-body (decs body &optional (local-exceptions nil) lexical-env)
  (if (null decs)
      body
      (let* ((dec (first decs))
             (extended-exceptions (extend-local-exceptions local-exceptions dec))
             (dec-bindings (local-declaration-lexical-bindings dec))
             (open-symbol-bindings (local-open-symbol-bindings dec))
             (open-constructor-bindings (local-open-constructor-bindings dec))
             (wrapped-body
               (let ((*sml-binding-symbol-env*
                       (append open-symbol-bindings *sml-binding-symbol-env*))
                     (*sml-constructor-symbol-env*
                       (append (local-datatype-constructor-bindings dec)
                               open-constructor-bindings
                               *sml-constructor-symbol-env*)))
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
         (body-env (append (declarations-lexical-bindings decs)
                           lexical-env))
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
                      (append open-constructor-bindings *sml-constructor-symbol-env*)))
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
     (let ((vals (remove :val (cdr dec) :key #'car :test-not #'eq))
           (recs (remove :val (cdr dec) :key #'car)))
       (compile-local-val-binding
        `(:pat-tuple ,@(mapcar #'second vals))
        `(list :tuple ,@(mapcar (lambda (val-dec)
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
            (local-bindings (declarations-lexical-bindings
                                    local-decs))
            (body-bindings (declarations-lexical-bindings body-decs))
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
                 (append local-bindings body-bindings lexical-env)))))
       (let ((*sml-local-structure-members* (append module-bindings
                                                    *sml-local-structure-members*))
             (*sml-local-structure-prefixes* (append structure-prefixes
                                                    *sml-local-structure-prefixes*)))
         (compile-local-decls-into-body local-decs inner-body local-exceptions lexical-env))))
    (t
     (error "Unknown decl in let: ~A" dec))))

(defun compile-top-level-val (pat expr &optional local-exceptions declared-type lexical-env)
  (let* ((expr-type (or declared-type
                        (infer-sml-ast-type expr :package *sml-package*)))
         (typed-bindings (pattern-type-bindings pat expr-type)))
    (cond
      ((and (listp pat) (member (car pat) '(:pat-var :var)))
       (let* ((sym (target-sml-symbol (second pat)))
              (declaim-form (compile-type-declaim-form sym expr-type))
              (compiled-expr (compile-functor-member-initializer
                              (second pat)
                              (compile-expr expr local-exceptions lexical-env))))
         `(progn
            ,@(when declaim-form (list declaim-form))
            (defparameter ,sym ,compiled-expr)
            ,(compile-type-registration-form sym expr-type)
            ,(compile-export-form (list sym)))))
      ((eq pat :wild)
       (compile-expr expr local-exceptions lexical-env))
      (t
       (let ((tmp (gensym "MATCHED"))
             (bound-symbols (remove-duplicates (pattern-bound-symbols pat) :test #'eq)))
         `(let ((,tmp ,(compile-functor-member-initializer
                        (format nil "~{~A~^,~}" (pattern-bound-names pat))
                        (compile-expr expr local-exceptions lexical-env))))
            (trivia:match ,tmp
              (,(compile-pat pat local-exceptions lexical-env)
               (progn
                 ;; DEFparameter makes SYM special before evaluating its init
                 ;; form.  Some implementations then resolve the RHS to the
                 ;; global binding instead of the pattern variable.
                 ,@(mapcar (lambda (sym)
                             `(setf (symbol-value ',sym) ,sym))
                           bound-symbols)
                 ,@(mapcar (lambda (binding)
                             (compile-type-registration-form (car binding) (cdr binding)))
                           typed-bindings)
                 ,(compile-export-form bound-symbols)
                 ,tmp))
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

(defun compile-record-pattern (fields local-exceptions lexical-env)
  "A pattern on the record FIELDS.  A flexible pattern (`{1 = x, ...}`) may
also be matched against a tuple."
  (let ((record (gensym "RECORD")))
    `(guard1 ,record
             ,(if (member :record-rest fields)
                  `(sml-record-or-tuple-p ,record)
                  `(sml-record-p ,record))
             ,@(mapcan (lambda (field)
                         `((sml-record-select ,record ,(first field))
                           ,(compile-pat (second field)
                                         local-exceptions
                                         lexical-env)))
                       (record-fields-sorted-by-label fields)))))

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
               (lexical-ctor (lexical-symbol-for-name name lexical-env))
               (known-ctor (known-constructor-symbol-for-name name))
               (global-ctor (or known-ctor
                                (resolved-sml-symbol name lexical-env))))
	          (cond
	            ((and lexical-ctor
	                  (not static-ctor-p)
	                  (not known-ctor)
	                  (not (sml-constructor-symbol-p global-ctor)))
	             lexical-ctor)
	            (lexical-ctor
	             `(guard1 ,it (eql ,it ,lexical-ctor)))
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
                        (ctor (constructor-symbol-for-name name lexical-env))
	                (payload (compile-pat (third pat) local-exceptions lexical-env)))
	           (if (or (known-constructor-symbol-for-name name)
                           (sml-constructor-symbol-p ctor))
	               `(cons ',ctor ,payload)
                       ;; Not resolvable now (e.g. reached through a functor
                       ;; parameter): compare against the run-time constructor.
                       ;; The value may be a nullary constructor (a symbol)
                       ;; of the same datatype, so test for a cons rather
                       ;; than assert one.
                       (let ((it (gensym "CTOR-VALUE")))
                         `(guard1 ,it
                                  (and (consp ,it)
                                       (sml-constructor-tag-p (car ,it) ',ctor))
                                  (cdr ,it) ,payload)))))))

    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     (or (lexical-symbol-for-name (second pat) lexical-env)
         (target-sml-symbol (second pat))))

    ((and (listp pat) (eq (car pat) :pat-unit))
     `(list :tuple))

    ((and (listp pat) (eq (car pat) :pat-tuple))
     `(list :tuple ,@(mapcar (lambda (subpat)
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
           (compile-record-pattern (cdr pat) local-exceptions lexical-env))))

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

(defun compile-overloaded-operation (name tyname args)
  "Specialized CL for overloaded identifier NAME used at type TYNAME and
applied to the compiled ARGS (one for unary, two for binary operators).
Returns NIL for combinations without a specialization."
  (let ((bits (sml-word-type-bits tyname))
        (comparison (cdr (assoc name '(("<" . <) (">" . >)
                                       ("<=" . <=) (">=" . >=))
                                :test #'string=))))
    (flet ((wrap (form)
             (cond (bits `(ldb (byte ,bits 0) ,form))
                   ;; IEEE results (infinities, NaNs) rather than traps.
                   ((string= tyname "real")
                    `(with-portable-float-traps-masked ,form))
                   (t form))))
      (cond
        ((= (length args) 1)
         (let ((x (first args)))
           (cond
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
             ((string= tyname "real")
              `(,(cdr (assoc comparison '((< . sml-real-<) (> . sml-real->)
                                          (<= . sml-real-<=) (>= . sml-real->=))))
                ,x ,y))
             (t `(,comparison ,x ,y)))))
        ((member name '("+" "-" "*") :test #'string=)
         (destructuring-bind (x y) args
           (wrap `(,(intern name :cl) ,x ,y))))
        ((string= name "div") `(sml-int-div ,@args))
        ((string= name "mod") `(sml-int-mod ,@args))))))

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

(defun compile-binary-primitive-application (ast local-exceptions lexical-env)
  "A direct two-argument call for `x op y' (or `op op (x, y)') when OP is an
unshadowed built-in operator: no pair is allocated.  NIL otherwise."
  (when (and (consp ast) (eq (car ast) :app)
             (consp (third ast)) (eq (car (third ast)) :tuple)
             (= (length (third ast)) 3))
    (let ((primitive (builtin-binary-primitive (second ast) lexical-env)))
      (when primitive
        `(,primitive ,(compile-expr (second (third ast)) local-exceptions lexical-env)
                     ,(compile-expr (third (third ast)) local-exceptions lexical-env))))))

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
     `(lambda (record)
        (sml-record-select record ,(second ast))))

    ((and (listp ast) (eq (car ast) :deref))
     `(funcall #'sml-deref ,(compile-expr (second ast) local-exceptions lexical-env)))

    ((and (listp ast) (eq (car ast) :infix-app))
     `(funcall ,(compile-expr `(:var ,(second ast)) local-exceptions lexical-env)
               (list :tuple
                     ,(compile-expr (third ast) local-exceptions lexical-env)
                     ,(compile-expr (fourth ast) local-exceptions lexical-env))))

    ;; `~` applied to a word literal is modular negation.
    ((and (listp ast) (eq (car ast) :app)
          (equal (second ast) '(:var "~"))
          (consp (third ast))
          (eq (car (third ast)) :word))
     (logand +sml-word-mask+ (- (second (third ast)))))

    ((compile-overloaded-application ast local-exceptions lexical-env))
    ((compile-binary-primitive-application ast local-exceptions lexical-env))

    ;; Replace the :app block in compile-expr
    ((and (listp ast) (eq (car ast) :app))
     (let ((head (second ast))
           (arg (third ast)))
	       (if (and (listp head)
	                (eq (car head) :ctor)
	                (not (exception-constructor-info (second head) local-exceptions))
	                (known-data-constructor-application-p (second head) lexical-env))
	           `(cons ',(constructor-symbol-for-name (second head) lexical-env)
	                  ,(compile-expr arg local-exceptions lexical-env))
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
        (list :tuple)))

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
     `(list :tuple))

    ((and (listp ast) (eq (car ast) :tuple))
     `(list :tuple ,@(mapcar (lambda (expr)
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
     (let ((vals (remove :val (cdr ast) :key #'car :test-not #'eq))
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
     (let ((name (target-sml-symbol (second ast))))
       `(progn
          (declaim (special ,name))
          (defparameter ,name nil)
          (setf ,name ,(let ((*sml-binding-symbol-env*
                               (acons (second ast) name *sml-binding-symbol-env*)))
                          (compile-expr (third ast)
                                        local-exceptions
                                        (acons (second ast) name lexical-env))))
          ,(compile-type-registration-form name (infer-sml-ast-type (third ast) :package *sml-package*))
          ,(compile-export-form (list name)))))
    ((eq (car ast) :val-recs)
     ;; `val rec f = ... and g = ...': every name is bound in every body.
     (let ((*sml-binding-symbol-env*
             (append (mapcar (lambda (val-rec)
                               (cons (second val-rec) (target-sml-symbol (second val-rec))))
                             (cdr ast))
                     *sml-binding-symbol-env*)))
       `(progn
          ,@(mapcar (lambda (val-rec)
                      `(defparameter ,(target-sml-symbol (second val-rec)) nil))
                    (cdr ast))
          ,@(mapcar (lambda (val-rec)
                      (compile-decl val-rec local-exceptions lexical-env))
                    (cdr ast)))))
    ((eq (car ast) :fun)
     (let* ((name (target-sml-symbol (second ast)))
            (fun-type (loop repeat (length (first (first (third ast))))
                            for result = :unknown then `(:fn :unknown ,result)
                            finally (return result))))
	       `(progn
	          (declaim (special ,name))
	          (defparameter ,name ,(let ((*sml-binding-symbol-env*
	                                       (acons (second ast) name *sml-binding-symbol-env*)))
	                                  (compile-fn-clauses (third ast)
	                                                       local-exceptions
	                                                       (second ast)
	                                                       (acons (second ast)
	                                                              name
	                                                              lexical-env))))
	          ,(compile-type-registration-form name fun-type)
	          ,(compile-export-form (list name)))))
    ((eq (car ast) :funs)
     (let ((*sml-binding-symbol-env*
             (append (mapcar (lambda (fun-dec)
                               (cons (second fun-dec)
                                     (target-sml-symbol (second fun-dec))))
                             (cdr ast))
                     *sml-binding-symbol-env*)))
       `(progn ,@(mapcar (lambda (fun-dec)
                           (compile-decl fun-dec local-exceptions lexical-env))
                         (cdr ast)))))

    ;; Replace the :datatype block in compile-decl
    ((eq (car ast) :datatype)
     (let ((ctors (third ast)))
       `(progn
          ,@(mapcar (lambda (c)
                      (let* ((cname (target-sml-symbol (second c)))
                             (keyword (target-sml-symbol (second c)))
                             (has-args (fourth c)))
                        (if has-args
                            ;; If it has args, the name refers to a constructor function
	                            `(progn
	                               (defun ,cname (payload) (cons ',keyword payload))
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
     `(alias-sml-structure-alias
       ,(target-sml-package-name)
       ,(current-qualified-sml-name (second ast))
       ,(resolve-structure-prefix (third ast))))
	    ((eq (car ast) :structure-app)
	     (let* ((argument-strexp (strip-sml-strexp-constraints (fourth ast)))
	            (target-name (current-qualified-sml-name (second ast)))
	            ;; A functor application or `let' argument is bound to a
	            ;; structure of its own first, as in `F (G (X))'.
	            (nested-name (and (member (car argument-strexp) '(:str-app :str-let))
	                              (format nil "~A.%ARG" (second ast))))
	            (nested-form (and nested-name
	                              (compile-decl (sml-structure-binding-node
	                                             nested-name nil argument-strexp)
	                                            local-exceptions lexical-env)))
	            (argument (and (not nested-name)
	                           (functor-argument-structure-name argument-strexp)))
	            ;; A `struct' argument is compiled as a module of its own.
	            (argument-decs (and (not argument) (not nested-name)
	                                (sml-strexp-body-decs argument-strexp)))
	            (argument-module
	              (and argument-decs
	                   (functor-argument-module-name target-name)))
	            (argument-members
	              (and argument-decs
	                   (declarations-bound-names argument-decs)))
	            (argument-prefixes
	              (and argument-decs
	                   (module-local-structure-prefixes argument-decs
	                                                    argument-module)))
	            (argument-forms
	              (and argument-decs
	                   (let ((*sml-module-prefix* argument-module)
	                         (*sml-local-structure-prefixes*
	                           (append argument-prefixes
	                                   *sml-local-structure-prefixes*)))
	                     (compile-program-decls argument-decs
	                                            local-exceptions
	                                            lexical-env)))))
	       `(progn
	          ,@(and nested-form (list nested-form))
	          ,@argument-forms
	          ,@(when argument-module
	              `((register-sml-structure-members
	                 ,(target-sml-package-name)
	                 ,argument-module
	                 ',argument-members)))
	          (alias-sml-functor-application
	           ,(target-sml-package-name)
	           ,target-name
	           ,(resolve-functor-name (third ast))
	           :argument ,(or argument-module
	                          (and nested-name (current-qualified-sml-name nested-name))
	                          (and argument
	                               (resolve-structure-prefix argument)))))))
	    ((eq (car ast) :functor)
	     (let* ((module-name (current-qualified-sml-name (second ast)))
	            (param-name (getf (cdddr ast) :param))
	            ;; The parameter's members, as its signature specifies them,
	            ;; so that the body can `open' it.
	            (*sml-local-structure-members*
	              (let ((param-sigexp (getf (cdddr ast) :param-sigexp)))
	                (multiple-value-bind (exports complete)
	                    (and param-name param-sigexp (sml-signature-exports param-sigexp))
	                  (if complete
	                      (acons param-name exports *sml-local-structure-members*)
	                      *sml-local-structure-members*))))
	            (members (restrict-structure-members-to-signature
	                      (declarations-bound-names (third ast))
	                      (getf (cdddr ast) :result-sigexp)))
	            (structure-prefixes (module-local-structure-prefixes (third ast) module-name))
	            (forms (let ((*sml-module-prefix* module-name)
                         (*sml-compiling-functor* t)
                         (*sml-local-structure-prefixes* (append structure-prefixes
                                                                 *sml-local-structure-prefixes*)))
	                     (compile-program-decls (third ast)
	                                            local-exceptions
	                                            lexical-env))))
	       `(progn
	          (register-sml-functor-members
	           ,(target-sml-package-name)
	           ,module-name
	           ',members
	           ,param-name)
	          (register-sml-functor-instantiator
	           ,(target-sml-package-name)
	           ,module-name
	           (lambda () ,@forms)))))
    ((eq (car ast) :structure)
     (let* ((module-name (current-qualified-sml-name (second ast)))
            (members (structure-declaration-members ast))
            (structure-prefixes (module-local-structure-prefixes (third ast) module-name))
            (forms (let ((*sml-module-prefix* module-name)
                         (*sml-local-structure-prefixes* (append structure-prefixes
                                                                 *sml-local-structure-prefixes*)))
                     (compile-program-decls (third ast)
                                            local-exceptions
                                            lexical-env))))
       `(progn
          ,@forms
          (register-sml-structure-members ,(target-sml-package-name) ,module-name ',members))))
    ((eq (car ast) :expr)
     (compile-expr (second ast) local-exceptions))
    ((eq (car ast) :exception)
     (let* ((name (target-sml-symbol (second ast)))
            (arg-type (getf (cddr ast) :arg-type)))
       (if arg-type
           `(progn
              (defparameter ,name (make-sml-exception-function ,(second ast)))
              ,(compile-type-registration-form
                name `(:fn ,(sml-type-ast->binding-type arg-type) "exn"))
              ,(compile-export-form (list name)))
           `(progn
              (defparameter ,name (make-sml-exception-constructor ,(second ast)))
              ,(compile-type-registration-form name "exn")
              ,(compile-export-form (list name))))))
    ((eq (car ast) :exception-alias)
     (compile-top-level-exception-alias (second ast) (third ast)))
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

(defun declaration-local-module-member-bindings (dec)
  (let ((name (and (consp dec) (second dec))))
    (case (and (consp dec) (car dec))
      (:structure
       (let* ((full-name (current-qualified-sml-name name))
              (members (structure-declaration-members dec)))
         (list (cons name members)
               (cons full-name members))))
      (:structure-app
       (let* ((full-name (current-qualified-sml-name name))
              (source-name (resolve-functor-name (third dec)))
              (members (lookup-sml-functor-members (target-sml-package-name)
                                                   source-name)))
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
         (let ((source (resolve-structure-prefix name)))
           (dolist (member (lookup-sml-module-members-for-compiler
                            name
                            *sml-local-structure-members*))
             (push (cons member
                         (sml-symbol (qualify-sml-name source member)))
                   bindings))))))
    (:local
     (mapcar (lambda (name)
               (cons name (target-sml-symbol name)))
             (declarations-bound-names (third dec))))
    (otherwise
     (mapcar (lambda (name)
               (cons name (target-sml-symbol name)))
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
             (let* ((source-symbol (sml-symbol (qualify-sml-name source member)))
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
             (constructor-bindings (declaration-constructor-symbol-bindings dec)))
        (cons form
              (let ((*sml-local-structure-members* (append module-bindings
                                                           *sml-local-structure-members*))
                    (*sml-binding-symbol-env* (append symbol-bindings
                                                     *sml-binding-symbol-env*))
                    (*sml-constructor-symbol-env* (append constructor-bindings
                                                          *sml-constructor-symbol-env*)))
                (compile-program-decls (rest decs)
                                       extended-exceptions
                                       lexical-env))))))

(defun compile-program (ast)
  (if (eq (car ast) :program)
      `(progn ,@(compile-program-decls (cdr ast)))
      (error "Not a program AST.")))
