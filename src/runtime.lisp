(in-package #:cl-sml)

(defun string-prefix-p (prefix string)
  (let ((prefix-length (length prefix)))
    (and (<= prefix-length (length string))
         (string= prefix string :end1 prefix-length :end2 prefix-length))))

(defun sanitize-sml-package-fragment (string)
  (let ((upper (string-upcase string)))
    (with-output-to-string (out)
      (loop for ch across upper
            do (write-char (if (or (alphanumericp ch) (char= ch #\-))
                               ch
                               #\-)
                           out)))))

(defun sml-package-name-p (name)
  (or (string= name "SML-USER")
      (string-prefix-p "SML." name)))

(defun ensure-sml-package (designator)
  (let* ((name (etypecase designator
                 (package (package-name designator))
                 (string (string-upcase designator))
                 (symbol (string-upcase (symbol-name designator)))))
         (existing (find-package name))
         (package (or existing
                      (make-package name :use '("COMMON-LISP")))))
    (when (and (null existing) (fboundp 'initialize-sml-package))
      (initialize-sml-package package))
    package))

;;; The Basis Library (src/basis/) is compiled once into the package
;;; SML.BASIS (src/basis.lisp).  Every other SML package sees its bindings:
;;; a name that a package does not bind itself denotes the basis's binding
;;; of that name, if there is one.  Packages do not inherit the basis's
;;; symbols (USE-PACKAGE), so that a program's own bindings, which set the
;;; package's symbols, can never change the basis.

(defparameter *sml-basis-package-name* "SML.BASIS")

(defun sml-basis-package ()
  (find-package *sml-basis-package-name*))

(defun sml-name-bound-p (symbol)
  "Does SYMBOL, a package's symbol for an SML name, hold a binding?"
  (or (boundp symbol) (get symbol 'sml-binding-cell)))

(defun sml-basis-symbol (name package)
  "The basis's symbol for NAME when PACKAGE sees the basis and the basis
binds NAME, else NIL."
  (let ((basis (sml-basis-package)))
    (when (and basis (not (eq basis package)))
      (let ((symbol (find-symbol name basis)))
        (and symbol
             (eq (symbol-package symbol) basis)
             (sml-name-bound-p symbol)
             symbol)))))

(defun sml-visible-symbol (name &optional (package *sml-package*))
  "The symbol holding the binding of NAME that PACKAGE sees: PACKAGE's own,
or else the basis's."
  (let* ((package (ensure-sml-package package))
         (symbol (sml-symbol name package)))
    (if (sml-name-bound-p symbol)
        symbol
        (or (sml-basis-symbol (symbol-name symbol) package) symbol))))

(defun sml-basis-package-key (package-name)
  "The registry key of the basis package when PACKAGE-NAME (a key of the
module registries below) sees the basis, else NIL."
  (let ((basis (string-upcase *sml-basis-package-name*)))
    (unless (string= (string-upcase (string package-name)) basis)
      basis)))

(defun current-sml-package-name (&optional (package *package*))
  (let ((name (package-name (find-package package))))
    (cond
      ((string= name "COMMON-LISP-USER") "SML-USER")
      ((string= name "CL-USER") "SML-USER")
      ((sml-package-name-p name) name)
      (t (format nil "SML.~A" (sanitize-sml-package-fragment name))))))

(defun current-sml-package (&optional (package *package*))
  (ensure-sml-package (current-sml-package-name package)))

(defun pathname->sml-package-name (pathname)
  (let* ((pn (pathname pathname))
         (directory (pathname-directory pn))
         (segments (append (when (listp directory) (rest directory))
                           (list (or (pathname-name pn) "ANONYMOUS")))))
    (format nil "SML.FILE~{.~A~}"
            (mapcar (lambda (segment)
                      (sanitize-sml-package-fragment (princ-to-string segment)))
                    segments))))

(defparameter *sml-package* (ensure-sml-package "SML-USER"))

(defparameter *sml-current-directory* nil)

(defvar *sml-type-checker* :native
  "The static checker that compilation, file loading, the REPL and #@sml
blocks run before generating code: :NATIVE (the native checker,
src/types/), :HAMLET (HaMLet, with the system cl-sml/hamlet-oracle
loaded), NIL (none: plain mode),
a checker object, or a function of the source text and file name.  See
src/type-checker.lisp.")

(define-condition sml-static-type-error (error)
  ((source :initarg :source :reader sml-static-type-error-source)
   (filename :initarg :filename :initform nil
             :reader sml-static-type-error-filename)
   (cause :initarg :cause :reader sml-static-type-error-cause))
  (:report (lambda (condition stream)
             (format stream "SML static elaboration failed~@[ for ~A~]: ~A"
                     (sml-static-type-error-filename condition)
                     (sml-static-type-error-cause condition)))))

(defgeneric type-check-sml-string-using (checker source &key filename))

(defmethod type-check-sml-string-using ((checker t) source &key filename)
  (declare (ignore source filename))
  (error "Unsupported SML type checker: ~S" checker))

(defun type-check-sml-string (source &key (checker *sml-type-checker*) filename)
  "Run CHECKER on SOURCE, preserving checker-specific session state."
  (when checker
    (if (functionp checker)
        (funcall checker source filename)
        (type-check-sml-string-using (resolve-sml-type-checker checker) source
                                     :filename filename))))

(defmacro with-sml-type-checker ((checker) &body body)
  `(let ((*sml-type-checker* ,checker))
     ,@body))

;;; Threads.  cl-sml runs SML on one thread; the optional system cl-sml/cml
;;; (Concurrent ML, docs/DESIGN-CML.md) runs it on several.  That system
;;; sets the two hooks below, and makes the global hash tables written at
;;; run time synchronized; while they are NIL (cl-sml/cml not loaded) the
;;; run time takes no lock, and checking them is all it costs.

(defvar *sml-runtime-lock-hook* nil
  "NIL, or a function that calls a thunk holding the run time's (recursive)
lock, which guards the interning of record and tuple shapes and of the
initial exceptions.")

(defvar *sml-stream-lock-hook* nil
  "NIL, or a function of a Lisp stream and a thunk that calls the thunk
holding that stream's lock.  Every TextIO and BinIO primitive and `print'
take it, so that threads do not interleave inside one operation.")

(defmacro with-sml-runtime-lock (() &body body)
  (let ((thunk (gensym "THUNK"))
        (hook (gensym "HOOK")))
    `(flet ((,thunk () ,@body))
       (declare (dynamic-extent #',thunk))
       (let ((,hook *sml-runtime-lock-hook*))
         (if ,hook (funcall ,hook #',thunk) (,thunk))))))

(defmacro with-sml-stream-lock ((stream) &body body)
  (let ((thunk (gensym "THUNK"))
        (hook (gensym "HOOK")))
    `(flet ((,thunk () ,@body))
       (declare (dynamic-extent #',thunk))
       (let ((,hook *sml-stream-lock-hook*))
         (if ,hook (funcall ,hook ,stream #',thunk) (,thunk))))))

(defparameter *sml-binding-types* (make-hash-table :test #'eq))

(defparameter *sml-constructor-symbols* (make-hash-table :test #'eq))

(defparameter *sml-type-aliases* (make-hash-table :test #'equal))

(defparameter *sml-structure-members* (make-hash-table :test #'equal))

(defparameter *sml-functor-members* (make-hash-table :test #'equal))

(defparameter *sml-functor-instantiators* (make-hash-table :test #'equal))

(defparameter *sml-exception-function-tags* (make-hash-table :test #'eq))

(defun sml-module-key (package-name module-name)
  (list (string-upcase (string package-name)) module-name))

(defun normalize-sml-member-names (member-names)
  (remove-duplicates (remove nil member-names) :test #'string=))

(defun register-sml-structure-members (package-name structure-name member-names)
  (setf (gethash (sml-module-key package-name structure-name) *sml-structure-members*)
        (normalize-sml-member-names member-names)))

(defun projected-sml-structure-members (package-name structure-name)
  (loop for separator = (position #\. structure-name :from-end t)
          then (and separator
                    (position #\. structure-name :from-end t
                              :end separator))
        while separator
        for ancestor = (subseq structure-name 0 separator)
        for nested-prefix = (format nil "~A." (subseq structure-name
                                                       (1+ separator)))
        for ancestor-members =
          (gethash (sml-module-key package-name ancestor)
                   *sml-structure-members*)
        for projected =
          (loop for member in ancestor-members
                when (string-prefix-p nested-prefix member)
                  collect (subseq member (length nested-prefix)))
        when projected
          return (normalize-sml-member-names projected)))

(defun lookup-sml-structure-members (package-name structure-name)
  (copy-list
   (or (gethash (sml-module-key package-name structure-name)
                *sml-structure-members*)
       (projected-sml-structure-members package-name structure-name)
       (let ((basis (sml-basis-package-key package-name)))
         (and basis (lookup-sml-structure-members basis structure-name))))))

(defun register-sml-functor-members (package-name functor-name member-specs)
  "Record the members of the structures FUNCTOR-NAME builds, as far as they
are known: MEMBER-SPECS are (name type constructorp) lists (see
FUNCTOR-SCOPE-MEMBER-SPECS)."
  (setf (gethash (sml-module-key package-name functor-name) *sml-functor-members*)
        (remove-duplicates (remove nil member-specs) :key #'first :test #'string=
                                                     :from-end t)))

(defun lookup-sml-functor-member-specs (package-name functor-name)
  (copy-list (gethash (sml-module-key package-name functor-name) *sml-functor-members*)))

(defun register-sml-functor-instantiator (package-name functor-name instantiator)
  (setf (gethash (sml-module-key package-name functor-name)
                 *sml-functor-instantiators*)
        instantiator))

(defun lookup-sml-functor-instantiator (package-name functor-name)
  (gethash (sml-module-key package-name functor-name)
           *sml-functor-instantiators*))

(defvar *sml-constructor-function-tags*
  (make-portable-weak-eq-hash-table)
  "Maps a constructor's function value to the tag its values carry.")

(defun register-sml-constructor (symbol &optional canonical-symbol)
  ;; A constructor is bound to the package's symbol itself, not to a cell
  ;; (see PUBLISH-SML-BINDING).
  (forget-sml-binding-cell symbol)
  (let ((canonical (or canonical-symbol symbol)))
    (setf (gethash symbol *sml-constructor-symbols*) canonical)
    (when (and (boundp symbol) (functionp (symbol-value symbol)))
      (setf (gethash (symbol-value symbol) *sml-constructor-function-tags*)
            canonical)))
  symbol)

(defun sml-constructor-function-p (value)
  (and (functionp value)
       (nth-value 1 (gethash value *sml-constructor-function-tags*))))

(defun sml-unqualified-name (symbol)
  (let* ((name (symbol-name symbol))
         (dot (position #\. name :from-end t)))
    (if dot (subseq name (1+ dot)) name)))

(defun sml-constructor-tag-p (tag constructor-symbol)
  "Does TAG belong to the constructor that CONSTRUCTOR-SYMBOL denotes?  Used by
patterns whose constructor could not be resolved at compile time, such as one
reached through a functor parameter (`Token.LrTable.T`).  If the alias is not
bound when the pattern runs, the unqualified names are compared: in a
well-typed program only values of the pattern's datatype reach it, and
constructor names are unique within a datatype."
  (and (symbolp tag)
       (or (eq tag constructor-symbol)
           (eq tag (sml-constructor-canonical-symbol constructor-symbol))
           (if (boundp constructor-symbol)
               (let ((value (symbol-value constructor-symbol)))
                 (if (functionp value)
                     (eq tag (gethash value *sml-constructor-function-tags*))
                     ;; A nullary constructor's value is its tag.
                     (eq tag value)))
               (string= (sml-unqualified-name tag)
                        (sml-unqualified-name constructor-symbol))))))

(defun sml-constructor-symbol-p (symbol)
  (gethash symbol *sml-constructor-symbols*))

(defun sml-constructor-canonical-symbol (symbol)
  (gethash symbol *sml-constructor-symbols*))

(defun sml-symbol-in-package-name (name package-name)
  (let* ((pkg (ensure-sml-package package-name))
         (symbol-name (string name)))
    (multiple-value-bind (symbol status) (find-symbol symbol-name pkg)
      (cond
        ((eq status :inherited)
         (shadow symbol-name pkg)
         (intern symbol-name pkg))
        (symbol symbol)
        (t (intern symbol-name pkg))))))

(defun alias-sml-module-member (package-name target-module source-module member-name)
  (alias-sml-module-member-to-name package-name
                                   (format nil "~A.~A" target-module member-name)
                                   source-module
                                   member-name))

(defun alias-sml-module-member-to-name (package-name target-name source-module member-name)
  (let* ((source (sml-visible-symbol (format nil "~A.~A" source-module member-name)
                                     package-name))
         (target (sml-symbol-in-package-name target-name package-name)))
    (when (boundp source)
      (proclaim `(special ,target))
      (setf (symbol-value target) (symbol-value source))
      (let ((type (lookup-sml-binding-type source)))
        (when type
          (register-sml-binding-type target type)))
      ;; TARGET now denotes SOURCE's binding (and its cell, if it has one).
      (let ((cell (sml-binding-cell source)))
        (if cell
            (setf (get target 'sml-binding-cell) cell)
            (forget-sml-binding-cell target)))
      (when (sml-constructor-symbol-p source)
        (register-sml-constructor target
                                  (sml-constructor-canonical-symbol source)))
      (export (list target) (ensure-sml-package package-name)))))

(defun alias-sml-constructor (target source)
  "Make constructor symbol TARGET denote the constructor SOURCE denotes."
  (forget-sml-binding-cell target)
  (proclaim `(special ,target))
  (setf (symbol-value target) (symbol-value source))
  (let ((type (lookup-sml-binding-type source)))
    (when type
      (register-sml-binding-type target type)))
  (when (sml-constructor-symbol-p source)
    (register-sml-constructor target (sml-constructor-canonical-symbol source)))
  (export (list target) (symbol-package target))
  target)

;;; Structure values.  A functor is a function from the value of its
;;; argument structure to the value of the structure it builds (see
;;; COMPILE-FUNCTOR-DECLARATION): an SML-STRUCTURE, which maps member names
;;; such as "x" or "Sub.y" to values.  A structure bound at top level is
;;; passed by name; its members are looked up when the functor needs them.

(defstruct (sml-structure (:constructor %make-sml-structure))
  ;; EQUAL hash table: member name -> value.
  (members (make-hash-table :test #'equal))
  ;; (name . SML-STRUCTURE) for each substructure known by its value; the
  ;; name "" stands for a structure whose members are all members of this
  ;; one (`open' in the structure's body).
  substructures
  ;; For a structure bound at top level: (package-name . qualified-name).
  source)

(defun make-sml-structure (members &optional substructures)
  "The structure value with MEMBERS, an alist from member names to values
(the first binding of a name wins), and SUBSTRUCTURES (see SML-STRUCTURE)."
  (let ((structure (%make-sml-structure :substructures substructures)))
    (loop with table = (sml-structure-members structure)
          for (name . value) in members
          unless (nth-value 1 (gethash name table))
            do (setf (gethash name table) value))
    structure))

(defun sml-global-structure (package-name structure-name)
  "The value of the structure STRUCTURE-NAME bound at top level of PACKAGE-NAME."
  (%make-sml-structure :source (cons package-name structure-name)))

(defun sml-structure-member (structure name)
  "The value of member NAME (e.g. \"x\" or \"Sub.y\") of STRUCTURE and
whether it has one; for a member of a structure bound at top level, the
package symbol holding it is the third value."
  (multiple-value-bind (value presentp) (gethash name (sml-structure-members structure))
    (when presentp
      (return-from sml-structure-member (values value t))))
  (loop for (prefix . substructure) in (sml-structure-substructures structure)
        do (multiple-value-bind (value presentp symbol)
               (cond ((string= prefix "")
                      (sml-structure-member substructure name))
                     ((and (> (length name) (length prefix))
                           (string-prefix-p prefix name)
                           (char= (char name (length prefix)) #\.))
                      (sml-structure-member substructure
                                            (subseq name (1+ (length prefix))))))
             (when presentp
               (return-from sml-structure-member (values value t symbol)))))
  (let ((source (sml-structure-source structure)))
    (when source
      (let ((symbol (sml-visible-symbol (format nil "~A.~A" (cdr source) name)
                                        (car source))))
        (when (boundp symbol)
          (return-from sml-structure-member
            (values (symbol-value symbol) t symbol))))))
  (values nil nil))

(defun sml-structure-member-or-unresolved (structure name)
  "The value of member NAME of STRUCTURE, or a function that reports it
missing when called."
  (multiple-value-bind (value presentp) (sml-structure-member structure name)
    (if presentp
        value
        (make-sml-unresolved-functor-member name))))

(defun sml-structure-member-names (structure)
  (let ((names nil))
    (maphash (lambda (name value)
               (declare (ignore value))
               (push name names))
             (sml-structure-members structure))
    (loop for (prefix . substructure) in (sml-structure-substructures structure)
          do (dolist (name (sml-structure-member-names substructure))
               (push (if (string= prefix "") name (format nil "~A.~A" prefix name))
                     names)))
    (let ((source (sml-structure-source structure)))
      (when source
        (setf names (append (reverse (lookup-sml-structure-members (car source)
                                                                   (cdr source)))
                            names))))
    (normalize-sml-member-names (nreverse names))))

(defun sml-substructure (structure name)
  "The value of substructure NAME (e.g. \"Sub\" or \"Sub.Deep\") of STRUCTURE."
  (let ((source (sml-structure-source structure)))
    (if source
        (sml-global-structure (car source) (format nil "~A.~A" (cdr source) name))
        (let ((prefix (format nil "~A." name))
              (members nil))
          (maphash (lambda (member value)
                     (when (string-prefix-p prefix member)
                       (push (cons (subseq member (length prefix)) value) members)))
                   (sml-structure-members structure))
          (make-sml-structure
           members
           (loop for (sub-prefix . substructure) in (sml-structure-substructures structure)
                 if (string= sub-prefix name)
                   collect (cons "" substructure)
                 else if (string-prefix-p prefix sub-prefix)
                        collect (cons (subseq sub-prefix (length prefix)) substructure)
                 ;; NAME lies inside this substructure (NAME "A.B", SUB-PREFIX "A").
                 else if (and (string/= sub-prefix "")
                              (string-prefix-p (format nil "~A." sub-prefix) name))
                        collect (cons "" (sml-substructure
                                          substructure
                                          (subseq name (1+ (length sub-prefix)))))
                 else if (string= sub-prefix "")
                        collect (cons "" (sml-substructure substructure name))))))))

(defun restrict-sml-structure (structure names)
  "STRUCTURE with only the members NAMES (a signature ascribed to it)."
  (make-sml-structure
   (loop for name in names
         for (value presentp) = (multiple-value-list (sml-structure-member structure name))
         when presentp
           collect (cons name value))))

(defun apply-sml-functor (package-name functor-name argument instance-name)
  "The structure that functor FUNCTOR-NAME builds from the structure value
ARGUMENT.  INSTANCE-NAME, the qualified name of the structure being bound,
names the instance's datatype constructors."
  (let ((instantiator (lookup-sml-functor-instantiator package-name functor-name)))
    (unless instantiator
      (error "Unbound SML functor ~A" functor-name))
    (funcall instantiator argument instance-name)))

(defun sml-instance-constructor-tag (package-name instance-name name)
  "The tag of the values of datatype constructor NAME (qualified by the
structures it is declared in, relative to the functor body) of the functor
instance INSTANCE-NAME: each instance has constructors of its own."
  (let ((tag (sml-symbol-in-package-name (format nil "~A.~A" instance-name name)
                                         package-name)))
    ;; Their values are (tag . argument) (SML-CONSTRUCTOR-ARITY).
    (setf (get tag 'sml-constructor-arity) 0)
    tag))

(defun make-sml-instance-constructor (tag)
  "The function that builds the values of a functor instance's datatype
constructor whose values TAG tags."
  (let ((constructor (lambda (payload) (cons tag payload))))
    (setf (gethash constructor *sml-constructor-function-tags*) tag)
    constructor))

(defun sml-constructor-value-tag (value)
  "The tag of the values of the constructor whose value is VALUE (a
nullary constructor's value is its tag)."
  (if (functionp value)
      (gethash value *sml-constructor-function-tags*)
      value))

(defun bind-sml-structure-member (package-name name value &optional source)
  "Bind qualified NAME, a member of a functor instance bound at top level,
to VALUE (the value of SOURCE, a package symbol, when there is one)."
  (let ((target (sml-symbol-in-package-name name package-name)))
    (proclaim `(special ,target))
    (setf (symbol-value target) value)
    (forget-sml-binding-cell target)
    (let ((type (and source (lookup-sml-binding-type source))))
      (cond
        (type (register-sml-binding-type target type))
        ((sml-exception-tag-p value) (register-sml-binding-type target "exn"))
        ((and (functionp value) (gethash value *sml-exception-function-tags*))
         (register-sml-binding-type target '(:fn :unknown "exn")))))
    (cond
      ((and source (sml-constructor-symbol-p source))
       (register-sml-constructor target (sml-constructor-canonical-symbol source)))
      ((sml-constructor-function-p value)
       (register-sml-constructor target (gethash value *sml-constructor-function-tags*)))
      ((and value (eq value target))
       ;; A nullary constructor of the instance (SML-INSTANCE-CONSTRUCTOR-TAG).
       (register-sml-constructor target))
      ((and value (symbolp value) (sml-constructor-symbol-p value))
       (register-sml-constructor target (sml-constructor-canonical-symbol value))))
    (export (list target) (ensure-sml-package package-name))
    target))

(defun bind-sml-functor-instance (package-name structure-name functor-name
                                  instance &optional bound-names)
  "Bind the members of INSTANCE, the value of structure STRUCTURE-NAME built
by functor FUNCTOR-NAME, that the compiled code did not bind itself
(BOUND-NAMES), and record the structure's members."
  (let ((names (sml-structure-member-names instance)))
    (dolist (name names)
      (multiple-value-bind (value presentp source) (sml-structure-member instance name)
        (when presentp
          (if (member name bound-names :test #'string=)
              ;; Bound in a cell; a constructor is bound to the package's
              ;; symbol as well (see REGISTER-SML-CONSTRUCTOR).
              (when (or (sml-constructor-function-p value)
                        (and value (symbolp value) (sml-constructor-symbol-p value))
                        (and source (sml-constructor-symbol-p source)))
                (bind-sml-structure-member package-name
                                           (format nil "~A.~A" structure-name name)
                                           value source))
              (bind-sml-structure-member package-name
                                         (format nil "~A.~A" structure-name name)
                                         value source)))))
    (register-sml-structure-members package-name structure-name names)
    structure-name))

(defun alias-sml-structure-alias (package-name target-structure source-structure)
  (let ((members (lookup-sml-structure-members package-name source-structure)))
    (dolist (member members)
      (alias-sml-module-member package-name target-structure source-structure member))
    (register-sml-structure-members package-name target-structure members)
    target-structure))

(defstruct (sml-exception-tag
            (:constructor %make-sml-exception-tag (name)))
  name)

(defun make-sml-exception-constructor (name)
  (%make-sml-exception-tag name))

(defun make-sml-exception-function (name)
  (let* ((tag (make-sml-exception-constructor name))
         (constructor (lambda (payload)
                        (cons tag payload))))
    (setf (gethash constructor *sml-exception-function-tags*) tag)
    constructor))

(defun sml-exception-constructor-tag (constructor)
  (cond
    ((sml-exception-tag-p constructor) constructor)
    ((functionp constructor)
     (or (gethash constructor *sml-exception-function-tags*)
         (error "Unknown SML exception constructor function: ~S" constructor)))
    ((and (consp constructor) (sml-exception-tag-p (car constructor)))
     (car constructor))
    (t
     (error "Not an SML exception constructor or value: ~S" constructor))))

(defun sml-exception-p (value)
  (or (sml-exception-tag-p value)
      (and (consp value)
           (sml-exception-tag-p (car value)))))

(defun sml-exception-payload (value)
  (and (consp value)
       (sml-exception-tag-p (car value))
       (cdr value)))

(defun sml-exception-name (value)
  (sml-exception-tag-name (sml-exception-constructor-tag value)))

(defparameter *sml-builtin-type-env*
  '(("+" . (:fn (:tuple "int" "int") "int"))
    ("~" . (:fn "int" "int"))
    ("-" . (:fn (:tuple "int" "int") "int"))
    ("*" . (:fn (:tuple "int" "int") "int"))
    ("/" . (:fn (:tuple "real" "real") "real"))
    ("div" . (:fn (:tuple "int" "int") "int"))
    ("mod" . (:fn (:tuple "int" "int") "int"))
    ("^" . (:fn (:tuple "string" "string") "string"))
    ("@" . (:fn (:tuple (:list :unknown) (:list :unknown)) (:list :unknown)))
    ("=" . (:fn (:tuple :unknown :unknown) "bool"))
    ("<>" . (:fn (:tuple :unknown :unknown) "bool"))
    (">" . (:fn (:tuple "int" "int") "bool"))
    ("<" . (:fn (:tuple "int" "int") "bool"))
    (">=" . (:fn (:tuple "int" "int") "bool"))
    ("<=" . (:fn (:tuple "int" "int") "bool"))
    ("::" . (:fn (:tuple :unknown (:list :unknown)) (:list :unknown)))
    (":=" . (:fn (:tuple "ref" :unknown) "unit"))
    ("!" . (:fn "ref" :unknown))
    ("ref" . (:fn :unknown "ref"))
    ("hd" . (:fn (:list :unknown) :unknown))
    ("tl" . (:fn (:list :unknown) (:list :unknown)))
    ("length" . (:fn (:list :unknown) "int"))
    ("null" . (:fn (:list :unknown) "bool"))
    ("rev" . (:fn (:list :unknown) (:list :unknown)))
    ("map" . (:fn (:fn :unknown :unknown) (:fn (:list :unknown) (:list :unknown))))
    ("app" . (:fn (:fn :unknown "unit") (:fn (:list :unknown) "unit")))
    ("foldl" . (:fn (:fn (:tuple :unknown :unknown) :unknown) (:fn :unknown (:fn (:list :unknown) :unknown))))
    ("foldr" . (:fn (:fn (:tuple :unknown :unknown) :unknown) (:fn :unknown (:fn (:list :unknown) :unknown))))
    ("concat" . (:fn (:list "string") "string"))
    ("size" . (:fn "string" "int"))
    ("explode" . (:fn "string" (:list "char")))
    ("implode" . (:fn (:list "char") "string"))
    ("ord" . (:fn "char" "int"))
    ("chr" . (:fn "int" "char"))
    ("str" . (:fn "char" "string"))
    ("abs" . (:fn "int" "int"))
    ("floor" . (:fn "real" "int"))
    ("ceil" . (:fn "real" "int"))
    ("round" . (:fn "real" "int"))
    ("trunc" . (:fn "real" "int"))
    ("sqrt" . (:fn "real" "real"))
    ("sin" . (:fn "real" "real"))
    ("cos" . (:fn "real" "real"))
    ("arctan" . (:fn "real" "real"))
    ("exp" . (:fn "real" "real"))
    ("ln" . (:fn "real" "real"))
    ("real" . (:fn "int" "real"))
    ("o" . (:fn (:tuple (:fn :unknown :unknown) (:fn :unknown :unknown)) (:fn :unknown :unknown)))
    ("before" . (:fn (:tuple :unknown :unknown) :unknown))
    ("ignore" . (:fn :unknown "unit"))
    ("not" . (:fn "bool" "bool"))
    ("print" . (:fn :unknown "unit"))
    ("use" . (:fn "string" "unit"))
    ("Math.pi" . "real")
    ("true" . "bool")
    ("false" . "bool")
    ("nil" . (:list :unknown))))

(defun sml-symbol (name &optional (package *sml-package*))
  (let* ((pkg (ensure-sml-package package))
         (symbol-name (string name)))
    (multiple-value-bind (symbol status) (find-symbol symbol-name pkg)
      (cond
        ((eq status :inherited)
         (shadow symbol-name pkg)
         (intern symbol-name pkg))
        (symbol symbol)
        (t (intern symbol-name pkg))))))

;;; Top-level bindings.  Each top-level binding of a value (`val', `fun',
;;; `val rec', `exception'), structure members included, is held in a special
;;; variable of its own, its cell: |x;1| for the first binding of `x' in a
;;; package, |x;2| for the next one, and so on.  Code refers to the cell of
;;; the binding in scope where it was compiled, so a later binding of the
;;; same name does not change it.  The package's symbol for the name, |x|,
;;; holds the value of the newest binding, for Common Lisp (SML-VALUE,
;;; CALL-SML, the REPL) and for run-time aliasing (`open', functor
;;; application), and records that binding's cell, which compilation units
;;; compiled later refer to.

(defun sml-binding-cell (symbol)
  "The cell holding the newest binding of SYMBOL, a package's symbol for an
SML name, or NIL when SYMBOL itself holds it (or nothing is bound)."
  (get symbol 'sml-binding-cell))

(defun forget-sml-binding-cell (symbol)
  "SYMBOL itself now holds its newest binding."
  (remprop symbol 'sml-binding-cell))

(defun sml-cell-index (cell symbol)
  "K when CELL is |name;K|, a cell for SYMBOL's name, else 0."
  (let ((prefix (concatenate 'string (symbol-name symbol) ";")))
    (or (and cell
             (string-prefix-p prefix (symbol-name cell))
             (parse-integer (symbol-name cell) :start (length prefix)
                                               :junk-allowed t))
        0)))

(defun published-sml-cell-count (symbol)
  "The highest K of the cells |name;K| published for SYMBOL's name."
  (get symbol 'sml-cell-count 0))

(defun publish-sml-binding (symbol cell &optional (type nil typep))
  "Make the binding held in CELL, of TYPE, the newest binding of SYMBOL, the
package's symbol for its SML name."
  (proclaim `(special ,symbol))
  (setf (symbol-value symbol) (symbol-value cell)
        (get symbol 'sml-binding-cell) cell
        (get symbol 'sml-cell-count) (max (published-sml-cell-count symbol)
                                          (sml-cell-index cell symbol)))
  (when typep
    (register-sml-binding-type symbol type)
    (register-sml-binding-type cell type))
  (export (list symbol) (symbol-package symbol))
  symbol)

(defun sml-value (name &optional (package *sml-package*))
  "Return the SML value named NAME in PACKAGE (or in the basis, which every
package sees), or signal an unbound error."
  (let ((symbol (sml-visible-symbol name package)))
    (if (boundp symbol)
        (symbol-value symbol)
        (error "Unbound SML value ~A in package ~A"
               name (package-name (ensure-sml-package package))))))

(defun (setf sml-value) (value name &optional (package *sml-package*))
  (let* ((symbol (sml-symbol name package))
         (cell (sml-binding-cell symbol)))
    ;; Code compiled against the binding reads its cell.
    (when cell
      (setf (symbol-value cell) value))
    (setf (symbol-value symbol) value)))

(defun sml-function (name &optional (package *sml-package*))
  "Return the unary function stored in the SML value namespace."
  (let ((value (sml-value name package)))
    (if (functionp value)
        value
        (error "SML value ~A in package ~A is not a function"
               name (package-name (ensure-sml-package package))))))

(defmacro with-sml-package ((package) &body body)
  `(let ((*sml-package* (ensure-sml-package ,package)))
     ,@body))

(defun call-sml (name &rest arguments)
  "Apply curried SML function NAME to ARGUMENTS in the current SML package."
  (reduce (lambda (function argument)
            (funcall function argument))
          arguments
          :initial-value (sml-function name *sml-package*)))

(declaim (notinline invoke-sml-case-action))
(defun invoke-sml-case-action (thunk)
  (funcall thunk))

(defun export-sml-symbols (symbols &optional (package *sml-package*))
  (when symbols
    (export symbols (ensure-sml-package package)))
  symbols)

(defun register-sml-binding-type (symbol type)
  (setf (gethash symbol *sml-binding-types*) type))

(defun register-sml-type-alias (package name target)
  (setf (gethash (list (package-name (ensure-sml-package package)) name)
                 *sml-type-aliases*)
        target))

(defun lookup-sml-type-alias (name &optional (package *sml-package*))
  (let ((package (ensure-sml-package package)))
    (multiple-value-bind (target presentp)
        (gethash (list (package-name package) name) *sml-type-aliases*)
      (if (or presentp (eq package (sml-basis-package)))
          target
          (gethash (list *sml-basis-package-name* name) *sml-type-aliases*)))))

(defun lookup-sml-binding-type (symbol-or-name &optional (package *sml-package*))
  (etypecase symbol-or-name
    (symbol (gethash symbol-or-name *sml-binding-types*))
    (string (or (cdr (assoc symbol-or-name *sml-builtin-type-env* :test #'string=))
                (let ((symbol (find-symbol symbol-or-name
                                           (package-name (ensure-sml-package package)))))
                  (and symbol (gethash symbol *sml-binding-types*)))
                (let ((symbol (sml-basis-symbol symbol-or-name
                                                (ensure-sml-package package))))
                  (and symbol (gethash symbol *sml-binding-types*)))))))

(defun sml-type->cl-type (type)
  (cond
    ((null type) t)
    ((stringp type)
     (cond
       ((string= type "int") 'integer)
       ((string= type "real") 'double-float)
       ((string= type "string") 'string)
       ((string= type "char") 'character)
       ((string= type "bool") 'boolean)
       ((string= type "unit") 'list)
       ((string= type "ref") 'vector)
       ((string= type "exn") t)
       (t t)))
    ((and (consp type) (eq (car type) :list))
     'list)
    ((and (consp type) (eq (car type) :tuple))
     'list)
    ((and (consp type) (eq (car type) :record))
     'list)
    ((and (consp type) (eq (car type) :fn))
     'function)
    (t t)))

(defun unify-sml-types (left right)
  (cond
    ((equal left right) left)
    ((eq left :unknown) right)
    ((eq right :unknown) left)
    (t :unknown)))

(defun infer-sml-list-type (elements &optional (package *sml-package*))
  (let ((element-type :unknown))
    (dolist (element elements (list :list element-type))
      (setf element-type
            (unify-sml-types element-type
                             (infer-sml-ast-type element :package package))))))

(defun infer-sml-record-type (fields &optional (package *sml-package*))
  (cons :record
        (sort-sml-record-fields
         (mapcar (lambda (field)
                   (cons (first field)
                         (infer-sml-ast-type (second field) :package package)))
                 fields))))

(defun infer-sml-ast-type (ast &key (package *sml-package*))
  (cond
    ((integerp ast) "int")
    ((typep ast 'double-float) "real")
    ((stringp ast) "string")
    ((characterp ast) "char")
    ((and (listp ast) (eq (car ast) :unit)) "unit")
    ((and (listp ast) (eq (car ast) :tuple))
     (cons :tuple (mapcar (lambda (item)
                            (infer-sml-ast-type item :package package))
                          (cdr ast))))
    ((and (listp ast) (eq (car ast) :record))
     (infer-sml-record-type (cdr ast) package))
    ((and (listp ast) (eq (car ast) :list))
     (infer-sml-list-type (cdr ast) package))
    ((and (listp ast) (eq (car ast) :typed))
     (third ast))
    ((and (listp ast) (member (car ast) '(:var :ctor) :test #'eq))
     (or (lookup-sml-binding-type (second ast) package) :unknown))
    ((and (listp ast) (eq (car ast) :selector))
     `(:fn (:record ((,(second ast) . :unknown))) :unknown))
    ((and (listp ast) (eq (car ast) :raise))
     :unknown)
    ((and (listp ast) (eq (car ast) :handle))
     (reduce #'unify-sml-types
             (cons (infer-sml-ast-type (second ast) :package package)
                   (mapcar (lambda (branch)
                             (infer-sml-ast-type (second branch) :package package))
                           (third ast)))
             :initial-value :unknown))
    ((and (listp ast) (eq (car ast) :if))
     (unify-sml-types (infer-sml-ast-type (third ast) :package package)
                      (infer-sml-ast-type (fourth ast) :package package)))
    ((and (listp ast) (eq (car ast) :seq))
     (infer-sml-ast-type (car (last ast)) :package package))
    ((and (listp ast) (eq (car ast) :let))
     (infer-sml-ast-type (car (last (third ast))) :package package))
    ((and (listp ast) (eq (car ast) :fn))
     '(:fn :unknown :unknown))
    ((and (listp ast) (eq (car ast) :case))
     (let ((branch-types
             (mapcar (lambda (branch)
                       (infer-sml-ast-type (second branch) :package package))
                     (cddr ast))))
       (reduce #'unify-sml-types branch-types :initial-value :unknown)))
    ((and (listp ast) (eq (car ast) :app))
     (let ((head (second ast))
           (head-type (infer-sml-ast-type (second ast) :package package)))
       (cond
         ((and (listp head) (eq (car head) :selector))
          (let ((arg-type (infer-sml-ast-type (third ast) :package package)))
            (if (and (consp arg-type) (eq (car arg-type) :record))
                (or (cdr (assoc (second head) (cdr arg-type) :test #'string=))
                    :unknown)
                :unknown)))
         ((and (consp head-type) (eq (car head-type) :fn))
          (third head-type))
         (t
          :unknown))))
    ((and (listp ast) (member (car ast) '(:andalso :orelse) :test #'eq))
     "bool")
    (t :unknown)))

(defun sml-type->string (type)
  (cond
    ((null type) "<unknown>")
    ((eq type :unknown) "<unknown>")
    ((stringp type) type)
    ((and (consp type) (eq (car type) :list))
     (format nil "~A list" (sml-type->string (second type))))
    ((and (consp type) (eq (car type) :tuple))
     (format nil "(~{~A~^ * ~})" (mapcar #'sml-type->string (cdr type))))
    ((and (consp type) (eq (car type) :record))
     (format nil "{~{~A~^, ~}}"
             (mapcar (lambda (field)
                       (format nil "~A: ~A"
                               (car field)
                               (sml-type->string (cdr field))))
                     (cdr type))))
    ((and (consp type) (eq (car type) :fn))
     (format nil "~A -> ~A"
             (sml-type->string (second type))
             (sml-type->string (third type))))
    (t (princ-to-string type))))

;;; Records and tuples (docs/ROADMAP.md items 10 and 15).  A record is a
;;; simple-vector whose slot 0 is its shape, an SML-RECORD-SHAPE naming its
;;; labels, and whose slots 1, ..., n hold its fields in canonical label
;;; order: numeric labels in numeric order, then the others in STRING<
;;; order.  A tuple is the record with labels 1, ..., n, so field i of a
;;; tuple is at index i, and `{1 = a, 2 = b}' is the same value as `(a, b)'.
;;; Shapes are interned, so values of one record type share their shape.
;;; The unit is the record with no fields, one object (SML-UNIT).
;;;
;;; The shape lets code that does not know a record's type find a field by
;;; its label (SML-RECORD-REF): plain mode does so for `#lab' and flexible
;;; record patterns, whose full label set it does not know.  With the native
;;; checker, and for tuples and non-flexible record patterns, the compiler
;;; computes the index and uses SVREF.

(defstruct (sml-record-shape (:constructor %make-sml-record-shape (labels))
                             (:copier nil))
  (labels #() :type simple-vector :read-only t))

(defmethod print-object ((shape sml-record-shape) stream)
  (print-unreadable-object (shape stream :type t)
    (format stream "~{~A~^ ~}" (coerce (sml-record-shape-labels shape) 'list))))

(defun sml-numeric-label-p (label)
  (and (plusp (length label))
       (every #'digit-char-p label)
       (char/= (char label 0) #\0)))

(defun sml-label< (a b)
  "SML's canonical order of record labels: numeric labels first, in numeric
order, then alphanumeric ones."
  (let ((na (sml-numeric-label-p a))
        (nb (sml-numeric-label-p b)))
    (cond ((and na nb) (< (parse-integer a) (parse-integer b)))
          (na t)
          (nb nil)
          (t (string< a b)))))

(defun sml-label-string (label)
  (if (integerp label) (princ-to-string label) (string label)))

(defun sort-sml-record-fields (fields)
  "FIELDS, an alist from labels, in canonical label order."
  (sort (copy-list fields) #'sml-label< :key #'car))

(defvar *sml-record-shapes* (make-hash-table :test #'equal)
  "Interned record shapes, by their list of labels.")

(defun sml-record-shape (labels)
  "The interned shape of the records with LABELS (strings, any order)."
  (let ((labels (sort (mapcar #'sml-label-string labels) #'sml-label<)))
    (with-sml-runtime-lock ()
      (or (gethash labels *sml-record-shapes*)
          (setf (gethash labels *sml-record-shapes*)
                (%make-sml-record-shape (coerce labels 'simple-vector)))))))

(defmethod make-load-form ((shape sml-record-shape) &optional environment)
  (declare (ignore environment))
  `(sml-record-shape ',(coerce (sml-record-shape-labels shape) 'list)))

(defvar *sml-tuple-shapes* (make-array 16 :initial-element nil)
  "The tuple shapes made so far, by size.  Growing it replaces the vector,
so that a thread reading it without the lock sees the old or the new one.")

(defun sml-tuple-shape (n)
  "The shape of N-tuples."
  (let ((shapes *sml-tuple-shapes*))
    (or (and (< n (length shapes)) (svref shapes n))
        (with-sml-runtime-lock ()
          (let ((shapes *sml-tuple-shapes*))
            (when (>= n (length shapes))
              (setf shapes (replace (make-array (* 2 (1+ n)) :initial-element nil)
                                    shapes)
                    *sml-tuple-shapes* shapes))
            (or (svref shapes n)
                (setf (svref shapes n)
                      (sml-record-shape (loop for i from 1 to n collect i)))))))))

(defvar *sml-unit* (vector (sml-tuple-shape 0)))

(declaim (inline sml-unit))
(defun sml-unit ()
  "The unit value, ()."
  *sml-unit*)

(defun sml-tuple (&rest fields)
  "The SML tuple of FIELDS (the unit when there are none)."
  (if fields
      (apply #'vector (sml-tuple-shape (length fields)) fields)
      *sml-unit*))

(define-compiler-macro sml-tuple (&rest fields)
  (if fields
      `(vector (load-time-value (sml-tuple-shape ,(length fields)) t) ,@fields)
      '(sml-unit)))

(defun sml-record-p (value)
  "Is VALUE an SML record?  Tuples and the unit are records."
  (and (simple-vector-p value)
       (plusp (length value))
       (sml-record-shape-p (svref value 0))))

(defun sml-tuple-p (value)
  "Is VALUE an SML tuple: a record whose labels are 1, ..., n, n /= 1?"
  (and (sml-record-p value)
       (let ((labels (sml-record-shape-labels (svref value 0))))
         (and (/= (length labels) 1)
              (eq (svref value 0) (sml-tuple-shape (length labels)))))))

(defun sml-record-or-tuple-p (value)
  (sml-record-p value))

(defun sml-record-labels (record)
  "The labels of RECORD, in canonical order."
  (coerce (sml-record-shape-labels (svref record 0)) 'list))

(defun sml-record-fields (record)
  "RECORD as an alist from labels to values, in canonical label order."
  (unless (sml-record-p record)
    (error "Expected SML record, got ~S" record))
  (loop for label across (sml-record-shape-labels (svref record 0))
        for i from 1
        collect (cons label (svref record i))))

(defun make-sml-record (fields)
  "The SML record with FIELDS, an alist from labels (strings, or integers
for numeric labels) to values, in any order."
  (let ((sorted (sort (mapcar (lambda (field)
                                (cons (sml-label-string (car field)) (cdr field)))
                              fields)
                      #'sml-label< :key #'car)))
    (apply #'vector (sml-record-shape (mapcar #'car sorted)) (mapcar #'cdr sorted))))

(defun sml-record-index (record label)
  "The index of the field LABEL of RECORD."
  (let ((labels (sml-record-shape-labels (svref record 0))))
    (if (integerp label)
        (and (<= 1 label (length labels))
             (string= (svref labels (1- label)) (princ-to-string label))
             label)
        (let ((position (position label labels :test #'string=)))
          (and position (1+ position))))))

(defun sml-record-ref (record label)
  "The field LABEL (a string, or an integer for a numeric label) of RECORD."
  (unless (sml-record-p record)
    (error "Expected SML record or tuple, got ~S" record))
  (let ((index (sml-record-index record label)))
    (if index
        (svref record index)
        (error "Record does not contain field ~A: ~S" label record))))

(defun sml-record-field (record label)
  "The field LABEL of RECORD, or NIL when RECORD is not a record with that
field."
  (let ((index (and (sml-record-p record) (sml-record-index record label))))
    (and index (svref record index))))

(defun sml-record-select (record label)
  (sml-record-ref record label))

(declaim (inline sml-tuple-ref))
(defun sml-tuple-ref (tuple i)
  "Field I (1-based, as in `#1') of TUPLE."
  (svref tuple i))

(declaim (inline sml-tuple-first sml-tuple-second))
(defun sml-tuple-first (tuple)
  (svref tuple 1))

(defun sml-tuple-second (tuple)
  (svref tuple 2))

(defun sml-tuple->list (tuple)
  "The fields of TUPLE, a list."
  (loop for i from 1 below (length tuple) collect (svref tuple i)))

(defun list->sml-tuple (list)
  "The tuple of the elements of LIST."
  (apply #'sml-tuple list))

;;; Constructed values.  A nullary constructor's value is its tag, a
;;; symbol.  A constructor whose argument type is written as a tuple type,
;;; `C of t1 * ... * tn', has the "flat" representation: the n-tuple with
;;; the tag in slot 0 instead of the shape, so `Node (l, x, r)' is one
;;; vector.  Other constructors, and exception constructors, pair their tag
;;; with the argument: (tag . argument).  The two cannot be confused (a cons
;;; and a vector), so code that does not know a constructor's
;;; representation tests the value (SML-CONSTRUCTED-TAG,
;;; SML-CONSTRUCTED-ARGUMENT).  The compiler knows it for constructors
;;; whose declaration it has seen (SML-CONSTRUCTOR-ARITY).

(declaim (inline sml-constructed-tag))
(defun sml-constructed-tag (value)
  "The tag of the constructed value VALUE built by a constructor with an
argument, or NIL when VALUE is not one."
  (cond ((consp value) (car value))
        ((and (simple-vector-p value) (plusp (length value)))
         (let ((tag (svref value 0)))
           (and (symbolp tag) tag)))))

(declaim (inline sml-flat-constructed-tag-p))
(defun sml-flat-constructed-tag-p (value tag)
  "Is VALUE a flat constructed value of the constructor with tag TAG?"
  (and (simple-vector-p value) (eq (svref value 0) tag)))

(defun sml-flat-constructed-argument (value)
  "The argument, a tuple, of the flat constructed value VALUE."
  (let ((tuple (copy-seq value)))
    (setf (svref tuple 0) (sml-tuple-shape (1- (length value))))
    tuple))

(defun sml-constructed-argument (value)
  "The argument of the constructed value VALUE."
  (if (consp value)
      (cdr value)
      (sml-flat-constructed-argument value)))

(defun sml-construct-flat (tag tuple)
  "The flat constructed value of constructor TAG applied to TUPLE."
  (let ((value (copy-seq tuple)))
    (setf (svref value 0) tag)
    value))

(defun sml-constructor-arity (symbol)
  "How the values of the constructor whose tag is SYMBOL are represented, as
far as known: N > 1 for the flat representation of an N-tuple argument, 0
for (tag . argument) or nullary, NIL when unknown."
  (get symbol 'sml-constructor-arity))

(defun sml-flat-constructed-p (value)
  "Is VALUE a flat constructed value?"
  (and (simple-vector-p value)
       (> (length value) 2)
       (symbolp (svref value 0))
       (eql (sml-constructor-arity (svref value 0)) (1- (length value)))))

(define-condition sml-raised-exception (error)
  ((value :initarg :value :reader sml-exception-value)
   ;; For Match and Bind raised by a failed match: the function (or "fn",
   ;; "case", "val") and source offset of the match, for diagnostics.
   (context :initarg :context :initform nil :reader sml-exception-context))
  (:report (lambda (condition stream)
             (format stream "uncaught exception ~A~@[ (~{no match in ~A~@[ at offset ~D~]~})~]"
                     (sml-exception-description (sml-exception-value condition))
                     (sml-exception-context condition)))))

(defun sml-exception-description (value)
  "How the REPL shows exception VALUE, e.g. `Match` or `Fail \"msg\"`."
  (cond
    ((sml-exception-tag-p value) (sml-exception-name value))
    ((sml-exception-p value)
     (format nil "~A ~A"
             (sml-exception-name value)
             ;; Defined with the REPL's printer.
             (funcall 'sml-value->string (sml-exception-payload value))))
    (t (princ-to-string value))))

(defun sml-raise (value)
  (error 'sml-raised-exception :value value))

(defun make-sml-adt (tag &optional payload)
  (cons tag payload))

(defun make-sml-ref (value)
  (vector :ref value))

(defun sml-value-binding-names (value &optional (package *sml-package*))
  (let ((names nil))
    (do-symbols (symbol (ensure-sml-package package) (nreverse names))
      (when (and (boundp symbol)
                 (eq (symbol-value symbol) value))
        (push (symbol-name symbol) names)))))

(defun ensure-sml-ref (cell)
  (unless (and (vectorp cell)
               (= (length cell) 2)
               (eq (aref cell 0) :ref))
    (error "Expected SML ref cell, got ~S~@[ (bound as ~{~A~^, ~})~]"
           cell
           (sml-value-binding-names cell)))
  cell)

(defun sml-list-hd (list)
  (if list
      (car list)
      (sml-raise-named-exception "Empty")))

(defun sml-list-tl (list)
  (if list
      (cdr list)
      (sml-raise-named-exception "Empty")))

;;; The Basis's binary operators.  SML-OP is a function of two arguments,
;;; which the compiler calls for an infix application `x op y' of the
;;; unshadowed operator; SML-OP-OP, a function of a pair, is the operator's
;;; value (`op +', as in `foldl (op +) 0 xs').
(defmacro define-sml-binary-primitive (name pair-name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list ,@body)
     (defun ,pair-name (pair)
       (,name (sml-tuple-first pair) (sml-tuple-second pair)))))

;;; Int.int is the 63-bit two's complement range, the same on every Common
;;; Lisp (64-bit SBCL's fixnums); arithmetic whose result leaves it raises
;;; Overflow.  The operators below are what the compiler calls for `+' and
;;; friends at type int (COMPILE-OVERLOADED-OPERATION), with a fast path for
;;; fixnum operands.
(defconstant +sml-min-int+ (- (ash 1 62)))
(defconstant +sml-max-int+ (1- (ash 1 62)))

(deftype sml-int () `(integer ,+sml-min-int+ ,+sml-max-int+))

(declaim (ftype (function () nil) sml-int-overflow))
(defun sml-int-overflow ()
  (sml-raise-named-exception "Overflow"))

(declaim (inline sml-check-int))
(defun sml-check-int (value)
  "VALUE, an Int.int result, or raise Overflow."
  (if (typep value 'sml-int) value (sml-int-overflow)))

(defmacro define-sml-int-operation (name lambda-list form)
  "Define NAME, the Int.int operation computing FORM from the integers of
LAMBDA-LIST, inline, with the result checked against Int's range."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list
       (if (and ,@(mapcar (lambda (var) `(typep ,var 'fixnum)) lambda-list))
           (let ,(mapcar (lambda (var) `(,var (the fixnum ,var))) lambda-list)
             (sml-check-int ,form))
           (sml-check-int ,form)))))

(define-sml-int-operation sml-int-+ (a b) (+ a b))
(define-sml-int-operation sml-int-- (a b) (- a b))
(define-sml-int-operation sml-int-* (a b) (* a b))
(define-sml-int-operation sml-int-negate (a) (- a))
(define-sml-int-operation sml-int-abs (a) (abs a))

(defmacro define-sml-int-division (name lambda-list documentation form)
  "Define NAME, a division of integers raising Div for a zero divisor,
inline, with a fast path for fixnum operands."
  (destructuring-bind (a b) lambda-list
    `(progn
       (declaim (inline ,name))
       (defun ,name ,lambda-list
         ,documentation
         (cond ((eql ,b 0) (sml-raise-named-exception "Div"))
               ((and (typep ,a 'fixnum) (typep ,b 'fixnum))
                (let ((,a ,a) (,b ,b))
                  (declare (fixnum ,a ,b))
                  ,form))
               (t ,form))))))

(define-sml-int-division sml-int-div (a b)
  "SML `div`: quotient rounded towards negative infinity."
  (sml-check-int (values (floor a b))))
(define-sml-int-division sml-integer-div (a b)
  "`div' on IntInf.int and the word types: SML-INT-DIV without the range
check."
  (values (floor a b)))
(define-sml-int-division sml-int-mod (a b)
  "SML `mod`: remainder with the sign of the divisor."
  (mod a b))
(define-sml-int-division sml-int-quot (a b)
  "Int.quot: quotient rounded towards zero."
  (sml-check-int (values (truncate a b))))
(define-sml-int-division sml-int-rem (a b)
  "Int.rem: remainder with the sign of the dividend."
  (rem a b))

;;; Without the static checker's resolution of overloading, `+' and friends
;;; are applied to values of any numeric type.  Reals follow IEEE; integers
;;; follow Int, except that an operand already beyond Int's range must be an
;;; IntInf.int, whose results are not checked.  (Words are integers below
;;; 2^62, so word arithmetic is only reduced modulo 2^wordSize when the
;;; checker resolves it, or the Word structure's functions are used.)
(defmacro sml-arithmetic (operator a b)
  "A and B combined by OPERATOR, with IEEE semantics when either is a real."
  `(cond
     ((and (typep ,a 'fixnum) (typep ,b 'fixnum))
      (sml-check-int (,operator (the fixnum ,a) (the fixnum ,b))))
     ((or (floatp ,a) (floatp ,b))
      (sml-ieee (,operator ,a ,b)))
     (t (sml-check-unchecked-int (,operator ,a ,b) ,a ,b))))

(defun sml-check-unchecked-int (value &rest operands)
  "VALUE, the result of integer OPERANDS of unknown type: Overflow if they
are all in Int's range and it is not."
  (if (or (typep value 'sml-int)
          (notevery (lambda (operand) (typep operand 'sml-int)) operands))
      value
      (sml-int-overflow)))

(define-sml-binary-primitive sml-+ sml-op-+ (a b) (sml-arithmetic + a b))
(defun sml-~ (a)
  (cond ((typep a 'fixnum) (sml-check-int (- a)))
        ((floatp a) (- a))
        (t (sml-check-unchecked-int (- a) a))))
(define-sml-binary-primitive sml-- sml-op-- (a b) (sml-arithmetic - a b))
(define-sml-binary-primitive sml-* sml-op-* (a b) (sml-arithmetic * a b))
(defun sml-unchecked-div (a b)
  (if (zerop b)
      (sml-raise-named-exception "Div")
      (sml-check-unchecked-int (values (floor a b)) a b)))
(define-sml-binary-primitive sml-div sml-op-div (a b) (sml-unchecked-div a b))
(define-sml-binary-primitive sml-mod sml-op-mod (a b) (sml-int-mod a b))

;;; Words are non-negative integers below 2^bits.  Code generation uses the
;;; static checker's resolution of overloaded operators to reduce the results
;;; of word arithmetic modulo the word type's size.
;;; The sizes are fixed rather than taken from the host's fixnums (62 bits on
;;; SBCL, 61 on ECL, 31 on ABCL) so that programs compute the same results on
;;; every Common Lisp; 62 keeps words fixnums on 64-bit SBCL.
(defconstant +sml-word-size+ 62
  "Word.wordSize.")

(defconstant +sml-word-mask+ (1- (ash 1 +sml-word-size+)))

(defun sml-word-type-bits (tyname)
  "The size in bits of word type TYNAME (as named by the static checker), or
NIL if TYNAME is not a word type."
  (cond ((string= tyname "word") +sml-word-size+)
        ((string= tyname "word8") 8)
        ((string= tyname "word16") 16)
        ((string= tyname "word32") 32)
        ((string= tyname "word64") 64)))

(defun sml-word-from-int (bits value)
  (ldb (byte bits 0) value))

(defun sml-word-to-int-x (bits word)
  "WORD read as a BITS-bit two's complement integer."
  (if (logbitp (1- bits) word)
      (- word (ash 1 bits))
      word))

(defun sml-word-shift (bits word shift)
  "WORD shifted left by SHIFT (right when SHIFT is negative), within BITS."
  (if (>= (abs shift) bits)
      0
      (ldb (byte bits 0) (ash word shift))))

(defun sml-word-arithmetic-shift-right (bits word shift)
  (ldb (byte bits 0) (ash (sml-word-to-int-x bits word) (- (min shift bits)))))

(defun sml-real-infinity (negativep)
  (portable-float-infinity negativep))

(defun sml-real-quotient (a b)
  "IEEE division: a zero divisor gives an infinity or a NaN."
  (sml-ieee (/ (float a 1d0) (float b 1d0))))

(define-sml-binary-primitive sml-/ sml-op-/ (a b) (sml-real-quotient a b))

;;; Comparisons involving a NaN are false.  They are tested for explicitly:
;;; comparing a NaN traps on some hosts, and SBCL's constraint propagation
;;; assumes numbers are totally ordered (once (< x y) is false it folds
;;; (>= x y) to true), so a real comparison must also be a call of its own.
(declaim (notinline sml-real-< sml-real-> sml-real-<= sml-real->=))
(defun sml-real-unordered-p (a b)
  (or (portable-float-nan-p a) (portable-float-nan-p b)))
(defun sml-real-< (a b) (and (not (sml-real-unordered-p a b)) (< a b)))
(defun sml-real-> (a b) (and (not (sml-real-unordered-p a b)) (> a b)))
(defun sml-real-<= (a b) (and (not (sml-real-unordered-p a b)) (<= a b)))
(defun sml-real->= (a b) (and (not (sml-real-unordered-p a b)) (>= a b)))

;;; Operations the compiler emits once the static checker has resolved an
;;; overloaded operator (COMPILE-OVERLOADED-OPERATION).  They tell the CL
;;; compiler the operands' representation without trusting it: a value
;;; of another representation is converted (reals) or reduced (words), never
;;; an error, so they are safe at any optimization policy.

(declaim (inline sml-double))
(defun sml-double (x)
  "Real X as a double-float (SML reals are doubles; this only informs the
CL compiler, and converts the odd rational or single-float from Lisp)."
  (if (typep x 'double-float) x (coerce x 'double-float)))

(defmacro sml-double-nan-p (x)
  #+sbcl `(sb-ext:float-nan-p ,x)
  #-sbcl `(portable-float-nan-p ,x))

(defmacro sml-real-comparison (operator a b)
  "A and B, reals, compared by OPERATOR; false when either is a NaN.  The
NaN tests come first and are explicit, so that the comparison itself never
sees a NaN (it could trap) and SBCL's constraint propagation, which takes
numbers to be totally ordered, cannot fold a later comparison of the same
operands (see SML-REAL-<)."
  (let ((x (gensym "X")) (y (gensym "Y")))
    `(let ((,x (sml-double ,a)) (,y (sml-double ,b)))
       (and (not (sml-double-nan-p ,x)) (not (sml-double-nan-p ,y))
            (,operator ,x ,y)))))

(defmacro sml-word-operand (bits form)
  "FORM, a value of a BITS-bit word type, declared as one."
  (let ((v (gensym "W")))
    `(let ((,v ,form))
       (if (typep ,v '(unsigned-byte ,bits)) ,v (ldb (byte ,bits 0) ,v)))))

(defmacro sml-int-comparison (operator a b)
  "A and B, Int.int values, compared by OPERATOR, inline for fixnums."
  (let ((x (gensym "X")) (y (gensym "Y")))
    `(let ((,x ,a) (,y ,b))
       (if (and (typep ,x 'fixnum) (typep ,y 'fixnum))
           (,operator (the fixnum ,x) (the fixnum ,y))
           (,operator ,x ,y)))))

(defun sml-ordered-compare (a b)
  "-1, 0 or 1 as A is less than, equal to or greater than B; NIL when they
are unordered (a NaN is involved)."
  (cond
    ((and (numberp a) (numberp b))
     (unless (sml-real-unordered-p a b)
       (cond ((< a b) -1)
             ((> a b) 1)
             (t 0))))
    ((and (characterp a) (characterp b))
     (cond ((char< a b) -1)
           ((char> a b) 1)
           (t 0)))
    ((and (stringp a) (stringp b))
     (cond ((string< a b) -1)
           ((string> a b) 1)
           (t 0)))
    (t
     (error "Unsupported SML ordered comparison: ~S and ~S" a b))))

(define-sml-binary-primitive sml-< sml-op-< (a b) (eql -1 (sml-ordered-compare a b)))
(define-sml-binary-primitive sml-> sml-op-> (a b) (eql 1 (sml-ordered-compare a b)))
(define-sml-binary-primitive sml->= sml-op->= (a b)
  (let ((order (sml-ordered-compare a b)))
    (and order (>= order 0))))
(define-sml-binary-primitive sml-<= sml-op-<= (a b)
  (let ((order (sml-ordered-compare a b)))
    (and order (<= order 0))))
(defun sml-ref-p (value)
  (and (simple-vector-p value)
       (= (length value) 2)
       (eq (svref value 0) :ref)))

(defun sml-equal (a b)
  "SML's polymorphic equality: structural on lists, tuples, records,
constructed values, strings and vectors; references and arrays are only
equal to themselves."
  (loop
    (cond
      ((eq a b) (return t))
      ((consp a)
       (unless (and (consp b) (sml-equal (car a) (car b)))
         (return nil))
       (setf a (cdr a)
             b (cdr b)))
      ((numberp a)
       (return (and (numberp b)
                    (if (or (floatp a) (floatp b))
                        (sml-ieee (= a b))
                        (= a b)))))
      ((stringp a) (return (and (stringp b) (string= a b))))
      ((simple-vector-p a)
       (return (and (simple-vector-p b)
                    (not (sml-ref-p a))
                    (not (sml-ref-p b))
                    (not (sml-array-p a))
                    (not (sml-array-p b))
                    (= (length a) (length b))
                    (every #'sml-equal a b))))
      (t (return nil)))))

(define-sml-binary-primitive sml-= sml-op-= (a b) (sml-equal a b))
(define-sml-binary-primitive sml-<> sml-op-<> (a b) (not (sml-equal a b)))

(define-sml-binary-primitive sml-^ sml-op-^ (a b) (concatenate 'string a b))
(define-sml-binary-primitive sml-@ sml-op-@ (a b) (append a b))

(defun sml-debug-funcall (function argument source-head)
  (unless (or (functionp function)
              (and (symbolp function) (fboundp function)))
    (error "SML application head ~S evaluated to non-function ~S"
           source-head function))
  (funcall function argument))

(defun sml-andalso (a) (lambda (b) (and a b)))
(defun sml-orelse (a) (lambda (b) (or a b)))
(defun sml-not (v) (not v))
(defun sml-abs (v)
  (cond ((typep v 'fixnum) (sml-check-int (abs v)))
        ((floatp v) (abs v))
        (t (sml-check-unchecked-int (abs v) v))))
(defun sml-floor (v) (sml-real-to-integer v #'floor))
(defun sml-ceil (v) (sml-real-to-integer v #'ceiling))
(defun sml-round (v) (sml-real-to-integer v #'round))
(defun sml-trunc (v) (sml-real-to-integer v #'truncate))
(declaim (inline sml-real-sqrt))
(defun sml-real-sqrt (x)
  "Math.sqrt of the double X, opened by the compiler: a NaN below zero."
  (declare (double-float x))
  (if (< x 0d0) (sml-nan) (sqrt x)))
(defun sml-sqrt (v) (sml-math-guarded #'sqrt v))
(defun sml-sin (v) (sml-math-guarded #'sin v))
(defun sml-cos (v) (sml-math-guarded #'cos v))
(defun sml-arctan (v) (sml-math-guarded #'atan v))
(defun sml-exp (v) (sml-math-guarded #'exp v))
(defun sml-ln (v) (sml-math-guarded #'log v))
(defun sml-real (v)
  "Real.fromInt: the nearest double; integers beyond the double range become
infinities."
  (if (floatp v)
      (coerce v 'double-float)
      (or (portable-rational-to-double v)
          (sml-real-infinity (minusp v)))))

(defun sml-int-to-string (value)
  (if (minusp value)
      (format nil "~~~A" (- value))
      (princ-to-string value)))

(define-sml-binary-primitive sml-cons sml-op-cons (a b) (cons a b))
(defun sml-hd (l) (sml-list-hd l))
(defun sml-tl (l) (sml-list-tl l))
(defun sml-length (l) (length l))
(defun sml-null (l) (null l))
(defun sml-rev (l) (reverse l))
(defun sml-map (f) (lambda (l) (mapcar f l)))
(defun sml-app (f)
  (lambda (l)
    (mapc f l)
    (sml-unit)))
(defun sml-foldl (fn)
  (lambda (init)
    (lambda (list)
      (let ((acc init))
        (dolist (item list acc)
          (setf acc (funcall fn (sml-tuple item acc))))))))
(defun sml-foldr (fn)
  (lambda (init)
    (lambda (list)
      (let ((acc init))
        (dolist (item (reverse list) acc)
          (setf acc (funcall fn (sml-tuple item acc))))))))
(defun sml-concat (strings)
  (apply #'concatenate 'string strings))

(defun sml-size (string)
  (length string))

(defun sml-explode (string)
  (coerce string 'list))

(defun sml-implode (chars)
  (coerce chars 'string))

(defun sml-ord (char)
  (char-code char))

(defun sml-chr (code)
  "Char.chr: SML characters are the 8-bit codes 0 to 255."
  (if (and (integerp code) (<= 0 code 255))
      (code-char code)
      (sml-raise-named-exception "Chr")))

(defun sml-str (char)
  (string char))

(defvar *sml-initial-exceptions* (make-hash-table :test #'equal)
  "The exceptions of the initial basis by name.  There is one of each, shared
by every SML package.")

(defparameter *sml-basis-declared-exceptions*
  '(("Empty" . "Empty") ("Fail" . "Fail") ("Span" . "Span")
    ("Option" . "Option.Option"))
  "The initial basis' exceptions that the Basis Library declares itself
(HaMLet's static basis does not have them), and their names there.  The run
time raises the basis' exceptions, so that a program's handlers catch them.")

(defparameter *sml-exceptions-with-payload* '("Fail" "Io")
  "The run time's exceptions that carry a value.")

(defun sml-initial-exception (name)
  "The initial basis' exception NAME (`Match`, `Div`, `Fail`, ...)."
  (let* ((basis-name (cdr (assoc name *sml-basis-declared-exceptions*
                                 :test #'string=)))
         (basis (and basis-name (sml-basis-package)))
         (symbol (and basis (find-symbol basis-name basis))))
    (if (and symbol (boundp symbol))
        (symbol-value symbol)
        (with-sml-runtime-lock ()
          (or (gethash name *sml-initial-exceptions*)
              (setf (gethash name *sml-initial-exceptions*)
                    (if (member name *sml-exceptions-with-payload* :test #'string=)
                        (make-sml-exception-function name)
                        (make-sml-exception-constructor name))))))))

(defun sml-raise-named-exception (name)
  "Raise the initial basis' exception NAME.  A program that binds the name to
another exception does not change what the primitives raise."
  (sml-raise (sml-initial-exception name)))

(declaim (ftype (function (t &optional t t) nil) sml-raise-match-failure))
(defun sml-raise-match-failure (exception-name &optional where position)
  "Raise `Match` or `Bind` (EXCEPTION-NAME) because no rule of a match
applied.  WHERE names the function, or is \"fn\", \"case\" or \"val\";
POSITION is the source offset of the match, when known."
  (error 'sml-raised-exception
         :value (sml-initial-exception exception-name)
         :context (and where (list where position))))

(defun sml-sequence-sub (sequence index)
  (if (and (integerp index)
           (<= 0 index)
           (< index (length sequence)))
      (elt sequence index)
      (sml-raise-named-exception "Subscript")))

;;; Vectors are simple-vectors of their elements.  An array of reals with
;;; elements is a (simple-array double-float (*)), its elements unboxed;
;;; any other array is a simple-vector whose slot 0 holds :ARRAY and slots
;;; 1..n the elements.  An array is made of reals when its elements are
;;; double-floats, which only reals are; an empty array of reals may be
;;; either, so every operation tests the representation, unless the
;;; compiler knows the element type is not real (SML-GENERIC-ARRAY-SUB).
;;; Like a ref (#(:ref value)), an array is equal only to itself
;;; (SML-EQUAL).  Its type in the Basis is still 'a ref vector ref
;;; (src/basis/Array.sml), for its equality.  The compiler opens SUB,
;;; UPDATE and LENGTH (IR-EMIT-BASIS-PRIMITIVE).

(deftype sml-real-array () '(simple-array double-float (*)))

(declaim (inline sml-vector-sub sml-array-p sml-array-length
                 sml-array-sub sml-array-update sml-generic-array-sub sml-generic-array-update))
(defun sml-vector-sub (vector index)
  (declare (simple-vector vector))
  (if (and (typep index 'fixnum) (< -1 index (length vector)))
      (svref vector index)
      (sml-raise-named-exception "Subscript")))

(defun sml-array-p (value)
  (or (typep value 'sml-real-array)
      (and (simple-vector-p value)
           (plusp (length value))
           (eq (svref value 0) :array))))

(defun sml-array-length (array)
  (if (typep array 'sml-real-array)
      (length array)
      (1- (length (the simple-vector array)))))

(defun sml-generic-array-sub (array index)
  (declare (simple-vector array))
  (if (and (typep index 'fixnum) (< -1 index (1- (length array))))
      (svref array (1+ index))
      (sml-raise-named-exception "Subscript")))

(defun sml-generic-array-update (array index value)
  (declare (simple-vector array))
  (if (and (typep index 'fixnum) (< -1 index (1- (length array))))
      (setf (svref array (1+ index)) value)
      (sml-raise-named-exception "Subscript"))
  (sml-unit))

(defun sml-array-sub (array index)
  (if (typep array 'sml-real-array)
      (if (and (typep index 'fixnum) (< -1 index (length array)))
          (aref array index)
          (sml-raise-named-exception "Subscript"))
      (sml-generic-array-sub array index)))

(defun sml-array-update (array index value)
  (if (typep array 'sml-real-array)
      (if (and (typep index 'fixnum) (< -1 index (length array)))
          (setf (aref array index) (sml-double value))
          (sml-raise-named-exception "Subscript"))
      (sml-generic-array-update array index value))
  (sml-unit))

(defconstant +sml-array-max-length+ (- array-dimension-limit 2)
  "Array.maxLen: one slot of a simple-vector holds :ARRAY.")

(defun make-sml-array (length initial)
  (unless (and (integerp length) (<= 0 length +sml-array-max-length+))
    (sml-raise-named-exception "Size"))
  (if (typep initial 'double-float)
      (make-array length :element-type 'double-float :initial-element initial)
      (let ((array (make-array (1+ length) :initial-element initial)))
        (setf (svref array 0) :array)
        array)))

(defun sml-array-from-list (list)
  (let ((array (make-sml-array (length list) (first list))))
    (loop for item in list
          for i from 0
          do (sml-array-update array i item))
    array))

(defun sml-array-elements (array)
  "The elements of ARRAY, a simple-vector or a real array, and their start."
  (if (typep array 'sml-real-array)
      (values array 0)
      (values array 1)))

(defun sml-array-vector (array)
  (multiple-value-bind (elements start) (sml-array-elements array)
    (coerce (subseq elements start) 'simple-vector)))

(defun sml-array-copy (src dst di)
  "Array.copy {src, dst, di}; also right when SRC is DST."
  (multiple-value-bind (from from-start) (sml-array-elements src)
    (multiple-value-bind (to to-start) (sml-array-elements dst)
      (if (and (typep di 'fixnum) (<= 0 di)
               (<= (+ di (sml-array-length src)) (sml-array-length dst)))
          (replace to from :start1 (+ to-start di) :start2 from-start)
          (sml-raise-named-exception "Subscript"))))
  (sml-unit))

(defun sml-array-copy-vector (src dst di)
  "Array.copyVec {src, dst, di}."
  (declare (simple-vector src))
  (multiple-value-bind (to to-start) (sml-array-elements dst)
    (if (and (typep di 'fixnum) (<= 0 di) (<= (+ di (length src)) (sml-array-length dst)))
        (replace to src :start1 (+ to-start di))
        (sml-raise-named-exception "Subscript")))
  (sml-unit))

(define-sml-binary-primitive sml-compose sml-o (f g)
  (lambda (x)
    (funcall f (funcall g x))))

(define-sml-binary-primitive sml-before-2 sml-before (a b)
  (declare (ignore b))
  a)

(defun sml-ignore (value)
  (declare (ignore value))
  (sml-unit))

(defun sml-ref (value)
  (make-sml-ref value))

(defun sml-deref (cell)
  (aref (ensure-sml-ref cell) 1))

(define-sml-binary-primitive sml-assign sml-op-assign (cell value)
  (setf (aref (ensure-sml-ref cell) 1) value)
  (sml-unit))

(defun sml-print (value)
  (let ((stream *standard-output*))
    (with-sml-stream-lock (stream)
      (princ value stream)))
  (sml-unit))

(defun sml-exn-name-primitive (value)
  (sml-exception-name value))

(defun sml-primitive-stub (name)
  (lambda (&optional arg)
    (declare (ignore arg))
    (error "Unimplemented SML basis primitive: ~A" name)))

(defun make-sml-unresolved-functor-member (name &optional cause)
  (when cause
    (warn "Deferring unresolved SML functor member ~A: ~A" name cause))
  (lambda (&rest args)
    (declare (ignore args))
    (if cause
        (error "Unresolved SML functor member ~A: ~A" name cause)
        (error "Unresolved SML functor member: ~A" name))))

(defun sml-constant-primitive (value)
  (lambda (&optional unit)
    (declare (ignore unit))
    value))

(defun sml-none-value ()
  "The basis's NONE, which every package sees (see INITIALIZE-SML-PACKAGE)."
  (sml-symbol "NONE" *sml-basis-package-name*))

(defun sml-some-value (value)
  "The basis's SOME VALUE."
  (let ((some (sml-symbol "SOME" *sml-basis-package-name*)))
    (if (and (boundp some) (functionp (symbol-value some)))
        (funcall (symbol-value some) value)
        (cons some value))))

(defun sml-word-not (value)
  (logand +sml-word-mask+ (lognot value)))

(defun sml-word-shift-left (tuple &optional (bits +sml-word-size+))
  (sml-word-shift bits (sml-tuple-first tuple) (sml-tuple-second tuple)))

(defun sml-word-shift-right (tuple &optional (bits +sml-word-size+))
  (sml-word-shift bits (sml-tuple-first tuple) (- (sml-tuple-second tuple))))

(defun sml-text-io-stream (stream)
  (case stream
    (:text-io-stdin *standard-input*)
    (:text-io-stdout *standard-output*)
    (:text-io-stderr *error-output*)
    (otherwise
     (if (streamp stream)
         stream
         (error "Invalid SML TextIO stream: ~S" stream)))))

(defparameter *sml-external-format* :utf-8
  "The encoding of SML source files, of the files TextIO opens and of the
Common Lisp cl-sml writes.  Implementations disagree on the default (ECL
follows the locale, ABCL the JVM), so it is always given explicitly.")

(defun sml-io-cause (message)
  "The cause of an IO.Io exception: OS.SysErr (MESSAGE, NONE)."
  (let ((sys-err (sml-basis-symbol "OS.SysErr" nil)))
    (if (and sys-err (functionp (symbol-value sys-err)))
        (funcall (symbol-value sys-err) (sml-tuple message (sml-none-value)))
        (funcall (sml-initial-exception "Fail") message))))

(defun sml-raise-io (function name message)
  "Raise IO.Io {name = NAME, function = FUNCTION, cause = ...}."
  (sml-raise (funcall (sml-initial-exception "Io")
                      (make-sml-record
                       (list (cons "cause" (sml-io-cause message))
                             (cons "function" function)
                             (cons "name" name))))))

(defun sml-open-file (function path &rest options)
  "OPEN PATH with OPTIONS for the Basis function FUNCTION, raising IO.Io
when the file cannot be opened."
  (or (handler-case (apply #'open path options)
        (file-error (condition)
          (sml-raise-io function path (princ-to-string condition))))
      (sml-raise-io function path "No such file or directory")))

(defun sml-text-io-open-in (path)
  (sml-open-file "TextIO.openIn" path
                 :direction :input :element-type 'character
                 :external-format *sml-external-format*
                 :if-does-not-exist nil))

(defun sml-text-io-open-out (path)
  (sml-open-file "TextIO.openOut" path
                 :direction :output :element-type 'character
                 :external-format *sml-external-format*
                 :if-exists :supersede :if-does-not-exist :create))

(defun sml-text-io-open-append (path)
  (sml-open-file "TextIO.openAppend" path
                 :direction :output :element-type 'character
                 :external-format *sml-external-format*
                 :if-exists :append :if-does-not-exist :create))

(defun sml-text-io-close-in (stream)
  (let ((stream (sml-text-io-stream stream)))
    (with-sml-stream-lock (stream)
      (close stream)))
  (sml-unit))

(defun sml-text-io-close-out (stream)
  (let ((stream (sml-text-io-stream stream)))
    (with-sml-stream-lock (stream)
      (close stream)))
  (sml-unit))

(defun sml-text-io-read-n (stream count)
  (when (minusp count)
    (error "TextIO.inputN called with a negative count: ~D" count))
  (let ((input (sml-text-io-stream stream)))
    (with-sml-stream-lock (input)
      (with-output-to-string (output)
        (loop repeat count
              for char = (read-char input nil nil)
              while char
              do (write-char char output))))))

(defun sml-text-io-input (stream)
  (sml-text-io-read-n stream 4096))

(defun sml-text-io-input1 (stream)
  (let ((char (let ((input (sml-text-io-stream stream)))
                (with-sml-stream-lock (input)
                  (read-char input nil nil)))))
    (if char
        (sml-some-value char)
        (sml-none-value))))

(defun sml-text-io-input-n (tuple)
  (sml-text-io-read-n (sml-tuple-first tuple) (sml-tuple-second tuple)))

(defun sml-text-io-input-all (stream)
  (let ((input (sml-text-io-stream stream)))
    (with-sml-stream-lock (input)
      (with-output-to-string (output)
        (loop for char = (read-char input nil nil)
              while char
              do (write-char char output))))))

(defun sml-text-io-input-line (stream)
  "TextIO.inputLine: the next line with its newline, which is supplied when
the stream ends without one; NONE at the end of the stream."
  (let ((input (sml-text-io-stream stream)))
    (with-sml-stream-lock (input)
      (if (null (peek-char nil input nil nil))
          (sml-none-value)
          (sml-some-value
           (with-output-to-string (output)
             (loop for char = (read-char input nil nil)
                   do (write-char (or char #\Newline) output)
                   until (or (null char) (char= char #\Newline)))))))))

(defun sml-text-io-lookahead (stream)
  (let ((char (let ((input (sml-text-io-stream stream)))
                (with-sml-stream-lock (input)
                  (peek-char nil input nil nil)))))
    (if char
        (sml-some-value char)
        (sml-none-value))))

(defun sml-text-io-end-of-stream (stream)
  (let ((input (sml-text-io-stream stream)))
    (with-sml-stream-lock (input)
      (null (peek-char nil input nil nil)))))

(defun sml-text-io-output (tuple)
  (let ((output (sml-text-io-stream (sml-tuple-first tuple))))
    (with-sml-stream-lock (output)
      (write-string (sml-tuple-second tuple) output)))
  (sml-unit))

(defun sml-text-io-output1 (tuple)
  (let ((output (sml-text-io-stream (sml-tuple-first tuple))))
    (with-sml-stream-lock (output)
      (write-char (sml-tuple-second tuple) output)))
  (sml-unit))

(defun sml-text-io-flush-out (stream)
  (let ((output (sml-text-io-stream stream)))
    (with-sml-stream-lock (output)
      (finish-output output)))
  (sml-unit))

;;; BinIO.  An input stream is an SML-BIN-INSTREAM: CL has no PEEK-BYTE, so
;;; lookahead keeps the byte it read.  Output streams are octet streams.

(defstruct (sml-bin-instream (:constructor make-sml-bin-instream (stream)))
  stream
  (pending nil))

(defun sml-bin-io-open-in (path)
  (make-sml-bin-instream
   (sml-open-file "BinIO.openIn" path
                  :direction :input :element-type '(unsigned-byte 8)
                  :if-does-not-exist nil)))

(defun sml-bin-io-open-out (path)
  (sml-open-file "BinIO.openOut" path
                 :direction :output :element-type '(unsigned-byte 8)
                 :if-exists :supersede :if-does-not-exist :create))

(defun sml-bin-io-open-append (path)
  (sml-open-file "BinIO.openAppend" path
                 :direction :output :element-type '(unsigned-byte 8)
                 :if-exists :append :if-does-not-exist :create))

(defun sml-bin-io-read-byte (in)
  "The next byte of IN, an SML-BIN-INSTREAM, or NIL at its end."
  (with-sml-stream-lock (in)
    (let ((pending (sml-bin-instream-pending in)))
      (if pending
          (progn (setf (sml-bin-instream-pending in) nil) pending)
          (read-byte (sml-bin-instream-stream in) nil nil)))))

(defun sml-bin-io-peek-byte (in)
  (with-sml-stream-lock (in)
    (or (sml-bin-instream-pending in)
        (setf (sml-bin-instream-pending in)
              (read-byte (sml-bin-instream-stream in) nil nil)))))

(defun sml-bin-io-read-n (in count)
  "A Word8Vector.vector of the next COUNT bytes of IN, or of fewer at its
end."
  (when (minusp count)
    (sml-raise-named-exception "Size"))
  (coerce (loop repeat count
                for byte = (sml-bin-io-read-byte in)
                while byte
                collect byte)
          'simple-vector))

(defun sml-bin-io-input-all (in)
  (coerce (loop for byte = (sml-bin-io-read-byte in)
                while byte
                collect byte)
          'simple-vector))

(defun sml-bin-io-output (tuple)
  (let ((out (sml-tuple-first tuple)))
    (with-sml-stream-lock (out)
      (loop for byte across (sml-tuple-second tuple)
            do (write-byte (ldb (byte 8 0) byte) out)))
    (sml-unit)))

;;; Time and Timer: times are integer nanoseconds.

(defconstant +unix-epoch-universal-time+ 2208988800
  "1970-01-01T00:00:00Z as a universal time.")

(defvar *sml-real-time-base* nil
  "A universal time and the internal real time at that moment.")

(defun internal-time-nanoseconds (internal-time)
  (round (* internal-time 1000000000) internal-time-units-per-second))

(defun sml-local-zone (unix-seconds)
  "The local time zone at UNIX-SECONDS (UTC, since the Unix epoch): its
offset west of UTC in seconds, daylight saving time included, and whether
daylight saving time is in effect.  Times the host cannot decode (before
1900, or far ahead) use the zone of the current time."
  (multiple-value-bind (s mi h d mo y wd dst zone)
      (let ((universal (+ unix-seconds +unix-epoch-universal-time+)))
        (if (<= 0 universal (* 400 366 86400))
            (decode-universal-time universal)
            (decode-universal-time (get-universal-time))))
    (declare (ignore s mi h d mo y wd))
    (values (- (round (* zone 3600)) (if dst 3600 0)) dst)))

(defun sml-time-now (unit)
  "Time.now: nanoseconds since the Unix epoch, to the precision of the
internal real time."
  (declare (ignore unit))
  (destructuring-bind (universal . internal)
      (or *sml-real-time-base*
          (setf *sml-real-time-base*
                (cons (get-universal-time) (get-internal-real-time))))
    (+ (* (- universal +unix-epoch-universal-time+) 1000000000)
       (internal-time-nanoseconds (- (get-internal-real-time) internal)))))

(defun sml-raise-sys-err (message)
  "Raise OS.SysErr (MESSAGE, NONE)."
  (sml-raise (sml-io-cause message)))

(defmacro with-sml-sys-err (&body body)
  "BODY, with file errors raised as OS.SysErr."
  `(handler-case (progn ,@body)
     (file-error (condition)
       (sml-raise-sys-err (princ-to-string condition)))))

(defun sml-os-file-sys-file-size (path)
  (with-sml-sys-err
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (file-length stream))))

(defun sml-os-file-sys-mod-time (path)
  (with-sml-sys-err
    (* (- (or (file-write-date path)
              (sml-raise-sys-err (format nil "~A: no modification time" path)))
          +unix-epoch-universal-time+)
       1000000000)))

(defun sml-os-file-sys-tmp-name (unit)
  (declare (ignore unit))
  (let ((pathname (temporary-sml-artifact-pathname)))
    ;; Created, so that no other caller gets the name.
    (with-open-file (stream pathname :direction :output :if-does-not-exist :create)
      (declare (ignore stream)))
    (namestring pathname)))

(defun sml-os-file-sys-get-dir (unit)
  (declare (ignore unit))
  (namestring (truename *default-pathname-defaults*)))

(defun sml-os-file-sys-ch-dir (path)
  (setf *default-pathname-defaults*
        (uiop:ensure-directory-pathname (truename path)))
  (sml-unit))

;;; Real, Math, command-line and file-system primitives used by the Basis.

(defmacro with-sml-float-traps-masked (&body body)
  `(sml-ieee (progn ,@body)))

(defun sml-nan ()
  (portable-float-nan))

(defun sml-real-nan-p (x)
  (portable-float-nan-p x))

(defun sml-real-infinite-p (x)
  (portable-float-infinite-p x))

(defun sml-real-finite-p (x)
  (not (or (sml-real-nan-p x) (sml-real-infinite-p x))))

(defun sml-real-to-integer (x rounder &optional (check #'sml-check-int))
  "Real.floor and friends: a NaN raises Domain, an infinity or a result
beyond Int's range (CHECK) Overflow."
  (cond ((sml-real-nan-p x) (sml-raise-named-exception "Domain"))
        ((sml-real-infinite-p x) (sml-raise-named-exception "Overflow"))
        (t (funcall check (values (funcall rounder x))))))

(defun sml-real-to-large-int (mode)
  "Real.toLargeInt MODE: the IEEEReal.rounding_mode constructor MODE picks
the rounding; the result is unbounded (LargeInt is IntInf)."
  (let ((rounder (cdr (assoc (sml-unqualified-name mode)
                             '(("TO_NEGINF" . floor) ("TO_POSINF" . ceiling)
                               ("TO_ZERO" . truncate) ("TO_NEAREST" . round))
                             :test #'string=))))
    (lambda (x) (sml-real-to-integer x rounder #'identity))))

;;; Real.fmt.  The digits are computed from the double's exact rational
;;; value with integer arithmetic, rounded to nearest even, so that every
;;; host prints the same; the layout is C's printf (%e, %f, %G) with SML's
;;; `~' for minus and unpadded exponents without `+', as Poly/ML has it.

(defun sml-decimal-exponent (q)
  "The E with 10^E <= Q < 10^(E+1), for a positive rational Q."
  (let ((e (floor (log (coerce q 'double-float) 10d0))))
    (loop while (< q (expt 10 e)) do (decf e))
    (loop while (>= q (expt 10 (1+ e))) do (incf e))
    e))

(defun sml-significant-digits (q n)
  "Q, a positive rational, rounded to N significant digits: the digits, as
an integer M with 10^(N-1) <= M < 10^N, and the exponent E of Q's
scientific notation M/10^(N-1) * 10^E."
  (let* ((e (sml-decimal-exponent q))
         (m (round (* q (expt 10 (- n 1 e))))))
    (if (>= m (expt 10 n))
        (values (/ m 10) (1+ e))
        (values m e))))

(defun sml-digits-with-point (m decimals)
  "The integer M divided by 10^DECIMALS, written with DECIMALS digits after
the point (and no point when DECIMALS is 0)."
  (multiple-value-bind (whole fraction) (floor m (expt 10 decimals))
    (if (zerop decimals)
        (princ-to-string whole)
        (format nil "~D.~V,'0D" whole decimals fraction))))

(defun sml-exponent-string (e)
  "An exponent, with `~', unpadded as Poly/ML writes it (C pads it to two
digits)."
  (format nil "E~:[~;~~~]~D" (minusp e) (abs e)))

(defun sml-real-fixed (q decimals)
  (sml-digits-with-point (round (* q (expt 10 decimals))) decimals))

(defun sml-real-scientific (q decimals)
  (if (zerop q)
      (concatenate 'string (sml-digits-with-point 0 decimals) (sml-exponent-string 0))
      (multiple-value-bind (m e) (sml-significant-digits q (1+ decimals))
        (concatenate 'string (sml-digits-with-point m decimals)
                     (sml-exponent-string e)))))

(defun sml-strip-fraction-zeros (string)
  "STRING, a number with a decimal point, without the fraction's trailing
zeros (and without the point if the fraction is all zeros)."
  (if (find #\. string)
      (let ((end (length string)))
        (loop while (char= (char string (1- end)) #\0) do (decf end))
        (when (char= (char string (1- end)) #\.) (decf end))
        (subseq string 0 end))
      string))

(defun sml-real-general (q digits)
  "%.DIGITSG: fixed notation when the exponent E is at least -4 and below
DIGITS, else scientific, with trailing zeros dropped."
  (let ((e (if (zerop q) 0 (nth-value 1 (sml-significant-digits q digits)))))
    (if (and (<= -4 e) (< e digits))
        (sml-strip-fraction-zeros (sml-real-fixed q (- digits 1 e)))
        (multiple-value-bind (m e) (sml-significant-digits q digits)
          (concatenate 'string
                       (sml-strip-fraction-zeros (sml-digits-with-point m (1- digits)))
                       (sml-exponent-string e))))))

(defun sml-real-exact (magnitude)
  "IEEEReal.toString of Real.toDecimal: 0.DIGITSEk, the shortest digits."
  (if (zerop magnitude)
      "0.0"
      (multiple-value-bind (k digits) (portable-flonum-to-digits magnitude)
        (format nil "0.~AE~:[~;~~~]~D" digits (minusp k) (abs k)))))

(defun sml-real-fmt (kind digits x)
  "Real.fmt: KIND is :sci, :fix or :gen (DIGITS digits after the point,
resp. significant) or :exact."
  (cond
    ((sml-real-nan-p x) "nan")
    ((sml-real-infinite-p x) (if (plusp x) "inf" "~inf"))
    (t
     (let* ((q (abs (rational x)))
            (body (ecase kind
                    (:sci (sml-real-scientific q digits))
                    (:fix (sml-real-fixed q digits))
                    (:gen (let ((general (sml-real-general q digits)))
                            ;; SML/NJ and Poly/ML (unlike C) keep a
                            ;; fractional part: 1.0, not 1.
                            (if (find-if (lambda (c) (find c ".E")) general)
                                general
                                (concatenate 'string general ".0"))))
                    (:exact (sml-real-exact (abs x))))))
       (if (minusp (float-sign x))
           (concatenate 'string "~" body)
           body)))))

(defun sml-real-to-man-exp (x)
  "Real.toManExp: {man, exp} with X = man * 2^exp and 0.5 <= |man| < 1."
  (if (or (zerop x) (not (sml-real-finite-p x)))
      (make-sml-record (list (cons "exp" 0) (cons "man" x)))
      (multiple-value-bind (significand exponent sign) (decode-float x)
        (make-sml-record (list (cons "exp" exponent)
                               (cons "man" (* sign significand)))))))

(defun sml-real-from-man-exp (record)
  "Real.fromManExp: man * 2^exp, rounded; an infinity or zero when it is
beyond the doubles."
  (let ((man (sml-record-select record "man"))
        (exp (sml-record-select record "exp")))
    (if (or (zerop man) (not (sml-real-finite-p man)))
        man
        ;; Beyond these exponents the result is an infinity or a zero.
        (let ((q (* (rational man) (expt 2 (max -2200 (min 2200 exp))))))
          (or (portable-rational-to-double q)
              (sml-real-infinity (minusp q)))))))

(defun sml-real-from-decimal-string (string)
  "The double nearest to STRING, [-]W.FE[-]X (W or F may be empty), as
Real.scan reads it."
  (let* ((negative (and (plusp (length string)) (char= (char string 0) #\-)))
         (start (if negative 1 0))
         (point (position #\. string))
         (e (position #\E string))
         (whole (subseq string start point))
         (fraction (subseq string (1+ point) e))
         (digits (concatenate 'string whole fraction))
         (mantissa (if (string= digits "") 0 (parse-integer digits)))
         (exponent (- (parse-integer string :start (1+ e)) (length fraction)))
         ;; The decimal exponent of the leading digit, roughly.
         (magnitude (+ exponent (length (string-left-trim "0" digits))))
         (value (cond ((zerop mantissa) 0d0)
                      ((> magnitude 330) nil)
                      ((< magnitude -360) 0d0)
                      (t (portable-rational-to-double
                          (* mantissa (expt 10 exponent)))))))
    (cond ((null value) (sml-real-infinity negative))
          (negative (- value))
          (t value))))

(defun sml-real-to-string (x)
  "Real.toString, which is Real.fmt (StringCvt.GEN NONE)."
  (sml-real-fmt :gen 12 x))

(defun sml-real-next-after (tuple)
  (let ((x (sml-tuple-first tuple))
        (y (sml-tuple-second tuple)))
    (cond ((or (sml-real-nan-p x) (sml-real-nan-p y)) (sml-nan))
          ((= x y) y)
          ((zerop x) (if (> y 0)
                         least-positive-double-float
                         (- least-positive-double-float)))
          (t
           (let ((ulp (scale-float 1d0 (- (nth-value 1 (decode-float x)) 53))))
             (with-sml-float-traps-masked
               (if (eq (> y x) (plusp x))
                   (+ x (* (signum x) ulp))
                   (- x (* (signum x) ulp)))))))))

(defun sml-math-guarded (function value)
  (with-sml-float-traps-masked
    (let ((result (funcall function (float value 1d0))))
      (if (complexp result)
          (sml-nan)
          (coerce result 'double-float)))))

(defun sml-command-line-name (unit)
  (declare (ignore unit))
  (or (uiop:argv0) "cl-sml"))

(defun sml-command-line-arguments (unit)
  (declare (ignore unit))
  (portable-command-line-arguments))

(defun sml-os-file-sys-is-dir (path)
  (and (uiop:directory-exists-p path) t))

(defun sml-os-file-sys-is-link (path)
  "Is PATH a symbolic link?  Its truename then differs from that of its
directory joined with its name."
  (with-sml-sys-err
    (let* ((file (uiop:parse-native-namestring path))
           (dir (truename (uiop:pathname-directory-pathname
                           (merge-pathnames file (uiop:getcwd))))))
      (if (uiop:directory-exists-p file)
          (not (equal (truename (uiop:ensure-directory-pathname file))
                      (merge-pathnames
                       (make-pathname :directory
                                      (list :relative (file-namestring file)))
                       dir)))
          (not (equal (truename file)
                      (merge-pathnames (file-namestring file) dir)))))))

(defun sml-os-file-sys-list-dir (path)
  "The names of the entries of directory PATH (OS.FileSys.openDir)."
  (with-sml-sys-err
    (let ((dir (uiop:ensure-directory-pathname path)))
      (unless (uiop:directory-exists-p dir)
        (error "No such directory: ~A" path))
      (append (mapcar (lambda (p) (car (last (pathname-directory p))))
                      (uiop:subdirectories dir))
              (mapcar (lambda (p) (file-namestring p))
                      (uiop:directory-files dir))))))

(defun sml-os-file-sys-mk-dir (path)
  (ensure-directories-exist (uiop:ensure-directory-pathname path))
  (sml-unit))

(defun sml-os-file-sys-rm-dir (path)
  (uiop:delete-empty-directory (uiop:ensure-directory-pathname path))
  (sml-unit))

(defun sml-os-process-terminate (status)
  (finish-output)
  (uiop:quit (if (integerp status) status 0)))

(defun sml-os-process-get-env (name)
  (let ((value (uiop:getenv name)))
    (if value (sml-some-value value) (sml-none-value))))

(defun sml-os-process-system (command)
  "Run COMMAND with the shell; its exit status (0 is success)."
  (finish-output)
  (nth-value 2 (uiop:run-program command :force-shell t :ignore-error-status t
                                         :output :interactive
                                         :error-output :interactive)))

(defun sml-word-primitives (structure bits)
  "The primitives of the word structure STRUCTURE, whose words have BITS
bits.  Unchecked code may have computed their arguments with integer
arithmetic (see SML-ARITHMETIC), so they reduce them modulo 2^BITS first."
  (flet ((name (operation) (format nil "~A.~A" structure operation))
         (w (x) (ldb (byte bits 0) x)))
    (flet ((binary (function)
             (lambda (tuple)
               (w (funcall function (w (sml-tuple-first tuple))
                           (w (sml-tuple-second tuple))))))
           (shift (function)
             (lambda (tuple)
               (funcall function (w (sml-tuple-first tuple))
                        (ldb (byte +sml-word-size+ 0) (sml-tuple-second tuple)))))
           (division (function)
             (lambda (tuple)
               (let ((divisor (w (sml-tuple-second tuple))))
                 (if (zerop divisor)
                     (sml-raise-named-exception "Div")
                     (values (funcall function (w (sml-tuple-first tuple)) divisor)))))))
      `((,(name "wordSize") . ,(sml-constant-primitive bits))
        (,(name "toInt") . ,(lambda (x) (sml-check-int (w x))))
        (,(name "toIntX") . ,(lambda (x) (sml-check-int (sml-word-to-int-x bits (w x)))))
        (,(name "fromInt") . ,(lambda (x) (w x)))
        ;; LargeWord is Word.
        (,(name "toLarge") . ,(lambda (x) (ldb (byte +sml-word-size+ 0) (w x))))
        (,(name "toLargeX") . ,(lambda (x)
                                 (sml-word-from-int +sml-word-size+
                                                    (sml-word-to-int-x bits (w x)))))
        (,(name "fromLarge") . ,(lambda (x) (w x)))
        ;; LargeInt is IntInf.
        (,(name "toLargeInt") . ,(lambda (x) (w x)))
        (,(name "toLargeIntX") . ,(lambda (x) (sml-word-to-int-x bits (w x))))
        (,(name "fromLargeInt") . ,(lambda (x) (w x)))
        (,(name "notb") . ,(lambda (x) (w (lognot x))))
        (,(name "orb") . ,(binary #'logior))
        (,(name "xorb") . ,(binary #'logxor))
        (,(name "andb") . ,(binary #'logand))
        (,(name "<<") . ,(shift (lambda (word count) (sml-word-shift bits word count))))
        (,(name ">>") . ,(shift (lambda (word count) (sml-word-shift bits word (- count)))))
        (,(name "~>>") . ,(shift (lambda (word count)
                                   (sml-word-arithmetic-shift-right bits word count))))
        (,(name "+") . ,(binary #'+))
        (,(name "-") . ,(binary #'-))
        (,(name "*") . ,(binary #'*))
        (,(name "div") . ,(division #'floor))
        (,(name "mod") . ,(division #'mod))
        (,(name "fromIntInf") . ,(lambda (x)
                                   (if (<= 0 x (ldb (byte bits 0) -1))
                                       x
                                       (sml-int-overflow))))
        (,(name "fmt") . ,(lambda (radix)
                            (lambda (x) (sml-integer-fmt radix (w x)))))))))

(defparameter *sml-word-structures*
  `(("Word" . ,+sml-word-size+) ("Word8" . 8)
    ("Word64" . 64) ("Word32" . 32) ("Word16" . 16))
  "The word structures whose primitives SML-WORD-PRIMITIVES makes.")

(defun sml-integer-fmt (radix value)
  "VALUE written in RADIX, the StringCvt.radix constructor BIN, OCT, DEC or
HEX, with upper-case digits and `~' for minus."
  (let ((base (cdr (assoc (sml-unqualified-name radix)
                          '(("BIN" . 2) ("OCT" . 8) ("DEC" . 10) ("HEX" . 16))
                          :test #'string=))))
    (format nil "~:[~;~~~]~:@(~vR~)" (minusp value) base (abs value))))

(defun sml-integer-primitives (structure bits)
  "The primitives of the integer structure STRUCTURE (IntInf, Int64, ...),
whose values are BITS-bit two's complement integers, or unbounded when BITS
is NIL; results beyond the range raise Overflow."
  (let ((min (and bits (- (ash 1 (1- bits)))))
        (max (and bits (1- (ash 1 (1- bits))))))
    (flet ((name (operation) (format nil "~A.~A" structure operation))
           (check (x) (if (or (null bits) (<= min x max)) x (sml-int-overflow))))
      (flet ((binary (function)
               (lambda (tuple)
                 (check (funcall function (sml-tuple-first tuple)
                                 (sml-tuple-second tuple)))))
             (division (function)
               (lambda (tuple)
                 (let ((divisor (sml-tuple-second tuple)))
                   (if (zerop divisor)
                       (sml-raise-named-exception "Div")
                       (check (values (funcall function (sml-tuple-first tuple)
                                               divisor))))))))
        `((,(name "precision") . ,(lambda (unit)
                                    (declare (ignore unit))
                                    (if bits (sml-some-value bits) (sml-none-value))))
          (,(name "minInt") . ,(lambda (unit)
                                 (declare (ignore unit))
                                 (if bits (sml-some-value min) (sml-none-value))))
          (,(name "maxInt") . ,(lambda (unit)
                                 (declare (ignore unit))
                                 (if bits (sml-some-value max) (sml-none-value))))
          ;; LargeInt is IntInf.
          (,(name "toLarge") . ,#'identity)
          (,(name "toInt") . ,#'sml-check-int)
          (,(name "fromLarge") . ,(lambda (x) (check x)))
          (,(name "fromInt") . ,(lambda (x) (check x)))
          (,(name "~") . ,(lambda (x) (check (- x))))
          (,(name "abs") . ,(lambda (x) (check (abs x))))
          (,(name "+") . ,(binary #'+))
          (,(name "-") . ,(binary #'-))
          (,(name "*") . ,(binary #'*))
          (,(name "div") . ,(division #'floor))
          (,(name "mod") . ,(division #'mod))
          (,(name "quot") . ,(division #'truncate))
          (,(name "rem") . ,(division #'rem))
          (,(name "fmt") . ,(lambda (radix)
                              (lambda (x) (sml-integer-fmt radix x))))
          ;; IntInf's own.
          (,(name "pow") . ,(lambda (tuple)
                              (let ((x (sml-tuple-first tuple))
                                    (n (sml-tuple-second tuple)))
                                (cond ((>= n 0) (check (expt x n)))
                                      ((zerop x) (sml-raise-named-exception "Div"))
                                      ((= x 1) 1)
                                      ((= x -1) (if (evenp n) 1 -1))
                                      (t 0)))))
          (,(name "log2") . ,(lambda (x)
                               (if (plusp x)
                                   (1- (integer-length x))
                                   (sml-raise-named-exception "Domain"))))
          (,(name "orb") . ,(binary #'logior))
          (,(name "xorb") . ,(binary #'logxor))
          (,(name "andb") . ,(binary #'logand))
          (,(name "notb") . ,#'lognot)
          (,(name "<<") . ,(binary #'ash))
          (,(name "~>>") . ,(binary (lambda (x n) (ash x (- n))))))))))

(defparameter *sml-integer-structures*
  '(("IntInf") ("Int64" . 64) ("Int32" . 32) ("Int16" . 16) ("Int8" . 8))
  "The integer structures whose primitives SML-INTEGER-PRIMITIVES makes.")

(defvar *sml-basis-extension-files* nil
  "The SML sources (namestrings) of the systems that extend the Basis
(cl-sml/cml), which the native checker and the Coalton Basis take after
the Basis's own (REGISTER-SML-BASIS-EXTENSION in src/basis.lisp).")

(defvar *sml-extension-primitives* (make-hash-table :test #'equal)
  "The primitives of the systems that extend the Basis (cl-sml/cml), by
name, for `use{b = NAME}'.")

(defun sml-basis-primitive (name)
  (or (and (plusp (hash-table-count *sml-extension-primitives*))
           (gethash name *sml-extension-primitives*))
      (let* ((dot (position #\. name :from-end t))
             (structure (and dot (subseq name 0 dot)))
             (word (assoc structure *sml-word-structures* :test #'equal))
             (integer (assoc structure *sml-integer-structures* :test #'equal)))
        (cond (word (cdr (assoc name (sml-word-primitives structure (cdr word))
                                :test #'string=)))
              (integer (cdr (assoc name (sml-integer-primitives structure (cdr integer))
                                   :test #'string=)))))
      (cdr (assoc name
                  `(("General.exnName" . ,#'sml-exn-name-primitive)
                    ("String.maxSize" . ,(sml-constant-primitive (1- array-dimension-limit)))
                    ("String.size" . ,#'sml-size)
                    ("String.sub" . ,(lambda (tuple)
                                       (sml-sequence-sub (sml-tuple-first tuple) (sml-tuple-second tuple))))
                    ("String.str" . ,#'sml-str)
                    ("String.^" . ,#'sml-op-^)
                    ("Char.ord" . ,#'sml-ord)
                    ("Char.chr" . ,#'sml-chr)
                    ("Int.precision" . ,(sml-constant-primitive (sml-some-value 63)))
                    ("Int.minInt" . ,(sml-constant-primitive (sml-some-value +sml-min-int+)))
                    ("Int.maxInt" . ,(sml-constant-primitive (sml-some-value +sml-max-int+)))
                    ("Int.quot" . ,(lambda (tuple)
                                      (sml-int-quot (sml-tuple-first tuple) (sml-tuple-second tuple))))
                    ("Int.rem" . ,(lambda (tuple)
                                     (sml-int-rem (sml-tuple-first tuple) (sml-tuple-second tuple))))
                    ("Vector.maxLen" . ,(sml-constant-primitive (1- array-dimension-limit)))
                    ("Vector.length" . ,#'length)
                    ("Vector.sub" . ,(lambda (tuple)
                                       (sml-vector-sub (sml-tuple-first tuple)
                                                       (sml-tuple-second tuple))))
                    ("Vector.fromList" . ,(lambda (list)
                                            (coerce list 'vector)))
                    ("Array.maxLen" . ,(sml-constant-primitive +sml-array-max-length+))
                    ("Array.array" . ,(lambda (tuple)
                                        (make-sml-array (sml-tuple-first tuple)
                                                        (sml-tuple-second tuple))))
                    ("Array.fromList" . ,#'sml-array-from-list)
                    ("Array.length" . ,#'sml-array-length)
                    ("Array.sub" . ,(lambda (tuple)
                                      (sml-array-sub (svref tuple 1) (svref tuple 2))))
                    ("Array.update" . ,(lambda (tuple)
                                         (sml-array-update (svref tuple 1) (svref tuple 2)
                                                           (svref tuple 3))))
                    ("Array.vector" . ,#'sml-array-vector)
                    ("Array.copy" . ,(lambda (tuple)
                                       (sml-array-copy (svref tuple 1) (svref tuple 2)
                                                       (svref tuple 3))))
                    ("Array.copyVec" . ,(lambda (tuple)
                                          (sml-array-copy-vector (svref tuple 1) (svref tuple 2)
                                                                 (svref tuple 3))))
                    ("CharVector.fromList" . ,(lambda (list)
                                                (coerce list 'string)))
	                    ("TextIO.stdIn" . ,(sml-constant-primitive :text-io-stdin))
	                    ("TextIO.stdOut" . ,(sml-constant-primitive :text-io-stdout))
	                    ("TextIO.stdErr" . ,(sml-constant-primitive :text-io-stderr))
	                    ("TextIO.openIn" . ,#'sml-text-io-open-in)
	                    ("TextIO.openOut" . ,#'sml-text-io-open-out)
	                    ("TextIO.openAppend" . ,#'sml-text-io-open-append)
	                    ("TextIO.closeIn" . ,#'sml-text-io-close-in)
	                    ("TextIO.closeOut" . ,#'sml-text-io-close-out)
	                    ("TextIO.input" . ,#'sml-text-io-input)
	                    ("TextIO.input1" . ,#'sml-text-io-input1)
	                    ("TextIO.inputN" . ,#'sml-text-io-input-n)
	                    ("TextIO.inputAll" . ,#'sml-text-io-input-all)
	                    ("TextIO.inputLine" . ,#'sml-text-io-input-line)
                    ("TextIO.lookahead" . ,#'sml-text-io-lookahead)
                    ("TextIO.openString" . ,#'make-string-input-stream)
                    ("BinIO.openIn" . ,#'sml-bin-io-open-in)
                    ("BinIO.openOut" . ,#'sml-bin-io-open-out)
                    ("BinIO.openAppend" . ,#'sml-bin-io-open-append)
                    ("BinIO.closeIn" . ,(lambda (in)
                                          (close (sml-bin-instream-stream in))
                                          (sml-unit)))
                    ("BinIO.closeOut" . ,(lambda (out) (close out) (sml-unit)))
                    ("BinIO.input" . ,(lambda (in) (sml-bin-io-read-n in 4096)))
                    ("BinIO.input1" . ,(lambda (in)
                                         (let ((byte (sml-bin-io-read-byte in)))
                                           (if byte (sml-some-value byte) (sml-none-value)))))
                    ("BinIO.inputN" . ,(lambda (tuple)
                                         (sml-bin-io-read-n (sml-tuple-first tuple)
                                                            (sml-tuple-second tuple))))
                    ("BinIO.inputAll" . ,#'sml-bin-io-input-all)
                    ("BinIO.lookahead" . ,(lambda (in)
                                            (let ((byte (sml-bin-io-peek-byte in)))
                                              (if byte (sml-some-value byte) (sml-none-value)))))
                    ("BinIO.endOfStream" . ,(lambda (in) (null (sml-bin-io-peek-byte in))))
                    ("BinIO.output" . ,#'sml-bin-io-output)
                    ("BinIO.output1" . ,(lambda (tuple)
                                          (write-byte (ldb (byte 8 0) (sml-tuple-second tuple))
                                                      (sml-tuple-first tuple))
                                          (sml-unit)))
                    ("BinIO.flushOut" . ,(lambda (out) (finish-output out) (sml-unit)))
	                    ("TextIO.endOfStream" . ,#'sml-text-io-end-of-stream)
	                    ("TextIO.output" . ,#'sml-text-io-output)
                    ("TextIO.output1" . ,#'sml-text-io-output1)
                    ("TextIO.flushOut" . ,#'sml-text-io-flush-out)
	                    ("OS.FileSys.getDir" . ,#'sml-os-file-sys-get-dir)
	                    ("OS.FileSys.chDir" . ,#'sml-os-file-sys-ch-dir)
                    ("Math.e" . ,(sml-constant-primitive (coerce (exp 1) 'double-float)))
                    ("Math.pi" . ,(sml-constant-primitive (coerce pi 'double-float)))
                    ("Math.sqrt" . ,#'sml-sqrt)
                    ("Math.sin" . ,#'sml-sin)
                    ("Math.cos" . ,#'sml-cos)
                    ("Math.exp" . ,#'sml-exp)
                    ("Math.ln" . ,#'sml-ln)
                    ("Math.tan" . ,(lambda (v) (sml-math-guarded #'tan v)))
                    ("Math.asin" . ,(lambda (v) (sml-math-guarded #'asin v)))
                    ("Math.acos" . ,(lambda (v) (sml-math-guarded #'acos v)))
                    ("Math.atan" . ,(lambda (v) (sml-math-guarded #'atan v)))
                    ("Math.atan2" . ,(lambda (tuple)
                                       (with-sml-float-traps-masked
                                         (coerce (atan (sml-tuple-first tuple)
                                                       (sml-tuple-second tuple))
                                                 'double-float))))
                    ("Math.sinh" . ,(lambda (v) (sml-math-guarded #'sinh v)))
                    ("Math.cosh" . ,(lambda (v) (sml-math-guarded #'cosh v)))
                    ("Math.tanh" . ,(lambda (v) (sml-math-guarded #'tanh v)))
                    ("Math.log10" . ,(lambda (v)
                                       (sml-math-guarded (lambda (x) (log x 10d0)) v)))
                    ("Math.pow" . ,(lambda (tuple)
                                     (with-sml-float-traps-masked
                                       (let ((result (expt (sml-tuple-first tuple)
                                                           (sml-tuple-second tuple))))
                                         (if (complexp result)
                                             (sml-nan)
                                             (coerce result 'double-float))))))
                    ("Real.fromInt" . ,#'sml-real)
                    ("Real.==" . ,(lambda (tuple)
                                    (with-sml-float-traps-masked
                                      (= (sml-tuple-first tuple) (sml-tuple-second tuple)))))
                    ("Real.?=" . ,(lambda (tuple)
                                    (let ((x (sml-tuple-first tuple))
                                          (y (sml-tuple-second tuple)))
                                      ;; True when unordered, as well as when equal.
                                      (or (sml-real-nan-p x) (sml-real-nan-p y)
                                          (= x y)))))
                    ("Real.floor" . ,(lambda (x) (sml-real-to-integer x #'floor)))
                    ("Real.ceil" . ,(lambda (x) (sml-real-to-integer x #'ceiling)))
                    ("Real.trunc" . ,(lambda (x) (sml-real-to-integer x #'truncate)))
                    ("Real.round" . ,(lambda (x) (sml-real-to-integer x #'round)))
                    ("Real.toLargeInt" . ,#'sml-real-to-large-int)
                    ("Real.realFloor" . ,(lambda (x) (if (sml-real-finite-p x) (ffloor x) x)))
                    ("Real.realCeil" . ,(lambda (x) (if (sml-real-finite-p x) (fceiling x) x)))
                    ("Real.realTrunc" . ,(lambda (x) (if (sml-real-finite-p x) (ftruncate x) x)))
                    ("Real.realRound" . ,(lambda (x) (if (sml-real-finite-p x) (fround x) x)))
                    ("Real.rem" . ,(lambda (tuple)
                                     (with-sml-float-traps-masked
                                       (rem (sml-tuple-first tuple) (sml-tuple-second tuple)))))
                    ("Real.copySign" . ,(lambda (tuple)
                                          (float-sign (sml-tuple-second tuple)
                                                      (abs (sml-tuple-first tuple)))))
                    ("Real.signBit" . ,(lambda (x) (minusp (float-sign x))))
                    ("Real.isNan" . ,#'sml-real-nan-p)
                    ("Real.isFinite" . ,#'sml-real-finite-p)
                    ("Real.isNormal" . ,(lambda (x)
                                          (and (sml-real-finite-p x)
                                               (>= (abs x)
                                                   least-positive-normalized-double-float))))
                    ("Real.checkFloat" . ,#'identity)
                    ("Real.nextAfter" . ,#'sml-real-next-after)
                    ("Real.maxFinite" . ,(sml-constant-primitive most-positive-double-float))
                    ("Real.minPos" . ,(sml-constant-primitive least-positive-double-float))
                    ("Real.minNormalPos"
                     . ,(sml-constant-primitive least-positive-normalized-double-float))
                    ("Real.precision" . ,(sml-constant-primitive 53))
                    ("Real.radix" . ,(sml-constant-primitive 2))
                    ("Real.toString" . ,#'sml-real-to-string)
                    ("Real.fromDecimalString" . ,#'sml-real-from-decimal-string)
                    ("Real.toManExp" . ,#'sml-real-to-man-exp)
                    ("Real.fromManExp" . ,#'sml-real-from-man-exp)
                    ("Real.fmt" . ,(lambda (tuple)
                                     (let ((kind (ecase (sml-tuple-first tuple)
                                                   (0 :sci) (1 :fix) (2 :gen) (3 :exact)))
                                           (digits (sml-tuple-second tuple)))
                                       (lambda (x) (sml-real-fmt kind digits x)))))
                    ("CommandLine.name" . ,#'sml-command-line-name)
                    ("CommandLine.arguments" . ,#'sml-command-line-arguments)
                    ("OS.FileSys.isDir" . ,#'sml-os-file-sys-is-dir)
                    ("OS.FileSys.listDir" . ,#'sml-os-file-sys-list-dir)
                    ("OS.FileSys.isLink" . ,#'sml-os-file-sys-is-link)
                    ("OS.FileSys.setTime" . ,(lambda (arguments)
                                              (with-sml-sys-err
                                                (error "setTime is not supported: ~A"
                                                       (sml-tuple-first arguments)))))
                    ("OS.FileSys.mkDir" . ,#'sml-os-file-sys-mk-dir)
                    ("OS.FileSys.rmDir" . ,#'sml-os-file-sys-rm-dir)
                    ("OS.Process.terminate" . ,#'sml-os-process-terminate)
                    ("OS.Process.getEnv" . ,#'sml-os-process-get-env)
                    ("OS.Process.system" . ,#'sml-os-process-system)
                    ("OS.FileSys.fullPath" . ,(lambda (path)
                                                (with-sml-sys-err
                                                  (namestring (truename path)))))
                    ("OS.FileSys.modTime" . ,#'sml-os-file-sys-mod-time)
                    ("OS.FileSys.fileSize" . ,#'sml-os-file-sys-file-size)
                    ("OS.FileSys.remove" . ,(lambda (path)
                                              (with-sml-sys-err
                                                (or (probe-file path)
                                                    (sml-raise-sys-err
                                                     (format nil "~A: no such file" path)))
                                                (delete-file path))
                                              (sml-unit)))
                    ("OS.FileSys.rename" . ,(lambda (record)
                                              (with-sml-sys-err
                                                (uiop:rename-file-overwriting-target
                                                 (sml-record-select record "old")
                                                 (sml-record-select record "new")))
                                              (sml-unit)))
                    ("OS.FileSys.exists" . ,(lambda (path)
                                              (and (or (probe-file path)
                                                       (uiop:directory-exists-p path))
                                                   t)))
                    ("OS.FileSys.tmpName" . ,#'sml-os-file-sys-tmp-name)
                    ("Time.now" . ,#'sml-time-now)
                    ("Date.localOffset" . ,(lambda (seconds) (values (sml-local-zone seconds))))
                    ("Date.isDst" . ,(lambda (seconds) (nth-value 1 (sml-local-zone seconds))))
                    ("Timer.cpuTime" . ,(lambda (unit)
                                          (declare (ignore unit))
                                          (internal-time-nanoseconds (get-internal-run-time))))
                    ("Timer.realTime" . ,(lambda (unit)
                                           (declare (ignore unit))
                                           (internal-time-nanoseconds (get-internal-real-time)))))
                  :test #'string=))
      (sml-primitive-stub name)))

(defun sml-use (path)
  (cond
    ((stringp path)
     (let ((pathname (merge-pathnames path
                                      (or *sml-current-directory*
                                          *default-pathname-defaults*))))
       (load-sml-file pathname :package *sml-package*)
       (sml-unit)))
    ((sml-record-p path)
     (sml-basis-primitive (sml-record-select path "b")))
    (t
     (error "Unsupported SML use argument: ~S" path))))

;;; The two-argument functions of the binary operators of *SML-ENV*.
(defparameter *sml-binary-primitives*
  '(("+" . sml-+) ("-" . sml--) ("*" . sml-*) ("/" . sml-/)
    ("div" . sml-div) ("mod" . sml-mod) ("^" . sml-^) ("@" . sml-@)
    ("=" . sml-=) ("<>" . sml-<>) ("<" . sml-<) (">" . sml->)
    ("<=" . sml-<=) (">=" . sml->=) ("::" . cons) (":=" . sml-assign)
    ("o" . sml-compose) ("before" . sml-before-2)))

(defparameter *sml-env*
  '(("+" . #'sml-op-+)
    ("~" . #'sml-~)
    ("-" . #'sml-op--)
    ("*" . #'sml-op-*)
    ("/" . #'sml-op-/)
    ("div" . #'sml-op-div)
    ("mod" . #'sml-op-mod)
    ("^" . #'sml-op-^)
    ("@" . #'sml-op-@)
    ("=" . #'sml-op-=)
    ("<>" . #'sml-op-<>)
    (">" . #'sml-op->)
    ("<" . #'sml-op-<)
    (">=" . #'sml-op->=)
    ("<=" . #'sml-op-<=)
    ("::" . #'sml-op-cons)
    (":=" . #'sml-op-assign)
    ("!" . #'sml-deref)
    ("ref" . #'sml-ref)
    ("hd" . #'sml-hd)
    ("tl" . #'sml-tl)
    ("length" . #'sml-length)
    ("null" . #'sml-null)
    ("rev" . #'sml-rev)
    ("map" . #'sml-map)
    ("app" . #'sml-app)
    ("foldl" . #'sml-foldl)
    ("foldr" . #'sml-foldr)
    ("concat" . #'sml-concat)
    ("size" . #'sml-size)
    ("explode" . #'sml-explode)
    ("implode" . #'sml-implode)
    ("ord" . #'sml-ord)
    ("chr" . #'sml-chr)
    ("str" . #'sml-str)
    ("abs" . #'sml-abs)
    ("floor" . #'sml-floor)
    ("ceil" . #'sml-ceil)
    ("round" . #'sml-round)
    ("trunc" . #'sml-trunc)
    ("sqrt" . #'sml-sqrt)
    ("sin" . #'sml-sin)
    ("cos" . #'sml-cos)
    ("arctan" . #'sml-arctan)
    ("exp" . #'sml-exp)
    ("ln" . #'sml-ln)
    ("real" . #'sml-real)
    ("o" . #'sml-o)
    ("before" . #'sml-before)
    ("ignore" . #'sml-ignore)
    ("not" . #'sml-not)
    ("print" . #'sml-print)
    ("use" . #'sml-use)
    ("Int.toString" . #'sml-int-to-string)
    ("Math.pi" . pi)
    ("true" . t)
    ("false" . nil)
    ("nil" . nil)))

(defun initialize-sml-package (package)
  "Give the basis package, before the Basis Library is compiled into it, the
bindings of the initial basis that the run time's primitives make: the
datatypes option and order (src/basis/types.sml declares them again, with
the same constructors), the built-in exceptions and IO.Io.  Other packages
see the basis's."
  (when (string= (package-name package) *sml-basis-package-name*)
    (initialize-sml-basis-package package))
  package)

(defparameter *sml-basis-cl-names* '("EQUAL" "SOME")
  "The basis's names that are also external symbols of COMMON-LISP.  The
basis package shadows them before the basis's FASL is loaded: a FASL names
symbols by package and name, so it would otherwise refer to COMMON-LISP's.")

(defun initialize-sml-basis-package (package)
  (shadow *sml-basis-cl-names* package)
  (let* ((none (sml-symbol-in-package-name "NONE" package))
         (some (sml-symbol-in-package-name "SOME" package)))
    (unless (boundp none)
      (setf (symbol-value none) none)
      (register-sml-constructor none)
      (register-sml-binding-type none '(:option :unknown)))
    (unless (boundp some)
      (setf (symbol-value some)
            (lambda (value) (cons some value)))
      (register-sml-constructor some)
      (register-sml-binding-type some
                                 '(:fn :unknown (:option :unknown))))
    (export (list none some) package)
    ;; The constructors of the initial basis's datatype order.
    (dolist (name '("LESS" "EQUAL" "GREATER"))
      (let ((symbol (sml-symbol-in-package-name name package)))
        (unless (boundp symbol)
          (setf (symbol-value symbol) symbol)
          (register-sml-constructor symbol)
          (register-sml-binding-type symbol "order"))
        (export symbol package)))
    ;; The exceptions of HaMLet's initial basis, raised by the run-time
    ;; primitives through SML-RAISE-NAMED-EXCEPTION.
    (loop for (name . sml-name) in '(("Bind" . "Bind") ("Chr" . "Chr")
                                     ("Div" . "Div") ("Domain" . "Domain")
                                     ("Match" . "Match") ("Overflow" . "Overflow")
                                     ("Size" . "Size") ("Subscript" . "Subscript")
                                     ("Io" . "IO.Io"))
          for symbol = (sml-symbol-in-package-name sml-name package)
          unless (boundp symbol)
            do (setf (symbol-value symbol) (sml-initial-exception name))
               (register-sml-binding-type symbol "exn")
               (export symbol package))
    (register-sml-structure-members (package-name package) "IO" '("Io"))
    package))

(initialize-sml-package (ensure-sml-package *sml-basis-package-name*))
