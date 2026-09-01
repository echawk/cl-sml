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

(defparameter *hamlet-image-package* "SML.HAMLET-IMAGE")

(defvar *sml-type-checker* nil)

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
        (type-check-sml-string-using checker source :filename filename))))

(defmacro with-sml-type-checker ((checker) &body body)
  `(let ((*sml-type-checker* ,checker))
     ,@body))

(defparameter *sml-binding-types* (make-hash-table :test #'eq))

(defparameter *sml-constructor-symbols* (make-hash-table :test #'eq))

(defparameter *sml-type-aliases* (make-hash-table :test #'equal))

(defparameter *sml-structure-members* (make-hash-table :test #'equal))

(defparameter *sml-functor-members* (make-hash-table :test #'equal))

(defparameter *sml-functor-params* (make-hash-table :test #'equal))

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
       (projected-sml-structure-members package-name structure-name))))

(defun register-sml-functor-members (package-name functor-name member-names
                                      &optional param-name)
  (let ((key (sml-module-key package-name functor-name)))
    (setf (gethash key *sml-functor-members*)
          (normalize-sml-member-names member-names))
    (if param-name
        (setf (gethash key *sml-functor-params*) param-name)
        (remhash key *sml-functor-params*))))

(defun lookup-sml-functor-members (package-name functor-name)
  (copy-list (gethash (sml-module-key package-name functor-name) *sml-functor-members*)))

(defun lookup-sml-functor-param (package-name functor-name)
  (gethash (sml-module-key package-name functor-name) *sml-functor-params*))

(defun register-sml-functor-instantiator (package-name functor-name instantiator)
  (setf (gethash (sml-module-key package-name functor-name)
                 *sml-functor-instantiators*)
        instantiator))

(defun lookup-sml-functor-instantiator (package-name functor-name)
  (gethash (sml-module-key package-name functor-name)
           *sml-functor-instantiators*))

(defun sml-module-descendant-member-names (package-name module-name)
  (let ((package (string-upcase (string package-name)))
        (prefix (format nil "~A." module-name))
        (members nil))
    (maphash
     (lambda (key child-members)
       (let ((child-package (first key))
             (child-name (second key)))
         (when (and (string= package child-package)
                    (string-prefix-p prefix child-name))
           (let ((relative-name (subseq child-name (length prefix))))
             (dolist (member child-members)
               (push (format nil "~A.~A" relative-name member) members))))))
     *sml-structure-members*)
    (normalize-sml-member-names members)))

(defun register-sml-constructor (symbol &optional canonical-symbol)
  (setf (gethash symbol *sml-constructor-symbols*) (or canonical-symbol symbol))
  symbol)

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

(defun sml-symbol-value-or-unresolved (symbol)
  (if (boundp symbol)
      (symbol-value symbol)
      (make-sml-unresolved-functor-member (symbol-name symbol))))

(defun call-with-sml-functor-bindings (bindings thunk)
  (if bindings
      (progv (mapcar #'car bindings)
          (mapcar (lambda (binding)
                    (sml-symbol-value-or-unresolved (cdr binding)))
                  bindings)
        (funcall thunk))
      (funcall thunk)))

(defun wrap-sml-functor-application-value (value bindings)
  (if (and bindings (functionp value))
      (lambda (&rest args)
        (call-with-sml-functor-bindings
         bindings
         (lambda ()
           (let ((result (apply value args)))
             (if (functionp result)
                 (wrap-sml-functor-application-value result bindings)
                 result)))))
      value))

(defun sml-functor-binding-symbols (package-name param-name argument value-bindings)
  (let ((pairs nil))
    (labels ((add-binding (member target-name)
               (let ((param-symbol
                       (sml-symbol-in-package-name (if param-name
                                                       (format nil "~A.~A"
                                                               param-name member)
                                                       member)
                                                   package-name))
                     (target-symbol
                       (sml-symbol-in-package-name target-name package-name)))
                 (proclaim `(special ,param-symbol))
                 (unless (assoc param-symbol pairs)
                   (push (cons param-symbol target-symbol) pairs))))
             (add-argument-member (member)
               (let ((target-name (format nil "~A.~A" argument member)))
                 (add-binding member target-name)
                 (unless param-name
                   (let ((separator (position #\. member :from-end t)))
                     (when separator
                       (add-binding (subseq member (1+ separator))
                                    target-name)))))))
      (dolist (binding value-bindings)
        (add-binding (car binding) (cdr binding)))
      (when argument
        (let ((members (or (lookup-sml-structure-members package-name argument)
                           (lookup-sml-functor-members package-name argument)
                           '("compare"))))
          (dolist (member members)
            (add-argument-member member))))
    (nreverse pairs))))

(defun sml-functor-structure-bindings (package-name param-name argument)
  (let ((bindings nil))
    (when argument
      (dolist (member (lookup-sml-structure-members package-name argument))
        (loop for separator = (position #\. member)
              then (position #\. member :start (1+ separator))
              while separator
              for prefix = (subseq member 0 separator)
              for remainder = (subseq member (1+ separator))
              for formal = (if param-name
                               (format nil "~A.~A" param-name prefix)
                               prefix)
              for binding = (assoc formal bindings :test #'string=)
              do (if binding
                     (pushnew remainder (cdr binding) :test #'string=)
                     (push (list formal remainder) bindings)))))
    bindings))

(defun call-with-sml-functor-structure-bindings (package-name bindings thunk)
  (let ((saved nil))
    (unwind-protect
         (progn
           (dolist (binding bindings)
             (let ((key (sml-module-key package-name (first binding))))
               (multiple-value-bind (members presentp)
                   (gethash key *sml-structure-members*)
                 (push (list key members presentp) saved))
               (register-sml-structure-members package-name
                                               (first binding)
                                               (rest binding))))
           (funcall thunk))
      (dolist (entry saved)
        (if (third entry)
            (setf (gethash (first entry) *sml-structure-members*)
                  (second entry))
            (remhash (first entry) *sml-structure-members*))))))

(defun alias-sml-module-member (package-name target-module source-module member-name
                                &optional dynamic-bindings)
  (alias-sml-module-member-to-name package-name
                                   (format nil "~A.~A" target-module member-name)
                                   source-module
                                   member-name
                                   dynamic-bindings))

(defun alias-sml-module-member-to-name (package-name target-name source-module member-name
                                        &optional dynamic-bindings)
  (let* ((source (sml-symbol-in-package-name (format nil "~A.~A" source-module member-name)
                                             package-name))
         (target (sml-symbol-in-package-name target-name package-name)))
    (when (boundp source)
      (proclaim `(special ,target))
      (setf (symbol-value target)
            (wrap-sml-functor-application-value (symbol-value source)
                                                dynamic-bindings))
      (let ((type (lookup-sml-binding-type source)))
        (when type
          (register-sml-binding-type target type)))
      (when (sml-constructor-symbol-p source)
        (register-sml-constructor target
                                  (sml-constructor-canonical-symbol source)))
      (export (list target) (ensure-sml-package package-name)))))

(defun alias-sml-functor-application (package-name target-structure functor-name
                                      &key argument value-bindings)
  (let* ((members (lookup-sml-functor-members package-name functor-name))
         (param-name (lookup-sml-functor-param package-name functor-name))
         (instantiator (lookup-sml-functor-instantiator package-name functor-name))
         (dynamic-bindings (and (or argument value-bindings)
                                (sml-functor-binding-symbols package-name
                                                             param-name
                                                             argument
                                                             value-bindings)))
         (structure-bindings
           (sml-functor-structure-bindings package-name param-name argument)))
    (when instantiator
      (call-with-sml-functor-bindings
       dynamic-bindings
       (lambda ()
         (call-with-sml-functor-structure-bindings
          package-name structure-bindings instantiator)))
      (setf members
            (normalize-sml-member-names
             (append members
                     (sml-module-descendant-member-names
                      package-name functor-name))))
      (register-sml-functor-members package-name functor-name members param-name))
    (dolist (member members)
      (alias-sml-module-member package-name
                               target-structure
                               functor-name
                               member
                               dynamic-bindings))
    (register-sml-structure-members package-name target-structure members)
    target-structure))

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
  '(("+" . (:fn "int" (:fn "int" "int")))
    ("~" . (:fn "int" "int"))
    ("-" . (:fn "int" (:fn "int" "int")))
    ("*" . (:fn "int" (:fn "int" "int")))
    ("/" . (:fn "real" (:fn "real" "real")))
    ("div" . (:fn "int" (:fn "int" "int")))
    ("mod" . (:fn "int" (:fn "int" "int")))
    ("^" . (:fn "string" (:fn "string" "string")))
    ("@" . (:fn (:list :unknown) (:fn (:list :unknown) (:list :unknown))))
    ("=" . (:fn :unknown (:fn :unknown "bool")))
    ("<>" . (:fn :unknown (:fn :unknown "bool")))
    (">" . (:fn "int" (:fn "int" "bool")))
    ("<" . (:fn "int" (:fn "int" "bool")))
    (">=" . (:fn "int" (:fn "int" "bool")))
    ("<=" . (:fn "int" (:fn "int" "bool")))
    ("::" . (:fn :unknown (:fn (:list :unknown) (:list :unknown))))
    (":=" . (:fn "ref" (:fn :unknown "unit")))
    ("!" . (:fn "ref" :unknown))
    ("ref" . (:fn :unknown "ref"))
    ("hd" . (:fn (:list :unknown) :unknown))
    ("tl" . (:fn (:list :unknown) (:list :unknown)))
    ("length" . (:fn (:list :unknown) "int"))
    ("null" . (:fn (:list :unknown) "bool"))
    ("rev" . (:fn (:list :unknown) (:list :unknown)))
    ("map" . (:fn (:fn :unknown :unknown) (:fn (:list :unknown) (:list :unknown))))
    ("app" . (:fn (:fn :unknown "unit") (:fn (:list :unknown) "unit")))
    ("foldl" . (:fn (:fn :unknown (:fn :unknown :unknown)) (:fn :unknown (:fn (:list :unknown) :unknown))))
    ("foldr" . (:fn (:fn :unknown (:fn :unknown :unknown)) (:fn :unknown (:fn (:list :unknown) :unknown))))
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
    ("o" . (:fn (:fn :unknown :unknown) (:fn (:fn :unknown :unknown) (:fn :unknown :unknown))))
    ("before" . (:fn :unknown (:fn :unknown :unknown)))
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

(defun sml-value (name &optional (package *sml-package*))
  "Return the SML value named NAME in PACKAGE, or signal an unbound error."
  (let ((symbol (sml-symbol name package)))
    (if (boundp symbol)
        (symbol-value symbol)
        (error "Unbound SML value ~A in package ~A"
               name (package-name (ensure-sml-package package))))))

(defun (setf sml-value) (value name &optional (package *sml-package*))
  (setf (symbol-value (sml-symbol name package)) value))

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

(defun hamlet-image-entry-point ()
  (with-sml-package (*hamlet-image-package*)
    (let ((arguments (uiop:command-line-arguments)))
      (if arguments
          (call-sml "Sml.execFiles" arguments)
          (call-sml "Sml.execSession" (sml-unit))))))

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
  (gethash (list (package-name (ensure-sml-package package)) name)
           *sml-type-aliases*))

(defun lookup-sml-binding-type (symbol-or-name &optional (package *sml-package*))
  (etypecase symbol-or-name
    (symbol (gethash symbol-or-name *sml-binding-types*))
    (string (or (cdr (assoc symbol-or-name *sml-builtin-type-env* :test #'string=))
                (let ((symbol (find-symbol symbol-or-name
                                           (package-name (ensure-sml-package package)))))
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

(defun sort-sml-record-fields (fields)
  (sort (copy-list fields) #'string< :key #'car))

(defun make-sml-record (fields)
  (cons :record (sort-sml-record-fields fields)))

(defun sml-record-p (value)
  (and (consp value)
       (eq (car value) :record)))

(defun sml-record-fields (record)
  (if (sml-record-p record)
      (cdr record)
      (error "Expected SML record, got ~S" record)))

(defun sml-record-select (record label)
  (cond
    ((sml-record-p record)
     (let ((field (assoc label (sml-record-fields record) :test #'string=)))
       (if field
           (cdr field)
           (error "Record does not contain field ~A: ~S" label record))))
    ((and (consp record)
          (eq (car record) :tuple)
          (stringp label)
          (plusp (length label))
          (every #'digit-char-p label))
     (let ((index (parse-integer label)))
       (if (<= 1 index (length (cdr record)))
           (nth index record)
           (error "Tuple does not contain field ~A: ~S" label record))))
    (t
     (error "Expected SML record or tuple, got ~S" record))))

(define-condition sml-raised-exception (error)
  ((value :initarg :value :reader sml-exception-value))
  (:report (lambda (condition stream)
             (format stream "Unhandled SML exception: ~A"
                     (sml-exception-value condition)))))

(defun sml-raise (value)
  (error 'sml-raised-exception :value value))

(defun make-sml-adt (tag &optional payload)
  (cons tag payload))

(defun sml-unit ()
  (list :tuple))

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
      (error "hd called on empty list")))

(defun sml-list-tl (list)
  (if list
      (cdr list)
      (error "tl called on empty list")))

;; --- Curried Standard Library ---
;; SML functions are auto-curried. Lisp's standard functions are not.
;; We must wrap Lisp's binary operators into curried closures.
(defun sml-+ (a) (lambda (b) (+ a b)))
(defun sml-~ (a) (- a))
(defun sml-- (a) (lambda (b) (- a b)))
(defun sml-* (a) (lambda (b) (* a b)))
(defun sml-div (a) (lambda (b) (truncate a b)))
(defun sml-mod (a) (lambda (b) (mod a b)))
(defun sml-real-infinity (negativep)
  #+sbcl
  (if negativep
      sb-ext:double-float-negative-infinity
      sb-ext:double-float-positive-infinity)
  #-sbcl
  (if negativep
      (- most-positive-double-float)
      most-positive-double-float))

(defun sml-/ (a)
  (lambda (b)
    (if (zerop b)
        (sml-real-infinity (minusp a))
        (/ a b))))

(defun sml-ordered-compare (a b)
  (cond
    ((and (numberp a) (numberp b))
     (cond ((< a b) -1)
           ((> a b) 1)
           (t 0)))
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

(defun sml-< (a) (lambda (b) (minusp (sml-ordered-compare a b))))
(defun sml-> (a) (lambda (b) (plusp (sml-ordered-compare a b))))
(defun sml->= (a) (lambda (b) (not (minusp (sml-ordered-compare a b)))))
(defun sml-<= (a) (lambda (b) (not (plusp (sml-ordered-compare a b)))))
(defun sml-= (a) (lambda (b) (equal a b)))
(defun sml-<> (a) (lambda (b) (not (equal a b))))

(defun sml-^ (a) (lambda (b) (concatenate 'string a b)))
(defun sml-@ (a) (lambda (b) (append a b)))

(defun sml-apply-tuple-or-curried-binary (fn left right)
  (let ((tuple-result
          (handler-case
              (funcall fn (list :tuple left right))
            (error () :sml-tuple-call-failed))))
    (if (or (eq tuple-result :sml-tuple-call-failed)
            (functionp tuple-result))
        (funcall (funcall fn left) right)
        tuple-result)))

(defun sml-tuple-or-curried-binary (fn)
  (lambda (left)
    (if (and (consp left)
             (eq (first left) :tuple)
             (= (length left) 3))
        (sml-apply-tuple-or-curried-binary fn (second left) (third left))
        (lambda (right)
          (sml-apply-tuple-or-curried-binary fn left right)))))

(defun sml-debug-funcall (function argument source-head)
  (unless (or (functionp function)
              (and (symbolp function) (fboundp function)))
    (error "SML application head ~S evaluated to non-function ~S"
           source-head function))
  (funcall function argument))

(defun sml-andalso (a) (lambda (b) (and a b)))
(defun sml-orelse (a) (lambda (b) (or a b)))
(defun sml-not (v) (not v))
(defun sml-abs (v) (abs v))
(defun sml-floor (v) (floor v))
(defun sml-ceil (v) (ceiling v))
(defun sml-round (v) (round v))
(defun sml-trunc (v) (truncate v))
(defun sml-sqrt (v) (coerce (sqrt v) 'double-float))
(defun sml-sin (v) (coerce (sin v) 'double-float))
(defun sml-cos (v) (coerce (cos v) 'double-float))
(defun sml-arctan (v) (coerce (atan v) 'double-float))
(defun sml-exp (v) (coerce (exp v) 'double-float))
(defun sml-ln (v) (coerce (log v) 'double-float))
(defun sml-real (v) (coerce v 'double-float))

(defun sml-int-to-string (value)
  (if (minusp value)
      (format nil "~~~A" (- value))
      (princ-to-string value)))

(defun sml-cons (a) (lambda (b) (cons a b)))
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
          (setf acc (sml-apply-tuple-or-curried-binary fn item acc)))))))
(defun sml-foldr (fn)
  (lambda (init)
    (lambda (list)
      (let ((acc init))
        (dolist (item (reverse list) acc)
          (setf acc (sml-apply-tuple-or-curried-binary fn item acc)))))))
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
  (code-char code))

(defun sml-str (char)
  (string char))

(defun sml-raise-named-exception (name)
  (let ((symbol (sml-symbol name *sml-package*)))
    (sml-raise
     (if (boundp symbol)
         (symbol-value symbol)
         (make-sml-exception-constructor name)))))

(defun sml-sequence-sub (sequence index)
  (if (and (integerp index)
           (<= 0 index)
           (< index (length sequence)))
      (elt sequence index)
      (sml-raise-named-exception "Subscript")))

(defun sml-o (tuple)
  (let ((f (sml-tuple-first tuple))
        (g (sml-tuple-second tuple)))
    (lambda (x)
      (funcall f (funcall g x)))))

(defun sml-before (tuple)
  (sml-tuple-first tuple))

(defun sml-ignore (value)
  (declare (ignore value))
  (sml-unit))

(defun sml-ref (value)
  (make-sml-ref value))

(defun sml-deref (cell)
  (aref (ensure-sml-ref cell) 1))

(defun sml-assign (cell)
  (lambda (value)
    (setf (aref (ensure-sml-ref cell) 1) value)
    (sml-unit)))

(defun sml-print (value)
  (princ value)
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
  (sml-symbol "NONE"))

(defun sml-some-value (value)
  (let ((some (sml-symbol "SOME")))
    (if (and (boundp some) (functionp (symbol-value some)))
        (funcall (symbol-value some) value)
        (cons some value))))

(defun sml-tuple-first (tuple)
  (second tuple))

(defun sml-tuple-second (tuple)
  (third tuple))

(defun sml-word-not (value)
  (logand most-positive-fixnum (lognot value)))

(defun sml-word-shift-left (tuple)
  (ash (sml-tuple-first tuple) (sml-tuple-second tuple)))

(defun sml-word-shift-right (tuple)
  (ash (sml-tuple-first tuple) (- (sml-tuple-second tuple))))

(defun sml-text-io-stream (stream)
  (case stream
    (:text-io-stdin *standard-input*)
    (:text-io-stdout *standard-output*)
    (:text-io-stderr *error-output*)
    (otherwise
     (if (streamp stream)
         stream
         (error "Invalid SML TextIO stream: ~S" stream)))))

(defun sml-text-io-open-in (path)
  (open path :direction :input :element-type 'character))

(defun sml-text-io-open-out (path)
  (open path :direction :output :element-type 'character
             :if-exists :supersede :if-does-not-exist :create))

(defun sml-text-io-open-append (path)
  (open path :direction :output :element-type 'character
             :if-exists :append :if-does-not-exist :create))

(defun sml-text-io-close-in (stream)
  (close (sml-text-io-stream stream))
  (sml-unit))

(defun sml-text-io-close-out (stream)
  (close (sml-text-io-stream stream))
  (sml-unit))

(defun sml-text-io-read-n (stream count)
  (when (minusp count)
    (error "TextIO.inputN called with a negative count: ~D" count))
  (let ((input (sml-text-io-stream stream)))
    (with-output-to-string (output)
      (loop repeat count
            for char = (read-char input nil nil)
            while char
            do (write-char char output)))))

(defun sml-text-io-input (stream)
  (sml-text-io-read-n stream 4096))

(defun sml-text-io-input1 (stream)
  (let ((char (read-char (sml-text-io-stream stream) nil nil)))
    (if char
        (sml-some-value char)
        (sml-none-value))))

(defun sml-text-io-input-n (tuple)
  (sml-text-io-read-n (sml-tuple-first tuple) (sml-tuple-second tuple)))

(defun sml-text-io-input-all (stream)
  (let ((input (sml-text-io-stream stream)))
    (with-output-to-string (output)
      (loop for char = (read-char input nil nil)
            while char
            do (write-char char output)))))

(defun sml-text-io-input-line (stream)
  (let ((input (sml-text-io-stream stream)))
    (if (null (peek-char nil input nil nil))
        (sml-none-value)
        (sml-some-value
         (with-output-to-string (output)
           (loop for char = (read-char input nil nil)
                 while char
                 do (write-char char output)
                 when (char= char #\Newline)
                   do (return)))))))

(defun sml-text-io-end-of-stream (stream)
  (null (peek-char nil (sml-text-io-stream stream) nil nil)))

(defun sml-text-io-output (tuple)
  (write-string (sml-tuple-second tuple)
                (sml-text-io-stream (sml-tuple-first tuple)))
  (sml-unit))

(defun sml-text-io-output1 (tuple)
  (write-char (sml-tuple-second tuple)
              (sml-text-io-stream (sml-tuple-first tuple)))
  (sml-unit))

(defun sml-text-io-flush-out (stream)
  (finish-output (sml-text-io-stream stream))
  (sml-unit))

(defun sml-os-file-sys-get-dir (unit)
  (declare (ignore unit))
  (namestring (truename *default-pathname-defaults*)))

(defun sml-os-file-sys-ch-dir (path)
  (setf *default-pathname-defaults*
        (uiop:ensure-directory-pathname (truename path)))
  (sml-unit))

(defun sml-basis-primitive (name)
  (or (cdr (assoc name
                  `(("General.exnName" . ,#'sml-exn-name-primitive)
                    ("String.maxSize" . ,(sml-constant-primitive most-positive-fixnum))
                    ("String.size" . ,#'sml-size)
                    ("String.sub" . ,(lambda (tuple)
                                       (sml-sequence-sub (second tuple) (third tuple))))
                    ("String.str" . ,#'sml-str)
                    ("String.^" . ,#'sml-^)
                    ("Char.ord" . ,#'sml-ord)
                    ("Char.chr" . ,#'sml-chr)
                    ("Int.precision" . ,(sml-constant-primitive (sml-none-value)))
                    ("Int.minInt" . ,(sml-constant-primitive (sml-some-value most-negative-fixnum)))
                    ("Int.maxInt" . ,(sml-constant-primitive (sml-some-value most-positive-fixnum)))
                    ("Int.quot" . ,(lambda (tuple)
                                      (truncate (second tuple) (third tuple))))
                    ("Int.rem" . ,(lambda (tuple)
                                     (rem (second tuple) (third tuple))))
                    ("Word.wordSize" . ,(sml-constant-primitive (integer-length most-positive-fixnum)))
                    ("Word.toInt" . ,#'identity)
                    ("Word.toIntX" . ,#'identity)
                    ("Word.fromInt" . ,#'identity)
                    ("Word.notb" . ,#'sml-word-not)
                    ("Word.orb" . ,(lambda (tuple)
                                     (logior (sml-tuple-first tuple) (sml-tuple-second tuple))))
                    ("Word.xorb" . ,(lambda (tuple)
                                      (logxor (sml-tuple-first tuple) (sml-tuple-second tuple))))
                    ("Word.andb" . ,(lambda (tuple)
                                      (logand (sml-tuple-first tuple) (sml-tuple-second tuple))))
                    ("Word.<<" . ,#'sml-word-shift-left)
                    ("Word.>>" . ,#'sml-word-shift-right)
                    ("Word.~>>" . ,#'sml-word-shift-right)
                    ("Word8.toLarge" . ,#'identity)
                    ("Word8.toLargeX" . ,#'identity)
                    ("Word8.fromLarge" . ,(lambda (value) (logand value #xff)))
                    ("Word8.toInt" . ,#'identity)
                    ("Word8.toIntX" . ,#'identity)
                    ("Word8.fromInt" . ,(lambda (value) (logand value #xff)))
                    ("Word8.notb" . ,(lambda (value) (logand #xff (lognot value))))
                    ("Word8.orb" . ,(lambda (tuple)
                                      (logand #xff (logior (sml-tuple-first tuple)
                                                           (sml-tuple-second tuple)))))
                    ("Word8.xorb" . ,(lambda (tuple)
                                       (logand #xff (logxor (sml-tuple-first tuple)
                                                            (sml-tuple-second tuple)))))
                    ("Word8.andb" . ,(lambda (tuple)
                                       (logand #xff (logand (sml-tuple-first tuple)
                                                            (sml-tuple-second tuple)))))
                    ("Word8.<<" . ,(lambda (tuple)
                                     (logand #xff (sml-word-shift-left tuple))))
                    ("Word8.>>" . ,#'sml-word-shift-right)
                    ("Word8.~>>" . ,#'sml-word-shift-right)
                    ("Vector.maxLen" . ,(sml-constant-primitive most-positive-fixnum))
                    ("Vector.length" . ,#'length)
                    ("Vector.sub" . ,(lambda (tuple)
                                       (sml-sequence-sub (sml-tuple-first tuple)
                                                         (sml-tuple-second tuple))))
                    ("Vector.fromList" . ,(lambda (list)
                                            (coerce list 'vector)))
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
                    ("Math.ln" . ,#'sml-ln))
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

(defparameter *sml-env*
  '(("+" . #'sml-+)
    ("~" . #'sml-~)
    ("-" . #'sml--)
    ("*" . #'sml-*)
    ("/" . #'sml-/)
    ("div" . #'sml-div)
    ("mod" . #'sml-mod)
    ("^" . #'sml-^)
    ("@" . #'sml-@)
    ("=" . #'sml-=)
    ("<>" . #'sml-<>)
    (">" . #'sml->)
    ("<" . #'sml-<)
    (">=" . #'sml->=)
    ("<=" . #'sml-<=)
    ("::" . #'sml-cons)
    (":=" . #'sml-assign)
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
    package))

(initialize-sml-package *sml-package*)
