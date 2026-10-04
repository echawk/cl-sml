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

(defparameter *sml-binary-infix-value-names*
  '("+" "-" "*" "/" "div" "mod" "^" "@" "::" ":=" "=" "<>" "<" "<=" ">" ">="
    "o" "before" "-->" "|->" "@@" "$$" "oplus" "plus" "plusVE" "plusTE" "plusSE"
    "plusVEandTE" "plusG" "plusF" "plusE" "plusT" "plusU" "plusI"
    "IBplusI" "TEplus" "oplusVEandTE" "oplusTE" "oplusSE" "oplusG"
    "oplusF" "oplusE"))

(defun sml-binary-infix-value-name-p (name)
  (or (member name *sml-binary-infix-value-names* :test #'string=)
      (and (some (lambda (ch)
                   (find ch "!%&$#+-/:<=>?@\\~`^|*" :test #'char=))
                 name)
           (not (string= name "~")))))

(defvar *sml-signature-texts* (make-hash-table :test #'equal)
  "Source text of the signatures compiled so far, by name.")

(defun sml-word-in-text-p (word text)
  (flet ((identifier-char-p (ch)
           (or (alphanumericp ch) (char= ch #\_) (char= ch #\'))))
    (loop for start = (search word text) then (search word text :start2 (1+ start))
          while start
          thereis (let ((end (+ start (length word))))
                    (and (or (zerop start)
                             (not (identifier-char-p (char text (1- start)))))
                         (or (= end (length text))
                             (not (identifier-char-p (char text end)))))))))

(defun restrict-sealed-structure-members (members signature-name)
  "Restrict the registered members of a structure ascribed SIGNATURE-NAME to
the names that signature mentions.  Hidden members (helpers, constructors of
abstract types, names brought in by `open` in the body) otherwise leak into
every scope that later `open`s the structure.  The check is textual, so a
signature using `include` is not restricted."
  (let ((text (and signature-name
                   (gethash signature-name *sml-signature-texts*))))
    (if (or (null text) (sml-word-in-text-p "include" text))
        members
        (remove-if-not (lambda (name)
                         (sml-word-in-text-p
                          (subseq name 0 (or (position #\. name) (length name)))
                          text))
                       members))))

(defun note-signature-declaration (dec)
  "Record a signature's text as soon as its declaration is seen, so that member
lists computed ahead of compilation can already honour it."
  (when (and (consp dec) (eq (car dec) :signature))
    (let ((text (getf (cddr dec) :text)))
      (when text
        (setf (gethash (second dec) *sml-signature-texts*) text)))))

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
according to the static checker, or NIL when unknown."
  (let ((fact (first (sml-static-facts-at (sml-ast-position dec) "dtrep"
                                          (second dec)))))
    (and fact (parse-sml-structure-members (third fact)))))

(defun compile-datatype-replication-constructors (dec)
  "Bind the constructors DEC = (:datatype-replication name source) brings
into scope to those of the replicated datatype."
  (let* ((source-type (string-trim '(#\Space #\Tab #\Newline #\;) (third dec)))
         (dot (position #\. source-type :from-end t))
         (source-prefix (and dot (subseq source-type 0 dot))))
    (loop for (nil . name) in (replicated-constructors dec)
          for source = (resolved-sml-symbol
                        (if source-prefix
                            (qualify-sml-name source-prefix name)
                            name))
          for target = (target-sml-symbol name)
          unless (eq source target)
            collect (if (symbolp source)
                        `(alias-sml-constructor ',target ',source)
                        ;; A built-in such as `ref`, compiled to a function.
                        `(progn (defparameter ,target ,source)
                                ,(compile-export-form (list target)))))))

(defun structure-declaration-members (dec &optional local-structures)
  "The members a (:structure name decs [:sig S]) declaration makes visible."
  (let ((members (declarations-bound-names (third dec) local-structures))
        (static-members (static-structure-members dec)))
    (if (eq static-members :unknown)
        (restrict-sealed-structure-members members (getf (cdddr dec) :sig))
        ;; Exactly the members the elaborated (and possibly ascribed)
        ;; structure has.
        (remove-if-not (lambda (name)
                         (member name static-members :test #'string=))
                       members))))

(defun maybe-wrap-infix-value-initializer (name form)
  (if (sml-binary-infix-value-name-p name)
      `(sml-tuple-or-curried-binary ,form)
      form))

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
  (intern name *sml-package*))

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

(defun trim-sml-functor-arg-text (text)
  (let ((trimmed (and text
                      (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
    (if (and trimmed
             (>= (length trimmed) 2)
             (char= (char trimmed 0) #\()
             (char= (char trimmed (1- (length trimmed))) #\)))
        (string-trim '(#\Space #\Tab #\Newline #\Return)
                     (subseq trimmed 1 (1- (length trimmed))))
        trimmed)))

(defun simple-sml-id-text-p (text)
  (and text
       (plusp (length text))
       (every (lambda (ch)
                (or (alphanumericp ch)
                    (member ch '(#\_ #\') :test #'char=)))
              text)))

(defun functor-argument-structure-name (args-text)
  (let ((inner (trim-sml-functor-arg-text args-text)))
    (when (simple-sml-id-text-p inner)
      inner)))

(defun read-functor-argument-target-name (text start)
  (let* ((limit (or (position-if (lambda (ch)
                                   (member ch '(#\; #\) #\Space #\Tab
                                                #\Newline #\Return)
                                           :test #'char=))
                                 text
                                 :start start)
                    (length text))))
    (subseq text start limit)))

(defun functor-argument-value-bindings (args-text)
  (let ((inner (trim-sml-functor-arg-text args-text))
        (bindings nil))
    (when inner
      (loop with search-start = 0
            for pos = (search "val" inner :start2 search-start)
            while pos
            do (let ((after-val (+ pos 3)))
                 (if (and (< after-val (length inner))
                          (member (char inner after-val)
                                  '(#\Space #\Tab #\Newline #\Return)
                                  :test #'char=))
                     (let* ((name-start
                              (position-if-not
                               (lambda (ch)
                                 (member ch '(#\Space #\Tab #\Newline #\Return)
                                         :test #'char=))
                               inner
                               :start after-val))
                            (name-end
                              (and name-start
                                   (position-if-not
                                    (lambda (ch)
                                      (or (alphanumericp ch)
                                          (member ch '(#\_ #\') :test #'char=)))
                                    inner
                                    :start name-start)))
                            (name (and name-start name-end
                                       (subseq inner name-start name-end)))
                            (eq-pos (and name-end
                                         (position #\= inner :start name-end))))
                       (when (and name (plusp (length name)) eq-pos)
                         (let* ((target-start
                                  (position-if-not
                                   (lambda (ch)
                                     (member ch '(#\Space #\Tab #\Newline #\Return)
                                             :test #'char=))
                                   inner
                                   :start (1+ eq-pos)))
                                (target (and target-start
                                             (read-functor-argument-target-name
                                              inner
                                              target-start))))
                           (when (and target (plusp (length target)))
                             (push (cons name target) bindings))))
                       (setf search-start (or eq-pos after-val)))
                     (setf search-start after-val)))))
    (nreverse bindings)))

(defun functor-argument-declarations (args-text)
  (let ((inner (trim-sml-functor-arg-text args-text)))
    (unless (simple-sml-id-text-p inner)
      (without-sml-static-facts (esrap:parse 'sml-decs inner)))))

(defun functor-argument-module-name (target-name args-text)
  (format nil "~A.%ARG-~36R"
          target-name
          (sxhash args-text)))

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
  "The static checker's identifier status for pattern node PAT, if known."
  (sml-node-fact-info pat "pat"))

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
  `(register-sml-type-alias ,(target-sml-package-name)
                            ,(current-qualified-sml-name name)
                            ,target))

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
    (:datatype (mapcar #'second (third dec)))
    (:datatype-replication (mapcar #'cdr (replicated-constructors dec)))
    (:exception (list (second dec)))
    (:exception-alias (list (second dec)))
    (:local (declarations-bound-names (third dec)))
    (otherwise nil)))

(defun split-sml-module-names (names)
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

(defun pattern-type-bindings (pat type)
  (cond
    ((or (numberp pat) (stringp pat) (characterp pat) (eq pat :wild))
     nil)
    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     (list (cons (target-sml-symbol (second pat)) type)))
    ((and (listp pat) (eq (car pat) :pat-typed))
     (pattern-type-bindings (second pat) (third pat)))
    ((and (listp pat) (eq (car pat) :pat-as))
     (append (pattern-type-bindings (second pat) type)
             (pattern-type-bindings (third pat) type)))
    ((and (listp pat) (member (car pat) '(:pat-ctor :ctor)))
     (unless (pattern-constructor-name-p (second pat))
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
    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     (list (target-sml-symbol (second pat))))
    ((and (listp pat) (eq (car pat) :pat-typed))
     (pattern-bound-symbols (second pat)))
    ((and (listp pat) (eq (car pat) :pat-as))
     (append (pattern-bound-symbols (second pat))
             (pattern-bound-symbols (third pat))))
    ((and (listp pat) (member (car pat) '(:pat-ctor :ctor)))
     (unless (pattern-constructor-name-p (second pat))
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
           `((otherwise (sml-raise-named-exception "Match")))))))

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
  #+sbcl
  (if name
      `(sb-int:named-lambda ,(format nil "sml:~A" name) (,arg) ,body)
      `(lambda (,arg) ,body))
  #-sbcl
  (progn name `(lambda (,arg) ,body)))

(defun compile-fn-clauses (clauses &optional local-exceptions function-name outer-lexical-env)
  (let* ((arity (length (first (first clauses))))
         (tmp-args (loop repeat arity collect (gensym "ARG"))))
    (unless (every (lambda (clause) (= (length (first clause)) arity)) clauses)
      (error "All fun clauses must have the same arity: ~A" clauses))
    (reduce (lambda (arg body) (sml-lambda-form function-name arg body))
            tmp-args
            :from-end t
            :initial-value
            `(trivia:match ,(if (= arity 1)
                                (first tmp-args)
                                `(list :tuple ,@tmp-args))
               ,@(mapcar (lambda (clause)
                           (let* ((pat (compile-clause-pattern (first clause)))
                                  (var-map (pattern-variable-map pat local-exceptions))
                                  (branch-env (append var-map outer-lexical-env)))
                             `(,(compile-pat pat local-exceptions branch-env)
                               ,(compile-expr (second clause)
                                              local-exceptions
                                             branch-env))))
                         clauses)
               (_ (error "Match failure in function ~A ~S on value ~S"
                         ,(or function-name "<anonymous>")
                         ',clauses
                         ,(if (= arity 1)
                              (first tmp-args)
                              `(list :tuple ,@tmp-args))))))))

(defun compile-local-val-binding (pat expr body &optional local-exceptions lexical-env)
  (cond
    ((and (listp pat) (member (car pat) '(:pat-var :var)))
     `(let ((,(sml-lexical-symbol (second pat))
              ,(maybe-wrap-infix-value-initializer (second pat) expr)))
        ,body))
    ((eq pat :wild)
     (let ((tmp (gensym "IGNORED")))
       `(let ((,tmp ,expr))
          (declare (ignore ,tmp))
          ,body)))
    (t
     (let ((tmp (gensym "MATCHED"))
           (binding-env
             (append
              (mapcar (lambda (name)
                        (cons name (sml-lexical-symbol name)))
                      (pattern-bound-names pat local-exceptions lexical-env))
              lexical-env)))
       `(let ((,tmp ,expr))
          (trivia:match ,tmp
            (,(let ((*sml-module-prefix* nil))
                (compile-pat pat local-exceptions binding-env))
             ,body)
            (_ (error "Pattern match failure for local val pattern ~S on value ~S"
                      ',pat ,tmp))))))))

(defun compile-local-datatype-bindings (ctors body)
  (let ((bindings
          (mapcar (lambda (ctor)
                    (let ((name (sml-lexical-symbol (second ctor))))
                      (list name
                            (if (fourth ctor)
                                `(lambda (payload)
                                   (cons ',name payload))
                                `',name))))
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

(defun local-declaration-lexical-bindings (dec)
  (case (and (consp dec) (car dec))
    (:val
     (mapcar (lambda (name) (cons name (sml-lexical-symbol name)))
             (pattern-bound-names (second dec))))
    (:vals
     (mapcan #'local-declaration-lexical-bindings (cdr dec)))
    (:val-rec
     (list (cons (second dec) (sml-lexical-symbol (second dec)))))
    (:fun
     (list (cons (second dec) (sml-lexical-symbol (second dec)))))
    (:funs
     (mapcan #'local-declaration-lexical-bindings (cdr dec)))
    (:datatype
     (mapcar (lambda (ctor)
               (cons (second ctor) (sml-lexical-symbol (second ctor))))
             (third dec)))
    (:exception
     (list (cons (second dec) (sml-lexical-symbol (second dec)))))
    (:exception-alias
     (list (cons (second dec) (sml-lexical-symbol (second dec)))))
    (:local
     (mapcan #'local-declaration-lexical-bindings (third dec)))
    (otherwise
     nil)))

(defun local-open-symbol-bindings (dec)
  (when (eq (car dec) :open)
    (declaration-binding-symbol-bindings dec)))

(defun local-open-constructor-bindings (dec)
  (when (eq (car dec) :open)
    (declaration-constructor-symbol-bindings dec)))

(defun declarations-local-open-symbol-bindings (decs)
  (mapcan #'local-open-symbol-bindings decs))

(defun declarations-local-open-constructor-bindings (decs)
  (mapcan #'local-open-constructor-bindings decs))

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
                       (append open-constructor-bindings *sml-constructor-symbol-env*)))
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
         (body-env (append (mapcan #'local-declaration-lexical-bindings decs)
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
                                local-exceptions
                                lexical-env))
    ((eq (car dec) :vals)
     (compile-local-decls-into-body (cdr dec) body local-exceptions lexical-env))
	    ((eq (car dec) :fun)
	     (let ((name (sml-lexical-symbol (second dec))))
	       `(let ((,name nil))
	          (setf ,name ,(maybe-wrap-infix-value-initializer
	                        (second dec)
	                        (compile-fn-clauses (third dec)
	                                            local-exceptions
	                                            (second dec)
	                                            (acons (second dec)
	                                                   name
	                                                   lexical-env))))
	          ,body)))
    ((eq (car dec) :funs)
     ;; `fun f ... and g ...` is mutually recursive: bind every name before
     ;; any of the closures is created.
     (let* ((funs (cdr dec))
            (bindings (mapcar (lambda (fun-dec)
                                (cons (second fun-dec)
                                      (sml-lexical-symbol (second fun-dec))))
                              funs))
            (extended-env (append bindings lexical-env)))
       `(let ,(mapcar (lambda (binding) `(,(cdr binding) nil)) bindings)
          ,@(mapcar (lambda (fun-dec)
                      `(setf ,(cdr (assoc (second fun-dec) bindings :test #'equal))
                             ,(maybe-wrap-infix-value-initializer
                               (second fun-dec)
                               (compile-fn-clauses (third fun-dec)
                                                   local-exceptions
                                                   (second fun-dec)
                                                   extended-env))))
                    funs)
          ,body)))
    ((eq (car dec) :val-rec)
     (let ((name (sml-lexical-symbol (second dec))))
       `(let ((,name nil))
          (setf ,name ,(compile-expr (third dec)
                                     local-exceptions
                                     (acons (second dec) name lexical-env)))
          ,body)))
    ((eq (car dec) :datatype)
     (compile-local-datatype-bindings (third dec) body))
    ((member (car dec) '(:type :infix :datatype-replication :expr :signature :open
                         :structure-alias :structure-app :functor))
     body)
    ((eq (car dec) :structure)
     (compile-local-decls-into-body (third dec) body local-exceptions lexical-env))
    ((eq (car dec) :exception)
     (let* ((name (sml-lexical-symbol (second dec)))
            (arg-type (getf (cddr dec) :arg-type)))
       (if arg-type
           `(let ((,name (make-sml-exception-function ,(second dec))))
              (declare (ignorable ,name))
              ,body)
           `(let ((,name (make-sml-exception-constructor ,(second dec))))
              (declare (ignorable ,name))
              ,body))))
    ((eq (car dec) :exception-alias)
     (let ((name (sml-lexical-symbol (second dec)))
           (target (resolved-sml-symbol (third dec))))
       `(let ((,name (if (boundp ',target)
                         (symbol-value ',target)
                         (make-sml-exception-constructor ,(second dec)))))
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
            (local-bindings (mapcan #'local-declaration-lexical-bindings
                                    local-decs))
            (body-bindings (mapcan #'local-declaration-lexical-bindings body-decs))
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
                              (maybe-wrap-infix-value-initializer
                               (second pat)
                               (compile-expr expr local-exceptions lexical-env)))))
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
                 ,@(mapcar (lambda (sym) `(defparameter ,sym ,sym)) bound-symbols)
                 ,@(mapcar (lambda (binding)
                             (compile-type-registration-form (car binding) (cdr binding)))
                           typed-bindings)
                 ,(compile-export-form bound-symbols)
                 ,tmp))
              (_ (error "Pattern match failure in top-level val")))))))))

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

(defun compile-pat (pat &optional local-exceptions lexical-env)
  (cond
    ((numberp pat) pat)
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
                       (let ((it (gensym "CTOR-VALUE")))
                         `(guard1 (,it :type cons)
                                  (sml-constructor-tag-p (car ,it) ',ctor)
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
     (let ((record (gensym "RECORD")))
       `(guard1 ,record
                (sml-record-p ,record)
                ,@(mapcan (lambda (field)
                            `((sml-record-select ,record ,(first field))
                              ,(compile-pat (second field)
                                            local-exceptions
                                            lexical-env)))
                          (record-fields-sorted-by-label (cdr pat))))))

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
             (if bits `(ldb (byte ,bits 0) ,form) form)))
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
             (t `(,comparison ,x ,y)))))
        ((member name '("+" "-" "*") :test #'string=)
         (destructuring-bind (x y) args
           (wrap `(,(intern name :cl) ,x ,y))))
        ((string= name "div") `(sml-int-div ,@args))
        ((string= name "mod") `(sml-int-mod ,@args))))))

(defun compile-overloaded-identifier-value (name tyname)
  "A curried function value for overloaded identifier NAME at TYNAME, or NIL."
  (let ((x (gensym "X"))
        (y (gensym "Y")))
    (if (member name '("~" "abs") :test #'string=)
        (let ((body (compile-overloaded-operation name tyname (list x))))
          (and body `(lambda (,x) ,body)))
        (let ((body (compile-overloaded-operation name tyname (list x y))))
          (and body `(lambda (,x) (lambda (,y) ,body)))))))

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
          (if name
              (cond
                ;; Unary application, or `op +` applied to a pair.
                ((and (eq (car head) :var)
                      (member name '("~" "abs") :test #'string=))
                 (compile-overloaded-operation name tyname (compile-args name tyname arg)))
                ((and (consp arg) (eq (car arg) :tuple) (= (length arg) 3))
                 (compile-overloaded-operation
                  name tyname (compile-args name tyname (second arg) (third arg)))))
              ;; Curried infix application: (:app (:app op x) y).
              (when (and (consp head) (eq (car head) :app))
                (multiple-value-bind (name tyname)
                    (overloaded-identifier-instance (second head))
                  (when name
                    (compile-overloaded-operation
                     name tyname (compile-args name tyname (third head) arg)))))))))))

(defun compile-checked-infix-application (ast local-exceptions lexical-env)
  "Compile `x op y`, parsed as the curried (:app (:app (:var op) x) y), as
the application of OP to the pair (x, y) when the static checker confirms OP
is a variable (and not overloaded).  Infix values are wrapped by
SML-TUPLE-OR-CURRIED-BINARY, which otherwise has to guess from the shape of
X: a pair X was taken for both arguments.  Returns NIL otherwise."
  (when (and (consp ast) (eq (car ast) :app)
             (consp (second ast)) (eq (car (second ast)) :app))
    (let ((op (second (second ast))))
      (when (and (consp op) (eq (car op) :var)
                 (sml-binary-infix-value-name-p (second op))
                 (equal (sml-node-fact-info op "id") "v")
                 (not (sml-overloaded-instance op)))
        (let ((function (resolved-sml-symbol (second op) lexical-env)))
          ;; Built-in primitives are plain curried functions.
          (when (symbolp function)
            `(funcall ,function
                      (list :tuple
                            ,(compile-expr (third (second ast))
                                           local-exceptions lexical-env)
                            ,(compile-expr (third ast)
                                           local-exceptions lexical-env)))))))))

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
     `(sml-tuple-or-curried-binary
       ,(multiple-value-bind (name tyname) (overloaded-identifier-instance ast)
          (or (and name (compile-overloaded-identifier-value name tyname))
              (compile-expr `(:var ,(second ast)) local-exceptions lexical-env)))))

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
     (logand most-positive-fixnum (- (second (third ast)))))

    ((compile-overloaded-application ast local-exceptions lexical-env))
    ((compile-checked-infix-application ast local-exceptions lexical-env))

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
                       branches))))))

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
     `(make-sml-record
       (list ,@(mapcar (lambda (field)
                         `(cons ,(first field)
                                ,(compile-expr (second field) local-exceptions lexical-env)))
                       (record-fields-sorted-by-label (cdr ast))))))

    ((and (listp ast) (eq (car ast) :unit))
     `(list :tuple))

    ((and (listp ast) (eq (car ast) :tuple))
     `(list :tuple ,@(mapcar (lambda (expr)
                               (compile-expr expr local-exceptions lexical-env))
                             (cdr ast))))

    ((and (listp ast) (eq (car ast) :fn))
     (let ((clauses (second ast))
           (tmp-arg (gensym "ARG")))
       `(lambda (,tmp-arg)
          (trivia:match ,tmp-arg
            ,@(mapcar (lambda (branch)
	                        (let* ((var-map (pattern-variable-map (first branch)
	                                                              local-exceptions))
                               (branch-env (append var-map lexical-env)))
                          `(,(compile-pat (first branch) local-exceptions branch-env)
                            ,(compile-expr (second branch) local-exceptions branch-env))))
                      clauses)
            (_ (error "Match failure in anonymous function ~S on value ~S"
                      ',clauses ,tmp-arg))))))

    (t (error "Unknown AST: ~A" ast))))

(defun compile-decl (ast &optional local-exceptions lexical-env)
  "Compiles top level declarations."
  (cond
    ((eq (car ast) :val)
     (compile-top-level-val (second ast) (third ast) local-exceptions
                            (getf (cdddr ast) :type) lexical-env))
    ((eq (car ast) :vals)
     `(progn ,@(mapcar (lambda (val-dec)
                         (compile-decl val-dec local-exceptions lexical-env))
                       (cdr ast))))
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
    ((eq (car ast) :fun)
     (let* ((name (target-sml-symbol (second ast)))
            (fun-type (loop repeat (length (first (first (third ast))))
                            for result = :unknown then `(:fn :unknown ,result)
                            finally (return result))))
	       `(progn
	          (declaim (special ,name))
	          (defparameter ,name ,(let ((*sml-binding-symbol-env*
	                                       (acons (second ast) name *sml-binding-symbol-env*)))
	                                  (maybe-wrap-infix-value-initializer
	                                   (second ast)
	                                   (compile-fn-clauses (third ast)
	                                                       local-exceptions
	                                                       (second ast)
	                                                       (acons (second ast)
	                                                              name
	                                                              lexical-env)))))
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
	                               ,(compile-type-registration-form cname `(:fn ,(or (getf (cddr c) :arg-type) :unknown) ,(second ast)))
	                               ,(compile-export-form (list cname)))
	                            ;; If no args, it's just the keyword constant
	                            `(progn
	                               (defparameter ,cname ',keyword)
	                               (register-sml-constructor ',cname)
	                               ,(compile-type-registration-form cname (second ast))
	                               ,(compile-export-form (list cname))))))
                    ctors)
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
	     (let* ((args-text (fourth ast))
	            (target-name (current-qualified-sml-name (second ast)))
	            (argument (functor-argument-structure-name args-text))
	            (argument-decs (functor-argument-declarations args-text))
	            (argument-module
	              (and argument-decs
	                   (functor-argument-module-name target-name args-text)))
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
	                                            lexical-env))))
	            (value-bindings
	              (unless argument-decs
	                (functor-argument-value-bindings args-text))))
	       `(progn
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
	                          (and argument
	                               (resolve-structure-prefix argument)))
	           :value-bindings ',value-bindings))))
	    ((eq (car ast) :functor)
	     (let* ((module-name (current-qualified-sml-name (second ast)))
	            (param-name (getf (cdddr ast) :param))
	            (members (declarations-bound-names (third ast)))
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
              ,(compile-type-registration-form name `(:fn ,arg-type "exn"))
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
              (mapcan #'local-declaration-lexical-bindings local-decs))
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
     (let ((bindings nil))
       (dolist (name (split-sml-module-names (second dec)))
         (let ((source (resolve-structure-prefix name)))
           (dolist (member (lookup-sml-module-members-for-compiler
                            name
                            *sml-local-structure-members*))
             (push (cons member
                         (sml-symbol (qualify-sml-name source member)))
                   bindings))))
       (nreverse bindings)))
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
     (let ((bindings nil))
       (dolist (name (split-sml-module-names (second dec)))
         (let ((source (resolve-structure-prefix name)))
           (dolist (member (lookup-sml-module-members-for-compiler
                            name
                            *sml-local-structure-members*))
             (let* ((source-symbol (sml-symbol (qualify-sml-name source member)))
                    (canonical (sml-constructor-canonical-symbol source-symbol)))
               (when canonical
                 (push (cons member canonical) bindings))))))
       (nreverse bindings)))
    (:local
     (mapcan #'declaration-constructor-symbol-bindings (third dec)))
    (otherwise
     nil)))

(defun compile-program-decls (decs &optional (local-exceptions nil) lexical-env)
  (if (null decs)
      nil
      (let* ((dec (first decs))
             (form (compile-decl dec local-exceptions lexical-env))
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
