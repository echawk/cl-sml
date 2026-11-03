(in-package #:cl-sml)

;;; The SML97 parser: recursive descent over the tokens of LEX-SML, with
;;; infix expressions and patterns resolved by precedence climbing over a
;;; scoped fixity environment (Definition of Standard ML (Revised), section
;;; 2.6 and Appendices A-B, chapter 3 for Modules).
;;;
;;; Entry points: PARSE-SML-PROGRAM, PARSE-SML-EXPRESSION,
;;; PARSE-SML-DECLARATIONS, PARSE-SML-PATTERN and PARSE-SML-TYPE.  Syntax
;;; errors are SML-SYNTAX-ERRORs reporting the line, the column and what was
;;; expected.
;;;
;;; The parser also tracks identifier status (Definition section 4.1): which
;;; names are value constructors or exception constructors.  Patterns need it
;;; (`fun f red = ...` matches the constructor `red` but binds a variable
;;; `x`), and with the HaMLet checker its static facts take precedence.
;;; Fixity and status persist per SML package across compilation units, so
;;; that a REPL phrase or a `use`d file sees the top-level declarations of
;;; earlier ones.
;;;
;;; AST summary (strings are SML names; long names are dotted strings):
;;;
;;; Types
;;;   (:ty-var "'a")  (:ty-con "list" (ty...))  (:ty-tuple ty ty...)
;;;   (:ty-arrow ty ty)  (:ty-record ("label" ty)...)
;;; Expressions
;;;   integer, double-float, string and character constants; (:word n)
;;;   (:var name)  (:ctor name)      identifier; :ctor if a constructor
;;;   (:app f x)                     an infix application `a op b' is
;;;                                  (:app (:var "op") (:tuple a b))
;;;   (:tuple e...)  (:unit)  (:record ("label" e)...)  (:selector "label")
;;;   (:list e...)  (:seq e...)  (:let decs (e...))  (:typed e ty)
;;;   (:andalso a b)  (:orelse a b)  (:if c a b)  (:while c e)
;;;   (:case e (pat e)...)  (:fn ((pat e)...))  (:handle e ((pat e)...))
;;;   (:raise e)
;;; Patterns
;;;   constants as above; :wild; (:pat-var name)
;;;   (:pat-ctor name [:status s])   s is the identifier status when known:
;;;                                  "c"/"c1" constructor, "e"/"e1" exception
;;;   (:pat-app ctor-pat pat)  (:pat-cons head tail)  (:pat-nil)
;;;   (:pat-tuple pat...)  (:pat-unit)  (:pat-record ("label" pat)... [:record-rest])
;;;   (:pat-typed pat ty)  (:pat-as var-pat pat)
;;; Declarations (a declaration sequence is a list)
;;;   (:val pat e [:type ty] [:tyvars tvs])       `val pat : ty = e'
;;;   (:vals val...)  (:val-rec name e [:type ty] [:tyvars tvs])
;;;   (:val-recs val-rec...)                      `val rec f = .. and g = ..'
;;;   (:fun name ((params e)...) [:tyvars tvs])  (:funs fun...)
;;;       an infix head `fun x ++ y = e' has the single parameter (x, y)
;;;   (:type name ty [:tyvars tvs])
;;;   (:datatype name ctor-defs :datbinds ((tyvars name ctor-defs)...)
;;;              :withtype ((tyvars name ty)...))
;;;       ctor-def: (:ctor-def name :has-args bool :arg-type ty)
;;;   (:datatype-replication name "longtycon" [:constructors ((status . name)...)])
;;;   (:exception name :arg-type ty)  (:exception-alias name "longvid")
;;;   (:local decs decs [:abstype t])            abstype is a :local whose
;;;                                               first part is the datatype
;;;   (:open ("longstrid"...))  (:infix "infix"|"infixr"|"nonfix" prec (name...))
;;;   (:expr e)                                   top-level expression
;;;   Bindings joined by `and' that the compiler treats one by one are
;;;   consecutive declarations, the later ones marked :and t.
;;; Modules
;;;   strexp: (:struct decs)  (:str-id "longstrid")  (:str-app funid strexp)
;;;           (:str-constraint strexp sigexp :opaque bool)  (:str-let decs strexp)
;;;   sigexp: (:sig specs)  (:sig-id name)  (:sig-where sigexp tyvars "longtycon" ty)
;;;   specs:  (:spec-val name ty)  (:spec-type tyvars name [ty])
;;;           (:spec-eqtype tyvars name)  (:spec-datatype ((tyvars name ctor-defs)...))
;;;           (:spec-datatype-replication name "longtycon")
;;;           (:spec-exception name ty)  (:spec-structure name sigexp)
;;;           (:spec-include sigexp)  (:spec-sharing-type ("longtycon"...))
;;;           (:spec-sharing ("longstrid"...))
;;;   A structure binding becomes, by the shape of its structure expression
;;;   once constraints are removed (:STREXP keeps the full expression):
;;;     (:structure name decs [:sigexp s :opaque bool] :strexp e)
;;;     (:structure-alias name "longstrid" [...] :strexp e)
;;;     (:structure-app name funid arg-strexp [...] :strexp e)
;;;   (:signature name sigexp)
;;;   (:functor name decs :param strid-or-nil :param-sigexp s :param-specs specs
;;;             :result-sigexp s :opaque bool :strexp body)
;;;
;;; The parser records the source offset of identifier, operator, word
;;; constant, structure binding and datatype replication nodes with
;;; NOTE-SML-AST-POSITION (see static-facts.lisp).

;;;; Token access

(defvar *sml-parse-text* "")
(defvar *sml-parse-tokens* (vector (make-sml-token :eof nil 0 0)))
(defvar *sml-parse-index* 0)
(declaim (type simple-vector *sml-parse-tokens*)
         (type fixnum *sml-parse-index*))

(declaim (inline current-sml-token))
(defun current-sml-token ()
  (svref *sml-parse-tokens* *sml-parse-index*))

(defun sml-token-ahead (k)
  (svref *sml-parse-tokens*
         (min (+ *sml-parse-index* k) (1- (length *sml-parse-tokens*)))))

(defun next-sml-token ()
  "Consume and return the current token."
  (prog1 (current-sml-token)
    (when (< *sml-parse-index* (1- (length *sml-parse-tokens*)))
      (incf *sml-parse-index*))))

(declaim (inline sml-token-reserved-p))
(defun sml-token-reserved-p (token value)
  (and (eq (sml-token-kind token) :reserved)
       (eq (sml-token-value token) value)))

(defun at-sml-reserved-p (value)
  (sml-token-reserved-p (current-sml-token) value))

(defun at-sml-eof-p ()
  (eq (sml-token-kind (current-sml-token)) :eof))

(defun accept-sml-reserved (value)
  (when (at-sml-reserved-p value)
    (next-sml-token)))

(defparameter *sml-reserved-symbol-texts*
  '((:lparen . "(") (:rparen . ")") (:lbracket . "[") (:rbracket . "]")
    (:lbrace . "{") (:rbrace . "}") (:comma . ",") (:semicolon . ";")
    (:dots . "...") (:underscore . "_") (:colon . ":") (:seal . ":>")
    (:bar . "|") (:equals . "=") (:darrow . "=>") (:arrow . "->") (:hash . "#")))

(defun sml-reserved-text (value)
  (or (cdr (assoc value *sml-reserved-symbol-texts*))
      (string-downcase (symbol-name value))))

(defun describe-sml-token (token)
  (case (sml-token-kind token)
    (:eof "end of input")
    (:reserved (format nil "`~A'" (sml-reserved-text (sml-token-value token))))
    ((:alnum :symbolic :long) (format nil "identifier `~A'" (sml-token-value token)))
    (:tyvar (format nil "type variable `~A'" (sml-token-value token)))
    (t (format nil "constant `~A'"
               (subseq *sml-parse-text* (sml-token-start token) (sml-token-end token))))))

(defun sml-parse-error-at (token control &rest arguments)
  (apply #'sml-syntax-error *sml-parse-text* (sml-token-start token) control arguments))

(defun sml-parse-expected (what)
  (sml-parse-error-at (current-sml-token) "expected ~A but found ~A"
                      what (describe-sml-token (current-sml-token))))

(defun expect-sml-reserved (value)
  (or (accept-sml-reserved value)
      (sml-parse-expected (format nil "`~A'" (sml-reserved-text value)))))

(defun sml-identifier-token-p (token)
  (member (sml-token-kind token) '(:alnum :symbolic)))

(defun sml-vid-token-p (token)
  "An identifier usable as a value identifier, including `='."
  (or (sml-identifier-token-p token)
      (sml-token-reserved-p token :equals)))

(defun sml-vid-token-name (token)
  (if (sml-token-reserved-p token :equals) "=" (sml-token-value token)))

(defun expect-sml-alnum-id (what)
  (let ((token (current-sml-token)))
    (unless (eq (sml-token-kind token) :alnum)
      (sml-parse-expected what))
    (next-sml-token)))

(defun expect-sml-long-id (what)
  "An alphanumeric identifier, possibly qualified."
  (let ((token (current-sml-token)))
    (unless (member (sml-token-kind token) '(:alnum :long))
      (sml-parse-expected what))
    (next-sml-token)))

(defun split-sml-long-name (name)
  "The structure identifiers and the final identifier of long NAME."
  (let ((parts nil)
        (start 0))
    (loop for dot = (position #\. name :start start)
          while dot
          do (push (subseq name start dot) parts)
             (setf start (1+ dot)))
    (values (nreverse parts) (subseq name start))))

;;;; Identifier status and fixity environments
;;;
;;; The environment is a list of SML-SCOPEs, innermost first.  Declarations
;;; bind in the first scope.  `open S` pushes S's environment (shared and
;;; never modified) and a fresh scope above it.  A scope marked OPAQUE-P
;;; stands for a structure whose contents are unknown: a value, structure or
;;; type constructor not found above it has unknown status.

(defstruct (sml-scope (:constructor make-sml-scope ()))
  (values nil)      ; name -> status "v" "c" "c1" "e" "e1"
  (structures nil)  ; strid -> structure environment (an SML-SCOPE) or :unknown
  (tycons nil)      ; tycon -> list of (constructor . status), or :plain
  (fixity nil)      ; name -> (:infix . prec), (:infixr . prec) or :nonfix
  (signatures nil)  ; sigid -> environment of its specs, or :unknown
  (functors nil)    ; funid -> environment of its result, or :unknown
  (opaque-p nil))

;;; The basis's environment is saved in its FASL (src/basis.lisp).  Hash
;;; tables are not externalizable, so a scope is rebuilt from alists.
(defmethod make-load-form ((scope sml-scope) &optional environment)
  (declare (ignore environment))
  (values `(make-sml-scope)
          `(restore-sml-scope
            ,scope
            ',(sml-scope-opaque-p scope)
            ',(loop for namespace in '(:values :structures :tycons :fixity
                                       :signatures :functors)
                    for table = (sml-scope-table scope namespace)
                    when table
                      collect (cons namespace
                                    (loop for name being the hash-keys of table
                                            using (hash-value value)
                                          collect (cons name value)))))))

(defun restore-sml-scope (scope opaque-p tables)
  (setf (sml-scope-opaque-p scope) opaque-p)
  (loop for (namespace . entries) in tables
        do (loop for (name . value) in entries
                 do (sml-scope-bind namespace name value scope)))
  scope)

(defvar *sml-scopes* nil
  "The identifier status and fixity environment of the current parse.")

(defun sml-scope-table (scope namespace)
  (ecase namespace
    (:values (sml-scope-values scope))
    (:structures (sml-scope-structures scope))
    (:tycons (sml-scope-tycons scope))
    (:fixity (sml-scope-fixity scope))
    (:signatures (sml-scope-signatures scope))
    (:functors (sml-scope-functors scope))))

(defun (setf sml-scope-table) (table scope namespace)
  (ecase namespace
    (:values (setf (sml-scope-values scope) table))
    (:structures (setf (sml-scope-structures scope) table))
    (:tycons (setf (sml-scope-tycons scope) table))
    (:fixity (setf (sml-scope-fixity scope) table))
    (:signatures (setf (sml-scope-signatures scope) table))
    (:functors (setf (sml-scope-functors scope) table))))

(defun sml-scope-bind (namespace name value &optional (scope (first *sml-scopes*)))
  (let ((table (or (sml-scope-table scope namespace)
                   (setf (sml-scope-table scope namespace)
                         (make-hash-table :test #'equal)))))
    (setf (gethash name table) value)))

(defun sml-opaque-namespace-p (namespace)
  (member namespace '(:values :structures :tycons)))

(defun sml-scope-lookup (namespace name &optional (scopes *sml-scopes*))
  "NAME's entry in NAMESPACE, :UNKNOWN if an opaque scope hides it, or NIL."
  (dolist (scope scopes nil)
    (let ((table (sml-scope-table scope namespace)))
      (when table
        (multiple-value-bind (value found) (gethash name table)
          (when found
            (return value)))))
    (when (and (sml-scope-opaque-p scope) (sml-opaque-namespace-p namespace))
      (return :unknown))))

(defun sml-environment-lookup (environment namespace name)
  "NAME's entry in structure ENVIRONMENT, or :UNKNOWN."
  (if (sml-scope-p environment)
      (let ((table (sml-scope-table environment namespace)))
        (multiple-value-bind (value found) (if table (gethash name table) (values nil nil))
          (if found value :unknown)))
      :unknown))

(defun merge-sml-scopes (frames target namespaces)
  "Copy the bindings of FRAMES (innermost first) into scope TARGET."
  (dolist (frame (reverse frames) target)
    (when (sml-scope-opaque-p frame)
      ;; Everything bound before an opaque scope may have been shadowed.
      (dolist (namespace '(:values :structures :tycons))
        (when (member namespace namespaces)
          (setf (sml-scope-table target namespace) nil)))
      (setf (sml-scope-opaque-p target) t))
    (dolist (namespace namespaces)
      (let ((table (sml-scope-table frame namespace)))
        (when table
          (maphash (lambda (name value) (sml-scope-bind namespace name value target))
                   table))))))

(defun collapse-sml-scopes (frames &optional (namespaces '(:values :structures :tycons)))
  "A fresh scope with the bindings of FRAMES (innermost first)."
  (merge-sml-scopes frames (make-sml-scope) namespaces))

(defparameter +sml-all-namespaces+
  '(:values :structures :tycons :fixity :signatures :functors))

(defun push-sml-scope (scope)
  "Make SCOPE's bindings visible, with a fresh scope above it for what follows."
  (setf *sml-scopes* (list* (make-sml-scope) scope *sml-scopes*)))

(defun open-sml-environment (environment)
  (push-sml-scope (if (sml-scope-p environment)
                      environment
                      (let ((opaque (make-sml-scope)))
                        (setf (sml-scope-opaque-p opaque) t)
                        opaque))))

(defmacro with-sml-scope (() &body body)
  `(let ((*sml-scopes* (cons (make-sml-scope) *sml-scopes*)))
     ,@body))

(defun sml-scope-frames-since (base)
  "The scopes pushed since *SML-SCOPES* was BASE."
  (ldiff *sml-scopes* base))

(defun make-sml-builtin-scope ()
  "Fixity and identifier status of the initial basis."
  (let ((scope (make-sml-scope)))
    (loop for (names fixity) in '((("*" "/" "div" "mod") (:infix . 7))
                                  (("+" "-" "^") (:infix . 6))
                                  (("::" "@") (:infixr . 5))
                                  (("=" "<>" ">" ">=" "<" "<=") (:infix . 4))
                                  ((":=" "o") (:infix . 3))
                                  (("before") (:infix . 0)))
          do (dolist (name names)
               (sml-scope-bind :fixity name fixity scope)))
    (loop for (tycon . constructors) in '(("bool" ("true" . "c") ("false" . "c"))
                                          ("list" ("nil" . "c") ("::" . "c1"))
                                          ("ref" ("ref" . "c1"))
                                          ("option" ("NONE" . "c") ("SOME" . "c1"))
                                          ("order" ("LESS" . "c") ("EQUAL" . "c")
                                           ("GREATER" . "c")))
          do (sml-scope-bind :tycons tycon constructors scope)
             (loop for (name . status) in constructors
                   do (sml-scope-bind :values name status scope)))
    (dolist (tycon '("int" "word" "real" "char" "string" "unit" "exn"))
      (sml-scope-bind :tycons tycon :plain scope))
    (dolist (name '("Bind" "Chr" "Div" "Domain" "Empty" "Match" "Option"
                    "Overflow" "Size" "Subscript"))
      (sml-scope-bind :values name "e" scope))
    (sml-scope-bind :values "Fail" "e1" scope)
    scope))

(defvar *sml-builtin-scope* (make-sml-builtin-scope))

(defvar *sml-package-scopes* (make-portable-weak-eq-hash-table)
  "SML package -> the environment its top-level declarations have built.")

(defun sml-package-parse-scopes (package)
  "The environment PACKAGE's top-level declarations have built; for a
package without declarations yet, the basis's (see SML-BASIS-PACKAGE)."
  (let ((package (ensure-sml-package package)))
    (or (gethash package *sml-package-scopes*)
        (let ((basis (sml-basis-package)))
          (and basis
               (not (eq basis package))
               (gethash basis *sml-package-scopes*)))
        (list *sml-builtin-scope*))))

(defun (setf sml-package-parse-scopes) (scopes package)
  (setf (gethash (ensure-sml-package package) *sml-package-scopes*) scopes))

(defun sml-fixity (name)
  (or (sml-scope-lookup :fixity name) :nonfix))

(defun sml-infix-name-p (name)
  (consp (sml-fixity name)))

(defun sml-lookup-structure (name)
  "The environment of long structure identifier NAME, or :UNKNOWN."
  (multiple-value-bind (qualifiers last) (split-sml-long-name name)
    (let ((environment (if qualifiers
                           (or (sml-scope-lookup :structures (first qualifiers)) :unknown)
                           (or (sml-scope-lookup :structures last) :unknown))))
      (when qualifiers
        (dolist (part (append (rest qualifiers) (list last)))
          (setf environment (sml-environment-lookup environment :structures part))))
      environment)))

(defun sml-lookup-long (namespace name)
  "NAME's entry in NAMESPACE: NIL if unbound, :UNKNOWN if not known."
  (multiple-value-bind (qualifiers last) (split-sml-long-name name)
    (if qualifiers
        (sml-environment-lookup
         (sml-lookup-structure (format nil "~{~A~^.~}" qualifiers))
         namespace last)
        (sml-scope-lookup namespace last))))

;;; Statuses use the encoding of the static facts: "v", "c", "c1", "e", "e1".

(defun sml-identifier-status (name start kind)
  "The status of identifier NAME at offset START, in a pattern (KIND :PAT)
or an expression (:EXP): a status string, :UNKNOWN, or NIL for an unbound
name (a variable)."
  (or (and *sml-static-facts*
           (if (eq kind :pat)
               (sml-pattern-status-at start name)
               (sml-expression-status-at start name)))
      (sml-lookup-long :values name)))

(defun sml-id-last-segment (name)
  (let ((pos (position #\. name :from-end t)))
    (if pos (subseq name (1+ pos)) name)))

(defun sml-constructor-looking-id-p (name)
  "Without status information, guess from the spelling (cl-sml's former
heuristic): capitalized names are constructors."
  (let ((segment (sml-id-last-segment name)))
    (or (member segment '("true" "false" "nil") :test #'string=)
        (and (plusp (length segment))
             (upper-case-p (char segment 0))
             (or (not (position #\_ segment))
                 (string= segment (string-upcase segment)))))))

(defun make-sml-pattern-id (name start)
  "The pattern node for identifier NAME at offset START."
  (let ((status (sml-identifier-status name start :pat)))
    (note-sml-ast-position
     (cond
       ((stringp status)
        (if (sml-status-constructor-p status)
            (list :pat-ctor name :status status)
            (list :pat-var name)))
       ((or (eq status :unknown) (sml-long-name-p name))
        ;; A long identifier in a pattern is always a constructor.
        (if (or (sml-long-name-p name) (sml-constructor-looking-id-p name))
            (list :pat-ctor name)
            (list :pat-var name)))
       (t (list :pat-var name)))
     start)))

(defun make-sml-pattern-constructor (name start)
  "The node for NAME applied as a constructor in a pattern."
  (let ((status (sml-identifier-status name start :pat)))
    (note-sml-ast-position
     (if (and (stringp status) (sml-status-constructor-p status))
         (list :pat-ctor name :status status)
         (list :pat-ctor name))
     start)))

(defun make-sml-expression-id (name start)
  "The expression node for identifier NAME at offset START."
  (let ((status (if (string= name "ref")
                    "v"                 ; compiled as a primitive function
                    (sml-identifier-status name start :exp))))
    (note-sml-ast-position
     (list (cond
             ((stringp status) (if (sml-status-constructor-p status) :ctor :var))
             ((eq status :unknown) (if (sml-constructor-looking-id-p name) :ctor :var))
             (t :var))
           name)
     start)))

;;;; Types

(defun sml-tycon-token-p (token)
  "Can TOKEN be a (long) type constructor: any identifier but `*'
(Definition section 2.4)."
  (or (member (sml-token-kind token) '(:alnum :long))
      (and (eq (sml-token-kind token) :symbolic)
           (string/= (sml-token-value token) "*"))))

(defun expect-sml-tycon (&optional longp)
  "A type constructor, qualified only when LONGP."
  (let ((token (current-sml-token)))
    (unless (and (sml-tycon-token-p token)
                 (or longp (not (eq (sml-token-kind token) :long))))
      (sml-parse-expected "a type constructor"))
    (next-sml-token)))

(defun at-sml-star-p ()
  (let ((token (current-sml-token)))
    (and (eq (sml-token-kind token) :symbolic)
         (string= (sml-token-value token) "*"))))

(defun parse-sml-ty ()
  (let ((ty (parse-sml-tuple-ty)))
    (if (accept-sml-reserved :arrow)
        (list :ty-arrow ty (parse-sml-ty))
        ty)))

(defun parse-sml-tuple-ty ()
  (let ((first (parse-sml-app-ty)))
    (if (at-sml-star-p)
        (let ((items (list first)))
          (loop while (at-sml-star-p)
                do (next-sml-token)
                   (push (parse-sml-app-ty) items))
          (cons :ty-tuple (nreverse items)))
        first)))

(defun parse-sml-app-ty ()
  (let ((ty (parse-sml-atomic-ty)))
    (loop while (sml-tycon-token-p (current-sml-token))
          do (setf ty (list :ty-con (sml-token-value (next-sml-token)) (list ty))))
    ty))

(defun parse-sml-atomic-ty ()
  (let ((token (current-sml-token)))
    (case (sml-token-kind token)
      (:tyvar
       (next-sml-token)
       (list :ty-var (sml-token-value token)))
      (t
       (cond
         ((sml-tycon-token-p token)
          (next-sml-token)
          (list :ty-con (sml-token-value token) nil))
         ((accept-sml-reserved :lbrace)
          (let ((fields nil))
            (unless (at-sml-reserved-p :rbrace)
              (loop
                (let ((label (parse-sml-label)))
                  (expect-sml-reserved :colon)
                  (push (list label (parse-sml-ty)) fields))
                (unless (accept-sml-reserved :comma)
                  (return))))
            (expect-sml-reserved :rbrace)
            (cons :ty-record (nreverse fields))))
         ((accept-sml-reserved :lparen)
          (let ((first (parse-sml-ty)))
            (if (accept-sml-reserved :comma)
                (let ((args (list first)))
                  (loop do (push (parse-sml-ty) args)
                        while (accept-sml-reserved :comma))
                  (expect-sml-reserved :rparen)
                  (unless (sml-tycon-token-p (current-sml-token))
                    (sml-parse-expected "a type constructor after a type argument sequence"))
                  (list :ty-con (sml-token-value (next-sml-token)) (nreverse args)))
                (progn
                  (expect-sml-reserved :rparen)
                  first))))
         (t (sml-parse-expected "a type")))))))

(defun parse-sml-tyvarseq ()
  "An optional type variable sequence: 'a or ('a, 'b)."
  (cond
    ((eq (sml-token-kind (current-sml-token)) :tyvar)
     (list (sml-token-value (next-sml-token))))
    ((and (at-sml-reserved-p :lparen)
          (eq (sml-token-kind (sml-token-ahead 1)) :tyvar))
     (next-sml-token)
     (let ((tyvars nil))
       (loop
         (let ((token (current-sml-token)))
           (unless (eq (sml-token-kind token) :tyvar)
             (sml-parse-expected "a type variable"))
           (push (sml-token-value (next-sml-token)) tyvars))
         (unless (accept-sml-reserved :comma)
           (return)))
       (expect-sml-reserved :rparen)
       (nreverse tyvars)))
    (t nil)))

(defun parse-sml-label ()
  "A record label: an identifier or a numeral 1, 2, ..."
  (let ((token (current-sml-token)))
    (case (sml-token-kind token)
      ((:alnum :symbolic)
       (next-sml-token)
       (sml-token-value token))
      (:int
       (let ((text (subseq *sml-parse-text* (sml-token-start token) (sml-token-end token))))
         (unless (and (every #'digit-char-p text) (char/= (char text 0) #\0))
           (sml-parse-expected "a record label"))
         (next-sml-token)
         text))
      (t (sml-parse-expected "a record label")))))

;;; Types as data, for the compiler and the REPL.

(defun sml-type-ast->string (ty)
  "SML syntax for type AST TY."
  (labels ((walk (ty context)
             ;; CONTEXT: 0 anywhere, 1 left of ->, 2 tuple component,
             ;; 3 type constructor argument
             (ecase (car ty)
               (:ty-var (second ty))
               (:ty-con
                (let ((args (third ty)))
                  (cond
                    ((null args) (second ty))
                    ((null (rest args))
                     (format nil "~A ~A" (walk (first args) 3) (second ty)))
                    (t (format nil "(~{~A~^, ~}) ~A"
                               (mapcar (lambda (arg) (walk arg 0)) args)
                               (second ty))))))
               (:ty-tuple
                (let ((text (format nil "~{~A~^ * ~}"
                                    (mapcar (lambda (item) (walk item 2)) (rest ty)))))
                  (if (>= context 2) (format nil "(~A)" text) text)))
               (:ty-arrow
                (let ((text (format nil "~A -> ~A" (walk (second ty) 1) (walk (third ty) 0))))
                  (if (>= context 1) (format nil "(~A)" text) text)))
               (:ty-record
                (format nil "{~{~A~^, ~}}"
                        (mapcar (lambda (field)
                                  (format nil "~A : ~A" (first field) (walk (second field) 0)))
                                (rest ty)))))))
    (walk ty 0)))

(defun sml-type-ast->binding-type (ty)
  "TY in the representation of REGISTER-SML-BINDING-TYPE: a type name string,
(:list ty), (:option ty), (:tuple ty...), (:record (label . ty)...) or
(:fn ty ty)."
  (if (not (and (consp ty) (keywordp (car ty))))
      ty
      (ecase (car ty)
        (:ty-var (second ty))
        (:ty-con
         (let ((name (second ty))
               (args (third ty)))
           (cond
             ((null args) name)
             ((and (string= name "list") (= (length args) 1))
              (list :list (sml-type-ast->binding-type (first args))))
             ((and (string= name "option") (= (length args) 1))
              (list :option (sml-type-ast->binding-type (first args))))
             ((and (string= name "ref") (= (length args) 1)) "ref")
             (t (sml-type-ast->string ty)))))
        (:ty-tuple (cons :tuple (mapcar #'sml-type-ast->binding-type (rest ty))))
        (:ty-arrow (list :fn
                         (sml-type-ast->binding-type (second ty))
                         (sml-type-ast->binding-type (third ty))))
        (:ty-record
         (if (rest ty)
             (cons :record
                   (sort (mapcar (lambda (field)
                                   (cons (first field)
                                         (sml-type-ast->binding-type (second field))))
                                 (rest ty))
                         #'string< :key #'car))
             "unit")))))

;;;; Infix resolution
;;;
;;; An infix expression or pattern is read as a flat list of atoms and
;;; operator items; juxtaposed atoms are applications, and the operators are
;;; resolved by precedence climbing.

(defstruct (sml-operator-item (:constructor make-sml-operator-item (name start fixity)))
  name start fixity)

(defun sml-operator-precedence (item)
  (cdr (sml-operator-item-fixity item)))

(defun sml-operator-right-p (item)
  (eq (car (sml-operator-item-fixity item)) :infixr))

(defun take-sml-operator-item ()
  (let* ((token (next-sml-token))
         (name (sml-vid-token-name token)))
    (make-sml-operator-item name (sml-token-start token) (sml-fixity name))))

(defvar *sml-pattern-start* 0
  "The offset of the pattern being read, for error messages.")

(defvar *sml-last-infix-pattern* nil
  "The pattern most recently built by infix resolution, so that a fun clause
can recognise the head `(x ++ y) z'.")

(defun check-sml-operator-associativity (operators)
  "Operators of equal precedence but different associativity may not be
mixed without parentheses (Definition section 2.6)."
  (loop for (item . rest) on operators
        for precedence = (sml-operator-precedence item)
        for next = (find-if (lambda (other)
                              (<= (sml-operator-precedence other) precedence))
                            rest)
        when (and next
                  (= (sml-operator-precedence next) precedence)
                  (not (eq (sml-operator-right-p next) (sml-operator-right-p item))))
          do (sml-syntax-error *sml-parse-text* (sml-operator-item-start next)
                               "operators `~A' and `~A' have the same precedence ~
                                but different associativity"
                               (sml-operator-item-name item)
                               (sml-operator-item-name next))))

(defun resolve-sml-infix (items kind)
  "Resolve ITEMS, atoms and SML-OPERATOR-ITEMs, of an expression or pattern
(KIND :EXP or :PAT)."
  (let ((operands nil)
        (operators nil)
        (run nil))
    (flet ((end-run (item)
             (when (null run)
               (if item
                   (sml-syntax-error *sml-parse-text* (sml-operator-item-start item)
                                     "infix operator `~A' is missing its left operand ~
                                      (use `op ~A' for the function itself)"
                                     (sml-operator-item-name item)
                                     (sml-operator-item-name item))
                   (sml-syntax-error *sml-parse-text*
                                     (sml-operator-item-start (first operators))
                                     "infix operator `~A' is missing its right operand"
                                     (sml-operator-item-name (first operators)))))
             (push (if (eq kind :exp)
                       (build-sml-application (nreverse run))
                       (build-sml-pattern-application (nreverse run)))
                   operands)
             (setf run nil)))
      (dolist (item items)
        (if (sml-operator-item-p item)
            (progn (end-run item) (push item operators))
            (push item run)))
      (end-run nil))
    (if (null operators)
        (first operands)
        (let* ((operator-list (nreverse operators))
               (operator-vector (coerce operator-list 'simple-vector))
               (operand-vector (coerce (nreverse operands) 'simple-vector))
               (index 0))
          (check-sml-operator-associativity operator-list)
          (labels ((climb (min)
                     (let ((left (svref operand-vector index)))
                       (loop while (and (< index (length operator-vector))
                                        (>= (sml-operator-precedence
                                             (svref operator-vector index))
                                            min))
                             do (let* ((item (svref operator-vector index))
                                       (precedence (sml-operator-precedence item)))
                                  (incf index)
                                  (let ((right (climb (if (sml-operator-right-p item)
                                                          precedence
                                                          (1+ precedence)))))
                                    (setf left (if (eq kind :exp)
                                                   (make-sml-infix-expression item left right)
                                                   (make-sml-infix-pattern item left right))))))
                       left)))
            (let ((result (climb 0)))
              (when (eq kind :pat)
                (setf *sml-last-infix-pattern* result))
              result))))))

(defun build-sml-application (atoms)
  (reduce (lambda (function argument) (list :app function argument))
          (rest atoms)
          :initial-value (first atoms)))

(defun make-sml-infix-expression (item left right)
  (list :app
        (make-sml-expression-id (sml-operator-item-name item) (sml-operator-item-start item))
        (list :tuple left right)))

(defun make-sml-infix-pattern (item left right)
  (let ((name (sml-operator-item-name item)))
    (if (string= name "::")
        (list :pat-cons left right)
        (list :pat-app
              (make-sml-pattern-constructor name (sml-operator-item-start item))
              (list :pat-tuple left right)))))

;;; Patterns are first read as raw atomic patterns, where an identifier is
;;; (:%id name start) until it is known whether it is a fun clause's name, a
;;; constructor applied to an argument, or an atomic pattern.

(defun sml-raw-id-p (item)
  (and (consp item) (eq (car item) :%id)))

(defun sml-raw-pattern-atom (item)
  (if (sml-raw-id-p item)
      (make-sml-pattern-id (second item) (third item))
      item))

(defun normalize-sml-constructor-pattern (constructor argument)
  (if (and (string= (second constructor) "::")
           (consp argument)
           (eq (car argument) :pat-tuple)
           (= (length argument) 3))
      (list :pat-cons (second argument) (third argument))
      (list :pat-app constructor argument)))

(defun build-sml-pattern-application (atoms)
  (case (length atoms)
    (1 (sml-raw-pattern-atom (first atoms)))
    (2 (let ((head (first atoms)))
         (unless (sml-raw-id-p head)
           (sml-syntax-error *sml-parse-text* *sml-pattern-start*
                             "only a constructor can be applied in a pattern"))
         (normalize-sml-constructor-pattern
          (make-sml-pattern-constructor (second head) (third head))
          (sml-raw-pattern-atom (second atoms)))))
    (t (let ((head (first atoms)))
         (sml-syntax-error *sml-parse-text*
                           (if (sml-raw-id-p head) (third head) *sml-pattern-start*)
                           "a constructor pattern takes exactly one argument")))))

;;;; Patterns

(defun sml-atpat-start-p (token)
  (case (sml-token-kind token)
    ((:int :word :real :string :char :long :alnum :symbolic) t)
    (:reserved (member (sml-token-value token)
                       '(:underscore :op :lbrace :lparen :lbracket)))
    (t nil)))

(defun sml-pattern-operator-token-p (token)
  (and (sml-identifier-token-p token)
       (sml-infix-name-p (sml-token-value token))))

(defun parse-sml-pat ()
  (let ((pat (parse-sml-infix-pat)))
    (loop while (accept-sml-reserved :colon)
          do (setf pat (list :pat-typed pat (parse-sml-ty))))
    (if (at-sml-reserved-p :as)
        (let ((as-token (next-sml-token)))
          (list :pat-as (sml-layered-pattern-variable pat as-token) (parse-sml-pat)))
        pat)))

(defun sml-layered-pattern-variable (pat as-token)
  "The `<op> vid <: ty>' before `as'."
  (cond
    ((and (consp pat) (eq (car pat) :pat-var)) pat)
    ((and (consp pat) (eq (car pat) :pat-ctor))
     ;; A layered pattern binds a variable whatever the name's status.
     (note-sml-ast-position (list :pat-var (second pat)) (sml-ast-position pat)))
    ((and (consp pat) (eq (car pat) :pat-typed))
     (list :pat-typed (sml-layered-pattern-variable (second pat) as-token) (third pat)))
    (t (sml-parse-error-at as-token "only a variable may precede `as'"))))

(defun parse-sml-infix-pat ()
  (let ((items nil)
        (*sml-pattern-start* (sml-token-start (current-sml-token))))
    (loop
      (let ((token (current-sml-token)))
        (cond
          ((sml-pattern-operator-token-p token)
           (push (take-sml-operator-item) items))
          ((sml-atpat-start-p token)
           (push (parse-sml-atpat-raw) items))
          (t (return)))))
    (when (null items)
      (sml-parse-expected "a pattern"))
    (resolve-sml-infix (nreverse items) :pat)))

(defun parse-sml-atpat-raw ()
  "An atomic pattern; an identifier is returned as (:%id name start)."
  (let ((token (next-sml-token)))
    (case (sml-token-kind token)
      ((:int :real :string :char) (sml-token-value token))
      (:word (note-sml-ast-position (list :word (sml-token-value token))
                                    (sml-token-start token)))
      ((:alnum :symbolic :long)
       (list :%id (sml-token-value token) (sml-token-start token)))
      (t
       (case (sml-token-value token)
         (:underscore :wild)
         (:op
          (let ((id (current-sml-token)))
            (unless (or (sml-vid-token-p id) (eq (sml-token-kind id) :long))
              (sml-parse-expected "an identifier after `op'"))
            (next-sml-token)
            (list :%id (sml-vid-token-name id) (sml-token-start id))))
         (:lbrace (parse-sml-record-pattern))
         (:lparen
          (if (accept-sml-reserved :rparen)
              (list :pat-unit)
              (let ((first (parse-sml-pat)))
                (if (accept-sml-reserved :comma)
                    (let ((items (list first)))
                      (loop do (push (parse-sml-pat) items)
                            while (accept-sml-reserved :comma))
                      (expect-sml-reserved :rparen)
                      (cons :pat-tuple (nreverse items)))
                    (progn
                      (expect-sml-reserved :rparen)
                      first)))))
         (:lbracket
          (let ((items nil))
            (unless (at-sml-reserved-p :rbracket)
              (loop do (push (parse-sml-pat) items)
                    while (accept-sml-reserved :comma)))
            (expect-sml-reserved :rbracket)
            (let ((list (list :pat-nil)))
              (dolist (item items list)
                (setf list (list :pat-cons item list))))))
         (t (sml-parse-error-at token "expected a pattern but found ~A"
                                (describe-sml-token token))))))))

(defun parse-sml-record-pattern ()
  ;; After `{'.
  (let ((fields nil))
    (unless (at-sml-reserved-p :rbrace)
      (loop
        (if (accept-sml-reserved :dots)
            (progn (push :record-rest fields) (return))
            (let* ((label-token (current-sml-token))
                   (label (parse-sml-label)))
              (if (accept-sml-reserved :equals)
                  (push (list label (parse-sml-pat)) fields)
                  ;; `vid <: ty> <as pat>' abbreviates `vid = vid <: ty> <as pat>'.
                  (let ((pat (make-sml-pattern-id label (sml-token-start label-token))))
                    (when (eq (sml-token-kind label-token) :int)
                      (sml-parse-error-at label-token "a numeric label needs `= pattern'"))
                    (when (eq (car pat) :pat-ctor)
                      (setf pat (if (getf (cddr pat) :status)
                                    pat
                                    (note-sml-ast-position (list :pat-var label)
                                                           (sml-token-start label-token)))))
                    (when (accept-sml-reserved :colon)
                      (setf pat (list :pat-typed pat (parse-sml-ty))))
                    (when (at-sml-reserved-p :as)
                      (let ((as-token (next-sml-token)))
                        (setf pat (list :pat-as (sml-layered-pattern-variable pat as-token)
                                        (parse-sml-pat)))))
                    (push (list label pat) fields)))))
        (unless (accept-sml-reserved :comma)
          (return))))
    (expect-sml-reserved :rbrace)
    (if (null fields)
        (list :pat-unit)
        (cons :pat-record (nreverse fields)))))

(defun sml-pattern-variable-names (pat)
  "The variables PAT binds."
  (cond
    ((atom pat) nil)
    ((eq (car pat) :pat-var) (list (second pat)))
    ((eq (car pat) :pat-ctor) nil)
    ((eq (car pat) :pat-record)
     (loop for field in (rest pat)
           unless (eq field :record-rest)
             append (sml-pattern-variable-names (second field))))
    ((member (car pat) '(:pat-typed)) (sml-pattern-variable-names (second pat)))
    ((member (car pat) '(:pat-app)) (sml-pattern-variable-names (third pat)))
    ((member (car pat) '(:pat-as :pat-cons))
     (append (sml-pattern-variable-names (second pat))
             (sml-pattern-variable-names (third pat))))
    ((eq (car pat) :pat-tuple) (mapcan #'sml-pattern-variable-names (rest pat)))
    (t nil)))

(defun bind-sml-variables (names)
  (dolist (name names)
    (sml-scope-bind :values name "v")))

;;;; Expressions

(defun sml-atexp-start-p (token)
  (case (sml-token-kind token)
    ((:int :word :real :string :char :long :alnum :symbolic) t)
    (:reserved (member (sml-token-value token)
                       '(:op :lbrace :hash :lparen :lbracket :let)))
    (t nil)))

(defun sml-expression-operator-token-p (token)
  (and (sml-vid-token-p token)
       (sml-infix-name-p (sml-vid-token-name token))))

(defun sml-prefix-expression-keyword-p (token)
  (and (eq (sml-token-kind token) :reserved)
       (member (sml-token-value token) '(:raise :fn :case :if :while))))

(defun parse-sml-exp ()
  (parse-sml-exp-level 0))

(defun parse-sml-exp-level (min)
  ;; Precedences: `:' 4, andalso 3, orelse 2, handle 1.  raise, fn, case,
  ;; if and while extend as far to the right as possible.
  (if (sml-prefix-expression-keyword-p (current-sml-token))
      (parse-sml-prefix-exp)
      (let ((exp (parse-sml-infix-exp)))
        (loop
          (cond
            ((and (<= min 4) (accept-sml-reserved :colon))
             (setf exp (list :typed exp (parse-sml-ty))))
            ((and (<= min 3) (accept-sml-reserved :andalso))
             (setf exp (list :andalso exp (parse-sml-exp-level 3))))
            ((and (<= min 2) (accept-sml-reserved :orelse))
             (setf exp (list :orelse exp (parse-sml-exp-level 2))))
            ((and (<= min 1) (accept-sml-reserved :handle))
             (setf exp (list :handle exp (parse-sml-match))))
            (t (return exp)))))))

(defun parse-sml-prefix-exp ()
  (let ((token (next-sml-token)))
    (ecase (sml-token-value token)
      (:raise (list :raise (parse-sml-exp)))
      (:fn (list :fn (parse-sml-match)))
      (:case
       (let ((exp (parse-sml-exp)))
         (expect-sml-reserved :of)
         (list* :case exp (parse-sml-match))))
      (:if
       (let ((test (parse-sml-exp)))
         (expect-sml-reserved :then)
         (let ((then (parse-sml-exp)))
           (expect-sml-reserved :else)
           (list :if test then (parse-sml-exp)))))
      (:while
       (let ((test (parse-sml-exp)))
         (expect-sml-reserved :do)
         (list :while test (parse-sml-exp)))))))

(defun parse-sml-match ()
  "Rules `pat => exp' separated by `|'."
  (let ((rules nil))
    (loop
      (let ((pat (parse-sml-pat)))
        (expect-sml-reserved :darrow)
        (push (list pat (parse-sml-exp)) rules))
      (unless (accept-sml-reserved :bar)
        (return)))
    (nreverse rules)))

(defun parse-sml-infix-exp ()
  (let ((items nil))
    (loop
      (let ((token (current-sml-token)))
        (cond
          ((sml-expression-operator-token-p token)
           (push (take-sml-operator-item) items))
          ((sml-atexp-start-p token)
           (push (parse-sml-atexp) items))
          (t (return)))))
    (when (null items)
      (sml-parse-expected "an expression"))
    (resolve-sml-infix (nreverse items) :exp)))

(defun parse-sml-atexp ()
  (let ((token (next-sml-token)))
    (case (sml-token-kind token)
      ((:int :real :string :char) (sml-token-value token))
      (:word (note-sml-ast-position (list :word (sml-token-value token))
                                    (sml-token-start token)))
      ((:alnum :symbolic :long)
       (make-sml-expression-id (sml-token-value token) (sml-token-start token)))
      (t
       (case (sml-token-value token)
         (:op
          (let ((id (current-sml-token)))
            (unless (or (sml-vid-token-p id) (eq (sml-token-kind id) :long))
              (sml-parse-expected "an identifier after `op'"))
            (next-sml-token)
            (make-sml-expression-id (sml-vid-token-name id) (sml-token-start id))))
         (:equals
          ;; `=' declared nonfix.
          (make-sml-expression-id "=" (sml-token-start token)))
         (:lbrace
          (let ((fields nil))
            (unless (at-sml-reserved-p :rbrace)
              (loop
                (let ((label (parse-sml-label)))
                  (expect-sml-reserved :equals)
                  (push (list label (parse-sml-exp)) fields))
                (unless (accept-sml-reserved :comma)
                  (return))))
            (expect-sml-reserved :rbrace)
            (if fields
                (cons :record (nreverse fields))
                (list :unit))))
         (:hash (list :selector (parse-sml-label)))
         (:lparen
          (if (accept-sml-reserved :rparen)
              (list :unit)
              (let ((first (parse-sml-exp)))
                (cond
                  ((accept-sml-reserved :comma)
                   (let ((items (list first)))
                     (loop do (push (parse-sml-exp) items)
                           while (accept-sml-reserved :comma))
                     (expect-sml-reserved :rparen)
                     (cons :tuple (nreverse items))))
                  ((accept-sml-reserved :semicolon)
                   (let ((items (list first)))
                     (loop do (push (parse-sml-exp) items)
                           while (accept-sml-reserved :semicolon))
                     (expect-sml-reserved :rparen)
                     (cons :seq (nreverse items))))
                  (t
                   (expect-sml-reserved :rparen)
                   first)))))
         (:lbracket
          (let ((items nil))
            (unless (at-sml-reserved-p :rbracket)
              (loop do (push (parse-sml-exp) items)
                    while (accept-sml-reserved :comma)))
            (expect-sml-reserved :rbracket)
            (cons :list (nreverse items))))
         (:let
          (with-sml-scope ()
            (let ((decs (parse-sml-decs)))
              (expect-sml-reserved :in)
              (let ((body (list (parse-sml-exp))))
                (loop while (accept-sml-reserved :semicolon)
                      do (push (parse-sml-exp) body))
                (expect-sml-reserved :end)
                (list :let decs (nreverse body))))))
         (t (sml-parse-error-at token "expected an expression but found ~A"
                                (describe-sml-token token))))))))

;;;; Core declarations

(defun sml-declaration-start-p (token)
  (and (eq (sml-token-kind token) :reserved)
       (member (sml-token-value token)
               '(:val :fun :type :datatype :abstype :exception :local :open
                 :infix :infixr :nonfix :structure :signature :functor))))

(defvar *sml-declaration-positions* nil
  "When non-NIL, an EQ hash table from declaration AST nodes to the offset
where the declaration starts, filled in by the parser.  Used to report where
the Common Lisp generated for a binding came from.")

(defun note-sml-declaration-position (dec start)
  (when (and *sml-declaration-positions* (consp dec))
    (setf (gethash dec *sml-declaration-positions*) start))
  dec)

(defun parse-sml-decs (&key top-level)
  "A declaration sequence; at TOP-LEVEL also expressions, as (:expr e)."
  (let ((decs nil))
    (loop
      (let* ((token (current-sml-token))
             (start (sml-token-start token)))
        (cond
          ((accept-sml-reserved :semicolon))
          ((sml-declaration-start-p token)
           (dolist (dec (parse-sml-dec))
             (push (note-sml-declaration-position dec start) decs)))
          ((and top-level (not (at-sml-eof-p)))
           (push (note-sml-declaration-position (list :expr (parse-sml-exp)) start)
                 decs))
          (t (return)))))
    (nreverse decs)))

(defun parse-sml-dec ()
  "One declaration; returns a list of nodes."
  (let ((token (next-sml-token)))
    (ecase (sml-token-value token)
      (:val (parse-sml-val-dec))
      (:fun (parse-sml-fun-dec))
      (:type (parse-sml-type-dec))
      (:datatype (parse-sml-datatype-dec))
      (:abstype (parse-sml-abstype-dec))
      (:exception (parse-sml-exception-dec))
      (:local (parse-sml-local-dec))
      (:open (parse-sml-open-dec))
      ((:infix :infixr :nonfix) (parse-sml-fixity-dec (sml-token-value token)))
      (:structure (parse-sml-structure-dec))
      (:signature (parse-sml-signature-dec))
      (:functor (parse-sml-functor-dec)))))

(defun mark-sml-and-bindings (nodes)
  "Mark all but the first of NODES, bindings joined by `and'."
  (cons (first nodes)
        (mapcar (lambda (node) (append node (list :and t))) (rest nodes))))

(defun sml-val-rec-nodes (pat exp tyvar-plist)
  "The :val-rec nodes of `rec PAT = EXP'.  PAT is a variable or a layered
pattern `v1 <: ty> as v2 <: ty> as ...', all of whose variables denote the
function (Definition section 2.9 and rule 26; HaMLet test valrec): the first
is bound to EXP, the others to its eta expansion.  The node of the innermost
variable, the one not bound by `as', is marked :atomic: when it names a
constructor, the binding raises Bind (rule 126 matches the pattern in the
environment outside the `val rec')."
  (let ((names nil)
        (atomic nil)
        (type nil))
    (labels ((walk (pat)
               (cond ((and (consp pat) (eq (car pat) :pat-typed))
                      (unless type (setf type (third pat)))
                      (walk (second pat)))
                     ((and (consp pat) (eq (car pat) :pat-as))
                      (walk (second pat))
                      (walk (third pat)))
                     ((and (consp pat) (member (car pat) '(:pat-var :pat-ctor)))
                      (setf atomic (second pat))
                      (push (second pat) names))
                     (t (sml-parse-expected "a variable in `val rec'")))))
      (walk pat))
    (setf names (nreverse names))
    (flet ((atomic-plist (name)
             (and (equal name atomic) (list :atomic t))))
      (let ((type-plist (and type (list :type type))))
        (cons `(:val-rec ,(first names) ,exp ,@type-plist ,@tyvar-plist
                         ,@(atomic-plist (first names)))
              (loop for name in (rest names)
                    collect `(:val-rec ,name
                                       (:fn (((:pat-var "x%") (:app (:var ,(first names))
                                                                   (:var "x%")))))
                                       ,@type-plist ,@tyvar-plist
                                       ,@(atomic-plist name))))))))

(defun parse-sml-val-dec ()
  (let ((tyvars (parse-sml-tyvarseq))
        (bindings nil)
        (recursive nil))
    (loop
      ;; `rec' may be repeated (Definition section 2.9: valbind ::= rec valbind).
      (loop while (accept-sml-reserved :rec)
            do (setf recursive t))
      (let ((pat (parse-sml-pat)))
        (expect-sml-reserved :equals)
        ;; The names `val rec' binds are variables in its expressions and
        ;; after it, even those that were constructors (rule 26).
        (when recursive
          (bind-sml-variables (mapcar #'second (sml-val-rec-nodes pat nil nil))))
        (push (list recursive pat (parse-sml-exp)) bindings))
      (unless (accept-sml-reserved :and)
        (return)))
    (setf bindings (nreverse bindings))
    (let* ((tyvar-plist (and tyvars (list :tyvars tyvars)))
           (nodes
             (mapcar
              (lambda (binding)
                (destructuring-bind (recursive pat exp) binding
                  (let ((type (and (consp pat) (eq (car pat) :pat-typed) (third pat)))
                        (inner (if (and (consp pat) (eq (car pat) :pat-typed))
                                   (second pat)
                                   pat)))
                    (if recursive
                        (sml-val-rec-nodes pat exp tyvar-plist)
                        (list `(:val ,inner ,exp ,@(and type (list :type type))
                                     ,@tyvar-plist))))))
              bindings)))
      (setf nodes (apply #'append nodes))
      (dolist (binding bindings)
        (bind-sml-variables (sml-pattern-variable-names (second binding))))
      (let* ((plain (remove :val-rec nodes :key #'car))
             (rec (remove :val nodes :key #'car))
             (rec-node (cond ((null rec) nil)
                             ((null (rest rec)) (first rec))
                             (t (cons :val-recs rec))))
             (all (append plain (and rec-node (list rec-node)))))
        (list (if (rest all) (cons :vals all) (first all)))))))

(defun parse-sml-fun-head ()
  "Read a fun clause head; returns the function name, its offset and the
parameter patterns (Definition section 2.6 and Appendix A)."
  (let ((items nil)
        (head-token (current-sml-token)))
    (loop until (or (at-sml-reserved-p :equals) (at-sml-reserved-p :colon))
          do (unless (sml-atpat-start-p (current-sml-token))
               (sml-parse-expected "`=' after the function clause head"))
             (let ((op-p (at-sml-reserved-p :op))
                   (item (parse-sml-atpat-raw)))
               (push (list item op-p (eq item *sml-last-infix-pattern*)) items)))
    (setf items (nreverse items))
    (flet ((infix-id-p (entry)
             (destructuring-bind (item op-p infix-paren-p) entry
               (declare (ignore infix-paren-p))
               (and (sml-raw-id-p item)
                    (not op-p)
                    (not (sml-long-name-p (second item)))
                    (sml-infix-name-p (second item)))))
           (param (entry) (sml-raw-pattern-atom (first entry))))
      (cond
        ;; atpat vid atpat, with vid infix
        ((and (= (length items) 3) (infix-id-p (second items)))
         (let ((name-item (first (second items))))
           (values (second name-item) (third name-item)
                   (list (list :pat-tuple (param (first items)) (param (third items)))))))
        ;; (atpat vid atpat) atpat ...
        ((and items (third (first items)))
         (let* ((pat (first (first items)))
                (op-name (if (eq (car pat) :pat-cons) "::" (second (second pat))))
                (op-start (if (eq (car pat) :pat-cons)
                              (sml-token-start head-token)
                              (or (sml-ast-position (second pat))
                                  (sml-token-start head-token))))
                (pair (if (eq (car pat) :pat-cons)
                          (list :pat-tuple (second pat) (third pat))
                          (third pat))))
           (values op-name op-start (cons pair (mapcar #'param (rest items))))))
        ;; <op> vid atpat ...
        ((and items
              (sml-raw-id-p (first (first items)))
              (not (sml-long-name-p (second (first (first items))))))
         (when (null (rest items))
           (sml-parse-error-at head-token "a function clause needs at least one argument"))
         (let ((name-item (first (first items))))
           (values (second name-item) (third name-item)
                   (mapcar #'param (rest items)))))
        (t (sml-parse-error-at head-token "expected a function name"))))))

(defun parse-sml-fun-clause ()
  (let ((head-token (current-sml-token)))
    (multiple-value-bind (name start params) (parse-sml-fun-head)
      (declare (ignore start))
      (let ((result-type (and (accept-sml-reserved :colon) (parse-sml-ty))))
        (expect-sml-reserved :equals)
        (let ((body (parse-sml-exp)))
          (list name
                params
                (if result-type (list :typed body result-type) body)
                head-token))))))

(defun parse-sml-fun-dec ()
  (let ((tyvars (parse-sml-tyvarseq))
        (functions nil))
    (loop
      (let ((clauses (list (parse-sml-fun-clause))))
        (loop while (accept-sml-reserved :bar)
              do (push (parse-sml-fun-clause) clauses))
        (setf clauses (nreverse clauses))
        (let ((name (first (first clauses)))
              (arity (length (second (first clauses)))))
          (dolist (clause (rest clauses))
            (unless (string= (first clause) name)
              (sml-parse-error-at (fourth clause)
                                  "clauses of one function must all define `~A', not `~A'"
                                  name (first clause)))
            (unless (= (length (second clause)) arity)
              (sml-parse-error-at (fourth clause)
                                  "clauses of `~A' take different numbers of arguments"
                                  name)))
          (push `(:fun ,name ,(mapcar (lambda (clause) (list (second clause) (third clause)))
                                      clauses)
                       ,@(and tyvars (list :tyvars tyvars)))
                functions)))
      (unless (accept-sml-reserved :and)
        (return)))
    (setf functions (nreverse functions))
    (bind-sml-variables (mapcar #'second functions))
    (list (if (rest functions) (cons :funs functions) (first functions)))))

(defun parse-sml-typbind ()
  "tyvarseq tycon = ty <and ...>; returns ((tyvars name ty) ...)."
  (let ((bindings nil))
    (loop
      (let* ((tyvars (parse-sml-tyvarseq))
             (name (sml-token-value (expect-sml-tycon))))
        (expect-sml-reserved :equals)
        (push (list tyvars name (parse-sml-ty)) bindings))
      (unless (accept-sml-reserved :and)
        (return)))
    (nreverse bindings)))

(defun parse-sml-type-dec ()
  (let ((bindings (parse-sml-typbind)))
    (dolist (binding bindings)
      (sml-scope-bind :tycons (second binding) :plain))
    (mark-sml-and-bindings
     (mapcar (lambda (binding)
               (destructuring-bind (tyvars name ty) binding
                 `(:type ,name ,ty ,@(and tyvars (list :tyvars tyvars)))))
             bindings))))

(defun parse-sml-condescs ()
  "Constructor descriptions `<op> vid <of ty> | ...'."
  (let ((constructors nil))
    (loop
      (accept-sml-reserved :op)
      (let ((token (current-sml-token)))
        (unless (sml-vid-token-p token)
          (sml-parse-expected "a constructor name"))
        (next-sml-token)
        (let ((arg-type (and (accept-sml-reserved :of) (parse-sml-ty))))
          (push (list :ctor-def (sml-vid-token-name token)
                      :has-args (and arg-type t)
                      :arg-type arg-type)
                constructors)))
      (unless (accept-sml-reserved :bar)
        (return)))
    (nreverse constructors)))

(defun sml-constructor-status (ctor-def)
  (if (getf (cddr ctor-def) :has-args) "c1" "c"))

(defun bind-sml-datatype (name constructors &optional (scope (first *sml-scopes*)))
  (sml-scope-bind :tycons name
                  (mapcar (lambda (ctor) (cons (second ctor) (sml-constructor-status ctor)))
                          constructors)
                  scope)
  (dolist (ctor constructors)
    (sml-scope-bind :values (second ctor) (sml-constructor-status ctor) scope)))

(defun parse-sml-datbinds ()
  "datbind <withtype typbind>; returns datbinds ((tyvars name ctors) ...) and
the withtype bindings."
  (let ((datbinds nil))
    (loop
      (let* ((tyvars (parse-sml-tyvarseq))
             (name (sml-token-value (expect-sml-tycon))))
        (expect-sml-reserved :equals)
        (push (list tyvars name (parse-sml-condescs)) datbinds))
      (unless (accept-sml-reserved :and)
        (return)))
    (values (nreverse datbinds)
            (and (accept-sml-reserved :withtype) (parse-sml-typbind)))))

(defun make-sml-datatype-node (datbinds withtype)
  `(:datatype ,(second (first datbinds))
              ,(mapcan (lambda (datbind) (copy-list (third datbind))) datbinds)
              :datbinds ,datbinds
              :withtype ,withtype))

(defun bind-sml-datbinds (datbinds withtype)
  (dolist (datbind datbinds)
    (bind-sml-datatype (second datbind) (third datbind)))
  (dolist (binding withtype)
    (sml-scope-bind :tycons (second binding) :plain)))

(defun bind-sml-replicated-datatype (name name-start longtycon)
  "Bind the constructors that `datatype NAME = datatype LONGTYCON' brings
into scope, when known.  Returns them as (status . name) pairs."
  (let* ((fact (and *sml-static-facts*
                    (first (sml-static-facts-at name-start "dtrep" name))))
         (constructors
           (if fact
               (mapcar (lambda (entry) (cons (cdr entry) (car entry)))
                       (parse-sml-structure-members (third fact)))
               (sml-lookup-long :tycons longtycon))))
    (cond
      ((listp constructors)
       (sml-scope-bind :tycons name constructors)
       (loop for (constructor . status) in constructors
             do (sml-scope-bind :values constructor status))
       (mapcar (lambda (entry) (cons (cdr entry) (car entry))) constructors))
      ((eq constructors :plain)
       (sml-scope-bind :tycons name :plain)
       nil)
      (t
       ;; Unknown constructors: names below become uncertain.
       (open-sml-environment :unknown)
       (sml-scope-bind :tycons name :plain)
       nil))))

(defun parse-sml-datatype-dec ()
  (if (and (eq (sml-token-kind (current-sml-token)) :alnum)
           (sml-token-reserved-p (sml-token-ahead 1) :equals)
           (sml-token-reserved-p (sml-token-ahead 2) :datatype))
      (let* ((name-token (next-sml-token))
             (name (sml-token-value name-token)))
        (next-sml-token)
        (next-sml-token)
        (let* ((longtycon (sml-token-value (expect-sml-tycon t)))
               (constructors (bind-sml-replicated-datatype
                              name (sml-token-start name-token) longtycon)))
          (list (note-sml-ast-position
                 `(:datatype-replication ,name ,longtycon
                                         ,@(and constructors
                                                (list :constructors constructors)))
                 (sml-token-start name-token)))))
      (multiple-value-bind (datbinds withtype) (parse-sml-datbinds)
        (bind-sml-datbinds datbinds withtype)
        (list (make-sml-datatype-node datbinds withtype)))))

(defun parse-sml-abstype-dec ()
  (multiple-value-bind (datbinds withtype) (parse-sml-datbinds)
    (let ((outer *sml-scopes*)
          (datatype (make-sml-datatype-node datbinds withtype)))
      (let ((body nil)
            (frames nil))
        (with-sml-scope ()
          (bind-sml-datbinds datbinds withtype)
          (let ((base *sml-scopes*))
            (push-sml-scope (make-sml-scope))
            (expect-sml-reserved :with)
            (setf body (parse-sml-decs))
            (expect-sml-reserved :end)
            (setf frames (sml-scope-frames-since base))))
        ;; The types stay visible, their constructors do not.
        (merge-sml-scopes frames (first outer) +sml-all-namespaces+)
        (dolist (datbind datbinds)
          (sml-scope-bind :tycons (second datbind) :plain (first outer)))
        (list (list :local (list datatype) body :abstype t))))))

(defun parse-sml-exception-dec ()
  (let ((nodes nil))
    (loop
      (accept-sml-reserved :op)
      (let ((token (current-sml-token)))
        (unless (sml-vid-token-p token)
          (sml-parse-expected "an exception name"))
        (next-sml-token)
        (let ((name (sml-vid-token-name token)))
          (cond
            ((accept-sml-reserved :equals)
             (accept-sml-reserved :op)
             (let* ((target-token (current-sml-token))
                    (target (progn
                              (unless (or (sml-vid-token-p target-token)
                                          (eq (sml-token-kind target-token) :long))
                                (sml-parse-expected "an exception name"))
                              (sml-vid-token-name (next-sml-token))))
                    (status (sml-identifier-status target (sml-token-start target-token) :pat)))
               (push (list :exception-alias name target) nodes)
               (sml-scope-bind :values name (if (and (stringp status)
                                                     (sml-status-exception-p status))
                                                status
                                                "e"))))
            (t
             (let ((arg-type (and (accept-sml-reserved :of) (parse-sml-ty))))
               (push (list :exception name :arg-type arg-type) nodes)
               (sml-scope-bind :values name (if arg-type "e1" "e")))))))
      (unless (accept-sml-reserved :and)
        (return)))
    (mark-sml-and-bindings (nreverse nodes))))

(defun parse-sml-local-dec ()
  (let ((outer *sml-scopes*)
        (local-decs nil)
        (body-decs nil)
        (frames nil))
    (with-sml-scope ()
      (setf local-decs (parse-sml-decs))
      (expect-sml-reserved :in)
      (let ((base *sml-scopes*))
        (push-sml-scope (make-sml-scope))
        (setf body-decs (parse-sml-decs))
        (setf frames (sml-scope-frames-since base))))
    (expect-sml-reserved :end)
    (merge-sml-scopes frames (first outer) +sml-all-namespaces+)
    (list (list :local local-decs body-decs))))

(defun parse-sml-open-dec ()
  (let ((names nil))
    (loop while (member (sml-token-kind (current-sml-token)) '(:alnum :long))
          do (push (sml-token-value (next-sml-token)) names))
    (when (null names)
      (sml-parse-expected "a structure name after `open'"))
    (setf names (nreverse names))
    (dolist (name names)
      (open-sml-environment (sml-lookup-structure name)))
    (list (list :open names))))

(defun parse-sml-fixity-dec (kind)
  (let ((precedence nil)
        (names nil))
    (when (and (member kind '(:infix :infixr))
               (eq (sml-token-kind (current-sml-token)) :int))
      (let* ((token (current-sml-token))
             (text (subseq *sml-parse-text* (sml-token-start token) (sml-token-end token))))
        (unless (and (= (length text) 1) (digit-char-p (char text 0)))
          (sml-parse-error-at token "a precedence must be a single digit 0-9"))
        (next-sml-token)
        (setf precedence (sml-token-value token))))
    (loop while (sml-vid-token-p (current-sml-token))
          do (push (sml-vid-token-name (next-sml-token)) names))
    (when (null names)
      (sml-parse-expected "an identifier"))
    (setf names (nreverse names))
    (dolist (name names)
      (sml-scope-bind :fixity name
                      (ecase kind
                        (:infix (cons :infix (or precedence 0)))
                        (:infixr (cons :infixr (or precedence 0)))
                        (:nonfix :nonfix))))
    (list (list :infix (string-downcase (symbol-name kind)) precedence names))))

;;;; Modules

(defun sml-scope-frames-environment (frames)
  (collapse-sml-scopes frames))

(defun parse-sml-strexp ()
  "A structure expression; returns its AST and its environment."
  (multiple-value-bind (strexp environment) (parse-sml-atomic-strexp)
    (loop
      (let ((opaque (cond ((accept-sml-reserved :colon) nil)
                          ((accept-sml-reserved :seal) t)
                          (t (return)))))
        (multiple-value-bind (sigexp signature-environment) (parse-sml-sigexp)
          (setf strexp (list :str-constraint strexp sigexp :opaque opaque))
          (when (sml-scope-p signature-environment)
            (setf environment signature-environment)))))
    (values strexp environment)))

(defun parse-sml-struct-body ()
  "Declarations up to `end'; returns them and their environment."
  (with-sml-scope ()
    (let* ((base (rest *sml-scopes*))
           (decs (parse-sml-decs)))
      (expect-sml-reserved :end)
      (values decs (sml-scope-frames-environment (sml-scope-frames-since base))))))

(defun parse-sml-atomic-strexp ()
  (let ((token (current-sml-token)))
    (cond
      ((accept-sml-reserved :struct)
       (multiple-value-bind (decs environment) (parse-sml-struct-body)
         (values (list :struct decs) environment)))
      ((accept-sml-reserved :let)
       (with-sml-scope ()
         (let ((decs (parse-sml-decs)))
           (expect-sml-reserved :in)
           (multiple-value-bind (strexp environment) (parse-sml-strexp)
             (expect-sml-reserved :end)
             (values (list :str-let decs strexp) environment)))))
      ((and (eq (sml-token-kind token) :alnum)
            (sml-token-reserved-p (sml-token-ahead 1) :lparen))
       ;; Functor application funid (strexp) or funid (strdec).
       (let ((funid (sml-token-value (next-sml-token))))
         (next-sml-token)
         (let ((argument
                 (if (or (at-sml-reserved-p :rparen)
                         (sml-declaration-start-p (current-sml-token)))
                     (with-sml-scope ()
                       (list :struct (parse-sml-decs)))
                     (parse-sml-strexp))))
           (expect-sml-reserved :rparen)
           (values (list :str-app funid argument)
                   (or (sml-scope-lookup :functors funid) :unknown)))))
      ((member (sml-token-kind token) '(:alnum :long))
       (next-sml-token)
       (values (list :str-id (sml-token-value token))
               (sml-lookup-structure (sml-token-value token))))
      (t (sml-parse-expected "a structure expression")))))

(defun strip-sml-strexp-constraints (strexp)
  "STREXP without constraints, the outermost signature and whether it is opaque."
  (let ((sigexp nil)
        (opaque nil))
    (loop while (eq (car strexp) :str-constraint)
          do (unless sigexp
               (setf sigexp (third strexp)
                     opaque (getf (cdddr strexp) :opaque)))
             (setf strexp (second strexp)))
    (values strexp sigexp opaque)))

(defun sml-strexp-body-decs (strexp)
  "Declarations whose bindings are the members of STREXP, for the compiler."
  (let ((core (strip-sml-strexp-constraints strexp)))
    (ecase (car core)
      (:struct (second core))
      (:str-let (list (list :local (second core) (sml-strexp-body-decs (third core)))))
      (:str-id (list (list :open (list (second core)))))
      (:str-app (list (sml-structure-binding-node "%Body" nil core)
                      (list :open (list "%Body")))))))

(defun sml-structure-binding-node (name start strexp)
  "The declaration node of `structure NAME = STREXP' (see the AST summary)."
  (multiple-value-bind (core sigexp opaque) (strip-sml-strexp-constraints strexp)
    (let ((signature (and sigexp (list :sigexp sigexp :opaque opaque))))
      (note-sml-ast-position
       (ecase (car core)
         (:struct `(:structure ,name ,(second core) ,@signature :strexp ,strexp))
         (:str-let `(:structure ,name ,(sml-strexp-body-decs core) ,@signature
                                :strexp ,strexp))
         (:str-id `(:structure-alias ,name ,(second core) ,@signature :strexp ,strexp))
         (:str-app `(:structure-app ,name ,(second core) ,(third core) ,@signature
                                    :strexp ,strexp)))
       start))))

(defun parse-sml-structure-dec ()
  (let ((nodes nil)
        (environments nil))
    (loop
      (let* ((name-token (expect-sml-alnum-id "a structure name"))
             (constraint (cond ((accept-sml-reserved :colon) :transparent)
                               ((accept-sml-reserved :seal) :opaque))))
        (multiple-value-bind (sigexp signature-environment)
            (and constraint (parse-sml-sigexp))
          (expect-sml-reserved :equals)
          (multiple-value-bind (strexp environment) (parse-sml-strexp)
            (when constraint
              ;; strid : sigexp = strexp abbreviates strid = strexp : sigexp.
              (setf strexp (list :str-constraint strexp sigexp
                                 :opaque (eq constraint :opaque)))
              (when (sml-scope-p signature-environment)
                (setf environment signature-environment)))
            (push (sml-structure-binding-node (sml-token-value name-token)
                                              (sml-token-start name-token)
                                              strexp)
                  nodes)
            (push (cons (sml-token-value name-token) environment) environments))))
      (unless (accept-sml-reserved :and)
        (return)))
    (loop for (name . environment) in (reverse environments)
          do (sml-scope-bind :structures name environment))
    (mark-sml-and-bindings (nreverse nodes))))

(defun parse-sml-sigexp ()
  "A signature expression; returns its AST and its environment."
  (multiple-value-bind (sigexp environment)
      (cond
        ((accept-sml-reserved :sig)
         (multiple-value-bind (specs environment) (parse-sml-sig-body)
           (values (list :sig specs) environment)))
        ((eq (sml-token-kind (current-sml-token)) :alnum)
         (let ((name (sml-token-value (next-sml-token))))
           (values (list :sig-id name)
                   (or (sml-scope-lookup :signatures name) :unknown))))
        (t (sml-parse-expected "a signature expression")))
    (values (parse-sml-where-clauses sigexp) environment)))

(defun parse-sml-where-clauses (sigexp)
  "`where type tyvarseq longtycon = ty', also joined by `and type'."
  (loop while (and (at-sml-reserved-p :where)
                   (sml-token-reserved-p (sml-token-ahead 1) :type))
        do (next-sml-token)
           (next-sml-token)
           (loop
             (let* ((tyvars (parse-sml-tyvarseq))
                    (longtycon (sml-token-value (expect-sml-tycon t))))
               (expect-sml-reserved :equals)
               (setf sigexp (list :sig-where sigexp tyvars longtycon (parse-sml-ty))))
             (unless (and (at-sml-reserved-p :and)
                          (sml-token-reserved-p (sml-token-ahead 1) :type))
               (return))
             (next-sml-token)
             (next-sml-token)))
  sigexp)

(defun parse-sml-sig-body ()
  "Specifications up to `end'; returns them and their environment."
  (with-sml-scope ()
    (let* ((base (rest *sml-scopes*))
           (specs (parse-sml-specs)))
      (expect-sml-reserved :end)
      (values specs (sml-scope-frames-environment (sml-scope-frames-since base))))))

(defun parse-sml-specs ()
  (let ((specs nil))
    (loop
      (cond
        ((accept-sml-reserved :semicolon))
        ((accept-sml-reserved :val)
         (loop
           (accept-sml-reserved :op)
           (let ((token (current-sml-token)))
             (unless (sml-vid-token-p token)
               (sml-parse-expected "a value name"))
             (next-sml-token)
             (expect-sml-reserved :colon)
             (push (list :spec-val (sml-vid-token-name token) (parse-sml-ty)) specs)
             (sml-scope-bind :values (sml-vid-token-name token) "v"))
           (unless (accept-sml-reserved :and)
             (return))))
        ((or (at-sml-reserved-p :type) (at-sml-reserved-p :eqtype))
         (let ((eqtype (sml-token-reserved-p (next-sml-token) :eqtype)))
           (loop
             (let* ((tyvars (parse-sml-tyvarseq))
                    (name (sml-token-value (expect-sml-tycon))))
               (sml-scope-bind :tycons name :plain)
               (push (cond
                       (eqtype (list :spec-eqtype tyvars name))
                       ((accept-sml-reserved :equals)
                        (list :spec-type tyvars name (parse-sml-ty)))
                       (t (list :spec-type tyvars name)))
                     specs))
             (unless (accept-sml-reserved :and)
               (return)))))
        ((accept-sml-reserved :datatype)
         (if (and (eq (sml-token-kind (current-sml-token)) :alnum)
                  (sml-token-reserved-p (sml-token-ahead 1) :equals)
                  (sml-token-reserved-p (sml-token-ahead 2) :datatype))
             (let ((name-token (next-sml-token)))
               (next-sml-token)
               (next-sml-token)
               (let ((longtycon (sml-token-value (expect-sml-tycon t))))
                 (bind-sml-replicated-datatype (sml-token-value name-token)
                                               (sml-token-start name-token)
                                               longtycon)
                 (push (list :spec-datatype-replication (sml-token-value name-token) longtycon)
                       specs)))
             (let ((datbinds nil))
               (loop
                 (let* ((tyvars (parse-sml-tyvarseq))
                        (name (sml-token-value (expect-sml-tycon))))
                   (expect-sml-reserved :equals)
                   (let ((constructors (parse-sml-condescs)))
                     (bind-sml-datatype name constructors)
                     (push (list tyvars name constructors) datbinds)))
                 (unless (accept-sml-reserved :and)
                   (return)))
               (push (list :spec-datatype (nreverse datbinds)) specs))))
        ((accept-sml-reserved :exception)
         (loop
           (accept-sml-reserved :op)
           (let ((token (current-sml-token)))
             (unless (sml-vid-token-p token)
               (sml-parse-expected "an exception name"))
             (next-sml-token)
             (let ((arg-type (and (accept-sml-reserved :of) (parse-sml-ty))))
               (push (list :spec-exception (sml-vid-token-name token) arg-type) specs)
               (sml-scope-bind :values (sml-vid-token-name token) (if arg-type "e1" "e"))))
           (unless (accept-sml-reserved :and)
             (return))))
        ((accept-sml-reserved :structure)
         (loop
           (let ((name (sml-token-value (expect-sml-alnum-id "a structure name"))))
             (expect-sml-reserved :colon)
             (multiple-value-bind (sigexp environment) (parse-sml-sigexp)
               (push (list :spec-structure name sigexp) specs)
               (sml-scope-bind :structures name environment)))
           (unless (accept-sml-reserved :and)
             (return))))
        ((accept-sml-reserved :include)
         (if (and (eq (sml-token-kind (current-sml-token)) :alnum)
                  (eq (sml-token-kind (sml-token-ahead 1)) :alnum))
             ;; include sigid1 ... sigidn
             (loop while (eq (sml-token-kind (current-sml-token)) :alnum)
                   do (let ((name (sml-token-value (next-sml-token))))
                        (push (list :spec-include (list :sig-id name)) specs)
                        (open-sml-environment (or (sml-scope-lookup :signatures name)
                                                  :unknown))))
             (multiple-value-bind (sigexp environment) (parse-sml-sigexp)
               (push (list :spec-include sigexp) specs)
               (open-sml-environment environment))))
        ((accept-sml-reserved :sharing)
         (let ((typep (accept-sml-reserved :type))
               (names (list (sml-token-value (expect-sml-long-id "a name to share")))))
           (loop while (accept-sml-reserved :equals)
                 do (push (sml-token-value (expect-sml-long-id "a name to share")) names))
           (push (list (if typep :spec-sharing-type :spec-sharing) (nreverse names))
                 specs)))
        (t (return))))
    (nreverse specs)))

(defun parse-sml-signature-dec ()
  (let ((nodes nil)
        (environments nil))
    (loop
      (let ((name (sml-token-value (expect-sml-alnum-id "a signature name"))))
        (expect-sml-reserved :equals)
        (multiple-value-bind (sigexp environment) (parse-sml-sigexp)
          (push (list :signature name sigexp) nodes)
          (push (cons name environment) environments)))
      (unless (accept-sml-reserved :and)
        (return)))
    (loop for (name . environment) in (reverse environments)
          do (sml-scope-bind :signatures name environment))
    (mark-sml-and-bindings (nreverse nodes))))

(defun parse-sml-functor-dec ()
  (let ((nodes nil)
        (results nil))
    (loop
      (let ((name (sml-token-value (expect-sml-alnum-id "a functor name")))
            (param nil)
            (param-sigexp nil)
            (param-specs nil))
        (expect-sml-reserved :lparen)
        (with-sml-scope ()
          (if (and (eq (sml-token-kind (current-sml-token)) :alnum)
                   (sml-token-reserved-p (sml-token-ahead 1) :colon))
              (progn
                (setf param (sml-token-value (next-sml-token)))
                (next-sml-token)
                (multiple-value-bind (sigexp environment) (parse-sml-sigexp)
                  (setf param-sigexp sigexp)
                  (sml-scope-bind :structures param environment)))
              ;; funid (spec): the specification's names are visible in the
              ;; body (Appendix A).
              (setf param-specs (parse-sml-specs)))
          (expect-sml-reserved :rparen)
          (let ((constraint (cond ((accept-sml-reserved :colon) :transparent)
                                  ((accept-sml-reserved :seal) :opaque))))
            (multiple-value-bind (result-sigexp result-environment)
                (and constraint (parse-sml-sigexp))
              (expect-sml-reserved :equals)
              (multiple-value-bind (body environment) (parse-sml-strexp)
                (push (cons name (if (sml-scope-p result-environment)
                                     result-environment
                                     environment))
                      results)
                (push `(:functor ,name ,(sml-strexp-body-decs body)
                                 :param ,param
                                 :param-sigexp ,param-sigexp
                                 :param-specs ,param-specs
                                 :result-sigexp ,result-sigexp
                                 :opaque ,(eq constraint :opaque)
                                 :strexp ,body)
                      nodes))))))
      (unless (accept-sml-reserved :and)
        (return)))
    (loop for (name . environment) in (reverse results)
          do (sml-scope-bind :functors name environment))
    (mark-sml-and-bindings (nreverse nodes))))

;;;; Entry points

(defun call-with-sml-tokens (text start end source-name scopes thunk)
  (let* ((*sml-source-name* source-name)
         (*sml-parse-text* text)
         (*sml-parse-tokens* (lex-sml text :start start :end (or end (length text))))
         (*sml-parse-index* 0)
         (*sml-last-infix-pattern* nil)
         (*sml-scopes* (cons (make-sml-scope) scopes)))
    (funcall thunk)))

(defun expect-sml-eof ()
  (unless (at-sml-eof-p)
    (sml-parse-expected "end of input")))

(defun parse-sml-program (text &key (start 0) end source-name
                                 (package *sml-package*)
                                 (scopes (sml-package-parse-scopes package)))
  "Parse TEXT (between START and END) as an SML program in the environment
SCOPES.  Returns (:program dec...) and the environment the program leaves
for the next one (see SML-PACKAGE-PARSE-SCOPES).  AST positions are offsets
into TEXT."
  (call-with-sml-tokens
   text start end source-name scopes
   (lambda ()
     (let ((decs (parse-sml-decs :top-level t)))
       (expect-sml-eof)
       (values (cons :program decs)
               (list (collapse-sml-scopes *sml-scopes* +sml-all-namespaces+)))))))

(defun parse-sml-declarations (text &key (start 0) end source-name
                                      (package *sml-package*)
                                      (scopes (sml-package-parse-scopes package)))
  "Parse TEXT as a declaration sequence; returns the list of declarations."
  (call-with-sml-tokens
   text start end source-name scopes
   (lambda ()
     (prog1 (parse-sml-decs)
       (expect-sml-eof)))))

(defun parse-sml-expression (text &key (start 0) end source-name
                                    (package *sml-package*)
                                    (scopes (sml-package-parse-scopes package)))
  "Parse TEXT as one SML expression."
  (call-with-sml-tokens
   text start end source-name scopes
   (lambda ()
     (prog1 (parse-sml-exp)
       (expect-sml-eof)))))

(defun parse-sml-pattern (text &key (start 0) end source-name
                                 (package *sml-package*)
                                 (scopes (sml-package-parse-scopes package)))
  "Parse TEXT as one SML pattern."
  (call-with-sml-tokens
   text start end source-name scopes
   (lambda ()
     (prog1 (parse-sml-pat)
       (expect-sml-eof)))))

(defun parse-sml-type (text &key (start 0) end source-name)
  "Parse TEXT as one SML type."
  (call-with-sml-tokens
   text start end source-name nil
   (lambda ()
     (prog1 (parse-sml-ty)
       (expect-sml-eof)))))

(defun parse-sml-string (text)
  "Parse TEXT as an SML expression (kept for older callers)."
  (parse-sml-expression text))
