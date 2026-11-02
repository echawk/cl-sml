(in-package #:cl-sml)

;;; A typed intermediate language between the checked AST and Common Lisp
;;; (docs/IR.md, ROADMAP items 19 and 12).
;;;
;;; COMPILE-EXPR lowers an SML expression to an IR term (IR-LOWER),
;;; simplifies it (IR-SIMPLIFY) and emits CL (IR-EMIT).  Constructs the IR
;;; does not cover yet are lowered to (:lisp TYPE FORM) nodes holding what
;;; the direct code generator, COMPILE-EXPR-DIRECT, makes of them; their
;;; subexpressions go through COMPILE-EXPR again, so each is its own term.
;;;
;;; A term is a list (KIND TYPE . FIELDS).  TYPE is the SML type the
;;; checker gave the node (identifier occurrences and constants), or NIL
;;; when unknown (other nodes, and plain mode).  Variables are the
;;; uninterned symbols the direct generator uses for lexical bindings, so
;;; that a (:lisp ...) node may refer to them; every binder binds a symbol
;;; of its own.  The kinds and their fields:
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
;;;   (:fn ty params body name)  a function of (length PARAMS) arguments
;;;   (:let ty symbol value body)
;;;   (:letrec ty ((symbol value) ...) body)
;;;   (:seq ty items)
;;;   (:if ty test then else)
;;;   (:while ty test body)
;;;   (:raise ty exn)
;;;   (:handle ty body ((pattern body) ...))
;;;   (:match ty scrutinee ((pattern body) ...) failure)  PATTERN is the
;;;                              compiled trivia pattern, FAILURE a CL form
;;;   (:tail-call ty target args)  a self tail call (see
;;;                              MARK-SML-SELF-TAIL-CALLS)
;;;   (:lisp ty form)            CL from the direct code generator

(defvar *sml-ir-inline-size* 40
  "The largest IR term (in nodes) of a function inlined at its calls.")

(defvar *sml-ir-simplify-rounds* 3
  "How many times the simplifier runs over a term at most.")

(defvar *sml-ir-log* nil
  "When a cons, compiling pushes (LOWERED . SIMPLIFIED) for each term onto
its car (see SHOW-SML-IR).")

(defun ir (kind type &rest fields) (list* kind type fields))
(declaim (inline ir-kind ir-type))
(defun ir-kind (node) (first node))
(defun ir-type (node) (second node))

(defun ir-node-type (ast)
  "The SML type the native checker gives AST, or NIL."
  (and (consp ast)
       (native-sml-static-facts-p)
       (funcall 'cl-sml.types:native-node-type ast *sml-static-facts*)))

(defun ir-variable-p (form)
  (and form (symbolp form) (null (symbol-package form))))

;;; Lowering

(defvar *ir-template-mode* nil
  "True while a function is lowered for inlining: a construct the IR does
not cover throws to IR-NO-TEMPLATE instead of compiling it directly.")

(defun ir-fallback (ast local-exceptions lexical-env)
  (when *ir-template-mode*
    (throw 'ir-no-template nil))
  (ir :lisp nil (compile-expr-direct ast local-exceptions lexical-env)))

(defun ir-lower-tuple-binary-args (arg)
  "The two operand ASTs of an infix application's argument ARG, or NIL."
  (and (consp arg) (eq (car arg) :tuple) (= (length arg) 3)
       (list (second arg) (third arg))))

(defun ir-lower-overloaded (ast local-exceptions lexical-env)
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
            (ir :prim nil (list :overloaded name tyname)
                (mapcar (lambda (a) (ir-lower a local-exceptions lexical-env)) args))))))))

(defun ir-lower-equality (ast local-exceptions lexical-env)
  "Like COMPILE-EQUALITY-APPLICATION, or NIL."
  (when (and (consp (second ast)) (eq (car (second ast)) :var)
             (member (second (second ast)) '("=" "<>") :test #'equal)
             (ir-lower-tuple-binary-args (third ast))
             (builtin-binary-primitive (second ast) lexical-env))
    (let ((test (case (sml-equality-kind (second ast))
                  (:eql 'eql) (:eq 'eq) (:string 'string=))))
      (when test
        (ir :prim nil (list :equality test (string= (second (second ast)) "<>"))
            (mapcar (lambda (a) (ir-lower a local-exceptions lexical-env))
                    (ir-lower-tuple-binary-args (third ast))))))))

(defun ir-lower-binary-primitive (ast local-exceptions lexical-env)
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
                (ir :prim nil (list :word-binary word-operation) args)
                (ir :prim nil (list :binary primitive) args))))))))

(defun ir-lower-constructor-application (head arg local-exceptions lexical-env)
  "The application of constructor HEAD to ARG, as in COMPILE-EXPR-DIRECT."
  (let ((tag (lexical-constructor-tag-form (second head) lexical-env)))
    (if tag
        (ir :con nil :cons tag (list (ir-lower arg local-exceptions lexical-env)))
        (let* ((symbol (constructor-symbol-for-name (second head) lexical-env))
               (arity (constructor-arity-at-compile-time symbol)))
          (cond
            ((null arity)
             (ir :app nil (ir-lower head local-exceptions lexical-env)
                 (ir-lower arg local-exceptions lexical-env)))
            ((eql arity 0)
             (ir :con nil :boxed symbol (list (ir-lower arg local-exceptions lexical-env))))
            ((and (consp arg) (eq (car arg) :tuple) (= (length (cdr arg)) arity))
             (ir :con nil :flat symbol
                 (mapcar (lambda (item) (ir-lower item local-exceptions lexical-env))
                         (cdr arg))))
            (t (ir :con nil :flat-construct symbol
                   (list (ir-lower arg local-exceptions lexical-env)))))))))

(defun ir-lower-tupled-call (head arg local-exceptions lexical-env)
  "Like COMPILE-TUPLED-CALL, or NIL."
  (when (and (consp head) (eq (car head) :var)
             (consp arg) (eq (car arg) :tuple))
    (let* ((symbol (resolved-sml-symbol (second head) lexical-env))
           (worker (and (symbolp symbol) (symbol-package symbol)
                        (get symbol 'sml-tupled-worker))))
      (when (and worker (= (cdr worker) (length (cdr arg))))
        (ir :call nil (car worker)
            (mapcar (lambda (item) (ir-lower item local-exceptions lexical-env))
                    (cdr arg))
            symbol)))))

(defun ir-lower-match-branches (branches local-exceptions lexical-env)
  "The (pattern body) branches of a match, as COMPILE-TRIVIA-CASE-CLAUSE
compiles them."
  (mapcar (lambda (branch)
            (let* ((var-map (pattern-variable-map (first branch) local-exceptions))
                   (branch-env (append var-map lexical-env)))
              (list (compile-pat (first branch) local-exceptions branch-env)
                    (ir-lower (second branch) local-exceptions branch-env))))
          branches))

(defun ir-lower-record (fields local-exceptions lexical-env)
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
           (field-nodes
             (mapcar (lambda (field)
                       (if temporaries
                           (ir :var nil (nth (position field fields) temporaries))
                           (ir-lower (second field) local-exceptions lexical-env)))
                     layout))
           (node (if (loop for label in labels
                           for i from 1
                           always (string= label (princ-to-string i)))
                     (ir :tuple nil field-nodes)
                     (ir :record nil labels field-nodes))))
      (if temporaries
          (reduce (lambda (pair body)
                    (ir :let nil (car pair)
                        (ir-lower (second (cdr pair)) local-exceptions lexical-env)
                        body))
                  (mapcar #'cons temporaries fields)
                  :from-end t :initial-value node)
          node))))

(defun ir-simple-let-declaration-p (dec)
  "Can the IR lower DEC, a declaration of a `let', itself?"
  (member (car dec) '(:val :fun :funs :val-rec :val-recs :type :infix :signature)))

(defun ir-lower-let (ast local-exceptions lexical-env)
  (let ((decs (second ast))
        (body-asts (third ast)))
    (if (or (notevery #'ir-simple-let-declaration-p decs)
            (declarations-local-module-member-bindings decs))
        (ir-fallback ast local-exceptions lexical-env)
        (labels ((body (lexical-env)
                   (let ((items (mapcar (lambda (e) (ir-lower e local-exceptions lexical-env))
                                        body-asts)))
                     (if (rest items) (ir :seq nil items) (first items))))
                 (decs (decs lexical-env)
                   (if (null decs)
                       (body lexical-env)
                       (let* ((dec (first decs))
                              (bindings (local-declaration-lexical-bindings dec))
                              (inner-env (append bindings lexical-env)))
                         (ir-lower-let-declaration
                          dec bindings local-exceptions lexical-env
                          (lambda () (decs (rest decs) inner-env)))))))
          (decs decs lexical-env)))))

(defun ir-lower-let-declaration (dec bindings local-exceptions lexical-env body-thunk)
  "DEC, a declaration of a `let' (IR-SIMPLE-LET-DECLARATION-P), around the
term BODY-THUNK returns, as COMPILE-LOCAL-DECL compiles it."
  (ecase (car dec)
    ((:type :infix :signature) (funcall body-thunk))
    (:val
     (let ((pat (second dec))
           (value (ir-lower (third dec) local-exceptions lexical-env)))
       (cond
         ((and (listp pat) (member (car pat) '(:pat-var :var)))
          (ir :let nil (local-binding-symbol (second pat) bindings) value
              (funcall body-thunk)))
         ((eq pat :wild)
          (ir :let nil (gensym "IGNORED") value (funcall body-thunk)))
         (t
          (let ((tmp (gensym "MATCHED")))
            (ir :let nil tmp value
                (ir :match nil (ir :var nil tmp)
                    (list (list (let ((*sml-module-prefix* nil))
                                  (compile-pat pat local-exceptions
                                               (append bindings lexical-env)))
                                (funcall body-thunk)))
                    (compile-match-failure "Bind" "val" pat))))))))
    ((:fun :funs)
     (when *ir-template-mode*
       (throw 'ir-no-template nil))
     (let* ((funs (if (eq (car dec) :fun) (list dec) (cdr dec)))
            (env (append bindings lexical-env)))
       (ir :letrec nil
           (mapcar (lambda (fun-dec)
                     (list (local-binding-symbol (second fun-dec) bindings)
                           (ir :lisp nil (compile-fn-clauses (third fun-dec) local-exceptions
                                                             (second fun-dec) env))))
                   funs)
           (funcall body-thunk))))
    ((:val-rec :val-recs)
     (let* ((recs (if (eq (car dec) :val-rec) (list dec) (cdr dec)))
            (env (append bindings lexical-env)))
       (ir :letrec nil
           (mapcar (lambda (rec)
                     (list (local-binding-symbol (second rec) bindings)
                           (ir-lower (third rec) local-exceptions env)))
                   recs)
           (funcall body-thunk))))))

(defun ir-lower-name (ast name lexical-env)
  "A variable or constructor occurrence NAME."
  (let ((form (resolved-sml-symbol name lexical-env)))
    (if (ir-variable-p form)
        (ir :var (ir-node-type ast) form)
        (ir :ref (ir-node-type ast) form))))

(defun ir-lower (ast local-exceptions lexical-env)
  "The IR term of expression AST (see COMPILE-EXPR-DIRECT, which this
follows case by case)."
  (cond
    ((or (numberp ast) (stringp ast) (characterp ast)) (ir :const nil ast))
    ((atom ast) (ir-fallback ast local-exceptions lexical-env))
    (t
     (case (car ast)
       (:var
        ;; `op +' and the like as values.
        (multiple-value-bind (name tyname) (overloaded-identifier-instance ast)
          (let ((value (and name (compile-overloaded-identifier-value name tyname))))
            (if value
                (ir :lisp nil value)
                (ir-lower-name ast (second ast) lexical-env)))))
       (:op-var (ir-lower `(:var ,(second ast)) local-exceptions lexical-env))
       (:word (ir :const (ir-node-type ast) (second ast)))
       (:ctor
        (cond ((string= (second ast) "true") (ir :const nil t))
              ((member (second ast) '("false" "nil") :test #'string=) (ir :const nil nil))
              (t (ir-lower-name ast (second ast) lexical-env))))
       (:typed (ir-lower (second ast) local-exceptions lexical-env))
       (:deref (ir :prim nil '(:deref)
                   (list (ir-lower (second ast) local-exceptions lexical-env))))
       (:infix-app
        (ir :app nil (ir-lower `(:var ,(second ast)) local-exceptions lexical-env)
            (ir :tuple nil (list (ir-lower (third ast) local-exceptions lexical-env)
                                 (ir-lower (fourth ast) local-exceptions lexical-env)))))
       (:app (ir-lower-application ast local-exceptions lexical-env))
       (:case
        (let ((branches (cddr ast)))
          (if (or (literal-case-branches-p branches)
                  (tuple-dispatch-case-branches-p branches))
              (ir-fallback ast local-exceptions lexical-env)
              (ir :match nil (ir-lower (second ast) local-exceptions lexical-env)
                  (ir-lower-match-branches branches local-exceptions lexical-env)
                  (compile-match-failure "Match" "case" (first (first branches)))))))
       (:handle
        (ir :handle nil (ir-lower (second ast) local-exceptions lexical-env)
            (ir-lower-match-branches (third ast) local-exceptions lexical-env)))
       (:if (ir :if nil (ir-lower (second ast) local-exceptions lexical-env)
                (ir-lower (third ast) local-exceptions lexical-env)
                (ir-lower (fourth ast) local-exceptions lexical-env)))
       (:while (ir :while nil (ir-lower (second ast) local-exceptions lexical-env)
                   (ir-lower (third ast) local-exceptions lexical-env)))
       (:raise (ir :raise nil (ir-lower (second ast) local-exceptions lexical-env)))
       (:let (ir-lower-let ast local-exceptions lexical-env))
       (:andalso (ir :if nil (ir-lower (second ast) local-exceptions lexical-env)
                     (ir-lower (third ast) local-exceptions lexical-env)
                     (ir :const nil nil)))
       (:orelse (ir :if nil (ir-lower (second ast) local-exceptions lexical-env)
                    (ir :const nil t)
                    (ir-lower (third ast) local-exceptions lexical-env)))
       (:seq (ir :seq nil (mapcar (lambda (e) (ir-lower e local-exceptions lexical-env))
                                  (cdr ast))))
       (:list (ir :prim nil '(:list)
                  (mapcar (lambda (e) (ir-lower e local-exceptions lexical-env))
                          (cdr ast))))
       (:record (ir-lower-record (cdr ast) local-exceptions lexical-env))
       (:unit (ir :unit nil))
       (:tuple (ir :tuple nil (mapcar (lambda (e) (ir-lower e local-exceptions lexical-env))
                                      (cdr ast))))
       (:self-tail-call
        (when *ir-template-mode*
          (throw 'ir-no-template nil))
        (ir :tail-call nil (second ast)
            (mapcar (lambda (e) (ir-lower e local-exceptions lexical-env)) (cddr ast))))
       (:fn
        (let ((clauses (second ast))
              (arg (gensym "ARG")))
          (ir :fn nil (list arg)
              (ir :match nil (ir :var nil arg)
                  (ir-lower-match-branches clauses local-exceptions lexical-env)
                  (compile-match-failure "Match" "fn" (first (first clauses))))
              nil)))
       (otherwise (ir-fallback ast local-exceptions lexical-env))))))

(defun ir-lower-application (ast local-exceptions lexical-env)
  (destructuring-bind (head arg) (cdr ast)
    (cond
      ;; `~' applied to a word literal is modular negation.
      ((and (equal head '(:var "~")) (consp arg) (eq (car arg) :word))
       (ir :const nil (logand +sml-word-mask+ (- (second arg)))))
      ((ir-lower-overloaded ast local-exceptions lexical-env))
      ((ir-lower-equality ast local-exceptions lexical-env))
      ((ir-lower-binary-primitive ast local-exceptions lexical-env))
      ((and (consp head) (eq (car head) :selector))
       (let* ((label (second head))
              (labels (sml-node-record-labels head))
              (index (and labels (position label labels :test #'string=)))
              (record (ir-lower arg local-exceptions lexical-env)))
         (if index
             (ir :select nil (1+ index) record)
             (ir :select-label nil label record))))
      ((and (consp head) (eq (car head) :ctor)
            (not (exception-constructor-info (second head) local-exceptions))
            (or (lexical-constructor-tag-form (second head) lexical-env)
                (known-data-constructor-application-p (second head) lexical-env)))
       (ir-lower-constructor-application head arg local-exceptions lexical-env))
      ((ir-lower-tupled-call head arg local-exceptions lexical-env))
      (t (ir :app nil (ir-lower head local-exceptions lexical-env)
             (ir-lower arg local-exceptions lexical-env))))))

;;; Emission

(defun ir-emit-list (nodes) (mapcar #'ir-emit nodes))

(defun ir-emit-match-branches (branches)
  (mapcar (lambda (branch) (list (first branch) (ir-emit (second branch)))) branches))

(defun ir-emit (node)
  "The CL form of IR term NODE."
  (ecase (ir-kind node)
    (:const (third node))
    (:unit '(sml-unit))
    (:var (third node))
    (:ref (third node))
    (:lisp (third node))
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
    (:app `(funcall ,(ir-emit (third node)) ,(ir-emit (fourth node))))
    (:call `(,(third node) ,@(ir-emit-list (fourth node))))
    (:fn (destructuring-bind (params body name) (cddr node)
           (if (= (length params) 1)
               (sml-lambda-form name (first params) (ir-emit body))
               `(lambda ,params ,@(portable-tail-call-declarations) ,(ir-emit body)))))
    (:let (destructuring-bind (symbol value body) (cddr node)
            `(let ((,symbol ,(ir-emit value)))
               (declare (ignorable ,symbol))
               ,(ir-emit body))))
    (:letrec (destructuring-bind (bindings body) (cddr node)
               `(let ,(mapcar (lambda (binding) `(,(first binding) nil)) bindings)
                  ,@(mapcar (lambda (binding)
                              `(setf ,(first binding) ,(ir-emit (second binding))))
                            bindings)
                  ,(ir-emit body))))
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
              (trivia:match value
                ,@(ir-emit-match-branches (fourth node))
                (_ (sml-raise value))))))))
    (:match
     (destructuring-bind (scrutinee branches failure) (cddr node)
       `(trivia:match ,(ir-emit scrutinee)
          ,@(ir-emit-match-branches branches)
          (_ ,failure))))
    (:tail-call
     (destructuring-bind (tag &rest variables) (third node)
       `(progn
          (psetq ,@(mapcan #'list variables (ir-emit-list (fourth node))))
          (go ,tag))))))

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

;;; Inline templates of top-level functions.  A `fun' of one clause whose
;;; parameters are variables (or one tuple of variables, for a function
;;; with a tupled worker), that is not recursive and whose body the IR
;;; covers entirely, records its IR on its cell, which is bound once and
;;; never assigned; the simplifier then inlines its calls.

;; IR-MAP rebuilds a node with each IR term directly inside it replaced.
(defun ir-map (function node)
  (flet ((f (x) (funcall function x))
         (fs (xs) (mapcar function xs))
         (branches (bs) (mapcar (lambda (b) (list (first b) (funcall function (second b)))) bs)))
    (destructuring-bind (kind type &rest fields) node
      (list* kind type
             (ecase kind
               ((:const :unit :var :ref :lisp) fields)
               (:prim (list (first fields) (fs (second fields))))
               (:tuple (list (fs (first fields))))
               (:record (list (first fields) (fs (second fields))))
               ((:select :select-label) (list (first fields) (f (second fields))))
               (:con (list (first fields) (second fields) (fs (third fields))))
               (:app (list (f (first fields)) (f (second fields))))
               (:call (list (first fields) (fs (second fields)) (third fields)))
               (:fn (list (first fields) (f (second fields)) (third fields)))
               (:let (list (first fields) (f (second fields)) (f (third fields))))
               (:letrec (list (branches (first fields)) (f (second fields))))
               (:seq (list (fs (first fields))))
               (:if (fs fields))
               (:while (fs fields))
               (:raise (fs fields))
               (:handle (list (f (first fields)) (branches (second fields))))
               (:match (list (f (first fields)) (branches (second fields)) (third fields)))
               (:tail-call (list (first fields) (fs (second fields)))))))))

(defun ir-strip-types (node)
  "NODE with its types removed, so that it can be written to a FASL."
  (let ((node (ir-map #'ir-strip-types node)))
    (setf (second node) nil)
    node))

(defun ir-variable-pattern-name (pat)
  (loop while (and (consp pat) (eq (car pat) :pat-typed))
        do (setf pat (second pat)))
  (cond ((and (consp pat) (member (car pat) '(:pat-var :var))) (second pat))
        ((eq pat :wild) :wild)))

(defun ir-mentions-p (node symbol)
  (cond ((eq node symbol) t)
        ((consp node) (or (ir-mentions-p (car node) symbol)
                          (ir-mentions-p (cdr node) symbol)))))

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
                 (body (catch 'ir-no-template
                         (let ((*ir-template-mode* t)
                               (*sml-hoisting-enabled* nil)
                               (*sml-binding-symbol-env*
                                 (acons (second ast) cell *sml-binding-symbol-env*)))
                           (ir-lower (second (first clauses)) local-exceptions env)))))
            (when body
              (let* ((body (ir-simplify body))
                     (template (if worker-arity
                                   (ir :fn nil symbols body nil)
                                   (reduce (lambda (symbol body) (ir :fn nil (list symbol) body nil))
                                           symbols :from-end t :initial-value body))))
                (when (and (<= (ir-size template) *sml-ir-inline-size*)
                           (not (ir-mentions-p template cell))
                           (null (ir-free-variables template)))
                  (let ((template (ir-strip-types template)))
                    (setf (get cell 'sml-ir-inline) template)
                    `((setf (get ',cell 'sml-ir-inline) ',template))))))))))))

(defun ir-size (node)
  (if (and (consp node) (keywordp (first node)))
      (1+ (loop for field in (cddr node)
                sum (ir-field-size field)))
      0))

(defun ir-field-size (field)
  (cond ((atom field) 0)
        ((keywordp (first field)) (if (eq (first field) :lisp) 1000 (ir-size field)))
        (t (loop for x in field sum (ir-field-size x)))))

(defun ir-free-variables (node)
  "The uninterned symbols NODE uses without binding them."
  (let ((bound (make-hash-table :test #'eq))
        (free nil))
    (labels ((bind (symbol) (setf (gethash symbol bound) t))
             (bind-pattern (pattern)
               (cond ((ir-variable-p pattern) (bind pattern))
                     ((consp pattern) (bind-pattern (car pattern))
                      (bind-pattern (cdr pattern)))))
             (use (symbol)
               (unless (gethash symbol bound) (pushnew symbol free)))
             (walk (node)
               (case (ir-kind node)
                 (:var (use (third node)))
                 ((:const :unit :ref :lisp) nil)
                 (:fn (mapc #'bind (third node)) (walk (fourth node)))
                 (:let (bind (third node)) (walk (fourth node)) (walk (fifth node)))
                 (:letrec (dolist (b (third node)) (bind (first b)))
                  (dolist (b (third node)) (walk (second b)))
                  (walk (fourth node)))
                 ((:match :handle)
                  (dolist (branch (if (eq (ir-kind node) :match) (fourth node) (fourth node)))
                    (bind-pattern (first branch)))
                  (walk (third node))
                  (dolist (branch (fourth node)) (walk (second branch))))
                 (otherwise (ir-walk-children #'walk node)))))
      (walk node))
    free))

(defun ir-walk-children (function node)
  "Call FUNCTION on each IR term directly inside NODE."
  (case (ir-kind node)
    ((:const :unit :var :ref :lisp) nil)
    ((:prim :tuple) (mapc function (if (eq (ir-kind node) :prim) (fourth node) (third node))))
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
    ((:handle :match) (funcall function (third node))
     (dolist (branch (fourth node)) (funcall function (second branch))))
    (:tail-call (mapc function (fourth node)))))

;;; Copying with fresh variables, for inlining.

(defun ir-map-binders (function node)
  "Call FUNCTION on each symbol a binder inside NODE binds: parameters, let
and letrec variables, and the uninterned symbols of patterns."
  (labels ((pattern (form)
             (cond ((ir-variable-p form) (funcall function form))
                   ((consp form) (pattern (car form)) (pattern (cdr form)))))
           (walk (node)
             (case (ir-kind node)
               (:fn (mapc function (third node)))
               (:let (funcall function (third node)))
               (:letrec (dolist (binding (third node)) (funcall function (first binding))))
               ((:match :handle) (dolist (branch (fourth node)) (pattern (first branch)))))
             (ir-walk-children #'walk node)))
    (walk node)))

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
  "Uninterned symbols used in CL forms (:lisp nodes, patterns) or assigned
(self tail calls): they are neither substituted nor dropped.")

(defun ir-take-census (node)
  (let ((census (make-hash-table :test #'eq))
        (opaque (make-hash-table :test #'eq)))
    (labels ((scan (form)
               (cond ((ir-variable-p form)
                      (incf (gethash form census 0))
                      (setf (gethash form opaque) t))
                     ((consp form) (scan (car form)) (scan (cdr form)))))
             (walk (node)
               (case (ir-kind node)
                 (:var (incf (gethash (third node) census 0)))
                 (:lisp (scan (third node)))
                 (:tail-call (scan (third node)) (mapc #'walk (fourth node)))
                 ((:match :handle)
                  (walk (third node))
                  (dolist (branch (fourth node))
                    ;; A variable pattern binds; others may refer to
                    ;; variables (local exception constructors).
                    (unless (ir-variable-p (first branch))
                      (scan (first branch)))
                    (walk (second branch)))
                  (when (eq (ir-kind node) :match) (scan (fifth node))))
                 (otherwise (ir-walk-children #'walk node)))))
      ;; Every variable bound in NODE has an entry, so that one missing is
      ;; one made since (see IR-USES).
      (ir-map-binders (lambda (symbol)
                        (unless (nth-value 1 (gethash symbol census))
                          (setf (gethash symbol census) 0)))
                      node)
      (walk node))
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

(defun ir-fold-primitive (op args)
  "The constant term OP gives on constant ARGS, or NIL.  Never folds an
operation that raises (Overflow, Div, ...) on these arguments."
  (let ((values (mapcar (lambda (arg) (multiple-value-list (ir-constant-value arg))) args)))
    (unless (every #'second values)
      (return-from ir-fold-primitive nil))
    (let ((values (mapcar #'first values)))
      (flet ((const (value) (ir :const nil value)))
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
  (reduce (lambda (pair body) (ir :let nil (car pair) (cdr pair) body))
          (mapcar #'cons (third fn) args)
          :from-end t :initial-value (fourth fn)))

(defun ir-simplify-node (node env)
  "One simplification of NODE.  ENV maps let-bound variables to known
values (tuples, constructors, functions) or to a term to substitute."
  (macrolet ((simp (x) `(ir-simplify-node ,x env)))
    (case (ir-kind node)
      ((:const :unit :ref :lisp) node)
      (:var
       (let ((known (gethash (third node) env)))
         (if (and known (eq (car known) :substitute))
             (cdr known)
             node)))
      (:prim
       (let* ((op (third node))
              (args (mapcar (lambda (a) (simp a)) (fourth node))))
         (or (ir-fold-primitive op args)
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
                                           (member (ir-kind (second binding)) '(:fn :lisp))
                                           (or (eq (ir-kind (second binding)) :fn)
                                               ;; A function the direct
                                               ;; generator compiled.
                                               (let ((form (third (second binding))))
                                                 (and (consp form)
                                                      (member (first form)
                                                              '(lambda named-lambda
                                                                #+sbcl sb-int:named-lambda)))))))
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
                   (mapcar (lambda (branch) (list (first branch) (simp (second branch))))
                           (fourth node))))
      (:match (ir-simplify-match node env))
      (:tail-call (ir :tail-call (ir-type node) (third node)
                      (mapcar (lambda (a) (simp a)) (fourth node)))))))

(defun ir-simplify-let (node env)
  (destructuring-bind (symbol value body) (cddr node)
    (let ((value (ir-simplify-node value env)))
      (cond
        ;; let x = (let y = a in b) in c  =>  let y = a in let x = b in c
        ((eq (ir-kind value) :let)
         (destructuring-bind (inner-symbol inner-value inner-body) (cddr value)
           (ir :let (ir-type value) inner-symbol inner-value
               (ir-simplify-let (ir :let (ir-type node) symbol inner-body body) env))))
        ((ir-opaque-p symbol)
         (ir :let (ir-type node) symbol value (ir-simplify-node body env)))
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
         (ir :let (ir-type function) symbol value
             (ir-simplify-application (ir :app (ir-type node) body arg) env))))
      ;; not e
      ((and (eq (ir-kind function) :ref) (equal (third function) '(function sml-not)))
       (or (ir-fold-primitive '(:not) (list arg))
           (ir :prim (ir-type node) '(:not) (list arg))))
      ;; (f o g) x  =>  f (g x), when f and g need not be evaluated first.
      ((and (eq (ir-kind function) :prim)
            (equal (third function) '(:binary sml-compose))
            (every #'ir-trivial-p (fourth function)))
       (destructuring-bind (f g) (fourth function)
         (ir-simplify-application
          (ir :app (ir-type node) f (ir :app nil g arg)) env)))
      ;; A Basis primitive the IR opens.
      ((and (eq (ir-kind function) :ref)
            (symbolp (third function)) (symbol-package (third function))
            (get (third function) 'sml-ir-primitive)
            (let ((arity (ir-basis-primitive-arity (get (third function) 'sml-ir-primitive))))
              (or (eql arity 1)
                  (and (eql arity 2) (eq (ir-kind arg) :tuple) (= (length (third arg)) 2)))))
       (let* ((name (get (third function) 'sml-ir-primitive))
              (args (if (eql (ir-basis-primitive-arity name) 1) (list arg) (third arg))))
         (or (ir-fold-primitive (list :basis name) args)
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

(defun ir-simplify-match (node env)
  (destructuring-bind (scrutinee branches failure) (cddr node)
    (let* ((scrutinee (ir-simplify-node scrutinee env))
           (first-pattern (first (first branches))))
      (cond
        ;; The first rule's pattern is a variable or a wildcard.
        ((ir-variable-p first-pattern)
         (ir-simplify-node (ir :let (ir-type node) first-pattern scrutinee
                               (second (first branches)))
                           env))
        ((eq first-pattern '_)
         (let ((body (ir-simplify-node (second (first branches)) env)))
           (if (ir-pure-p scrutinee)
               body
               (ir :seq (ir-type node) (list scrutinee body)))))
        ;; A boolean match.
        ((and (= (length branches) 2)
              (member first-pattern '(t nil))
              (let ((second-pattern (first (second branches))))
                (or (eq second-pattern '_)
                    (and (member second-pattern '(t nil))
                         (not (eq second-pattern first-pattern))))))
         (let ((then (second (first branches)))
               (else (second (second branches))))
           (ir-simplify-node (if first-pattern
                                 (ir :if (ir-type node) scrutinee then else)
                                 (ir :if (ir-type node) scrutinee else then))
                             env)))
        (t
         (ir :match (ir-type node) scrutinee
             (mapcar (lambda (branch)
                       (list (first branch) (ir-simplify-node (second branch) env)))
                     branches)
             failure))))))

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
  (member (ir-kind node) '(:const :unit :var :ref :lisp)))

(defun ir-compile-expression (ast local-exceptions lexical-env)
  "The CL form of expression AST, compiled through the IR."
  (let* ((lowered (ir-lower ast local-exceptions lexical-env))
         (simplified (if (and *sml-ir-simplify* (not (ir-trivial-term-p lowered)))
                         (ir-simplify lowered)
                         lowered)))
    (when (and *sml-ir-log* (not (ir-trivial-term-p lowered)))
      (push (cons lowered simplified) (car *sml-ir-log*)))
    (ir-emit simplified)))
