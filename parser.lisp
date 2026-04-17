(in-package #:cl-sml)

;; We need a helper function to compile left-associative operators at parse-time.
;; It translates `a + b + c` into `((+ a) b) (+ c)` (Curried AST form)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun build-infix-ast (first rest)
    (if (null rest)
        first
        (reduce (lambda (left group)
                  (let ((op (second group))
                        (right (fourth group)))
                    ;; Creates an AST of: (:app (:app (:var "+") left) right)
                    `(:app (:app (:var ,op) ,left) ,right)))
                rest
                :initial-value first)))
  (defun decode-sml-string-literal (token)
    (with-output-to-string (out)
      (loop for i from 1 below (1- (length token))
            for ch = (char token i)
            do (if (char= ch #\\)
                   (progn
                     (incf i)
                     (when (>= i (1- (length token)))
                       (error "Invalid SML string literal: ~A" token))
                     (write-char
                      (case (char token i)
                        (#\\ #\\)
                        (#\" #\")
                        (#\n #\Newline)
                        (#\t #\Tab)
                        (#\r #\Return)
                        (t (char token i)))
                      out))
                   (write-char ch out)))))
  (defun decode-sml-char-literal (token)
    (let ((string-value (decode-sml-string-literal (subseq token 1))))
      (unless (= (length string-value) 1)
        (error "Invalid SML char literal: ~A" token))
      (char string-value 0))))

(defrule sml-comment-char
  (and (! "(*") (! "*)") character))

(defrule sml-comment-body (* (or sml-comment sml-comment-char)))

(defrule sml-comment (and "(*" sml-comment-body "*)")
  (:constant nil))

;; Whitespace
(defrule ws (* (or #\Space #\Tab #\Newline sml-comment)) (:constant nil))

;; --- KEYWORD AND ID RULES ---
;; Define reserved keywords (added andalso, orelse, if, then, else)
(defrule sml-keyword
  (and (or "val" "rec" "fun" "fn" "case" "of" "if" "then" "else" "let" "in" "end" "datatype" "andalso" "orelse")
       (! (or (alphanumericp character) #\_ #\'))))

;; A raw identifier is any standard word
(defrule sml-id-raw (and (alpha-char-p character) (* (or (alphanumericp character) #\_ #\')))
  (:text t))

(defrule sml-capitalized-id (and (character-ranges (#\A #\Z)) (* (or (alphanumericp character) #\_ #\')))
  (:text t))

;; A valid SML identifier is a raw ID that is NOT a keyword
(defrule sml-id (and (! sml-keyword) sml-id-raw)
  (:destructure (not-kw id)
    (declare (ignore not-kw))
    id))

(defrule sml-int (and (? "~") (+ (character-ranges (#\0 #\9))))
  (:destructure (neg digits)
    (let ((n (parse-integer (text digits))))
      (if neg (- n) n))))

(defrule sml-real
  (and (? "~") (+ (character-ranges (#\0 #\9))) "." (+ (character-ranges (#\0 #\9))))
  (:destructure (neg whole dot frac)
    (declare (ignore dot))
    (let* ((whole-part (parse-integer (text whole)))
           (frac-text (text frac))
           (frac-part (parse-integer frac-text))
           (scale (expt 10 (length frac-text)))
           (value (+ whole-part (/ frac-part scale))))
      (coerce (if neg (- value) value) 'double-float))))

(defrule sml-string
  (and #\" (* (or (and #\\ character)
                  (and (! (or #\\ #\")) character)))
       #\")
  (:text t)
  (:lambda (token)
    (decode-sml-string-literal token)))

(defrule sml-char
  (and #\# #\" (* (or (and #\\ character)
                      (and (! (or #\\ #\")) character)))
       #\")
  (:text t)
  (:lambda (token)
    (decode-sml-char-literal token)))

(defrule sml-pat-ctor-head sml-capitalized-id
  (:lambda (name)
    `(:pat-ctor ,name)))

;; Notice we check upper/lower case to distinguish variables from constructors!
(defrule sml-pat-var-or-ctor sml-id
  (:lambda (name)
    (if (upper-case-p (char name 0))
        `(:pat-ctor ,name)
        `(:pat-var ,name))))

(defrule sml-var-or-ctor sml-id
  (:lambda (name)
    (if (upper-case-p (char name 0))
        `(:ctor ,name)
        `(:var ,name))))

;; Operators
(defrule sml-op-mult (or "*" "div" "mod" "/") (:text t))
(defrule sml-op-add  (or "+" "-" "^") (:text t))
(defrule sml-op-rel  (or "<=" ">=" "<>" "<" ">" "=") (:text t))
(defrule sml-op-list (or "::" "@") (:text t))

;; --- NEW DATATYPE RULES ---
;; Parse everything after "of" until we hit a "|" or ";"
;; We do this because Lisp doesn't need the static type info at runtime!
(defrule sml-type-blob (and ws "of" (+ (and (! (or "|" ";")) character)))
  (:constant t))

(defrule sml-ctor-def (and ws sml-id (? sml-type-blob))
  (:destructure (w1 name has-args) (declare (ignore w1))
    (if has-args
        `(:ctor-def ,name :has-args t)
        `(:ctor-def ,name :has-args nil))))

(defrule sml-ctor-defs (and sml-ctor-def (* (and ws "|" sml-ctor-def)))
  (:destructure (first rest)
    (cons first (mapcar #'third rest))))

(defrule sml-datatype (and "datatype" ws sml-id ws "=" sml-ctor-defs ws ";")
  (:destructure (dt w1 name w2 eq defs w3 semi) (declare (ignore dt w1 w2 eq w3 semi))
    `(:datatype ,name ,defs)))


(defrule sml-decs (* (and ws (or sml-datatype sml-val-rec sml-val sml-fun) ws))
  (:destructure (&rest items)
    (mapcar #'second items)))

(defrule sml-let (and "let" ws sml-decs ws "in" ws sml-expr (* (and ws ";" ws sml-expr)) ws (? ";") ws "end")
  (:destructure (let-kw w1 decs w2 in-kw w3 e1 rest w4 opt-semi w5 end-kw)
    (declare (ignore let-kw w1 w2 in-kw w3 w4 opt-semi w5 end-kw))
    `(:let ,decs (,e1 ,@(mapcar #'fourth rest)))))

;; --- LIST EXPRESSIONS ---
;; Parse comma-separated list elements
(defrule sml-list-elements (and sml-expr (* (and ws "," ws sml-expr)))
  (:destructure (e1 rest)
    (cons e1 (mapcar #'fourth rest))))

;; Parse the actual list [ ... ]
(defrule sml-list (and "[" ws (? sml-list-elements) ws "]")
  (:destructure (lb w1 elems w2 rb) (declare (ignore lb w1 w2 rb))
    `(:list ,@elems)))

(defrule sml-paren-elements (and sml-expr (* (and ws "," ws sml-expr)))
  (:destructure (first rest)
    (cons first (mapcar #'fourth rest))))

;; Base Expressions
(defrule sml-atomic (or sml-let sml-list sml-char sml-string sml-real sml-int sml-var-or-ctor sml-parens))

(defrule sml-deref (and "!" ws sml-prefix)
  (:destructure (bang w expr) (declare (ignore bang w))
    `(:deref ,expr)))

(defrule sml-prefix (or sml-deref sml-atomic))

(defrule sml-parens (and "(" ws (? sml-paren-elements) ws ")")
  (:destructure (lp w1 elems w2 rp) (declare (ignore lp w1 w2 rp))
    (cond
      ((null elems) '(:unit))
      ((null (rest elems)) (first elems))
      (t `(:tuple ,@elems)))))

;; Application: f x y
(defrule sml-app (and sml-prefix (* (and ws sml-prefix)))
  (:destructure (first rest)
    (if (null rest)
        first
        (reduce (lambda (left group)
                  `(:app ,left ,(second group)))
                rest
                :initial-value first))))

;; --- INFIX PRECEDENCE CLIMBING ---

;; 1. Multiplication level (*, div, mod, /)
(defrule sml-mult-expr (and sml-app (* (and ws sml-op-mult ws sml-app)))
  (:destructure (first rest) (build-infix-ast first rest)))

;; 2. Addition level (+, -, ^)
(defrule sml-add-expr (and sml-mult-expr (* (and ws sml-op-add ws sml-mult-expr)))
  (:destructure (first rest) (build-infix-ast first rest)))


;; 4. Logical AND (andalso)
(defrule sml-andalso-expr (and sml-rel-expr (* (and ws "andalso" ws sml-rel-expr)))
  (:destructure (first rest)
    (if (null rest)
        first
        (reduce (lambda (left group) `(:andalso ,left ,(fourth group)))
                rest :initial-value first))))

;; 5. Logical OR (orelse)
(defrule sml-orelse-expr (and sml-andalso-expr (* (and ws "orelse" ws sml-andalso-expr)))
  (:destructure (first rest)
    (if (null rest)
        first
        (reduce (lambda (left group) `(:orelse ,left ,(fourth group)))
                rest :initial-value first))))

(defrule sml-list-expr (and sml-add-expr (? (and ws sml-op-list ws sml-list-expr)))
  (:destructure (left opt-right)
    (if opt-right
        `(:app (:app (:var ,(second opt-right)) ,left) ,(fourth opt-right))
        left)))

;; 3. Relational level (=, <, >)
(defrule sml-rel-expr (and sml-list-expr (* (and ws sml-op-rel ws sml-add-expr)))
  (:destructure (first rest) (build-infix-ast first rest)))

;; --- CONTROL FLOW & PATTERN MATCHING ---

(defrule sml-if (and "if" ws sml-expr ws "then" ws sml-expr ws "else" ws sml-expr)
  (:destructure (i w1 cond w2 t1 w3 then-expr w4 e1 w5 else-expr)
    (declare (ignore i w1 w2 t1 w3 w4 e1 w5))
    `(:if ,cond ,then-expr ,else-expr)))

;; --- LIST PATTERNS (For Case Statements) ---
;; Parse empty list pattern []
(defrule sml-pat-empty-list (and "[" ws "]")
  (:constant '(:pat-nil)))

;; Parse the cons pattern: h :: t
;; (Keeping it simple for now with two variables: e.g., x :: xs)
(defrule sml-pat-cons (and sml-id ws "::" ws sml-id)
  (:destructure (h w1 op w2 t-var) (declare (ignore w1 op w2))
    `(:pat-cons ,h ,t-var)))


(defrule sml-pat-app (and sml-pat-ctor-head ws sml-pat-primary)
  (:destructure (ctor w pat) (declare (ignore w))
    (if (eq (car ctor) :pat-ctor)
        `(:pat-app ,ctor ,pat)
        ;; If it wasn't capitalized, it's not a valid pattern app in SML
        (error "Pattern application head must be a Constructor (Capitalized)"))))

(defrule sml-pat-parens (and "(" ws sml-pat ws ")")
  (:destructure (lp w1 pat w2 rp) (declare (ignore lp w1 w2 rp)) pat))

(defrule sml-pat-paren-elements (and sml-pat (* (and ws "," ws sml-pat)))
  (:destructure (first rest)
    (cons first (mapcar #'fourth rest))))

(defrule sml-pat-tuple-or-parens (and "(" ws (? sml-pat-paren-elements) ws ")")
  (:destructure (lp w1 elems w2 rp) (declare (ignore lp w1 w2 rp))
    (cond
      ((null elems) '(:pat-unit))
      ((null (rest elems)) (first elems))
      (t `(:pat-tuple ,@elems)))))


(defrule sml-pat-primary
    (or sml-real
        sml-int
        sml-string
        sml-char
        sml-pat-var-or-ctor
        (and "_" (:constant :wild))
        sml-pat-tuple-or-parens))

;; Patterns for case statements
;; Order is critical! Complex patterns (cons, app) must come before simple vars

(defrule sml-pat (or sml-pat-cons
                     sml-pat-empty-list
                     sml-pat-app
                     sml-pat-primary))

;; Case statement
(defrule sml-match-branch (and ws "|" ws sml-pat ws "=>" ws sml-expr)
  (:destructure (w1 bar w2 pat w3 arr w4 expr) (declare (ignore w1 bar w2 w3 arr w4))
    `(,pat ,expr)))

(defrule sml-first-branch (and sml-pat ws "=>" ws sml-expr)
  (:destructure (pat w1 arr w2 expr) (declare (ignore w1 arr w2))
    `(,pat ,expr)))

(defrule sml-case (and "case" ws sml-expr ws "of" ws sml-first-branch (* sml-match-branch))
  (:destructure (c w1 expr w2 o w3 first rest) (declare (ignore c w1 w2 o w3))
    `(:case ,expr ,first ,@rest)))

(defrule sml-fn (and "fn" ws sml-first-branch (* sml-match-branch))
  (:destructure (f w1 first rest)
    (declare (ignore f w1))
    `(:fn (,first ,@rest))))

(defrule sml-assign-expr (and sml-orelse-expr (? (and ws ":=" ws sml-assign-expr)))
  (:destructure (left opt-right)
    (if opt-right
        `(:app (:app (:var ":=") ,left) ,(fourth opt-right))
        left)))

(defrule sml-seq-expr (and sml-assign-expr (* (and ws ";" ws sml-assign-expr)))
  (:destructure (first rest)
    (if (null rest)
        first
        `(:seq ,first ,@(mapcar #'fourth rest)))))

(defrule sml-expr (or sml-fn sml-case sml-if sml-seq-expr))

(defrule sml-val (and "val" ws sml-pat ws "=" ws sml-expr ws ";")
  (:destructure (v w1 pat w2 eq w3 expr w4 semi) (declare (ignore v w1 w2 eq w3 w4 semi))
    `(:val ,pat ,expr)))

(defrule sml-val-rec (and "val" ws "rec" ws sml-id ws "=" ws sml-expr ws ";")
  (:destructure (v w1 rec w2 name w3 eq w4 expr w5 semi)
    (declare (ignore v w1 rec w2 w3 eq w4 w5 semi))
    `(:val-rec ,name ,expr)))

(defrule sml-fun-clause (and sml-id (+ (and ws sml-pat)) ws "=" ws sml-expr)
  (:destructure (name params w1 eq w2 expr)
    (declare (ignore w1 eq w2))
    `(,name ,(mapcar #'second params) ,expr)))

(defrule sml-fun (and "fun" ws sml-fun-clause (* (and ws "|" ws sml-fun-clause)) ws ";")
  (:destructure (f w1 first rest w2 semi)
    (declare (ignore f w1 w2 semi))
    (let* ((name (first first))
           (clauses (cons first (mapcar #'fourth rest))))
      (unless (every (lambda (clause) (string= (first clause) name)) clauses)
        (error "All clauses in a fun binding must name the same function: ~S" clauses))
      `(:fun ,name ,(mapcar (lambda (clause)
                              `(,(second clause) ,(third clause)))
                            clauses)))))

;; Program rule simply uses the new reusable sml-decs block
(defrule sml-program sml-decs
  (:lambda (decs)
    `(:program ,@decs)))

;; 2. Add this helper function at the bottom of the file
(defun parse-sml-string (str)
  (esrap:parse 'sml-expr str))
