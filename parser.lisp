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
                :initial-value first))))

;; Whitespace
(defrule ws (* (or #\Space #\Tab #\Newline)) (:constant nil))

;; --- KEYWORD AND ID RULES ---
;; Define reserved keywords (added andalso, orelse, if, then, else)
(defrule sml-keyword
  (and (or "val" "fun" "case" "of" "if" "then" "else" "let" "in" "end" "andalso" "orelse")
       (! (or (alphanumericp character) #\_ #\'))))

;; A raw identifier is any standard word
(defrule sml-id-raw (and (alpha-char-p character) (* (or (alphanumericp character) #\_ #\')))
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

(defrule sml-pat-ctor-head sml-id
  (:lambda (name)
    (if (upper-case-p (char name 0))
        `(:pat-ctor ,name)
        (error "Expected a constructor (capitalized), got ~A" name))))

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
(defrule sml-op-cons "::" (:text t))

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


(defrule sml-decs (* (and ws (or sml-datatype sml-val sml-fun) ws))
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


;; Base Expressions
(defrule sml-primary (or sml-let sml-list sml-int sml-var-or-ctor sml-parens))

(defrule sml-parens (and "(" ws sml-expr ws ")")
  (:destructure (lp w1 expr w2 rp) (declare (ignore lp w1 w2 rp)) expr))

;; Application: f x y
(defrule sml-app (and sml-primary (* (and ws sml-primary)))
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

(defrule sml-cons-expr (and sml-add-expr (? (and ws sml-op-cons ws sml-cons-expr)))
  (:destructure (left opt-right)
    (if opt-right
        `(:app (:app (:var "::") ,left) ,(fourth opt-right))
        left)))

;; 3. Relational level (=, <, >)
(defrule sml-rel-expr (and sml-cons-expr (* (and ws sml-op-rel ws sml-add-expr)))
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


(defrule sml-pat-primary
    (or sml-int
        sml-pat-var-or-ctor
        (and "_" (:constant :wild))
        sml-pat-parens))

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

;; === TOP LEVEL EXPRESSION RULE ===
;; This MUST point to sml-orelse-expr to catch the entire logic chain!
(defrule sml-expr (or sml-fn sml-case sml-if sml-orelse-expr))

(defrule sml-val (and "val" ws sml-id ws "=" ws sml-expr ws ";")
  (:destructure (v w1 name w2 eq w3 expr w4 semi) (declare (ignore v w1 w2 eq w3 w4 semi))
    `(:val ,name ,expr)))

(defrule sml-fun (and "fun" ws sml-id (+ (and ws sml-id)) ws "=" ws sml-expr ws ";")
  (:destructure (f w1 name params w2 eq w3 expr w4 semi) (declare (ignore f w1 w2 eq w3 w4 semi))
    `(:fun ,name ,(mapcar #'second params) ,expr)))

;; Program rule simply uses the new reusable sml-decs block
(defrule sml-program sml-decs
  (:lambda (decs)
    `(:program ,@decs)))

;; 2. Add this helper function at the bottom of the file
(defun parse-sml-string (str)
  (esrap:parse 'sml-expr str))
