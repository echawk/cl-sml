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
      (char string-value 0)))
  (defun trim-and-collapse-sml-type-text (text)
    (let ((trimmed (trim-sml-type-text text)))
      (with-output-to-string (out)
        (loop with previous-space = nil
              for ch across trimmed
              for spacep = (member ch '(#\Space #\Tab #\Newline #\Return))
              do (cond
                   ((and spacep (not previous-space))
                    (write-char #\Space out)
                    (setf previous-space t))
                   ((not spacep)
                    (write-char ch out)
                    (setf previous-space nil)))))))
  (defun trim-sml-type-text (text)
    (string-trim '(#\Space #\Tab #\Newline #\Return) text))
  (defun join-sml-id-parts (first rest)
    (with-output-to-string (out)
      (write-string first out)
      (dolist (part rest)
        (write-char #\. out)
        (write-string (second part) out))))
  (defun sml-id-last-segment (name)
    (let ((pos (position #\. name :from-end t)))
      (if pos
          (subseq name (1+ pos))
          name)))
  (defun sml-constructor-looking-id-p (name)
    (let ((segment (sml-id-last-segment name)))
      (or (member segment '("true" "false" "nil") :test #'string=)
          (and (upper-case-p (char segment 0))
               (or (not (position #\_ segment))
                   (string= segment (string-upcase segment)))))))
  (defun build-fun-binding-ast (first rest)
    (let* ((name (first first))
           (clauses (cons first (mapcar #'fourth rest))))
      (unless (every (lambda (clause) (string= (first clause) name)) clauses)
        (error "All clauses in a fun binding must name the same function: ~S" clauses))
      `(:fun ,name ,(mapcar (lambda (clause)
                              `(,(second clause) ,(third clause)))
                            clauses))))
  (defun build-list-pattern-ast (elements)
    (reduce (lambda (element tail)
              `(:pat-cons ,element ,tail))
            elements
            :from-end t
            :initial-value '(:pat-nil))))

(defrule sml-comment-char
  (and (! "(*") (! "*)") character))

(defrule sml-comment-body (* (or sml-comment sml-comment-char)))

(defrule sml-comment (and "(*" sml-comment-body "*)")
  (:constant nil))

;; Whitespace
(defrule ws (* (or #\Space #\Tab #\Newline sml-comment)) (:constant nil))
(defrule ws1 (+ (or #\Space #\Tab #\Newline sml-comment)) (:constant nil))

;; --- KEYWORD AND ID RULES ---
;; Define reserved keywords (added andalso, orelse, if, then, else)
(defrule sml-keyword
  (and (or "signature" "structure" "exception" "datatype" "withtype" "andalso"
           "functor" "abstype" "include" "sharing" "infixr" "orelse" "handle"
           "nonfix" "struct" "infix" "where" "local" "raise" "eqtype" "while"
           "case" "then" "else" "type" "with" "open" "fun" "val" "rec" "and"
           "sig" "let" "end" "do" "fn" "if" "in" "of" "op" "as")
       (! (or (alphanumericp character) #\_ #\'))))

(defrule sml-sig-keyword (and "sig" (! (or (alphanumericp character) #\_ #\')))
  (:constant "sig"))

(defrule sml-struct-keyword (and "struct" (! (or (alphanumericp character) #\_ #\')))
  (:constant "struct"))

(defrule sml-end-keyword (and "end" (! (or (alphanumericp character) #\_ #\')))
  (:constant "end"))

;; A raw identifier is any standard word
(defrule sml-id-raw (and (alpha-char-p character) (* (or (alphanumericp character) #\_ #\')))
  (:text t))

(defrule sml-capitalized-id (and (character-ranges (#\A #\Z)) (* (or (alphanumericp character) #\_ #\')))
  (:text t))

(defrule sml-symbolic-char
  (or #\! #\% #\& #\$ #\# #\+ #\- #\/ #\: #\< #\= #\> #\? #\@ #\\ #\~ #\` #\^ #\| #\*))

(defrule sml-symbolic-id
  (+ sml-symbolic-char)
  (:text t))

(defrule sml-op-spaced-id (and "op" ws1 (or sml-symbolic-id sml-id))
  (:destructure (op-kw w id)
    (declare (ignore op-kw w))
    id))

(defrule sml-op-symbolic-id (and "op" sml-symbolic-id)
  (:destructure (op-kw id)
    (declare (ignore op-kw))
    id))

(defrule sml-op-id (or sml-op-spaced-id sml-op-symbolic-id))

(defrule sml-numeric-label (+ (character-ranges (#\0 #\9)))
  (:text t))

(defrule sml-label
  (or sml-id-raw
      sml-numeric-label))

(defrule sml-tyvar (and "'" (+ (or (alphanumericp character) #\_ #\')))
  (:text t))

;; A valid SML identifier is a raw ID or long ID whose head is NOT a keyword.
(defrule sml-id (and (! sml-keyword) sml-id-raw (* (and "." (or sml-id-raw sml-symbolic-id))))
  (:destructure (not-kw id rest)
    (declare (ignore not-kw))
    (join-sml-id-parts id rest)))

(defrule sml-int (and (? "~") (+ (character-ranges (#\0 #\9))))
  (:destructure (neg digits)
    (let ((n (parse-integer (text digits))))
      (if neg (- n) n))))

(defrule sml-hex-digit
  (or (character-ranges (#\0 #\9))
      (character-ranges (#\a #\f))
      (character-ranges (#\A #\F))))

(defrule sml-word-hex (and "0w" (or "x" "X") (+ sml-hex-digit))
  (:destructure (prefix marker digits)
    (declare (ignore prefix marker))
    (parse-integer (text digits) :radix 16)))

(defrule sml-word-dec (and "0w" (+ (character-ranges (#\0 #\9))))
  (:destructure (prefix digits)
    (declare (ignore prefix))
    (parse-integer (text digits))))

(defrule sml-word (or sml-word-hex sml-word-dec))

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

(defrule sml-long-capitalized-tail
  (or (and sml-id-raw "." sml-long-capitalized-tail)
      sml-capitalized-id)
  (:lambda (value)
    (if (stringp value)
        value
        (format nil "~A.~A" (first value) (third value)))))

(defrule sml-long-capitalized-id
  (and (! sml-keyword) sml-id-raw "." sml-long-capitalized-tail)
  (:destructure (not-kw first dot rest)
    (declare (ignore not-kw dot))
    (format nil "~A.~A" first rest)))

(defrule sml-pat-ctor-head (or sml-long-capitalized-id sml-capitalized-id)
  (:lambda (name)
    `(:pat-ctor ,name)))

(defrule sml-pat-app-head
  (or sml-pat-ctor-head
      (and "ref" (! (or (alphanumericp character) #\_ #\'))))
  (:lambda (name)
    (if (and (consp name) (eq (car name) :pat-ctor))
        name
        '(:pat-ctor "ref"))))

;; Notice we check upper/lower case to distinguish variables from constructors!
(defrule sml-pat-var-or-ctor sml-id
  (:lambda (name)
    (if (sml-constructor-looking-id-p name)
        `(:pat-ctor ,name)
        `(:pat-var ,name))))

(defrule sml-op-var sml-op-id
  (:lambda (name)
    `(:var ,name)))

(defrule sml-bare-bar-symbol (and "|" (! sml-symbolic-char)))

(defrule sml-bare-colon-symbol (and ":" (! sml-symbolic-char)))

(defrule sml-bare-equals-symbol (and "=" (! sml-symbolic-char)))

(defrule sml-bare-tilde-symbol (and "~" (! sml-symbolic-char)))

(defrule sml-selector-start
  (and "#" (or (alpha-char-p character) (character-ranges (#\0 #\9)))))

(defrule sml-char-start "#\"")

(defrule sml-symbolic-var
  (and (! ":=") (! "=>") (! "->") (! sml-selector-start) (! sml-char-start)
       (! sml-bare-equals-symbol) (! sml-bare-bar-symbol) (! sml-bare-colon-symbol)
       (! sml-bare-tilde-symbol) sml-symbolic-id)
  (:destructure (not-assign not-match-arrow not-type-arrow not-selector not-char not-equals
                 not-bar not-colon not-tilde name)
    (declare (ignore not-assign not-match-arrow not-type-arrow not-selector not-char
                     not-equals not-bar not-colon not-tilde))
    `(:var ,name)))

(defrule sml-pat-op-var sml-op-id
  (:lambda (name)
    `(:pat-var ,name)))

(defrule sml-pat-symbolic-var (and (! "=") (! "|") sml-symbolic-id)
  (:destructure (not-equals not-bar name)
    (declare (ignore not-equals not-bar))
    `(:pat-var ,name)))

(defrule sml-var-or-ctor sml-id
  (:lambda (name)
    (if (sml-constructor-looking-id-p name)
        `(:ctor ,name)
        `(:var ,name))))

;; Operators
(defrule sml-op-mult (or "*" "div" "mod" "/") (:text t))
(defrule sml-op-add  (or "+" "-" "^") (:text t))
(defrule sml-op-rel  (or "<=" ">=" "<>" "<" ">" "=") (:text t))
(defrule sml-op-list (or "::" "@") (:text t))

;; --- NEW DATATYPE RULES ---
(defrule sml-type-brace-block
  (and "{" (* (or sml-type-brace-block
              sml-type-paren-block
              (and (! (or "{" "}")) character)))
       "}")
  (:text t)
  (:lambda (text)
    (trim-and-collapse-sml-type-text text)))

(defrule sml-type-blob (and ws "of" ws (or sml-type-brace-block
                                           (+ (and (! (or "|" ";" #\Newline)) character))))
  (:destructure (w1 of w2 chars)
    (declare (ignore w1 of w2))
    (if (stringp chars)
        chars
        (trim-and-collapse-sml-type-text (text chars)))))

(defrule sml-type-text-id (and (! sml-end-keyword) sml-id-raw))

(defrule sml-type-text-to-eol
  (+ (or sml-type-text-id
         (and (! (or ";" #\Newline sml-end-keyword)) character)))
  (:text t)
  (:lambda (text)
    (trim-and-collapse-sml-type-text text)))

(defrule sml-type-paren-block
  (and "(" (* (or sml-type-paren-block
              (and (! (or "(" ")")) character)))
       ")"))

(defrule sml-type-text-inline
  (+ (or sml-type-paren-block
         (and (! (or ";" #\Newline "," ")" sml-end-keyword)) character)))
  (:text t)
  (:lambda (text)
    (trim-and-collapse-sml-type-text text)))

(defrule sml-type-text-before-equals (+ (and (! "=") character))
  (:text t)
  (:lambda (text)
    (trim-and-collapse-sml-type-text text)))

(defrule sml-type-constraint (and ws ":" ws sml-type-text-before-equals)
  (:destructure (w1 colon w2 type)
    (declare (ignore w1 colon w2))
    type))

(defrule sml-expression-type-constraint (and ws ":" ws sml-type-text-to-eol)
  (:destructure (w1 colon w2 type)
    (declare (ignore w1 colon w2))
    type))

(defrule sml-expression-type-ascription (and ws ":" ws sml-type-text-inline)
  (:destructure (w1 colon w2 type)
    (declare (ignore w1 colon w2))
    type))

(defrule sml-pattern-type-text-inline
  (+ (or sml-type-paren-block
         (and (! (or ";" #\Newline "," ")" "]" "}" "::" "=>" "=" sml-end-keyword))
              character)))
  (:text t)
  (:lambda (text)
    (trim-and-collapse-sml-type-text text)))

(defrule sml-pattern-type-ascription (and ws ":" (! ":") ws sml-pattern-type-text-inline)
  (:destructure (w1 colon not-colon w2 type)
    (declare (ignore w1 colon not-colon w2))
    type))

(defrule sml-type-params
  (or sml-tyvar
      (and "(" ws sml-tyvar (* (and ws "," ws sml-tyvar)) ws ")")))

(defrule sml-tycon-name (and (? (and sml-type-params ws)) sml-id)
  (:destructure (params name)
    (declare (ignore params))
    name))

(defrule sml-ctor-def (and ws sml-id (? sml-type-blob))
  (:destructure (w1 name has-args) (declare (ignore w1))
    (if has-args
        `(:ctor-def ,name :has-args t :arg-type ,has-args)
        `(:ctor-def ,name :has-args nil :arg-type nil))))

(defrule sml-ctor-defs (and sml-ctor-def (* (and ws "|" sml-ctor-def)))
  (:destructure (first rest)
    (cons first (mapcar #'third rest))))

(defrule sml-datatype-replication
  (and "datatype" ws sml-tycon-name ws "=" ws "datatype" ws sml-type-text-to-eol ws (? ";"))
  (:destructure (dt w1 name w2 eq w3 dt2 w4 source w5 semi)
    (declare (ignore dt w1 w2 eq w3 dt2 w4 w5 semi))
    `(:datatype-replication ,name ,source)))

(defrule sml-datatype (and "datatype" ws sml-tycon-name ws "=" sml-ctor-defs ws (? ";"))
  (:destructure (dt w1 name w2 eq defs w3 semi) (declare (ignore dt w1 w2 eq w3 semi))
    `(:datatype ,name ,defs)))

(defrule sml-exception-alias (and "exception" ws sml-id ws "=" ws sml-type-text-to-eol ws (? ";"))
  (:destructure (exn w1 name w2 eq w3 target w4 semi)
    (declare (ignore exn w1 w2 eq w3 w4 semi))
    `(:exception-alias ,name ,target)))

(defrule sml-exception (and "exception" ws sml-id (? sml-type-blob) ws (? ";"))
  (:destructure (exn w1 name arg-type w2 semi)
    (declare (ignore exn w1 w2 semi))
    `(:exception ,name :arg-type ,arg-type)))

(defrule sml-type-decl-rhs (or sml-type-brace-block sml-type-text-to-eol))

(defrule sml-type-decl (and "type" ws sml-tycon-name ws "=" ws sml-type-decl-rhs ws (? ";"))
  (:destructure (type-kw w1 name w2 eq w3 target w4 semi)
    (declare (ignore type-kw w1 w2 eq w3 w4 semi))
    `(:type ,name ,target)))

(defrule sml-infix-decl
  (and (or "infixr" "infix" "nonfix") (? (and ws (+ (character-ranges (#\0 #\9))))) ws
       sml-type-text-to-eol ws (? ";"))
  (:destructure (kind precedence w1 names w2 semi)
    (declare (ignore w1 w2 semi))
    `(:infix ,kind ,(and precedence (parse-integer (text (second precedence)))) ,names)))

(defrule sml-module-ignore-id
  (and (! (or sml-sig-keyword sml-struct-keyword sml-end-keyword)) sml-id-raw))

(defrule sml-module-ignore-char
  (and (! (or sml-sig-keyword sml-struct-keyword sml-end-keyword)) character))

(defrule sml-module-ignore-body
  (* (or sml-sig-block sml-ignored-struct-block sml-module-ignore-id sml-module-ignore-char)))

(defrule sml-sig-block (and sml-sig-keyword sml-module-ignore-body sml-end-keyword)
  (:constant nil))

(defrule sml-ignored-struct-block (and sml-struct-keyword sml-module-ignore-body sml-end-keyword)
  (:constant nil))

(defrule sml-signature (and "signature" ws sml-id ws "=" ws sml-sig-block ws (? ";"))
  (:destructure (sig-kw w1 name w2 eq w3 body w4 semi)
    (declare (ignore sig-kw w1 w2 eq w3 body w4 semi))
    `(:signature ,name)))

(defrule sml-open (and "open" ws sml-type-text-to-eol ws (? ";"))
  (:destructure (open-kw w1 names w2 semi)
    (declare (ignore open-kw w1 w2 semi))
    `(:open ,names)))

(defrule sml-structure-equals-struct (and "=" ws sml-struct-keyword))

(defrule sml-module-paren-block
  (and "(" (* (or sml-module-paren-block
              sml-sig-block
              sml-ignored-struct-block
              (and (! (or "(" ")")) character)))
       ")")
  (:constant nil))

(defrule sml-module-post-ascription
  (and ws ":" (? ">") ws (or sml-sig-block sml-type-text-to-eol))
  (:constant nil))

(defrule sml-structure-ascription-char (and (! sml-structure-equals-struct) character))

(defrule sml-structure-equals-functor-app
  (and "=" ws sml-id ws sml-module-paren-block))

(defrule sml-structure-functor-app-char
  (and (! "=") character))

(defrule sml-structure-functor-app
  (and "structure" ws sml-id ws (* sml-structure-functor-app-char)
       "=" ws sml-id ws sml-module-paren-block (? sml-module-post-ascription) ws (? ";"))
  (:destructure (structure-kw w1 name w2 ascription eq w3 functor-name w4 args post w5 semi)
    (declare (ignore structure-kw w1 w2 ascription eq w3 w4 args post w5 semi))
    `(:structure-app ,name ,functor-name)))

(defrule sml-structure-alias
  (and "structure" ws sml-id ws (* (and (! (or "where" "=")) character)) "=" ws sml-id ws (? ";"))
  (:destructure (structure-kw w1 name w2 ascription eq w3 target w4 semi)
    (declare (ignore structure-kw w1 w2 ascription eq w3 w4 semi))
    `(:structure-alias ,name ,target)))

(defrule sml-structure
  (and "structure" ws sml-id ws (* sml-structure-ascription-char)
       "=" ws sml-struct-keyword ws sml-decs ws sml-end-keyword ws (? ";"))
  (:destructure (structure-kw w1 name w2 ascription eq w3 struct-kw w4 decs w5 end-kw w6 semi)
    (declare (ignore structure-kw w1 w2 ascription eq w3 struct-kw w4 w5 end-kw w6 semi))
    `(:structure ,name ,decs)))

(defrule sml-functor
  (and "functor" ws sml-id ws (* sml-structure-ascription-char)
       "=" ws sml-struct-keyword ws sml-decs ws sml-end-keyword ws (? ";"))
  (:destructure (functor-kw w1 name w2 ascription eq w3 struct-kw w4 decs w5 end-kw w6 semi)
    (declare (ignore functor-kw w1 w2 ascription eq w3 struct-kw w4 w5 end-kw w6 semi))
    `(:functor ,name ,decs)))

(defrule sml-decs (* (and ws (or sml-signature sml-functor sml-structure-functor-app
                                 sml-structure-alias sml-structure sml-open
                                 sml-local sml-infix-decl sml-type-decl
                                 sml-datatype-replication sml-datatype
                                 sml-exception-alias sml-exception sml-val-rec
                                 sml-val sml-fun sml-top-expr) ws))
  (:destructure (&rest items)
    (mapcar #'second items)))

(defrule sml-local (and "local" ws sml-decs ws "in" ws sml-decs ws "end" ws (? ";"))
  (:destructure (local-kw w1 local-decs w2 in-kw w3 body-decs w4 end-kw w5 opt-semi)
    (declare (ignore local-kw w1 w2 in-kw w3 w4 end-kw w5 opt-semi))
    `(:local ,local-decs ,body-decs)))

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

(defrule sml-record-field (and sml-label ws "=" ws sml-expr)
  (:destructure (label w1 eq w2 expr)
    (declare (ignore w1 eq w2))
    (list label expr)))

(defrule sml-record-fields (and sml-record-field (* (and ws "," ws sml-record-field)))
  (:destructure (first rest)
    (cons first (mapcar #'fourth rest))))

(defrule sml-record (and "{" ws (? sml-record-fields) ws "}")
  (:destructure (lb w1 fields w2 rb)
    (declare (ignore lb w1 w2 rb))
    `(:record ,@fields)))

(defrule sml-paren-elements (and sml-expr (* (and ws "," ws sml-expr)))
  (:destructure (first rest)
    (cons first (mapcar #'fourth rest))))

;; Base Expressions
(defrule sml-selector (and "#" sml-label)
  (:destructure (hash label)
    (declare (ignore hash))
    `(:selector ,label)))

(defrule sml-atomic (or sml-let sml-record sml-list sml-selector sml-char sml-string sml-word sml-real sml-int
                        sml-op-var sml-symbolic-var sml-var-or-ctor sml-parens))

(defrule sml-deref (and "!" ws sml-prefix)
  (:destructure (bang w expr) (declare (ignore bang w))
    `(:deref ,expr)))

(defrule sml-negate (and sml-bare-tilde-symbol ws sml-prefix)
  (:destructure (tilde w expr) (declare (ignore tilde w))
    `(:app (:var "~") ,expr)))

(defrule sml-prefix (or sml-deref sml-negate sml-atomic))

(defrule sml-parens (and "(" ws (? sml-paren-elements) ws ")")
  (:destructure (lp w1 elems w2 rp) (declare (ignore lp w1 w2 rp))
    (cond
      ((null elems) '(:unit))
      ((null (rest elems)) (first elems))
      (t `(:tuple ,@elems)))))

;; Application: f x y
(defrule sml-app-arg (and ws (! sml-generic-word-infix-op) (! sml-generic-symbolic-infix-op) sml-prefix))

(defrule sml-app (and sml-prefix (* sml-app-arg))
  (:destructure (first rest)
    (if (null rest)
        first
        (reduce (lambda (left group)
                  `(:app ,left ,(fourth group)))
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
(defrule sml-generic-symbolic-infix-op
  (and (! ":=") (! "->") (! "=>") (! sml-selector-start) (! sml-char-start)
       (! sml-bare-bar-symbol) (! sml-bare-colon-symbol) (! sml-bare-equals-symbol)
       (! sml-bare-tilde-symbol) sml-symbolic-id)
  (:destructure (not-assign not-type-arrow not-match-arrow not-selector not-char not-bar
                 not-colon not-equals not-tilde op)
    (declare (ignore not-assign not-type-arrow not-match-arrow not-selector not-char
                     not-bar not-colon not-equals not-tilde))
    op))

(defrule sml-generic-word-infix-op
  (or (and "before" (! (or (alphanumericp character) #\_ #\')))
      (and "o" (! (or (alphanumericp character) #\_ #\'))))
  (:text t))

(defrule sml-generic-infix-op
  (or sml-generic-symbolic-infix-op sml-generic-word-infix-op))

(defrule sml-generic-infix-expr (and sml-rel-expr (* (and ws sml-generic-infix-op ws sml-rel-expr)))
  (:destructure (first rest) (build-infix-ast first rest)))

(defrule sml-andalso-expr (and sml-generic-infix-expr (* (and ws "andalso" ws sml-generic-infix-expr)))
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

(defrule sml-pat-list-elements (and sml-pat (* (and ws "," ws sml-pat)))
  (:destructure (first rest)
    (cons first (mapcar #'fourth rest))))

(defrule sml-pat-list (and "[" ws (? sml-pat-list-elements) ws "]")
  (:destructure (lb w1 elems w2 rb)
    (declare (ignore lb w1 w2 rb))
    (build-list-pattern-ast (or elems nil))))

(defrule sml-pat-app (and sml-pat-app-head ws sml-pat-app-primary)
  (:destructure (ctor w pat) (declare (ignore w))
    (if (eq (car ctor) :pat-ctor)
        `(:pat-app ,ctor ,pat)
        ;; If it wasn't capitalized, it's not a valid pattern app in SML
        (error "Pattern application head must be a Constructor (Capitalized)"))))

(defrule sml-pat-record-app (and sml-pat-app-head ws sml-pat-record)
  (:destructure (ctor w pat) (declare (ignore w))
    `(:pat-app ,ctor ,pat)))

(defrule sml-pat-parens (and "(" ws sml-pat ws ")")
  (:destructure (lp w1 pat w2 rp) (declare (ignore lp w1 w2 rp)) pat))

(defrule sml-pat-paren-elements (and sml-pat (* (and ws "," ws sml-pat)))
  (:destructure (first rest)
    (cons first (mapcar #'fourth rest))))

(defrule sml-pat-record-rest "..."
  (:constant :record-rest))

(defrule sml-pat-record-field
  (or sml-pat-record-rest
      (and sml-label ws "=" ws sml-pat)
      sml-label)
  (:lambda (field)
    (cond
      ((eq field :record-rest) field)
      ((stringp field)
       (list field `(:pat-var ,field)))
      (t
       (list (first field) (fifth field))))))

(defrule sml-pat-record-fields (and sml-pat-record-field (* (and ws "," ws sml-pat-record-field)))
  (:destructure (first rest)
    (remove :record-rest (cons first (mapcar #'fourth rest)))))

(defrule sml-pat-record (and "{" ws (? sml-pat-record-fields) ws "}")
  (:destructure (lb w1 fields w2 rb)
    (declare (ignore lb w1 w2 rb))
    `(:pat-record ,@fields)))

(defrule sml-pat-tuple-or-parens (and "(" ws (? sml-pat-paren-elements) ws ")")
  (:destructure (lp w1 elems w2 rp) (declare (ignore lp w1 w2 rp))
    (cond
      ((null elems) '(:pat-unit))
      ((null (rest elems)) (first elems))
      (t `(:pat-tuple ,@elems)))))

(defrule sml-wildcard "_"
  (:lambda (token)
    (declare (ignore token))
    :wild))

(defrule sml-pat-primary
    (or sml-word
        sml-real
        sml-int
        sml-string
        sml-char
        sml-pat-record
        sml-pat-op-var
        sml-pat-symbolic-var
        sml-pat-var-or-ctor
        sml-wildcard
        sml-pat-tuple-or-parens))

(defrule sml-pat-app-primary
    (or sml-word
        sml-real
        sml-int
        sml-string
        sml-char
        sml-pat-record
        sml-pat-var-or-ctor
        sml-wildcard
        sml-pat-tuple-or-parens))

(defrule sml-pat-non-cons
  (or sml-pat-list
      sml-pat-record-app
      sml-pat-app
      sml-pat-primary))

(defrule sml-pat-ascribed (and sml-pat-non-cons (? sml-pattern-type-ascription))
  (:destructure (pat type)
    (if type
        `(:pat-typed ,pat ,type)
        pat)))

(defrule sml-pat-as (and sml-pat-ascribed ws "as" ws sml-pat)
  (:destructure (alias w1 as-kw w2 pat)
    (declare (ignore w1 as-kw w2))
    `(:pat-as ,alias ,pat)))

;; Parse the right-associative cons pattern: h :: t.
(defrule sml-pat-cons (and (or sml-pat-as sml-pat-ascribed) ws "::" ws sml-pat)
  (:destructure (h w1 op w2 t-pat) (declare (ignore w1 op w2))
    `(:pat-cons ,h ,t-pat)))

;; Patterns for case statements
;; Order is critical! Complex patterns (cons, app) must come before simple vars

(defrule sml-pat (or sml-pat-cons
                     sml-pat-as
                     sml-pat-ascribed))

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

(defrule sml-handle-branch (and sml-pat ws "=>" ws sml-expr)
  (:destructure (pat w1 arr w2 expr)
    (declare (ignore w1 arr w2))
    `(,pat ,expr)))

(defrule sml-handle-match-branch (and ws "|" ws sml-pat ws "=>" ws sml-expr)
  (:destructure (w1 bar w2 pat w3 arr w4 expr)
    (declare (ignore w1 bar w2 w3 arr w4))
    `(,pat ,expr)))

(defrule sml-base-expr
  (or sml-fn
      sml-case
      sml-if
      sml-assign-expr))

(defrule sml-handle-expr (and sml-base-expr (? (and ws "handle" ws sml-handle-branch (* sml-handle-match-branch))))
  (:destructure (expr opt-handle)
    (if opt-handle
        `(:handle ,expr (,(fourth opt-handle) ,@(fifth opt-handle)))
        expr)))

(defrule sml-raise-expr
  (or (and "raise" ws sml-raise-expr)
      sml-handle-expr)
  (:lambda (result)
    (if (and (consp result)
             (= (length result) 3)
             (stringp (first result))
             (string= (first result) "raise"))
        `(:raise ,(third result))
        result)))

(defrule sml-ascribed-expr (and sml-raise-expr (? sml-expression-type-ascription))
  (:destructure (expr type)
    (if type
        `(:typed ,expr ,type)
        expr)))

(defrule sml-seq-expr (and sml-ascribed-expr (* (and ws ";" ws sml-ascribed-expr)))
  (:destructure (first rest)
    (if (null rest)
        first
        `(:seq ,first ,@(mapcar #'fourth rest)))))

(defrule sml-expr sml-seq-expr)

(defrule sml-val (and "val" ws sml-pat (? sml-type-constraint) ws "=" ws sml-expr
                      (? sml-expression-type-constraint) ws (? ";"))
  (:destructure (v w1 pat type w2 eq w3 expr expr-type w4 semi)
    (declare (ignore v w1 w2 eq w3 w4 semi))
    (let* ((inline-type (and (consp expr) (eq (car expr) :typed) (third expr)))
           (pat-type (and (consp pat) (eq (car pat) :pat-typed) (third pat)))
           (value-pat (if pat-type (second pat) pat))
           (value-expr (if inline-type (second expr) expr)))
      (cond
        (type `(:val ,value-pat ,value-expr :type ,type))
        (pat-type `(:val ,value-pat ,value-expr :type ,pat-type))
        (expr-type `(:val ,value-pat ,value-expr :type ,expr-type))
        (inline-type `(:val ,value-pat ,value-expr :type ,inline-type))
        (t `(:val ,pat ,expr))))))

(defrule sml-val-rec (and "val" ws "rec" ws sml-id ws "=" ws sml-expr ws (? ";"))
  (:destructure (v w1 rec w2 name w3 eq w4 expr w5 semi)
    (declare (ignore v w1 rec w2 w3 eq w4 w5 semi))
    `(:val-rec ,name ,expr)))

(defrule sml-fun-name (or sml-op-id sml-symbolic-id sml-id))

(defrule sml-infix-fun-name (or sml-op-id sml-symbolic-id "o" "before")
  (:text t))

(defrule sml-fun-prefix-clause (and sml-fun-name (+ (and ws sml-pat)) ws "=" ws sml-expr)
  (:destructure (name params w1 eq w2 expr)
    (declare (ignore w1 eq w2))
    `(,name ,(mapcar #'second params) ,expr)))

(defrule sml-fun-paren-infix-clause
  (and "(" ws sml-pat ws1 sml-infix-fun-name ws1 sml-pat ws ")" (* (and ws sml-pat)) ws "=" ws sml-expr)
  (:destructure (lp w1 left w2 name w3 right w4 rp rest w5 eq w6 expr)
    (declare (ignore lp w1 w2 w3 w4 rp w5 eq w6))
    `(,name (,left ,right ,@(mapcar #'second rest)) ,expr)))

(defrule sml-fun-bare-infix-clause
  (and sml-pat ws1 sml-infix-fun-name ws1 sml-pat (* (and ws sml-pat)) ws "=" ws sml-expr)
  (:destructure (left w1 name w2 right rest w3 eq w4 expr)
    (declare (ignore w1 w2 w3 eq w4))
    `(,name (,left ,right ,@(mapcar #'second rest)) ,expr)))

(defrule sml-fun-clause (or sml-fun-paren-infix-clause
                            sml-fun-bare-infix-clause
                            sml-fun-prefix-clause))

(defrule sml-fun-binding (and sml-fun-clause (* (and ws "|" ws sml-fun-clause)))
  (:destructure (first rest)
    (build-fun-binding-ast first rest)))

(defrule sml-fun (and "fun" ws sml-fun-binding (* (and ws "and" ws sml-fun-binding)) ws (? ";"))
  (:destructure (f w1 first rest w2 semi)
    (declare (ignore f w1 w2 semi))
    (if rest
        `(:funs ,first ,@(mapcar #'fourth rest))
        first)))

(defrule sml-top-expr (and sml-expr ws ";")
  (:destructure (expr w semi)
    (declare (ignore w semi))
    `(:expr ,expr)))

;; Program rule simply uses the new reusable sml-decs block
(defrule sml-program (and ws sml-decs ws)
  (:destructure (w1 decs w2)
    (declare (ignore w1 w2))
    `(:program ,@decs)))

;; 2. Add this helper function at the bottom of the file
(defun parse-sml-string (str)
  (esrap:parse 'sml-expr str))
