(in-package #:cl-sml)

;;; The SML97 lexer (Definition of Standard ML (Revised), chapter 2).
;;;
;;; LEX-SML turns source text into a vector of SML-TOKENs.  Each token
;;; records its start and end offsets into the text; the parser uses them to
;;; position AST nodes (static facts from HaMLet are keyed by these offsets)
;;; and to report syntax errors by line and column.
;;;
;;; Token kinds:
;;;   :reserved  a reserved word or symbol; VALUE is a keyword such as :VAL,
;;;              :LPAREN, :EQUALS (=), :DARROW (=>), :ARROW (->), :SEAL (:>)
;;;   :alnum     an alphanumeric identifier; VALUE is its name
;;;   :symbolic  a symbolic identifier such as + or ::
;;;   :long      a long identifier such as List.map or Word.<<; VALUE is the
;;;              whole dotted name
;;;   :tyvar     a type variable such as 'a or ''key
;;;   :int :word :real :string :char   special constants; VALUE is the
;;;              decoded value (an integer, a double-float, a string or a
;;;              character)
;;;   :eof       end of input

(define-condition sml-syntax-error (error)
  ((source-name :initarg :source-name :initform nil :reader sml-syntax-error-source-name)
   (line :initarg :line :reader sml-syntax-error-line)
   (column :initarg :column :reader sml-syntax-error-column)
   (message :initarg :message :reader sml-syntax-error-message))
  (:report (lambda (condition stream)
             (format stream "~@[~A:~]~D:~D: syntax error: ~A"
                     (sml-syntax-error-source-name condition)
                     (sml-syntax-error-line condition)
                     (sml-syntax-error-column condition)
                     (sml-syntax-error-message condition)))))

(defvar *sml-source-name* nil
  "The file name reported in syntax errors, if any.")

(defun sml-offset-line-column (text offset)
  "The 1-based line and 1-based column of OFFSET in TEXT."
  (let ((line 1)
        (line-start 0))
    (loop for i from 0 below (min offset (length text))
          when (char= (char text i) #\Newline)
            do (incf line)
               (setf line-start (1+ i)))
    (values line (1+ (- offset line-start)))))

(defun sml-syntax-error (text offset format-control &rest arguments)
  (multiple-value-bind (line column) (sml-offset-line-column text offset)
    (error 'sml-syntax-error
           :source-name *sml-source-name*
           :line line
           :column column
           :message (apply #'format nil format-control arguments))))

(defstruct (sml-token (:constructor make-sml-token (kind value start end)))
  (kind :eof :type keyword)
  value
  (start 0 :type fixnum)
  (end 0 :type fixnum))

(defparameter *sml-reserved-words*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (word '("abstype" "and" "andalso" "as" "case" "datatype" "do"
                    "else" "end" "exception" "fn" "fun" "handle" "if" "in"
                    "infix" "infixr" "let" "local" "nonfix" "of" "op" "open"
                    "orelse" "raise" "rec" "then" "type" "val" "with"
                    "withtype" "while"
                    ;; Modules
                    "eqtype" "functor" "include" "sharing" "sig" "signature"
                    "struct" "structure" "where")
             table)
      (setf (gethash word table) (intern (string-upcase word) :keyword))))
  "Reserved alphanumeric words, mapped to their token values.")

(defparameter *sml-reserved-symbols*
  '((":" . :colon) (":>" . :seal) ("|" . :bar) ("=" . :equals)
    ("=>" . :darrow) ("->" . :arrow) ("#" . :hash))
  "Symbolic sequences that are reserved rather than identifiers.")

(declaim (inline sml-symbolic-char-p sml-alnum-char-p sml-letter-p
                 sml-format-char-p))

(defun sml-symbolic-char-p (ch)
  (find ch "!%&$#+-/:<=>?@\\~`^|*"))

(defun sml-letter-p (ch)
  (or (char<= #\a ch #\z) (char<= #\A ch #\Z)))

(defun sml-alnum-char-p (ch)
  (or (sml-letter-p ch) (char<= #\0 ch #\9) (char= ch #\_) (char= ch #\')))

(defun sml-format-char-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun sml-whitespace-char-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return #\Page #.(code-char 11))))

(defun lex-sml (text &key (start 0) (end (length text)))
  "Split TEXT between START and END into a simple-vector of SML-TOKENs ending
with an :EOF token.  Signals SML-SYNTAX-ERROR on a lexical error."
  (let ((tokens (make-array 64 :adjustable t :fill-pointer 0))
        (i start))
    (declare (type fixnum i end))
    (labels ((peek (&optional (k 0))
               (let ((j (+ i k)))
                 (if (< j end) (char text j) nil)))
             (fail (offset control &rest args)
               (apply #'sml-syntax-error text offset control args))
             (emit (kind value token-start)
               (vector-push-extend (make-sml-token kind value token-start i) tokens))
             (skip-comment ()
               ;; I is at "(*"; comments nest.
               (let ((comment-start i)
                     (depth 0))
                 (loop
                   (cond
                     ((>= i end)
                      (fail comment-start "unterminated comment"))
                     ((and (char= (char text i) #\() (eql (peek 1) #\*))
                      (incf depth)
                      (incf i 2))
                     ((and (char= (char text i) #\*) (eql (peek 1) #\)))
                      (decf depth)
                      (incf i 2)
                      (when (zerop depth) (return)))
                     (t (incf i))))))
             (digits (pred)
               (loop while (and (< i end) (funcall pred (char text i)))
                     do (incf i)))
             (hex-digit-p (ch) (digit-char-p ch 16))
             (lex-number (token-start)
               ;; Optional ~, then an integer, word or real constant.
               (let ((negative (char= (char text i) #\~)))
                 (when negative (incf i))
                 (cond
                   ;; 0x hexadecimal integer
                   ((and (eql (peek) #\0) (eql (peek 1) #\x)
                         (peek 2) (hex-digit-p (peek 2)))
                    (incf i 2)
                    (let ((digits-start i))
                      (digits #'hex-digit-p)
                      (let ((n (parse-integer text :start digits-start :end i :radix 16)))
                        (emit :int (if negative (- n) n) token-start))))
                   ;; 0w word, 0wx hexadecimal word (words have no sign)
                   ((and (not negative) (eql (peek) #\0) (eql (peek 1) #\w)
                         (or (and (peek 2) (digit-char-p (peek 2)))
                             (and (member (peek 2) '(#\x #\X))
                                  (peek 3) (hex-digit-p (peek 3)))))
                    (if (digit-char-p (peek 2))
                        (progn
                          (incf i 2)
                          (let ((digits-start i))
                            (digits #'digit-char-p)
                            (emit :word (parse-integer text :start digits-start :end i)
                                  token-start)))
                        (progn
                          (incf i 3)
                          (let ((digits-start i))
                            (digits #'hex-digit-p)
                            (emit :word (parse-integer text :start digits-start :end i
                                                            :radix 16)
                                  token-start)))))
                   (t
                    (let ((int-start i)
                          (fraction-start nil)
                          (fraction-end nil)
                          (exponent nil))
                      (digits #'digit-char-p)
                      (let ((int-end i))
                        (when (and (eql (peek) #\.) (peek 1) (digit-char-p (peek 1)))
                          (incf i)
                          (setf fraction-start i)
                          (digits #'digit-char-p)
                          (setf fraction-end i))
                        (when (and (member (peek) '(#\e #\E))
                                   (or (and (peek 1) (digit-char-p (peek 1)))
                                       (and (eql (peek 1) #\~) (peek 2)
                                            (digit-char-p (peek 2)))))
                          (incf i)
                          (let ((exponent-negative (eql (peek) #\~)))
                            (when exponent-negative (incf i))
                            (let ((exponent-start i))
                              (digits #'digit-char-p)
                              (setf exponent
                                    (* (if exponent-negative -1 1)
                                       (parse-integer text :start exponent-start
                                                           :end i))))))
                        (if (or fraction-start exponent)
                            (emit :real
                                  (sml-real-constant-value
                                   text token-start negative
                                   (parse-integer text :start int-start :end int-end)
                                   (if fraction-start
                                       (subseq text fraction-start fraction-end)
                                       "")
                                   (or exponent 0))
                                  token-start)
                            (let ((n (parse-integer text :start int-start :end int-end)))
                              (emit :int (if negative (- n) n) token-start)))))))))
             (lex-escape (literal-start out)
               ;; I is just after a backslash inside a string or character
               ;; constant.  Writes the decoded character, if any, to OUT.
               (let ((ch (peek))
                     (escape-start (1- i)))
                 (when (null ch)
                   (fail literal-start "unterminated string constant"))
                 (flet ((simple (code)
                          (incf i)
                          (write-char (code-char code) out)))
                   (case ch
                     (#\a (simple 7))
                     (#\b (simple 8))
                     (#\t (simple 9))
                     (#\n (simple 10))
                     (#\v (simple 11))
                     (#\f (simple 12))
                     (#\r (simple 13))
                     (#\" (simple 34))
                     (#\\ (simple 92))
                     (#\^
                      (let ((control (peek 1)))
                        (unless (and control (char<= #\@ control #\_))
                          (fail escape-start "invalid control escape \\^~@[~C~]" control))
                        (incf i 2)
                        (write-char (code-char (- (char-code control) 64)) out)))
                     (#\u
                      (unless (and (<= (+ i 5) end)
                                   (every #'hex-digit-p (subseq text (1+ i) (+ i 5))))
                        (fail escape-start "\\u must be followed by four hexadecimal digits"))
                      (write-char (code-char (parse-integer text :start (1+ i) :end (+ i 5)
                                                                 :radix 16))
                                  out)
                      (incf i 5))
                     (t
                      (cond
                        ((digit-char-p ch)
                         (unless (and (<= (+ i 3) end)
                                      (every #'digit-char-p (subseq text i (+ i 3))))
                           (fail escape-start "\\ddd must have three decimal digits"))
                         (let ((code (parse-integer text :start i :end (+ i 3))))
                           (when (> code 255)
                             (fail escape-start "character code ~D is out of range" code))
                           (write-char (code-char code) out)
                           (incf i 3)))
                        ((sml-format-char-p ch)
                         ;; A gap \f...f\ is ignored.
                         (loop while (and (< i end) (sml-format-char-p (char text i)))
                               do (incf i))
                         (unless (eql (peek) #\\)
                           (fail escape-start "unterminated string gap"))
                         (incf i))
                        (t
                         (fail escape-start "invalid escape sequence \\~C" ch))))))))
             (lex-string-body (literal-start)
               ;; I is just after the opening quote.  Returns the decoded text.
               (with-output-to-string (out)
                 (loop
                   (let ((ch (peek)))
                     (cond
                       ((or (null ch) (char= ch #\Newline))
                        (fail literal-start "unterminated string constant"))
                       ((char= ch #\")
                        (incf i)
                        (return))
                       ((char= ch #\\)
                        (incf i)
                        (lex-escape literal-start out))
                       (t
                        ;; Printable characters, and bytes outside ASCII
                        ;; (SML97 strings are 8-bit) are taken literally.
                        (write-char ch out)
                        (incf i)))))))
             (lex-alnum-id ()
               (let ((id-start i))
                 (incf i)
                 (digits #'sml-alnum-char-p)
                 (subseq text id-start i)))
             (lex-symbolic-id ()
               (let ((id-start i))
                 (digits #'sml-symbolic-char-p)
                 (subseq text id-start i))))
      (loop
        ;; Whitespace and comments.
        (loop
          (cond
            ((>= i end) (return))
            ((sml-whitespace-char-p (char text i)) (incf i))
            ((and (char= (char text i) #\() (eql (peek 1) #\*)) (skip-comment))
            (t (return))))
        (when (>= i end)
          (emit :eof nil i)
          (return))
        (let ((ch (char text i))
              (token-start i))
          (cond
            ((char= ch #\() (incf i) (emit :reserved :lparen token-start))
            ((char= ch #\)) (incf i) (emit :reserved :rparen token-start))
            ((char= ch #\[) (incf i) (emit :reserved :lbracket token-start))
            ((char= ch #\]) (incf i) (emit :reserved :rbracket token-start))
            ((char= ch #\{) (incf i) (emit :reserved :lbrace token-start))
            ((char= ch #\}) (incf i) (emit :reserved :rbrace token-start))
            ((char= ch #\,) (incf i) (emit :reserved :comma token-start))
            ((char= ch #\;) (incf i) (emit :reserved :semicolon token-start))
            ((char= ch #\")
             (incf i)
             (emit :string (lex-string-body token-start) token-start))
            ((and (char= ch #\#) (eql (peek 1) #\"))
             (incf i 2)
             (let ((value (lex-string-body token-start)))
               (unless (= (length value) 1)
                 (fail token-start "a character constant must contain exactly one character"))
               (emit :char (char value 0) token-start)))
            ((char= ch #\')
             (incf i)
             ;; A lone ' (or '') is a type variable too (Definition section
             ;; 2.4: tyvar ::= 'alnum, where alnum may be empty).
             (digits #'sml-alnum-char-p)
             (emit :tyvar (subseq text token-start i) token-start))
            ((or (digit-char-p ch)
                 (and (char= ch #\~) (peek 1) (digit-char-p (peek 1))))
             (lex-number token-start))
            ((char= ch #\.)
             (if (and (eql (peek 1) #\.) (eql (peek 2) #\.))
                 (progn (incf i 3) (emit :reserved :dots token-start))
                 (fail token-start "unexpected character `.'")))
            ((char= ch #\_)
             (incf i)
             (emit :reserved :underscore token-start))
            ((sml-letter-p ch)
             (let* ((name (lex-alnum-id))
                    (reserved (gethash name *sml-reserved-words*)))
               (cond
                 (reserved
                  (emit :reserved reserved token-start))
                 ((and (eql (peek) #\.)
                       (peek 1)
                       (or (sml-letter-p (peek 1)) (sml-symbolic-char-p (peek 1))))
                  ;; A long identifier strid.strid...id; the last part may
                  ;; be symbolic.  No whitespace is allowed around the dots.
                  (loop
                    (unless (and (eql (peek) #\.)
                                 (peek 1)
                                 (or (sml-letter-p (peek 1))
                                     (sml-symbolic-char-p (peek 1))))
                      (return))
                    (incf i)
                    (let ((part-start i)
                          (part (if (sml-letter-p (peek))
                                    (lex-alnum-id)
                                    (lex-symbolic-id))))
                      (when (gethash part *sml-reserved-words*)
                        (fail part-start "reserved word `~A' in a long identifier" part))
                      (unless (sml-letter-p (char part 0))
                        (return))))
                  (emit :long (subseq text token-start i) token-start))
                 (t
                  (emit :alnum name token-start)))))
            ((sml-symbolic-char-p ch)
             (let* ((name (lex-symbolic-id))
                    (reserved (cdr (assoc name *sml-reserved-symbols* :test #'string=))))
               (if reserved
                   (emit :reserved reserved token-start)
                   (emit :symbolic name token-start))))
            (t
             (fail token-start "unexpected character `~A'" ch))))))
    (coerce tokens 'simple-vector)))

(defun sml-real-constant-value (text start negative integer fraction exponent)
  "The double nearest to the real constant INTEGER.FRACTION * 10^EXPONENT,
the same on every host (see PORTABLE-RATIONAL-TO-DOUBLE)."
  (let* ((mantissa (+ integer
                      (if (string= fraction "")
                          0
                          (/ (parse-integer fraction) (expt 10 (length fraction))))))
         (value (cond ((zerop mantissa) 0d0)
                      ((< exponent -10000) 0d0)
                      ((> exponent 10000) nil)
                      (t (portable-rational-to-double (* mantissa (expt 10 exponent)))))))
    (unless value
      (sml-syntax-error text start "real constant out of range"))
    (if negative (- value) value)))
