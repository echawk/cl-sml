(in-package #:cl-sml)

(defun repl-it-symbol ()
  (sml-symbol "it"))

(defun trim-repl-input (source)
  (string-trim '(#\Space #\Tab #\Newline #\Return) source))

(defun parse-repl-input (source)
  "Decide whether the REPL buffer SOURCE is complete: returns :EXPR or
:PROGRAM and the AST, or NIL, NIL and the syntax error."
  (let* ((trimmed (trim-repl-input source))
         (length (length trimmed)))
    (cond
      ((zerop length)
       (values nil nil nil))
      ((char= (char trimmed (1- length)) #\;)
       (handler-case
           ;; Parsed without committing the environment it declares: the
           ;; phrase is compiled (and the environment committed) later.
           (let ((program (parse-sml-program trimmed :source-name "stdIn")))
             (if (and (= (length program) 2)
                      (consp (second program))
                      (eq (car (second program)) :expr))
                 (values :expr (second (second program)) nil)
                 (values :program program nil)))
         (sml-syntax-error (program-error)
           (values nil nil program-error))))
      (t
       (handler-case
           (values :expr (parse-sml-expression trimmed :source-name "stdIn") nil)
         (sml-syntax-error (condition)
           (values nil nil condition)))))))

(defun sml-namespaced-symbol-p (value)
  (and (symbolp value)
       (let ((package (symbol-package value)))
         (and package
              (sml-package-name-p (package-name package))))))

(defun constructor-value-p (value)
  (and (consp value)
       (sml-namespaced-symbol-p (car value))))

(defun sml-type-known-p (type)
  (cond
    ((null type) nil)
    ((eq type :unknown) nil)
    ((stringp type) t)
    ((and (consp type) (eq (car type) :list))
     (sml-type-known-p (second type)))
    ((and (consp type) (eq (car type) :tuple))
     (every #'sml-type-known-p (cdr type)))
    ((and (consp type) (eq (car type) :record))
     (every #'sml-type-known-p (mapcar #'cdr (cdr type))))
    ((and (consp type) (eq (car type) :fn))
     (and (sml-type-known-p (second type))
          (sml-type-known-p (third type))))
    (t nil)))

(defvar *repl-checked-value-types* nil
  "Alist from value name to the type HaMLet inferred for the current phrase.")

(defun hamlet-description-value-types (description)
  "Parse the `val NAME : TYPE' entries of a HaMLet basis description, joining
the indented continuation lines of long types."
  (let ((entries nil))
    (dolist (line (uiop:split-string description :separator '(#\Newline)))
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (cond
          ((and (> (length trimmed) 4) (string= "val " trimmed :end2 4))
           ;; A long type is wrapped as "val name :" plus indented lines.
           (let ((colon (or (search " : " trimmed)
                            (and (> (length trimmed) 6)
                                 (string= " :" trimmed
                                          :start2 (- (length trimmed) 2))
                                 (- (length trimmed) 2)))))
             (when colon
               (push (cons (subseq trimmed 4 colon)
                           (subseq trimmed (min (length trimmed) (+ colon 3))))
                     entries))))
          ((and entries
                (plusp (length line))
                (member (char line 0) '(#\Space #\Tab))
                (plusp (length trimmed))
                (not (search "(*" trimmed))
                (not (member trimmed '("end" "sig" "struct") :test #'string=))
                (not (find #\: trimmed :end 1)))
           (setf (cdr (first entries))
                 (if (string= (cdr (first entries)) "")
                     trimmed
                     (concatenate 'string (cdr (first entries)) " " trimmed)))))))
    (nreverse entries)))

(defun repl-type-suffix (symbol)
  (let ((checked (cdr (assoc (symbol-name symbol) *repl-checked-value-types*
                             :test #'string=)))
        (type (lookup-sml-binding-type symbol)))
    (cond
      (checked (format nil " : ~A" checked))
      ((sml-type-known-p type)
       (format nil " : ~A" (sml-type->string type)))
      (t ""))))

(defun sml-value->string (value)
  (cond
    ((functionp value) "<fn>")
    ((and (vectorp value)
          (= (length value) 2)
          (eq (aref value 0) :ref))
     (format nil "ref ~A" (sml-value->string (aref value 1))))
    ((sml-record-p value)
     (format nil "{~{~A~^, ~}}"
             (mapcar (lambda (field)
                       (format nil "~A = ~A"
                               (car field)
                               (sml-value->string (cdr field))))
                     (sml-record-fields value))))
    ((and (consp value) (sml-exception-p value))
     (format nil "~A ~A"
             (sml-exception-name value)
             (sml-value->string (sml-exception-payload value))))
    ((sml-exception-tag-p value)
     (sml-exception-name value))
    ((eq value t) "true")
    ((null value) "[]")
    ((and (listp value) (eq (car value) :tuple))
     (if (null (cdr value))
         "()"
         (format nil "(~{~A~^, ~})"
                 (mapcar #'sml-value->string (cdr value)))))
    ((constructor-value-p value)
     (format nil "~A ~A"
             (symbol-name (car value))
             (sml-value->string (cdr value))))
    ((and (listp value) (not (constructor-value-p value)))
     (format nil "[~{~A~^, ~}]"
             (mapcar #'sml-value->string value)))
    ((characterp value)
     (format nil "#~S" (string value)))
    ((stringp value)
     (prin1-to-string value))
    ((sml-namespaced-symbol-p value)
     (symbol-name value))
    (t
     (princ-to-string value))))

(defun repl-symbol-display-name (symbol)
  ;; SML identifiers are case-sensitive and interned with their exact spelling.
  (symbol-name symbol))

(defun repl-report-decl (decl)
  (case (car decl)
    (:val
     (mapcar (lambda (sym)
               (format nil "val ~A = ~A~A"
                       (repl-symbol-display-name sym)
                       (sml-value->string (symbol-value sym))
                       (repl-type-suffix sym)))
             (remove-duplicates (pattern-bound-symbols (second decl)) :test #'eq)))
    (:val-rec
     (let ((sym (sml-symbol (second decl))))
       (list (format nil "val ~A = <fn>~A"
                     (repl-symbol-display-name sym)
                     (repl-type-suffix sym)))))
    (:fun
     (let ((sym (sml-symbol (second decl))))
       (list (format nil "val ~A = <fn>~A"
                     (repl-symbol-display-name sym)
                     (repl-type-suffix sym)))))
    (:datatype
     (list (format nil "datatype ~A"
                   (string-downcase (second decl)))))
    (:exception
     (let ((sym (sml-symbol (second decl))))
       (list (format nil "exception ~A~A"
                     (sml-exception-name (symbol-value sym))
                     (if (getf (cddr decl) :arg-type)
                         (format nil " of ~A"
                                 (sml-type-ast->string (getf (cddr decl) :arg-type)))
                         "")))))
    ((:vals :funs :val-recs)
     (mapcan #'repl-report-decl (cdr decl)))
    (:local
     (mapcan #'repl-report-decl (third decl)))
    ((:structure :structure-app :structure-alias)
     (list (format nil "structure ~A" (second decl))))
    ((:functor :signature :type)
     (list (format nil "~(~A~) ~A" (car decl) (second decl))))
    (:datatype-replication
     (list (format nil "datatype ~A" (second decl))))
    (:exception-alias
     (list (format nil "exception ~A" (second decl))))
    ((:open :infix)
     nil)
    (otherwise
     (list (format nil "~S" decl)))))

(defun repl-expression-text (trimmed)
  "The expression of the REPL phrase TRIMMED, without a final `;'."
  (string-right-trim '(#\Space #\Tab #\Newline #\Return)
                     (if (and (plusp (length trimmed))
                              (char= (char trimmed (1- (length trimmed))) #\;))
                         (subseq trimmed 0 (1- (length trimmed)))
                         trimmed)))

(defun compile-repl-phrase (kind source &key (first-line 1))
  "Check and compile the REPL phrase SOURCE of KIND (:EXPR or :PROGRAM) the
way files are compiled: generating code with what the static checker
learned, and recording the Lisp of declared bindings.  FIRST-LINE is the
session line SOURCE starts on.  Returns the form, the phrase's AST, and the
recording to commit once the form has been evaluated."
  (let ((text (trim-repl-input source)))
    (ecase kind
      (:expr
       (multiple-value-bind (form ast)
           (compile-checked-sml-expression-string (repl-expression-text text)
                                                  :package *sml-package*)
         (values form ast nil)))
      (:program
       (multiple-value-bind (form ast recording)
           (check-and-compile-sml-program
            text (make-sml-source-lines text :source "stdIn"
                                             :first-line first-line))
         (values form ast recording))))))

(defun eval-repl-program (form ast recording)
  (with-sml-phase (:eval)
    (eval form))
  (commit-sml-lisp-recording recording)
  (mapcan #'repl-report-decl (cdr ast)))

(defun eval-repl-expression (form ast)
  (let* ((type (infer-sml-ast-type ast :package *sml-package*))
         (value (with-sml-phase (:eval)
                  (eval form))))
    (setf (symbol-value (repl-it-symbol)) value)
    ;; `it' is now bound to the symbol itself, not to a binding cell.
    (forget-sml-binding-cell (repl-it-symbol))
    (register-sml-binding-type (repl-it-symbol) type)
    (export-sml-symbols (list (repl-it-symbol)))
    (list (format nil "val it = ~A~A"
                  (sml-value->string value)
                  (repl-type-suffix (repl-it-symbol))))))

(defun type-check-repl-input (kind source)
  (let ((trimmed (trim-repl-input source)))
    (type-check-sml-string
     (if (eq kind :expr)
         (let ((expression
                 (if (and (plusp (length trimmed))
                          (char= (char trimmed (1- (length trimmed))) #\;))
                     (subseq trimmed 0 (1- (length trimmed)))
                     trimmed)))
           (format nil "val it = (~A);" expression))
         trimmed))))

(defun static-checker-diagnostic (source)
  "When our parser rejects SOURCE, prefer the static checker's diagnostic: the
phrase may simply be invalid SML.  Returns NIL if there is no checker or it
accepts SOURCE (which then points at a gap in our parser)."
  (when *sml-type-checker*
    (handler-case (progn (type-check-repl-input :program source) nil)
      (sml-static-type-error (condition) condition))))

;;; REPL commands are lines starting with `:' and a letter, typed at the
;;; primary prompt.  No SML phrase starts like that: `:' is reserved, and a
;;; symbolic identifier such as `:>>' cannot contain letters.

(defparameter *repl-commands*
  '(("help" "" "List these commands.")
    ("quit" "" "Leave the REPL (also :q and :exit).")
    ("lisp" "NAME" "Show the Lisp generated for the binding NAME, e.g. Foo.bar.")
    ("lisp-local" "NAME"
     "Show it with its enclosing `local' declaration, if any.")
    ("expand" "PHRASE" "Show the Lisp for an SML phrase without running it.")
    ("show-lisp" "on|off" "Print the Lisp of each phrase as it is evaluated.")
    ("time" "on|off" "Report how long each compilation phase takes.")
    ("disassemble" "NAME" "Disassemble the compiled function bound to NAME.")))

(defun repl-command-line-p (line)
  (let ((trimmed (trim-repl-input line)))
    (and (> (length trimmed) 1)
         (char= (char trimmed 0) #\:)
         (alpha-char-p (char trimmed 1)))))

(defun split-repl-command (line)
  "The command name of LINE, without the colon, and its argument text."
  (let* ((trimmed (trim-repl-input line))
         (space (position-if (lambda (ch) (member ch '(#\Space #\Tab)))
                             trimmed)))
    (values (string-downcase (subseq trimmed 1 space))
            (if space (trim-repl-input (subseq trimmed space)) ""))))

(defun repl-on-off-argument (argument command)
  (cond
    ((string-equal argument "on") t)
    ((string-equal argument "off") nil)
    (t (error "Usage: :~A on|off" command))))

(defun repl-required-argument (argument command what)
  (if (string= argument "")
      (error "Usage: :~A ~A" command what)
      argument))

(defun print-repl-help (output)
  (format output "~&Commands (other input is evaluated as SML):~%")
  (loop for (name argument description) in *repl-commands*
        do (format output "  :~18A ~A~%"
                   (format nil "~A~@[ ~A~]" name
                           (and (string/= argument "") argument))
                   description)))

(defun disassemble-sml-value (name output)
  (let ((value (sml-value name)))
    (unless (functionp value)
      (error "~A is not a function: ~A" name (sml-value->string value)))
    (let ((*standard-output* output))
      (disassemble value))))

(defun prompt-string (continuation-p)
  (if continuation-p "= " "- "))

(defun repl (&key (input *standard-input*)
                  (output *standard-output*)
                  (error-output *error-output*)
                  (prompt t)
                  (package "SML-USER"))
  (let ((*sml-package* (ensure-sml-package package))
        (*repl-checked-value-types* nil)
        (buffer "")
        ;; Session line numbers, for the locations of REPL declarations.
        (line-number 0)
        (buffer-line 1)
        (show-lisp nil)
        (time-phases nil))
    (labels ((emit-prompt ()
               (when prompt
                 (write-string (prompt-string (not (string= buffer ""))) output)
                 (finish-output output)))
             (reset-buffer ()
               (setf buffer ""))
             (run-command (line)
               (multiple-value-bind (command argument) (split-repl-command line)
                 (cond
                   ((member command '("help" "h" "?") :test #'string=)
                    (print-repl-help output))
                   ((member command '("lisp" "lisp-local") :test #'string=)
                    (show-sml-lisp (repl-required-argument argument command "NAME")
                                   :stream output
                                   :enclosing (string= command "lisp-local")))
                   ((string= command "expand")
                    (print-sml-lisp
                     (sml->lisp (repl-required-argument argument command "PHRASE"))
                     :stream output))
                   ((string= command "show-lisp")
                    (setf show-lisp (repl-on-off-argument argument command)))
                   ((string= command "time")
                    (setf time-phases (repl-on-off-argument argument command)))
                   ((string= command "disassemble")
                    (disassemble-sml-value
                     (repl-required-argument argument command "NAME")
                     output))
                   (t
                    (error "Unknown command :~A; :help lists the commands."
                           command)))))
             (evaluate (kind)
               (with-sml-cell-reservations
                 (evaluate-phrase kind)))
             (evaluate-phrase (kind)
               (multiple-value-bind (form ast recording)
                   (compile-repl-phrase kind buffer :first-line buffer-line)
                 (setf *repl-checked-value-types*
                       (and (hamlet-type-checker-p *sml-type-checker*)
                            (hamlet-description-value-types
                             (hamlet-type-checker-last-description
                              *sml-type-checker*))))
                 (when show-lisp
                   (print-sml-lisp form :stream output))
                 (ecase kind
                   (:program (eval-repl-program form ast recording))
                   (:expr (eval-repl-expression form ast)))))
             (evaluate-and-report (kind)
               (dolist (line (if time-phases
                                 (with-sml-phase-timings (output)
                                   (evaluate kind))
                                 (evaluate kind)))
                 (format output "~A~%" line))))
      (loop
        (emit-prompt)
        (let ((line (read-line input nil :eof)))
          (when (eql line :eof)
            (return :eof))
          (incf line-number)
          (when (member (string-downcase (trim-repl-input line))
                        '(":quit" ":q" ":exit")
                        :test #'string=)
            (return :quit))
          (if (and (string= buffer "") (repl-command-line-p line))
              (handler-case (run-command line)
                (error (condition)
                  (format error-output "Error: ~A~%" condition)))
              (progn
                (when (string= buffer "")
                  (setf buffer-line line-number))
                (setf buffer (concatenate 'string buffer line (string #\Newline)))
                (multiple-value-bind (kind ast error) (parse-repl-input buffer)
                  (declare (ignore ast))
                  (let ((trimmed-buffer (trim-repl-input buffer)))
                    (cond
                      (kind
                       (handler-case (evaluate-and-report kind)
                         (sml-raised-exception (condition)
                           (format error-output "uncaught exception ~A~%"
                                   (sml-exception-description
                                    (sml-exception-value condition))))
                         (error (condition)
                           (format error-output "Error: ~A~%" condition)))
                       (reset-buffer))
                      ((or (and (> (length trimmed-buffer) 0)
                                (char= (char trimmed-buffer
                                             (1- (length trimmed-buffer)))
                                       #\;))
                           (string= (trim-repl-input line) ""))
                       (when error
                         (format error-output "Error: ~A~%"
                                 (or (static-checker-diagnostic buffer) error)))
                       (reset-buffer))))))))))))

;;; The cl-sml executable (see build-repl.sh).

(defparameter *main-usage*
  "Usage: cl-sml [--time] [--run FILE.sml | --emit-lisp FILE.sml | --help]

Without --run or --emit-lisp, start the SML REPL (:help lists its commands).
  --run FILE.sml        Load and run FILE.sml, then exit.
  --emit-lisp FILE.sml  Print the Common Lisp generated for FILE.sml, then exit.
  --time                Report how long each compilation phase takes.
  --help                Show this message.
")

(defun write-sml-file-lisp (pathname stream)
  "Write the Lisp artifact for the SML file PATHNAME to STREAM."
  (multiple-value-bind (form package declarations) (compile-sml-file pathname)
    (write-sml-lisp-artifact form package pathname stream declarations)))

(defun main (&optional (arguments (portable-command-line-arguments)))
  "Entry point of the cl-sml executable: act on the command line ARGUMENTS
and return the exit status."
  (let ((time-phases nil)
        (action nil))
    (loop while arguments
          do (let ((argument (pop arguments)))
               (cond
                 ((string= argument "--time")
                  (setf time-phases t))
                 ((member argument '("--help" "-h") :test #'string=)
                  (setf action (list :help)))
                 ((and (member argument '("--run" "--emit-lisp") :test #'string=)
                       arguments)
                  (setf action (list (if (string= argument "--run") :run :emit)
                                     (pop arguments))))
                 (t
                  (format *error-output* "cl-sml: unexpected argument ~A~%~A"
                          argument *main-usage*)
                  (return-from main 2)))))
    (flet ((run ()
             (ecase (first action)
               ((nil) (repl))
               (:help (write-string *main-usage*))
               (:run (load-sml-file (second action)))
               (:emit (write-sml-file-lisp (second action) *standard-output*)))))
      (handler-case
          (progn
            (if time-phases
                (with-sml-phase-timings (*error-output*)
                  (run))
                (run))
            (finish-output)
            0)
        (error (condition)
          (finish-output)
          (format *error-output* "~&cl-sml: ~A~%" condition)
          1)))))
