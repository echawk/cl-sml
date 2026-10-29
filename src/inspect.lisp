(in-package #:cl-sml)

;;; Inspecting the compilation pipeline.
;;;
;;; While SML declarations are compiled, the Common Lisp form generated for
;;; each declaration is remembered under the qualified names it binds, per
;;; SML package, together with the hoisted helper functions it calls and the
;;; SML source line it came from.  SML-LISP-SOURCE and SHOW-SML-LISP (and the
;;; REPL's :lisp command) look these up; SML->LISP compiles a phrase without
;;; evaluating or recording it.  WITH-SML-PHASE-TIMINGS reports where the
;;; time of a compilation goes.

(defvar *record-sml-lisp-source* t
  "When true, compiling SML declarations remembers the Common Lisp generated
for each binding, for SML-LISP-SOURCE.  The forms stay alive as long as their
package's records do; bind this to NIL to compile without keeping them.")

(defstruct (sml-lisp-definition (:constructor %make-sml-lisp-definition))
  ;; Qualified SML name, e.g. "Foo.bar".
  name
  ;; :VALUE, :TYPE, :STRUCTURE or :FUNCTOR.
  namespace
  ;; The declaration's SML keyword, e.g. "fun" or "structure".
  keyword
  ;; The declaration AST node, while its compilation is being recorded.
  dec
  ;; The CL form compiled for the declaration.
  form
  ;; The hoisted top-level DEFUNs FORM calls, directly or indirectly.
  helpers
  ;; Where the declaration came from: a file name or "stdIn", and a line.
  source
  line
  ;; The outermost `local' declaration enclosing this one, as an
  ;; SML-LISP-DEFINITION (its form binds what FORM refers to), or NIL.
  enclosing
  ;; For a structure that is a functor instance or a structure alias, the
  ;; qualified name of the functor or structure its members come from.
  origin)

(defvar *sml-lisp-definitions* (make-hash-table :test #'equal)
  "SML package name -> EQUAL hash table from qualified SML name to the
SML-LISP-DEFINITIONs recorded for that name, at most one per namespace.")

(defun sml-lisp-definitions-table (package &optional create)
  (let ((name (package-name (ensure-sml-package package))))
    (or (gethash name *sml-lisp-definitions*)
        (and create
             (setf (gethash name *sml-lisp-definitions*)
                   (make-hash-table :test #'equal))))))

(defun clear-sml-lisp-source (&optional package)
  "Forget the generated Common Lisp recorded for PACKAGE, or for every
package when PACKAGE is NIL."
  (if package
      (remhash (package-name (ensure-sml-package package)) *sml-lisp-definitions*)
      (clrhash *sml-lisp-definitions*))
  (values))

;;; Source lines

(defstruct (sml-source-lines (:constructor %make-sml-source-lines))
  ;; Source name for locations, e.g. a file name or "stdIn", or NIL.
  source
  ;; Offsets at which the lines of the parsed text start.
  line-starts
  ;; Source line of each line of the parsed text, when the parsed text was
  ;; rewritten (see NORMALIZE-SML-TYPE-DECLARATION-CONTINUATIONS); otherwise
  ;; the parsed text's first line is FIRST-LINE.
  line-map
  (first-line 1))

(defun make-sml-source-lines (text &key source line-map (first-line 1))
  (%make-sml-source-lines :source source
                          :line-starts (sml-line-start-offsets text)
                          :line-map line-map
                          :first-line first-line))

(defun sml-source-line (lines offset)
  "The source line of OFFSET in the parsed text LINES describes."
  (let* ((starts (sml-source-lines-line-starts lines))
         (index (let ((low 0) (high (1- (length starts))))
                  ;; The last line starting at or before OFFSET.
                  (loop while (< low high)
                        do (let ((middle (ceiling (+ low high) 2)))
                             (if (<= (aref starts middle) offset)
                                 (setf low middle)
                                 (setf high (1- middle)))))
                  low))
         (line-map (sml-source-lines-line-map lines)))
    (if (and line-map (< index (length line-map)))
        (aref line-map index)
        (+ (sml-source-lines-first-line lines) index))))

(defun sml-declaration-line (dec lines)
  "The source line where declaration DEC starts, when the parser recorded it
in *SML-DECLARATION-POSITIONS*."
  (let ((offset (and *sml-declaration-positions* lines
                     (gethash dec *sml-declaration-positions*))))
    (and offset (sml-source-line lines offset))))

(defun make-sml-declaration-position-table ()
  ;; Backtracking parses create declarations that never reach the AST.
  (make-portable-weak-eq-hash-table))

;;; Recording

(defstruct (sml-lisp-recording (:constructor %make-sml-lisp-recording))
  ;; The SML package being compiled.
  package
  ;; The SML-SOURCE-LINES of the text being compiled.
  lines
  ;; SML-LISP-DEFINITIONs of this compilation, newest first.
  (definitions nil))

(defun make-sml-lisp-recording (lines &key (package *sml-package*))
  "A recording of the declarations compiled from the text LINES describes,
or NIL when *RECORD-SML-LISP-SOURCE* is false."
  (when *record-sml-lisp-source*
    (%make-sml-lisp-recording :package (ensure-sml-package package)
                              :lines lines)))

(defun call-with-sml-lisp-recording (recording thunk)
  "Call THUNK, which compiles parsed declarations, recording each declaration
it compiles in RECORDING.  A NIL RECORDING records nothing."
  (if recording
      (let ((*sml-declaration-compiled-hook*
              (lambda (dec form)
                (record-compiled-sml-declaration recording dec form))))
        (funcall thunk))
      (funcall thunk)))

(defmacro with-sml-lisp-recording ((recording) &body body)
  `(call-with-sml-lisp-recording ,recording (lambda () ,@body)))

(defun sml-declaration-keyword (dec)
  (case (car dec)
    ((:fun :funs) "fun")
    ((:val :vals) "val")
    (:val-rec "val rec")
    ((:datatype :datatype-replication) "datatype")
    ((:exception :exception-alias) "exception")
    ((:structure :structure-app :structure-alias) "structure")
    (:functor "functor")
    (:type "type")
    (:local "local")
    (otherwise nil)))

(defun sml-declaration-description (dec)
  "A short description of top-level declaration DEC, e.g. \"fun fact\"."
  (let ((keyword (case (car dec)
                   (:open "open")
                   (:signature "signature")
                   (:infix (second dec))
                   (:expr "expression")
                   (otherwise (sml-declaration-keyword dec))))
        (names (case (car dec)
                 ((:structure :structure-app :structure-alias :functor
                   :type :datatype :datatype-replication :signature)
                  (list (second dec)))
                 ((:open :infix :expr) nil)
                 (otherwise (ignore-errors (declaration-direct-bound-names dec))))))
    (format nil "~A~@[ ~{~A~^, ~}~]~:[~; ...~]"
            (or keyword (string-downcase (car dec)))
            (subseq names 0 (min 6 (length names)))
            (> (length names) 6))))

(defun sml-declaration-bindings (dec)
  "The (NAMESPACE . NAME) pairs DEC binds, with NAMEs unqualified.  The
members of structures are recorded by their own declarations."
  (case (car dec)
    ((:structure :structure-app :structure-alias)
     (list (cons :structure (second dec))))
    (:functor (list (cons :functor (second dec))))
    (:type (list (cons :type (second dec))))
    ((:datatype :datatype-replication)
     (cons (cons :type (second dec))
           (mapcar (lambda (name) (cons :value name))
                   (declaration-direct-bound-names dec))))
    ((:fun :funs :val :vals :val-rec :exception :exception-alias)
     (mapcar (lambda (name) (cons :value name))
             (declaration-direct-bound-names dec)))
    (otherwise nil)))

(defun sml-declaration-origin (dec)
  (case (car dec)
    (:structure-app (resolve-functor-name (third dec)))
    (:structure-alias (resolve-structure-prefix (third dec)))
    (otherwise nil)))

(defun hoisted-sml-helpers-called-by (form)
  "The hoisted DEFUNs of the current compilation that FORM calls, directly
or through other hoisted functions, in the order they were hoisted."
  (when *sml-hoisted-forms*
    (let ((by-name (make-hash-table :test #'eq))
          (seen (make-hash-table :test #'eq))
          (used (make-hash-table :test #'eq)))
      (dolist (hoisted *sml-hoisted-forms*)
        (setf (gethash (second hoisted) by-name) hoisted))
      (labels ((walk (x)
                 (cond
                   ((symbolp x)
                    (let ((hoisted (gethash x by-name)))
                      (when (and hoisted (not (gethash x used)))
                        (setf (gethash x used) t)
                        (walk (cddr hoisted)))))
                   ((and (consp x) (not (gethash x seen)))
                    (setf (gethash x seen) t)
                    (walk (car x))
                    (walk (cdr x))))))
        (walk form))
      (loop for hoisted in (reverse *sml-hoisted-forms*)
            when (gethash (second hoisted) used)
              collect hoisted))))

(defun sml-ast-subtree-conses (tree)
  (let ((conses (make-hash-table :test #'eq)))
    (labels ((walk (x)
               (when (and (consp x) (not (gethash x conses)))
                 (setf (gethash x conses) t)
                 (walk (car x))
                 (walk (cdr x)))))
      (walk tree))
    conses))

(defun record-compiled-sml-declaration (recording dec form)
  (when (consp dec)
    (let* ((lines (sml-lisp-recording-lines recording))
           (source (sml-source-lines-source lines))
           (line (sml-declaration-line dec lines))
           (keyword (sml-declaration-keyword dec))
           (helpers (hoisted-sml-helpers-called-by form)))
      (when (eq (car dec) :local)
        ;; The declarations of a `local' body were recorded with their own
        ;; forms, which refer to variables the local part binds.
        (let ((enclosing (%make-sml-lisp-definition
                          :name nil :namespace :local :keyword keyword
                          :form form :helpers helpers
                          :source source
                          :line line))
              (body (sml-ast-subtree-conses (third dec))))
          (dolist (definition (sml-lisp-recording-definitions recording))
            (when (gethash (sml-lisp-definition-dec definition) body)
              (setf (sml-lisp-definition-enclosing definition) enclosing)))))
      (dolist (binding (sml-declaration-bindings dec))
        (push (%make-sml-lisp-definition
               :name (current-qualified-sml-name (cdr binding))
               :namespace (car binding)
               :keyword (if (and (eq (car binding) :value)
                                 (member (car dec) '(:datatype
                                                     :datatype-replication)))
                            "constructor"
                            keyword)
               :dec dec
               :form form
               :helpers helpers
               :source source
               :line line
               :origin (sml-declaration-origin dec))
              (sml-lisp-recording-definitions recording))))))

(defun commit-sml-lisp-recording (recording)
  "Make the definitions RECORDING collected visible to SML-LISP-SOURCE."
  (when recording
    (let ((table (sml-lisp-definitions-table
                  (sml-lisp-recording-package recording) t)))
      (dolist (definition (reverse (sml-lisp-recording-definitions recording)))
        (setf (sml-lisp-definition-dec definition) nil)
        (let ((name (sml-lisp-definition-name definition)))
          (setf (gethash name table)
                (cons definition
                      (remove (sml-lisp-definition-namespace definition)
                              (gethash name table)
                              :key #'sml-lisp-definition-namespace))))))
    (setf (sml-lisp-recording-definitions recording) nil)))

;;; Lookup

(defun find-sml-lisp-definitions (name package &optional (depth 0))
  "The recorded definitions of qualified NAME in PACKAGE.  A member of a
structure that is a functor instance or an alias is looked up where its
code was generated, in the functor or structure it comes from."
  (let ((table (sml-lisp-definitions-table package)))
    (when table
      (or (gethash name table)
          (and (< depth 16)
               (let ((dot (length name)))
                 (loop
                   (setf dot (position #\. name :from-end t :end dot))
                   (unless dot
                     (return nil))
                   (let ((origin (some #'sml-lisp-definition-origin
                                       (gethash (subseq name 0 dot) table))))
                     (when origin
                       (return (find-sml-lisp-definitions
                                (qualify-sml-name origin (subseq name (1+ dot)))
                                package
                                (1+ depth))))))))))))

(defun sml-lisp-source (name &key (package *sml-package*) enclosing)
  "The Common Lisp generated for the SML binding NAME (a qualified name such
as \"Foo.bar\") in PACKAGE: a list of forms, the hoisted helper functions the
definition calls followed by the definition itself.  With ENCLOSING, a
binding declared inside `local ... in ... end' is shown by the form of the
outermost enclosing `local', which also binds what the binding refers to.
Returns NIL when nothing was recorded for NAME; the second value is the list
of SML-LISP-DEFINITIONs found."
  (let ((definitions (find-sml-lisp-definitions name package)))
    (values
     (remove-duplicates
      (loop for definition in definitions
            for shown = (or (and enclosing
                                 (sml-lisp-definition-enclosing definition))
                            definition)
            append (append (sml-lisp-definition-helpers shown)
                           (list (sml-lisp-definition-form shown))))
      :test #'eq :from-end t)
     definitions)))

;;; Printing

(defun call-with-sml-lisp-printing (package thunk)
  "Call THUNK with printer settings for showing generated code: symbols of
PACKAGE (SML identifiers) and Common Lisp print unqualified, cl-sml's
runtime as cl-sml::NAME, and generated temporaries without #:."
  (let ((*package* (ensure-sml-package package))
        (*print-case* :downcase)
        (*print-pretty* t)
        (*print-circle* nil)
        (*print-gensym* nil)
        (*print-readably* nil)
        (*print-escape* t)
        (*print-length* nil)
        (*print-level* nil)
        (*print-lines* nil)
        (*print-right-margin* 100))
    (funcall thunk)))

(defun print-sml-lisp (form &key (package *sml-package*) (stream *standard-output*))
  "Pretty-print FORM, code generated for PACKAGE, readably to STREAM."
  (call-with-sml-lisp-printing
   package
   (lambda ()
     (fresh-line stream)
     (write form :stream stream)
     (terpri stream)))
  (values))

(defun sml-lisp-definition-location (definition)
  (let ((source (sml-lisp-definition-source definition))
        (line (sml-lisp-definition-line definition)))
    (cond
      ((and source line) (format nil "~A:~D" source line))
      (source (princ-to-string source))
      (line (format nil "line ~D" line)))))

(defun show-sml-lisp (name &key (package *sml-package*) (stream *standard-output*)
                                enclosing)
  "Print the Common Lisp generated for the SML binding NAME in PACKAGE (see
SML-LISP-SOURCE), with comments saying where each definition came from."
  (let ((definitions (find-sml-lisp-definitions name package))
        (printed nil))
    (unless definitions
      (error "No generated Lisp is recorded for ~A in ~A~:[~;; it is bound, ~
              but was not compiled from SML here (e.g. it is part of the Basis)~]."
             name (package-name (ensure-sml-package package))
             (let ((symbol (find-symbol name (ensure-sml-package package))))
               (and symbol (boundp symbol)))))
    (dolist (definition definitions)
      (let* ((local (sml-lisp-definition-enclosing definition))
             (shown (or (and enclosing local) definition))
             (location (sml-lisp-definition-location definition)))
        (format stream "~&;;; ~A ~A~@[ -- ~A~]~%"
                (sml-lisp-definition-keyword definition)
                (sml-lisp-definition-name definition)
                location)
        (when (and local (not enclosing))
          (format stream ";;; Declared inside `local'~@[ at ~A~]; the local ~
                          declarations are bound around it.~%"
                  (sml-lisp-definition-location local)))
        (when (and local enclosing)
          (format stream ";;; Shown with its enclosing `local'~@[ at ~A~].~%"
                  (sml-lisp-definition-location local)))
        (unless (member (sml-lisp-definition-form shown) printed :test #'eq)
          (push (sml-lisp-definition-form shown) printed)
          (print-sml-lisp (sml-lisp-definition-form shown)
                          :package package :stream stream)
          (when (sml-lisp-definition-helpers shown)
            (format stream "~&;;; Hoisted helper function~P it calls:~%"
                    (length (sml-lisp-definition-helpers shown)))
            (dolist (helper (sml-lisp-definition-helpers shown))
              (print-sml-lisp helper :package package :stream stream))))))
    (values)))

;;; Phrases

(defun sml-phrase-expression-text (text)
  "If TEXT is a single SML expression (optionally ending with `;'), the
expression's text, else NIL."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text))
         (program (handler-case (esrap:parse 'sml-program trimmed)
                    (error () nil))))
    (cond
      (program
       (and (= (length program) 2)
            (consp (second program))
            (eq (car (second program)) :expr)
            (string-right-trim '(#\Space #\Tab #\Newline #\Return #\;) trimmed)))
      ((handler-case (progn (esrap:parse 'sml-expr trimmed) t)
         (error () nil))
       trimmed))))

(defun sml->lisp (text &key (package *sml-package*) (type-checker *sml-type-checker*))
  "The Common Lisp form cl-sml generates for the SML phrase TEXT, a single
expression or a sequence of declarations, compiled in PACKAGE without
evaluating it.  TYPE-CHECKER, if any, is consulted as in normal compilation
but does not keep what it elaborates."
  (let ((*sml-package* (ensure-sml-package package))
        (*record-sml-lisp-source* nil))
    (call-preserving-sml-checker-session
     type-checker
     (lambda ()
       (let ((expression (sml-phrase-expression-text text)))
         (if expression
             (values (compile-checked-sml-expression-string
                      expression :package *sml-package* :type-checker type-checker))
             (values (compile-sml-program-string
                      text :package *sml-package* :type-checker type-checker))))))))

;;; Phase timings

(defvar *sml-phase-timings* nil
  "While timing, a list of (PHASE INTERNAL-TIME COUNT) entries.")

(defvar *sml-phase-nested-time* 0
  "Internal time spent in phases nested in the one being timed.")

(defparameter *sml-phase-descriptions*
  '((:check . "static check")
    (:parse . "parse SML")
    (:compile . "SML -> Lisp")
    (:write . "write Lisp file")
    (:cl-compile . "compile Lisp")
    (:load . "load compiled Lisp")
    (:eval . "evaluate Lisp")))

(defun call-with-sml-phase (phase thunk)
  "Call THUNK, adding its time (less that of nested phases) to PHASE when
timing phases."
  (if (null *sml-phase-timings*)
      (funcall thunk)
      (let ((start (get-internal-real-time))
            (nested 0))
        (unwind-protect
             (let ((*sml-phase-nested-time* 0))
               (unwind-protect (funcall thunk)
                 (setf nested *sml-phase-nested-time*)))
          (let* ((elapsed (- (get-internal-real-time) start))
                 (entry (or (assoc phase (cdr *sml-phase-timings*))
                            (let ((entry (list phase 0 0)))
                              (setf (cdr (last *sml-phase-timings*)) (list entry))
                              entry))))
            (incf (second entry) (- elapsed nested))
            (incf (third entry))
            (incf *sml-phase-nested-time* elapsed))))))

(defmacro with-sml-phase ((phase) &body body)
  `(call-with-sml-phase ,phase (lambda () ,@body)))

(defun report-sml-phase-timings (timings stream)
  (flet ((milliseconds (time)
           (/ (* 1000 time) internal-time-units-per-second)))
    (dolist (entry timings)
      (destructuring-bind (phase time count) entry
        (format stream "~&;; ~18A ~10,1F ms~@[  (~D times)~]~%"
                (or (cdr (assoc phase *sml-phase-descriptions*))
                    (string-downcase phase))
                (milliseconds time)
                (and (> count 1) count))))
    (format stream "~&;; ~18A ~10,1F ms~%" "total"
            (milliseconds (reduce #'+ timings :key #'second)))))

(defun call-with-sml-phase-timings (stream thunk)
  (let ((*sml-phase-timings* (list :timings))
        (*sml-phase-nested-time* 0))
    (multiple-value-prog1 (funcall thunk)
      (report-sml-phase-timings (cdr *sml-phase-timings*) stream))))

(defmacro with-sml-phase-timings ((&optional (stream '*trace-output*)) &body body)
  "Run BODY, then report to STREAM how long each compilation phase it ran
took: static checking, parsing, generating Lisp, compiling and loading or
evaluating the Lisp."
  `(call-with-sml-phase-timings ,stream (lambda () ,@body)))
