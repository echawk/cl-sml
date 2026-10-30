(in-package #:cl-sml)

(defun read-sml-source (stream)
  (with-output-to-string (out)
    (loop for c = (read-char stream nil :eof)
          until (eql c :eof)
          do (write-char c out))))

(defun sml-file-directory (pathname)
  (make-pathname :directory (pathname-directory (truename pathname))))

(defun compile-sml-program-string (sml-text &key package source-name
                                                    (type-checker *sml-type-checker*))
  "Compile the SML program SML-TEXT to a Common Lisp form, without evaluating
it.  The second value has an entry (LINE . DESCRIPTION) for each top-level
declaration, in the order of the declarations' forms in the program, where
LINE is the declaration's line in SML-TEXT (or NIL) and DESCRIPTION is e.g.
\"fun fact\".  The Lisp generated for each binding is recorded for
SML-LISP-SOURCE when *RECORD-SML-LISP-SOURCE* is true."
  (let* ((*sml-package* (ensure-sml-package (or package (current-sml-package))))
         (lines (make-sml-source-lines sml-text :source source-name)))
    (multiple-value-bind (form ast recording positions)
        (check-and-compile-sml-program sml-text lines
                                       :type-checker type-checker
                                       :filename source-name)
      (commit-sml-lisp-recording recording)
      (values form
              (let ((*sml-declaration-positions* positions))
                (mapcar (lambda (dec)
                          (cons (sml-declaration-line dec lines)
                                (sml-declaration-description dec)))
                        (cdr ast)))))))

(defun check-and-compile-sml-program (text lines &key (type-checker *sml-type-checker*)
                                                      filename)
  "Statically check the SML program TEXT, then parse it and compile it to a
Common Lisp form with what the checker learned, recording the Lisp of its
bindings.  LINES are TEXT's SML-SOURCE-LINES.  Returns the form, the
program's AST, the recording to commit (or NIL), and the table of
declaration positions."
  (let* ((recording (make-sml-lisp-recording lines))
         ;; The static checker runs first: it is the authority on what is a
         ;; valid SML program and gives better diagnostics than our parser.
         ;; What it learned guides both parsing and code generation.
         (facts (with-sml-phase (:check)
                  (static-facts-from-check
                   (multiple-value-list
                    (type-check-sml-string text
                                           :checker type-checker
                                           :filename filename)))))
         ;; Generated temporaries are numbered from 0 for each program, so
         ;; that emitted artifacts do not depend on what the image compiled
         ;; before.
         (*gensym-counter* 0)
         (*sml-declaration-positions* (make-sml-declaration-position-table)))
    (with-sml-static-facts (facts)
      (multiple-value-bind (ast scopes)
          (with-sml-phase (:parse)
            (parse-sml-program text :source-name filename))
        (let ((form (with-sml-phase (:compile)
                      (with-sml-lisp-recording (recording)
                        (compile-with-hoisted-sml-forms
                         (lambda () (compile-program ast))
                         :identity text)))))
          ;; Later programs in this package see the fixity and identifier
          ;; status this one declared.
          (setf (sml-package-parse-scopes *sml-package*) scopes)
          (values form ast recording *sml-declaration-positions*))))))

(defun static-facts-from-check (results)
  "The SML-STATIC-FACTS among the values a type checker returned, if any."
  (find-if #'sml-static-facts-p results))

(defun compile-sml-declarations-string (sml-text &key package)
  (compile-sml-program-string sml-text :package package))

(defun compile-sml-expression-string (sml-text &key package)
  (let ((*sml-package* (ensure-sml-package (or package (current-sml-package)))))
    (compile-with-hoisted-sml-forms
     (lambda ()
       (compile-expr (parse-sml-expression sml-text)))
     :identity sml-text)))

(defparameter *sml-checked-expression-prefix* "val it = ("
  "Expressions are statically checked as the declaration `val it = (EXP);'.")

(defun compile-checked-sml-expression-string (sml-text &key package
                                                         (type-checker *sml-type-checker*))
  "Compile the SML expression SML-TEXT like COMPILE-SML-EXPRESSION-STRING,
but check it first, as the REPL does, and use what the checker learned.
Returns the form and the expression's AST."
  (let* ((*sml-package* (ensure-sml-package (or package (current-sml-package))))
         (facts (with-sml-phase (:check)
                  (static-facts-from-check
                   (multiple-value-list
                    (type-check-sml-string
                     (concatenate 'string *sml-checked-expression-prefix*
                                  sml-text ");")
                     :checker type-checker))))))
    (with-sml-static-facts (facts :offset (length *sml-checked-expression-prefix*))
      (let ((ast (with-sml-phase (:parse) (parse-sml-expression sml-text))))
        (values (with-sml-phase (:compile)
                  (compile-with-hoisted-sml-forms
                   (lambda () (compile-expr ast))
                   :identity sml-text))
                ast)))))

(defun compile-sml-file (pathname &key package)
  "Compile the SML file PATHNAME to a Common Lisp form, without evaluating
it.  Returns the form, the SML package, and the top-level declaration notes
of COMPILE-SML-PROGRAM-STRING."
  (with-open-file (stream pathname :direction :input
                                   :external-format *sml-external-format*)
    (let ((target-package (ensure-sml-package (or package (pathname->sml-package-name pathname)))))
      (multiple-value-bind (form declarations)
          (compile-sml-program-string
           (read-sml-source stream)
           :package target-package
           :source-name (namestring pathname))
        (values form target-package declarations)))))

(defun hoisted-sml-function-definition-p (form)
  (and (consp form)
       (eq (first form) 'defun)
       (symbolp (second form))
       (uiop:string-prefix-p "%CL-SML-CASE-ACTION-"
                             (symbol-name (second form)))))

(defun split-hoisted-sml-forms (form)
  (if (and (consp form) (eq (first form) 'progn))
      (loop with hoisted = nil
            with remaining = (rest form)
            while (and remaining
                       (hoisted-sml-function-definition-p (first remaining)))
            do (push (pop remaining) hoisted)
            finally (return (values (nreverse hoisted)
                                    `(progn ,@remaining))))
      (values nil form)))

(defmacro with-sml-artifact-context ((source-directory) &body body)
  "Run BODY, top-level code of an emitted artifact, in the SML package the
artifact is read in (its IN-PACKAGE) and with SOURCE-DIRECTORY, the SML
file's directory, as the directory `use' resolves names against."
  `(let ((*sml-package* (ensure-sml-package ,(package-name *package*)))
         (*sml-current-directory* ,source-directory))
     ,@body))

(defun sml-artifact-shadowed-names (forms package)
  "Names of the symbols of PACKAGE in FORMS that shadow Common Lisp symbols:
read in a fresh PACKAGE, they would be the inherited CL symbols."
  (let ((names nil)
        (seen (make-hash-table :test #'eq)))
    (labels ((walk (x)
               (cond
                 ((symbolp x)
                  (when (and (eq (symbol-package x) package)
                             (eq :external
                                 (nth-value 1 (find-symbol (symbol-name x)
                                                           :common-lisp))))
                    (pushnew (symbol-name x) names :test #'string=)))
                 ((and (consp x) (not (gethash x seen)))
                  (setf (gethash x seen) t)
                  (walk (car x))
                  (walk (cdr x))))))
      (mapc #'walk forms))
    (sort names #'string<)))

(defun write-sml-lisp-artifact (form target-package source-pathname stream
                                &optional declarations)
  "Write FORM, compiled from SOURCE-PATHNAME for TARGET-PACKAGE, as a
loadable Common Lisp file to STREAM.  Each top-level declaration becomes a
top-level form, preceded by a comment with its SML source line when
DECLARATIONS (see COMPILE-SML-PROGRAM-STRING) are given.  The output only
depends on FORM and the source location."
  ;; Standard I/O syntax first, so that no user printer setting (e.g. a
  ;; pprint dispatch table) leaks into the artifact; the bindings below
  ;; then choose how it is printed.
  (with-standard-io-syntax
  (let* ((package-name (package-name target-package))
         (source-directory (sml-file-directory source-pathname))
         (*package* (find-package :cl-user))
         (*print-pretty* t)
         (*print-circle* t)
         (*print-case* :downcase)
         (*print-escape* t)
         (*print-readably* nil)
         (*print-gensym* t)
         (*print-base* 10)
         (*print-radix* nil)
         (*print-length* nil)
         (*print-level* nil)
         (*print-lines* nil)
         (*print-right-margin* 100)
         (*read-default-float-format* 'single-float))
    (multiple-value-bind (hoisted-forms program-form)
        (split-hoisted-sml-forms form)
      (let ((declaration-forms
              ;; The (PROGN declaration-form ...) of COMPILE-PROGRAM, which
              ;; SPLIT-HOISTED-SML-FORMS wraps once more if it split off
              ;; hoisted helpers.
              (rest (if hoisted-forms (second program-form) program-form)))
            (shadowed (sml-artifact-shadowed-names (list form) target-package)))
        (format stream ";;; Generated by cl-sml from ~A; do not edit.~%~
                        ;;; Loading it (with cl-sml loaded) defines the SML ~
                        program's bindings~%;;; in package ~A.~%"
                (namestring source-pathname) package-name)
        (pprint `(eval-when (:compile-toplevel :load-toplevel :execute)
                   (cl-sml::ensure-sml-package ,package-name)
                   ,@(when shadowed
                       `((shadow ',shadowed ,package-name))))
                stream)
        (pprint `(in-package ,package-name) stream)
        (terpri stream)
        (let ((*package* target-package)
              (source-name (file-namestring source-pathname)))
          (when hoisted-forms
            (format stream "~%;;; Helper functions hoisted out of large case ~
                            expressions.~%")
            (dolist (hoisted-form hoisted-forms)
              (pprint hoisted-form stream))
            (terpri stream))
          (loop for declaration-form in declaration-forms
                for notes = declarations then (rest notes)
                for (line . description) = (first notes)
                do (format stream "~%~@[;;; ~A~]~%"
                           (cond
                             (line (format nil "~A:~D: ~A"
                                           source-name line description))
                             (description)))
                   (write `(cl-sml::with-sml-artifact-context (,source-directory)
                             ,declaration-form)
                          :stream stream)
                   (terpri stream))))))))

(defun emit-sml-file (pathname output-pathname
                      &key package (if-exists :supersede))
  "Compile PATHNAME and write loadable Common Lisp to OUTPUT-PATHNAME."
  (multiple-value-bind (form target-package declarations)
      (compile-sml-file pathname :package package)
    (with-open-file (stream output-pathname
                            :direction :output
                            :if-exists if-exists
                            :if-does-not-exist :create
                            :external-format *sml-external-format*)
      (write-sml-lisp-artifact form target-package pathname stream declarations))
    (values (truename output-pathname) target-package)))

(defvar *sml-artifact-random-state* (make-random-state t))

(defun temporary-sml-artifact-pathname ()
  "A fresh pathname for an emitted artifact.  Gensym counters restart in
every image, so the name also carries a random component."
  (loop for pathname = (merge-pathnames
                        (make-pathname
                         :name (format nil "~(~A~)-~36R"
                                       (symbol-name (gensym "cl-sml-load-"))
                                       (random (expt 36 6)
                                               *sml-artifact-random-state*))
                         :type "lisp")
                        (uiop:temporary-directory))
        unless (probe-file pathname)
          return pathname))

(defvar *show-lisp-compiler-output* nil
  "When true, LOAD-SML-FILE lets the Lisp compiler report on the code it
generated as it compiles it.  The report, mostly notes about generated code,
is discarded otherwise.")

(defun compile-sml-lisp-artifact (artifact)
  "COMPILE-FILE ARTIFACT, discarding the compiler's report unless
*SHOW-LISP-COMPILER-OUTPUT*."
  (flet ((compile-artifact ()
           (handler-bind ((style-warning #'muffle-warning))
             (with-standard-io-syntax
               (compile-file artifact
                             :external-format *sml-external-format*)))))
    (if *show-lisp-compiler-output*
        (compile-artifact)
        ;; Not kept in a string: for large programs it takes enough memory
        ;; to exhaust a 1GB heap while HaMLet loads.
        (let* ((discard (make-broadcast-stream))
               (*standard-output* discard)
               (*error-output* discard)
               (*compile-verbose* nil)
               (*compile-print* nil))
          (compile-artifact)))))

(defun execute-compiled-sml-file (form target-package source-pathname
                                  &optional declarations)
  ;; COMPILE-FILE lets JVM-hosted CL implementations split very large generated
  ;; programs across methods instead of hitting a single-method bytecode limit.
  (let ((artifact (temporary-sml-artifact-pathname))
        (compiled-artifact nil))
    (unwind-protect
         (progn
           (with-sml-phase (:write)
             (with-open-file (stream artifact
                                     :direction :output
                                     :if-exists :error
                                     :if-does-not-exist :create
                                     :external-format *sml-external-format*)
               (write-sml-lisp-artifact form target-package source-pathname
                                        stream declarations)))
           (multiple-value-bind (output warnings-p failure-p)
               (with-sml-phase (:cl-compile)
                 (compile-sml-lisp-artifact artifact))
             (declare (ignore warnings-p failure-p))
             ;; Some implementations report optimization/type-inference
             ;; warnings as FAILURE-P while still producing a usable FASL.
             (unless (and output (probe-file output))
               (error "Failed to compile generated Lisp for ~A~:[ (set ~
                       CL-SML:*SHOW-LISP-COMPILER-OUTPUT* to see why)~;~]"
                      source-pathname *show-lisp-compiler-output*))
             (setf compiled-artifact output))
           (with-sml-phase (:load)
             (load compiled-artifact)))
      (when (and compiled-artifact (probe-file compiled-artifact))
        (delete-file compiled-artifact))
      (when (probe-file artifact)
        (delete-file artifact)))))

(defun load-sml-file (pathname &key package (compile t))
  (with-sml-cell-reservations
    (load-sml-file-1 pathname package compile)))

(defun load-sml-file-1 (pathname package compile)
  (multiple-value-bind (form target-package declarations)
      (compile-sml-file pathname :package package)
    (values target-package
            (if compile
                (execute-compiled-sml-file form target-package pathname
                                           declarations)
                (let ((*sml-package* target-package)
                      (*sml-current-directory* (sml-file-directory pathname)))
                  (with-sml-phase (:eval)
                    (eval form)))))))

(defun read-sml-block (stream char arg)
  (declare (ignore char arg))
  (let ((sml-text
          (with-output-to-string (out)
            (loop for c = (read-char stream nil :eof)
                  when (eql c :eof) do (error "Unterminated SML block")
                  when (char= c #\})
                    do (let ((next (read-char stream nil :eof)))
                         (if (char= next #\#)
                             (return)
                             (progn (write-char c out) (write-char next out))))
                  else do (write-char c out)))))
    (values (compile-sml-program-string sml-text :package (current-sml-package)))))

(defreadtable sml-readtable
  (:merge :standard)
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\{ #'read-sml-block))
