(in-package #:cl-sml)

;;; cl-sml's Basis Library.  The SML sources in src/basis/ (derived from
;;; HaMLet's; see src/basis/all.sml) are compiled into the package SML.BASIS
;;; when this file is compiled, and the Lisp they compile to becomes part of
;;; this file's FASL: loading cl-sml loads the basis without compiling any
;;; SML.  Every SML package sees the basis's bindings (SML-VISIBLE-SYMBOL in
;;; runtime.lisp).  cl-sml.asd makes the SML sources inputs of this file's
;;; compilation, so changing one recompiles the basis.

;;; The functions below run when this file is compiled.
(eval-when (:compile-toplevel :load-toplevel :execute)

(defun sml-basis-directory ()
  "src/basis/ of the cl-sml system."
  (asdf:system-relative-pathname "cl-sml" "src/basis/"))

(defun strip-sml-comments (text)
  "TEXT with its (nested) comments replaced by spaces; strings are kept."
  (let ((out (make-string-output-stream))
        (depth 0)
        (i 0)
        (n (length text)))
    (loop while (< i n)
          do (let ((ch (char text i))
                   (next (and (< (1+ i) n) (char text (1+ i)))))
               (cond
                 ((and (char= ch #\() (eql next #\*))
                  (incf depth)
                  (incf i 2))
                 ((and (plusp depth) (char= ch #\*) (eql next #\)))
                  (decf depth)
                  (incf i 2))
                 ((plusp depth) (incf i))
                 ((char= ch #\")
                  ;; Copy the string literal, escapes included.
                  (write-char ch out)
                  (incf i)
                  (loop while (< i n)
                        do (let ((c (char text i)))
                             (write-char c out)
                             (incf i)
                             (cond ((char= c #\\)
                                    (when (< i n)
                                      (write-char (char text i) out)
                                      (incf i)))
                                   ((char= c #\") (return))))))
                 (t (write-char ch out) (incf i)))))
    (get-output-stream-string out)))

(defun sml-basis-source-files (&optional (directory (sml-basis-directory)))
  "The files that DIRECTORY's all.sml `use's, in order."
  (let ((text (strip-sml-comments
               (uiop:read-file-string (merge-pathnames "all.sml" directory)
                                      :external-format *sml-external-format*)))
        (files nil))
    (loop with start = 0
          for use = (search "use \"" text :start2 start)
          while use
          do (let* ((name-start (+ use 5))
                    (name-end (position #\" text :start name-start)))
               (push (merge-pathnames (subseq text name-start name-end) directory)
                     files)
               (setf start name-end)))
    (nreverse files)))

(defun sml-basis-signature-expressions (package-name)
  (loop for key being the hash-keys of *sml-signature-expressions*
          using (hash-value sigexp)
        when (string= (first key) package-name)
          collect (cons (second key) sigexp)))

(defun restore-sml-basis-compile-time-state (scopes signatures)
  "Restore what compiling the basis left for the compilation of the programs
that see it: its identifier status and fixity environment, and its
signatures."
  (let ((package (ensure-sml-package *sml-basis-package-name*)))
    (setf (sml-package-parse-scopes package) scopes)
    (loop for (name . sigexp) in signatures
          do (setf (gethash (list (package-name package) name)
                            *sml-signature-expressions*)
                   sigexp))))

(defun compile-sml-basis (&optional (directory (sml-basis-directory)))
  "Compile and run the Basis Library sources in DIRECTORY in the basis
package.  Returns a form that recreates the basis in a fresh image."
  (let* ((package (ensure-sml-package *sml-basis-package-name*))
         (*sml-type-checker* nil)
         (*record-sml-lisp-source* nil)
         (hoisted-forms nil)
         (declaration-forms nil))
    (with-sml-cell-reservations
      (dolist (file (sml-basis-source-files directory))
        (let ((form (compile-sml-file file :package package))
              (*sml-package* package)
              (*sml-current-directory* directory))
          (eval form)
          (multiple-value-bind (hoisted program) (split-hoisted-sml-forms form)
            (setf hoisted-forms (append hoisted-forms hoisted))
            (push program declaration-forms)))))
    (let ((shadowed (sml-artifact-shadowed-names (append hoisted-forms declaration-forms)
                                                 package)))
      (unless (subsetp shadowed *sml-basis-cl-names* :test #'string=)
        (error "The basis binds names of COMMON-LISP symbols that ~
                *SML-BASIS-CL-NAMES* lacks: ~{~A~^, ~}"
               (set-difference shadowed *sml-basis-cl-names* :test #'string=))))
    `(progn
       ,@hoisted-forms
       ,@(mapcar (lambda (form)
                   `(let ((*sml-package* (ensure-sml-package ,*sml-basis-package-name*)))
                      ,form))
                 (nreverse declaration-forms))
       (restore-sml-basis-compile-time-state
        ',(sml-package-parse-scopes package)
        ',(sml-basis-signature-expressions (package-name package))))))

)

(defmacro define-sml-basis ()
  "The basis, compiled from its SML sources at macroexpansion time."
  (compile-sml-basis))

(define-sml-basis)
