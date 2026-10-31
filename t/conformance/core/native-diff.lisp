;;; Differential check of the native type checker (src/types/) against
;;; HaMLet on the Core conformance corpus: accept/reject, and for programs
;;; both accept, the printed type of each top-level binding.
;;;
;;; Load cl-sml first, then this file, then call (native-diff:main).  HaMLet
;;; needs a deep control stack (sbcl --control-stack-size 1GB).  Long
;;; identifiers (Basis structures) are elaborated as unchecked, so bindings
;;; whose type depends on one print with `?' and are not compared.  Types
;;; that differ only in the names of their type variables count as renamed.

(defpackage #:native-diff
  (:use #:cl)
  (:export #:main))

(in-package #:native-diff)

(defparameter *here*
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *default-pathname-defaults*)))

(defun description-items (text)
  "The bindings of a printed basis as an alist (\"kind name\" . text), the
last binding of a name winning, with continuation lines joined, runs of
white space collapsed and `(* ... *)' comment lines dropped."
  (let ((items nil))
    (dolist (line (uiop:split-string text :separator '(#\Newline)))
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (cond ((string= trimmed ""))
              ((and (> (length trimmed) 1) (string= "(*" trimmed :end2 2)))
              ((and items (member (char line 0) '(#\Space #\Tab)))
               (setf (cdr (first items))
                     (concatenate 'string (cdr (first items)) " " trimmed)))
              (t (push (cons nil trimmed) items)))))
    (let ((result nil))
      (dolist (item (reverse items))
        (let* ((text (format nil "~{~A~^ ~}"
                             (remove "" (uiop:split-string (cdr item) :separator '(#\Space))
                                     :test #'string=)))
               (words (uiop:split-string text :separator '(#\Space)))
               (key (format nil "~A ~A" (first words)
                            ;; `type 'a t = ...', `datatype ('a, 'b) t = ...'
                            (let ((rest (rest words)))
                              (loop while (and rest (or (char= (char (first rest) 0) #\')
                                                        (char= (char (first rest) 0) #\()))
                                    do (pop rest)
                                    finally (return (string-right-trim ":" (or (first rest) ""))))))))
          (setf result (remove key result :key #'car :test #'string=))
          (push (cons key text) result)))
      (nreverse result))))

(defun canonical-tyvars (text)
  "TEXT with its type variables renamed 'a, 'b, ... in order of first
occurrence: the Definition does not fix the names, and HaMLet's depend on
the order its unifier links variables."
  (let ((names nil))
    (with-output-to-string (out)
      (loop with i = 0
            while (< i (length text))
            do (if (char= (char text i) #\')
                   (let* ((end (or (position-if-not (lambda (ch)
                                                      (or (alphanumericp ch) (member ch '(#\' #\_))))
                                                    text :start i)
                                   (length text)))
                          (name (subseq text i end))
                          (equality (and (> (length name) 1) (char= (char name 1) #\')))
                          (entry (or (assoc name names :test #'string=)
                                     (first (push (cons name (format nil "~:[~;'~]'v~D"
                                                                     equality (length names)))
                                                  names)))))
                     (write-string (cdr entry) out)
                     (setf i end))
                   (progn (write-char (char text i) out) (incf i)))))))

(defun hamlet-check (checker source)
  (clrhash (cl-sml::hamlet-type-checker-sessions checker))
  (handler-case (values (cl-sml:hamlet-type-check-string checker source) nil)
    (error (condition) (values nil (princ-to-string condition)))))

(defun native-check (source)
  (handler-case
      (handler-bind ((warning #'muffle-warning))
        (let ((cl-sml:*native-unchecked-long-ids* t))
          (values (cl-sml:typecheck-sml-string source) nil)))
    (error (condition) (values nil (princ-to-string condition)))))

(defun one-line (text &optional (max 200))
  (let ((text (substitute #\Space #\Newline text)))
    (subseq text 0 (min max (length text)))))

(defun main (&key (names nil) (verbose t) (directory (merge-pathnames "tests/" *here*)))
  "Compare the checkers on the .sml files of DIRECTORY (by default this
corpus; HaMLet's own hamlet/test/ is another)."
  (let* ((checker (cl-sml:make-hamlet-type-checker))
         (files (sort (directory (merge-pathnames "*.sml" directory))
                      #'string< :key #'pathname-name))
         (counts (list :both-accept 0 :both-reject 0 :only-hamlet-accepts 0
                       :only-native-accepts 0 :types-agree 0 :types-differ 0
                       :bindings-compared 0 :bindings-renamed 0 :bindings-differ 0
                       :bindings-unknown 0)))
    (dolist (file files)
      (let ((name (pathname-name file)))
        (when (or (null names) (member name names :test #'string=))
          (let ((source (uiop:read-file-string file)))
            (multiple-value-bind (hamlet hamlet-error) (hamlet-check checker source)
              (multiple-value-bind (native native-error) (native-check source)
                (cond
                  ((and hamlet native)
                   (incf (getf counts :both-accept))
                   (let ((expected (description-items hamlet))
                         (differ nil))
                     (loop for (key . text) in (description-items native)
                           for other = (cdr (assoc key expected :test #'string=))
                           do (cond ((find #\? text) (incf (getf counts :bindings-unknown)))
                                    (t (incf (getf counts :bindings-compared))
                                       (cond ((equal text other))
                                             ((and other (equal (canonical-tyvars text)
                                                                (canonical-tyvars other)))
                                              (incf (getf counts :bindings-renamed)))
                                             (t (incf (getf counts :bindings-differ))
                                                (push (list text other) differ))))))
                     (if differ
                         (progn (incf (getf counts :types-differ))
                                (when verbose
                                  (format t "~&TYPES ~A~%~:{  native: ~A~%  hamlet: ~A~%~}"
                                          name differ)))
                         (incf (getf counts :types-agree)))))
                  ((and (not hamlet) (not native)) (incf (getf counts :both-reject)))
                  (hamlet
                   (incf (getf counts :only-hamlet-accepts))
                   (when verbose
                     (format t "~&NATIVE REJECTS ~A: ~A~%" name (one-line native-error))))
                  (t
                   (incf (getf counts :only-native-accepts))
                   (when verbose
                     (format t "~&HAMLET REJECTS ~A: ~A~%" name (one-line hamlet-error)))))))))))
    (format t "~&~{~(~A~): ~D~%~}" counts)
    counts))
