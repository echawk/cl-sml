(in-package #:cl-sml)

;;; Static facts: what the static checker (HaMLet, see hamlet-check.sml)
;;; learned about a program, made available to the parser and code generator.
;;;
;;; The checker reports each fact at the (line, column) where the phrase it
;;; describes starts.  Our parser reads the same text, so facts are indexed by
;;; character offset, and the parser records the offset of the AST nodes that
;;; facts can be about (identifiers, operators, special constants).  A node
;;; without a recorded offset, or an offset without a fact, simply falls back
;;; to the heuristics used when no checker is installed.

(defstruct (sml-static-facts (:constructor %make-sml-static-facts))
  ;; offset -> list of (kind name info)
  (table (make-hash-table) :type hash-table)
  ;; structure name -> list of (status . member-name), for "str" facts
  (structures (make-hash-table :test #'equal) :type hash-table))

(defvar *sml-static-facts* nil
  "The SML-STATIC-FACTS for the text currently being parsed and compiled.")

(defvar *sml-static-facts-offset* 0
  "Offset of the parsed text within the text the checker saw.")

(defvar *sml-ast-positions* nil
  "When non-NIL, an EQ hash table from AST nodes to source offsets, filled in
by the parser.")

(defun sml-line-start-offsets (source)
  (let ((starts (list 0)))
    (loop for i from 0 below (length source)
          when (char= (char source i) #\Newline)
            do (push (1+ i) starts))
    (coerce (nreverse starts) 'vector)))

(defun sml-line-column->offset (source line-starts line column)
  "Map HaMLet's position (1-based LINE, 0-based COLUMN, tabs advancing to the
next multiple of 8) to a character offset into SOURCE."
  (when (<= 1 line (length line-starts))
    (let ((offset (aref line-starts (1- line)))
          (col 0))
      (loop while (and (< col column)
                       (< offset (length source))
                       (char/= (char source offset) #\Newline))
            do (setf col (if (char= (char source offset) #\Tab)
                             (+ col (- 8 (mod col 8)))
                             (1+ col)))
               (incf offset))
      (and (= col column) offset))))

(defun parse-sml-structure-members (text)
  (loop for entry in (uiop:split-string text :separator '(#\Space))
        for colon = (position #\: entry)
        when (and colon (plusp colon))
          collect (cons (subseq entry 0 colon) (subseq entry (1+ colon)))))

(defun make-sml-static-facts (source facts)
  "Build an SML-STATIC-FACTS from SOURCE and FACTS, a list of
(kind line column name info) as reported by the checker."
  (let ((result (%make-sml-static-facts))
        (line-starts (sml-line-start-offsets source)))
    (dolist (fact facts result)
      (destructuring-bind (kind line column name info) fact
        (let ((offset (sml-line-column->offset source line-starts line column)))
          (when offset
            (push (list kind name info)
                  (gethash offset (sml-static-facts-table result))))
          (when (string= kind "str")
            (setf (gethash name (sml-static-facts-structures result))
                  (parse-sml-structure-members info))))))))

(defmacro with-sml-static-facts ((facts &key (offset 0)) &body body)
  `(let ((*sml-static-facts* ,facts)
         (*sml-static-facts-offset* ,offset)
         (*sml-ast-positions* (make-hash-table :test #'eq)))
     ,@body))

(defmacro without-sml-static-facts (&body body)
  "Run BODY (e.g. a parse of text that is not the checked source) without
recording or consulting facts."
  `(let ((*sml-static-facts* nil)
         (*sml-ast-positions* nil))
     ,@body))

(defun note-sml-ast-position (node start)
  "Record that NODE starts at offset START of the parsed text.  Returns NODE."
  (when (and *sml-ast-positions* (consp node))
    (setf (gethash node *sml-ast-positions*) start))
  node)

(defun sml-ast-position (node)
  (and *sml-ast-positions* (consp node)
       (values (gethash node *sml-ast-positions*))))

(defun sml-static-facts-at (offset kind &optional name)
  "Facts of KIND (and NAME, if given) about the phrase at OFFSET of the
parsed text."
  (when (and *sml-static-facts* offset)
    (loop for fact in (gethash (+ offset *sml-static-facts-offset*)
                               (sml-static-facts-table *sml-static-facts*))
          when (and (string= (first fact) kind)
                    (or (null name) (string= (second fact) name)))
            collect fact)))

(defun sml-static-fact-info (offset kind &optional name)
  (third (first (sml-static-facts-at offset kind name))))

(defun sml-node-fact-info (node kind &optional name)
  "The info of the KIND fact about AST NODE, or NIL."
  (sml-static-fact-info (sml-ast-position node) kind
                        (or name (and (stringp (second node)) (second node)))))

;;; Identifier status: "v" (variable), "c"/"c1" (constructor without/with an
;;; argument), "e"/"e1" (exception constructor without/with an argument).

(defun sml-status-constructor-p (status)
  (and status (plusp (length status)) (find (char status 0) "ce")))

(defun sml-status-exception-p (status)
  (and status (plusp (length status)) (char= (char status 0) #\e)))

(defun sml-status-takes-argument-p (status)
  (and status (= (length status) 2)))

(defun sml-pattern-status-at (offset name)
  (sml-static-fact-info offset "pat" name))

(defun sml-expression-status-at (offset name)
  (sml-static-fact-info offset "id" name))

(defun sml-overloaded-instance (node)
  "The type name (\"int\", \"word\", \"word8\", \"real\", \"char\" or
\"string\") at which the overloaded identifier NODE is used, or NIL."
  (sml-node-fact-info node "ov"))

(defun sml-static-structure-members (name)
  "The value members (status . name) of structure NAME bound in the checked
text, or NIL when unknown."
  (and *sml-static-facts*
       (gethash name (sml-static-facts-structures *sml-static-facts*))))
