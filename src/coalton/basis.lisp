(in-package #:cl-sml)

;;; The Basis Library in Coalton: src/basis's SML, checked by the native
;;; checker as one program and emitted through the backend into the package
;;; SML.COALTON-BASIS.  `use{b = NAME}' there is the run time's primitive
;;; NAME (CE-BASIS-PRIMITIVE); everything else is the Basis's own SML.  A
;;; declaration the backend or Coalton cannot handle is skipped and recorded
;;; with the reason (COALTON-BASIS-REPORT); the declarations that use it
;;; fail in turn.

(defparameter *coalton-basis-prefix*
  "val use : {b : string} -> 'a = fn _ => raise Fail \"primitive\"
datatype cl_object = CL_OBJECT
"
  "Declared before the Basis's sources: `use{b = NAME} : t' then has type t;
cl_object stands for the run time's objects (streams), which the Basis's
sources call int (*CE-BUILTIN-TYPES*).")

(defparameter *coalton-basis-patches*
  '(("TextIO" ("type instream   = int" . "type instream   = cl_object")
              ("type outstream  = int" . "type outstream  = cl_object"))
    ("BinIO" ("type instream   = int" . "type instream   = cl_object")
             ("type outstream  = int" . "type outstream  = cl_object")))
  "Changes to the Basis's sources for Coalton: the CL backend represents
streams as the sources say (int), while Coalton's IFix is a fixnum.
IntInf.int stays Int.int (its operations use int's), so IntInf values
outside the fixnum range do not work in Coalton.")

(defun coalton-basis-source ()
  (with-output-to-string (out)
    (write-string *coalton-basis-prefix* out)
    (dolist (file (sml-basis-source-files))
      (let ((text (uiop:read-file-string file :external-format *sml-external-format*)))
        (loop for (old . new) in (cdr (assoc (pathname-name file) *coalton-basis-patches*
                                             :test #'string=))
              do (let ((start (search old text)))
                   (assert start () "Coalton Basis patch not found: ~A" old)
                   (setf text (concatenate 'string (subseq text 0 start) new
                                           (subseq text (+ start (length old)))))))
        (when (string= (pathname-name file) "values")
          (let ((use (search "val use       = use : string -> unit;" text)))
            (when use (setf text (concatenate 'string (subseq text 0 use) ";")))))
        (write-string text out)
        (terpri out)))))

(defvar *coalton-basis-results* nil
  "((declaration status reason) ...) of the last Basis build.")

(defun coalton-declaration-paths (declaration)
  (case (first declaration)
    (:val (mapcar #'first (second declaration)))
    (:datatype (mapcar #'first (second declaration)))
    ((:exception :exception-alias) (list (second declaration)))
    (t nil)))

(defun coalton-condition-summary (condition)
  (let* ((text (remove #\Return (princ-to-string condition)))
         (lines (remove "" (mapcar (lambda (line) (string-trim " " line))
                                   (uiop:split-string text :separator '(#\Newline)))
                        :test #'string=)))
    (format nil "~{~A~^ | ~}" (subseq lines 0 (min 12 (length lines))))))

(defun build-coalton-basis ()
  "Emit the Basis to Coalton and load it.  Returns the registry."
  (let* ((declarations (sml-program-ir-for-coalton (coalton-basis-source)
                                                   "SML.COALTON-BASIS.IR"))
         (package (coalton-program-package "SML.COALTON-BASIS"))
         (registry (make-coalton-registry :package package))
         (results nil)
         (*coalton-basis* nil))
    (with-coalton-emission (:basis t)
      (let ((*ce-package* package))
        (dolist (declaration declarations)
          (unless (equal (coalton-declaration-paths declaration) '("use"))
            (let ((*ce-shapes* nil))
              (multiple-value-bind (forms condition)
                  (handler-case (values (ce-declaration declaration) nil)
                    (coalton-unsupported (c) (values nil c)))
                (cond
                  (condition (push (list declaration :unsupported
                                         (coalton-condition-summary condition))
                                   results))
                  (t
                   (handler-case
                       (progn
                         (ensure-coalton-shape-types *ce-shapes*)
                         (load-coalton-forms forms package)
                         (push (list declaration :ok nil) results)
                         (coalton-register-declaration registry declaration))
                     (error (c)
                       (push (list declaration :coalton-error (coalton-condition-summary c))
                             results))))))))))
      (setf (coalton-registry-types registry) *ce-type-paths*))
    (setf *coalton-basis-results* (nreverse results))
    (setf (coalton-registry-blocked registry)
          (loop for (declaration status reason) in *coalton-basis-results*
                unless (eq status :ok)
                  append (mapcar (lambda (path) (cons path reason))
                                 (coalton-declaration-paths declaration))))
    registry))

(defun coalton-register-declaration (registry declaration)
  "Record in REGISTRY what loaded DECLARATION bound."
  (case (first declaration)
    (:val
     (dolist (binding (second declaration))
       (destructuring-bind (path cell type) binding
         (declare (ignore type))
         (let ((symbol (gethash cell *ce-globals*))
               (worker (gethash cell *ce-workers*))
               (term (and (= (length (second declaration)) 1) (first (third declaration)))))
           (let ((primitive (and term (ce-primitive-use-p term))))
             (when primitive
               (setf (gethash path (coalton-registry-primitives registry)) primitive)))
           (when symbol (setf (gethash path (coalton-registry-values registry)) symbol))
           (when worker (setf (gethash path (coalton-registry-workers registry)) worker))))))
    ((:exception :exception-alias)
     (let* ((path (second declaration))
            (cell (fifth declaration)))
       (when cell
         (setf (gethash path (coalton-registry-values registry)) (gethash cell *ce-globals*)
               (gethash path (coalton-registry-exceptions registry))
               (gethash cell *ce-exn-tags*)))))))

(defun load-coalton-basis ()
  (setf *coalton-basis* (build-coalton-basis)))

(defun coalton-basis-report (&key (stream *standard-output*) (detail t))
  "Print which of the Basis's declarations the Coalton backend emitted and
which it did not, by structure."
  (let ((by-structure (make-hash-table :test #'equal))
        (order nil))
    (loop for (declaration status reason) in *coalton-basis-results*
          for paths = (coalton-declaration-paths declaration)
          for structure = (let* ((path (or (first paths) ""))
                                 (dot (position #\. path)))
                            (if dot (subseq path 0 dot) "(top level)"))
          do (unless (gethash structure by-structure) (push structure order))
             (push (list paths status reason) (gethash structure by-structure)))
    (dolist (structure (nreverse order))
      (let* ((entries (reverse (gethash structure by-structure)))
             (ok (count :ok entries :key #'second)))
        (format stream "~&~A: ~D/~D declarations~%" structure ok (length entries))
        (when detail
          (loop for (paths status reason) in entries
                unless (eq status :ok)
                  do (format stream "  ~{~A~^, ~}: ~(~A~): ~A~%" paths status
                             (subseq reason 0 (min 400 (length reason))))))))))

(export '(coalton-basis-report load-coalton-basis))
