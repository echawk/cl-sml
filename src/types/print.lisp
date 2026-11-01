(in-package #:cl-sml.types)

;;; Printing types and environments in HaMLet's format (HaMLet elab/PPType,
;;; PPStaticEnv), so that differential tests can compare the text.  HaMLet
;;; breaks lines at 79 columns; we print each binding on one line.  HaMLet
;;; also starts a basis with a comment listing its type names, which we
;;; leave out.

(defstruct (type-namer (:constructor make-type-namer ()))
  (names nil)                           ; alist TVAR -> name
  (count 0))

(defun type-var-name (index equality)
  "HaMLet TyVar.fromInt: 'a ... 'z, 'a1 ..."
  (multiple-value-bind (round letter) (floor index 26)
    (format nil "~:[~;'~]'~C~:[~;~:*~D~]"
            equality (code-char (+ (char-code #\a) letter))
            (and (plusp round) round))))

(defun name-scheme-vars (namer vars)
  (dolist (var vars namer)
    (push (cons var (type-var-name (type-namer-count namer) (tvar-equality var)))
          (type-namer-names namer))
    (incf (type-namer-count namer))))

(defun oclass-string (class)
  (format nil "[~{~A~^, ~}]"
          ;; The Basis's types are told apart by their structures' names.
          (mapcar #'tyname-instance-name
                  (cons (oclass-default class)
                        (sort (remove (oclass-default class) (copy-list (oclass-tynames class)))
                              #'< :key #'tyname-stamp)))))

(defun tvar-string (var namer)
  (cond ((cdr (assoc var (type-namer-names namer))))
        ((tvar-unknown var) "?")
        ((and (tvar-rigid var) (tvar-name var)))
        (t (let ((name (concatenate 'string
                                    (type-var-name (type-namer-count namer)
                                                   (tvar-equality var))
                                    (if (tvar-overload var)
                                        (oclass-string (tvar-overload var))
                                        ""))))
             (incf (type-namer-count namer))
             (push (cons var name) (type-namer-names namer))
             name))))

(defun tuple-fields-p (fields)
  "Are FIELDS labelled 1..n, n /= 1 (HaMLet PPType.isTuple)?"
  (and (/= (length fields) 1)
       (loop for field in fields
             for i from 1
             always (string= (car field) (princ-to-string i)))))

(defun write-type (type stream namer &optional (precedence 0))
  ;; Precedences: 0 top, 1 arrow, 2 star, 3 application, 4 atom.
  (let ((type (repr type)))
    (flet ((parens-at (p thunk)
             (if (> precedence p)
                 (progn (write-char #\( stream) (funcall thunk) (write-char #\) stream))
                 (funcall thunk))))
      (cond
        ((tvar-p type) (write-string (tvar-string type namer) stream))
        (t
         (ecase (car type)
           (:con
            (let ((args (cddr type)))
              (cond ((null args))
                    ((null (rest args))
                     (write-type (first args) stream namer 3)
                     (write-char #\Space stream))
                    (t (write-char #\( stream)
                       (loop for (arg . more) on args
                             do (write-type arg stream namer 0)
                                (when more (write-string ", " stream)))
                       (write-string ") " stream)))
              (write-string (tyname-name (second type)) stream)))
           (:arrow
            (parens-at 1 (lambda ()
                           (write-type (second type) stream namer 2)
                           (write-string " -> " stream)
                           (write-type (third type) stream namer 1))))
           (:record
            (multiple-value-bind (fields row) (expand-record type)
              (cond ((and (null row) (null fields)) (write-string "unit" stream))
                    ((and (null row) (tuple-fields-p fields))
                     (parens-at 2 (lambda ()
                                    (loop for (field . more) on fields
                                          do (write-type (cdr field) stream namer 3)
                                             (when more (write-string " * " stream))))))
                    (t (write-char #\{ stream)
                       (loop for (field . more) on fields
                             do (format stream "~A : " (car field))
                                (write-type (cdr field) stream namer 0)
                                (when more (write-string ", " stream)))
                       (when row
                         (write-string (if fields ", ..." "...") stream))
                       (write-char #\} stream)))))))))))

(defun type->string (type &optional (namer (make-type-namer)))
  "TYPE in SML syntax.  Variables are named in order of appearance, unless
NAMER already names them."
  (with-output-to-string (stream)
    (write-type type stream namer)))

(defun scheme->string (scheme)
  "SCHEME's type with its bound variables named 'a, 'b, ... (HaMLet
TypeScheme.normalise)."
  (type->string (scheme-type scheme)
                (name-scheme-vars (make-type-namer) (scheme-vars scheme))))

(defun tyvarseq-string (params)
  (cond ((null params) "")
        ((null (rest params)) (format nil "~A " (tvar-name (first params))))
        (t (format nil "(~{~A~^, ~}) " (mapcar #'tvar-name params)))))

(defun constructor-argument (valstr)
  "The argument type of constructor VALSTR, or NIL."
  (let ((type (repr (scheme-type (valstr-scheme valstr)))))
    (and (consp type) (eq (car type) :arrow) (second type))))

(defun abstract-tystr-p (tycon tystr tynames)
  "Is TYSTR, bound to TYCON, a type of the signature whose flexible type
names are TYNAMES, printed `type t' (HaMLet PPStaticEnv.absTy)?"
  (let ((tyname (tyfcn-tyname (tystr-tyfcn tystr))))
    (and tyname
         (null (tystr-ve tystr))
         (string= (tyname-name tyname) tycon)
         (member tyname tynames))))

(defun spaces (n)
  (make-string n :initial-element #\Space))

(defun write-env (env stream indent tynames)
  "ENV's bindings, as HaMLet prints them (PPStaticEnv.ppEnv'): structures,
abstract types, type abbreviations, datatypes, exceptions, values, each
group sorted by name, one per line indented by INDENT.  TYNAMES are the
flexible type names of the signature ENV belongs to."
  (let ((se (sort (env-latest (env-se env)) #'string< :key #'car))
        (te (sort (env-latest (env-te env)) #'string< :key #'car))
        (ve (sort (env-latest (env-ve env)) #'string< :key #'car)))
    (loop for (name . sub) in se
          do (format stream "~Astructure ~A :~%" (spaces indent) name)
             (write-sig sub stream (+ indent 2) tynames))
    (loop for (name . tystr) in te
          when (abstract-tystr-p name tystr tynames)
            do (format stream "~A~:[type~;eqtype~] ~A~A~%" (spaces indent)
                       (tyname-equality (tyfcn-tyname (tystr-tyfcn tystr)))
                       (tyvarseq-string (tyfcn-params (tystr-tyfcn tystr))) name))
    (loop for (name . tystr) in te
          when (and (null (tystr-ve tystr)) (not (abstract-tystr-p name tystr tynames)))
            do (format stream "~Atype ~A~A = ~A~%" (spaces indent)
                       (tyvarseq-string (tyfcn-params (tystr-tyfcn tystr))) name
                       (type->string (tyfcn-body (tystr-tyfcn tystr)))))
    (loop for (name . tystr) in te
          when (tystr-ve tystr)
            do (format stream "~Adatatype ~A~A = ~{~A~^ | ~}~%" (spaces indent)
                       (tyvarseq-string (tyfcn-params (tystr-tyfcn tystr))) name
                       (loop for (con . valstr) in (sort (copy-list (tystr-ve tystr))
                                                         #'string< :key #'car)
                             for arg = (constructor-argument valstr)
                             collect (if arg
                                         (format nil "~A of ~A" con (type->string arg))
                                         con))))
    (loop for (name . valstr) in ve
          when (eq (valstr-status valstr) :e)
            do (let ((arg (constructor-argument valstr)))
                 (format stream "~Aexception ~A~@[ of ~A~]~%" (spaces indent) name
                         (and arg (type->string arg)))))
    (loop for (name . valstr) in ve
          when (eq (valstr-status valstr) :v)
            do (format stream "~Aval ~A : ~A~%" (spaces indent) name
                       (scheme->string (valstr-scheme valstr))))))

(defun write-sig (env stream indent tynames)
  (format stream "~Asig~%" (spaces indent))
  (write-env env stream (+ indent 2) tynames)
  (format stream "~Aend~%" (spaces indent)))

(defun describe-env (env)
  "ENV's bindings, as HaMLet prints a basis (PPStaticBasis.ppBasis):
signatures, functors, then the environment.  HaMLet starts with a comment
listing the basis's type names, and puts one in each signature, which we
leave out."
  (with-output-to-string (out)
    (loop for (name . sig) in (sort (env-latest (env-g env)) #'string< :key #'car)
          do (format out "signature ~A =~%" name)
             (write-sig (sig-env sig) out 2 (sig-tynames sig)))
    (loop for (name . funsig) in (sort (env-latest (env-f env)) #'string< :key #'car)
          do (format out "functor ~A(Arg :~%" name)
             (write-sig (funsig-env funsig) out 4 (funsig-tynames funsig))
             (format out "  ) :~%")
             (write-sig (funsig-result-env funsig) out 4 (funsig-result-tynames funsig)))
    (write-env env out 0 nil)))
