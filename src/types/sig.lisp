(in-package #:cl-sml.types)

;;; Realisations, signatures and signature matching (Definition sections
;;; 5.2-5.6; HaMLet elab/Sig, FunSig, StaticEnv.realise and enriches,
;;; TypeFcn).
;;;
;;; A realisation phi is an EQ hash table from type names to type
;;; functions.  Realising copies: types are lists, so the original objects
;;; are never changed.  The type names of a signature (T)E are the flexible
;;; ones it binds; renaming (Sig.rename) replaces them with fresh ones,
;;; which is how each functor application and each opaque ascription gets
;;; new types.

;;;; Type names in objects

(defun tyname-tyfcn (tyname)
  "The type function eta-equivalent to TYNAME."
  (let ((params (loop for i below (tyname-arity tyname)
                      collect (make-rigid-tvar (type-var-name i nil) :level 0))))
    (make-tyfcn params (apply #'con-type tyname params))))

(defun scheme-tynames (scheme acc)
  (type-tynames (scheme-type scheme) acc))

(defun env-tynames (env &optional acc)
  "The type names occurring in ENV (HaMLet StaticEnv.tynames)."
  (loop for (nil . valstr) in (env-ve env)
        do (setf acc (scheme-tynames (valstr-scheme valstr) acc)))
  (loop for (nil . tystr) in (env-te env)
        do (setf acc (type-tynames (tyfcn-body (tystr-tyfcn tystr)) acc))
           (loop for (nil . valstr) in (tystr-ve tystr)
                 do (setf acc (scheme-tynames (valstr-scheme valstr) acc))))
  (loop for (nil . sub) in (env-se env)
        do (setf acc (env-tynames sub acc)))
  acc)

;;;; Realisation [Section 5.2]

(defun realise-type (type phi)
  (let ((type (repr type)))
    (cond ((tvar-p type) type)
          (t (ecase (car type)
               (:con (let ((args (mapcar (lambda (arg) (realise-type arg phi)) (cddr type)))
                           (tyfcn (gethash (second type) phi)))
                       (if tyfcn
                           (apply-tyfcn tyfcn args)
                           (list* :con (second type) args))))
               (:arrow (list :arrow (realise-type (second type) phi)
                             (realise-type (third type) phi)))
               (:record (multiple-value-bind (fields row) (expand-record type)
                          (list :record
                                (mapcar (lambda (field)
                                          (cons (car field) (realise-type (cdr field) phi)))
                                        fields)
                                row))))))))

(defun realise-tyfcn (tyfcn phi)
  (make-tyfcn (tyfcn-params tyfcn) (realise-type (tyfcn-body tyfcn) phi)))

(defun realise-valstr (valstr phi)
  (let ((scheme (valstr-scheme valstr)))
    (make-valstr (make-scheme (scheme-vars scheme) (realise-type (scheme-type scheme) phi))
                 (valstr-status valstr) (valstr-id valstr))))

(defun realise-ve (ve phi)
  (loop for (name . valstr) in ve
        collect (cons name (realise-valstr valstr phi))))

(defun realise-env (env phi)
  "phi(ENV), sharing nothing that phi changes."
  (if (zerop (hash-table-count phi))
      env
      (make-env (realise-ve (env-ve env) phi)
                (loop for (name . tystr) in (env-te env)
                      collect (cons name (make-tystr (realise-tyfcn (tystr-tyfcn tystr) phi)
                                                     (realise-ve (tystr-ve tystr) phi)
                                                     (tystr-kind tystr))))
                (loop for (name . sub) in (env-se env)
                      collect (cons name (realise-env sub phi))))))

;;;; Renaming

(defun rename-tyname (tyname)
  (make-tyname (tyname-name tyname) (tyname-arity tyname)
               :equality (tyname-equality tyname)
               :constructors (tyname-constructors tyname)
               :level *level*))

(defun renaming (tynames &optional (phi (make-hash-table :test 'eq)))
  "PHI extended to map each of TYNAMES to a fresh type name; returns PHI and
the fresh names."
  (values phi
          (loop for tyname in tynames
                collect (let ((fresh (rename-tyname tyname)))
                          (setf (gethash tyname phi) (tyname-tyfcn fresh))
                          fresh))))

(defun rename-sig (sig)
  "An alpha-variant of SIG with fresh type names (HaMLet Sig.rename)."
  (multiple-value-bind (phi fresh) (renaming (sig-tynames sig))
    (make-sig fresh (realise-env (sig-env sig) phi))))

;;;; Equality of types and type functions

(defun type-equal-p (type1 type2)
  (let ((type1 (repr type1))
        (type2 (repr type2)))
    (cond ((or (tvar-p type1) (tvar-p type2)) (eq type1 type2))
          ((not (eq (car type1) (car type2))) nil)
          (t (ecase (car type1)
               (:con (and (eq (second type1) (second type2))
                          (every #'type-equal-p (cddr type1) (cddr type2))))
               (:arrow (and (type-equal-p (second type1) (second type2))
                            (type-equal-p (third type1) (third type2))))
               (:record
                (multiple-value-bind (fields1 row1) (expand-record type1)
                  (multiple-value-bind (fields2 row2) (expand-record type2)
                    (and (eq row1 row2)
                         (= (length fields1) (length fields2))
                         (every (lambda (field1 field2)
                                  (and (string= (car field1) (car field2))
                                       (type-equal-p (cdr field1) (cdr field2))))
                                fields1 fields2))))))))))

(defun tyfcn-equal-p (tyfcn1 tyfcn2)
  (and (= (tyfcn-arity tyfcn1) (tyfcn-arity tyfcn2))
       (let ((args (loop repeat (tyfcn-arity tyfcn1) collect (make-rigid-tvar "'x"))))
         (type-equal-p (apply-tyfcn tyfcn1 args) (apply-tyfcn tyfcn2 args)))))

(defun tyfcn-admits-equality-p (tyfcn)
  "Does TYFCN admit equality (Section 4.4): does its body, assuming that
its parameters do?"
  (datatype-admits-equality-p (tyfcn-body tyfcn)))

;;;; Matching [Section 5.6]

(defun sig-mismatch (node path control &rest arguments)
  (elab-error node "~:[~;~:*structure ~{~A~^.~}: ~]~?" (reverse path) control arguments))

(defun match-realisation (env sig-env tynames phi node path)
  "Extend PHI so that it realises the flexible TYNAMES of SIG-ENV by what
ENV has at the same places (HaMLet Sig.matchE)."
  (loop for (tycon . sig-tystr) in (env-latest (env-te sig-env))
        do (let ((tystr (cdr (assoc tycon (env-te env) :test #'string=))))
             (unless tystr
               (sig-mismatch node path "type ~A is specified but not declared" tycon))
             (let ((sig-tyfcn (tystr-tyfcn sig-tystr))
                   (tyfcn (tystr-tyfcn tystr)))
               (unless (= (tyfcn-arity sig-tyfcn) (tyfcn-arity tyfcn))
                 (sig-mismatch node path "type ~A takes ~D argument~:P, the signature says ~D"
                           tycon (tyfcn-arity tyfcn) (tyfcn-arity sig-tyfcn)))
               (let ((tyname (tyfcn-tyname sig-tyfcn)))
                 (when (and tyname (member tyname tynames) (not (gethash tyname phi)))
                   (when (and (tyname-equality tyname)
                              (not (tyfcn-admits-equality-p tyfcn)))
                     (sig-mismatch node path "type ~A does not admit equality, the signature says ~
                                          eqtype" tycon))
                   (setf (gethash tyname phi) tyfcn))))))
  (loop for (strid . sig-sub) in (env-latest (env-se sig-env))
        do (let ((sub (env-lookup-strid env strid)))
             (unless sub
               (sig-mismatch node path "structure ~A is specified but not declared" strid))
             (match-realisation sub sig-sub tynames phi node (cons strid path))))
  phi)

(defun scheme-generalises-p (scheme spec-scheme)
  "Is SPEC-SCHEME (whose bound variables are rigid) an instance of SCHEME?
A free (undetermined) type variable of SCHEME may be determined by the
match, but not to a type variable of SPEC-SCHEME: that would make a
monomorphic value polymorphic (HaMLet test undetermined3)."
  (let ((free (scheme-free-tvars scheme)))
    (handler-case
        (progn (unify (instantiate scheme) (scheme-type spec-scheme))
               (notany (lambda (var)
                         (intersection (free-tvars var) (scheme-vars spec-scheme)))
                       free))
      (unify-failure () nil))))

(defun status-name (status)
  (ecase status (:v "value") (:c "constructor") (:e "exception")))

(defun check-enrichment (env sig-env node path)
  "Signal an error unless ENV enriches SIG-ENV (Section 5.5)."
  (loop for (strid . sig-sub) in (env-latest (env-se sig-env))
        do (check-enrichment (env-lookup-strid env strid) sig-sub node (cons strid path)))
  (loop for (tycon . sig-tystr) in (env-latest (env-te sig-env))
        do (let ((tystr (cdr (assoc tycon (env-te env) :test #'string=))))
             (unless (tyfcn-equal-p (tystr-tyfcn tystr) (tystr-tyfcn sig-tystr))
               (sig-mismatch node path "type ~A is ~A, the signature says ~A" tycon
                         (type->string (tyfcn-body (tystr-tyfcn tystr)))
                         (type->string (tyfcn-body (tystr-tyfcn sig-tystr)))))
             (when (tystr-ve sig-tystr)
               (unless (and (= (length (tystr-ve tystr)) (length (tystr-ve sig-tystr)))
                            (every (lambda (entry)
                                     (assoc (car entry) (tystr-ve tystr) :test #'string=))
                                   (tystr-ve sig-tystr)))
                 (sig-mismatch node path "datatype ~A does not have the constructors the ~
                                      signature specifies" tycon)))))
  (loop for (vid . sig-valstr) in (env-latest (env-ve sig-env))
        do (let ((valstr (cdr (assoc vid (env-ve env) :test #'string=))))
             (unless valstr
               (sig-mismatch node path "~A ~A is specified but not declared"
                         (status-name (valstr-status sig-valstr)) vid))
             (unless (or (eq (valstr-status sig-valstr) :v)
                         (eq (valstr-status sig-valstr) (valstr-status valstr)))
               (sig-mismatch node path "~A is a ~A, the signature says ~A" vid
                         (status-name (valstr-status valstr))
                         (status-name (valstr-status sig-valstr))))
             (unless (scheme-generalises-p (valstr-scheme valstr) (valstr-scheme sig-valstr))
               (sig-mismatch node path "~A has type ~A, the signature says ~A" vid
                         (scheme->string (valstr-scheme valstr))
                         (scheme->string (valstr-scheme sig-valstr)))))))

(defun adopt-exception-ids (sig-env env &optional (ids (make-hash-table :test 'equal)))
  "SIG-ENV with each exception taking the identity of the exception of the
same name in ENV, which matches it; IDS maps each replaced identity to its
replacement.  An exception is generative at its declaration, not at a
specification (Definition rules 31 and 73): two structures matched
against one signature keep their exceptions apart."
  (values
   (make-env (loop for (vid . valstr) in (env-ve sig-env)
                   collect (let ((actual (cdr (assoc vid (env-ve env) :test #'string=))))
                             (cons vid
                                   (if (and actual
                                            (eq (valstr-status valstr) :e)
                                            (eq (valstr-status actual) :e))
                                       (progn (setf (gethash (valstr-id valstr) ids)
                                                    (valstr-id actual))
                                              (make-valstr (valstr-scheme valstr) :e
                                                           (valstr-id actual)))
                                       valstr))))
             (env-te sig-env)
             (loop for (strid . sub) in (env-se sig-env)
                   collect (cons strid
                                 (let ((actual (env-lookup-strid env strid)))
                                   (if actual
                                       (adopt-exception-ids sub actual ids)
                                       sub))))
             (env-g sig-env)
             (env-f sig-env))
   ids))

(defun match-sig (env sig node)
  "Match ENV against signature SIG (HaMLet Sig.match): returns phi of SIG's
environment, which ENV enriches, with ENV's exceptions (ADOPT-EXCEPTION-IDS),
the realisation phi, and the map from SIG's exception identities to ENV's."
  (let* ((phi (match-realisation env (sig-env sig) (sig-tynames sig)
                                 (make-hash-table :test 'eq) node nil))
         (instance (realise-env (sig-env sig) phi)))
    (check-enrichment env instance node nil)
    (multiple-value-bind (instance ids) (adopt-exception-ids instance env)
      (values instance phi ids))))
