(in-package #:cl-sml.types)

;;; Elaboration of Modules (Definition sections 5.7, rules 50-89, and the
;;; derived forms of Appendix A; HaMLet elab/ElabModule).  A context here
;;; is a basis: its environment also carries the signature and functor
;;; environments (env.lisp).  The parser's module AST is summarised at the
;;; top of src/parser.lisp.
;;;
;;; The set T of type names of a basis is not kept: a type name is fresh
;;; when it is created, and the type names a signature binds are those
;;; created while elaborating it, which have a larger stamp than any type
;;; name of the context (rules 64, 65, 78 and 86 test membership in T this
;;; way).

(defun node-key (node key)
  "The value of KEY in the keyword part of module declaration NODE."
  (second (member key node)))

;;;; Structure expressions [rules 50-55]

(defun elab-strdecs (c decs)
  "[Rules 56-60] A structure-level declaration sequence.  Overloading is
resolved at the end of each declaration, so that a structure is complete
before it is matched against a signature."
  (let ((env (make-env)))
    (do-dec-groups (group decs (context-plus-env c env))
      (setf env (env-plus env (elab-dec (context-plus-env c env) group)))
      (let ((*position-fallback* (first group)))
        (resolve-overloading)))
    env))

(defun elab-strexp (c strexp)
  "The environment STREXP elaborates to."
  (let ((*position-fallback* (if (node-offset strexp) strexp *position-fallback*)))
    (when *functor-instance-checks*
      (dolist (template (gethash strexp *functor-instance-checks*))
        (check-functor-instance c template strexp)))
    (ecase (car strexp)
      (:struct                          ; [Rule 50]
       (env-clean (elab-strdecs c (second strexp))))
      (:str-id                          ; [Rule 51]
       (or (lookup-long-strid c (second strexp) strexp)
           (make-env)))
      (:str-constraint
       (let ((env (elab-strexp c (second strexp)))
             (sig (elab-sigexp c (third strexp))))
         ;; [Rule 52] transparent: the instance of the signature;
         ;; [Rule 53] opaque: the signature's own (fresh) type names.
         (let ((instance (match-sig env sig strexp)))
           (if (getf (cdddr strexp) :opaque)
               (values (adopt-exception-ids (sig-env sig) env))
               instance))))
      (:str-app (elab-functor-application c strexp (second strexp) (third strexp)))
      (:str-let                         ; [Rule 55]
       (let ((env (elab-strdecs c (second strexp))))
         (elab-strexp (context-plus-env c env) (third strexp)))))))

(defun lookup-funsig (c name node)
  (note-identifier c node name :funid)
  (or (cdr (assoc name (env-f (context-env c)) :test #'string=))
      (elab-error node "unknown functor ~A" name)))

(defun elab-functor-application (c node funid arg)
  "[Rule 54] funid ( strexp ): match the argument against the parameter
signature, then realise the result, with fresh names for the type names
the functor body generates."
  (let* ((funsig (lookup-funsig c funid node))
         (env (elab-strexp c arg)))
    (multiple-value-bind (instance phi ids)
        (match-sig env (make-sig (funsig-tynames funsig) (funsig-env funsig)) node)
      (declare (ignore instance))
      (let ((phi (renaming (funsig-result-tynames funsig) phi)))
        (renew-exceptions (realise-env (funsig-result-env funsig) phi)
                          ids (funsig-stamp funsig))))))

(defun renew-exceptions (env ids stamp)
  "ENV with the exceptions of the functor parameter (the keys of IDS) taken
from the argument, and those the functor body declared (identities after
STAMP) fresh: each application generates new exceptions (rules 31, 54)."
  (labels ((renew (id)
             (or (gethash id ids)
                 (if (and (consp id) (integerp (cdr id)) (> (cdr id) stamp))
                     (setf (gethash id ids) (new-exception-id))
                     id)))
           (walk (env)
             (make-env (loop for (vid . valstr) in (env-ve env)
                             collect (cons vid
                                           (if (eq (valstr-status valstr) :e)
                                               (make-valstr (valstr-scheme valstr) :e
                                                            (renew (valstr-id valstr)))
                                               valstr)))
                       (env-te env)
                       (loop for (strid . sub) in (env-se env)
                             collect (cons strid (walk sub)))
                       (env-g env)
                       (env-f env))))
    (walk env)))

;;;; Structure bindings [rules 57, 61]

(defun elab-strbinds (c group)
  "structure strid1 = strexp1 and ...: each strexp sees C only."
  (check-distinct-labels (first group) (mapcar #'second group))
  (make-env nil nil
            (loop for node in group
                  collect (cons (second node)
                                (let ((*position-fallback* node))
                                  (let ((env (elab-strexp c (node-key node :strexp))))
                                    ;; For the code generator: the members
                                    ;; the structure has after ascription.
                                    (annotate node :members env)
                                    env))))))

;;;; Signature expressions [rules 62-65]

(defun elab-sigexp (c sigexp)
  "[Rule 65] The signature (T)E of SIGEXP."
  (let* ((stamp *stamp*)
         (env (elab-sigexp-env c sigexp)))
    (make-sig (remove-if-not (lambda (tyname) (> (tyname-stamp tyname) stamp))
                             (env-tynames env))
              env)))

(defun lookup-sig (c name node)
  (note-identifier c node name :sigid)
  (or (cdr (assoc name (env-g (context-env c)) :test #'string=))
      (elab-error node "unknown signature ~A" name)))

(defun flexible-tyname (env longtycon node stamp what)
  "The type name LONGTYCON denotes in ENV, which must be flexible: created
after STAMP, that is, not in T of the context."
  (multiple-value-bind (strids last) (split-long-name longtycon)
    (let* ((sub (or (env-lookup-strids env strids)
                    (elab-error node "unknown structure in ~A" longtycon)))
           (tystr (or (cdr (assoc last (env-te sub) :test #'string=))
                      (elab-error node "unknown type ~A in ~A" longtycon what)))
           (tyname (or (tyfcn-tyname (tystr-tyfcn tystr))
                       (elab-error node "~A of non-flexible type ~A" what longtycon))))
      (unless (> (tyname-stamp tyname) stamp)
        (elab-error node "~A of rigid type ~A" what longtycon))
      tyname)))

(defun check-well-formed (env node)
  "A realised datatype must still be a type name (HaMLet
StaticEnv.isWellFormed)."
  (loop for (tycon . tystr) in (env-te env)
        when (and (tystr-ve tystr) (null (tyfcn-tyname (tystr-tyfcn tystr))))
          do (elab-error node "type realisation does not respect datatype ~A" tycon))
  (loop for (nil . sub) in (env-se env)
        do (check-well-formed sub node)))

(defun elab-sigexp-env (c sigexp)
  "The environment E of SIGEXP, whose flexible type names are fresh."
  (let ((*position-fallback* (if (node-offset sigexp) sigexp *position-fallback*)))
    (ecase (car sigexp)
      (:sig (elab-specs c (second sigexp)))  ; [Rule 62]
      (:sig-id                               ; [Rule 63]
       (sig-env (rename-sig (lookup-sig c (second sigexp) sigexp))))
      (:sig-where                            ; [Rule 64]
       (destructuring-bind (inner tyvars longtycon ty) (rest sigexp)
         (let* ((stamp *stamp*)
                (env (elab-sigexp-env c inner))
                (params (mapcar #'make-rigid-tvar tyvars))
                (type (elab-ty (make-context (context-env c) (mapcar #'cons tyvars params)) ty))
                (tyfcn (make-tyfcn params type))
                (tyname (flexible-tyname env longtycon sigexp stamp "where type")))
           (unless (= (tyname-arity tyname) (length params))
             (elab-error sigexp "where type ~A does not respect arity" longtycon))
           (when (and (tyname-equality tyname) (not (tyfcn-admits-equality-p tyfcn)))
             (elab-error sigexp "where type ~A does not respect equality" longtycon))
           (let ((phi (make-hash-table :test 'eq)))
             (setf (gethash tyname phi) tyfcn)
             (let ((result (realise-env env phi)))
               (check-well-formed result sigexp)
               result))))))))

;;;; Specifications [rules 68-84, Appendix A]

(defun spec-domain-clash (env1 env2)
  "A name both ENV1 and ENV2 specify, or NIL (side condition of rule 77)."
  (flet ((clash (alist1 alist2)
           (loop for (name) in alist2
                 when (assoc name alist1 :test #'string=) return name)))
    (or (clash (env-ve env1) (env-ve env2))
        (clash (env-te env1) (env-te env2))
        (clash (env-se env1) (env-se env2)))))

(defun spec-scheme-of (c ty)
  "[Rule 68, 79] The closed type scheme of a value description."
  (let* ((names (reverse (ty-tyvars ty)))
         (vars (mapcar #'make-rigid-tvar names))
         (type (elab-ty (make-context (context-env c) (mapcar #'cons names vars)) ty)))
    (make-scheme (remove-if-not (lambda (var) (member var vars))
                                (reverse (free-tvars type)))
                 type)))

(defun elab-typdesc (tyvars name equality)
  "[Rule 80] A fresh flexible type name."
  (let* ((params (mapcar #'make-rigid-tvar tyvars))
         (tyname (make-tyname name (length params) :equality equality :level *level*)))
    (cons name (make-tystr (make-tyfcn params (apply #'con-type tyname params)) nil :type))))

(defun elab-spec (c spec)
  "The environment of one specification, in context C."
  (ecase (car spec)
    (:spec-val                          ; [Rules 68, 79]
     (make-env (list (cons (second spec) (make-valstr (spec-scheme-of c (third spec)) :v)))))
    (:spec-type                         ; [Rules 69, 80; Appendix A]
     (destructuring-bind (tyvars name &optional ty) (rest spec)
       (if ty
           ;; type tyvarseq tycon = ty: `include sig type tycon end where
           ;; type tyvarseq tycon = ty' amounts to an abbreviation.
           (make-env nil (elab-typbinds c (list (list tyvars name ty))))
           (make-env nil (list (elab-typdesc tyvars name nil))))))
    (:spec-eqtype                       ; [Rule 70]
     (make-env nil (list (elab-typdesc (second spec) (third spec) t))))
    (:spec-datatype                     ; [Rules 71, 81, 82]
     (values (elab-datatype c (list :datatype nil nil :datbinds (second spec)))))
    (:spec-datatype-replication         ; [Rule 72]
     (let ((tystr (or (lookup-long-tycon c (third spec) spec)
                      (elab-error spec "cannot replicate unchecked type ~A" (third spec)))))
       (make-env (tystr-ve tystr)
                 (list (cons (second spec) tystr)))))
    (:spec-exception                    ; [Rules 73, 83]
     (let ((ty (third spec)))
       (make-env (list (cons (second spec)
                             (make-valstr (mono-scheme (if ty
                                                           (arrow-type (elab-ty c ty) (exn-type))
                                                           (exn-type)))
                                          :e (new-exception-id)))))))
    (:spec-structure                    ; [Rules 74, 84]
     (make-env nil nil (list (cons (second spec) (elab-sigexp-env c (third spec))))))
    (:spec-include                      ; [Rule 75]
     (elab-sigexp-env c (second spec)))))

(defun elab-specs (c specs)
  "[Rules 76-78] A specification sequence.  A sharing specification
applies to all the specifications before it."
  (let ((stamp *stamp*)
        (env (make-env)))
    (dolist (spec specs (env-clean env))
      (let ((*position-fallback* (if (node-offset spec) spec *position-fallback*)))
        (case (car spec)
          (:spec-sharing-type (setf env (share-types env (second spec) spec stamp)))
          (:spec-sharing (setf env (share-structures env (second spec) spec stamp)))
          (t (let* ((increment (elab-spec (context-plus-env c env) spec))
                    (clash (spec-domain-clash env increment)))
               (when clash
                 (elab-error spec "duplicate specification of ~A in signature" clash))
               (setf env (env-plus env increment)))))))))

(defun share-types (env longtycons node stamp)
  "[Rule 78] spec sharing type longtycon1 = ... = longtyconn."
  (let* ((tynames (mapcar (lambda (longtycon)
                            (flexible-tyname env longtycon node stamp "sharing"))
                          longtycons))
         (equality (some #'tyname-equality tynames))
         (target (find-if (lambda (tyname) (eq (and (tyname-equality tyname) t)
                                               (and equality t)))
                          tynames))
         (phi (make-hash-table :test 'eq)))
    (unless (every (lambda (tyname) (= (tyname-arity tyname) (tyname-arity target))) tynames)
      (elab-error node "type sharing does not respect arity"))
    (dolist (tyname tynames)
      (unless (eq tyname target)
        (setf (gethash tyname phi) (tyname-tyfcn target))))
    (realise-env env phi)))

(defun share-structures (env longstrids node stamp)
  "Appendix A: spec sharing longstrid1 = ... = longstridn shares every type
the structures have in common, pairwise (HaMLet ElabModule, SHARINGSpec)."
  (let ((phi (make-hash-table :test 'eq)))
    (labels ((current (tyfcn)
               (tyfcn-tyname (realise-tyfcn tyfcn phi)))
             (share-te (env1 env2)
               (loop for (tycon . tystr1) in (env-te env1)
                     for tystr2 = (cdr (assoc tycon (env-te env2) :test #'string=))
                     when tystr2
                       do (let ((t1 (current (tystr-tyfcn tystr1)))
                                (t2 (current (tystr-tyfcn tystr2))))
                            (unless (and t1 t2)
                              (elab-error node "structure sharing of non-flexible type ~A" tycon))
                            (unless (and (> (tyname-stamp t1) stamp) (> (tyname-stamp t2) stamp))
                              (elab-error node "structure sharing of rigid type ~A" tycon))
                            (unless (= (tyname-arity t1) (tyname-arity t2))
                              (elab-error node "sharing does not respect arity of type ~A" tycon))
                            (unless (eq t1 t2)
                              (let* ((fresh (make-tyname (tyname-name t1) (tyname-arity t1)
                                                         :equality (or (tyname-equality t1)
                                                                       (tyname-equality t2))
                                                         :level *level*))
                                     (tyfcn (tyname-tyfcn fresh)))
                                ;; Names already mapped to T1 or T2 follow.
                                (maphash (lambda (key value)
                                           (when (member (tyfcn-tyname value) (list t1 t2))
                                             (setf (gethash key phi) tyfcn)))
                                         phi)
                                (setf (gethash t1 phi) tyfcn
                                      (gethash t2 phi) tyfcn))))))
             (share-env (env1 env2)
               (share-te env1 env2)
               (loop for (strid . sub1) in (env-se env1)
                     for sub2 = (env-lookup-strid env2 strid)
                     when sub2 do (share-env sub1 sub2))))
      (let ((envs (mapcar (lambda (longstrid)
                            (multiple-value-bind (strids last) (split-long-name longstrid)
                              (or (env-lookup-strids env (append strids (list last)))
                                  (elab-error node "unknown structure ~A in sharing" longstrid))))
                          longstrids)))
        (loop for (env1 . more) on envs
              do (dolist (env2 more)
                   (share-env env1 env2))))
      (realise-env env phi))))

;;;; Signature declarations [rules 66-67]

(defun elab-sigbinds (c group)
  (check-distinct-labels (first group) (mapcar #'second group))
  (make-env nil nil nil
            (loop for node in group
                  collect (cons (second node)
                                (let ((*position-fallback* node))
                                  (elab-sigexp c (third node)))))))

;;;; Functor declarations [rules 85-86, Appendix A]

(defun elab-funbind (c node)
  "[Rule 86] functor funid (strid : sigexp) <: sigexp'> = strexp, and the
derived form funid (spec), whose specification's names the body sees as if
opened (Appendix A).  With defunctorisation, the functor signature also
carries the template its applications copy."
  (let* ((stamp *stamp*)
         (scan (and (defunctorizing-p) (make-functor-scan (context-env c))))
         (*functor-scan* scan)
         (param (node-key node :param))
         (param-sigexp (or (node-key node :param-sigexp)
                           (list :sig (node-key node :param-specs))))
         (sig (elab-sigexp c param-sigexp))
         (scanned-param (when scan
                          (setf (functor-scan-param scan) param
                                (functor-scan-param-env scan) (sig-env sig))))
         (body-context
           (if param
               (context-plus-env c (make-env nil nil (list (cons param (sig-env sig)))))
               (context-plus-env c (sig-env sig))))
         (result-sigexp (node-key node :result-sigexp))
         (body (if result-sigexp
                   (list :str-constraint (node-key node :strexp) result-sigexp
                         :opaque (node-key node :opaque))
                   (node-key node :strexp)))
         (result (env-clean (elab-strexp body-context body)))
         (generated (remove-if (lambda (tyname)
                                 (or (<= (tyname-stamp tyname) stamp)
                                     (member tyname (sig-tynames sig))))
                               (env-tynames result))))
    (declare (ignore scanned-param))
    (make-funsig (sig-tynames sig) (sig-env sig) generated result
                 (and scan (functor-template scan node param-sigexp))
                 stamp)))

(defun elab-funbinds (c group)
  (check-distinct-labels (first group) (mapcar #'second group))
  (make-env nil nil nil nil
            (loop for node in group
                  collect (cons (second node)
                                (let ((*position-fallback* node))
                                  (elab-funbind c node))))))
