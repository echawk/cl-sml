(in-package #:cl-sml.types)

;;; Static environments and contexts (Definition sections 4.2-4.3; HaMLet
;;; elab/StaticEnv, Context, IdStatus).
;;;
;;; An environment E = (TE, VE) of the Core (structure environments come
;;; with modules, milestone M2) is kept as two alists, innermost binding
;;; first, so that extending a context shares structure with it:
;;;   VE: vid -> VALSTR (type scheme, identifier status)
;;;   TE: tycon -> TYSTR (type function, constructor environment)
;;; A context C = (U, E) adds the explicit type variables in scope; its
;;; set of type names T is implicit in the type names' levels.

(defstruct (valstr (:constructor make-valstr (scheme status &optional (id (list :exn)))))
  scheme
  status                                ; :v, :c or :e
  ;; For an exception constructor, its identity (shared by `exception E = F').
  id)

(defstruct (tystr (:constructor make-tystr (tyfcn ve &optional (kind :type))))
  tyfcn
  ve                                    ; constructors, an alist vid -> VALSTR
  kind)                                 ; :type, :datatype or :abstype

(defstruct (env (:constructor make-env (&optional ve te)))
  ve
  te)

(defun env-plus (env1 env2)
  "ENV1 + ENV2: ENV2's bindings shadow ENV1's."
  (make-env (append (env-ve env2) (env-ve env1))
            (append (env-te env2) (env-te env1))))

(defun env-plus-ve (env ve)
  (make-env (append ve (env-ve env)) (env-te env)))

(defun env-plus-te (env te)
  (make-env (env-ve env) (append te (env-te env))))

(defstruct (context (:constructor make-context (env &optional u)))
  env
  ;; The explicit type variables in scope: alist name -> rigid TVAR.
  u)

(defun context-plus-env (context env)
  (make-context (env-plus (context-env context) env) (context-u context)))

(defun context-plus-ve (context ve)
  (make-context (env-plus-ve (context-env context) ve) (context-u context)))

(defun context-plus-te (context te)
  (make-context (env-plus-te (context-env context) te) (context-u context)))

(defun context-plus-u (context u)
  (make-context (context-env context) (append u (context-u context))))

(defun lookup-vid (context name)
  "The VALSTR of short identifier NAME, or NIL."
  (cdr (assoc name (env-ve (context-env context)) :test #'string=)))

(defun lookup-tycon (context name)
  (cdr (assoc name (env-te (context-env context)) :test #'string=)))

(defun lookup-tyvar (context name)
  (cdr (assoc name (context-u context) :test #'string=)))

(defun status-string (valstr)
  "The status in the encoding of static-facts.lisp: v, c, c1, e or e1."
  (let ((status (valstr-status valstr)))
    (if (eq status :v)
        "v"
        (format nil "~(~A~)~:[~;1~]" status
                (eq (car (repr (scheme-type (valstr-scheme valstr)))) :arrow)))))

(defun env-latest (alist)
  "The bindings of ALIST that are not shadowed, as an alist."
  (let ((seen (make-hash-table :test #'equal))
        (result nil))
    (dolist (entry alist (nreverse result))
      (unless (gethash (car entry) seen)
        (setf (gethash (car entry) seen) t)
        (push entry result)))))
