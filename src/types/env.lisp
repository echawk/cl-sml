(in-package #:cl-sml.types)

;;; Static environments and contexts (Definition sections 4.2-4.3; HaMLet
;;; elab/StaticEnv, Context, IdStatus).
;;;
;;; An environment E = (SE, TE, VE) is kept as alists, innermost binding
;;; first, so that extending a context shares structure with it:
;;;   VE: vid -> VALSTR (type scheme, identifier status)
;;;   TE: tycon -> TYSTR (type function, constructor environment)
;;;   SE: strid -> ENV (Section 5.1)
;;; The same structure also carries the signature and functor environments
;;; G and F of a basis B = (T, F, G, E) (Section 5.1; HaMLet StaticBasis),
;;; which are empty except at top level, so that a context can be a basis.
;;; A context C = (U, E) adds the explicit type variables in scope; its
;;; set of type names T is implicit in the type names' levels and stamps.

(defstruct (valstr (:constructor make-valstr (scheme status &optional (id (list :exn)))))
  scheme
  status                                ; :v, :c or :e
  ;; For an exception constructor, its identity (shared by `exception E = F').
  id)

(defstruct (tystr (:constructor make-tystr (tyfcn ve &optional (kind :type))))
  tyfcn
  ve                                    ; constructors, an alist vid -> VALSTR
  kind)                                 ; :type, :datatype or :abstype

(defstruct (env (:constructor make-env (&optional ve te se g f)))
  ve
  te
  se                                    ; strid -> ENV
  g                                     ; sigid -> SIG (top level only)
  f)                                    ; funid -> FUNSIG (top level only)

(defun env-plus (env1 env2)
  "ENV1 + ENV2: ENV2's bindings shadow ENV1's."
  (make-env (append (env-ve env2) (env-ve env1))
            (append (env-te env2) (env-te env1))
            (append (env-se env2) (env-se env1))
            (append (env-g env2) (env-g env1))
            (append (env-f env2) (env-f env1))))

(defun env-plus-ve (env ve)
  (make-env (append ve (env-ve env)) (env-te env) (env-se env) (env-g env) (env-f env)))

(defun env-plus-te (env te)
  (make-env (env-ve env) (append te (env-te env)) (env-se env) (env-g env) (env-f env)))

(defun env-clean (env)
  "ENV without its shadowed bindings."
  (make-env (env-latest (env-ve env)) (env-latest (env-te env)) (env-latest (env-se env))
            (env-latest (env-g env)) (env-latest (env-f env))))

;;;; Signatures and functor signatures [Section 5.1]

(defstruct (sig (:constructor make-sig (tynames env)))
  tynames                               ; T, the bound (flexible) type names
  env)

(defstruct (funsig (:constructor make-funsig (tynames env result-tynames result-env)))
  tynames env                           ; (T)E, the parameter signature
  result-tynames result-env)            ; (T')E', the result

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

(defun split-long-name (name)
  "The structure identifiers and the last identifier of long identifier
NAME: \"A.B.x\" gives (\"A\" \"B\") and \"x\".  Symbolic identifiers do not
contain dots, so `Int.+' and `Word8.~>>' split at the dot before them."
  (let ((strids nil)
        (start 0))
    (loop for dot = (position #\. name :start start)
          while (and dot (> dot start) (alpha-char-p (char name start))
                     (< (1+ dot) (length name)))
          do (push (subseq name start dot) strids)
             (setf start (1+ dot)))
    (values (nreverse strids) (subseq name start))))

(defun env-lookup-strid (env name)
  (cdr (assoc name (env-se env) :test #'string=)))

(defun env-lookup-strids (env strids)
  "The environment of the structure at path STRIDS from ENV, or NIL; the
second value is the first missing structure identifier."
  (dolist (strid strids (values env nil))
    (setf env (env-lookup-strid env strid))
    (unless env
      (return (values nil strid)))))

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
