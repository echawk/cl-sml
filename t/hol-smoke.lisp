(load "~/.sbclrc")
(ql:quickload :cl-sml)

(defparameter *hol-smoke-package* "SML.HOL-SMOKE")
(defparameter *hol-smoke-sources*
  '("HOL/src/portableML/quotation_dtype.sml"
    "HOL/src/portableML/monads/optmonad.sml"
    "HOL/src/prekernel/Type_dtype.sml"
    "HOL/src/0/KernelTypes.sml"))

(unless (probe-file #P"HOL/README.md")
  (error "HOL checkout is not available at ./HOL; run `git submodule update --init HOL`."))

(dolist (source *hol-smoke-sources*)
  (format t "[HOL smoke] loading ~A~%" source)
  (cl-sml:load-sml-file source :package *hol-smoke-package*))

(dolist (name '("quotation_dtype.QUOTE"
                "optmonad.return"
                "Type_dtype.Tyv"
                "KernelTypes.Bv"))
  (unless (functionp (cl-sml:sml-value name *hol-smoke-package*))
    (error "HOL smoke value ~A is not callable" name)))

(let* ((return (cl-sml:sml-function "optmonad.return" *hol-smoke-package*))
       (result (funcall (funcall return 42) "state")))
  (unless (and (consp result)
               (equal (cdr result) (list :tuple "state" 42)))
    (error "Unexpected optmonad.return result: ~S" result)))

(format t "HOL portable smoke passed (~D source files).~%"
        (length *hol-smoke-sources*))
