;; For each Basis value in vals.txt: is it in HaMLet's static basis, and is it
;; bound at run time after loading hamlet/basis/all.sml with cl-sml?
(in-package :cl-user)
(defvar *dir* (uiop:pathname-directory-pathname *load-truename*))
(let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream)))
  (cl-sml:enable-hamlet-type-checker)
  (let ((cl-sml:*sml-type-checker* nil))
    (cl-sml:load-sml-file (asdf:system-relative-pathname "cl-sml" "hamlet/basis/all.sml") :package "INV")))
(with-open-file (out (merge-pathnames "inventory.tsv" *dir*) :direction :output :if-exists :supersede)
  (dolist (name (uiop:read-file-lines (merge-pathnames "vals.txt" *dir*)))
    (let* ((static (handler-case
                       (let ((cl-sml::*sml-package* (cl-sml::ensure-sml-package "INV")))
                         (let ((*error-output* (make-broadcast-stream)))
                           (cl-sml:type-check-sml-string (format nil "val _ = op ~A;" name)))
                         "hamlet")
                     (error () "-")))
           (runtime (if (boundp (cl-sml::sml-symbol name "INV")) "bound" "-")))
      (format out "~A	~A	~A~%" name static runtime))))
