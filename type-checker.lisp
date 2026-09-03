(in-package #:cl-sml)

(defstruct (hamlet-type-checker
            (:constructor %make-hamlet-type-checker (package argument)))
  package
  argument)

(defun configure-hamlet-basis-path (package basis-path)
  (with-sml-package (package)
    (let ((cell (sml-value "Sml.basisPath")))
      (setf (aref (ensure-sml-ref cell) 1)
            (sml-some-value (namestring (truename basis-path)))))))

(defun initialize-hamlet-type-checker (package)
  (with-sml-package (package)
    (let ((library-argument (call-sml "Sml.lib" (sml-unit))))
      (%make-hamlet-type-checker
       (package-name (ensure-sml-package package))
       (call-sml "Sml.elabArg" library-argument)))))

(defun make-hamlet-type-checker (&key
                                   (package "SML.HAMLET-TYPE-CHECKER")
                                   (hamlet-root #P"hamlet/")
                                   (load t))
  "Load HaMLet and return a stateful SML97 static elaboration session."
  (let* ((root (uiop:ensure-directory-pathname (truename hamlet-root)))
         (basis-source (merge-pathnames #P"basis/all.sml" root))
         (hamlet-source (merge-pathnames #P"hamlet.sml" root))
         (basis-path (merge-pathnames #P"basis/" root))
         (*sml-type-checker* nil))
    (when load
      (load-sml-file basis-source :package package)
      (load-sml-file hamlet-source :package package))
    (configure-hamlet-basis-path package basis-path)
    (initialize-hamlet-type-checker package)))

(defun hamlet-type-check-string (checker source &key filename)
  "Elaborate SOURCE with CHECKER and advance its static session on success."
  (let ((package (hamlet-type-checker-package checker)))
    (handler-case
        (with-sml-package (package)
          (let ((source-pair
                  (list :tuple
                        (if filename
                            (sml-some-value filename)
                            (sml-none-value))
                        source)))
            (setf (hamlet-type-checker-argument checker)
                  (call-sml "Sml.elab"
                            (hamlet-type-checker-argument checker)
                            source-pair))))
      (sml-raised-exception (cause)
        (error 'sml-static-type-error
               :source source
               :filename filename
               :cause cause)))))

(defmethod type-check-sml-string-using ((checker hamlet-type-checker) source
                                        &key filename)
  (hamlet-type-check-string checker source :filename filename))
