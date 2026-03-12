(asdf:defsystem #:cl-sml
  :description "A compiler for SML syntax into Common Lisp"
  :depends-on (#:esrap #:named-readtables #:trivia)
  :serial t
  :components ((:file "package")
               (:file "runtime")
               (:file "parser")
               (:file "compiler")
               (:file "reader")))
