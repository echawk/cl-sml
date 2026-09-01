(defpackage #:cl-sml
  (:use #:cl #:esrap #:named-readtables #:trivia)
  (:export #:sml-readtable
           #:sml-adt
           #:make-sml-adt
           #:compile-sml-expression-string
           #:compile-sml-declarations-string
           #:compile-sml-program-string
           #:compile-sml-file
           #:emit-sml-file
           #:load-sml-file
           #:sml-value
           #:sml-function
           #:with-sml-package
           #:call-sml
           #:lookup-sml-binding-type
           #:sml-type->string
           #:*sml-type-checker*
           #:type-check-sml-string
           #:with-sml-type-checker
           #:sml-static-type-error
           #:make-hamlet-type-checker
           #:hamlet-type-check-string
           #:repl))
