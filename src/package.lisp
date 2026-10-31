(defpackage #:cl-sml
  (:use #:cl #:named-readtables #:trivia)
  (:export #:sml-readtable
           #:enable-sml-syntax
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
           #:sml-syntax-error
           #:parse-sml-program
           #:parse-sml-expression
           #:make-hamlet-type-checker
           #:hamlet-type-check-string
           #:hamlet-type-checker-last-description
           #:enable-hamlet-type-checker
           #:disable-type-checker
           #:*record-sml-lisp-source*
           #:*show-lisp-compiler-output*
           #:sml-lisp-source
           #:show-sml-lisp
           #:print-sml-lisp
           #:clear-sml-lisp-source
           #:sml->lisp
           #:with-sml-phase-timings
           #:repl
           #:main
           ;; The native type checker (src/types/)
           #:typecheck-sml-string
           #:typecheck-sml-file
           #:sml-native-type-error
           #:sml-native-type-error-message
           #:sml-native-type-error-line
           #:sml-native-type-error-column
           #:sml-native-type-error-types
           #:sml-match-warning
           #:*native-unchecked-long-ids*
           #:*native-relaxed-value-restriction*
           #:*native-match-warnings*))

;;; The native SML '97 static semantics (DESIGN-TYPECHECKER.md), milestone
;;; M1: the Core language against a hand-written initial environment.
(defpackage #:cl-sml.types
  (:use #:cl)
  (:import-from #:cl-sml
                #:parse-sml-program
                #:sml-static-type-error
                #:typecheck-sml-string
                #:typecheck-sml-file
                #:sml-native-type-error
                #:sml-native-type-error-message
                #:sml-native-type-error-line
                #:sml-native-type-error-column
                #:sml-native-type-error-types
                #:sml-match-warning
                #:*native-unchecked-long-ids*
                #:*native-relaxed-value-restriction*
                #:*native-match-warnings*)
  (:export #:*native-annotations*
           #:native-annotation
           #:native-node-fact-info
           #:native-overloaded-instance
           #:native-node-type
           #:native-equality-type
           #:native-record-row
           #:native-match-info
           #:type->string))
