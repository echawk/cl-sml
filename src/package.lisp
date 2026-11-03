(defpackage #:cl-sml
  (:use #:cl #:named-readtables #:trivia)
  (:export #:sml-readtable
           #:enable-sml-syntax
           #:sml-adt
           #:make-sml-adt
           #:sml-unit
           #:sml-tuple
           #:sml-tuple-p
           #:sml-tuple-ref
           #:sml-tuple->list
           #:list->sml-tuple
           #:make-sml-record
           #:sml-record-p
           #:sml-record-ref
           #:sml-record-labels
           #:sml-record-fields
           #:sml-constructed-tag
           #:sml-constructed-argument
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
           #:*sml-optimize-policy*
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
           #:enable-native-type-checker
           #:disable-type-checker
           #:*record-sml-lisp-source*
           #:*show-lisp-compiler-output*
           #:sml-lisp-source
           #:show-sml-lisp
           #:show-sml-ir
           #:sml-ir-program
           #:*sml-ir*
           #:*sml-ir-simplify*
           #:*sml-decision-trees*
           #:*sml-defunctorize*
           #:*sml-ir-inline-size*
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

;;; The native SML '97 static semantics (docs/DESIGN-TYPECHECKER.md), milestone
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
           #:native-equality-kind
           #:native-node-representation
           #:native-record-row
           #:native-record-labels
           #:native-match-info
           #:type->string))
