(defpackage #:cl-sml-semantics-tests
  (:use #:cl #:fiveam))

(in-package #:cl-sml-semantics-tests)

(def-suite cl-sml-semantics-suite
  :description "SML97 dynamic semantics of compiled programs: static scoping
of rebound names, Match and Bind, records and tuples")

(in-suite cl-sml-semantics-suite)

(defvar *package-counter* 0)

(defun fresh-sml-package ()
  (let ((name (format nil "SML.SEMANTICS-TEST-~D" (incf *package-counter*))))
    (when (find-package name)
      (delete-package name))
    name))

(defun run-sml (&rest sources)
  "Compile and run each of SOURCES, in order and as separate compilation
units, in a fresh SML package.  Returns what they printed and the package."
  (let ((package (fresh-sml-package)))
    (values (with-output-to-string (*standard-output*)
              (dolist (source sources)
                (eval (cl-sml:compile-sml-program-string source
                                                         :package package))))
            package)))

(defun load-sml (source)
  "Like RUN-SML for one SOURCE, but through LOAD-SML-FILE, which compiles the
generated Lisp with COMPILE-FILE."
  (let ((package (fresh-sml-package))
        (pathname (uiop:with-temporary-file (:pathname p :type "sml" :keep t)
                    p)))
    (unwind-protect
         (progn
           (with-open-file (stream pathname :direction :output
                                            :if-exists :supersede)
             (write-string source stream))
           (values (with-output-to-string (*standard-output*)
                     (handler-bind ((warning #'muffle-warning))
                       (cl-sml:load-sml-file pathname :package package)))
                   package))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun lines (&rest lines)
  (format nil "~{~A~%~}" lines))

;;; Modules

(test open-several-structures-later-ones-shadow
  (is (string= (lines "B.c" "A.c")
               (run-sml "structure A = struct val c = \"A.c\" end
structure B = struct val c = \"B.c\" end
open A B
val _ = print (c ^ \"\\n\")
structure C = struct
open B A
val d = c
end
val _ = print (C.d ^ \"\\n\")")))
  (is (string= (lines "A.B.x")
               (run-sml "structure A = struct
val x = \"A.x\"
structure B = struct val x = \"A.B.x\" end
end
open A A.B
val _ = print (x ^ \"\\n\")"))))

(test long-constructor-pattern-rejects-nullary-value
  (is (string= (lines "circle 3" "empty")
               (run-sml "structure Shapes = struct
datatype shape = Circle of int | Empty
end
fun describe (Shapes.Circle r) = \"circle \" ^ Int.toString r
  | describe Shapes.Empty = \"empty\"
val _ = print (describe (Shapes.Circle 3) ^ \"\\n\")
val _ = print (describe Shapes.Empty ^ \"\\n\")"))))
