#!/bin/sh -e
cd "$(dirname "$0")"
export XDG_CACHE_HOME="$PWD/.cache"

# Loading HaMLet (t/hamlet-tests.lisp) needs a deep control stack on SBCL.
# Set LISP=ecl or LISP=abcl to run the same ASDF suite elsewhere.
LISP=${LISP:-sbcl}
case "$(basename "$LISP")" in
    sbcl)
        "$LISP" --control-stack-size 1GB --non-interactive \
            --eval '(load "~/.sbclrc")' \
            --eval "(ql:quickload '(:cl-sml :cl-sml/tests))" \
            --eval '(handler-case (progn (asdf:test-system :cl-sml) (load "t/readtable-smoke.lisp")) (error (condition) (format *error-output* "~&~A~%" condition) (sb-ext:exit :code 1)))'
        ;;
    ecl)
        "$LISP" \
            -eval '(ql:quickload :cl-sml)' \
            -eval '(handler-case (progn (asdf:test-system :cl-sml) (load "t/readtable-smoke.lisp")) (error (condition) (format *error-output* "~&~A~%" condition) (uiop:quit 1)))' \
            -eval '(quit)'
        ;;
    abcl)
        "$LISP" --batch \
            --eval '(ql:quickload :cl-sml)' \
            --eval '(handler-case (progn (asdf:test-system :cl-sml) (load "t/readtable-smoke.lisp")) (error (condition) (format *error-output* "~&~A~%" condition) (uiop:quit 1)))' \
            --eval '(quit)'
        ;;
    *)
        echo "Unsupported Common Lisp implementation: $LISP" >&2
        exit 2
        ;;
esac
