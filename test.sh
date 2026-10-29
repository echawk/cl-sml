#!/bin/sh -e
cd "$(dirname "$0")"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$PWD/.cache}"

# Test this checkout, not whichever cl-sml ASDF would otherwise find.
REGISTER="(push #p\"$PWD/\" asdf:*central-registry*)"
LOAD="(ql:quickload '(:cl-sml :cl-sml/tests))"
RUN='(handler-case (progn (asdf:test-system :cl-sml) (load "t/readtable-smoke.lisp")) (error (condition) (format *error-output* "~&~A~%" condition) (uiop:quit 1)))'

# Loading HaMLet (t/hamlet-tests.lisp) needs a deep control stack, and its
# live heap peaks near 700MB, too close to a 1GB SBCL heap for the collector.
# Set LISP=ecl or LISP=abcl to run the same ASDF suite elsewhere.
LISP=${LISP:-sbcl}
case "$(basename "$LISP")" in
    sbcl)
        "$LISP" --dynamic-space-size 4GB --control-stack-size 1GB \
            --non-interactive \
            --eval '(load "~/.sbclrc")' \
            --eval "$REGISTER" --eval "$LOAD" --eval "$RUN"
        ;;
    ecl)
        # ECL's default stacks are far smaller than SBCL's; the C stack is
        # in bytes (capped by `ulimit -Hs`), the others in entries.
        "$LISP" --c-stack 1073741824 --lisp-stack 16777216 \
            --frame-stack 1048576 \
            -eval "$REGISTER" -eval "$LOAD" -eval "$RUN" -eval '(quit)'
        ;;
    abcl)
        # ABCL runs on a fixed-size JVM thread stack (no option enlarges it).
        "$LISP" --batch \
            --eval "$REGISTER" --eval "$LOAD" --eval "$RUN" --eval '(quit)'
        ;;
    *)
        echo "Unsupported Common Lisp implementation: $LISP" >&2
        exit 2
        ;;
esac
