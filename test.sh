#!/bin/sh -e
cd "$(dirname "$0")"
export XDG_CACHE_HOME="$PWD/.cache"

# Loading HaMLet (t/hamlet-tests.lisp) needs a deep control stack.
# asdf:test-system runs every suite of cl-sml/tests and fails if one fails.
sbcl --control-stack-size 1GB --non-interactive \
    --eval '(load "~/.sbclrc")' \
    --eval "(ql:quickload '(:cl-sml :cl-sml/tests))" \
    --eval '(handler-case (asdf:test-system :cl-sml) (error (condition) (format *error-output* "~&~A~%" condition) (sb-ext:exit :code 1)))' \
    --load t/readtable-smoke.lisp
