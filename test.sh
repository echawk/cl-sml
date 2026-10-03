#!/bin/sh -ex
export XDG_CACHE_HOME="$PWD/.cache"

# Loading HaMLet (hamlet-tests.lisp) needs a deep control stack.
sbcl --control-stack-size 1GB \
    --eval '(load "~/.sbclrc")' \
    --eval "(ql:quickload '(:cl-sml :fiveam))" \
     --load parser-tests.lisp \
     --load compiler-tests.lisp \
     --load runtime-tests.lisp \
     --load repl-tests.lisp \
     --load hamlet-tests.lisp \
     --load test.lisp \
     --eval "(quit)"
