#!/bin/sh -ex
export XDG_CACHE_HOME="$PWD/.cache"

sbcl --eval "(ql:quickload '(:cl-sml :fiveam))" \
     --load parser-tests.lisp \
     --load runtime-tests.lisp \
     --load test.lisp \
     --eval "(quit)"
