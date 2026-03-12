#!/bin/sh -ex
sbcl --eval "(ql:quickload '(:cl-sml :fiveam))" \
     --load parser-tests.lisp \
     --load test.lisp \
     --eval "(quit)"
