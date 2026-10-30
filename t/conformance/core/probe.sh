#!/bin/sh
# Debug helper: run SML files through cl-sml, printing output and full errors.
# Usage: probe.sh [--checker] [--basis] FILE...
HERE=$(cd "$(dirname "$0")" && pwd)
CHK=nil; BASIS=nil
while [ $# -gt 0 ]; do case "$1" in --checker) CHK=t; shift;; --basis) BASIS=t; shift;; *) break;; esac; done
FILES=""
for f in "$@"; do FILES="$FILES \"$(cd "$(dirname "$f")" && pwd)/$(basename "$f")\""; done
export TMPDIR="$HERE/build/tmp"; mkdir -p "$TMPDIR"
export XDG_CACHE_HOME=/Users/ethan/sml/cl-sml/.cache
cd /Users/ethan/sml/cl-sml
sbcl --control-stack-size 1GB --noinform --non-interactive --eval '(load "~/.sbclrc")' \
 --eval '(let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream))) (ql:quickload :cl-sml :silent t))' \
 --eval "(when $CHK (let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream))) (cl-sml:enable-hamlet-type-checker)))" \
 --eval "(let ((n 0)) (dolist (f (list $FILES)) (incf n) (format t \"~&=== ~A~%\" f) (let ((pkg (format nil \"SML.PROBE~D\" n))) (handler-case (let ((*error-output* (make-broadcast-stream)) (*compile-verbose* nil)) (when $BASIS (let ((cl-sml::*sml-type-checker* nil) (*standard-output* (make-broadcast-stream))) (cl-sml:load-sml-file (asdf:system-relative-pathname \"cl-sml\" \"hamlet/basis/all.sml\") :package pkg))) (handler-bind ((warning #'muffle-warning)) (cl-sml:load-sml-file (pathname f) :package pkg))) (serious-condition (c) (let ((s (princ-to-string c))) (format t \"~&!! ~A: ~A~%\" (type-of c) (subseq s 0 (min 300 (length s))))))))))" < /dev/null 2>&1 | grep -v "^WARNING: redefining"
