#!/bin/sh
# build-image.sh: SBCL core with cl-sml + HaMLet checker loaded and HaMLet's
# basis sources preloaded into package SML.RW (user packages have no Basis
# structures of their own; see t/hol-checked.lisp).  Rebuilt by run.sh when
# anything under src/ or hamlet/basis/ is newer than the core.
set -eu
R=$(cd "$(dirname "$0")/.." && pwd)
P=${CLSML_ROOT:-/Users/ethan/sml/cl-sml}
mkdir -p "$R/cache" "$R/tmp"
export XDG_CACHE_HOME="$R/cache" TMPDIR="$R/tmp"
rm -f "$R/cl-sml-rw.core.new"
sbcl --control-stack-size 1GB --dynamic-space-size 8GB --noinform --non-interactive \
  --eval '(load "~/.sbclrc")' \
  --eval "(push #p\"$P/\" asdf:*central-registry*)" \
  --eval '(ql:quickload :cl-sml :silent t)' \
  --eval '(let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream))) (cl-sml:enable-hamlet-type-checker) (let ((cl-sml:*sml-type-checker* nil)) (cl-sml:load-sml-file (asdf:system-relative-pathname "cl-sml" "hamlet/basis/all.sml") :package "SML.RW")) (cl-sml:disable-type-checker))' \
  --eval "(sb-ext:save-lisp-and-die \"$R/cl-sml-rw.core.new\")" > "$R/build-image.log" 2>&1
mv "$R/cl-sml-rw.core.new" "$R/cl-sml-rw.core"
echo "built $R/cl-sml-rw.core"
