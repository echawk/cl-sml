#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$ROOT"

SBCL="${SBCL:-sbcl}"
OUTPUT="$ROOT/cl-sml"
# Loading HaMLet needs a deep control stack; :save-runtime-options bakes this
# size into the executable.
STACK="${CL_SML_STACK:-1GB}"

# The REPL type-checks every phrase with HaMLet's static semantics.  The
# checker (HaMLet plus its elaborated basis) is created here so that it is
# part of the image.  Set CL_SML_NO_TYPE_CHECKER=1 to build without it.
if [ -n "${CL_SML_NO_TYPE_CHECKER:-}" ]; then
    ENABLE='nil'
else
    ENABLE='(cl-sml:enable-hamlet-type-checker)'
fi

"$SBCL" --control-stack-size "$STACK" --noinform --non-interactive \
    --eval '(load "~/.sbclrc")' \
    --eval "(ql:quickload '(:cl-sml))" \
    --eval "$ENABLE" \
    --eval "(sb-ext:save-lisp-and-die \"$OUTPUT\" :executable t :save-runtime-options t :toplevel (lambda () (cl-sml:repl) (sb-ext:exit :code 0)))"
