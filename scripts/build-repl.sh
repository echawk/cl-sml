#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$ROOT"

SBCL="${SBCL:-sbcl}"
OUTPUT="$ROOT/cl-sml"

# The image is saved with sb-ext:save-lisp-and-die: SBCL only.
if [ "$(basename "${LISP:-$SBCL}")" != sbcl ]; then
    echo "$(basename "$0") builds an SBCL image; LISP/SBCL must name SBCL." >&2
    exit 2
fi

# HaMLet and deep SML recursion need a deep control stack; :save-runtime-options bakes this
# size into the executable.
STACK="${CL_SML_STACK:-1GB}"

# The REPL type-checks every phrase with the native checker (src/types/),
# whose elaborated Basis is made here so that it is part of the image.
# CL_SML_HAMLET=1 makes HaMLet the checker instead (the test-only system
# cl-sml/hamlet-oracle and HaMLet, from hamlet/, loaded into the image);
# CL_SML_NO_TYPE_CHECKER=1 builds an image that does not check (plain mode).
if [ -n "${CL_SML_NO_TYPE_CHECKER:-}" ]; then
    ENABLE='(cl-sml:disable-type-checker)'
elif [ -n "${CL_SML_HAMLET:-}" ]; then
    SYSTEMS=':cl-sml :cl-sml/hamlet-oracle'
    ENABLE='(uiop:symbol-call (quote #:cl-sml) (quote #:enable-hamlet-type-checker))'
else
    ENABLE='(uiop:symbol-call (quote #:cl-sml.types) (quote #:initial-env))'
fi

# Loading HaMLet peaks near 700MB of live heap; CL_SML_HEAP sets SBCL's heap.
"$SBCL" --dynamic-space-size "${CL_SML_HEAP:-4GB}" \
    --control-stack-size "$STACK" --noinform --non-interactive \
    --eval '(load "~/.sbclrc")' \
    --eval "(push #p\"$ROOT/\" asdf:*central-registry*)" \
    --eval "(ql:quickload '(${SYSTEMS:-:cl-sml}))" \
    --eval "$ENABLE" \
    --eval "(sb-ext:save-lisp-and-die \"$OUTPUT\" :executable t :save-runtime-options t :toplevel (lambda () (sb-ext:exit :code (cl-sml:main))))"
