#!/bin/sh
# Build cl-hamlet: HaMLet (hamlet/, a git submodule) compiled by cl-sml and
# saved as an SBCL executable that runs SML programs with HaMLet's
# interpreter (cl-hamlet FILE.sml ...; without files, HaMLet's session).
# Test-only, like the rest of t/hamlet-oracle/: cl-sml itself does not
# refer to hamlet/.
#   t/hamlet-oracle/build-cl-hamlet.sh [OUTPUT]   (default t/hamlet-oracle/build/cl-hamlet)
# Then runs a small program with it.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
cd "$ROOT"

SBCL="${SBCL:-sbcl}"
OUTPUT="${1:-$ROOT/t/hamlet-oracle/build/cl-hamlet}"
mkdir -p "$(dirname "$OUTPUT")"
OUTPUT="$(cd "$(dirname "$OUTPUT")" && pwd)/$(basename "$OUTPUT")"
PACKAGE="SML.HAMLET-IMAGE"

if [ ! -f hamlet/hamlet.sml ]; then
    echo "$(basename "$0"): hamlet/ is missing (git submodule update --init hamlet)" >&2
    exit 2
fi

# Loading HaMLet needs a deep control stack (:save-runtime-options bakes it
# into the executable) and peaks near 700MB of live heap.
"$SBCL" --dynamic-space-size "${CL_SML_HEAP:-4GB}" \
    --control-stack-size "${CL_HAMLET_STACK:-1GB}" --noinform --non-interactive \
    --eval '(load "~/.sbclrc")' \
    --eval "(push #p\"$ROOT/\" asdf:*central-registry*)" \
    --eval '(ql:quickload :cl-sml)' \
    --eval "(cl-sml:load-sml-file #p\"$ROOT/hamlet/hamlet.sml\" :package \"$PACKAGE\")" \
    --eval "(cl-sml::with-sml-package (\"$PACKAGE\")
              (setf (aref (cl-sml::ensure-sml-ref (cl-sml::sml-value \"Sml.basisPath\")) 1)
                    (cl-sml::sml-some-value \"$ROOT/hamlet/basis/\")))" \
    --eval "(sb-ext:save-lisp-and-die \"$OUTPUT\" :executable t :save-runtime-options t
              :toplevel (lambda ()
                          (cl-sml::with-sml-package (\"$PACKAGE\")
                            (let ((arguments (uiop:command-line-arguments)))
                              (if arguments
                                  (cl-sml::call-sml \"Sml.execFiles\" arguments)
                                  (cl-sml::call-sml \"Sml.execSession\" (cl-sml:sml-unit)))))
                          (sb-ext:exit :code 0)))"

SMOKE=$(mktemp "${TMPDIR:-/tmp}/cl-hamlet-smoke.XXXXXX")
trap 'rm -f "$SMOKE"' EXIT
printf 'fun fact 0 = 1 | fact n = n * fact (n - 1);\nval _ = print (Int.toString (fact 10) ^ "\\n");\n' > "$SMOKE"
"$OUTPUT" "$SMOKE" | tee /dev/stderr | grep -q '^3628800$'
echo "Built $OUTPUT"
