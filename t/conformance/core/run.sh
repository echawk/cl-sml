#!/bin/sh
# Run the cl-sml Core conformance corpus.
# Usage: run.sh [--checker] [--timeout SECONDS] [NAME...]
#   --checker   enable the HaMLet static checker (slow start-up)
#   --native    use the native static checker (cl-sml's default)
#   --coalton   compile through the Coalton backend (cl-sml/coalton)
#   (neither: plain mode, no checker)
#   NAME...     run only these tests (default: every tests/*.sml)
# Environment: CLSML (cl-sml checkout, default: this checkout), LISP (sbcl).
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
CLSML=${CLSML:-$(cd "$(dirname "$0")/../../.." && pwd)}
LISP=${LISP:-sbcl}
CONF_CHECKER=0
CONF_TIMEOUT=60
CONF_BACKEND=
while [ $# -gt 0 ]; do
  case "$1" in
    --checker) CONF_CHECKER=1; shift ;;
    --native) CONF_CHECKER=native; shift ;;
    --coalton) CONF_BACKEND=coalton; shift ;;
    --timeout) CONF_TIMEOUT=$2; shift 2 ;;
    *) break ;;
  esac
done
CONF_TESTS="$*"
export CLSML CONF_CHECKER CONF_TIMEOUT CONF_TESTS CONF_BACKEND
# cl-sml writes its generated Lisp and FASLs to the temporary directory.
export TMPDIR="$HERE/build/tmp"
mkdir -p "$TMPDIR"
export XDG_CACHE_HOME="$CLSML/.cache"
cd "$CLSML"
# SBCL gets a bounded heap (other Lisps take no such option).
case "$(basename "$LISP")" in sbcl*) HEAP="--dynamic-space-size 4GB" ;; *) HEAP= ;; esac
exec "$LISP" $HEAP --control-stack-size 1GB --noinform --non-interactive \
  --eval '(load "~/.sbclrc")' \
  --load "$HERE/run.lisp" \
  --eval '(uiop:quit (if (conf-core:main) 0 1))' < /dev/null
