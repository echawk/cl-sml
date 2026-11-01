#!/bin/sh
# Run the cl-sml Modules/Basis conformance corpus.
#   ./run.sh                 plain mode (no static checker)
#   ./run.sh --checker       with the HaMLet type checker
#   ./run.sh --native        with the native type checker (cl-sml's default)
#   ./run.sh [--checker] [--basis auto|none|all] [--timeout N] [--isolate fork|none] [test-name ...]
# Results: results/{plain,checker,native}/NAME.{status,actual,diag} and summary.txt.
# CL_SML_ROOT defaults to this checkout.
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT=${CL_SML_ROOT:-$(cd "$(dirname "$0")/../../.." && pwd)}
CONF_CHECKER=0; CONF_BASIS=auto; CONF_TIMEOUT=60; CONF_TESTS=; ISOLATE=
while [ $# -gt 0 ]; do
  case "$1" in
    --checker) CONF_CHECKER=1 ;;
    --native) CONF_CHECKER=native ;;
    --basis) shift; CONF_BASIS=$1 ;;
    --timeout) shift; CONF_TIMEOUT=$1 ;;
    --isolate) shift; ISOLATE=$1 ;;
    *) CONF_TESTS="$CONF_TESTS $1" ;;
  esac
  shift
done
if [ -z "$ISOLATE" ]; then
  if [ "$CONF_CHECKER" = 1 ]; then ISOLATE=none; else ISOLATE=fork; fi
fi
CONF_ISOLATE=${CONF_ISOLATE:-$ISOLATE}
export CONF_CHECKER CONF_BASIS CONF_TIMEOUT CONF_TESTS CONF_ISOLATE
# Keep ASDF's fasl cache and cl-sml's generated artifacts out of $HOME and /tmp.
export XDG_CACHE_HOME="$HERE/.cache"
export TMPDIR="$HERE/tmp/"
mkdir -p "$TMPDIR"
cd "$ROOT" || exit 2
run_lisp() {
  sbcl --control-stack-size 1GB --noinform --non-interactive \
    --eval '(load "~/.sbclrc")' \
    --eval '(ql:quickload (quote (:cl-sml :cl-sml/hamlet-oracle)) :silent t)' \
    --load "$HERE/run.lisp" \
    --eval '(uiop:quit (if (cl-sml-conformance::main) 0 1))'
}
CONF_RESUME=${CONF_RESUME:-0}; export CONF_RESUME
run_lisp; rc=$?
# In-process mode: a test that kills the Lisp (heap exhaustion) is recorded
# as CRASH and the run resumes after it.
tries=0
while [ "$CONF_ISOLATE" = none ] && [ $rc -gt 1 ] && [ $tries -lt 20 ]; do
  echo ";; Lisp died (rc=$rc); resuming"
  CONF_RESUME=1; export CONF_RESUME
  run_lisp; rc=$?
  tries=$((tries+1))
done
exit $rc
