#!/bin/sh
# refs.sh [WORKDIR...]: Poly/ML reference outputs for the corpus (cached).
R=$(cd "$(dirname "$0")/.." && pwd)
if [ $# -gt 0 ]; then for d in "$@"; do echo "$d"; done; else ls -d "$R"/work/*/; fi |
  xargs -P "${JOBS:-6}" -n 1 "$R/bin/ref-one.sh"
