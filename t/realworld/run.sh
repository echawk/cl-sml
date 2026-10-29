#!/bin/sh
# run.sh [NAME-SUBSTRING...] -- re-run the real-world corpus under cl-sml and
# print the classification table.
#
#   MODES="plain checked"  checker modes to run (plain = no static checker,
#                          checked = HaMLet checker; add -bare to skip the
#                          HaMLet-basis preload, e.g. MODES=plain-bare)
#   JOBS=6                 parallel programs
#   TIMEOUT=120            seconds per program (hard kill at +60s)
#   REBUILD=1              force rebuilding cl-sml-rw.core (auto when src/,
#                          hamlet/basis/ or this harness's driver is newer)
#   ASSEMBLE=1             re-assemble work/ from the sources (auto if missing)
#   NORUN=1                only print the table from existing results
#   SKIP_DONE=1            only run programs without a result for a mode
#
# Results: work/<prog>/<mode>.{res,out,bt,log}; table also in results.tsv.
set -u
R=$(cd "$(dirname "$0")" && pwd)
P=${CLSML_ROOT:-/Users/ethan/sml/cl-sml}
MODES=${MODES:-plain checked}
JOBS=${JOBS:-6}
export TIMEOUT=${TIMEOUT:-120}

if [ -z "${NORUN:-}" ]; then
  if [ -n "${ASSEMBLE:-}" ] || [ ! -d "$R/work" ]; then "$R/bin/assemble.sh" >/dev/null; fi
  "$R/bin/refs.sh" >/dev/null 2>&1
  if [ -n "${REBUILD:-}" ] || [ ! -f "$R/cl-sml-rw.core" ] ||
     [ -n "$(find "$P/src" "$P/hamlet/basis" "$P/cl-sml.asd" -newer "$R/cl-sml-rw.core" -type f 2>/dev/null | head -1)" ]; then
    echo "building image..." >&2
    "$R/bin/build-image.sh" >&2 || { echo "image build failed; see $R/build-image.log" >&2; exit 1; }
  fi
  if [ $# -gt 0 ]; then
    dirs=$(for n in "$@"; do ls -d "$R"/work/*"$n"*/ 2>/dev/null; done | sort -u)
  else
    dirs=$(ls -d "$R"/work/*/)
  fi
  for m in $MODES; do for d in $dirs; do
    # SKIP_DONE=1: keep existing results (resume an interrupted run)
    [ -n "${SKIP_DONE:-}" ] && [ -s "$d/$m.res" ] && continue
    echo "$d $m"; done; done |
    xargs -P "$JOBS" -n 2 "$R/bin/run-one.sh" >> "$R/run.log" 2>&1
fi
modes=$(echo $MODES | tr ' ' ',')
python3 "$R/bin/report.py" --modes "$modes" --tsv "$R/results.tsv" "$@"
