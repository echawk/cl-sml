#!/bin/sh
# Regenerate tests/NAME.out from reference SML implementations.
# The expected output is MLton's stdout (MLton reports diagnostics on stderr);
# a test containing "@reference poly" takes Poly/ML's instead.  Poly/ML (with
# its compiler warnings filtered out) and Moscow ML are run as cross-checks.
# Usage: gen-expected.sh [NAME...]   (default: every tests/*.sml)
cd "$(dirname "$0")"
mkdir -p build/ref
if [ $# -eq 0 ]; then set -- $(cd tests && ls *.sml | sed 's/\.sml$//'); fi
filter_poly () {
  awk '/^tests\/.*: (warning|error):/ {skip=1; next}
       skip && (/^[ \t]/ || /^Found near/) {next}
       {skip=0; print}'
}
for name in "$@"; do
  src=tests/$name.sml
  r=build/ref/$name
  poly --script "$src" < /dev/null 2> $r.poly.err | tee $r.poly.raw | filter_poly > $r.poly
  if mlton -output $r.mlton.exe "$src" > $r.mlton.err 2>&1; then
    $r.mlton.exe > $r.mlton 2>> $r.mlton.err < /dev/null
  else
    echo "MLTON COMPILE FAILED" > $r.mlton
  fi
  # Moscow ML prints one extra newline when it exits.
  mosml -P full -quietdec "$src" < /dev/null 2> $r.mosml.err | sed -e '${/^$/d;}' > $r.mosml
  if grep -q '@reference poly' "$src"; then primary=poly; else primary=mlton; fi
  status=""
  for other in poly mlton mosml; do
    [ $other = $primary ] && continue
    cmp -s $r.$primary $r.$other || status="$status $primary/$other-differ"
  done
  cp $r.$primary tests/$name.out
  echo "$name:${status:- agree}"
done
