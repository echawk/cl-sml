#!/bin/sh
# hamlet-ref.sh: elaborate every corpus program with a reference HaMLet built
# by MLton (hamlet-ref/hamlet/hamlet, see README) -> work/<n>/hamlet-ref.txt:
# "OK" or the first error.  bm-* programs get compat/shim.sml (BinIO) first,
# like the *-shim modes.  Separates HaMLet's own (Definition-strict) verdicts
# from differences in cl-sml's compiled copy of HaMLet.
R=$(cd "$(dirname "$0")/.." && pwd)
# State (work/, image, caches, results) lives in $RW_STATE (default: the harness dir).
S=${RW_STATE:-$R}
H=$S/hamlet-ref/hamlet/hamlet
mkdir -p "$S/tmp/hamlet-ref"; cd "$S/tmp/hamlet-ref" || exit 1
for d in "$R"/work/*/; do
  n=$(basename "$d")
  f=$n.sml
  case $n in bm-*) cat "$R/compat/shim.sml" "$d/prog.sml" > "$f" ;; *) cp "$d/prog.sml" "$f" ;; esac
  "$R/bin/timeout.pl" 120 "$H" -l "$f" > "$n.out" 2>&1
  rc=$?
  e=$(grep -E "^$f:[0-9.-]+: " "$n.out" | grep -v "warning:" | head -1 | sed "s|^$f:||")
  [ -z "$e" ] && [ $rc = 124 ] && e="TIMEOUT (120s)"
  [ -z "$e" ] && e=OK
  echo "$e" > "$d/hamlet-ref.txt"
  rm -f "$f" "$n.out"
  printf '%-36s %s\n' "$n" "$e"
done
