#!/bin/sh
# try.sh FILE.sml [MODE]: run one ad-hoc SML file under cl-sml (default MODE
# plain) and print the result line, stdout and backtrace.
R=$(cd "$(dirname "$0")/.." && pwd)
# State (work/, image, caches, results) lives in $RW_STATE (default: the harness dir).
S=${RW_STATE:-$R}
f=$1; m=${2:-plain}
n=$(basename "$f" .sml)
d=$S/try/$n
mkdir -p "$d"; cp "$f" "$d/prog.sml"; rm -f "$d/driver.sml"
TIMEOUT=${TIMEOUT:-60} "$R/bin/run-one.sh" "$d" "$m" > /dev/null
echo "== $(cut -f1,2,6 "$d/$m.res")"
head -c 2000 "$d/$m.out"
[ -f "$d/$m.bt" ] && sed -n '3,14p' "$d/$m.bt"
exit 0
