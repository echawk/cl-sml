#!/bin/sh
# ref-one.sh WORKDIR: reference run -> ref.out ref.err ref.rc ref.time ref.impl,
# files the program writes land in WORKDIR/run-ref/.
# Poly/ML first; if Poly rejects the program (compile error) or crashes in
# its compiler, fall back to MLton (-default-type int64).
R=$(cd "$(dirname "$0")/.." && pwd)
# State (work/, image, caches, results) lives in $RW_STATE (default: the harness dir).
S=${RW_STATE:-$R}
d=$(cd "$1" && pwd)
[ -f "$d/ref.out" ] && [ -z "${FORCE:-}" ] && exit 0
now() { perl -MTime::HiRes=time -e 'printf "%.3f", time'; }
"$R/bin/mkrundir.sh" "$d" ref
cd "$d/run-ref" || exit 1
s=$(now)
"$R/bin/timeout.pl" "${REF_TIMEOUT:-300}" env POLYRUN_FILE=../ref.sml poly --maxheap 4096 --script "$R/bin/polyrun.sml" > ../ref.out 2> ../ref.err
rc=$?
e=$(now)
impl=poly
if [ $rc != 0 ] && grep -q "error:\|InternalError" ../ref.out ../ref.err; then
  if "$R/bin/timeout.pl" 600 mlton @MLton max-heap 6g -- -default-type int64 \
       -default-ann 'allowExtendedTextConsts true' \
       -output ../ref.exe ../ref.sml > ../mlton.err 2>&1; then
    "$R/bin/mkrundir.sh" "$d" ref; cd "$d/run-ref"
    s=$(now)
    "$R/bin/timeout.pl" "${REF_TIMEOUT:-300}" ../ref.exe @MLton max-heap 4g -- > ../ref.out 2> ../ref.err
    rc=$?
    e=$(now)
    impl=mlton
  else
    impl=none
  fi
fi
echo $rc > ../ref.rc
echo $impl > ../ref.impl
perl -e "printf '%.2f', $e - $s" > ../ref.time
echo "$(basename "$d") impl=$impl rc=$rc t=$(cat ../ref.time)"
