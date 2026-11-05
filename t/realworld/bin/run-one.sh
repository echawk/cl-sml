#!/bin/sh
# run-one.sh WORKDIR MODE: run one program under cl-sml (MODE plain|checked
# [-bare]) in a fresh process from cl-sml-rw.core; writes WORKDIR/MODE.{out,res,log,bt}.
R=$(cd "$(dirname "$0")/.." && pwd)
# State (work/, image, caches, results) lives in $RW_STATE (default: the harness dir).
S=${RW_STATE:-$R}
d=$(cd "$1" && pwd); m=$2
T=${TIMEOUT:-120}
export XDG_CACHE_HOME="$S/cache" TMPDIR="$S/tmp/$(basename "$d")-$m"
rm -rf "$TMPDIR"; mkdir -p "$TMPDIR"
"$R/bin/mkrundir.sh" "$d" "$m"
rm -f "$d/$m.res" "$d/$m.bt" "$d/$m.out"
"$R/bin/timeout.pl" $((T + 60)) sbcl --core "$S/cl-sml-rw.core" \
  --control-stack-size 1GB --dynamic-space-size 4GB --noinform --non-interactive \
  --no-sysinit --no-userinit \
  --load "$R/bin/driver.lisp" \
  --eval "(rw:run \"$d/\" \"$m\" $T)" > "$d/$m.stdout" 2>&1
rc=$?
if [ ! -s "$d/$m.res" ]; then
  if [ $rc = 124 ]; then printf 'TIMEOUT\tKILLED\t\t\t0\thard kill after %ss\n' $((T + 60)) > "$d/$m.res"
  else printf 'CRASH\t\t\t\t0\tsbcl exit %s: %s\n' $rc "$(tail -c 300 "$d/$m.stdout" | tr '\n\t' '  ')" > "$d/$m.res"; fi
fi
rm -rf "$TMPDIR"
printf '%s %s %s\n' "$(basename "$d")" "$m" "$(cut -f1,6 "$d/$m.res" | cut -c1-150)"
