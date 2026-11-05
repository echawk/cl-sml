#!/bin/sh
# mk-nb.sh NAME...: nb/<NAME> = work/<NAME> rewritten by workaround.py (all
# rules) to look past the first blockers; checks the rewrite with Poly/ML.
R=$(cd "$(dirname "$0")/.." && pwd)
# State (work/, image, caches, results) lives in $RW_STATE (default: the harness dir).
S=${RW_STATE:-$R}
for n in "$@"; do
  w=$S/work/$n; d=$S/nb/$n
  rm -rf "$d"; mkdir -p "$d"
  rules=$(python3 "$R/bin/workaround.py" "$w/prog.sml" "$d/prog.sml" $RULES)
  [ -f "$w/driver.sml" ] && cp "$w/driver.sml" "$d/"
  [ -d "$w/DATA" ] && cp -R "$w/DATA" "$d/DATA"
  for f in ref.out ref.rc ref.impl expected.out; do [ -f "$w/$f" ] && cp "$w/$f" "$d/"; done
  # Poly check of the rewrite (skipped when the reference was MLton)
  if [ "$(cat "$w/ref.impl")" = poly ]; then
    mkdir -p "$d/run-polycheck"; [ -d "$d/DATA" ] && cp -R "$d/DATA" "$d/run-polycheck/"
    cat "$d/prog.sml" "$d/driver.sml" 2>/dev/null > "$d/run-polycheck/all.sml"
    (cd "$d/run-polycheck" && "$R/bin/timeout.pl" 300 env POLYRUN_FILE=all.sml poly --maxheap 4096 --script "$R/bin/polyrun.sml" > ../polycheck.out 2> ../polycheck.err)
    if cmp -s "$d/polycheck.out" "$w/ref.out"; then pc=same; else pc=DIFFERS; fi
  else pc=skip; fi
  echo "$n: rules=[$rules] poly=$pc"
done
