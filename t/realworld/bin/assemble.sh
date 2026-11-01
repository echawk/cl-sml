#!/bin/sh
# Assemble the corpus into $S/work/<name>/{prog.sml,driver.sml,ref.sml}.
#   bm-*   SML/NJ benchmark suite (benchmarks/programs): util/bmark.sig +
#          util/log.sml + FILES (minus ../BASIS/*) + main.sml; driver.sml runs
#          Main.testit with Log output on stdout.
#   ln-*   LunarML test/should_run and test/mlbasis/should_run single-file
#          tests (no .mlb); expected output in expected.out.
set -eu
R=$(cd "$(dirname "$0")/.." && pwd)
# State (work/, image, caches, results) lives in $RW_STATE (default: the harness dir).
S=${RW_STATE:-$R}
P=${CLSML_ROOT:-$(cd "$R/../.." && pwd)}
# Corpora (untracked, under external/); RW_BENCHMARKS and RW_LUNARML override.
B=${RW_BENCHMARKS:-$P/external/benchmarks}
L=${RW_LUNARML:-$P/external/LunarML}
W=$S/work
mkdir -p "$W"

for dir in "$B"/programs/*/; do
  name=$(basename "$dir")
  [ -f "$dir/main.sml" ] || continue
  out=$W/bm-$name
  mkdir -p "$out"
  prog=$out/prog.sml
  : > "$prog"
  missing=""
  # SML/NJ-only names that Poly/ML lacks too: prepend a portable shim
  # (compat/*.sml) when the program mentions them; recorded in COMPAT.
  srcs="$dir/main.sml"
  [ -f "$dir/FILES" ] && for f in $(cat "$dir/FILES"); do
    case $f in ../BASIS/*) continue ;; esac
    [ -f "$dir/$f" ] && srcs="$srcs $dir/$f"
  done
  compat=""
  for c in Real64 Real64Array Fn LibBase Unsafe Option.isNone String.concatWithMap; do
    case $c in
      *.*) found=$(cat $srcs | grep -v '^ *(\*' | grep -cF "$c" || true) ;;
      *) found=$(cat $srcs | grep -v '^ *(\*' | grep -cw "$c" || true) ;;
    esac
    if [ "$found" != 0 ]; then
      echo "(******** compat/$c.sml ********)" >> "$prog"; cat "$R/compat/$c.sml" >> "$prog"
      compat="$compat $c"
    fi
  done
  echo "$compat" > "$out/COMPAT"
  for f in bmark.sig log.sml; do
    echo "(******** util/$f ********)" >> "$prog"; cat "$B/util/$f" >> "$prog"
  done
  if [ -f "$dir/FILES" ]; then
    for f in $(cat "$dir/FILES"); do
      case $f in ../BASIS/*) continue ;; esac
      if [ -f "$dir/$f" ]; then
        echo "(******** $f ********)" >> "$prog"; cat "$dir/$f" >> "$prog"
      else
        missing="$missing $f"
      fi
    done
  fi
  echo "(******** main.sml ********)" >> "$prog"; cat "$dir/main.sml" >> "$prog"
  echo 'val _ = Log.withOutput TextIO.stdOut Main.testit ();' > "$out/driver.sml"
  cat "$prog" "$out/driver.sml" > "$out/ref.sml"
  [ -n "$missing" ] && echo "$missing" > "$out/MISSING" || rm -f "$out/MISSING"
  # Programs read and WRITE under DATA/ (lexgen, mlyacc, vliw): keep a private
  # pristine copy; every run gets its own copy in run-<tag>/ (see mkrundir.sh).
  rm -rf "$out/DATA"
  [ -d "$dir/DATA" ] && cp -R "$dir/DATA" "$out/DATA"
  [ -f "$dir/ANSWER" ] && cp "$dir/ANSWER" "$out/expected.out"
  :
done

for sub in should_run mlbasis/should_run; do
  tag=$(echo "$sub" | sed 's|/should_run||; s|should_run|core|')
  for f in "$L"/test/$sub/*.sml; do
    base=$(basename "$f" .sml)
    [ -f "${f%.sml}.mlb" ] && continue
    [ -f "${f%.sml}.stdout" ] || continue
    out=$W/ln-$tag-$base
    mkdir -p "$out"
    cp "$f" "$out/prog.sml"
    cp "$f" "$out/ref.sml"
    rm -f "$out/driver.sml"
    cp "${f%.sml}.stdout" "$out/expected.out"
  done
done
ls "$W" | wc -l
