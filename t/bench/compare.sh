#!/bin/sh
# compare.sh [NAME-SUBSTRING...] -- time programs under Poly/ML, MLton and
# cl-sml (default and fast policy) and print a comparison table.
#
# Only the run is timed, never compilation: each program is followed by a
# driver that reads Time.now around the work and prints the elapsed time
# on stderr, so every implementation measures the same thing in the same
# way.  Programs:
#   tb-NAME   t/bench/NAME.sml: `benchmark ()' run REPS times (its result
#             is printed once)
#   bm-NAME   the SML/NJ benchmark suite (RW_BENCHMARKS, assembled as in
#             t/realworld/bin/assemble.sh): Main.testit () writes the
#             output checked (untimed), then Main.doit (), the benchmark
#             proper, is timed
# The outputs of all implementations are compared with Poly/ML's (MLton's
# when Poly/ML rejects the program); a mismatch is marked in the table.
#
#   IMPLS="poly mlton clsml clsml-fast"   what to run
#   TIMEOUT=300            seconds per run (killed after)
#   RUNS=1                 runs per program and implementation (best kept)
#   REPS=...               repetitions of the tb-* benchmarks (see below)
#   CMP_STATE=DIR          work directories, SBCL image, MLton binaries,
#                          results (default: $TMPDIR/cl-sml-compare)
#   RW_BENCHMARKS=DIR      the SML/NJ suite (default: external/benchmarks)
#   CLSML_ROOT=DIR         the cl-sml checkout (default: this one)
#   POLY, MLTON            the executables (default: poly, mlton on PATH)
#   NORUN=1                only print the table from existing results
#   SKIP_DONE=1            keep existing results
#
# Results: CMP_STATE/work/<prog>/<impl>.{out,err,time}, the table on stdout
# and in CMP_STATE/table.md.
set -u
R=$(cd "$(dirname "$0")" && pwd)
P=${CLSML_ROOT:-$(cd "$R/../.." && pwd)}
S=${CMP_STATE:-${TMPDIR:-/tmp}/cl-sml-compare}
B=${RW_BENCHMARKS:-$P/external/benchmarks}
IMPLS=${IMPLS:-poly mlton clsml clsml-fast}
TIMEOUT=${TIMEOUT:-300}
RUNS=${RUNS:-1}
POLY=${POLY:-poly}
MLTON=${MLTON:-mlton}
RW=$P/t/realworld
W=$S/work
mkdir -p "$S" "$W"

# Repetitions of each t/bench program, so that one timed run takes about a
# second under Poly/ML.
reps() {
  case $1 in
    fib) echo 100 ;; tak) echo 300 ;; real) echo 10 ;; bst) echo 10 ;;
    eq) echo 10 ;; word) echo 100 ;; rec) echo 10 ;; match) echo 10 ;;
    *) echo 10 ;;
  esac
}

assemble() {
  RW_STATE=$S RW_BENCHMARKS=$B RW_LUNARML=/nonexistent CLSML_ROOT=$P \
    "$RW/bin/assemble.sh" > /dev/null
  rm -rf "$W"/ln-*
  for f in "$R"/*.sml; do
    n=$(basename "$f" .sml)
    d=$W/tb-$n
    mkdir -p "$d"
    cp "$f" "$d/prog.sml"
    k=${REPS:-$(reps "$n")}
    cat > "$d/timed.sml" <<EOF
val _ = let val t0 = Time.now ()
            fun loop (0, r) = r | loop (n, _) = loop (n - 1, benchmark ())
            val r = loop ($k, 0)
            val t1 = Time.now ()
        in print (Int.toString r ^ "\n");
           TextIO.output (TextIO.stdErr, "@@TIME " ^ LargeInt.toString
             (Time.toMicroseconds (Time.- (t1, t0))) ^ "\n")
        end;
EOF
  done
  for d in "$W"/bm-*/; do
    cat > "$d/timed.sml" <<'EOF'
val _ = Log.withOutput TextIO.stdOut Main.testit ();
val _ = let val t0 = Time.now ()
            val () = Main.doit ()
            val t1 = Time.now ()
        in TextIO.output (TextIO.stdErr, "@@TIME " ^ LargeInt.toString
             (Time.toMicroseconds (Time.- (t1, t0))) ^ "\n")
        end;
EOF
  done
  for d in "$W"/*/; do cat "$d/prog.sml" "$d/timed.sml" > "$d/all.sml"; done
}

build_image() {
  if [ ! -f "$S/cl-sml.core" ] ||
     [ -n "$(find "$P/src" "$P/cl-sml.asd" "$R/compare.lisp" -newer "$S/cl-sml.core" -type f 2>/dev/null | head -1)" ]; then
    echo "building image..." >&2
    mkdir -p "$S/cache" "$S/tmp"
    XDG_CACHE_HOME=$S/cache TMPDIR=$S/tmp \
    sbcl --control-stack-size 1GB --dynamic-space-size 4GB --noinform --non-interactive \
      --eval '(load "~/.sbclrc")' \
      --eval "(push #p\"$P/\" asdf:*central-registry*)" \
      --eval '(ql:quickload :cl-sml/cml :silent t)' \
      --eval '(let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream))) (uiop:symbol-call (quote #:cl-sml.types) (quote #:initial-env)))' \
      --eval "(load \"$R/compare.lisp\")" \
      --eval "(sb-ext:save-lisp-and-die \"$S/cl-sml.core.new\")" > "$S/build-image.log" 2>&1 ||
      { echo "image build failed; see $S/build-image.log" >&2; exit 1; }
    mv "$S/cl-sml.core.new" "$S/cl-sml.core"
  fi
}

# run_one DIR IMPL: one timed run; writes IMPL.out, IMPL.err, IMPL.time
# (seconds, or TIMEOUT / FAIL / NA).
run_one() {
  d=$1; i=$2
  "$RW/bin/mkrundir.sh" "$d" "$i"
  cd "$d/run-$i" || return
  case $i in
    poly)
      "$RW/bin/timeout.pl" "$TIMEOUT" env POLYRUN_FILE=../all.sml \
        "$POLY" --maxheap 4096 --script "$RW/bin/polyrun.sml" > ../$i.out 2> ../$i.err ;;
    mlton)
      if [ ! -x ../mlton.exe ] || [ ../all.sml -nt ../mlton.exe ]; then
        rm -f ../mlton.exe
        "$MLTON" @MLton max-heap 6g -- -default-type int64 -default-ann 'allowExtendedTextConsts true' \
          -output ../mlton.exe ../all.sml > ../mlton-compile.err 2>&1
      fi
      if [ -x ../mlton.exe ]; then
        "$RW/bin/timeout.pl" "$TIMEOUT" ../mlton.exe @MLton max-heap 4g -- > ../$i.out 2> ../$i.err
      else
        echo NA > ../$i.time; cd "$S"; return
      fi ;;
    clsml|clsml-fast)
      policy=:default; [ $i = clsml-fast ] && policy=:fast
      XDG_CACHE_HOME=$S/cache TMPDIR=$S/tmp \
      "$RW/bin/timeout.pl" "$TIMEOUT" sbcl --core "$S/cl-sml.core" \
        --control-stack-size 1GB --dynamic-space-size 4GB --noinform --non-interactive \
        --no-sysinit --no-userinit \
        --eval "(cmp:run \"$d/\" $policy)" > ../$i.out 2> ../$i.err ;;
  esac
  rc=$?
  t=$(sed -n 's/^@@TIME \([0-9]*\).*/\1/p' ../$i.err | tail -1)
  if [ -n "$t" ]; then perl -e "printf '%.3f', $t / 1e6" > ../$i.time
  elif [ $rc = 124 ]; then echo TIMEOUT > ../$i.time
  else echo FAIL > ../$i.time; fi
  cd "$S"
}

if [ -z "${NORUN:-}" ]; then
  assemble
  case " $IMPLS " in *" clsml"*) build_image ;; esac
  if [ $# -gt 0 ]; then
    dirs=$(for n in "$@"; do ls -d "$W"/*"$n"*/ 2>/dev/null; done | sort -u)
  else
    dirs=$(ls -d "$W"/*/)
  fi
  for d in $dirs; do
    d=${d%/}
    for i in $IMPLS; do
      [ -n "${SKIP_DONE:-}" ] && [ -s "$d/$i.time" ] && continue
      best=""
      k=0
      while [ $k -lt "$RUNS" ]; do
        run_one "$d" "$i"
        t=$(cat "$d/$i.time")
        case $t in [0-9]*)
          if [ -z "$best" ] || perl -e "exit !($t < $best)"; then
            best=$t; cp "$d/$i.out" "$d/$i.best.out"; fi ;;
        *) break ;; esac
        k=$((k + 1))
      done
      [ -n "$best" ] && { echo "$best" > "$d/$i.time"; mv "$d/$i.best.out" "$d/$i.out"; }
      echo "$(basename "$d") $i $(cat "$d/$i.time")" >&2
    done
  done
fi
TIMEOUT=$TIMEOUT python3 "$R/compare-report.py" "$W" $IMPLS | tee "$S/table.md"
