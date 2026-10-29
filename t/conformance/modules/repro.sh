#!/bin/sh
# ./repro.sh [--checker] [--basis] name...   runs repro/NAME.sml, prints output + diagnostics
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ARGS=""; MODE=plain; BASIS=none
for a in "$@"; do case "$a" in --checker) ARGS="$ARGS --checker"; MODE=checker;; --basis) BASIS=all;; *) NAMES="$NAMES $a";; esac; done
mkdir -p "$HERE/repro-results/$MODE"
CONF_TESTDIR="$HERE/repro/" CONF_RESULTS="$HERE/repro-results/$MODE/" "$HERE/run.sh" $ARGS --basis $BASIS $NAMES 2>&1 | grep -v "^WARNING: redef\|^mode:\|^[A-Z]*: [0-9]*$\|^$"
for n in $NAMES; do echo "--- $n output:"; cat "$HERE/repro-results/$MODE/$n.actual"; echo "--- diag:"; head -c 1500 "$HERE/repro-results/$MODE/$n.diag"; echo; done
