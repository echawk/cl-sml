#!/bin/sh
# Generate reference outputs with Poly/ML and MLton for every test.
# Usage: ./gen-ref.sh [test-name ...]
cd "$(dirname "$0")"
TESTS=${*:-$(cd tests && ls *.sml | sed 's/\.sml$//')}
for t in $TESTS; do
  poly --script "tests/$t.sml" > "ref/poly/$t.out" 2> "ref/poly/$t.err"
  echo $? > "ref/poly/$t.rc"
  if mlton -output "ref/mlton-bin/$t" "tests/$t.sml" > "ref/mlton/$t.cerr" 2>&1; then
    "ref/mlton-bin/$t" > "ref/mlton/$t.out" 2> "ref/mlton/$t.err"
    echo $? > "ref/mlton/$t.rc"
  else
    echo "COMPILE-ERROR" > "ref/mlton/$t.out"; echo 99 > "ref/mlton/$t.rc"
  fi
  if cmp -s "ref/poly/$t.out" "ref/mlton/$t.out"; then s=AGREE; else s=DIFFER; fi
  echo "$t poly=$(cat ref/poly/$t.rc) mlton=$(cat ref/mlton/$t.rc) $s"
done
