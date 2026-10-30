#!/bin/sh
# mkrundir.sh WORKDIR TAG: fresh WORKDIR/run-TAG with a copy of WORKDIR/DATA.
# Programs run with this as their current directory; files they write end up
# here and are compared between the reference and cl-sml runs.
d=$1; t=$2
rm -rf "$d/run-$t"; mkdir -p "$d/run-$t"
[ -d "$d/DATA" ] && cp -R "$d/DATA" "$d/run-$t/DATA"
exit 0
