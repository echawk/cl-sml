#!/usr/bin/env python3
"""Print the table of t/bench/compare.sh from WORKDIR/<prog>/<impl>.{time,out}.

usage: compare-report.py WORKDIR IMPL...

A time is in seconds; TIMEOUT, FAIL (an error, or no time printed) and NA
(MLton rejected the program) are shown as such.  A time followed by `!'
means the output differs from the reference (Poly/ML's, or MLton's when
Poly/ML fails).  The ratios are cl-sml's time over Poly/ML's, and the
geometric means are over the programs where both finished with the same
output (and, as a lower bound, with each timeout counted as TIMEOUT
seconds).
"""
import math, os, sys

W, impls = sys.argv[1], sys.argv[2:]

def read(path):
    try:
        with open(path, "rb") as f:
            return f.read()
    except OSError:
        return None

def number(s):
    try:
        return float(s)
    except (TypeError, ValueError):
        return None

names = {"poly": "poly s", "mlton": "mlton s", "clsml": "cl-sml s",
         "clsml-fast": "cl-sml fast s"}
ratio_impls = [i for i in ("clsml", "clsml-fast") if i in impls]
header = ["program"] + [names.get(i, i) for i in impls] + \
         ["%s / poly" % names[i][:-2] for i in ratio_impls if "poly" in impls]
timeout = float(os.environ.get("TIMEOUT", "300"))
rows, logs = [], {i: [] for i in ratio_impls}
bounds = {i: [] for i in ratio_impls}
for prog in sorted(os.listdir(W)):
    d = os.path.join(W, prog)
    if not os.path.isfile(os.path.join(d, "all.sml")):
        continue
    times, ok = {}, {}
    for i in impls:
        t = read(os.path.join(d, i + ".time"))
        times[i] = t.decode().strip() if t is not None else "-"
    ref = None
    for r in ("poly", "mlton"):
        if number(times.get(r)) is not None:
            ref = read(os.path.join(d, r + ".out"))
            break
    cells = [prog]
    for i in impls:
        t = times[i]
        ok[i] = number(t) is not None and (ref is None or read(os.path.join(d, i + ".out")) == ref)
        if number(t) is not None:
            cells.append("%.2f%s" % (number(t), "" if ok[i] else "!"))
        else:
            cells.append(t)
    if "poly" in impls:
        p = number(times["poly"])
        for i in ratio_impls:
            c = number(times[i])
            if p and c is not None and ok[i] and ok["poly"]:
                ratio = max(c, 0.001) / max(p, 0.001)
                logs[i].append(math.log(ratio))
                bounds[i].append(math.log(ratio))
                cells.append("%.1f" % ratio)
            elif p and ok["poly"] and times[i] == "TIMEOUT":
                bounds[i].append(math.log(timeout / max(p, 0.001)))
                cells.append(">%.0f" % (timeout / max(p, 0.001)))
            else:
                cells.append("")
    rows.append(cells)

print("| " + " | ".join(header) + " |")
print("|" + "---|" * len(header))
for r in rows:
    print("| " + " | ".join(r) + " |")
if "poly" in impls:
    for i in ratio_impls:
        if logs[i]:
            print("\ngeometric mean of %s / poly: %.2f over %d programs"
                  % (names[i][:-2], math.exp(sum(logs[i]) / len(logs[i])), len(logs[i])))
        if len(bounds[i]) > len(logs[i]):
            print("counting each timeout as %.0f s: at least %.2f over %d programs"
                  % (timeout, math.exp(sum(bounds[i]) / len(bounds[i])), len(bounds[i])))
