#!/usr/bin/env python3
"""Print the classification table for the corpus from work/*/{ref,MODE}.*

usage: report.py [--modes plain,checked] [--tsv FILE] [NAME-SUBSTRING...]

Classes: PASS, WRONG, PARSE, COMPILE, RUNTIME, TIMEOUT, CHECKER, plus
NOREF (no reference implementation accepts the program: score excluded) and
'-' (not run).  The detail column refines COMPILE (lisp = SBCL rejected the
generated Lisp) and RUNTIME (exn = uncaught SML exception, stack, heap, crash).
"""
import os, sys, subprocess

R = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
S = os.environ.get("RW_STATE", R)  # state dir (work/, image, results)
W = os.path.join(S, "work")

def read(path, default=None):
    try:
        with open(path, "rb") as f:
            return f.read()
    except OSError:
        return default

def files_differ(d, mode):
    a, b = os.path.join(d, "run-ref"), os.path.join(d, "run-" + mode)
    if not os.path.isdir(a) or not os.path.isdir(b):
        return False
    r = subprocess.run(["diff", "-rq", a, b], capture_output=True, text=True)
    return r.returncode != 0

def ref_ok(d):
    rc = (read(os.path.join(d, "ref.rc"), b"1").strip() or b"1")
    return rc == b"0" and (read(os.path.join(d, "ref.impl"), b"none").strip() != b"none")

def classify(d, mode):
    res = read(os.path.join(d, mode + ".res"))
    if res is None:
        return "-", "", None, None, ""
    f = res.decode("utf-8", "replace").rstrip("\n").split("\t")
    f += [""] * (6 - len(f))
    cls, phase, tc, tr, warns, msg = f[:6]
    tc = float(tc) if tc else None
    tr = float(tr) if tr else None
    out = read(os.path.join(d, mode + ".out"), b"")
    refs = []
    impl = read(os.path.join(d, "ref.impl"), b"none").strip()
    if ref_ok(d) or (impl != b"none" and read(os.path.join(d, "ref.out"), b"")):
        # A reference that ran but died (uncaught exception) still defines
        # the expected output up to that point.
        refs.append(read(os.path.join(d, "ref.out"), b""))
    exp = read(os.path.join(d, "expected.out"))
    if exp is not None:
        refs.append(exp)
    detail = ""
    if cls == "OK":
        if not refs:
            final = "NOREF"
        elif any(out == r for r in refs) and not files_differ(d, mode):
            final = "PASS"
        else:
            final = "WRONG"
            detail = "files" if any(out == r for r in refs) else "stdout"
    elif cls in ("PARSE", "CHECKER", "TIMEOUT"):
        final = cls
        if cls == "TIMEOUT":
            detail = phase.lower()
    elif cls in ("COMPILE", "LISPCOMPILE"):
        final, detail = "COMPILE", ("lisp" if cls == "LISPCOMPILE" else "")
    elif cls == "SMLEXN":
        # The reference may die with the same exception (e.g. a test that
        # prints and then raises); then compare the output so far.
        if not ref_ok(d) and refs and any(out == r for r in refs) \
           and not files_differ(d, mode):
            final = "PASS"
        else:
            final, detail = "RUNTIME", "exn"
    elif cls in ("STACK", "HEAP", "CRASH"):
        final, detail = "RUNTIME", cls.lower()
    else:
        final = "RUNTIME"
    if not refs and final not in ("PASS",):
        detail = (detail + " noref").strip()
    return final, detail, tc, tr, msg

def fmt(t):
    return "" if t is None else ("%.2f" % t)

def main(argv):
    modes = ["plain", "checked"]
    tsv = None
    names = []
    i = 0
    while i < len(argv):
        if argv[i] == "--modes":
            modes = argv[i + 1].split(","); i += 2
        elif argv[i] == "--tsv":
            tsv = argv[i + 1]; i += 2
        else:
            names.append(argv[i]); i += 1
    dirs = sorted(x for x in os.listdir(W) if os.path.isdir(os.path.join(W, x)))
    if names:
        dirs = [x for x in dirs if any(n in x for n in names)]
    rows = []
    counts = {m: {} for m in modes}
    for name in dirs:
        d = os.path.join(W, name)
        impl = (read(os.path.join(d, "ref.impl"), b"?").decode().strip())
        reft = read(os.path.join(d, "ref.time"), b"").decode().strip()
        row = [name, impl if ref_ok(d) else (impl + "!"), reft]
        for m in modes:
            final, detail, tc, tr, msg = classify(d, m)
            counts[m][final] = counts[m].get(final, 0) + 1
            row += [final + ("/" + detail if detail else ""), fmt(tc), fmt(tr), msg[:110]]
        rows.append(row)
    hdr = ["program", "ref", "ref_s"]
    for m in modes:
        hdr += [m, m[:1] + "_comp_s", m[:1] + "_run_s", m[:1] + "_first_error"]
    if tsv:
        with open(tsv, "w") as f:
            f.write("\t".join(hdr) + "\n")
            for r in rows:
                f.write("\t".join(r) + "\n")
    # terminal table: hide error columns except the first mode with an error
    short = [0, 1, 2] + [3 + 4 * k + j for k in range(len(modes)) for j in range(3)]
    widths = [max(len(hdr[c]), *(len(r[c]) for r in rows)) if rows else len(hdr[c]) for c in short]
    print("  ".join(hdr[c].ljust(w) for c, w in zip(short, widths)) + "  first_error")
    for r in rows:
        err = next((r[3 + 4 * k + 3] for k in range(len(modes)) if r[3 + 4 * k + 3]), "")
        print("  ".join(r[c].ljust(w) for c, w in zip(short, widths)) + "  " + err)
    print()
    for m in modes:
        tot = sum(counts[m].values())
        print("%-8s %s  (total %d)" % (m, "  ".join("%s=%d" % kv for kv in sorted(counts[m].items())), tot))

if __name__ == "__main__":
    main(sys.argv[1:])
