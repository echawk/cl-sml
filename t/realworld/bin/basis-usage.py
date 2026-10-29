#!/usr/bin/env python3
"""basis-usage.py: which Basis identifiers does the corpus use, and which are
missing from (a) HaMLet's static basis (reference HaMLet, `hamlet -l`) and
(b) cl-sml's run time (symbol unbound in SML.RW of cl-sml-rw.core)?

Direct qualified uses only (Struct.id); structure aliases are not followed.
Writes basis-usage.tsv and prints missing identifiers with program counts.
"""
import os, re, subprocess, sys, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from workaround import strip_comments

R = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
W = os.path.join(R, "work")
STRUCTS = """General Option Bool List ListPair Char String Substring StringCvt Int
LargeInt Position Word Word8 LargeWord IEEEReal Math Real LargeReal Vector
Word8Vector CharVector VectorSlice Word8VectorSlice CharVectorSlice Array
Word8Array CharArray ArraySlice Word8ArraySlice CharArraySlice Byte Text IO
TextIO OS CommandLine IntInf Int8 Int16 Int31 Int32 Int63 Int64 Word16 Word31 Word32 Word63
Word64 Real32 Real64 RealArray RealVector Array2 Vector2 Timer Time Date BinIO
Unsafe SMLofNJ WideString WideChar WideSubstring PackRealBig PackRealLittle
PackWord32Big PackWord32Little MLton FixedInt""".split()
SYM = r"[!%&$#+\-/:<=>?@\\~`^|*]+"
pat = re.compile(r"(?<![A-Za-z0-9_'.])(" + "|".join(STRUCTS) +
                 r")\.((?:[A-Z][A-Za-z0-9_']*\.)*(?:[A-Za-z_][A-Za-z0-9_']*|" + SYM + "))")

uses = collections.defaultdict(set)   # id -> programs
for name in sorted(os.listdir(W)):
    p = os.path.join(W, name, "prog.sml")
    if not os.path.exists(p):
        continue
    src = strip_comments(open(p, encoding="utf-8", errors="replace").read())
    for m in pat.finditer(src):
        ident = m.group(1) + "." + m.group(2)
        uses[ident].add(name)

ids = sorted(uses)
# (a) reference HaMLet static basis
hamlet = os.path.join(R, "hamlet-ref", "hamlet", "hamlet")
tmp = os.path.join(R, "tmp", "basis-usage"); os.makedirs(tmp, exist_ok=True)
def hamlet_has(ident):
    last = ident.split(".")[-1]
    # types/structures/exceptions too: try value, then type, then structure
    tries = ["val _ = %s" % (("op " + ident) if not re.match(r"[A-Za-z]", last) else ident),
             "type t = %s" % ident, "structure S = %s" % ident,
             "exception E = %s" % ident, "type 'a t = 'a %s" % ident]
    for k, t in enumerate(tries):
        f = os.path.join(tmp, "q%d_%d.sml" % (abs(hash(ident)), k))
        open(f, "w").write(t + "\n")
        p = subprocess.run([hamlet, "-l", f], capture_output=True, text=True, cwd=tmp); out = p.stdout + p.stderr
        if "unknown" not in out and "error" not in out and "mismatch" not in out:
            return True
    return False
# (b) cl-sml run time: symbol bound in SML.RW (values only; types are static)
lisp = "(progn " + " ".join(
    '(format t "~A~C~A~%%" %s #\\Tab (let ((s (find-symbol %s "SML.RW"))) (and s (boundp s))))'
    % (json_s, json_s) for json_s in ['"%s"' % i.replace("\\", "\\\\") for i in ids]) + ")"
lf = os.path.join(tmp, "q.lisp"); open(lf, "w").write(lisp)
out = subprocess.run(["sbcl", "--core", os.path.join(R, "cl-sml-rw.core"), "--noinform",
                      "--non-interactive", "--no-userinit", "--load", lf],
                     capture_output=True, text=True).stdout
bound = {}
for line in out.splitlines():
    if "\t" in line:
        k, v = line.split("\t", 1); bound[k] = (v.strip() == "T")
from concurrent.futures import ThreadPoolExecutor
with ThreadPoolExecutor(8) as ex:
    has = dict(zip(ids, ex.map(hamlet_has, ids)))
rows = []
for i in ids:
    rows.append((i, len(uses[i]), has[i], bound.get(i, False), sorted(uses[i])))
with open(os.path.join(R, "basis-usage.tsv"), "w") as f:
    f.write("id\tnprogs\thamlet_static\tclsml_runtime_value\tprograms\n")
    for i, n, h, b, ps in rows:
        f.write("%s\t%d\t%s\t%s\t%s\n" % (i, n, h, b, " ".join(ps)))
print("== missing from HaMLet's static basis (blocks checked mode):")
for i, n, h, b, ps in sorted(rows, key=lambda r: -r[1]):
    if not h:
        print("  %-32s %2d  %s" % (i, n, " ".join(ps)[:120]))
print("== in HaMLet's basis but unbound at cl-sml run time (value ids only):")
for i, n, h, b, ps in sorted(rows, key=lambda r: -r[1]):
    if h and not b and re.match(r"[a-z]|" + SYM + "$", i.split(".")[-1]):
        print("  %-32s %2d  %s" % (i, n, " ".join(ps)[:120]))
