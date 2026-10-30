#!/usr/bin/env python3
"""workaround.py IN OUT [rule...] -- rewrite an SML program around known
cl-sml blockers, to find the next blocker.  Semantics-preserving for the
corpus programs (only drops signature ascriptions/annotations).  Rules
(default: all):
  realexp   1.5e~3 -> 0.0015          (sml-real has no exponent part)
  inlinesig structure X : sig .. end = struct  ->  structure X = struct
  ascribe   structure X = struct .. end : S    ->  structure X = struct .. end
  tyvarseq  fun 'a f / val 'a x / fun ('a,'b) f -> fun f / val x
  abstype   abstype t = .. with D end -> datatype t = .. D
  joinctor  multi-line constructor argument types joined onto one line
  bracerec  `of {..} * t` / `= {..} list` types wrapped in parentheses
  recpatty  {a : t, b : u} in fun/fn patterns -> {a, b}
Prints the rules that changed something.
"""
import re, sys

def strip_comments(src):
    """Same length, comments/strings blanked (for scanning only)."""
    out = list(src)
    i, n, depth = 0, len(src), 0
    while i < n:
        if depth == 0 and src[i] == '"':
            j = i + 1
            while j < n and src[j] != '"':
                j += 2 if src[j] == '\\' else 1
            for k in range(i + 1, min(j, n)):
                if out[k] != '\n': out[k] = ' '
            i = j + 1
            continue
        if src.startswith('(*', i):
            depth += 1; out[i] = out[i+1] = ' '; i += 2; continue
        if depth and src.startswith('*)', i):
            depth -= 1; out[i] = out[i+1] = ' '; i += 2; continue
        if depth and out[i] != '\n':
            out[i] = ' '
        i += 1
    return ''.join(out)

TOK = re.compile(r"[A-Za-z_][A-Za-z0-9_'.]*")
OPENERS = {'sig', 'struct', 'let', 'local', 'abstype'}

def match_end(clean, start):
    """Index just after the `end` closing the opener at START."""
    depth = 0
    for m in TOK.finditer(clean, start):
        w = m.group(0)
        if w in OPENERS: depth += 1
        elif w == 'end':
            depth -= 1
            if depth == 0: return m.end()
    return None

def realexp(src):
    pat = re.compile(r"(?<![A-Za-z0-9_.'])(~?)(\d+(?:\.\d+)?)[eE](~?)(\d+)(?![A-Za-z0-9_])")
    def f(m):
        v = float(m.group(2) + 'e' + ('-' if m.group(3) else '') + m.group(4))
        s = ('%.17f' % v).rstrip('0')
        if s.endswith('.'): s += '0'
        return m.group(1) + s
    return pat.sub(f, src)

def inlinesig(src):
    out, pos = [], 0
    clean = strip_comments(src)
    for m in re.finditer(r"\b(structure|functor)\s+[A-Za-z0-9_']+\s*(?:\([^)]*\))?\s*:>?\s*(sig)\b", clean):
        if m.start() < pos: continue
        e = match_end(clean, m.start(2))
        if e is None: continue
        after = re.match(r"\s*=", clean[e:])
        if not after: continue
        colon = clean.rfind(':', m.start(), m.start(2))
        out.append(src[pos:colon]); pos = e
    out.append(src[pos:])
    return ''.join(out)

def ascribe(src):
    clean = strip_comments(src)
    out, pos = [], 0
    for m in re.finditer(r"\bstruct\b", clean):
        if m.start() < pos: continue
        e = match_end(clean, m.start())
        if e is None: continue
        a = re.match(r"\s*:>?\s*[A-Z][A-Za-z0-9_'.]*(\s+where\s+type[^;\n]*)?", clean[e:])
        if a and not re.match(r"\s*:>?\s*[A-Z][A-Za-z0-9_'.]*\s*=", clean[e:]):
            out.append(src[pos:e]); pos = e + a.end()
    out.append(src[pos:])
    return ''.join(out)

def tyvarseq(src):
    src = re.sub(r"\b(fun|val)\s+('[A-Za-z0-9_']*|\(\s*'[A-Za-z0-9_']*(\s*,\s*'[A-Za-z0-9_']*)*\s*\))\s+",
                 r"\1 ", src)
    return src

def abstype(src):
    clean = strip_comments(src)
    out, pos = [], 0
    for m in re.finditer(r"\babstype\b", clean):
        if m.start() < pos: continue
        e = match_end(clean, m.start())
        w = re.compile(r"\bwith\b").search(clean, m.start(), e)
        if e is None or w is None: continue
        out.append(src[pos:m.start()]); out.append('datatype')
        out.append(src[m.end():w.start()]); out.append(' ')
        out.append(src[w.end():e - 3]); pos = e
    out.append(src[pos:])
    return ''.join(out)

def joinctor(src):
    lines = src.split('\n')
    res, i = [], 0
    while i < len(lines):
        line = lines[i]
        if re.search(r"\bof\s*$|\bof\s+.*[*(]\s*(\(\*.*\*\))?\s*$", strip_comments(line)) and i + 1 < len(lines):
            # keep joining while the line ends inside a type (trailing * or `of` or open paren)
            buf = line
            while i + 1 < len(lines) and re.search(r"(\bof|\*|\(|->)\s*$", strip_comments(buf).rstrip()):
                i += 1
                buf = strip_comments(buf).rstrip() + ' ' + lines[i].strip()
            res.append(buf)
        else:
            res.append(line)
        i += 1
    return '\n'.join(res)

def bracerec(src):
    clean = strip_comments(src)
    out, pos = [], 0
    for m in re.finditer(r"(\bof|\btype\s+[^=]*=)\s*\{", clean):
        b = m.end() - 1
        depth, j = 0, b
        while j < len(clean):
            if clean[j] == '{': depth += 1
            elif clean[j] == '}':
                depth -= 1
                if depth == 0: break
            j += 1
        tail = re.match(r"[ \t]*(\*[^|\n;]*|[a-z][A-Za-z0-9_.']*([ \t]+[a-z][A-Za-z0-9_.']*)*)", clean[j+1:])
        if tail and tail.group(0).strip() and not re.match(r"\s*(and|val|fun|type|datatype|end|in|withtype)\b", clean[j+1:]):
            k = j + 1 + tail.end()
            out.append(src[pos:b]); out.append('(' + src[b:k].rstrip() + ')'); pos = k
    out.append(src[pos:])
    return ''.join(out)

def recpatty(src):
    def f(m):
        inner = m.group(2)
        if '=' in inner: return m.group(0)
        fields = re.sub(r"([A-Za-z_][A-Za-z0-9_']*)\s*:\s*[^,}]+", r"\1", inner)
        return m.group(1) + '{' + fields + '}'
    return re.sub(r"(\bfun\s+[A-Za-z0-9_']+\s*|\bfn\s*)\{([^{}]*)\}", f, src)

RULES = [('realexp', realexp), ('inlinesig', inlinesig), ('ascribe', ascribe),
         ('tyvarseq', tyvarseq), ('abstype', abstype), ('joinctor', joinctor),
         ('bracerec', bracerec), ('recpatty', recpatty)]

def main():
    src = open(sys.argv[1]).read()
    want = sys.argv[3:] or [r for r, _ in RULES]
    changed = []
    for name, fn in RULES:
        if name in want:
            new = fn(src)
            if new != src: changed.append(name)
            src = new
    open(sys.argv[2], 'w').write(src)
    print(' '.join(changed))

if __name__ == '__main__':
    main()
