# cl-sml real-world corpus assessment

## Latest numbers (2026-11-01, after M3: flexible records, LargeInt, Date)

`MODES=native`, SBCL, 6 jobs, 120 s timeout, on a loaded machine (the
test suites ran alongside); plain mode was not rerun.

| mode | PASS | CHECKER | PARSE | RUNTIME | TIMEOUT | WRONG |
|---|---|---|---|---|---|---|
| native (119) | 95 | 9 | 4 | 3 | 7 | 1 |
| previous native (M3) | 90 | 13 | 4 | 3 | 9 | – |
| previous plain / HaMLet-checked | 86 / 62 | | | | | |

New passes: nbody and ln parser_combinator (flexible records now resolve
at the end of the top-level declaration, as in Poly/ML and MLton), ln
int-inf-rand and int64 (LargeInt is IntInf). mandelbrot-rat no longer
times out but prints 4032708 iterations against Poly's 4053935: it
computes with `Word.~>>` on 31-bit-truncated rationals, and our word
size (62 bits, Poly 63) differs. The 9 CHECKER results are all not SML97
or not our Basis: `SMLofNJ.Cont` (cml-sieve, pingpong), `Posix`
(plclub-ray), `Int54`, `WideString`/`WideChar` (2), `''a`/`'a` in a
local datatype or without a binding `val` (ln equality, local_datatype;
Poly rejects both too), barnes-hut (`testit : outstream -> unit`,
upstream BROKEN).

The harness's `run.sh` and `bin/refs.sh` listed `work/` in the harness
directory instead of `$RW_STATE`; fixed.

## Numbers after M3 (2026-11-01, native checker)

`MODES="plain native"`, SBCL, 6 jobs, 120 s timeout; `native` is the
native checker (src/types/), now cl-sml's default; `checked` (HaMLet) was
not rerun.

| mode | PASS | CHECKER | PARSE | RUNTIME | TIMEOUT |
|---|---|---|---|---|---|
| plain (119) | 86 | – | 4 | 21 | 8 |
| native (119) | 90 | 13 | 4 | 3 | 9 |
| previous: plain / HaMLet-checked | 81 / 62 | | | | |

Native passes 9 programs plain mode fails, all of them overloading at
IntInf (`+` etc. raised Overflow in plain mode: iter-pidigits, pidigits,
seki-bernoulli, the int-inf-* tests); plain passes 5 that native rejects.
Of the 13 CHECKER results then, four are fixed since (above): flexible
records (nbody, ln parser_combinator) and LargeInt = Int (ln
int-inf-rand, int64).

bm-aobench now runs (plain raised Overflow) but times out; ratio-regions
takes about 42 s in both modes alone and timed out once under load.
Compile times are mostly within noise of plain mode (black-scholes 28 s
both); the largest increases are kcfa 2.2 to 5.3 s and mlyacc 5.3 to
7.9 s, under a loaded machine.

## Original assessment (2026-10-29)

Baseline: master 53fdd69, SBCL. Assessed 2026-10-29.

## Setup

- **Corpus:** 41 SML/NJ benchmarks (`Main.testit`) and 78 single-file
  LunarML tests (`test/should_run`, `test/mlbasis/should_run`).
- **References:** Poly/ML 5.9.2 through `bin/polyrun.sml`, which keeps
  compiler warnings out of stdout. MLton (`-default-type int64`) is the
  fallback where Poly rejects or crashes (`InternalError asFPReg` on
  id-ray, ray, simple, nucleic; UTF-8 strings in sat). LunarML's
  `.stdout` files are also accepted.
- **SML/NJ-only names:** Real64, Real64Array, Unsafe, Fn, LibBase,
  Option.isNone and String.concatWithMap get portable shims in
  `compat/`, used by both the reference and the cl-sml runs.
- **Basis:** HaMLet's basis sources are preloaded into each program's
  package, because user packages have no Basis of their own.
- **Modes:** `plain` (no checker), `checked` (HaMLet checker), and
  `*-shim` (adds `compat/shim.sml`, a BinIO built on TextIO, to see past
  blocker 1).
- **Runs:** one process per program from a saved core, 120 s timeout,
  8 in parallel, so times are noisy.

## Summary

| mode | PASS | WRONG | PARSE | RUNTIME | TIMEOUT | CHECKER |
|---|---|---|---|---|---|---|
| plain (119) | 34 | 3 | 29 | 48 | 5 | – |
| checked (119) | 24 | 3 | 8 | 9 | 1 | 74 |
| plain-shim (41 benchmarks) | 10 | 0 | 16 | 11 | 4 | – |
| checked-shim (41 benchmarks) | 8 | 0 | 7 | 2 | 4 | 20 |

No program ended in COMPILE.

Benchmarks:

- plain: 10 of 41 pass (binary-trees, fannkuch, knuth-bendix, logic,
  mazefun, nucleic, safe-for-space, sat, stream-sieve, twenty-four).
- checked: 0 pass, because of BinIO (blocker 1). checked-shim: 8 pass.
- Not scorable: barnes-hut and dlx are marked BROKEN upstream and have
  no reference; cml-sieve and pingpong need `SMLofNJ.Cont` callcc and
  have only an MLton reference.

LunarML: 24 of 78 pass in both modes. int-inf, int54, real and
real-intinf are not SML97 (Poly and MLton both reject them). equality
and local_datatype are rejected by both HaMLet and Poly.

Plain mode passes more today (34 against 24). Checked mode is more
correct where it gets through (constructor/variable resolution and
overloading: ln word, ln signature4, boyer after workarounds). Four
things hold checked mode back: gaps in HaMLet's basis; HaMLet's
Definition-strict typing, which rejects 6 benchmarks that Poly accepts;
a cl-sml bug that crashes the compiled HaMLet on every `while`; and
speed. Measure fixes in both plain and checked-shim modes. The table
per program (class, compile and run seconds per mode, reference time) is
in `table.md` and `results.tsv`.

## Blockers, ranked by programs blocked

"First" means the program's first blocker. "+later" means programs that
hit it after earlier blockers were worked around (`bin/workaround.py`,
`nb-results.tsv`). Repros are in `min/`.

1. **BinIO is missing from HaMLet's basis and from the runtime.**
   `hamlet/basis/all.sml:92-93` comments it out, and
   `sml-basis-primitive` (`src/runtime.lisp:1232`) has no BinIO
   primitives. It is the first blocker for 36 benchmarks in checked mode
   (through `util/log.sml`); in plain mode aobench, id-ray, ray and
   mc-ray fail when they call it. Fix: BinIO over `(unsigned-byte 8)`
   streams, plus a BIN_IO signature. The shim alone gives 8 more passes.
2. **`structure X : sig ... end = struct` parses as a structure alias**
   when the signature contains `=`. `sml-structure-alias`
   (`src/parser.lisp:799`) is tried before `sml-structure` in `sml-decs`
   (`:827`); its ascription scan stops at the first `=` inside the sig
   and takes `= A` as the alias target (`min/sig-dt2.sml`). First
   blocker in both modes for black-scholes, delta-blue, mc-ray, minimax,
   tsp and vliw. With a workaround, minimax passes, and delta-blue
   passes in checked mode.
3. **Types are skipped as text to end of line.** `sml-type-blob`
   (`parser.lisp:557`) and `sml-type-decl-rhs` (`:709`) break
   constructor types over several lines, `of {..} * t`,
   `type t = {..} list`, and a one-line
   `let datatype t = T of int in ... end`, which swallows the `in`
   (`min/a1.sml`, `min/type-brace-list.sml`, `min/let-datatype-T.sml`).
   First blocker: boyer, plclub-ray, mlyacc, ln local_datatype. +later:
   delta-blue, kcfa, count-graphs, tsp, vliw, tyan.
4. **`val rec` is matched without a word boundary.** `sml-val-rec`
   (`parser.lisp:1261`) is `"val" ws "rec"` with `ws` allowed to be
   empty, so `val recx = 1` binds `x` (`min/val-rec-prefix.sml`). This
   miscompiles HaMLet's `WHILEExp` (`hamlet/parse/DerivedFormsCore.sml:300`,
   `val recbind`), and the checker built by cl-sml then crashes on every
   program that contains `while` ("variable ...::recbind is unbound",
   `min/while-top.sml`). First blocker in checked mode: lexgen, vliw,
   ln fib_linear, ln fizzbuzz. Latent in kcfa, plclub-ray, mlyacc and
   ratio-regions (they contain `while`), and in delta-blue, cml-sieve
   and pingpong (`val rec...` names).
5. **HaMLet's Definition-strict elaboration rejects programs that Poly
   and MLton accept** (checked mode only): "type mismatch on
   application" (overloaded `+ * /` default to int within the smallest
   enclosing declaration) and "unresolved flexible record type". HaMLet
   built with MLton gives the same errors (`hamlet-ref.tsv`). First
   blocker: nucleic, nbody, mc-ray, simple, tsp, id-ray, ln
   parser_combinator. Fix: fall back to unchecked compilation on these
   two errors, or resolve overloading per top-level declaration.
6. **Missing Basis structures**, both in HaMLet's static basis and in
   the runtime: IntInf (iter-pidigits, smith-nf, pidigits and 7 ln
   tests); Word16/32/64 and Int8/16/32/64 (13 ln tests, plclub-ray,
   aobench); RealArray (aobench, fft); TextIO.lookahead (ray);
   Real.realFloor (barnes-hut). IntInf is cheap (Lisp integers); WordN
   and IntN can be masked integers that raise Overflow.
7. **User infix declarations are ignored.** Alphanumeric infix names
   are a hard-coded HaMLet list (`parser.lisp:954-976`, `:1268-1275`),
   so `infix 9 over; ~2 over 1` becomes an application ("-2 is not of
   type FUNCTION", `min/alpha-infix-fun.sml`); `infixr` associativity is
   ignored (`min/user-infixr.sml` prints 3, Poly prints 7). First
   blocker: mandelbrot-rat, ln overflow. +later: life (`at`).
8. **Missing record pattern and expression forms.**
   `sml-pat-record-field` (`parser.lisp:1056`) has no `lab : ty` or
   `lab as pat`; `sml-record-field` (`:857`) rejects `lab = e : ty`
   (`min/record-pat-typed.sml`, `min/layered-in-record.sml`,
   `min/record-exp-typed.sml`). First blocker: kcfa, pidigits, ray,
   barnes-hut, ln signature3, ln seki-bernoulli.
9. **No explicit type-variable binders** `fun 'a f` / `val 'a x`
   (`parser.lisp:1341`, `:1252`, `min/fun-tyvar.sml`). Blocks
   count-graphs, ln listrev, ln prime_as_typevar.
10. **Real literals have no exponent part.** `sml-real`
    (`parser.lisp:385`) reads `1.5e01` as `1.5` applied to `e01`
    (`min/real-exp.sml`). First blocker: nbody, id-ray. Exponent
    literals appear in 12 corpus files.
11. **Module forms**, about 9 programs: `struct ... end : S` after the
    body (ln signature1, 2, 5; `min/struct-ascribe-after.sml`); a
    functor applied to `(struct ... end)` loses its members, `S.f`
    unbound (ln functor1, `min/functor-struct-arg.sml`); a functor body
    `let ... in struct ... end end` (ln functor3); a nested structure
    that refers to an outer one of the same name leaves the member
    unbound (`min/nested-struct-shadow.sml`; aobench, plclub-ray);
    `open A A.B` shadows in the wrong order (ln open); functor-parameter
    members are unresolved at run time (kcfa `K.compare`, dlx);
    exceptions are not generative per functor application in plain mode
    (`min/functor-exn-gen.sml`).
12. **abstype has no parser rule** (`min/abstype.sml`). Blocks life and
    ln abstype; +later mlyacc.
13. **`Real.toString` prints 17 significant digits, not 12**
    (`sml-real-to-string`, `src/runtime.lisp:1159`). This is the only
    remaining difference for black-scholes (all 4096 values agree within
    1e-9) and f-arith.
14. **`fun` clause heads are misread.** `sml-fun-clause`
    (`parser.lisp:1330`) tries the infix forms first, so
    `fun copy ~1 = ...` and `fun (A :: C) D E` take `~` and `::` as the
    function name. Blocks tyan and ln fun.
15. **Plain mode guesses constructor or variable** (checked mode gets it
    right). Lowercase constructors and lowercase exceptions in handlers
    fail to parse (lexgen; later boyer, count-graphs). `T.NULL` through
    a structure alias becomes a variable (`min/qual-ctor-alias.sml`;
    tsp).
16. **SML names that equal COMMON-LISP symbols.** Packages
    `:use "COMMON-LISP"` (`src/runtime.lisp:28`) and names are interned
    there (`compiler.lisp:162`), so `let val T = 5` compiles to
    `(LET ((T 5)))` (`min/local-T.sml`). Blocks simple and ln
    local_datatype.
17. **Word and Int arithmetic** (`src/runtime.lisp:1032-1039`, the Word
    primitives, `compile-overloaded-operation` at `compiler.lisp:1310`).
    Word results are not wrapped, so `Word8.- (0w0, 0w1)` then hangs in
    `Word8.toString` (ln word8 times out in both modes). `Word8.~>>`
    does not sign-extend, and a huge `Word.<<` count crashes (ln
    word_shift). `Word.wordSize` is 62; Poly has 63 and MLton and
    LunarML 64 (ln xorshift64). Int never raises Overflow (ln overflow).
18. **Polymorphic equality is CL `equal`** (`src/runtime.lisp:856`), so
    `vector [1,2] = vector [1,2]` is false (ln datatype-equality).
19. **Non-ASCII bytes in a string literal crash the checker.** In the
    HaMLet lexer as compiled by cl-sml, a bare exception tag reaches code
    that expects a tag-plus-payload pair (`min/utf8-string.sml`). Blocks
    sat in checked mode.
20. **Smaller runtime bugs:** `Char.fromString "\\uFEFF"` returns SOME
    instead of NONE (ln char); a floating-point trap on the NaN path (ln
    scan-real); spurious Subscript (ln string-issubstring) and Div (ln
    scan-word); an unexplained top-level pattern failure (ln general);
    `Char.chr 300` does not raise Chr; ratio-regions gives a different
    result after 214 s.

### Next blockers after workarounds (`nb-results.tsv`)

boyer: PASS (checked). minimax: PASS (plain, 113 s). delta-blue: PASS
(checked). black-scholes: WRONG, only blocker 13. count-graphs: plain
stops on blocker 15; checked runs past 120 s. kcfa: plain stops on 11
(`K.compare`); checked times out in the checker. life: stops on 7
(`at`). mlyacc: plain stops on abstype inside a functor (12); checked
times out in the checker. nbody: `Real.fmt` unbound at run time.
pidigits: uncaught `Empty`. tsp: stops on 15. vliw: uncaught
`NAMESETTONODESET`. id-ray, mc-ray: time out.

## Missing Basis (direct qualified uses; programs per structure)

| structure or ids | programs | which |
|---|---|---|
| BinIO (openOut, output, output1, closeOut, flushOut) | 41 | all benchmarks |
| IntInf | 11 | 3 benchmarks + 8 ln |
| Word64 | 8 | aobench, plclub-ray + 6 ln |
| Word32 | 3 | dlx + 2 ln |
| Word16 | 3 | 3 ln |
| Int8, Int16, Int32, Int64 | 1 each | ln tests |
| Real.fmt (in HaMLet's signature, no runtime value) | 3 | nbody, barnes-hut, ln real-intinf |
| Real.realFloor, Real.realMod | 2 each | barnes-hut, plclub-ray, ln real |
| other Real.* (maxFinite, minPos, toManExp, split, rem, realCeil, ...) | 1–2 | ln real, ln real-intinf |
| RealArray | 2 | aobench, fft |
| Array2 | 2 | plclub-ray, simple |
| TextIO.lookahead | 1 | ray |
| Time | 1 | kcfa |
| WideString, WideChar | 2 | ln tests |
| SMLofNJ.Cont | 2 | cml-sieve, pingpong |

`bin/basis-usage.py` regenerates this table. Its earlier TSV output had
a wrong `hamlet_static` column and was dropped; the script is fixed but
has not been re-run.

## Performance

Plain mode unless noted. Poly's time includes its compile.

| program | cl-sml run | reference | slowdown |
|---|---|---|---|
| fannkuch | 19.0 s (checked-shim 9.3 s) | 0.14 s | 65–135× |
| logic | 33.5 s (checked-shim 21.7 s) | 0.47 s | 45–70× |
| minimax (workaround) | 113 s | 1.19 s | ~95× |
| f-arith | 121 s | 1.5 s | ~80× |
| ratio-regions | 214 s, WRONG | 0.26 s | ~800× |
| mandelbrot | over 120 s | 30 s | over 4× |
| simple | over 108 s | 6.2 s (MLton) | over 17× |

Compile-time outliers: nucleic takes about 10 s alone (parse 6.1 s, SBCL
compile 3.6 s) and 25.8 s under parallel load; ln capture takes 78 s for
310 lines (parse 59 s). Checker time: mlyacc over 120 s, kcfa 67 s, tyan
54 s, delta-blue 34 s, knuth-bendix 32 s, against 0.3–0.8 s in plain
mode. HaMLet built with MLton takes under 1 s on each of these.

Causes: every binary operator allocates a closure in plain mode
(`(funcall (funcall #'sml-+ a) b)`); every argument tuple is a fresh
`(list :tuple ...)` matched by trivia; top-level functions are special
variables called through `funcall`; there are no type declarations, and
equality is generic `equal`; the PEG parser backtracks heavily; and
HaMLet compiled by cl-sml is 30–100× slower than HaMLet built with
MLton. See `docs/ROADMAP.md`.

## Harness

State (work dirs, SBCL image, caches, results) goes to `$RW_STATE`,
which defaults to the harness directory, so set it to a directory
outside the repo. The cl-sml checkout comes from `CLSML_ROOT` (default:
the repository that contains this directory). The image is rebuilt
when `src/`, `hamlet/basis/` or `cl-sml.asd` is newer than it.

```sh
cd t/realworld
RW_STATE=/some/dir ./run.sh            # assemble, references, image, plain+checked, table
RW_STATE=/some/dir MODES="plain checked plain-shim checked-shim" ./run.sh bm-
RW_STATE=/some/dir NORUN=1 ./run.sh    # reprint the table; also JOBS, TIMEOUT, REBUILD, SKIP_DONE
bin/try.sh min/real-exp.sml checked    # one file: class, stdout, backtrace
```

Requires sbcl with Quicklisp, poly, mlton, perl and python3.
`bin/hamlet-ref.sh` also needs a reference HaMLet: copy `hamlet/` to
`$RW_STATE/hamlet-ref/hamlet` and run `make with-mlton` there.
