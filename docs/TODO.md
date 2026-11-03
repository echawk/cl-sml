# Project Todo

- [ ] Continue SML97 conformance work.  The default pipeline is the native
  checker (`src/types/`) and the CL backend; HaMLet is only a test oracle
  (`cl-sml/hamlet-oracle`) and a large test program.
  - [x] Own corpora: `t/conformance/core` 63/63 and `t/conformance/modules`
    57/57 with `--native`.
  - HaMLet's conformance programs (`hamlet/test/*.sml`, judged by
    `hamlet/test/CONFORMANCE.txt`), default pipeline, 2026-11:
    - Must be accepted and run: 19/23 (was 17/23).  Fixed: `exhaustive`
      (a match on all 256 chars is exhaustive), `id` (symbolic type
      constructors such as `&`).  Failing: `abstype` and `fun-case` (notes
      4 and 5: infeasible together with `abstype2` / in general; HaMLet
      fails them too), `functor-poly` (HaMLet fails it too), `valrec`
      (parses now; `val rec LESS = fn ...` is rejected, "val rec cannot
      rebind constructor", where the Definition accepts it and raises Bind
      at run time, see its note 3).
    - Must be rejected or warned: 9/9.  `dec-strdec`: overloading is
      resolved per value declaration, so cases A to C are rejected like
      HaMLet does (allowed) and B'' and C' are rejected (required).
    - Matter of interpretation: `flexrecord2`, `undetermined2` rejected,
      as HaMLet does.
  - Real-world corpus (`t/realworld/run.sh`, `MODES=native`, 119 programs
    from external/benchmarks and LunarML's tests): 98/119 (was 96/119;
    fixed bm-mlyacc and ln-mlbasis-scan-word).  Failures by class:
    - Non-standard extensions (out of scope): SMLofNJ.Cont (cml-sieve,
      pingpong), Int54 (int54), numeric literals with `_` (int-inf) and
      hexadecimal reals (real, real-intinf), a `;` before `end` in a `let`
      body (dlx), datatypes and `type` declarations that use an enclosing
      type variable (core-equality, core-local_datatype; Successor ML,
      ruled out by Definition section 2.9), `Word.wordSize = 64` required
      (core-xorshift64).
    - Optional Basis structures not provided: Posix (plclub-ray),
      WideChar/WideString (widestring-*).
    - Program bug (also fails under the reference compilers): barnes-hut.
    - Word size: mandelbrot-rat assumes `Word.wordSize >= Int.precision`
      (`Word.~>>` of `Word.fromInt n` for n near 2^62); our word is 62
      bits.  The Basis allows any word size, so not a bug, but every
      common compiler has a word at least as wide as int.
    - Time-outs (performance, 120 s): count-graphs, fft, id-ray,
      mandelbrot, mc-ray, simple; aobench takes 27 s alone and can time out
      when six programs run in parallel.
  - Remaining, in priority order:
    1. `val rec` rebinding a constructor (HaMLet `valrec`): accept, and
       raise Bind at run time (Definition rules 26 and 126).
    2. Performance of the time-outs above (real arithmetic, arrays).
    3. Optional Basis structures (Posix subset, WideChar/WideString) if
       programs need them.
    4. A word as wide as int (63 bits) to match other compilers.
  - [x] Core tuples, unit, patterns, records, exceptions, local declarations,
    type annotations/metadata, file loading, package namespaces, and REPL.
  - [x] Load all files in `hamlet/basis/all.sml` and `hamlet/hamlet.sml`.
  - [ ] Load and run representative programs from `HOL/`.  `scripts/test-hol`
    loads 170 portableML, Holmake hfs, prekernel, src/0 and src/thm
    sources (with shims in `t/testdata/hol-shims/`), checked by the native
    checker (`t/hol-corpus.lisp`): everything up to the kernel, Lib, Count,
    Dep, Tag, Lexis, Context, KernelTypes, Type, Term, Net, Compute and Thm
    (std-thm), and runs checks that build types, terms and a theorem
    (`REFL`).  Next: `src/thm/Overlay.sml`, then `src/1` onwards.
    - Not attempted: HOLsexp_parser (ML-Yacc output), SHA1 (SHA1_ML),
      holpathdb (TOML), tools-poly Help/Mosml/quse (Poly/ML specific).
- [ ] Replace shallow module handling with explicit module/signature
  elaboration; retain CL packages as the external namespace substrate.
  With the HaMLet checker, structure members and constructor/variable
  status come from its elaboration (see below); without it, the parser
  tracks identifier status and fixity per package (`src/parser.lisp`),
  ascribed structures export what their signature's specifications
  name, and only names behind an `open` of an unknown structure fall back
  to guessing constructors from capitalization.
- [x] SML97 front end: a lexer (`src/lexer.lisp`) and a recursive-descent
  parser with scoped fixity, types, signatures and structure expressions
  as ASTs (`src/parser.lisp`).  Infix applications are pair applications.
- [ ] Distinguish word from int without a checker (with HaMLet, overloaded
  operators are specialized and word arithmetic is modular; without it `~`
  on words is special-cased only for literals).
- [x] SML97 static semantics via HaMLet (git submodule `hamlet/`, unmodified)
  through `t/hamlet-oracle/hamlet-check.sml` and `src/type-checker.lisp`.  The checker runs before
  our parser in `compile-sml-program-string`, file loading and the REPL;
  `(enable-hamlet-type-checker)` turns it on, and `scripts/build-repl.sh` preloads it
  into the `cl-sml` image.  The REPL prints HaMLet's inferred types.
- [x] Use HaMLet's elaboration in code generation.  `t/hamlet-oracle/hamlet-check.sml`
  reports facts keyed by source position (`src/static-facts.lisp`), which the
  parser and compiler consult, for files and REPL phrases alike:
  - identifier status decides constructor vs variable patterns
  - overloaded operators are compiled for their resolved type (modular
    word/word8 arithmetic, flooring `div`, char/string comparisons)
  - structure members are those of the elaborated, ascribed environment
  - datatype replication binds the replicated constructors
  - infix applications of user operators are compiled as pair applications
- [ ] Further uses of HaMLet's types: specialize `=` (vectors compare
  structurally), declare CL types from inferred types, and retire
  `register-sml-binding-type` / `infer-sml-ast-type` once the checker is
  always on.
- [x] Keep one checker session per SML package.
- [ ] Complete the Basis features required by HaMLet, especially TextIO input,
  files, OS paths, command-line handling, arrays, and vectors.
- [x] Emit inspectable Common Lisp from expressions, declarations, and files.
- [x] Show the Lisp generated for a binding after it was compiled
  (`sml-lisp-source`, `show-sml-lisp`; REPL `:lisp`), for a phrase without
  running it (`sml->lisp`; REPL `:expand`), echo it (`:show-lisp`), and
  `:disassemble`.  Records are kept per SML package with the binding's SML
  file and line; the HaMLet checker's own package is not recorded.
- [x] Report per-phase compile times (`with-sml-phase-timings`, REPL `:time`,
  `cl-sml --time`); `cl-sml --run FILE` and `--emit-lisp FILE`.
- [x] Emitted artifacts are deterministic (gensyms numbered per program),
  have one top-level form per declaration with a `;;; file.sml:LINE: fun f`
  comment, print in the SML package, and shadow SML names that collide with
  CL symbols.  The Lisp compiler's notes on generated code are hidden unless
  `*show-lisp-compiler-output*`.
- [ ] Add dependency manifests for `use`, full SML-to-CL source maps (below
  declaration level), and a compile-project API returning source, FASL and
  dependency paths.
- [ ] Cache the FASLs of loaded SML files, keyed by a hash of the source, the
  checker state and the cl-sml version.
- [x] Add basic CL interop through `sml-value`, `(setf sml-value)`,
  `sml-function`, `call-sml`, and `with-sml-package`.
- [ ] Add module handles, generated uncurried wrappers, and a conservative
  `with-sml-syntax` API.
- [x] Add `build-hamlet`, the intended preloaded `cl-hamlet` image recipe and
  executable smoke test.
- [x] Produce `cl-hamlet` on SBCL (`./build-hamlet`; needs the 1GB control
  stack that `build-hamlet` bakes in via `:save-runtime-options`).
- [-] (Dropped: `build-hamlet`/`cl-hamlet` were removed; HaMLet is test-only.) Preload/cache the elaborated basis in the `cl-hamlet` image so startup
  does not re-elaborate `hamlet/basis` each time.
- [ ] Portability (ECL, ABCL): self tail calls are compiled as jumps, but
  other tail calls (`fun even ... and odd ...`, CPS) still take a stack
  frame per call outside SBCL.  ABCL's interpreter (REPL, `:compile nil`)
  overflows its fixed thread stack at around a thousand nested SML calls.
- [ ] Evaluate `cl-iparse` as a future grammar implementation.
- [ ] Evaluate `rewrite-cl` for typed normalization and backend rewrites.
- [ ] Prototype an optional Coalton backend only after a backend-neutral typed
  IR exists; see `PLAN.md`.
  - Ensure this is entirely optional; based on whether we are able to typecheck (ie hamlet.sml exists).
