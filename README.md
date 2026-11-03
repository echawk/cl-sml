# cl-sml

cl-sml compiles Standard ML (SML97) to Common Lisp.  An SML program is read,
parsed, statically checked by cl-sml's own SML97 type checker (`src/types/`,
see docs/DESIGN-TYPECHECKER.md), translated to ordinary Common Lisp forms, and
then compiled and run by the host Lisp:

```
SML text -> parse -> static check -> SML -> Lisp -> compile Lisp -> load / run
```

## Checked and unchecked modes

`cl-sml:*sml-type-checker*` chooses the static checker that files, strings,
the REPL and `#@sml` blocks go through:

- `:native` (the default): the native checker.  Ill-typed programs are
  rejected before any code runs, with the position and the two types that
  clash (`SML-NATIVE-TYPE-ERROR`, a subtype of `SML-STATIC-TYPE-ERROR`).
  Non-exhaustive and redundant matches are warnings on `*error-output*`
  (`cl-sml:*native-match-warnings*` turns them off).  The code generator
  uses what the checker learned: identifier status, overloading resolved
  at `int`, `IntInf.int`, `Word8.word`, `real`, ..., and the members of
  structures after signature matching.
- `nil` (`(cl-sml:disable-type-checker)`, or `cl-sml --unchecked`): plain
  mode, no checking; the compiler guesses what a checker would tell it,
  e.g. overloading is resolved at run time.
- `:hamlet` (`(cl-sml:enable-hamlet-type-checker)`), test-only:
  [HaMLet](https://github.com/rossberg/hamlet) (commit 37275b9)'s
  elaborator, compiled by cl-sml and loaded on first use (some 30 s), the
  oracle the native checker is tested against.  It needs the `hamlet`
  submodule and the system `cl-sml/hamlet-oracle`
  (`t/hamlet-oracle/`): `(ql:quickload :cl-sml/hamlet-oracle)`.

A checker keeps one static basis per SML package, starting from the Basis:
each top-level declaration or REPL input that is accepted extends it, and
rebinding a name shadows it without changing code compiled earlier.

The generated Lisp is a supported artifact: it can be inspected, written to
files, compiled and shipped.  SML bindings live in per-program Lisp packages
(`SML-USER`, `SML.FILE.<path>`), with structure members as symbols such as
`|Parse.Parser.parse|`, so Lisp code can call SML code and vice versa.

## Building and running

Requirements: SBCL (ECL and ABCL are also supported) and Quicklisp; the
`hamlet` git submodule (`git submodule update --init hamlet`) only for the
`:hamlet` oracle and its tests; without it cl-sml builds and runs, and
`./test.sh` skips the HaMLet suite.  Put this repository where ASDF finds it
(e.g. Quicklisp's local projects), then:

```lisp
(ql:quickload :cl-sml)
(cl-sml:repl)                         ; an SML REPL; :help lists its commands
(cl-sml:load-sml-file #p"program.sml")
```

`scripts/build-repl.sh` builds `./cl-sml`, an executable with the checker's
Basis preloaded (`CL_SML_NO_TYPE_CHECKER=1` builds one that does not
check, `CL_SML_HAMLET=1` one that checks with the HaMLet oracle):

```
./cl-sml                          # the REPL
./cl-sml --run program.sml        # load and run a file
./cl-sml --emit-lisp program.sml  # print the Lisp generated for a file
./cl-sml --time --run program.sml # also report the time of each phase
./cl-sml --unchecked --run program.sml  # plain mode
```

Loading HaMLet needs a deep control stack: start SBCL with
`--control-stack-size 1GB` (the executable has it built in).

## The Basis Library

cl-sml's Basis Library is in `src/basis/`: SML sources derived from
HaMLet's (see `src/basis/all.sml` and `src/basis/LICENSE.txt`) over run-time
primitives (`use{b = "Name"}`, implemented by `sml-basis-primitive` in
`src/runtime.lisp`).  Compiling cl-sml compiles them once, into the package
`SML.BASIS` and the FASL of `src/basis.lisp`; every SML package sees the
basis's bindings, and its own bindings shadow them without changing them.
Both checkers elaborate the same sources as the initial basis of the
programs they check.

## SML in Lisp files

SML can be written inside Lisp code in `#@sml{ ... }#`
blocks.  Turn the syntax on for the rest of a file with

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (cl-sml:enable-sml-syntax (copy-readtable))))
;; or: (named-readtables:in-readtable cl-sml:sml-readtable)
```

```lisp
#@sml{ fun fib n = if n < 2 then n else fib (n - 1) + fib (n - 2) }#

(+ 1 #@sml{ 2 * 3 }#)                          ; => 7
(defun fib-of (n) #@sml{ fib ($n : int) }#)    ; $n is the Lisp variable N
(defun adder (k) #@sml{ fn x => x + $k }#)     ; (funcall (adder 10) 32) => 42
#@sml{ List.map (fn x => x * x) $(list 1 2 3) }#  ; => (1 4 9)
```

- An expression block is a Lisp expression with the SML value.  A block of
  declarations defines them in the SML package of the current Lisp package
  (`SML-USER` for `CL-USER`, `SML.FOO` for `FOO`); a block of declarations
  followed by an expression returns the expression's value.  From Lisp, the
  SML value `name` is the special variable `|name|` of that package, e.g.
  `(symbol-value (find-symbol "fib" "SML.FOO"))`.
- `$name` and `$(form)` splice Lisp values into the SML.  They are
  evaluated where the block is, in its lexical environment, every time it
  runs.  `$name` takes letters, digits and `-_*%!?`; write `$(x)` before
  other characters.  A `$` after a symbolic character (`<$>`) is SML.
  Blocks are type-checked.  A splice has a type of its own, which the SML
  around it determines; annotate it, `($x : int)`, to fix it (an
  overloaded operator on splices alone defaults to `int`).  HaMLet does
  not check blocks with splices.
- `}#` and `$` in SML strings, characters and comments are SML.
- The SML is compiled when the block is macroexpanded, so `compile-file`
  puts compiled code in the FASL and loading it does not compile SML.
  Declarations without splices are also evaluated at compile time, so that
  later blocks know their constructors, structures and fixity.  SML errors
  are reported with the Lisp file's name and line.
- Values are cl-sml's runtime values: integers, reals (double floats),
  strings, chars, `t`/`nil` for booleans, Lisp lists for lists, Lisp
  vectors for SML vectors.  SML functions are Lisp functions of one
  argument (curried functions return functions).  Tuples and records are
  simple-vectors whose slot 0 describes their labels; build and take them
  apart with the functions below rather than by hand:

  | Function | |
  |---|---|
  | `(sml-tuple a b ...)` | the tuple `(a, b, ...)`; `(sml-tuple)` is `()` |
  | `(sml-unit)` | `()`, the one value of type `unit` |
  | `(sml-tuple-ref tuple i)` | field `#i` (1-based) |
  | `(sml-tuple->list tuple)`, `(list->sml-tuple list)` | conversions |
  | `(make-sml-record '(("a" . 1) ("b" . 2)))` | the record `{a = 1, b = 2}`; labels are strings (or integers), in any order |
  | `(sml-record-ref record "a")` | field `#a` |
  | `(sml-record-fields record)`, `(sml-record-labels record)` | the record as an alist, its labels |
  | `(sml-tuple-p x)`, `(sml-record-p x)` | tuples and the unit are records too |
  | `(sml-constructed-tag x)`, `(sml-constructed-argument x)` | for `C v`, the symbol tagging `C`'s values and `v` |

  A tuple is the record with labels `1`, ..., `n`: `(sml-tuple 1 2)` is
  `{1 = 1, 2 = 2}`, and field `i` of a tuple is `(svref tuple i)`.  A
  nullary constructor is a symbol (`NONE` is the symbol `|NONE|` of the
  `SML.BASIS` package); `SOME 1` and other constructed values are opaque
  objects, so use `sml-constructed-tag` and `sml-constructed-argument`, or
  build them with the constructor's SML value, e.g.
  `(funcall (cl-sml:sml-value "SOME") 1)`.

`examples/embedded-sml.lisp` shows it all.

## Tests

`./test.sh` runs the fiveam suites in `t/` (set `LISP=ecl` or `LISP=abcl`
to use another implementation).  `scripts/test-hol` checks a HOL corpus.

## Seeing the generated Lisp

The Lisp generated for every top-level binding is recorded when it is
compiled, together with the SML file and line it came from:

```lisp
(cl-sml:sml-lisp-source "Foo.bar" :package "SML-USER") ; => list of forms
(cl-sml:show-sml-lisp "Foo.bar")    ; pretty-printed, with a location comment
(cl-sml:sml->lisp "fun f x = x + 1") ; Lisp for a phrase, not evaluated
(cl-sml:print-sml-lisp (cl-sml:sml->lisp "f 3"))
```

Expressions are compiled through a small typed IR (docs/IR.md), which a
simplifier rewrites first: it inlines small non-recursive top-level
functions and Basis primitives such as `Word.xorb`, beta-reduces, folds
constants and drops dead bindings.  `cl-sml:show-sml-ir` prints the IR of a
binding, as `show-sml-lisp` prints its Lisp:

```lisp
(cl-sml:show-sml-ir "Foo.bar")              ; simplified IR terms
(cl-sml:show-sml-ir "Foo.bar" :lowered t)   ; before simplification
(setf cl-sml:*sml-ir-simplify* nil)         ; compile without the simplifier
(setf cl-sml:*sml-ir* nil)                  ; compile without the IR
```

Forms print in the SML package, so SML names appear unqualified (`|bar|`)
and runtime helpers as `cl-sml::sml-+`.  `sml-lisp-source` includes the
helper functions hoisted out of large `case` expressions; with
`:enclosing t`, a binding declared inside `local ... in ... end` is shown
with the enclosing `local`.  Members of functor instances are found in the
functor.  Bind `cl-sml:*record-sml-lisp-source*` to `nil` to compile without
keeping the forms; `cl-sml:clear-sml-lisp-source` forgets them.

In the REPL:

```
- fun square x = x * x;
val square = <fn>
- :lisp square
;;; fun square -- stdIn:1
(progn
 (declaim (special |square|))
 (defparameter |square| ...))
- :expand square 4 + 1
(funcall (funcall #'cl-sml::sml-+ (funcall |square| 4)) 1)
```

| Command | |
|---|---|
| `:lisp NAME` | the Lisp generated for a binding, e.g. `:lisp Foo.bar` |
| `:lisp-local NAME` | the same, with its enclosing `local` declaration |
| `:ir NAME` | the IR compiled for a binding (docs/IR.md) |
| `:expand PHRASE` | the Lisp for an SML phrase, without running it |
| `:show-lisp on\|off` | print the Lisp of each phrase as it is evaluated |
| `:time on\|off` | report the time of each compilation phase |
| `:disassemble NAME` | disassemble the compiled function bound to `NAME` |
| `:help`, `:quit` | |

Commands are recognized only at the primary prompt, on lines starting with
`:` and a letter, which no SML phrase does.

## The Coalton backend (optional)

The system `cl-sml/coalton` compiles SML to [Coalton](https://github.com/coalton-lang/coalton)
instead of plain Lisp, through the typed IR (docs/IR.md, "The Coalton
backend").  Only that system depends on Coalton.  It covers the core
language and structures (no functors), and its Basis Library is
`src/basis` itself, emitted to Coalton when it is first needed.

```lisp
(ql:quickload :cl-sml/coalton)
(cl-sml:load-sml-via-coalton "val _ = print \"hello\\n\"")   ; compile and run
(cl-sml:load-sml-file-via-coalton #p"prog.sml")
(cl-sml:emit-coalton-sml-string "fun f x = x + 1")       ; the Coalton forms
(cl-sml:show-sml-coalton "fun f x = x + 1")              ; printed
(cl-sml:coalton-basis-report)        ; which Basis declarations Coalton has
```

Coalton type-checks what is emitted again.  Tests: `(asdf:test-system
:cl-sml/coalton)`; the corpora and benchmarks take `--coalton`
(`t/conformance/core/run.sh --coalton`, `t/conformance/modules/run.sh
--coalton`, `sbcl --script t/bench/run.lisp coalton`).

## Files of generated Lisp

`(cl-sml:emit-sml-file #p"program.sml" #p"program.lisp")` (or
`./cl-sml --emit-lisp`) writes a loadable Lisp file: it creates the SML
package, switches to it, and has one top-level form per SML declaration,
each preceded by a `;;; program.sml:LINE: fun name` comment.  The output
depends only on the program and its location, so it can be diffed.
`load-sml-file` compiles such a file and loads the result;
`cl-sml:*show-lisp-compiler-output*` shows what the Lisp compiler reports
about the generated code.

`(cl-sml:with-sml-phase-timings () (cl-sml:load-sml-file ...))` reports the
time spent in each phase: static check, parse, SML -> Lisp, writing and
compiling the Lisp, and loading or evaluating it.

## License

See `LICENSE`.
