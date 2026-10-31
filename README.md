# cl-sml

cl-sml compiles Standard ML (SML97) to Common Lisp.  An SML program is read,
statically checked (optionally, by [HaMLet](https://github.com/rossberg/hamlet)'s
SML97 elaborator), parsed, translated to ordinary Common Lisp forms, and then
compiled and run by the host Lisp:

```
SML text -> static check (HaMLet) -> parse -> SML -> Lisp -> compile Lisp -> load / run
```

The generated Lisp is a supported artifact: it can be inspected, written to
files, compiled and shipped.  SML bindings live in per-program Lisp packages
(`SML-USER`, `SML.FILE.<path>`), with structure members as symbols such as
`|Parse.Parser.parse|`, so Lisp code can call SML code and vice versa.

## Building and running

Requirements: SBCL (ECL and ABCL are also supported), Quicklisp, and the
`hamlet` git submodule (`git submodule update --init hamlet`).  Put this
repository where ASDF finds it (e.g. Quicklisp's local projects), then:

```lisp
(ql:quickload :cl-sml)
(cl-sml:enable-hamlet-type-checker)   ; optional: SML97 type checking, ~30s
(cl-sml:repl)                         ; an SML REPL; :help lists its commands
(cl-sml:load-sml-file #p"program.sml")
```

`./build-repl.sh` builds `./cl-sml`, an executable with the type checker
preloaded (`CL_SML_NO_TYPE_CHECKER=1` builds one without it):

```
./cl-sml                          # the REPL
./cl-sml --run program.sml        # load and run a file
./cl-sml --emit-lisp program.sml  # print the Lisp generated for a file
./cl-sml --time --run program.sml # also report the time of each phase
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
HaMLet's checker elaborates the same sources as the initial basis of the
programs it checks.

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
  Blocks with splices are not checked by `*sml-type-checker*`; annotate a
  splice, `($x : int)`, to say what its type is meant to be.
- `}#` and `$` in SML strings, characters and comments are SML.
- The SML is compiled when the block is macroexpanded, so `compile-file`
  puts compiled code in the FASL and loading it does not compile SML.
  Declarations without splices are also evaluated at compile time, so that
  later blocks know their constructors, structures and fixity.  SML errors
  are reported with the Lisp file's name and line.
- Values are cl-sml's runtime values: integers, reals (double floats),
  strings, chars, `t`/`nil` for booleans, Lisp lists for lists, `(:tuple
  a b)` for tuples, `(:record ("a" . 1) ...)` for records.  SML functions
  are Lisp functions of one argument (curried functions return functions).

`examples/embedded-sml.lisp` shows it all.

## Tests

`./test.sh` runs the fiveam suites in `t/` (set `LISP=ecl` or `LISP=abcl`
to use another implementation).  `./test-hol` checks a HOL corpus.

## Seeing the generated Lisp

The Lisp generated for every top-level binding is recorded when it is
compiled, together with the SML file and line it came from:

```lisp
(cl-sml:sml-lisp-source "Foo.bar" :package "SML-USER") ; => list of forms
(cl-sml:show-sml-lisp "Foo.bar")    ; pretty-printed, with a location comment
(cl-sml:sml->lisp "fun f x = x + 1") ; Lisp for a phrase, not evaluated
(cl-sml:print-sml-lisp (cl-sml:sml->lisp "f 3"))
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
| `:expand PHRASE` | the Lisp for an SML phrase, without running it |
| `:show-lisp on\|off` | print the Lisp of each phrase as it is evaluated |
| `:time on\|off` | report the time of each compilation phase |
| `:disassemble NAME` | disassemble the compiled function bound to `NAME` |
| `:help`, `:quit` | |

Commands are recognized only at the primary prompt, on lines starting with
`:` and a letter, which no SML phrase does.

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
