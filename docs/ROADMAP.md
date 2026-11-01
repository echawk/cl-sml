# Roadmap: native type checker and optimizations

This file lists work that would make cl-sml faster and less dependent on
HaMLet. Items are in order from least to most difficult. Each item names
what it needs first. Conformance gaps are in `todo-sml-conformance.md`.
That file also has the detailed design for the native type checker (its
item 0); this file only places that work in the order.

Difficulty: **S** is a local change of a few hours, **M** is a few days,
**L** is a week or more, and **XL** is a multi-week project.

## Where we start

cl-sml does no optimization today. `src/compiler.lisp` translates each
SML construct directly. This is what it emits for
`fun fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)`, with
package prefixes removed:

```lisp
(declaim (special |fib|))
(defparameter |fib|
  (named-lambda "sml:fib" (#:arg454)
    (trivia:match #:arg454
      (#:n455
       (if (funcall (funcall #'sml-< #:n455) 2)
           #:n455
           (funcall (funcall #'sml-+ (funcall |fib| (funcall (funcall #'sml-- #:n455) 1)))
                    (funcall |fib| (funcall (funcall #'sml-- #:n455) 2)))))
      (_ (error "Match failure in function ~A ~S on value ~S" "fib"
                '(... the whole SML AST of fib ...) #:arg454)))))
```

Every arithmetic operator allocates a closure (`sml-+` is
`(lambda (a) (lambda (b) (+ a b)))`, `src/runtime.lisp:798`). Every call
reads a special variable. Every function keeps its own AST as data for
the error message. A tuple is a tagged list `(:tuple a b)`. A record is
an alist with string keys, and `#a r` searches it by string. A nullary
constructor test is `(and (boundp 'Leaf) (eql x (symbol-value 'Leaf)))`.

Baseline, measured 2026-10-29 on SBCL: `fib 30` takes 6.0 s and conses
217 MB in cl-sml, including compile time. Poly/ML takes 0.18 s.

Use `external/bench/run` and the SML/NJ suite in `external/benchmarks/programs` to measure
every change below. Keep the result checks stable.

## Easy: local rewrites in the compiler, no types needed

### 1. Call binary primitives directly (S)

**Today:** `a + b` compiles to `(funcall (funcall #'sml-+ a) b)`. That
allocates a closure for each arithmetic operation or comparison.

**Do:** when the operator resolves to the Basis primitive (it is not
shadowed, and HaMLet's identifier status agrees when the checker is on),
emit a two-argument call such as `(sml-add a b)`. Keep the curried
functions for first-class uses such as `op +` or `foldl (op +)`.

### 2. Test nullary constructors with `eq` (S)

**Today:** the compiler matches `Leaf` with a `boundp` test and a
`symbol-value` lookup on every test.

**Do:** when the constructor resolves at compile time, emit
`(eq x 'Leaf)`.

### 3. Small match-failure branches (S)

**Today:** each function quotes its whole SML AST into its failure
branch, and calls CL `error`, so SML code cannot handle it.

**Do:** call one runtime function that raises the SML `Match` exception
(or `Bind` for `val`), passing the function name and source position.
This also closes conformance item 8.

### 4. Bind irrefutable patterns directly (S)

**Today:** every function body goes through `trivia:match`, even for a
plain variable. `fun curry a b c = ...` builds `(list :tuple a b c)` on
every call only to take it apart again.

**Do:** when a clause has only variables, wildcards and tuples of them,
emit `let` bindings, with no match and no failure branch.

### 5. Less metadata per binding (S)

**Today:** each top-level binding also emits
`(register-sml-binding-type 'f ':unknown)` and its own `export` form.

**Do:** leave out `:unknown` registrations, and emit one `export` per
compilation unit.

### 6. An explicit optimization policy (S)

**Do:** declare an `optimize` policy at the top of each emitted artifact.
Choose it so that SBCL keeps merging tail calls. Make debug builds
selectable.

## Moderate: local analyses, still no types needed

### 7. A static cell for each top-level binding (M)

**Today:** each top-level `val` or `fun` is a special variable. A later
binding of the same name overwrites it, so earlier code sees the new
value. This is a conformance bug as well as a cost:

```sml
val x = 1   fun g () = x   val x = 2   (* g () must be 1; cl-sml gives 2 *)
```

**Do:** give each top-level definition its own cell, for example a fresh
symbol `|x;2|`. Code compiled later refers to the cell that is in scope.
`sml-value` and the REPL map the name to the newest cell. After that,
emit known top-level functions with `defun`, so CL compilers make direct
calls without reading a special variable.

### 8. Self-tail calls as loops (M)

Compile the tail calls of a self-recursive function into a loop. ECL and
ABCL do not eliminate tail calls, so without this a tail-recursive SML
loop runs out of stack. This is also a portability fix, and it is in
progress on the portability branch.

### 9. Uncurrying and flattening tuple arguments (M)

**Do:** emit a worker function and a wrapper for each known function.
`fun f a b c` gets a 3-argument worker plus the curried wrapper, and a
call that supplies all arguments calls the worker directly.
`fun add (x, y)` gets a 2-argument worker, and `add (1, 2)` calls it
without allocating the tuple. Needs item 7 to know which function a
call reaches.

### 10. Cheaper representations for tuples and constructors (M)

**Do:** represent tuples as simple vectors, not tagged lists, so access
takes constant time. Represent constructed values with a small fixnum
tag, or as structs. Keep these working: polymorphic equality, the rule
that `{1=a, 2=b}` is the same value as `(a, b)`, and the CL interop
functions (`sml-value`, `call-sml`).

### 11. Pattern matching as decision trees (M)

**Do:** compile `case`, `fn` and `fun` clauses into decision trees
(Maranget, "Compiling pattern matching to good decision trees", ML
Workshop 2008), so each subterm is tested once, with a `case` on
constructor tags. First check how much `trivia` already gives after
items 2 and 10.

### 12. Inlining and simplification (M)

**Do:** inline small Basis functions (`not`, `o`, `!`, `:=`, `fst`) and
small user functions, beta-reduce `(fn x => e) a`, and drop unused pure
bindings. This is simpler on an IR (item 19), but the first pass can
work on the AST.

## Type-directed: needs the type of each expression

These items need types at each use. A first version can get them from
HaMLet: extend `t/hamlet-oracle/hamlet-check.sml` to report types by source
position, as it already does for overloaded operators. The native
checker (items 16 to 18) gives them directly later.

### 13. Specialize arithmetic and implement `Overflow` (M)

**Today:** generic `+` is CL `+` on integers. Without the checker, an
`int` result never overflows: `4611686018427387903 + 1` returns
`4611686018427387904`, but Poly/ML raises `Overflow`.

**Do:** compile `int` operations as fixnum operations with an overflow
check that raises `Overflow`, `real` operations as `double-float`, and
`word` operations as modular arithmetic. Choose `Int.precision` once for
all Lisps. With the checker, the compiler already specializes overloaded
operators; extend that to the case without the checker, and to every
use.

### 14. Specialize equality (M)

**Today:** `=` and `<>` use `equal` at all types (`src/runtime.lisp:857`).

**Do:** use `eql` for int, char and word, and `string=` for string.
Generate one equality function per datatype. Compare refs and arrays
with `eq`.

### 15. Records at fixed offsets, plus type declarations (L)

**Do:** represent a record as a vector with its labels in sorted order,
so `#a r` becomes `svref` at a constant index. This needs the full label
set at every selector and at every flexible record pattern. In the same
pass, emit CL declarations from the inferred types (`fixnum`,
`double-float`, `simple-vector`) so that SBCL can generate native
arithmetic and keep floats unboxed.

### 16. Exhaustiveness and redundancy checks (M)

This needs the datatype of each scrutinee, from HaMLet or from item 18.

**Do:** warn about matches that are not exhaustive or have redundant
rules (Definition section 4.11). Leave out the failure branch where the
match is exhaustive. Use the usefulness algorithm from Maranget,
"Warnings for pattern matching" (JFP 2007).

## Native type checker

The design is in `todo-sml-conformance.md`, item 0. The order and the
prerequisites are below.

### 17. A real front end (L)

**Today:** the parser keeps types as text, skips signature bodies, and
skips structure headers by scanning characters
(`src/parser.lisp:548-830`). Infix precedence is a fixed grammar.

**Do:** write a lexer that follows Definition chapter 2. Write a parser
for the full Core and Modules grammar with scoped fixity
(`infix`, `infixr`, `nonfix`). Produce real ASTs for types, signatures
and structure expressions, and expand the derived forms of Appendix A.
Test it against HaMLet's parser: on every corpus file, both must accept
or reject the same programs. This also closes conformance items 1 to 5.
Every item below needs this one.

### 18. Core elaboration (L)

**Do:** implement items 0.1, 0.2 and 0.4 of `todo-sml-conformance.md`:
unification with levels, let-polymorphism and the value restriction,
records and flexible records, datatypes, exceptions, explicit type
variables, overloading defaults, and equality types. Output a typed AST
that the compiler reads in place of `src/static-facts.lisp`. Run HaMLet
as a differential oracle in the tests.

### 19. A typed IR between the AST and CL (L)

**Do:** add a small typed intermediate language (A-normal form is
enough) between the typed AST and CL. Move items 9 to 15 onto it as
separate passes, so that they compose and can be tested one at a time.

### 20. Modules elaboration (XL)

**Do:** implement item 0.3 of `todo-sml-conformance.md`: signatures,
realisations, enrichment and signature matching, transparent and opaque
ascription, `where type`, `sharing`, and generative functors. Then make
the native checker the default. Keep HaMLet as an optional oracle, and
remove `register-sml-binding-type` and the position-keyed static facts.

## Hardest: whole-program optimization

### 21. Whole-program optimization (XL)

**Do:** as MLton does: apply functors at compile time (defunctorize),
use control-flow analysis to find the target of each call, contify
local functions, convert closures, and monomorphize hot polymorphic
code. Item 19 and a whole-program build mode are needed first.

### 22. Optional Coalton backend (XL)

See `PLAN.md`. This needs items 18 to 20. The backend must stay
optional, and the plain CL backend must keep working without it.
