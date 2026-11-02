# The typed IR

`src/ir.lisp` holds a small typed intermediate language between the
checked AST and Common Lisp (ROADMAP items 19 and 12).  `compile-expr`
compiles each SML expression in three steps:

1. `ir-lower` turns the AST into an IR term, using the checker's
   annotations (`src/types/annotations.lisp`, through
   `src/static-facts.lisp`) exactly as the code generator did before.
2. `ir-simplify` rewrites the term (below).  `cl-sml:*sml-ir-simplify*`
   set to `nil` turns this step off.
3. `ir-emit` turns the term into CL.

`cl-sml:*sml-ir*` set to `nil` turns the IR off: `compile-expr-direct`, the
former code generator, then compiles every expression directly.  Without
the simplifier, the emitted CL is the same as the direct generator's, up
to `and`/`or` becoming `if` and `let` gaining `(declare (ignorable ...))`.

## Terms

A term is a list `(kind type . fields)`.  `type` is the checker's type of
the node where the checker records one (identifier occurrences and
constants); it is `nil` (unknown) for other nodes and in plain mode, where
there is no checker.  The type is carried for later passes and for
`show-sml-ir`; no pass needs it yet, because what the passes need
(resolved overloading, equality kind, record layout) is resolved during
lowering.

| kind | fields | |
|---|---|---|
| `:const` | value | integer, word, char, string, real; `t`/`nil` for `true`, `false`, `nil` |
| `:unit` | | `()` |
| `:var` | symbol | a lexical variable |
| `:ref` | form | a global binding: its static cell (`|f;2|`, ROADMAP 7), or a built-in such as `#'sml-not` |
| `:prim` | op, args | a primitive resolved by type: `(:overloaded "+" "int")`, `(:equality eql negated)`, `(:binary sml-assign)`, `(:basis "Word.xorb")`, `(:not)`, `(:deref)`, `(:list)` |
| `:tuple`, `:record` | (labels,) args | vectors (ROADMAP 10, 15), labels in canonical order |
| `:select`, `:select-label` | index or label, arg | `#lab e` at a known index, or looked up |
| `:con` | repr, tag, args | a constructor applied: `:boxed` `(tag . arg)`, `:flat` `#(tag f1 ... fn)`, `:flat-construct`, or `:cons` (tag computed, in functor bodies) |
| `:app` | f, arg | unknown call of a one-argument function |
| `:call` | worker, args, cell | known call of the tupled worker of a top-level function (ROADMAP 9) |
| `:fn` | params, body, name | a function of `(length params)` arguments |
| `:let`, `:letrec` | binding(s), body | `letrec` binds functions (local `fun`, `val rec`) |
| `:seq`, `:if`, `:while` | | |
| `:raise`, `:handle` | | handler patterns as compiled trivia patterns |
| `:match` | scrutinee, branches, failure | `case`, `fn` and pattern bindings; patterns are compiled trivia patterns, bodies are terms |
| `:tail-call` | target, args | a self tail call, as a jump (ROADMAP 8) |
| `:lisp` | form | CL from the direct generator, for what the IR does not cover |

Variables are the uninterned symbols the direct generator uses for
lexical bindings, so a `:lisp` node can refer to the variables of the
term around it.  Each binder binds a symbol of its own, so the passes
need no environment for scoping.  Global bindings are referred to through
their static cells, which are bound once and never assigned: what the
simplifier knows about a cell stays true.

## What the IR covers

Everything in expressions except, for now:

- `case` on literals and the tuple dispatch of large `case`s
  (`compile-literal-case`, `compile-tuple-dispatch-case`);
- `let` declarations other than `val`, `fun`, `val rec`, `type` and
  `infix` (`datatype`, `exception`, `open`, `local`, structures,
  `val ... and ...`);
- `#lab` and overloaded operators used as values;
- the clauses of `fun` (top-level, structure-level and local), which the
  direct generator still matches (`compile-fn-clauses-of-arity`, with the
  irrefutable bindings of ROADMAP 4 and the tail loops of ROADMAP 8).

These become `:lisp` nodes; the expressions inside them (clause bodies,
case actions) are compiled through the IR again, as terms of their own.
Declarations (top-level, structures, functors with their current scheme)
are compiled as before; their expressions go through the IR.  Plain mode
uses the IR too: lowering follows the same code as the direct generator,
which already consulted the checker only when there is one.

## The simplifier

`ir-simplify` runs up to `*sml-ir-simplify-rounds*` (3) rounds.  Each
takes a census of the uses of each variable, then rewrites the term once:

- **Inlining.**  A top-level `fun` of one clause whose parameters are
  variables (or a tuple of variables), that is not recursive, whose body
  the IR covers and whose simplified term has at most
  `cl-sml:*sml-ir-inline-size*` (40) nodes records an inline template on
  its cell (`ir-note-inline-template`; also written to the compiled file,
  so the Basis's templates, e.g. `Int.max`, reach programs).  A call
  through that cell, curried or through the tupled worker, is replaced by
  a renamed copy.  A rebinding of the name makes a new cell, so code
  compiled before it keeps the old function.  Functions bound by `let` to
  an `fn` are inlined when used once or small.
- **Basis primitives.**  A cell bound by `val x = use{b = "Word.xorb"}`
  records the primitive's name; its applications to a pair become a
  `:prim` that emits open code (`logxor` on declared words, shifts, `toInt`,
  `fromInt`, `String.sub`, `String.size`, `Char.ord`, `Char.chr`).  `not`
  becomes `(not x)`; `(f o g) x` becomes `f (g x)`.
- **Beta reduction.**  `(fn x => b) a` is `let x = a in b`; the arguments
  of an inlined tupled function bind its parameters in order; a call of a
  `let` is moved into its body.
- **Lets.**  A variable bound to a variable or a constant is substituted;
  an unused binding of a pure term is dropped (an impure one is kept for
  its effect); `let x = (let y = a in b) in c` is flattened.  `#i` of a
  tuple bound in the term takes the field.  A pattern match whose first
  rule's pattern is a variable or a wildcard becomes a `let`; a match on
  `true`/`false` becomes an `if`.
- **Constant folding** of `int` and word arithmetic and comparisons, char
  comparisons, `=` at eqtypes, `not`, and the pure word primitives on
  literals.  An `int` result outside the range of `int` is not folded, so
  `Overflow` is still raised at run time; `div`, `mod` and the primitives
  that raise (`Char.chr`, `String.sub`) are never folded.
- **Known conditions.**  `if` on a constant takes its branch; `if not c`
  swaps the branches.

Pure means without effects and without exceptions: constants, variables,
cells, functions, tuples, records and constructors of pure terms, and the
primitives that cannot raise (comparisons, word arithmetic, real
arithmetic).  Int arithmetic can raise `Overflow`, so it is not pure.

Not done: case-of-known-constructor for datatypes other than `bool` (the
patterns are compiled trivia patterns; doing it needs the pattern ASTs
kept in the term), and inlining of functions with several clauses.

## Seeing the IR

```lisp
(cl-sml:show-sml-ir "k")                ; the simplified terms of binding k
(cl-sml:show-sml-ir "k" :lowered t)     ; as lowered, before simplification
```

prints the terms compiled for a binding's declaration, in the order they
were compiled, with each known type after its node's fields:

```
;;; fun k -- line 2
(:prim (:overloaded "+" "int")
 ((:let x76 (:prim (:overloaded "+" "int") ((:var n61 :type "int") (:const 1)))
   (:prim (:overloaded "*" "int") ((:var x76) (:var x76))))
  (:const 5)))
```

for `fun sq x = x * x  fun k n = sq (n + 1) + (fn z => z + 1) 4`.

## Towards Coalton

The IR is the natural input of a Coalton backend (ROADMAP 22,
docs/DESIGN-TYPECHECKER.md section 8): its nodes map onto Coalton's forms
(`:fn` to `fn`, `:match` to `match` once patterns are kept as ASTs, `:let`
and `:letrec` to `let`, `:prim` to the monomorphic operations, `:con` to
the constructors of `define-type`).  Before that, every node needs its
type (types of compound nodes are not recorded yet) and the remaining
`:lisp` fallbacks must be lowered.
