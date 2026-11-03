# The typed IR

`src/ir.lisp` holds a typed intermediate language between the checked AST
and Common Lisp (ROADMAP items 19, 12 and 11).  `compile-expr` compiles
each SML expression, and `compile-fn-clauses-of-arity` each function of
clauses (`fun`, top-level, structure-level or local), in three steps:

1. `ir-lower` (`ir-lower-fn-clauses`) turns the AST into an IR term, using
   the checker's annotations (`src/types/annotations.lisp`, through
   `src/static-facts.lisp`).
2. `ir-simplify` rewrites the term (below).  `cl-sml:*sml-ir-simplify*`
   set to `nil` turns this step off.
3. `ir-emit` turns the term into CL.

`cl-sml:*sml-ir*` set to `nil` turns the IR off: `compile-expr-direct` and
`compile-fn-clauses-of-arity-direct`, the former code generator, then
compile everything directly.  Declarations (top-level, structures,
functors) are compiled by `compile-decl` as before; their expressions and
functions go through the IR, and the IR declarations they make are
recorded on request (see "Modules").

## Terms

A term is a list `(kind type . fields)`.  `type` is the node's SML type
(below); in plain mode, which has no checker, it is `nil`.

| kind | fields | |
|---|---|---|
| `:const` | value | integer, word, char, string, real; `t`/`nil` for `true`, `false`, `nil` |
| `:unit` | | `()` |
| `:var` | symbol | a lexical variable |
| `:ref` | form | a global binding: its static cell (`\|f;2\|`, ROADMAP 7), or a built-in such as `#'sml-not` |
| `:prim` | op, args | a primitive resolved by type: `(:overloaded "+" "int")`, `(:equality eql negated)`, `(:binary sml-assign)`, `(:basis "Word.xorb")`, `(:not)`, `(:deref)`, `(:list)` |
| `:tuple`, `:record` | (labels,) args | vectors (ROADMAP 10, 15), labels in canonical order |
| `:select`, `:select-label` | index or label, arg | `#lab e` at a known index, or looked up |
| `:con` | repr, tag, args | a constructor applied: `:boxed` `(tag . arg)`, `:flat` `#(tag f1 ... fn)`, `:flat-construct`, or `:cons` (tag computed, in functor bodies) |
| `:app` | f, arg | unknown call of a one-argument function |
| `:call` | worker, args, cell | known call of the tupled worker of a top-level function (ROADMAP 9) |
| `:fn` | params, body, name | a function of `(length params)` arguments; its type has a tuple argument for several |
| `:let`, `:letrec` | binding(s), body | `letrec` binds functions (local `fun`, `val rec`) |
| `:seq`, `:if`, `:while`, `:raise` | | |
| `:handle` | body, ((pattern body) ...) | |
| `:match` | scrutinees, ((patterns body) ...), failure | `case`, `fn`, `fun` clauses (one scrutinee per curried argument) and pattern bindings; failure is `(exception where offset)`, raised when no rule matches |
| `:loop` | tag, variables, inits, body | the loop of a function with self tail calls (ROADMAP 8) |
| `:tail-call` | (tag . variables), args | a self tail call: assign the loop's variables and restart it |
| `:decl` | declaration, body | a local `datatype` (`(:datatype ((symbol tag argp name) ...))`), `exception`, exception alias, or `(:module form)` |

There is no `:lisp` node any more.  The one escape hatch left is
`(:decl (:module form) body)`: a structure or functor declared inside a
`let` or `local` expression is compiled by `compile-decl` like any
declaration, and its CL form is kept.  `:ref` holds the CL form that reads
a global (a cell, or a built-in function).

Variables are uninterned symbols; each binder binds a symbol of its own,
so the passes need no environment for scoping.  Global bindings are
referred to through their static cells, which are bound once and never
assigned: what the simplifier knows about a cell stays true.

### Patterns

Patterns are IR data too, `(kind type . fields)`; `ir-emit-pattern` turns
them into trivia patterns.  Lowering takes every decision `compile-pat`
takes and records it:

| kind | fields |
|---|---|
| `:p-wild`, `:p-var` | -, symbol and representation (`:real` for a double-float) |
| `:p-const` | value (`t`/`nil` for `true`, `false` and `nil`) |
| `:p-tuple` | patterns |
| `:p-record` | width, ((index label pattern) ...) |
| `:p-record-lookup` | ((label pattern) ...), when the layout is unknown |
| `:p-as`, `:p-cons`, `:p-ref` | |
| `:p-exn` | name, form of the exception, payload pattern or `nil` |
| `:p-con` | name, how, data, payload pattern or `nil` |

`how` says how a constructor's values are recognized: nullary ones by
`:eq-tag` (the tag symbol), `:eql-form`, `:bound-eql`, `:value-eql` or
`:tag-p`; applied ones by `:boxed`, `:flat` (tag and arity), `:functor-tag`
(a constructor of the functor body being compiled), `:unknown-repr`,
`:variable` (held by a functor argument's variable) or `:runtime`.

### Emission

`ir-emit` reproduces what the direct generator made: a match with one rule
whose patterns cannot fail binds with `let*` (ROADMAP 4; now for every
such match, not only `fun` clauses); a match on literals is a CL `case`
whose actions are thunks; a match of 12 or more rules on the first field
of a tuple is a `case` on that field whose groups are hoisted into
global functions (taking the variables in scope that they use, computed at
emission); others are `trivia:match` or, for several scrutinees,
`trivia:match*`.

## Types

IR types do not refer to the checker's objects (`src/ir-types.lisp`
converts them):

| type | |
|---|---|
| `(:con tycon arg ...)` | `tycon` an `ir-tycon`: name, arity, equality (`t`, `nil`, or `:ref`), the constructors of a datatype as `(name . takes-argument-p)`, and the checker's stamp |
| `(:tyvar id name equality)` | a type variable |
| `(:arrow domain range)` | |
| `(:record ((label . type) ...) open)` | labels in canonical order; a tuple has labels `1`..`n`, `unit` none |

Types are instantiated (an occurrence of a polymorphic identifier has its
instance), overloading is resolved (an overloaded type variable left
unresolved stands for its default), and type abbreviations are expanded,
as the checker leaves them.  The native checker records the type of every
expression and pattern node (`annotate-type` in
`src/types/elab-core.lisp`) and of each `fun`'s clauses (the function's
type); nodes the lowering makes for itself take their types from their
parts.  `ir-type-string` prints a type in SML syntax.

Inline templates keep their types (made independent of the session by
`ir-portable-types`).  The Basis is compiled without the checker, so its
templates, and the nodes they bring into a simplified term, are untyped.

### The type lint

`ir-lint` checks a term: every node and pattern has a type (in native
mode), and types fit together: function and argument against the
application, the test and branches of `if`, the body of `let`,
`letrec`, `loop` and `decl`, the rules and patterns of `match` and
`handle`, tuples and selections, function bodies.  Type variables are
compatible with any type (inlined polymorphic code keeps its own).  In a
simplified term, untyped nodes are allowed (Basis templates), and one type
constructor may meet another: inlining a function from a structure
ascribed opaquely exposes the representation of an abstract type.

`cl-sml::*sml-ir-lint*` set to `:error` (or the environment variable
`CL_SML_IR_LINT=error` when cl-sml is loaded) lints every lowered and
simplified term and signals the problems; `:collect` gathers them in
`cl-sml::*sml-ir-lint-problems*`.  The whole SBCL suite passes with
`CL_SML_IR_LINT=error ./test.sh`, and the compiler test
`ir-type-lint-accepts-the-benchmarks-and-corpora` lints the benchmarks
and the Core and Modules corpora.

## Modules

`(cl-sml:sml-ir-program source :package name)` compiles a program (without
running it) and returns its IR declarations, in order; the compiler
records them whenever `cl-sml::*sml-ir-program*` holds a list.
Structures are flattened: each member is a declaration named by its path,
and the structure's own declaration follows its members.

| declaration | |
|---|---|
| `(:val ((path cell type) ...) terms [:patterns ((pattern index ((variable . cell) ...)) ...)])` | `val`, `val rec`, `fun` (and `and` groups): the names bound, their cells and types (schemes' bodies), and the simplified IR terms compiled, trivial ones included; for a `val` whose pattern is not a variable, its IR pattern, the index of its expression among the terms, and the cells of the pattern's variables |
| `(:datatype ((path arity params ((con-path type flat-arity) ...)) ...) terms)` | each constructor's type (an arrow from its argument), and the width of its flat representation (0 for boxed or nullary) |
| `(:type ((path params body) ...))` | |
| `(:exception path nil type cell)`, `(:exception-alias path target type cell)` | |
| `(:structure path :signature sigexp :opaque bool :types ((name arity kind params body constructors) ...))` | the types as the signature exports them: an abstract type's body is a type constructor of its own without constructors |
| `(:structure-alias path source)` | |
| `(:functor name :ast ast)` | the functor's AST, for a whole-program pass to specialize its applications (ROADMAP 21) |
| `(:functor-application path functor argument :signature sigexp :opaque bool)` | |
| `(:signature name sigexp)`, `(:open names)` | |

Functor bodies are compiled as before (one closure per functor, applied at
run time); their declarations are not recorded separately.

## The simplifier

`ir-simplify` runs up to `*sml-ir-simplify-rounds*` (3) rounds.  Each
takes a census of the uses of each variable, then rewrites the term once:

- **Inlining.**  A top-level `fun` of one clause whose parameters are
  variables (or a tuple of variables), that is not recursive and whose
  simplified term has at most `cl-sml:*sml-ir-inline-size*` (40) nodes
  records an inline template on its cell (`ir-note-inline-template`; also
  written to the compiled file, so the Basis's templates, e.g. `Int.max`,
  reach programs).  A call through that cell, curried or through the
  tupled worker, is replaced by a renamed copy.  A rebinding of the name
  makes a new cell, so code compiled before it keeps the old function.
  Functions bound by `let` to an `fn` are inlined when used once or small.
- **Basis primitives.**  A cell bound by `val x = use{b = "Word.xorb"}`
  records the primitive's name; its applications to a pair become a
  `:prim` that emits open code (`logxor` on declared words, shifts,
  `toInt`, `fromInt`, `String.sub`, `String.size`, `Char.ord`,
  `Char.chr`).  `not` becomes `(not x)`; `(f o g) x` becomes `f (g x)`.
- **Beta reduction.**  `(fn x => b) a` is `let x = a in b`; the arguments
  of an inlined tupled function bind its parameters in order; a call of a
  `let` is moved into its body.
- **Lets.**  A variable bound to a variable or a constant is substituted;
  an unused binding of a pure term is dropped (an impure one is kept for
  its effect); `let x = (let y = a in b) in c` is flattened.  `#i` of a
  tuple bound in the term takes the field.
- **Matches.**  A match whose first rule's patterns are variables or
  wildcards becomes lets; a match on `true`/`false` becomes an `if`.
  **Case-of-known-constructor:** when every scrutinee is a constant, a
  variable, or a tuple or constructed value the simplifier knows (bound in
  the term, with trivial fields), the rules that cannot match are dropped
  and the first that must match is taken, its pattern variables bound to
  the known parts.  This covers every datatype, tuples, constants and
  nested patterns.
- **Constant folding** of `int` and word arithmetic and comparisons, char
  comparisons, `=` at eqtypes, `not`, and the pure word primitives on
  literals.  An `int` result outside the range of `int` is not folded, so
  `Overflow` is still raised at run time; `div`, `mod` and the primitives
  that raise (`Char.chr`, `String.sub`) are never folded.
- **Known conditions.**  `if` on a constant takes its branch; `if not c`
  swaps the branches.

Pure means without effects and without exceptions: constants, variables,
cells, functions, tuples, records and constructors of pure terms, and the
primitives that cannot raise.  Int arithmetic can raise `Overflow`, so it
is not pure.

Pattern matching is still compiled by trivia, rule by rule; decision trees
(ROADMAP 11) were not added (see the ROADMAP for why).

## Seeing the IR

```lisp
(cl-sml:show-sml-ir "k")                ; the simplified terms of binding k
(cl-sml:show-sml-ir "k" :lowered t)     ; as lowered, before simplification
(cl-sml:sml-ir-program "...")           ; a program's IR declarations
```

prints the terms compiled for a binding's declaration, in the order they
were compiled, each node followed by its type:

```
;;; fun k -- line 2
(:fn (arg20)
 (:prim (:overloaded "+" "int")
  ((:let x32
    (:prim (:overloaded "+" "int") ((:var arg20 :type "int") (:const 1 :type "int")) :type "int")
    (:prim (:overloaded "*" "int") ((:var x32 :type "int") (:var x32 :type "int")) :type "int")
    :type "int")
   (:const 6 :type "int"))
  :type "int")
 "k" :type "int -> int")
```

for `fun sq x = x * x  fun k n = sq (n + 1) + 2 * 3`.

## What a Coalton emitter needs, and has

The IR is the input of a Coalton backend (ROADMAP 22,
docs/DESIGN-TYPECHECKER.md section 8).

Has:

- every construct of the core language as IR, with no CL inside except a
  structure declared in a `let`;
- the type of every node (native mode), instantiated and with overloading
  resolved, in a representation of its own (type constructors with
  arity, equality and constructors; variables; records; arrows);
- patterns as data, with constructor names and how values are tested;
  `:match` with several scrutinees for curried clauses;
- datatypes (constructor types and arities), exceptions, type
  abbreviations, structures flattened by path, the types a signature
  exports (abstract ones recognisable), functors and their applications,
  as IR declarations (`sml-ir-program`).

Still to do for Coalton:

- a whole-program driver: Coalton wants every definition known, so
  functor applications must be specialised (defunctorised) from the
  recorded ASTs, and the Basis compiled with the checker so that its
  terms are typed;
- `:let` binders and `:fn` parameters carry no type of their own (their
  uses do; the `:fn` type gives the parameters');
- polymorphism: types inside a let-polymorphic function carry its type
  variables, but the IR does not mark which are generalised (a scheme);
  Coalton infers that itself;
- exceptions, `ref` and the `:prim` operations need a mapping to Coalton
  (section 8 of the design note);
- the representation decisions (`:boxed`/`:flat`, tuple workers) are made
  during lowering; a Coalton emitter ignores them and uses the names and
  types.
