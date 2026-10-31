# Design: a native type checker, and a Coalton backend

Status: milestone M1 implemented in `src/types/` (2026-10-31), opt-in through
`cl-sml:typecheck-sml-string`; M2-M4 are still design. This expands
ROADMAP.md items 18-20 (core elaboration, typed IR, modules elaboration) and
item 22 (Coalton backend).

M1 as built, where it settles what this document left open:

- Files: `objects`, `unify`, `env`, `print`, `initial`, `scope-tyvars`,
  `check-pattern`, `elab-core`, `annotations`, `typecheck` under
  `src/types/`, in package `cl-sml.types`. Tests: `t/typecheck-tests.lisp`.
  Differential against HaMLet: `t/conformance/core/native-diff.lisp`.
- Overloading is defaulted at the end of each top-level declaration (HaMLet
  defaults at the innermost value declaration). Flexible records must be
  resolved by the innermost value declaration, as in HaMLet. Undetermined
  types are an error at the end of the program, as in HaMLet.
- A type name may not be substituted for a type variable older than it
  (HaMLet's time stamps); `let` additionally checks its result type.
- `*native-relaxed-value-restriction*` generalises the variables of an
  expansive binding that occur only covariantly (list, option, vector and
  records are covariant; every other type constructor counts as invariant).
- `*native-unchecked-long-ids*` gives each long identifier a fresh type
  printed as `?`; without it a long identifier is an error, like structure,
  signature, functor and `open` declarations.
- Match warnings (`*native-match-warnings*`, default on) use Maranget's
  usefulness algorithm rather than a port of HaMLet's CheckPattern.
- Printing follows HaMLet's text but puts each binding on one line and
  omits the leading comment listing type names.

## 1. Goals

- Accept exactly the programs the Definition (SML '97) accepts, and reject the
  rest with a source position and a readable message.
- Replace HaMLet as the static front end, but keep HaMLet as a differential
  oracle in the test suite.
- Produce a typed program that later passes can use: resolved overloading, the
  type at every `=`, the full record type at every `#lab` and `...` pattern,
  and match exhaustiveness. ROADMAP items 13-16 consume this output.
- Be easy to grow. Each rule of the Definition has one obvious home in the
  code, so missing rules can be added one at a time as conformance work goes
  on. We do not need to get everything right in one pass.

## 2. Which algorithm

Textbook Algorithm W (Damas–Milner) threads an explicit substitution and
composes substitutions. Production ML compilers do not use it literally. They
use the imperative variant, often called Algorithm J:

- A type variable is a mutable cell (union-find). Unification links cells
  destructively instead of building substitutions.
- Every unification variable carries a *level* (or *rank*): the let-nesting
  depth where it was created. Generalisation at a `val` binding quantifies the
  variables whose level is deeper than the current one. This avoids scanning
  the whole environment for free variables, which W has to do (Rémy 1992;
  Kuan & MacQueen 2007 describe the SML/NJ version).

SML/NJ, MLton, Poly/ML and OCaml all work this way. HaMLet follows the
Definition's declarative rules almost one to one, but implements unification
the same way, with mutable type variables. It computes closure (generalisation)
by comparing against the free variables of the context, which is simpler and
slower than levels.

Coalton is Hindley–Milner plus Haskell-style type classes. As far as I recall,
its checker started from Jones's *Typing Haskell in Haskell* (substitutions and
predicates). Its soundness issue was the classic one: polymorphism together with
mutable cells (coalton issue #84, "Certain combinations of polymorphism and
mutation can lead to type-unsoundness"). It was closed in February 2026 by PR
#1750, which adds a relaxed value restriction with weak type variables. One
related issue is still open: #1454, "On recompilation, warn when callers are type
unsound". That one comes from Lisp-style redefinition in a live image, not
from inference. SML does not have this problem: our top-level binding cells
mean that rebinding a name never changes code compiled earlier (see section 7).

**Decision: Algorithm J with levels, following HaMLet's module structure.**

### Why not microKanren (cl-kanren) for unification

cl-kanren gives us first-order unification over persistent substitutions. It
would work for a prototype, but it is a poor fit for the real checker:

- SML type variables carry attributes that unification has to merge. These are
  the level, the equality flag (`''a`), the overloading class (`Int`, `Real`,
  `Word`, ... resolved by defaulting), and for flexible records a partial row.
  Attributes on mutable cells are trivial to handle. On logic variables they are
  awkward.
- Generalisation with a substitution needs the free variables of the
  environment (W-style). That is quadratic on long programs. Levels are
  linear.
- Error messages need the two types as they were when the clash happened.
  With mutable cells we already have them. With a substitution we would have to
  walk the result back.

The unifier is about 150 lines of Lisp. It is cheaper to write than to adapt.
cl-kanren could still be useful as a test oracle for the unifier itself
(random terms, compare most-general unifiers) if we want one.

## 3. Shape of the code

The code follows HaMLet's `elab/` directory, one Lisp file per group of
HaMLet files. Rule numbers from the Definition go in comments at each case
(HaMLet does the same), so a missing rule shows up as a missing number.

| Lisp file | HaMLet counterpart | Contents |
|---|---|---|
| `src/types/objects.lisp` | `StaticObjectsCore`, `TyName`, `Type`, `TypeFcn`, `TypeScheme`, `OverloadingClass` | Type names (stamp, arity, equality attribute, span), types, type variables with attributes, type functions, schemes |
| `src/types/unify.lisp` | parts of `Type` | Union-find, occurs check, attribute merging, level adjustment |
| `src/types/env.lisp` | `StaticEnv`, `Context`, `IdStatus` | VE/TE/SE/Env, contexts C = (T, U, E), lookups of long identifiers |
| `src/types/scope-tyvars.lisp` | `ScopeTyVars` | Implicit scoping of explicit type variables (Definition section 4.6) |
| `src/types/elab-core.lisp` | `ElabCore`, `Clos` | Rules 1-55: expressions, matches, patterns, declarations, value restriction (4.7), closure (4.8) |
| `src/types/check-pattern.lisp` | `CheckPattern` | Exhaustiveness and redundancy (4.11), as warnings |
| `src/types/elab-module.lisp` | `ElabModule`, `Sig`, `FunSig`, `StaticBasis` | Rules 56-100: structures, signatures, matching with realisation, functors, sharing, `where type` |
| `src/types/initial.lisp` | `InitialStaticEnv`, `InitialStaticBasis` | The top-level environment of Appendix C (bool, list, ref, exceptions, overloaded operators) |
| `src/types/print.lisp` | `PPType`, `PPStaticBasis` | Printing in HaMLet's format, so differential tests compare text |

Input: the AST from `src/parser.lisp`. The parser keeps derived forms
(`fun`, `case`, `if`, `andalso`, `while`, tuples, lists, `#lab`), and the
checker elaborates them directly using the Definition's Appendix A
expansions. That keeps error positions on the code the user wrote.

Output: an annotation table from AST node to static information:

- For each variable occurrence: identifier status, plus the instantiated type.
- For each overloaded operator or literal: the resolved type name.
- For each `=` and `<>`: the type compared.
- For each record selector and flexible record pattern: the full row.
- For each `fn`, `case` and `handle`: exhaustiveness and redundancy.
- For each declaration: the bound environment.

The table uses the same lookup functions as today's `src/static-facts.lisp`
(`sml-node-fact-info`, `sml-overloaded-instance`, the status predicates). The
compiler then switches from HaMLet to the native checker with a flag and no
other change. Today's facts are keyed by source offset because HaMLet
re-parses the text. The native checker keys by AST node, which is more robust.

## 4. The parts that need care

These are the places where simple HM checkers usually go wrong for SML. Each
one gets its own fiveam test group.

1. **Value restriction (4.7).** Only non-expansive expressions generalise. We
   need the SML '97 definition exactly, not OCaml's relaxed one, or we will
   accept programs that Poly and MLton reject. A relaxed mode could be an
   option later.
2. **Equality types.** Type names carry an equality attribute, which is
   computed for datatypes by a fixpoint over the constructors (4.9). `''a`
   variables only unify with types that admit equality. `ref` always admits
   equality. Functions and `real` never do.
3. **Overloading (Appendix E).** Overloaded identifiers and literals get a
   variable carrying an overload class. The class is defaulted (`int`, `real`,
   `word`) at the end of the enclosing top-level declaration if it is still
   unresolved. Classes come from the Basis we actually load, so `IntInf.int`,
   `Int32.int` and `Word8.word` take part. HaMLet's classes are fixed, which is
   why checked mode cannot use literals on `IntInf` today.
4. **Flexible records.** `#lab` and `{a, ...}` patterns leave a row variable.
   The checker resolves it from context before the end of the top-level
   declaration, or reports an error.
5. **Type-name generativity and escape.** `datatype` makes fresh names. A
   local datatype must not escape its scope (side condition of rule 17). Each
   functor application makes fresh names.
6. **Explicit type variables.** Explicit type variables are scoped implicitly
   at the innermost enclosing `val` (4.6), and a bound variable cannot be
   instantiated.
7. **Signature matching.** This means enrichment plus realisation, `where
   type`, `sharing`, opaque versus transparent ascription, and type
   specifications that admit equality. It is the largest piece. HaMLet's
   `Sig.match` is the model to follow.

## 5. The Basis problem, and the milestone order

Our Basis (`src/basis/`) is SML code using structures and signatures. The
native checker can only type `List.map` once it can elaborate modules. The
milestones are:

- **M1, core (ROADMAP 18).** Unifier, objects, environments, `elab-core`, and
  the Appendix C initial environment written by hand (about 40 entries). Long
  identifiers into structures are an error, or are allowed but flagged as
  unchecked by an option. Tests: the core conformance corpus programs that
  only use the top-level environment, plus targeted well-typed and ill-typed
  programs. Entry point: `cl-sml:typecheck-sml-string`, opt-in.
- **M2, modules (ROADMAP 20).** `elab-module` and signature matching. This
  milestone is done when the checker elaborates `src/basis` itself. At that
  point it has the whole initial basis, with no hand-written tables and no
  HaMLet.
- **M3, switch over.** The compiler takes static facts from the native checker
  by default. HaMLet runs only in the differential test target (whole
  corpora, compare accept/reject and printed signatures).
- **M4, typed IR (ROADMAP 19).** The optimizations of ROADMAP 13-16 come
  next, plus the Coalton backend (section 8).

This moves ROADMAP item 20 ahead of items 13-16, because the type-directed
optimizations are only useful once the checker covers real programs, and real
programs use the Basis.

## 6. Testing

- **Unit:** the unifier (occurs check, attribute merging, levels).
- **Rule tests:** one small program per rule, or per side condition, with
  the expected printed environment or the expected error.
- **Differential:** every corpus program (t/conformance, t/realworld) is
  checked by both the native checker and HaMLet. Accept/reject must agree, and
  for accepted programs the printed top-level environment must agree. Any
  disagreement is a bug in one of the two. HaMLet is more often right, but not
  always (see the strictness notes in t/realworld/REPORT.md).
- **Errors:** a small golden set of error messages, so they stay readable.

## 7. The REPL and incremental use

The checker keeps a persistent static basis. Each top-level declaration
elaborates against it and extends it, which is exactly how the Definition
models a program (rules 186-189). Rebinding a name shadows it. Code compiled
earlier keeps its old binding, both statically (its types are fixed) and
dynamically (binding cells). So the redefinition problem in coalton #1454
cannot happen here, unless we later add a "redefine in place" REPL feature,
which we should not.

## 8. Sketch: compiling SML to Coalton

Coalton is HM plus type classes, embedded in CL, and compiles to fast CL. A
typed SML program (output of M4) maps onto it fairly directly. Coalton
type-checks what we emit again, so the target has to be a program Coalton
accepts as well. That double check is a good test of our checker.

| SML | Coalton |
|---|---|
| `datatype t = A \| B of int` | `(define-type T A (B Integer))` (or `I64`, see below) |
| polymorphic datatype `'a tree` | `(define-type (Tree :a) ...)` |
| tuples | `Tuple`, nested for arity above 2, or a generated `define-struct` per arity |
| records `{x: int, y: real}` | a generated `define-struct` per record type, with a canonical label order |
| `fun f x y = ...` (curried) | a `define` of a curried function. After uncurrying (ROADMAP 9), a multi-argument `define` |
| `case`, `fn` with patterns | `match`. Decision trees (ROADMAP 11) if Coalton's `match` cannot express the nested patterns |
| `let val ... in ... end` | `let` |
| `'a ref`, `:=`, `!` | `Cell` with `cell:new`, `cell:write!`, `cell:read`. With the relaxed value restriction of #1750, every program SML accepts is accepted |
| `''a` equality type variables | an `Eq :a` constraint. Equality on a datatype becomes a derived `Eq` instance |
| overloading (`+` at int/real/word) | already resolved by our checker, so we emit the monomorphic operation and never need `Num` |
| `int` with `Overflow` | `I64` (or `Integer` plus a range check) with explicit overflow checks, because SML `int` must raise `Overflow` |
| `real`, `string`, `char`, `bool`, `unit` | `F64`, `String`, `Char`, `Boolean`, `Unit` |
| `exn`, `exception E of t`, `raise`, `handle` | `exn` is an extensible sum, which Coalton has no direct counterpart for. Lower to Lisp conditions behind a small `lisp` escape, or use Coalton's exception support if it covers payloads of arbitrary type |
| structures | flattened, with names prefixed by the structure path, or one CL package per top-level structure |
| signatures, opaque ascription | erased after checking. Abstract types become `repr :transparent` newtypes only where the type has to stay distinct for the program to make sense to Coalton |
| functors | defunctorised: each application is specialised by copying, as MLton does. Coalton has no module system, so this needs the whole program |
| polymorphic recursion, higher-rank | not in SML '97 either, so not an issue |

Caveats:

- Whole program only, because of functors and because Coalton wants its
  definitions to be known. The current backend stays for the REPL and for
  separate compilation.
- The result is fairly idiomatic Coalton, apart from exceptions, which are
  the one part of SML with no direct Coalton counterpart.
- The double check catches bugs. It also means any unsoundness in Coalton (the
  issue the user remembered) could make Coalton accept something our checker
  rejected. That is harmless, because our checker runs first.

## 9. Open questions

- Should a relaxed value restriction be an opt-in mode? It is not SML '97, so
  the default must stay strict.
- Should the checker warn about non-exhaustive matches by default? Poly and
  MLton do.
- Where should the typed IR live? It could be a separate AST copy with types,
  or the annotation table plus the existing AST. The table is the smaller
  change, and it is enough for ROADMAP 13-16.
