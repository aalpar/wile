# Core `let` in Compiler Design

Status: shipped. All four binding forms (`let`, `let*`, `letrec`, `letrec*`) are
core forms: expanded by `expander_let.go`, validated into a `validate.ValidatedLet`
carrying a `LetKind`, and compiled by `CompileValidatedLet` into
`OpPushEnv` / `StoreLocal` / `OpPopEnv`. The macro that expanded `let` into
`((lambda (x ...) body) val ...)` is gone from `bootstrap_macros.scm`.

This document is the design rationale behind that move: is it a Wile-specific optimization, or is it how compilers actually work? Who else does this? And what forced them to?

## The Pedagogical Illusion

Every Scheme textbook teaches that `let` is syntactic sugar for lambda application:

```scheme
(let ((x 1) (y 2)) (+ x y))
;; is equivalent to:
((lambda (x y) (+ x y)) 1 2)
```

This is true *semantically*. The R7RS spec defines `let` this way (Section 7.3, derived expressions). SICP's metacircular evaluator uses this definition. It's elegant — you don't need `let` as a primitive; lambda already handles binding.

But here's the thing: the R7RS spec is describing *what* `let` means, not *how* to implement it. The equivalence is a specification technique, not a compilation strategy. Confusing the two is one of the most common mistakes in language implementation.

## What Goes Wrong When `let` Expands to Lambda

Consider what the macro expansion did to `(let ((x 1)) x)`:

1. The macro expander rewrites it to `((lambda (x) x) 1)`
2. The compiler sees an application of a lambda literal
3. It emits: `SaveContinuation`, `Push 1`, `MakeClosure`, `Apply`
4. At runtime: allocate a closure, allocate an env frame, check arity, bind parameter, jump into the body
5. On return: `RestoreContinuation` restores the previous state

That's 5-6 dispatch cycles and a closure allocation for what should be "put 1 in a slot." Worse, the compiler *can't see the binding structure*. It sees a function call. The peephole optimizer can't help because the lambda is a literal, not a cached binding or a local reference — it doesn't match any fusion pattern.

But the performance cost isn't even the real problem. The real problem is information loss.

## The Information Destruction Problem

When `let` expands to lambda, the compiler loses three critical facts:

1. **These bindings are local and non-escaping** (unless a closure captures them). Lambda application doesn't encode this — it's just a call.
2. **The init expressions don't see the bindings** (for `let`; `let*` is sequential). After expansion, this constraint becomes invisible — it's enforced only because the macro happened to put the init expressions outside the lambda.
3. **The binding scope is bounded** — `let` bindings die at the end of the body. A general function call carries no such guarantee.

These facts are exactly what optimizers need. Constant propagation asks: "Is this binding always bound to a known value?" Dead code elimination asks: "Is this binding ever referenced?" Register allocation asks: "How long does this binding live?" If the compiler can't see the `let`, it can't answer these questions without reconstructing the information that was already there before macro expansion destroyed it.

This is why every compiler that does optimization keeps `let` (or something isomorphic to it) as a core form in its intermediate representation.

## The Landscape: Who Uses Core `let`?

### Production Scheme Compilers

**Chez Scheme** (Dybvig et al.) uses a nanopass architecture with ~50 intermediate languages. `let`, `letrec`, and `letrec*` are core forms that persist through most passes. The expander converts surface `let` into an internal representation before the compiler ever sees it. Optimizations like copy propagation, constant folding, and closure optimization all depend on seeing binding structure directly.

**Guile** (GNU's Scheme) compiles to Tree-IL, where `<let>`, `<letrec>`, and `<fix>` (for self-referential closures) are distinct node types. The Tree-IL then lowers to CPS, where bindings become `$kargs` continuation nodes. At no point does `let` exist as a lambda application.

**Chicken Scheme** compiles through CPS, where every `let` becomes a continuation that receives a value. The binding structure is explicit — the optimizer can see exactly which values flow into which names.

**Racket's** expander produces "fully expanded programs" where `let-values` is a core form — one of about 15 core forms that survive expansion. Surface `let`, `let*`, `letrec` all expand to core `let-values` or `letrec-values`. The bytecode compiler and JIT both operate on these core forms.

**Gambit** (Feeley) uses an internal representation where `let` is a node type. The compiler performs lambda-lifting, closure conversion, and CPS transform on this representation — all of which need to distinguish "local binding" from "general function call."

**Larceny** (Clinger et al.) similarly preserves `let` in its intermediate representation. The MacScheme compilation strategy it's based on emits direct slot operations for `let` bindings.

### Beyond Scheme

This isn't a Scheme-specific pattern. The design appears everywhere that compilers need to reason about bindings:

**GHC (Haskell)** — perhaps the most famous example. GHC's Core intermediate language has exactly two binding forms: `let` (non-recursive) and `letrec` (recursive). The entire optimization pipeline — inlining, specialization, strictness analysis, demand analysis, worker-wrapper — operates on Core, and all of it depends on `let` being a first-class construct. Simon Peyton Jones has written extensively about why Core's `let` is essential. When GHC encounters a Haskell `where` clause or pattern binding, it desugars to Core `let` — not to lambda application.

**OCaml** compiles to a Lambda IR where `Llet`, `Lletrec`, and `Lmutlet` are distinct node types. The compiler needs to distinguish non-recursive bindings (which can be reordered or eliminated) from recursive ones (which can't) from mutable ones (which block many optimizations).

**SML/NJ** (Standard ML of New Jersey) uses CPS as its IR, where `let`-like binding forms are explicit continuation applications — but crucially, they're *known* continuation applications, not arbitrary calls.

**LLVM** operates on SSA form, where every phi node is effectively a `let` binding. SSA was invented precisely to make the "which definition reaches this use?" question trivial — and that question is the binding question in disguise.

### Where Lambda-Expansion Survives

The macro-expansion approach survives in exactly two contexts:

1. **Pedagogical interpreters** — SICP's metacircular evaluator, university PL courses, "build a Lisp in 50 lines" blog posts. These are optimized for conceptual clarity, not for compilation.
2. **Interpreters with no optimization passes** — if you evaluate by tree-walking and never transform the AST, you don't need binding structure in the IR because you don't have an IR. The syntax tree *is* your representation, and the `let` semantics are encoded in the evaluation rule for lambda application.

Once you add *any* optimization pass — even something as simple as "don't allocate a closure for a non-escaping lambda" — you need to recover the binding information that expansion destroyed. At that point, you're doing pattern-matching on `((lambda (...) ...) ...)` to reconstruct what you already knew, which is strictly worse than just keeping the `let`.

## The Two Design Points

There are really two decisions, and they're independent:

**Decision 1: Where is `let` recognized — in the expander or the compiler?**

Some systems (Racket, Chez) have the *expander* recognize `let` and produce a core form. The compiler never sees `let` as a keyword — it sees a core IR node. Other systems could hypothetically have the compiler pattern-match on the lambda expansion to recover binding info.

Wile uses the first approach: the expander recognizes `let`, the validator produces `ValidatedLet`, and the compiler emits direct bytecode. This is the clean design — the information is preserved where it originates, not reconstructed later.

**Decision 2: What does the compiler emit — specialized binding ops, or generic call ops?**

Even if the compiler sees `let` as a core form, it could still compile it as a function call (just with metadata attached). The alternative is dedicated opcodes for binding: push an env frame, store into slots, pop the frame.

Wile uses `OpPushEnv` / `StoreLocal` / `OpPopEnv` — dedicated binding operations. This is also standard. Chez Scheme, Guile (in its VM), and Chicken all have binding-specific bytecodes or instructions. The reason is the same as before: if the VM can distinguish "this is a local binding" from "this is a function call," it can optimize accordingly (frame reuse, stack allocation, avoiding arity checks).

## The Deeper Pattern: Compilers Need Binding Structure

Step back and look at the major IRs in compiler design:

| IR | How `let` appears |
|----|-------------------|
| **CPS** | `(let ((x (car y))) ...)` becomes a continuation: `(lambda (x) ...)` applied to `(car y)` — but the compiler knows it's a *known* continuation, not an arbitrary call |
| **ANF** | `let x = car(y) in ...` — `let` is the *only* binding form. Every intermediate value gets a name via `let` |
| **SSA** | `x1 = car(y)` — assignment *is* binding. Phi nodes at join points are the SSA equivalent of `let` |
| **Core (GHC)** | `let x = car y in ...` — first-class `let` node |
| **Tree-IL (Guile)** | `<let>` node with bindings and body |

Notice the pattern? Every IR designed for optimization has an explicit binding construct. Some call it `let`, some call it "assignment," some encode it as continuation application — but the structure is the same: **"bind these names to these values in this scope."**

The lambda-expansion trick (`let` is just `((lambda ...) ...)`) collapses this structure into "call a function." That's fine for semantics — the meaning is preserved. But it's fatal for optimization, because "call a function" is the most general operation a compiler has, and the most opaque. The compiler can deduce very little from "call a function" without whole-program analysis. It can deduce a great deal from "bind x to 1 in this scope."

## Where Wile's Core `let` Sits

Wile occupies a specific point in this design space:

- **Not a tree-walking interpreter** — it compiles to bytecode, so it *has* an IR (the bytecode itself, plus the `Validated*` types)
- **Not doing ANF/CPS** — the `Validated*` types are a direct-style IR, not CPS or ANF
- **Foundation for further optimization** — core `let` is the prerequisite for ANF, constant propagation, and dead binding elimination. The first thing built on it was procedure inlining, which synthesizes a `ValidatedLet` at the call site; see [Procedure Inlining](inlining.md)

This is the standard progression. You can't do ANF without `let` — ANF *is* `let` all the way down. You can't do constant propagation without knowing which bindings exist. You can't do dead binding elimination without seeing bindings as first-class IR nodes.

The move took Wile from the "pedagogical interpreter" design (where `let` is a macro) to the "production compiler" design (where `let` is core). Every Scheme implementation that grew beyond educational use made this move at some point. It's not an optimization — it's infrastructure.

> For how ANF and CPS use `let` as their fundamental building block, see
> [CPS and ANF as Intermediate Forms](anf-and-cps.md).
> For why SSA (the non-Scheme equivalent) doesn't fit Wile's architecture, see
> [Would SSA Help the Wile Compiler?](ssa.md).

## Further Reading

- Appel, *Compiling with Continuations* (1992) — the canonical treatment of CPS, where `let` bindings become continuation lambdas
- Flanagan et al., "The Essence of Compiling with Continuations" (1993) — introduces ANF as `let`-based alternative to CPS
- Dybvig, "Three Implementation Models for Scheme" (1987) — early treatment of direct `let` compilation
- Peyton Jones, "Compiling Haskell by Program Transformation" (1996) — GHC's Core language and why `let`/`letrec` are the only binding forms
- Flatt, "Binding as Sets of Scopes" (2016) — the hygiene model, which determines *which* `let` bindings are visible where
- Keep and Dybvig, "A Nanopass Framework for Commercial Compiler Development" (2013) — Chez's compiler architecture where `let` persists through ~50 IR passes
