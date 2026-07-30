# CPS and ANF as Intermediate Forms — Would They Help Wile?

The previous document ([SSA for Scheme Compilers](ssa.md)) argued that SSA doesn't fit Wile's architecture. But SSA comes from the world of C and Fortran compilers. Scheme has its *own* intermediate representations, developed specifically for languages with first-class continuations and closures:

- **CPS** (Continuation-Passing Style) — Steele 1978, Appel 1992
- **ANF** (A-Normal Form) — Flanagan et al. 1993

These are the IRs that real Scheme compilers use. Chez Scheme, Gambit, and Chicken all use CPS. Guile uses CPS internally. The question isn't whether these *could* work for Scheme — they were invented for Scheme. The question is whether they'd help *this* Scheme compiler, targeting *this* VM.

## What CPS Does

Consider a simple function call:

```scheme
(+ (car x) (* y 2))
```

In direct style (how you write it, how Wile compiles it), this means "evaluate `(car x)`, evaluate `(* y 2)`, add them." The "and then" relationship is implicit — it's just the evaluation order.

CPS makes every "and then" explicit by passing a *continuation function* to each operation:

```scheme
;; CPS transform of (+ (car x) (* y 2))
(car x (lambda (a)          ;; "after you get car of x, call me with the result"
  (* y 2 (lambda (b)        ;; "after you get y*2, call me with the result"
    (+ a b k)))))            ;; "after you add them, pass result to k"
```

Every function takes an extra argument: the continuation that receives its result. There are no "returns" — only calls. A function call in tail position is just... a call. A function call in non-tail position passes a continuation that does the remaining work.

This is powerful because:
1. **Tail calls become obvious** — they're calls where the continuation is the one you were given, not a new lambda
2. **All control flow is uniform** — `if`, `call/cc`, function calls, and returns are all just function calls
3. **Intermediate values get names** — each lambda parameter names a result

But notice the cost: the original expression had 3 operations. The CPS version has 3 lambda allocations. Those lambdas need closure conversion, environment handling, and allocation. For a compiler targeting native code (where lambdas become jump targets, not heap objects), this is fine. For a bytecode interpreter where lambdas mean actual closures...

## What ANF Does

ANF was developed in 1993 by Flanagan et al. specifically because CPS was overkill for many compilation purposes. Their key observation: **you don't need to name the continuation — you just need to name the intermediate values.**

The same expression in ANF:

```scheme
(let ((a (car x)))
  (let ((b (* y 2)))
    (+ a b)))
```

Every non-trivial sub-expression gets a name via `let`. Function arguments must be "trivial" (variables or constants), never complex expressions. This gives you the same benefit as CPS — intermediate values are named, evaluation order is explicit — without the continuation lambdas.

ANF is strictly simpler than CPS. Flanagan et al.'s paper is titled "The Essence of Compiling with Continuations" — the claim is that ANF captures the *useful* part of CPS (naming intermediates, making order explicit) while discarding the part that adds complexity without benefit (explicit continuation lambdas).

## Where Wile Sits

Wile uses neither CPS nor ANF. It uses **direct-style compilation** following Dybvig 1987, Chapter 3. The compiler walks the validated AST and emits stack-machine bytecode directly:

```scheme
(+ (car x) (* y 2))
```

compiles to:

```
LoadLocal x       ; value = x
Push              ; stack = [x]
Car               ; value = (car x)
Push              ; stack = [(car x)]
LoadLocal y       ; value = y
Push              ; stack = [(car x), y]
PushLiteral 2     ; stack = [(car x), y, 2]
Mul               ; value = (* y 2)
Push              ; stack = [(car x), (* y 2)]
Pull              ; value = +       (pulled from bottom)
Apply             ; call +(car x, *y2)
```

The stack implicitly names intermediates. Position 0 holds `(car x)`, position 1 holds `(* y 2)`. Evaluation order is the bytecode emission order. Tail position is tracked by a boolean flag (`CompileTimeCallContext.inTail`).

This is the simplest possible approach. No intermediate representation, no transformation passes. The question is: what would we gain by adding one?

## CPS: What It Would Buy (and Cost)

### The benefits

**Uniform call/cc handling.** In CPS, `call/cc` is trivial — it just captures the current continuation parameter and packages it as a value. In direct-style, `call/cc` requires special VM support (`SaveContinuation`, `MachineContinuation`, escape closures, prompt abort propagation). Wile carries roughly a thousand lines of continuation machinery across `machine_continuation.go`, `captured_continuation.go`, `composable_continuation.go`, and the winding files.

But that machinery *already works*. It's been tested, debugged, and handles edge cases (cross-thread rejection, winding stack preservation, inline vs. sub-context modes). CPS would eliminate this machinery but replace it with closure-conversion complexity.

**Optimization on the IR.** CPS enables "contraction" — beta-reducing continuation lambdas that are called exactly once. This eliminates temporary closures. It enables eta-reduction (removing trivial wrapper lambdas) and constant folding across continuation boundaries.

But these optimizations assume you're targeting native code where eliminated closures translate to eliminated heap allocations and jumps. On a stack-based bytecode VM, the "closures" that CPS introduces don't exist as runtime objects — they're compilation artifacts that must be eliminated before code generation, or they become actual performance overhead.

### The costs

**Every subexpression becomes a closure.** CPS transforms `(+ a b)` into `(+/k a b k)` where `k` is a continuation lambda. If these lambdas aren't optimized away before bytecode emission, they become real `MakeClosure` + `Apply` sequences — dramatically worse than the Push/Pop that direct style uses.

In practice, a CPS compiler spends significant effort *undoing* the CPS transform during code generation: recognizing that a continuation lambda is "just a let binding" or "just a return." Appel's "Compiling with Continuations" devotes multiple chapters to this. You're adding complexity to a representation and then working to remove that complexity.

**Compilation speed.** CPS transform is an O(n) pass, but it creates an entirely new tree (doubled in size), which must then be traversed by every subsequent pass. For a REPL-oriented interpreter, this matters. Wile's current compile path (validated AST → bytecode) is a single recursive walk with no intermediate allocation.

**Increased closure count.** This is the fatal one for Wile specifically. Consider what CPS does to `let`:

```scheme
;; Source
(let ((x (+ a 1)))
  (* x 2))

;; CPS
(+/k a 1 (lambda (x)    ;; continuation closure
  (*/k x 2 k)))
```

In Wile, `let` compiles to a frame push and slot stores with no lambda at all. In CPS, every intermediate becomes a lambda, and each one would need to be recognized and eliminated to get back to what core `let` already emits. The optimizer would have to handle more patterns to reach the same result as today's direct compilation.

### Who benefits from CPS?

Compilers targeting native code: Gambit (compiles to C), Chicken (compiles to C), Chez Scheme (compiles to machine code). For them, CPS lambdas become jump labels, not heap objects. The overhead is in the compiler, not the runtime.

## ANF: Closer to Home

ANF is the tempting choice because it's simpler than CPS and addresses a real issue: naming intermediate values enables analysis.

### What ANF would look like for Wile

Today, when the compiler sees `(+ (car x) (* y 2))`, it recursively compiles subexpressions, emitting Push after each one. The intermediate values live on the stack, unnamed.

With an ANF pass before compilation:

```scheme
;; ANF of (+ (car x) (* y 2))
(let ((t1 (car x)))
  (let ((t2 (* y 2)))
    (+ t1 t2)))
```

Now `t1` and `t2` are named. An optimization pass could check: is `t1` used once? Is `t2` a constant? Does this `+` always receive integers?

### The `let` cost objection, and why it no longer holds

When this document was written, `let` was a macro expanding to `((lambda (name ...) body ...) val ...)`, so an ANF-introduced `let` binding cost a closure allocation plus a full call protocol: easily 10+ instructions and a heap-allocated environment frame, where direct compilation emitted four instructions and no allocation. That was the sharpest objection to ANF here: **ANF names intermediates by binding them in `let`, and Wile's `let` was expensive.**

That objection is retired. `let` is now a core compiled form (see [Core `let`](core-let.md)); `CompileValidatedLet` emits `OpPushEnv(n)`, one `StoreLocal` per binding, and (on a non-tail exit) `OpPopEnv`, with no closure and no template boundary. An ANF-introduced binding would cost a slot in a frame that the enclosing form is pushing anyway.

So the remaining question is not cost of representation but payoff of analysis, which is the subject of the next section.

### What analysis would ANF enable?

With named intermediates, you could do:

**Constant folding beyond `if`.** Today, Wile folds `(if #f X Y)` → `Y`. With ANF:

```scheme
(let ((x 5))
  (let ((y (+ x 3)))    ;; ANF intermediate
    (* y 2)))
```

You could propagate `x = 5`, fold `(+ 5 3)` → `8`, fold `(* 8 2)` → `16`. But this requires tracking which bindings are constants and which are used by `set!`, across the full scope of the function. For dynamically-typed Scheme where most bindings are opaque values, the analysis proves something useful only for literal constants — a narrow case.

**Dead binding elimination.** If `t1` is never referenced, don't compute it. But in Scheme, computing `(car x)` might raise an error (if `x` is not a pair), so you can't eliminate it unless you know it's pure. Purity analysis in Scheme is undecidable in general (any call might invoke a continuation that escapes), so you'd need conservative "known pure" lists for built-in primitives. Doable, but the payoff is eliminating computations that good Scheme programmers don't write in the first place.

**Common subexpression elimination.** If `(car x)` appears twice and `x` isn't mutated between them, use the same value. This requires alias analysis (is `x` the same `x`?), mutation tracking (no `set!` on `x`, no `set-car!` through any alias), and purity analysis (no side effects between the two uses). In practice, Scheme programmers bind shared subexpressions with `let` themselves — the language idiom handles this at the source level.

## The Deeper Question: What's the Bottleneck?

CPS and ANF are solutions to the problem of *analyzing and transforming programs before generating code*. They create a uniform representation where optimizations are easy to express.

But Wile's profiling data shows that the bottleneck isn't redundant computation or missed optimizations — it's **dispatch overhead** in the VM loop. Every opcode fetch + switch branch costs real time. The optimizations that have delivered measurable speedups are:

| Optimization | Speedup | What it reduces |
|-------------|---------|-----------------|
| Opcode promotion (Phase 1) | -43% takl | Indirect call → inlined switch case |
| Opcode promotion (Phase 2) | -71% sumfp | Indirect call → inlined switch case |
| Superinstruction fusion | -1 dispatch/site | Adjacent opcodes → single opcode |
| GC pressure reduction | -8.9% geo mean | Allocation pressure |

None of these require an IR. They operate on the bytecode directly. And they target the *actual* cost center — the interpreter loop, not the compiled code's computational redundancy.

An IR makes sense when you have many transformations to run and the IR simplifies each one. With zero or one transformation, the IR is a toll road to nowhere — you pay the conversion cost and get nothing back.

## When Would an IR Become Worth It?

**If Wile added a JIT tier.** A JIT needs to analyze hot loops, specialize types based on profiling feedback, and emit native code. That analysis needs an IR — and CPS or ANF would be the right choice (CPS for a tracing JIT, ANF for a method JIT).

**If Wile added type inference.** Even limited flow-sensitive type inference ("this `+` always receives integers in this function") would enable specialization. That analysis runs on an IR, not on bytecode. ANF would be the natural choice — it names the values you want to track types for.

**If compile-time optimization became measurably important.** If profiling revealed that programs spend significant time in redundant computation (as opposed to dispatch overhead), an IR would provide the framework for constant propagation, CSE, and dead code elimination. But the profiling would need to show this first.

None of these are on Wile's roadmap. The interpreter is designed for embedding, not for running compute-intensive programs at native speed. For embedding, compilation speed (fast startup) matters more than runtime optimization depth.

## Summary: The Spectrum of Scheme Compilation

```
Simplest                                                    Most powerful
   |                                                              |
   v                                                              v

Direct style ──── ANF ──── CPS ──── SSA ──── Native codegen
(Wile, Chibi)   (Guile)  (Chez,   (V8's    (LLVM backend)
                         Gambit)  TurboFan)
```

Each step right adds compilation complexity and enables more optimization. Each step is justified when the runtime savings exceed the compilation cost. For a Go-hosted bytecode interpreter with REPL latency requirements, Wile is at the right point on this spectrum.

The interesting observation is that Wile's `ValidatedExpr` types already function as a lightweight IR — they guarantee syntactic correctness and enable dispatch by form type. They just don't name intermediate values. The second step of the path one place right on this spectrum has since been taken: `let` is a core compiled form, and the compiler already synthesizes a `ValidatedLet` to inline a call. What remains is extending `ValidatedExpr` with ANF-style intermediate naming and adding constant propagation. That's a meaningful project, but the motivation would need to come from profiling data, not from theory.

## References

- Steele, "Rabbit: A Compiler for Scheme", MIT AI Memo 474, 1978 (original CPS compiler)
- Appel, *Compiling with Continuations*, Cambridge, 1992 (the CPS bible)
- Flanagan, Sabry, Duba, Felleisen, "The Essence of Compiling with Continuations", PLDI 1993 (ANF)
- Dybvig, *Three Implementation Models for Scheme*, UNC PhD thesis, 1987, Ch. 3 (direct-style, Wile's lineage)
- Kelsey, "A Correspondence between Continuation Passing Style and Static Single Assignment Form", IR'95 (CPS ≅ SSA)
- Kennedy, "Compiling with Continuations, Continued", ICFP 2007 (modern reassessment)
