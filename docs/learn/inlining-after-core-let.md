# Procedure Inlining: The Next Step After Core `let`

Suppose you write this:

```scheme
(define (square x) (* x x))
(+ (square 3) (square 4))
```

The Wile compiler produces two templates. Template 0 (the caller) compiles `(+ (square 3) (square 4))`. Template 1 (the body of `square`) compiles `(* x x)`. Every call to `square` goes through the full call protocol: save the continuation, push the argument, load the closure, apply, check arity, allocate an env frame, bind the parameter, execute the body, restore the continuation.

That's a lot of work for "multiply a number by itself." An inlining pass would replace the call with the body, producing something equivalent to:

```scheme
(+ (let ((x 3)) (* x x))
   (let ((x 4)) (* x x)))
```

Which, with core `let`, stays in a single template — no closure, no call protocol, no template boundary. Then constant propagation could fold it to `(+ 9 16)` → `25`.

This document explores what inlining means for Wile specifically: what infrastructure exists, what's missing, where it fits in the pipeline, and what makes it hard.

## What Call Overhead Actually Costs

Before talking about eliminating calls, we should know what a call costs. Here's the bytecode for `(square 3)` in a non-tail position, after peephole optimization:

```
SaveContinuation(+4)     ; save env, template, pc, evals
PushLiteral 3             ; argument
CallCachedBinding idx     ; fused: load square + drain args + check arity + apply
                          ; (inside template 1: bind x, compute (* x x), restore)
; return point            ; continuation restored here
```

The peephole optimizer already fused `PushCachedBinding + PullApply` into `CallCachedBinding`, and if `square` held a foreign closure, it would fuse even further into `CallForeignCached`. But `square` is a `MachineClosure`, not a `ForeignClosure`, so the call goes through the general `Apply` path.

What `Apply` does for a `MachineClosure`:

1. Acquire env frame from pool (or allocate)
2. Copy parameter values into local slots
3. Set `mc.env`, `mc.template`, `mc.pc = 0`
4. Execute the body
5. `RestoreContinuation`: restore saved env, template, pc, evals

Steps 1-3 and 5 are pure overhead when the body is a single expression like `(* x x)`. The body itself — two `LoadLocal` + `Mul` — takes three dispatches. The call protocol around it takes five or six. The overhead dominates.

## Why Core `let` Is the Prerequisite

Inlining replaces a function call with the function's body, wrapped in a binding form for the parameters. Without core `let`, the inlined code would *still* be a lambda application:

```
;; Inlined (square 3) without core let:
((lambda (x) (* x x)) 3)
```

This saves nothing. The compiler would emit `MakeClosure` + `Apply` for the inlined code — the same template boundary, the same call overhead. You've just moved the call from one place to another.

With core `let`, the inlined code becomes:

```
;; Inlined (square 3) with core let:
(let ((x 3)) (* x x))
```

Which compiles to:

```
PushLiteral 3
OpPushEnv(1)
StoreLocal x
LoadLocal x     ; could be further optimized: x is known to be 3
LoadLocal x
Mul
OpPopEnv
```

All in one template. No closure. No call protocol. And now the optimizer can see that `x` is always `3`, which enables constant propagation to eliminate the `LoadLocal` instructions entirely.

The progression is: **core `let` makes inlining possible; inlining makes constant propagation profitable.**

## What the Compiler Needs to Know

Inlining a call requires three pieces of information that the compiler currently doesn't have:

### 1. Is the binding immutable?

```scheme
(define (square x) (* x x))
(set! square (lambda (x) (+ x x)))  ; oops
(square 3)                            ; which square?
```

If `square` can be reassigned, inlining it is unsound — the inlined body might not match what `square` actually holds at runtime. The compiler must prove that the binding is never targeted by `set!`.

Wile's `BindingType` enum has `Variable`, `Syntax`, `Primitive`, and `Unknown`. There's no `Mutable` flag. The `CompileValidatedSetBang` method compiles `set!` but doesn't mark the target binding. The `set!` is fire-and-forget — the compiler generates a `StoreLocal` or `StoreGlobal` and moves on.

**What's needed:** A mutability analysis pass. After validation, scan all `ValidatedSetBang` nodes and mark their target bindings as mutable. Any binding not marked is an inlining candidate. This is a simple whole-module pass — visit every expression, collect `set!` targets into a set.

> Note: R7RS primitive bindings are a special case. Bindings like `+`, `car`, `cons` are `BindingTypePrimitive` and are never `set!`-able (the language guarantees this). The existing `CallForeignCached` optimization already exploits this — it resolves the binding at compile time and emits a direct call. Inlining extends this: instead of calling `+`, emit `Add` directly. The opcode promotion system already does this for the 11 hottest primitives. Inlining user-defined procedures is the generalization.

### 2. Is the body small enough?

Inlining duplicates the body at every call site. If `square` has a 2-instruction body, duplicating it saves net cycles. If `fibonacci` has a 50-instruction body with recursive calls, duplicating it bloats the code and may *slow things down* (instruction cache pressure, larger templates).

**What's needed:** A cost model. Assign each bytecode instruction a cost (1 for simple loads/stores, 2 for branches, N for calls). Sum the costs for the closure's template. If the total is below a threshold, the closure is an inlining candidate.

The cost model doesn't need to be sophisticated. Chez Scheme's initial heuristic was roughly "inline if the body is a single expression." GHC's is "inline if the body is smaller than the call site would be." For Wile, a reasonable starting point:

- **Always inline**: Body is a single non-call expression (literal, variable reference, arithmetic on locals). This covers accessors, predicates, and simple arithmetic.
- **Maybe inline**: Body is 2-5 instructions with no internal calls. Decision depends on call frequency (but we don't have profile data, so use static heuristics).
- **Never inline**: Body contains calls, branches, or closures. Too complex to duplicate without analysis.

### 3. Does the closure escape?

```scheme
(define (make-adder n)
  (lambda (x) (+ n x)))

(define add5 (make-adder 5))
(add5 3)  ; can we inline this?
```

`add5` holds a closure returned by `make-adder`. The closure captures `n`. To inline `(add5 3)`, we'd need to know *which* closure `add5` holds and what `n` was bound to. This requires escape analysis and interprocedural constant propagation — well beyond what a simple inlining pass can do.

**For now:** Only inline closures whose definition is visible at the call site. If `square` is defined in the same scope (or a lexically enclosing scope) and is not `set!`-ed, the compiler can see its body directly via the `cachedBindings` or the compile-time environment. Closures returned from other functions or stored in data structures are not candidates.

This is exactly how the existing `CallForeignCached` optimization works: it checks whether `cachedBindings[idx].Value()` is a `*ForeignClosure` at compile time. The same mechanism can check whether it's a `*MachineClosure` and, if so, whether its template is small enough to inline.

## Where Inlining Fits in the Pipeline

Wile's compilation pipeline is:

```
Source → Tokenizer → Parser → Expander → Validator → Compiler → Peephole → VM
```

Inlining could happen at two points:

### Option A: During Compilation (Validated IR)

The compiler sees a `ValidatedApply` whose callee is a `ValidatedSymbol` pointing to a known `ValidatedLambda`. Instead of emitting `SaveContinuation` + `CallCachedBinding`, it emits the lambda body wrapped in `PushEnv`/`StoreLocal`/`PopEnv`.

**Advantages:**
- Works at the validated IR level, where binding information is explicit
- The compiler already has the callee's `ValidatedLambda` in scope (for calls to locally-defined functions)
- Natural integration with core `let` — the inlined code becomes a `ValidatedLet`

**Disadvantages:**
- Requires the callee's validated IR to be available at the call site. For cross-module calls, this means storing validated IR in the library registry (not currently done).
- Must handle recursion carefully — inlining a recursive function is an infinite loop.

### Option B: After Compilation (Bytecode)

The peephole optimizer already transforms bytecode patterns. A new pass could recognize `CallCachedBinding` where the target is a small `MachineClosure` template, and splice that template's bytecode into the caller.

**Advantages:**
- Works with the existing peephole infrastructure (`EditPlan`, branch target tracking)
- The callee's template is always available (it's in the `cachedBindings` or the `literals` pool)
- Doesn't need cross-module validated IR

**Disadvantages:**
- Bytecode splicing is complex: local variable indices need remapping, branch offsets need recomputation, source references need merging
- The bytecode doesn't carry binding structure — we'd lose the ability to do constant propagation on the inlined parameters (unless we reconstruct it)

### The Right Answer

**Option A is the right design for Wile.** The validated IR carries the binding information that makes inlining profitable. Bytecode splicing is the kind of low-level surgery that compilers do when they have no higher-level representation — but Wile *has* one (the `Validated*` types).

The implementation sketch:

1. The compiler encounters a `ValidatedApply` in `compileValidated`
2. It resolves the callee symbol to a binding
3. If the binding is immutable and holds a `ValidatedLambda`:
   a. Check the body cost against the threshold
   b. If below threshold and non-recursive: synthesize a `ValidatedLet` from the lambda's parameters and the call's arguments
   c. Compile the `ValidatedLet` instead of emitting a call

This is a source-to-source transformation at the IR level. The core `let` compiler handles the rest.

## What Wile Already Has (and What's Missing)

Here's a concrete inventory.

### Already exists

| Capability | Evidence |
|-----------|----------|
| Compile-time binding resolution | `cachedBindings` on `NativeTemplate`, used by `CallForeignCached` |
| Callee specialization | Peephole checks `cachedBindings[idx].Value().(*ForeignClosure)` |
| Per-template optimization | `Optimize()` with `EditPlan`, branch target tracking, multi-pass |
| Opcode promotion (primitive inlining) | 11 primitives inlined as VM opcodes (Phases 1-3) |
| Cost-free binding forms | Core `let` (planned) — `OpPushEnv`/`StoreLocal`/`OpPopEnv` |
| Body compilation infrastructure | `compileBody`, `compileClosureBody`, `compileValidatedSequence` |

### Missing

| Capability | Needed for | Difficulty |
|-----------|-----------|------------|
| **Mutability tracking** | Knowing which bindings are safe to inline | Low — single pass over `ValidatedSetBang` nodes |
| **Body cost estimation** | Deciding whether to inline | Low — count instructions in template or validated exprs |
| **Inline decision at compile time** | Replacing `ValidatedApply` with `ValidatedLet` | Medium — needs access to callee's validated IR at the call site |
| **Recursion detection** | Preventing infinite inlining | Low — check if callee's body references the callee |
| **Cross-module inlining** | Inlining library-exported functions | High — requires storing validated IR in library registry |

## The Progression

The optimizations Wile has done (and plans to do) form a clear escalation ladder:

```
Level 0: Interpret everything (tree-walking)
Level 1: Compile to bytecode (Wile baseline)
Level 2: Fuse instruction sequences (peephole — done)
Level 3: Specialize known calls (CallForeignCached — done)
Level 4: Inline hot primitives (opcode promotion — done)
Level 5: Eliminate false template boundaries (core let — planned)
Level 6: Inline user-defined procedures (this document)
Level 7: Propagate constants through inlined code (future)
Level 8: Eliminate dead bindings (future)
```

Each level depends on the ones below it. Inlining without core `let` produces lambda applications. Core `let` without mutability tracking can't prove inlining is safe. Constant propagation without inlining has nothing to propagate (values are trapped behind template boundaries).

The key insight: **levels 5-8 are all about making binding information visible to the optimizer.** Core `let` makes binding *structure* visible. Inlining makes binding *values* visible across call boundaries. Constant propagation exploits the visible values. Dead binding elimination cleans up the results.

## A Concrete Example

Walk through what happens to `(+ (square 3) (square 4))` at each level:

**Level 3 (current):**
```
SaveContinuation(+6)
PushLiteral 3
CallCachedBinding square    ; → Template 1: bind x=3, compute (* x x), restore
Push                         ; save result (9)
SaveContinuation(+6)
PushLiteral 4
CallCachedBinding square    ; → Template 1: bind x=4, compute (* x x), restore
Push                         ; save result (16)
Add                          ; promoted opcode: 9 + 16 = 25
```

Two template transitions, two env frame allocations, two arity checks.

**Level 6 (after inlining + core let):**
```
PushLiteral 3
OpPushEnv(1)
StoreLocal x
LoadLocal x
Push
LoadLocal x
Mul                          ; 3 * 3 = 9
OpPopEnv
Push                         ; save 9
PushLiteral 4
OpPushEnv(1)
StoreLocal x
LoadLocal x
Push
LoadLocal x
Mul                          ; 4 * 4 = 16
OpPopEnv
Push                         ; save 16
Add                          ; 9 + 16 = 25
```

Everything in one template. No call overhead. But there's redundancy — we're storing 3 into a slot just to load it back twice.

**Level 7 (after constant propagation, future):**
```
PushLiteral 3
Push
PushLiteral 3
Mul                          ; 9
Push
PushLiteral 4
Push
PushLiteral 4
Mul                          ; 16
Push
Add                          ; 25
```

The `let` bindings are gone — the optimizer proved `x` is always the literal and substituted it directly.

**Level 8 (after constant folding, future):**
```
PushLiteral 25
```

The entire computation collapsed to a single literal. This is what Chez Scheme does for this program. The path from Level 3 to Level 8 requires each intervening level to work.

## The Hard Parts

### Recursive functions

```scheme
(define (fib n)
  (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))
```

Inlining `fib` into itself is obviously an infinite loop. The compiler must detect self-reference in the callee's body and refuse to inline. But what about *mutual* recursion? `even?` calls `odd?` which calls `even?` — inlining either one starts a cycle. Detection requires building a call graph at the validated IR level and checking for cycles.

For a first implementation: refuse to inline any closure whose body contains a reference to itself. This misses mutual recursion but catches the common case and is trivially cheap.

### Closures that capture variables

```scheme
(let ((count 0))
  (define (increment!) (set! count (+ count 1)) count)
  (increment!)
  (increment!))
```

Inlining `increment!` is sound (the binding is never reassigned), but the inlined body references `count` — a variable in the enclosing scope that gets mutated. The inlined code needs to reference the same `count` slot, not a copy. This works naturally with Wile's environment model (the `let` env frame is the parent of the inlined code's env), but the compiler needs to verify that the inlined body's free variables are all resolvable in the caller's compile-time environment.

### Tail position preservation

```scheme
(define (loop n)
  (if (= n 0) 'done (loop (- n 1))))
```

If `loop` is inlined at a call site that's NOT in tail position, the recursive call `(loop (- n 1))` in the inlined body must also not be in tail position — otherwise it would tail-call into a different continuation than expected. The compiler already tracks tail position via `CompileTimeCallContext.inTail`; the inlining pass must propagate this correctly into the synthesized `ValidatedLet`.

### Side effects and evaluation order

```scheme
(define (f x y) (+ x y))
(f (read) (print "hi"))
```

Inlining `(f (read) (print "hi"))` to `(let ((x (read)) (y (print "hi"))) (+ x y))` preserves left-to-right evaluation. But if the inliner tries to substitute directly (replacing `x` with `(read)` and `y` with `(print "hi")` inside the body), it must ensure each argument is evaluated exactly once and in the original order. The `let`-wrapping approach naturally handles this — it's the safe default.

## What This Means for Wile

Inlining is the natural next optimization after core `let`, and the infrastructure for it is largely already present. The missing pieces are small (mutability tracking, cost estimation) and the architecture fits cleanly into the existing validated IR → compiler path.

The immediate benefit is eliminating call overhead for small utility functions — the kind that Scheme programs use constantly (accessors, predicates, simple combinators). The long-term benefit is opening the door to constant propagation and dead binding elimination, which together can collapse entire computation chains.

The recommended path:

1. **Ship core `let`** (all four binding forms). This provides the target representation.
2. **Add mutability tracking** — flag bindings targeted by `set!` during validation.
3. **Implement simple inlining** — single-expression non-recursive closures with visible definitions. Use `ValidatedLet` as the inlined representation.
4. **Measure** — run Gabriel benchmarks. The binding-heavy programs (fib, ackermann, nqueens) should benefit most.
5. **Extend** — raise the body size threshold, add cross-module inlining, add constant propagation.

> See [Core `let` in Compiler Design](core-let-in-compiler-design.md) for why core
> `let` is the standard approach. See [CPS and ANF](anf-and-cps-for-scheme-compilers.md)
> for how intermediate representations relate to binding structure. See
> `plans/CORE-LET.md` for the core `let` implementation plan.
