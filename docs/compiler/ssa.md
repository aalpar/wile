# Would SSA Help the Wile Compiler?

Suppose you're compiling this:

```scheme
(define (f x)
  (let ((a (+ x 1))
        (b (* x 2)))
    (if (< a b)
        (+ a b)
        (- a b))))
```

A production C compiler would build an SSA (Static Single Assignment) intermediate representation, propagate constants, eliminate dead code, and allocate registers — all in linear time thanks to SSA's clean def-use chains. So shouldn't Wile do the same?

The short answer is no. But the *reason* it's no is more interesting than the answer itself — it reveals something fundamental about what SSA actually solves and why Wile's architecture already sidesteps those problems.

## What SSA Actually Solves

Forget the formal definition for a moment. SSA exists to answer one question: **which definition of a variable reaches this point in the program?**

Consider this pseudocode:

```
x = 5
if condition:
    x = 10
print(x)    // is x 5 or 10?
```

In a flat list of instructions, `x` has been assigned twice. When we reach `print(x)`, which value does `x` hold? The answer depends on which branch was taken — and that's a runtime decision. If we want to optimize at compile time (propagate the constant `5`, eliminate dead code, allocate registers), we need to track which definitions reach which uses.

SSA solves this by renaming:

```
x1 = 5
if condition:
    x2 = 10
x3 = phi(x1, x2)    // "whichever one got here"
print(x3)
```

Now every variable is assigned exactly once. The phi-function at the merge point makes the control flow dependency explicit. Optimizations like constant propagation, dead code elimination, and register allocation become almost trivial graph operations on this representation.

This is genuinely powerful. It's the backbone of LLVM, GCC, V8's TurboFan, and every serious optimizing compiler built in the last 30 years.

## Why Wile's Stack Machine Already Has the Key Property

Here's the insight: **a stack machine is naturally in something like SSA form already.**

When Wile compiles `(+ (car x) (* y 2))`, it produces:

```
LoadLocal x        ; value = x
Push               ; stack = [x]
Car                ; value = (car x)    -- new "version" of value register
Push               ; stack = [x, (car x)]
LoadLocal y        ; value = y
Push               ; stack = [x, (car x), y]
PushLiteral 2      ; stack = [x, (car x), y, 2]
Mul                ; value = (* y 2)    -- new "version"
Push               ; stack = [x, (car x), y, 2, (* y 2)]
...
Add                ; value = result
```

Each `Push` creates a new stack slot that is consumed exactly once. Each write to the value register creates a new "version" that lives until the next write. **There is no "which definition reaches here?" ambiguity** — the stack discipline guarantees it. Values flow through the stack in a single, deterministic order. You never need to ask "which `x`?" because the stack position *is* the identity.

In a register machine, the compiler must decide which of 16 (or 32, or 64) registers holds each intermediate value, and must handle the case where two different control flow paths assign different values to the same register. That's the problem SSA solves. A stack machine doesn't have that problem — there's always room for one more value, and the order is implicit.

## The Five Reasons SSA Doesn't Help Wile

### 1. No register allocation problem

SSA's single biggest payoff in production compilers is making register allocation tractable. SSA variables map cleanly to a graph coloring problem that's efficient to solve.

Wile targets a *stack* machine. There are no registers to allocate. The eval stack and the value register together handle all intermediate values. The "allocation" is just Push and Pop. Adding SSA to solve a register allocation problem that doesn't exist is pure overhead.

### 2. Dynamic types block the best optimizations

The most profitable SSA optimizations are type-specialized:

- "This `+` always receives integers, so emit an `iadd` instead of a polymorphic call"
- "This comparison is always int-vs-int, so skip the type dispatch"
- "This value is always a fixnum that fits in a register, so skip the heap allocation"

Wile's `+` must handle integers, rationals, reals, and complex numbers — determined at runtime by the actual values. Without a type inference pass (which Scheme's semantics make extremely hard), SSA gives you def-use chains to values whose types you don't know. You can see that `a` flows to the `+` in `(+ a b)`, but you can't specialize the `+` because `a` might be anything.

This is not a Wile limitation — it's a Scheme limitation. Languages like JavaScript invested enormous effort in speculative type specialization (V8's TurboFan uses SSA + profiling feedback). But they're willing to deoptimize and fall back when speculation is wrong. That's a fundamentally different cost model than an embeddable bytecode interpreter.

### 3. First-class continuations break SSA assumptions

SSA relies on structured control flow. The phi-functions at merge points assume that control arrives from one of a known set of predecessors. Every optimization assumes that once a value is "dead" (no more uses on any path), its storage can be reclaimed.

`call/cc` breaks both assumptions:

```scheme
(define k #f)
(define (f x)
  (let ((a (+ x 1)))
    (call/cc (lambda (k0) (set! k k0)))
    (display a)      ;; a must still be alive — k might bring us back here
    a))
```

After `call/cc`, the continuation `k` can re-enter the body of `f` at any later time. The variable `a` can never be declared dead — the captured continuation holds a reference to the entire environment frame. Any SSA-based liveness analysis would need to conservatively mark every variable as live if a continuation is captured anywhere in scope. At that point, the analysis is doing work to produce the same answer as "keep everything" — which is what Wile's environment frames already do.

Wile's linked-closure environment model (where each closure points to its parent frame) is actually the *correct* representation for a language with first-class continuations. It's not a naive choice that SSA would improve — it's the design that handles the hard case correctly.

### 4. `set!` requires memory cells anyway

SSA can handle mutable variables, but it does so by converting them to explicit memory loads and stores — effectively removing them from SSA form. In Wile, mutable variables live in environment frame slots, which are already explicit memory:

```scheme
(let ((x 0))
  (set! x (+ x 1))    ;; StoreLocal / LoadLocal — already memory operations
  x)
```

If you built SSA for this, the `set!` would force `x` into a "cell" (an allocated memory slot), and the SSA would consist of `load(cell)` and `store(cell, value)` — exactly what `LoadLocal`/`StoreLocal` already are. The SSA form would be a verbose restatement of what the bytecode already says.

### 5. Compilation speed matters more than optimization depth

Wile is an interpreter with a REPL. Every expression the user types goes through tokenize -> parse -> expand -> validate -> compile -> execute. That pipeline needs to be fast. SSA construction (dominance frontiers, phi-function insertion, renaming pass) and SSA destruction (phi elimination, register coalescing) add meaningful overhead to compilation.

For a language where functions are typically 5-20 lines and where most runtime cost is in the VM dispatch loop (not in redundant computation), the compilation overhead of SSA would likely *increase* total wall-clock time for typical workloads. Wile's peephole optimizer runs in O(n) over the bytecode and catches the patterns that actually matter — instruction fusion.

## What Wile Optimizes Instead (And Why It's Right)

Wile's actual performance bottleneck is **VM dispatch overhead**: the cost of fetching each opcode, branching through the switch statement, and executing the operation. The profiling data confirms this — the opcode promotion work (inlining `car`, `cdr`, `+`, `-`, `<`, etc. as dedicated opcodes) produced 30-70% speedups on benchmarks. That dwarfs anything SSA-based optimization could deliver.

The optimization strategy Wile has chosen is:

| Technique | What it saves | Example |
|-----------|---------------|---------|
| Superinstruction fusion | Dispatch overhead | `Load+Push -> PushX` |
| Opcode promotion | Indirect call + arity check | `(+ a b)` -> `OpAdd` |
| Constant folding | Branch + dead code | `(if #t X Y)` -> `X` |
| Dead LoadVoid removal | Wasted dispatch | `LoadVoid; LoadLiteral` -> `LoadLiteral` |
| Cached binding resolution | Environment chain walk | `OpPushCachedBinding` |
| Call fusion | Pull+Apply+SaveCont overhead | `OpCallForeignCached` |

These all target the actual bottleneck (dispatch) rather than a theoretical bottleneck (redundant computation). For a Go-hosted interpreter without `unsafe`, dispatch is where the time goes.

## When Would SSA Become Worthwhile?

SSA would become interesting if Wile ever:

1. **Added a JIT compiler** targeting native machine code. Then register allocation becomes a real problem, and SSA is the right tool.
2. **Added a type inference pass** (flow-sensitive or profiling-based). Then SSA def-use chains would enable type specialization — the optimization with the highest payoff.
3. **Targeted a register-based bytecode VM** instead of a stack machine. Register VMs (like Lua 5.x or Dalvik) need SSA or something like it for efficient register allocation.

None of these are on the roadmap, and each would be a fundamental architectural change. For a stack-based VM with dynamic types and first-class continuations, the current approach — direct compilation with peephole optimization — is the right design point.

## The Deeper Lesson

SSA is not a universal "make things faster" tool. It's a specific solution to the register allocation and dataflow analysis problems that arise when compiling statically-typed languages to register machines. Remove any of those conditions — stack machine instead of registers, dynamic types instead of static, reifiable continuations instead of structured control flow — and SSA's value proposition weakens dramatically. Remove all three, and it becomes overhead.

The skill is knowing which optimization framework matches your actual constraints. Wile's constraints (Go-hosted, stack VM, dynamic types, `call/cc`, REPL latency) point toward dispatch reduction, not dataflow optimization. The peephole optimizer and opcode promotion are the right tools for this job.
