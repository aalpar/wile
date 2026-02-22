# Allocation Optimization Plan

Date: 2026-02-22
Status: In progress — three optimizations complete (-42.1% combined), remaining items pending

## Problem Statement

Peephole fusion reduced dispatched ops by 32% in ZebraPuzzle but only improved wall-clock by 2.4% — backtracking/env-copying dominates. The dispatch loop is no longer the bottleneck; per-call allocation is.

## Completed: Foreign Closure noCopyApply Fix

**Commit:** `713661d` (2026-02-22)

`NewForeignClosure` (`machine/util.go`) never called `computeNoCopyApply()`, so every primitive call (`<=`, `+`, `-`, `car`, `cdr`, etc.) allocated a fresh `EnvironmentFrame` + `[]Binding` on every invocation. Foreign closure templates contain no `OpSaveContinuation` or `OpMakeClosure`, so they all qualify for the no-copy fast path.

**Fix:** One line — add `tpl.computeNoCopyApply()` to `NewForeignClosure`.

**Impact:**

| Metric | Result |
|--------|--------|
| Gabriel geo-mean (all 16) | **-24.5%** |
| Gabriel geo-mean (call-heavy 11) | -24.9% |
| Gabriel geo-mean (arithmetic 5) | -23.7% |
| ZebraPuzzle | **-21.2%** (18.36s → 14.47s) |
| Best individual | deriv -35.8%, sieve -35.8%, sum -30.6% |
| Worst individual | ctak -6.4% (continuation-heavy) |

**Why it was large:** In `(fib 10)`, 441 of 618 total Apply calls were to primitives (71%). All 441 were needlessly copying their environment. CPU profiling showed 52.5% of fib time was allocation + GC, with `NewApplyFrame` + `copyForApplyInto` as the top allocators.

## Completed: 2-Argument Numeric Fast Path

**Commit:** `6282c36` (2026-02-22)

The numeric helpers (`NumericChainCompare`, `NumericChainCompareReal`, `NumericFoldVariadic`, `NumericFoldWithFirst`, `NumericExtremum`) allocated closure literals for `ForEach` iteration on every call, even for the common 2-argument case (`(+ a b)`, `(<= x y)`). `emptyListType` implements `Tuple`, so existing early-exit checks (`pr.Cdr().(values.Tuple)`) didn't fire for single-element rest lists — they fell through to ForEach.

**Fix:** Add `values.Single(t Tuple) (Value, bool)` that extracts the sole element from a single-element list. Use it in all five numeric helpers to skip ForEach and its closure allocation. Also add `IsEmptyList` guard in `NumericFoldWithFirst` before the `Cdr().(values.Tuple)` check. For `NumericChainCompareReal`, add a complete 2-arg fast path that skips the wrapper closure passed to `NumericChainCompare`.

**Impact (incremental, on top of noCopyApply fix):**

| Metric | Result |
|--------|--------|
| Gabriel geo-mean vs noCopy baseline | **-20.1%** |
| Gabriel geo-mean vs original master | **-33.4%** (combined) |
| Best individual vs noCopy | sum -41.1%, primes -37.8%, ackermann -35.3%, sieve -33.9% |
| Best individual vs master | fib -39.5%, primes -39.4%, ackermann -38.4%, tak -37.8% |
| ZebraPuzzle (combined) | **-22.2%** (18.36s → 14.29s) — minimal incremental over noCopyApply alone (-21.2%) |

**Why it was large:** In the pre-fix fib profile, numeric helper closures (ForEach callbacks) accounted for 31.8% of all allocations. The 2-arg case (`(+ a b)`, `(<= n 1)`, `(- n 1)`) is overwhelmingly dominant in practice.

## Completed: Pull Backing Array Preservation

**Commit:** `db452db` (2026-02-22)

`Stack.Pull()` used `(*p)[1:]` to remove the bottom element, which advances the slice start pointer. Each Pull erodes the usable capacity by 1. After pool recycling via `acquireStack` (which resets to `[:0]` but preserves the shifted start), the cap shrinks: 8→7→6→... until `Push` triggers `growslice`, allocating a fresh backing array. The pooled array is wasted. `Stack.Push` growslice accounted for 22.0% of all allocations post-2-arg-fix.

**Fix:** Replace `(*p)[1:]` with `copy((*p), (*p)[1:])` + length decrement. This shifts elements down, preserving the original backing array start for pool reuse. Also extracted `stackInitialCap` constant (currently 8) in `pool.go`.

**Impact (incremental, on top of noCopyApply + 2-arg fast path):**

| Metric | Result |
|--------|--------|
| Gabriel geo-mean vs previous | **-13.0%** |
| Gabriel geo-mean vs original master | **-42.1%** (combined) |
| ZebraPuzzle (all 3 combined) | **-29.5%** (18.36s → 12.95s) |
| Best individual vs master | sum -51.9%, sieve -49.2%, fib -47.9%, ackermann -47.0% |

## Current State — What's Already Optimized

| Optimization | Mechanism | Location |
|---|---|---|
| Continuation frames | `sync.Pool` | `pool.go:106` |
| Eval stacks | `sync.Pool` (cap 8) | `pool.go:30` |
| Sub-contexts | `sync.Pool` | `pool.go:41` |
| Macro contexts | `acquireMacroContext` | `pool.go:92` |
| No-copy Apply | `noCopyApply` flag skips env copy for leaf functions | `native_template.go:315` |
| **No-copy foreign closures** | `computeNoCopyApply()` in `NewForeignClosure` | `util.go:25` |
| Fused NewApplyFrame | Single alloc instead of CopyForApply + NewEnvironmentFrameWithParent | `environment_frame.go:164` |
| CoW keys map | Shared between copies, only cloned on mutation | `local_environment_frame.go:139-141` |
| RestoreAndRelease | Transfer evals ownership for normal returns (no copy) | `machine_context.go:240` |
| Contiguous bindings | `[]Binding` not `[]*Binding` — cache-friendly, one alloc | `local_environment_frame.go:182-184` |
| **2-arg numeric fast path** | `values.Single()` skips ForEach closure for 2-arg calls | `registry/helpers/numeric.go` |
| **Pull backing array fix** | `copy()` instead of reslice preserves pool capacity | `stack.go:44`, `pool.go:32` |

## Profiling Findings (fib 10, post-fix baseline)

### CPU Time Breakdown (pre-fix)

| Category | Self time | Share |
|----------|----------|-------|
| Allocation (malloc+init) | 6.69s | 35.7% |
| GC (scan+mark+sweep) | 3.15s | 16.8% |
| sync.Pool ops | 1.24s | 6.6% |
| Global/symbol lookup | 1.23s | 6.6% |
| VM dispatch (Run switch) | 1.08s | 5.8% |
| Primitives (+,-,<=) | 0.92s | 4.9% |
| Env copy+create | 0.89s | 4.7% |
| Other | 4.55s | 18.9% |

**Allocation + GC = 52.5% of total CPU time.** Only 5.6% does useful computation.

### Allocation Objects (pre-fix, per iteration of fib 10)

| Allocator | Count | Share |
|-----------|-------|-------|
| Stack.Push growslice | 53.8M | 17.9% |
| values.NewCons (variadic rest-arg) | 49.2M | 16.4% |
| copyForApplyInto ([]Binding) | 48.4M | 16.2% |
| NewApplyFrame (&EnvironmentFrame) | 46.9M | 15.6% |
| Numeric helper closures (ForEach) | 95.3M | 31.8% |

### Post-fix profiling (after all three fixes)

Total allocs reduced 43.6% (300M → 169M). Per-iteration: 4,618 → 1,335. CPU time per op: 169μs → 88μs (-48%).

**CPU breakdown evolution:**

| Category | Pre-fix | After fix 1+2 | After all 3 |
|----------|---------|---------------|-------------|
| Allocation + GC | 52.5% | 40.0% | 46.1% |
| sync.Pool ops | 6.6% | 21.9% | **17.3%** |
| VM dispatch | 5.8% | 11.7% | 10.8% |
| Global/symbol lookup | 6.6% | 7.9% | 8.1% |
| Apply + env copy | 4.7% | 5.0% | 5.4% |

GC share went UP (16.8% → 26.1%) despite fewer allocs because non-GC work shrank more.

**Allocation objects (after all 3 fixes):**

| Allocator | Share | Per iter | Notes |
|-----------|-------|----------|-------|
| values.NewCons | **49.6%** | 662 | Rest-arg boxing — `values.List()` in Apply for every variadic call |
| NewApplyFrame | 14.4% | 191 | Scheme closure calls only (fib). Reducible via pooling (#3), slim binding (#2), memcpy (#7) |
| NumericFoldWithFirst | 14.3% | 190 | `*Integer` results from `binOp`, NOT closures — 2-arg fast paths work correctly |
| copyForApplyInto | 14.2% | 190 | Same lifecycle as NewApplyFrame |
| NumericFoldVariadic | 7.2% | 95 | `*Integer` results from `binOp`, NOT closures — irreducible arithmetic |

**Stack.Push growslice: ELIMINATED** by Pull backing array fix.

**Key clarification:** The NumericFoldWithFirst/Variadic allocations in the profiler are `*Integer` values created by `binOp()` (e.g., `(- n 1)` → new Integer), not ForEach closure allocations. The 2-arg fast paths are working correctly — ForEach is never reached for 2-arg calls.

**Key concern:** sync.Pool is 17.3% of CPU. Adding more pooled objects (e.g., env frame pooling in #3) may be net-negative. Must benchmark pool-disabled vs pool-enabled before expanding pooling.

## Remaining Allocations Per Non-Tail Call

For a Scheme closure with N bindings, every non-tail call still allocates:

1. **`make([]Binding, N)`** in `copyForApplyInto` (`local_environment_frame.go:218`) — one slice per call
2. **`&EnvironmentFrame{}`** in `NewApplyFrame` (`environment_frame.go:172`) — one struct per call
3. **`make([]values.Value, n)`** in `PopAll` (`stack.go:114`) — one slice per Apply to collect arguments

The continuation frame and eval stack are pooled. The bindings slice and EnvironmentFrame are not.

Note: primitive calls (foreign closures) no longer allocate #1 or #2 thanks to the noCopyApply fix.

## Remaining Optimization Tiers

### Tier 1: High Impact, Moderate Complexity

#### 1. Eliminate PopAll Allocation

`PopAll` (`stack.go:109-119`) allocates `make([]values.Value, n)` on every Apply/PullApply. But `Apply` (`machine_context.go:323`) immediately iterates the slice to set binding values, then the slice is dead.

**Approach A — Read directly from the stack:** Make `Apply` read args from the `*Stack` using indexed access, then clear the stack. Zero allocations. `Apply` already knows the arity (`tpl.ParameterCount()`), so it reads `evals[0..N]` directly.

**Approach B — Reuse a scratch buffer:** Pool a `[]values.Value` alongside the eval stack.

Approach A is cleaner. No semantic changes.

**Previous attempt:** PR #310 tried `Stack.Drain()` (zero-copy view with aliasing). Gabriel geo-mean was -2.3% — not worth the aliasing complexity. The small impact is expected now that the foreign closure fix removed the majority of env-copy allocations; PopAll is a smaller fraction of remaining overhead.

**Files:** `machine/stack.go`, `machine/machine_context.go` (Apply, ApplyCallable, Run loop at OpApply/OpPullApply)

#### 2. Slim Binding Struct for Runtime

Each `Binding` is currently 56 bytes:
```go
type Binding struct {
    value       values.Value         // 16 bytes (interface)
    bindingType BindingType          //  1 byte + padding → 8 bytes
    scopes      []*syntax.Scope      // 24 bytes (slice header)
    source      *syntax.SourceContext //  8 bytes
}
```

At runtime, `scopes` and `source` are never read — they're compile-time metadata. Splitting into a runtime part (value + type = 24 bytes) and compile-time part reduces bytes copied in `copyForApplyInto` by ~2.3x.

**Risk:** Binding is used throughout compile-time and runtime. Need to verify that runtime code never reads scopes/source. The `copyForApplyInto` loop currently copies all four fields — if scopes/source are nil at runtime, we're copying 32 bytes of zeros per binding.

**Files:** `environment/binding.go`, `environment/local_environment_frame.go`

#### 3. Pool EnvironmentFrame + Bindings

`NewApplyFrame` allocates an `EnvironmentFrame` plus `[]Binding`. These follow the same lifecycle as continuation frames for many call patterns.

**Subtlety:** Unlike continuations, environment frames escape into the continuation chain (`cont.env`). Pooling is only safe when the frame is consumed exactly once. Cases:
- `noCopyApply = true`: already skips allocation entirely
- Single non-tail call that returns normally: frame lives in one continuation, consumed by `RestoreAndRelease` — **poolable**
- Multiple non-tail calls or `call/cc`: frame may be shared — **not poolable**

**Approach:** Add a `poolable` flag to EnvironmentFrame (or use the continuation's `shared` flag as proxy). When `RestoreAndRelease` runs on an unshared continuation, pool the env frame too.

**Needs investigation:** What fraction of Apply calls produce frames with bounded (single-consumer) lifetimes? The counters (`EnvsCopied` vs `SharedFrameRestores`) may already answer this.

**Files:** `machine/pool.go`, `environment/environment_frame.go`, `machine/machine_context.go`

#### ~~4. Fast-path for 2-argument Numeric Primitives~~ — DONE

Completed as part of `6282c36`. See "Completed: 2-Argument Numeric Fast Path" above.

#### 4. Eliminate Variadic Rest-Arg Cons Cells

**Current #1 remaining allocator at 39.9% of all objects (post-fix).**

Every variadic primitive call creates cons cells via `values.List(vs[l-1:]...)` in `Apply` for the rest parameter. For `(<= n 1)`, this allocates a 1-element linked list just to pass the second argument.

**Approach:** For the common case of 1-2 rest args, use pre-allocated singleton/pair list structures, or change the calling convention so primitives read args directly from the stack rather than from environment bindings.

**Risk:** Changes the foreign function calling convention. Requires audit of all primitive implementations.

**Files:** `machine/machine_context.go` (Apply), `values/pair.go`

#### 5. Evaluate sync.Pool Overhead

**sync.Pool is now 21.9% of CPU (up from 6.6% pre-fix).** This is a profile inversion: reducing allocation made the pooling overhead dominant. The continuation pool Get/Put/pin/CAS is now more expensive than the GC it avoids.

**Investigation needed:** Benchmark with continuation pooling disabled vs enabled to measure net effect. If GC pressure is now low enough, pooling may be net-negative. Alternatively, consider a simpler pooling strategy (e.g., per-goroutine free list) with less atomic contention.

**Files:** `machine/pool.go`

#### 6. Increase Stack Pool Capacity

**Stack.Push growslice is 22.0% of remaining allocations (post-fix).** The eval stack pool starts at cap 8, but the stack frequently exceeds this, triggering `growslice`. Bumping the initial capacity would eliminate these allocations.

**Approach:** Check `VMCounters.StackMaxDepth` and depth histogram across benchmarks to find the right cap. Current counters show most depths are 0-2 but some reach 42.

**Risk:** Minimal — only affects initial allocation size.

**Files:** `machine/pool.go`

### Tier 2: Moderate Impact, Lower Complexity

#### 6. Binding Copy Optimization — memcpy Path

If bindings remain a flat struct, `copyForApplyInto` could use `copy()` on the `[]Binding` slice directly instead of a field-by-field loop, letting the Go compiler emit a single `memmove`. This requires the binding struct to be trivially copyable (no pointers that need special handling).

Currently the loop copies field-by-field (`local_environment_frame.go:203-209`). If we accept that scopes/source are shared (already true — they're immutable at runtime), `copy(dst.bindings, p.bindings)` is equivalent and faster for large N.

**Files:** `environment/local_environment_frame.go`

### Tier 3: Larger Architectural Changes

#### 7. Flat Closures (Display-Based Environments)

Current model: closures capture a linked list of EnvironmentFrame nodes. `NewApplyFrame` copies the leaf frame's bindings. Parent chain is shared.

**Alternative:** At compile time, analyze which free variables a closure references. Copy only those values into a flat array on the closure. Eliminates:
- Parent-chain walk for `Up > 0` lookups
- Copying entire local frame (only copy free variables)
- EnvironmentFrame allocation (closure *is* its environment)

This is what Chez Scheme, Larceny, and Gambit do.

**Trade-offs:**
- Requires compile-time free-variable analysis pass
- Changes closure representation fundamentally
- `set!` on closed-over variables requires boxing (heap-allocate mutable cell, close over the box)
- Significant compiler + VM changes

#### 8. Stack Frames Instead of Continuation Chains

Replace per-call `MachineContinuation` allocation with a contiguous stack of frames. Save/restore becomes pointer arithmetic.

**Trade-off:** Incompatible with first-class continuations in general. Hybrid approach: stack frames for normal path, materialize continuation objects only when `call/cc` is invoked (stack-to-heap copy).

#### 9. NaN-Boxing or Tagged Pointers

`values.Value` is a Go interface (16 bytes). Small integers, booleans, characters could be encoded in 64 bits. Eliminates interface overhead, reduces stack/binding sizes by 50%.

**Trade-off:** Massive change affecting every value operation. Go's type system makes this awkward (unsafe.Pointer gymnastics).

## Recommended Execution Order (revised)

1. ~~**Foreign closure noCopyApply**~~ — **DONE** (`713661d`, -24.5% geo-mean)
2. ~~**2-arg numeric fast path**~~ — **DONE** (`6282c36`, -20.1% incremental, -33.4% combined)
3. ~~**Pull backing array fix**~~ — **DONE** (`db452db`, -13.0% incremental, -42.1% combined)
4. **#4 Eliminate variadic rest-arg cons cells** — current #1 allocator at 39.9%, higher risk
5. **#5 Evaluate sync.Pool overhead** — 21.9% of CPU, investigation needed
6. **#1 Eliminate PopAll allocation** — smaller impact now
7. **#7 Binding copy via `copy()`** — one-line change if scopes/source sharing is verified
8. **#2 Slim Binding struct** — requires audit of runtime scopes/source usage
9. **#3 Pool EnvironmentFrame** — needs lifetime analysis, moderate risk (but reconsider given sync.Pool overhead findings)
10. **#8 Flat closures** — large project, highest potential payoff, plan separately

## Measurement

Use existing `VMCounters` to track:
- `EnvsCopied` / `BindingsCopied` — measures impact of #2, #3
- `StackPopAlls` / `StackElementsCopied` — measures impact of #1
- `NoCopyApplies` / `NoCopyBindingsSaved` — baseline for what's already saved

Run benchmarks: `make bench-gabriel` for the 16-benchmark Gabriel suite. ZebraPuzzle (`go test -bench=BenchmarkZebraPuzzle`) for backtracking stress test. Profile with `go test -bench=X -cpuprofile` and `go test -bench=X -memprofile` for allocation analysis.

### Benchmark Categories

Gabriel benchmarks split into two groups, but the foreign closure fix showed both benefit equally since primitives dominate both:

**Call-heavy** (non-tail recursion): tak, takl, ctak, fib, ackermann, nqueens, sieve, deriv, primes, peval, triangl

**Arithmetic-dominated** (tail recursion): sum, sumfp, diviter, divrec, cpstak
