# Continuation-Heavy Workload Optimizations

This document explains the performance optimizations in the VM's Apply, continuation, and environment paths. These optimizations reduced allocation pressure by **40%** and wall time by **23%** on continuation-heavy workloads (the Zebra puzzle benchmark: 24.2s → 18.6s, 36.2 GB → 21.5 GB allocated).

The code touched by these optimizations is intentionally more complex than a naive implementation. Each optimization exists because profiling identified a specific allocation hot spot responsible for gigabytes of memory pressure. **Do not simplify this code without understanding what it costs.**

---

## Why This Matters

The VM spends ~70% of wall time in the Go garbage collector on continuation-heavy workloads. The actual `Run()` loop is ~11% of wall time. The bottleneck is allocation pressure and resulting memory traffic, not VM dispatch speed.

This was confirmed empirically:

| GOGC | Wall Time | Implication |
|------|-----------|-------------|
| 100 (default) | 24.2s | Baseline |
| 400 | 20.7s | Less GC frequency helps |
| off | 21.9s | 36 GB working set trashes CPU caches |

`GOGC=off` being *slower* than `GOGC=400` proves the problem is memory traffic, not just GC overhead. More allocations mean more cache misses even without collection.

---

## The Apply Hot Path

Every closure application (`Apply`) is the central hot path. The Zebra benchmark executes **112.9 million** closure applications. Before optimization, each one allocated:

1. A `LocalEnvironmentFrame` (bindings copy)
2. An `EnvironmentFrame` (wrapper with parent chain)
3. A `*LocalIndex` per variable load/store
4. Growth of the eval stack from nil after each `PopAll`

The diagram below shows the allocation flow for a single closure call:

```
┌─────────────────────────────────────────────────────────────────────┐
│                     Before: Apply Allocation Flow                   │
│                                                                     │
│  SaveContinuation                                                   │
│    ├── acquireContinuation()     ← pooled (already optimized)       │
│    └── mc.evals transferred to continuation frame                   │
│                                                                     │
│  Apply(closure, args...)                                            │
│    ├── CopyForApply()            ← alloc: []Binding copy            │
│    │     └── []*Binding          ← alloc: N pointer slots (GONE)    │
│    │           └── *Binding ×N   ← alloc: N heap objects (GONE)     │
│    ├── NewEnvironmentFrameWithParent() ← alloc: wrapper (GONE)      │
│    └── OpPush after PopAll       ← alloc: stack growth from nil     │
│                                                                     │
│  OpLoadLocal / OpStoreLocal                                         │
│    └── NewLocalIndex()           ← alloc: *[2]int (GONE)            │
│                                                                     │
│  call/cc capture                                                    │
│    └── DeepCopy() entire chain   ← alloc: O(depth) frames (GONE)   │
│                                                                     │
│  (GONE) = eliminated by optimizations                               │
└─────────────────────────────────────────────────────────────────────┘
```

---

## Optimization 1: LocalIndex Value Type

**Files:** `environment/environment_frame.go`, `machine/machine_context.go`
**Allocation saved:** 1.8 GB, 110.7M allocations

### Problem

`NewLocalIndex` returned `*LocalIndex` (pointer to `[2]int`), forcing a heap escape on every `OpLoadLocal` and `OpStoreLocal`. The VM loop executed these billions of times.

### Solution

Added `GetLocalBindingBySlotDepth(slot, depth int)` and `SetLocalValueBySlotDepth(slot, depth int, v)` to `EnvironmentFrame`. The VM decodes slot and depth as raw integers from the instruction's packed `Arg` field, bypassing the `*LocalIndex` allocation entirely.

### Why the code looks this way

`environment_frame.go` has two parallel sets of methods: `GetLocalBinding(*LocalIndex)` / `SetLocalValue(*LocalIndex, v)` and `GetLocalBindingBySlotDepth(slot, depth)` / `SetLocalValueBySlotDepth(slot, depth, v)`. The `*LocalIndex` variants still exist for compile-time use (where allocation doesn't matter). The `BySlotDepth` variants are the hot-path versions used by the VM loop. **Do not consolidate these** — the duplication eliminates a heap allocation per VM instruction.

---

## Optimization 2: Value-Type Bindings

**Files:** `environment/local_environment_frame.go`
**Allocation saved:** 1.3 GB, 113.1M allocations

### Problem

`LocalEnvironmentFrame.bindings` was `[]*Binding`. Each `CopyForApply` allocated N pointer slots plus N individual `Binding` heap objects.

### Solution

Changed to `[]Binding` (value slice). Now `CopyForApply` does a single `make([]Binding, n)` and copies structs directly. All accessors return `&p.bindings[i]` (pointer to slice element).

### Why the code looks this way

All binding access uses `&p.bindings[i]` rather than `p.bindings[i]`. This is deliberate — returning a pointer to the slice element allows mutation in place without an extra copy. Code that previously held `*Binding` pointers across frame copies was audited and confirmed safe: no code retains a `*Binding` that outlives the frame it came from.

---

## Optimization 3: EnvironmentFrame Struct Fusion

**Files:** `environment/environment_frame.go`, `machine/machine_context.go`
**Allocation saved:** 1.6 GB, 113.1M allocations (exactly matching `closures_applied`)

### Problem

Each `Apply` created two allocations: a `LocalEnvironmentFrame` (via `CopyForApply`) and an `EnvironmentFrame` (via `NewEnvironmentFrameWithParent`). The `EnvironmentFrame` was a thin 56-byte wrapper.

### Solution

Embedded `LocalEnvironmentFrame` by value inside `EnvironmentFrame`:

```
┌──────────────────────────────────────────────────────┐
│ Before: Two allocations per Apply                    │
│                                                      │
│  EnvironmentFrame ──ptr──▶ LocalEnvironmentFrame     │
│  (56 bytes heap)            (keys + bindings heap)   │
│                                                      │
├──────────────────────────────────────────────────────┤
│ After: One allocation per Apply                      │
│                                                      │
│  EnvironmentFrame                                    │
│  ├── parent, global, phaseLevel, phases, namespace   │
│  └── local: LocalEnvironmentFrame (embedded value)   │
│       ├── keys map[Symbol]int (shared, CoW)          │
│       └── bindings []Binding (fresh copy)            │
│                                                      │
│  Sentinel: local.keys == nil means "no local env"    │
└──────────────────────────────────────────────────────┘
```

The fused allocation `NewApplyFrame()` replaces the old `CopyForApply() + NewEnvironmentFrameWithParent()` two-step.

### Why the code looks this way

`EnvironmentFrame.hasLocal()` checks `local.keys != nil` — this is the sentinel for "frame has no local bindings." This is a zero-value sentinel, not a nil pointer check. Code paths that previously nil-checked a `*LocalEnvironmentFrame` pointer now check `hasLocal()`. The `LocalEnvironment()` method returns `&p.local` (pointer to embedded field), or nil if `!hasLocal()`. **The nil sentinel is load-bearing** — removing it breaks frames that legitimately have no local bindings (top-level, global-only).

The `copyForApplyInto(dst *LocalEnvironmentFrame)` method copies bindings into a pre-allocated destination rather than returning a new allocation. This is the mechanism that eliminates the second heap allocation.

---

## Optimization 4: Stack Backing Array Retention

**Files:** `machine/stack.go`
**Allocation saved:** 4.5 GB, 192.7M allocations

### Problem

`PopAll` previously set `*p = nil`, discarding the backing array. After every `Apply` (112.9M times), the stack was nil. The next `Push` did `append(nil, v)`, triggering a fresh allocation. This growth chain from nil produced 264.7M allocations (7.8 GB).

### Solution

`PopAll` now copies data out and retains the backing array:

```
┌──────────────────────────────────────────────────────┐
│ Before: PopAll discards backing array                │
│                                                      │
│  PopAll: *p = nil                                    │
│  Push:   append(nil, v) → alloc new backing array    │
│  Push:   append(len=1,  v) → may grow again          │
│          ... growth chain repeats every Apply         │
│                                                      │
├──────────────────────────────────────────────────────┤
│ After: PopAll retains backing array                  │
│                                                      │
│  PopAll:                                             │
│    result = make([]Value, n)  ← caller's copy        │
│    copy(result, *p)                                  │
│    clear((*p)[:n])            ← GC can collect vals  │
│    *p = (*p)[:0]              ← keep cap, reset len  │
│                                                      │
│  Push:   append(len=0/cap=8, v) → no allocation      │
└──────────────────────────────────────────────────────┘
```

97.4% of `PopAll` events had depth 0–2. The pool-provided capacity of 8 covers 99.94% of cases.

### Why the code looks this way

`PopAll` has a `clear((*p)[:n])` call between `copy` and the length reset. This zeroes the retained slice elements so the GC can collect the values they pointed to. **Removing this line causes memory leaks** — the backing array would hold stale references to values that should be collected. The `make([]Value, n)` for the result is the one remaining allocation per non-empty `PopAll`; this is unavoidable because the caller needs exclusive ownership of the data.

---

## Optimization 5: Shared-Flag Continuation Optimization

**Files:** `machine/machine_continuation.go`, `machine/machine_context.go`, `machine/pool.go`
**Allocation saved:** 3.8 GB, 52.8M allocations

This is the most architecturally significant optimization and the one most likely to be misunderstood.

### Problem

`call/cc` captures the current continuation chain. The old implementation called `DeepCopy()`, which deep-copied every frame in the chain. This was necessary because `RestoreAndRelease` pools frames — if a captured continuation shares frames with the live chain, pooling corrupts the capture.

### Solution: Lazy Sharing Protocol

Instead of eagerly deep-copying on capture, mark frames as shared and defer the work to restore time:

```
┌──────────────────────────────────────────────────────────────────┐
│ Capture (call/cc): O(depth) mark, not O(depth) copy             │
│                                                                  │
│  cont chain:  [A] → [B] → [C] → [D] → nil                      │
│                                                                  │
│  MarkChainShared() walks the chain:                              │
│    A.shared = true                                               │
│    B.shared = true                                               │
│    C.shared = true   ← if C.shared was already true, STOP       │
│    (D and beyond already shared from a prior capture)            │
│                                                                  │
│  Key insight: sharing propagates toward the root.                │
│  If frame C is shared, all its ancestors must be too.            │
│  The early-exit makes repeated captures O(new frames only).      │
│                                                                  │
├──────────────────────────────────────────────────────────────────┤
│ Restore (normal return): Branch on shared flag                   │
│                                                                  │
│  RestoreAndRelease(cont):                                        │
│    if cont.shared:                                               │
│      ├── Copy evals (preserve for re-invocation)                 │
│      └── Do NOT pool frame (leave for GC)                        │
│    else:                                                         │
│      ├── Transfer evals ownership (zero-copy)                    │
│      └── Pool frame via releaseContinuation()                    │
│                                                                  │
│  The shared branch does the SAME work as the old DeepCopy,       │
│  but only for frames that are actually restored — not the        │
│  entire chain at capture time.                                   │
└──────────────────────────────────────────────────────────────────┘
```

### Why the code looks this way

**`MachineContinuation.shared bool`**: This field controls whether `RestoreAndRelease` can pool the frame. It is set by `MarkChainShared()` during `call/cc` capture and never cleared. Once shared, always shared — clearing it would allow pooling of a frame that another continuation still references.

**`MarkChainShared()` early exit**: The `if frame.shared { return }` in `MarkChainShared()` is the critical optimization for repeated captures. In a coroutine-style workload, `call/cc` is called many times with mostly the same ancestor chain. Without the early exit, each capture would re-walk the entire chain. With it, each capture only marks the new frames since the last capture.

**`RestoreAndRelease` two branches**: The `if cont.shared` branch in `RestoreAndRelease` copies evals and skips pooling. The `else` branch transfers evals and pools. **These must remain separate paths.** The shared path preserves the frame for re-invocation; the unshared path recycles it. Merging them (e.g., always copying, always pooling) either corrupts captured continuations or defeats the pooling optimization.

**`releaseContinuation` precondition**: The comment on `releaseContinuation` in `pool.go` states shared frames must NOT be passed to it. This is enforced by the `if cont.shared` check in `RestoreAndRelease`. There is no runtime assertion — the check is structural, not defensive.

**`DeepCopy` still exists**: It is still used by composable continuations (`applyComposableContinuation`), which need a full independent copy of a continuation segment before grafting it onto a different chain. The `call/cc` path no longer uses it. **Do not remove `DeepCopy`** — it serves a different use case.

### Invariants

1. If `frame.shared == true`, all ancestors of `frame` are also shared
2. `shared` is monotonic: once true, never reverted to false
3. `RestoreAndRelease` never pools a shared frame
4. `DeepCopy` is only used for composable continuations, not `call/cc`

---

## Optimization 6: Compile-Time Escape Analysis (NoCopyApply) — REMOVED

> **Removed in PR #561.** NoCopyApply was removed to prevent data races when
> SRFI-18 threads concurrently call the same closure. The optimization reused
> the closure's own environment frame for leaf functions, but concurrent callers
> would write parameters to the same binding slots, producing torn reads on the
> two-word `values.Value` interface. Apply now always acquires a fresh env frame
> from the pool (except for parentless top-level thunks with no parameters).

**Files (historical):** `machine/native_template.go`, `machine/machine_context.go`, `machine/compile_validated.go`
**Allocation saved (historical):** 1.8 GB, 24.6M allocations

### Historical: How it worked (pre-PR #561)

The following describes the optimization as it existed before removal. It is
retained for historical context — none of this code exists in the current
codebase.

#### Problem

Every `Apply` copied the closure's environment frame to prevent parameter
corruption from recursive calls. But many closures (leaf functions, simple
predicates) never captured their environment — they had no `SaveContinuation`
and no `MakeClosure`. For these, the copy was wasted work.

#### Solution

After compiling a lambda body, `computeNoCopyApply()` scanned the bytecode:

```
┌──────────────────────────────────────────────────────────────────┐
│ Escape Analysis: Can bindings outlive the call?                  │
│                                                                  │
│  Template bytecode scan (code[] only):                           │
│    code[] contains OpSaveContinuation?  → bindings may be        │
│      captured in a continuation frame (mc.env saved to cont)     │
│                                                                  │
│    code[] contains OpMakeClosure? → bindings may be captured     │
│      as a closure parent (env becomes closure.env)               │
│                                                                  │
│  If NEITHER was present:                                         │
│    noCopyApply = true                                            │
│    Apply reused closure's own env frame (0 allocations)          │
│                                                                  │
│  If EITHER was present:                                          │
│    noCopyApply = false                                           │
│    Apply took the standard NewApplyFrame() copy path             │
│                                                                  │
│  This was a CONSERVATIVE analysis — any SaveContinuation or      │
│  MakeClosure ANYWHERE in the template disabled the optimization, │
│  even if unreachable. No control flow analysis was performed.    │
└──────────────────────────────────────────────────────────────────┘
```

#### Why the code looked this way

**`NativeTemplate.noCopyApply` field**: Set once after compilation by
`computeNoCopyApply()`. Never changed after that. The field was checked on
every `Apply` call in the noCopy path of `MachineContext`, so it had to be
pre-computed, not calculated per call.

**Conservative analysis**: The analysis had no false positives (never marked a
template as safe when it wasn't), but had false negatives (marked some safe
templates as unsafe). This was the correct trade-off: a false positive would
corrupt execution; a false negative only cost one extra allocation. In the
Zebra benchmark, 10.9% of applications took the no-copy path — a modest but
measurable win.

**Two escape paths**: `OpSaveContinuation` captured `mc.env` into the
continuation chain (it became `cont.env`). `OpMakeClosure` captured `mc.env`
as a closure's parent environment. Both allowed the bindings to outlive the
call frame. If neither was present, the bindings were dead after the call
returned, so sharing them was safe.

#### Why it was considered safe (single-threaded)

When `noCopyApply == true`, the noCopy Apply path reused `mcls.env` directly,
mutating its bindings in place for the new call's parameters. This was safe
in single-threaded execution because:

1. No `SaveContinuation` meant no non-tail calls, so no recursive invocations that would read the old parameter values while new ones were being written
2. No `MakeClosure` meant no inner closures that referenced the bindings after the call returned

If either condition was violated, the standard copy path (via `NewApplyFrame`)
was taken, creating fresh bindings independent of the closure's template.

#### Why it was removed

The analysis was correct for single-threaded execution but unsound under SRFI-18
concurrency. When multiple threads called the same closure concurrently, all
callers wrote parameters to the same binding slots. `Binding.value` is a
`values.Value` interface (two machine words); concurrent writes produced torn
reads — one thread's type pointer with another's data pointer.

---

## Summary of Structural Changes

| Component | Before | After | Why |
|-----------|--------|-------|-----|
| `LocalEnvironmentFrame.bindings` | `[]*Binding` | `[]Binding` | Eliminates N pointer + N object allocations per copy |
| `EnvironmentFrame.local` | `*LocalEnvironmentFrame` | `LocalEnvironmentFrame` (embedded) | Eliminates wrapper allocation per Apply |
| `EnvironmentFrame.hasLocal()` | nil pointer check | `local.keys != nil` sentinel | Zero-value sentinel for embedded struct |
| `NewApplyFrame()` | `CopyForApply() + NewEnvironmentFrameWithParent()` | Single fused method | One allocation instead of two |
| `Stack.PopAll()` | `*p = nil` | Copy out, retain backing array | Prevents growth-from-nil chain |
| `CurrentContinuation()` | `DeepCopy()` | `MarkChainShared()` | O(new) mark vs O(depth) copy |
| `RestoreAndRelease()` | Always pool | Branch on `shared` flag | Preserves shared frames for re-invocation |
| `OpLoadLocal` / `OpStoreLocal` | `NewLocalIndex()` → `*[2]int` | `GetLocalBindingBySlotDepth(slot, depth)` | Bypasses pointer allocation |
| `NativeTemplate.noCopyApply` | N/A | ~~Compile-time escape analysis flag~~ | **Removed** (PR #561) — SRFI-18 thread safety |

---

## Measured Results (Zebra Benchmark, Apple M4 Max)

| Metric | Before | After | Reduction |
|--------|--------|-------|-----------|
| Wall time | 24.2s | 18.6s | 23% |
| Bytes allocated | 36.2 GB | 21.5 GB | 41% |
| Allocation count | 907.8M | 300.9M | 67% |

---

## What Not To Simplify

1. **Do not merge the `BySlotDepth` methods with the `*LocalIndex` methods.** The duplication avoids a heap allocation per VM instruction.

2. **Do not remove the `shared` flag and go back to `DeepCopy` for call/cc.** The flag turns O(chain depth) copies into O(new frames) marks.

3. **Do not remove `clear()` from `PopAll`.** It prevents stale GC references in the retained backing array.

4. **Do not merge the two `RestoreAndRelease` branches.** Shared frames must not be pooled; unshared frames should be.

5. ~~**Do not remove `computeNoCopyApply` or the `noCopyApply` flag.**~~ **Removed in PR #561** to fix SRFI-18 thread races. The optimization saved allocations for 10.9% of closure applications but was unsafe under concurrent invocation.

6. **Do not embed `*LocalEnvironmentFrame` by pointer again.** The value embedding eliminates one allocation per `Apply` — 112.9M allocations in the benchmark.

7. **Do not change `PopAll` to discard the backing array.** The current code retains it for reuse, avoiding 192.7M re-allocations.

---

## References

- `machine/pool.go` — Continuation and stack pooling
- `machine/machine_context.go` — `RestoreAndRelease` with shared-flag branching
- `machine/machine_context_apply.go` — Apply always-copy path (nil-parent exception)
- `machine/machine_continuation.go` — `MarkChainShared` with early exit
- `environment/environment_frame.go` — `NewApplyFrame` fused allocation
- `machine/stack.go` — `PopAll` with backing array retention
