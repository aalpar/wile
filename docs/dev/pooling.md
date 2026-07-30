# Object Pooling Contract

The VM recycles short-lived allocations on the call/return hot path. Each
non-tail call creates a continuation frame and eval stack; pooling avoids
per-call heap allocations.

Three kinds of object (stacks, continuations, and environment frames) are
pooled **per thread**. `threadPools` (`pkg/machine/pool.go`) mints one set of
freelists at each thread root (`NewMachineContext`, `NewThreadSubContext`,
`AcquireTopLevelContext`) and every same-goroutine context inherits it by
reference, so no two goroutines ever touch the same freelist. Those use
`unsyncFreeList[T]`, which drops the mutex, the atomic counters, and the
enabled flag that the process-global pools carry: a frame allocated by one
thread is never released by another. A context with no thread root
(cold/expand-time) falls back to the process-global `FreeList[T]`
(mutex-guarded slice) of the same three kinds; the acquire/release helpers on
`MachineContext` branch between the two.

Both freelist flavors survive GC, unlike `sync.Pool` which is cleared every GC
cycle — a problem for recursive Scheme workloads where GC runs 1000+ times per
second. One pool uses `Pool[T]` (`sync.Pool`-backed): sub-contexts, which have
a longer lifecycle and lower churn.

For performance motivation and benchmark data, see
`docs/continuations/optimizations.md`.

---

## Pool Inventory

The process-global pools are registered with the package-level `PoolManager`
(`pools`) and defined in `pkg/machine/pool.go`. The per-thread freelists are
deliberately **not** registered: they are single-goroutine, so they must not
share the manager's lock or aggregate its counters.

| Pool | Type | Backend | Acquired | Released | Reset |
|------|------|---------|----------|----------|-------|
| `stackPool` | `*Stack` | per-thread `unsyncFreeList`, global `FreeList` fallback | `SaveContinuation` (standard path, depth > `inlineEvalsCap`), `AcquireTopLevelContext`, `acquireMacroContext`, `NewSubContext` | `RestoreAndRelease` (old mc.evals), `releaseStack` | Nil all slots, reset length to 0, retain backing array |
| `subContextPool` | `*MachineContext` | sync.Pool (global only) | `acquireSubContext`, `AcquireTopLevelContext` | `ReleaseSubContext`, `ReleaseTopLevelContext` | Release inner evals stack, zero all fields |
| `continuationPool` | `*MachineContinuation` | per-thread `unsyncFreeList`, global `FreeList` fallback | `NewMachineContinuationFromMachineContext`, `Copy` (global, by design) | `RestoreAndRelease` (unshared only) | Release inner evals stack, zero all fields |
| `envFramePool` | `*EnvironmentFrame` | per-thread `unsyncFreeList`, global `FreeList` fallback | `Apply` (copy path), `applyForeign`, `callForeignCached` | `RestoreAndRelease` (when `envPooled && oldEnv != newEnv`), `OpReleaseEnvFrame` | `ResetForPool()`, pre-allocates bindings cap 4 |

`Copy` acquires from the global continuation pool even when the copying thread
has its own, so a copied frame can be released into a per-thread pool later.
The asymmetry is accounting drift, not a race: see the comment on
`MachineContinuation.Copy`.

---

## Two Return Paths

The continuation pool has two return paths depending on whether `call/cc` has
captured the continuation chain.

### Normal Return (unshared)

The common case. The continuation frame is consumed exactly once.

```
SaveContinuation
  acquireContinuation() ──── pool ───► frame
  mc.evals transferred to frame (or inlined)
  mc.evals = acquireStack() ──── pool ───► new stack
  ...
RestoreAndRelease
  mc.evals ──── pool ◄─── release old stack
  frame.evals ──── transfer ───► mc.evals  (no copy)
  frame.evals = nil  (prevent double-release)
  frame ──── pool ◄─── releaseContinuation
```

Ownership transfers are zero-copy: the stack pointer moves from frame to
context, and the consumed frame returns to the pool.

### Continuation Capture (shared)

When `call/cc` or `CurrentContinuation` captures the continuation chain,
`MarkChainShared()` marks every frame in the chain as `shared = true`.
Shared frames may be re-invoked, so they cannot be recycled.

```
SaveContinuation
  acquireContinuation() ──── pool ───► frame
  ...
CurrentContinuation / call/cc
  cont.MarkChainShared()  ── marks all frames shared
  ...
RestoreAndRelease (shared path)
  mc.evals ──── pool ◄─── release old stack
  frame.evals ──── Copy() ───► mc.evals  (copy, not transfer)
  frame left for GC  (NOT pooled)
```

The frame's evals are copied (not transferred) so the captured continuation
retains its original state for re-invocation. The frame itself is never
returned to the pool — it remains live on the heap until no continuation
references it.

### MarkChainShared Early Exit

`MarkChainShared` walks from the current frame toward the root. If it
encounters a frame that is already shared, it stops — all ancestors must
already be shared from a prior capture. This makes repeated captures O(new
frames) rather than O(chain length).

---

## The `envPooled` Flag

`Apply` has two paths:

- **Copy path** (default): acquires an `EnvironmentFrame` from `envFramePool`,
  sets `mc.envPooled = true`. Critical for recursive functions with
  `SaveContinuation` — without copying, all invocations share the same
  bindings.
- **Nil-parent path**: reuses the closure's own environment for parentless
  top-level thunks (no local parameter bindings), sets `mc.envPooled = false`.

`RestoreAndRelease` checks `envPooled` before releasing the old environment:

```
if oldEnvPooled && oldEnv != newEnv:
    releaseEnvFrame(oldEnv)
```

The identity check (`oldEnv != newEnv`) prevents releasing a live frame when
no `Apply` occurred between save and restore (e.g., a foreign function call
where `oldEnv` and `cont.env` are the same pointer).

### Sites That Set `envPooled`

| Site | Value | Rationale |
|------|-------|-----------|
| `Apply` (copy path) | `true` | Frame from pool; safe to recycle |
| `Apply` (nil-parent path) | `false` | Closure's own env; must not recycle |
| `applyForeign`, `callForeignCached` | `true` | Fresh frame per foreign call (SRFI-18 binding-slot races) |
| `OpReleaseEnvFrame` | `false` | Frame released early before a reclaimable tail call; clearing prevents a double release |
| `RestoreAndRelease` (unshared) | from continuation | Propagates caller's ownership |
| `RestoreAndRelease` (shared) | `false` | Shared chain may be re-invoked; env must stay live |
| `OpMakeClosure` | `false` | Closure captures `mc.env` via parent chain; must not recycle |
| `OpPopEnv` | `false` | Parent env was never pooled |
| `NewSubContext` | `false` (zero) | Fresh context |
| `BindPatternVars` | `false` | Heap-allocated child env |

**Invariant**: every site that writes `mc.env` must also set `mc.envPooled`.
Violating this can cause `releaseEnvFrame` on a non-pooled frame — a
use-after-release if a closure still references it.

---

## Composable Continuations

`ComposableContinuation.AcquireSegment` has a first-invocation optimization:

1. **First call**: marks the segment shared via `MarkChainShared` and returns
   it directly. No `DeepCopy`. Shared marking ensures `RestoreAndRelease`
   copies evals instead of transferring, preserving frames for re-invocation.

2. **Subsequent calls**: resets the bottom frame's parent to `nil` (undoing
   `GraftContinuation`'s parent mutation), then returns `DeepCopy()` of the
   segment. Each copy acquires frames from `continuationPool`.

---

## Inline Evals Optimization

When the eval stack at `SaveContinuation` time has 2 or fewer elements
(`inlineEvalsCap = 2`), values are copied into fixed-size slots
(`inlineEvals[2]`) on the continuation struct. The `evals` pointer is set to
`nil` as a sentinel.

This avoids a stack pool acquire/release round-trip for the common case.
Profile data confirms save-time depths of 0-1 account for >95% of
continuations.

`RestoreAndRelease` detects `cont.evals == nil` and reconstructs from inline
slots, clearing them before pooling (unshared) or leaving them intact
(shared, for re-invocation).

---

## Observability

`PoolManager` (`pkg/machine/pool_generic.go`) provides unified observation and
control over the four process-global pools. It does **not** see the per-thread
freelists, so on a rooted context (which is every ordinary execution) the
stack, continuation, and env-frame counters it reports stay near zero. Read
`unsyncFreeList.Stats()` from the owning goroutine for those.

| Method | Purpose |
|--------|---------|
| `AllStats()` | Point-in-time `PoolSnapshot` for each pool (acquires, releases, misses, in-flight) |
| `DrainAll()` | Clears all pools (triggers `runtime.GC()` for sync.Pool; drains FreeList slices) |
| `SetAllEnabled(bool)` | Toggle all pools on/off; disabled pools allocate fresh and discard on release |
| `String()` | Tabular summary of all pool counters |

Per-context counters on `MachineContext` (`counters` field) track pool
operations at a finer grain: `StackPoolReleases`, `ContinuationPoolReleases`,
`EnvFramePoolReleases`, `SharedFrameRestores`, `InlineEvalsSaved`, etc.
