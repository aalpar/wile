# Per-Thread Allocation Pools — and the Continuation-Confinement Invariant They Depend On

**Date**: 2026-06-08.
**Status**: Invariant recorded; pool surgery NOT yet implemented.
**Scope**: Documents a load-bearing invariant *before* the per-thread pool work,
so the precondition cannot silently regress in the meantime.

## The intended design (not yet built)

To get real CPU parallelism from SRFI-18 threads, the allocation substrate must
stop being process-global. Today `stackPool`, `continuationPool`, and
`envFramePool` (`machine/pool.go`) are package-level `var`s — mutex-guarded
freelists with atomic stat counters, hit on **every non-tail closure call**
(`envFramePool` in the `Apply` copy path, `continuationPool` on every non-tail
call, `stackPool` per sub-context). Two goroutines doing compute serialize on
those mutexes and ping-pong the counters' cache lines. Measured: 16-thread
env-frame acquire/release ≈ 80× the 1-thread cost with pooling on, ≈ 48× with
pooling off (see `memory/vm-no-cpu-parallelism.md`).

The fix is to anchor the pools **per thread**, at the root `MachineContext`:

- `NewMachineContext` (primordial thread root) — **mints** a fresh pool.
- `NewThreadSubContext` (spawned thread root) — **mints** a fresh pool.
- `NewSubContext` (same-goroutine child) — **inherits** the parent's pool by
  pointer.

The pool reference is a plain `MachineContext` field (like `parentMC`, `thread`,
`reconfigured`) — **never in `vmState`**, because `vmState` is embedded by
`MachineContinuation` and save/restored on every continuation op; a pooled
reference riding there would be swapped around by `Restore`.

With per-thread pools each freelist's mutex is uncontended: the `FreeList` still
carries its `sync.Mutex` (`machine/pool_generic.go`), but the lock is touched by
exactly one goroutine, so it never serializes and its cache line never ping-pongs
between cores. The win is the removal of *contention*, not of the lock itself.

## Why this is safe — the invariant

Per-thread pools are correct **only because continuations are thread-confined**.
A frame allocated by thread A's execution is released back to a pool only by A's
execution. The only way thread B could release an A-allocated frame is by
*executing* a continuation captured in A. Wile forbids exactly that:

- **`applyCapturedContinuation`** (`machine/captured_continuation.go`) — rejects
  invoking a call/cc escape continuation from a different thread:
  `if p.ThreadID() != capt.threadID → werr.ErrCrossThreadContinuation`.
- **`applyComposableContinuation`** (`machine/machine_context_apply.go`) — the
  same check for composable continuations captured via
  `call-with-composable-continuation`.
- The call/cc escape closure (`registry/core/prim_exit.go`) carries the same
  `capturingThreadID` check.

A continuation is a first-class value, so its *reference* can reach another
thread (shared variable, channel). But its *invocation* is thread-stamped and
rejected. Continuations can be referenced across threads, never executed across
them — and only execution touches pools.

### Belt-and-suspenders: captured frames leave the pool entirely

Independently of threading, frames captured by *any* continuation are marked
shared (`MarkChainShared` during capture) and are **never returned to a pool** —
the restore path (`machine/machine_context_continuation.go`) skips pooling when
`cont.shared`, leaving those frames for GC (`machine/pool.go` documents
"Shared frames (marked by call/cc) are never pooled"). So even if the thread
check were somehow bypassed, a captured frame is outside the pool system and
there is nothing to corrupt — only a `shared` frame, GC-managed.

These two guarantees are orthogonal: thread-confinement stops B from running A's
frames; shared-frame exclusion stops any captured frame from being pooled at all.

## What would break it

A **work-stealing / continuation-migration** scheduler — moving a continuation
from one OS thread to another for load balancing — re-introduces cross-thread
execution and is incompatible with per-thread pools as designed. Choosing
per-thread pools is implicitly choosing **not** to do continuation migration.
This aligns with the share-nothing / "places" concurrency model, where
thread-confined continuations are a natural property rather than a restriction.
If migration is ever wanted, the allocator must be reworked first (e.g.
per-scheduler-P pools, or relying entirely on the shared-frame exclusion).

## Enforcement

- `werr.ErrCrossThreadContinuation` is the sentinel; the checks above must keep
  returning it.
- Regression test:
  `extensions/threads/prim_threads_continuation_invariant_test.go`
  (`TestCrossThreadContinuationIsAllocatorInvariant`) pins the **exact** sentinel
  for both continuation flavors (captured and composable). Do not weaken these
  checks without reworking the allocation design and updating this document.
- The enforcement sites carry a short comment pointing back here.
