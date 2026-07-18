# Blocking synchronization primitives and VM cancellation

How Scheme-visible blocking locks (`rw-mutex-write-lock!`, `rw-mutex-read-lock!`,
SRFI-18 `mutex-lock!`) couple to the VM's context-cancellation machinery, so a
thread parked *acquiring* one observes `thread-terminate!` / a VM deadline instead
of parking a goroutine forever.

**Status:** as-built. `values.RWMutex` and the SRFI-18 `values.Mutex` acquire
through a single cond-based ctx bridge; a thread blocked on either is unparked by
`thread-terminate!`. Go channels and Go-style wait-groups, which previously shared
this concern, were removed from the Scheme surface 2026-07-17 (`git log` for the
history); this document covers only the locks that remain.

**Code:** `pkg/values/cond_wait.go` (`waitOnCondCtx`, the one ctx-to-cond bridge),
`pkg/values/rw_mutex.go` (the cond-based `RWMutex` state machine),
`pkg/values/mutex.go` (`Mutex.LockContext`, the SRFI-18 mutex),
`extensions/gointerop/prim_gointerop.go` (`finishBlockingSync`, the primitive-layer
cancellation policy), `pkg/machine/call_foreign_cached.go` (`callForeignCached`,
the eager timer recheck this design leans on), `pkg/values/thread.go`
(`Thread.Terminate`, the SRFI-18 ctx; `Thread.setOutcome`, the write-once rule).

---

## What problem this solves

`sync.WaitGroup.Wait` and `sync.RWMutex.Lock`/`RLock` have no ctx-aware form: a
goroutine parked in one cannot be interrupted. When the Scheme lock primitives
wrapped those Go types directly, `(thread-terminate! t)` on a thread blocked
acquiring a lock cancelled the thread's ctx, marked it terminated, and left the
goroutine parked forever. `thread-join!` then raised `JoinTimeoutException` — the
observable form of a leaked goroutine — and the VM's teardown stalled. Locks were
a blocking primitive family that ignored `ctx`; `thread-sleep!` had always
honored it.

The naive fix — run the blocking call in a helper goroutine and `select` on
`ctx.Done()` — is wrong for a lock. If ctx wins the race the helper is still
parked in `Lock()` and will *eventually acquire*, but the Scheme caller already
returned; nobody unlocks it, so the lock is held by a phantom forever. That
converts a goroutine leak into a permanent deadlock. Acquisition has to be
**atomic with cancellation**: either win the state transition or return without
acquiring, never "cancelled but the lock silently landed anyway."

## The structural change: Wile-owned state machines

`values.RWMutex` and `values.WaitGroup` (the latter since removed) dropped their
`sync` wrappers for explicit state machines guarded by a `sync.Mutex` + `sync.Cond`;
the SRFI-18 `values.Mutex` already had this shape. Acquisition transitions the
state *under the guard mutex*, so it is atomic with cancellation — no phantom
hold. Blocking waits go through one helper:

```
// waitOnCondCtx: park on cond until woken OR ctx cancelled. Caller holds cond.L,
// calls it in a predicate loop. Returns false iff ctx cancelled.
func waitOnCondCtx(ctx, cond) bool {
    if ctx.Err() != nil { return false }
    done := make(chan struct{})
    go func() {
        select {
        case <-ctx.Done(): cond.L.Lock(); cond.Broadcast(); cond.L.Unlock()
        case <-done:
        }
    }()
    cond.Wait()
    close(done)
    return ctx.Err() == nil
}
```

The side goroutine takes `cond.L` before it `Broadcast`s. Because the caller holds
`cond.L` continuously until `cond.Wait` atomically releases it and parks, that
Broadcast cannot land before the caller is parked — the lost-wakeup that would
otherwise hang the waiter forever. The goroutine is bounded: it exits on
`ctx.Done()` or on `done` closing after `Wait` returns.

`RWMutex` acquisition (`LockContext`/`RLockContext`) loops on `waitOnCondCtx`
until grantable, then flips the state; on cancellation it returns `false` with the
state untouched. Writers are preferred (a waiting writer blocks new readers), a
deliberate self-contained fairness choice that differs slightly from `sync.RWMutex`.

## The primitive-layer policy: wait side vs held side

A thread blocked **acquiring** wakes on ctx and returns *without acquiring*, so
nothing is half-held. A thread that already **holds** a lock is untouched: a
terminated holder's lock stays held. Force-unlocking it would expose the guarded
resource mid-transition, out of serialization order — the exact race the lock
exists to prevent. A stuck lock is the safe outcome; there is deliberately no
abandonment for `RWMutex` (SRFI-18 `Mutex` keeps its own owner-driven
`MarkAbandoned`, which is a different, spec-mandated path).

How a cancelled acquire surfaces to Scheme differs by primitive, and the
difference is real, not accidental:

- `rw-mutex-*-lock!` return `Void` on success, so they have no value channel for
  "did not acquire"; they raise `werr.ErrOperationCancelled` (via
  `finishBlockingSync`), EXCEPT on the `with-timeout` source — see the carve-out
  below.
- `mutex-lock!` already signals "did not acquire" as `#f` (its timeout form does),
  so a cancelled acquire returns `#f`. Returning it *error-free* also lets a
  wrapping `with-timeout` handler run without a carve-out (see below).

## Who cancels the ctx: three sources, three safety arguments

The ctx a lock op selects on is `mc.Context()`. Three distinct events cancel it,
and correctness holds for a *different reason* in each case.

| Source | Cause on ctx | Why the handling is safe |
|---|---|---|
| `with-timeout` | `ErrTimerExpired` (`WithTimeoutCause`) | **Eager recheck + carve-out.** `callForeignCached` does a non-blocking `ctx.Done()` check *after every foreign return* and, on `ErrTimerExpired`, returns `ErrTimerInterrupt` — but only on the *error-free* return path (`if err != nil` returns first). So `finishBlockingSync` returns a placeholder `Void` (not `ErrOperationCancelled`) on this source, and `mutex-lock!` returns `#f`; the recheck then fires and the timeout handler runs, discarding the placeholder before anything observes it. |
| `thread-terminate!` | `context.Canceled` (plain `cancel()`) | **The outcome is claimed, not raced.** The eager recheck is `ErrTimerExpired`-only, so it does not fire here; the acquire raises `ErrOperationCancelled` (or returns `#f`). Safety comes from `Terminate` storing the SRFI-18 terminated-thread exception *before* it cancels, plus the outcome being write-once (`Thread.setOutcome`): neither the raised error nor a returned value can become the thread's result, whichever path ends the goroutine. |
| Embedder deadline | `context.DeadlineExceeded`, `mc.timer == nil` | **The path the distinct sentinel exists for.** No eager recheck (not a timer) and no thread teardown, so `ErrOperationCancelled` propagates as a distinct, catchable condition. The op still parks up to `contextCheckMask` (≈1024) ops before `DeadlineExceeded` reaches it — the same VM-wide cancellation latency every primitive has — but it replaces the old **infinite hang** with a bounded, correctly-labelled result. |

The single mechanism that makes the first row work — the eager `ErrTimerExpired`
recheck in `callForeignCached` — is the invariant `finishBlockingSync`'s carve-out
comment must name. Remove or narrow it and the `with-timeout` composition
regresses: the parked op's `ErrOperationCancelled` escapes instead of the handler
running. `TestWithTimeoutInterruptsParkedRWMutex` is the test that fails when it
does.

### The tail-position hole (found and fixed 2026-07-16)

The `thread-terminate!` row's safety was originally attributed to the ≈1024-op
unwind window: the terminated thread would reach the top-of-loop ctx check and its
value would be discarded on that unwind. It is not, when the parked acquire sits
in **tail position of the thread thunk** — nothing follows it, so no op ever
triggers the check, and the acquire's ordinary return (a `#f`, or a value) becomes
the thunk's result. `Thread.Start`'s goroutine then overwrote the exception
`Terminate` had stored, and `(thread-join! t)` reported a terminated thread as
having *succeeded*.

The defect was in `thread.go`. SRFI-18 gives `thread-terminate!` an *outcome*, not
merely an effect: a terminated-thread exception stored in the end-exception field,
which `thread-join!` raises. The outcome is now write-once, so the first writer
(`Terminate`) wins over the goroutine's completion path, and no ordinary return can
be reported as a terminated thread's result. Pinned channel-free by
`TestThreadTerminateStoresEndException`.

## Boundaries and non-goals

- **Held-lock abandonment is out of scope.** A terminated thread that holds a lock
  does not release it (see the policy section). For `RWMutex` this is now
  *possible* — Wile owns the state machine, so a terminated holder *could* release
  — but it remains an unproposed follow-up, deliberately not built, because
  releasing mid-transition is the race we are avoiding.
- **The timed `mutex-lock!` path wakes within its timeout, not immediately.** Only
  the *untimed* `Mutex.Lock` slow path was made ctx-aware; a thread in
  `(mutex-lock! m T)` under termination wakes within `T` (bounded, never an
  infinite stall), which was judged sufficient.
- **No transactional guarantee across operations.** These are locks, not
  transactions; standard mutual-exclusion semantics apply.

## Test coverage

`extensions/gointerop/sync_cancellation_test.go` drives the integrations from
Scheme (they live above `pkg/values` and cannot be reached from a Go-level test):

- `TestTerminateUnparksBlockedSyncPrimitive` — parks a thread acquiring each lock
  (`rw-mutex-write-lock!`, `rw-mutex-read-lock!`, `mutex-lock!`) behind a lock the
  main thread holds, terminates it, and joins. The rendezvous is a lock-free
  `atomic` flag polled with `thread-yield!`. A leaked goroutine surfaces as
  `JoinTimeoutException`; reaching the terminated-thread exception proves the
  goroutine exited on ctx cancellation.
- `TestEmbedderDeadlineRWMutexRaisesCancelled` — a `rw-mutex-write-lock!` parked
  under an embedder deadline raises the distinct `ErrOperationCancelled`.
- `TestWithTimeoutInterruptsParkedRWMutex` — a `rw-mutex-write-lock!` parked inside
  a `with-timeout` runs the handler (returns its value), guarding the
  `ErrTimerExpired` carve-out.

The SRFI-18 write-once outcome the terminate test leans on is pinned separately,
and lock-free, in `extensions/threads/prim_threads_terminate_outcome_test.go`
(`TestThreadTerminateStoresEndException`).
