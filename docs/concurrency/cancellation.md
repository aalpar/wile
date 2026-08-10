# Blocking synchronization primitives and VM cancellation

How Scheme-visible blocking synchronization (SRFI-18 `mutex-lock!` and the
condition-variable wait `(mutex-unlock! m cv)`) couples to the VM's
context-cancellation machinery, so a thread parked *acquiring* one observes
`thread-terminate!` / a VM deadline instead of parking a goroutine forever.

**Status:** as-built. The SRFI-18 `values.Mutex` acquires through a cond-based ctx
bridge; a thread blocked on it is unparked by `thread-terminate!`. The
condition-variable wait, `(mutex-unlock! m cv)`, is cancellable too, by its own
channel-based route rather than the cond bridge. Three families that previously
shared this concern have since left the Scheme surface: Go channels and Go-style
wait-groups in 1.19.1, and the `rw-mutex-*` family in the removal that produced
this revision (`git log` for the history).

**Code:** `pkg/values/cond_wait.go` (`waitOnCondCtx`, the one ctx-to-cond bridge),
`pkg/values/mutex.go` (`Mutex.LockContext`, the SRFI-18 mutex;
`Mutex.UnlockContext`, the atomic unlock-and-wait),
`pkg/values/condition_variable.go` (`registerWaiter`/`blockOnWaiter`, the cv wait's
own ctx arm), `pkg/machine/call_foreign_cached.go` (`callForeignCached`, the eager
timer recheck this design leans on), `pkg/values/thread.go` (`Thread.Terminate`,
the SRFI-18 ctx; `Thread.setOutcome`, the write-once rule).

---

## What problem this solves

`sync.Mutex.Lock` and `sync.WaitGroup.Wait` have no ctx-aware form: a goroutine
parked in one cannot be interrupted. When the Scheme synchronization primitives
wrapped those Go types directly, `(thread-terminate! t)` on a thread blocked
acquiring cancelled the thread's ctx, marked it terminated, and left the goroutine
parked forever. `thread-join!` then raised `JoinTimeoutException` — the observable
form of a leaked goroutine — and the VM's teardown stalled. Locks were a blocking
primitive family that ignored `ctx`; `thread-sleep!` had always honored it.

The naive fix — run the blocking call in a helper goroutine and `select` on
`ctx.Done()` — is wrong for a lock. If ctx wins the race the helper is still
parked in `Lock()` and will *eventually acquire*, but the Scheme caller already
returned; nobody unlocks it, so the lock is held by a phantom forever. That
converts a goroutine leak into a permanent deadlock. Acquisition has to be
**atomic with cancellation**: either win the state transition or return without
acquiring, never "cancelled but the lock silently landed anyway."

## The structural change: a Wile-owned state machine

`values.Mutex` is an explicit state machine guarded by a `sync.Mutex` +
`sync.Cond`, not a `sync` wrapper. Acquisition transitions the state *under the
guard mutex*, so it is atomic with cancellation — no phantom hold. Blocking waits
go through one helper:

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

`Mutex.LockContext`'s untimed slow path loops on `waitOnCondCtx` until the mutex is
grantable, then flips the state; on cancellation it returns `false` with the state
untouched.

## The primitive-layer policy: wait side vs held side

A thread blocked **acquiring** wakes on ctx and returns *without acquiring*, so
nothing is half-held. A thread that already **holds** a lock is untouched: a
terminated holder's lock stays held. Force-unlocking it would expose the guarded
resource mid-transition, out of serialization order — the exact race the lock
exists to prevent. A stuck lock is the safe outcome. SRFI-18's own
owner-driven `MarkAbandoned` is a different, spec-mandated path, not an
override of this.

How a cancelled wait surfaces to Scheme is decided by one question: does the
primitive have a free value channel?

- `mutex-lock!` already signals "did not acquire" as `#f` (its timeout form does),
  so a cancelled acquire returns `#f`. Returning it *error-free* is load-bearing —
  see the `with-timeout` row below.
- `(mutex-unlock! m cv)` takes the same shape: it already reports "the wait ended
  without a signal" as `#f`, so a cancelled cv wait returns `#f`. `blockOnWaiter`
  deregisters the waiter on the ctx arm, except when `Signal`/`Broadcast` claimed
  it at the boundary, which still reports signaled.
- `thread-sleep!` and `thread-join!` have no such channel — one returns Void, the
  other returns the joinee's result. A cancelled wait is reported as
  `werr.ErrOperationCancelled` carrying the raw ctx cause, so an embedder gets a
  sentinel to match rather than a bare `context.Canceled`. The price is that these
  two must discriminate on the cancellation *source* (`waitCancelled` in
  `extensions/threads/prim_threads.go`): under `ErrTimerExpired` they return
  error-free, because the eager recheck below runs only on that path and is the
  only thing that dispatches the `with-timeout` handler. That special case is
  exactly what a free value channel buys the other two out of.

  For `thread-join!` this is a *fourth* outcome alongside the three SRFI-18
  conditions. The other three describe how the JOINEE ended; this one says the
  joiner stopped waiting, and SRFI-18 has no condition for it, so it reaches a
  `guard` as an ordinary error object.

## Who cancels the ctx: three sources, three safety arguments

The ctx a blocking op selects on is `mc.Context()`. Three distinct events cancel
it, and correctness holds for a *different reason* in each case.

| Source | Cause on ctx | Why the handling is safe |
|---|---|---|
| `with-timeout` | `ErrTimerExpired` (`WithTimeoutCause`) | **Eager recheck.** `callForeignCached` does a non-blocking `ctx.Done()` check *after every foreign return under an active timer* (`mc.timer != nil`) and, on `ErrTimerExpired`, returns `ErrTimerInterrupt` — but only on the *error-free* return path (`if err != nil` returns first). A cancelled `mutex-lock!` returns `#f` error-free, so the recheck fires and the timeout handler runs, discarding the `#f` before anything observes it. |
| `thread-terminate!` | `context.Canceled` (plain `cancel()`) | **The outcome is claimed, not raced.** The eager recheck is `ErrTimerExpired`-only, so it does not fire here; the acquire simply returns `#f`. Safety comes from `Terminate` holding the thread's own lock across both `cancel()` and the store of the SRFI-18 terminated-thread exception, so a goroutine unparked by that cancellation cannot slip its own outcome in between, plus the outcome being write-once (`Thread.setOutcome`): no returned value can become the thread's result, whichever path ends the goroutine. |
| Embedder deadline | `context.DeadlineExceeded`, `mc.timer == nil` | **Bounded, not infinite.** No eager recheck (not a timer) and no thread teardown, so the acquire returns `#f` and the program continues; the VM's own top-of-loop ctx check then surfaces `DeadlineExceeded` within one poll period (1024 ops; `contextCheckMask` is the 1023 mask). That is the same VM-wide cancellation latency every primitive has, and it replaces the old **infinite hang**. |

The mechanism that makes the first row work — the eager `ErrTimerExpired` recheck
in `callForeignCached` — is non-local: it lives in the VM, while the decision to
return `#f` *error-free* lives in the primitive. Narrow either half and the
`with-timeout` composition regresses, with the parked op's result escaping instead
of the handler running. `TestWithTimeoutInterruptsParkedMutexLock` is the test that
fails when it does, and it is the only test that spans both halves.

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
  does not release it (see the policy section). It is *possible* — Wile owns the
  state machine, so a terminated holder *could* release — but it remains an
  unproposed follow-up, deliberately not built, because releasing mid-transition
  is the race we are avoiding.
- **The timed `mutex-lock!` path wakes within its timeout, not immediately.** Only
  the *untimed* `Mutex.LockContext` slow path was made ctx-aware; a thread in
  `(mutex-lock! m T)` under termination wakes within `T` (bounded, never an
  infinite stall), which was judged sufficient.
- **No transactional guarantee across operations.** These are locks, not
  transactions; standard mutual-exclusion semantics apply.

## Test coverage

`extensions/gointerop/sync_cancellation_test.go` drives the integrations from
Scheme (they live above `pkg/values` and cannot be reached from a Go-level test):

- `TestTerminateUnparksBlockedSyncPrimitive` — parks a thread in `mutex-lock!`
  behind a lock the main thread holds, terminates it, and joins. The rendezvous is
  a lock-free `atomic` flag polled with `thread-yield!`. A leaked goroutine
  surfaces as `JoinTimeoutException`; reaching the terminated-thread exception
  proves the goroutine exited on ctx cancellation.
- `TestWithTimeoutInterruptsParkedMutexLock` — a `mutex-lock!` parked inside a
  `with-timeout` runs the handler (returns its value). This is the composition
  proof for the eager-recheck / error-free-`#f` pairing above; no Go-level test of
  either half covers the join.
- `TestTerminateUnparksCVWait` — the cv-path analogue of the first: a thread parked
  in an untimed `(mutex-unlock! m cv)` is reaped rather than leaking its goroutine.
  Its sibling `TestUnlockCVDeliversSignalAcrossThreads` pins the other half of that
  path, that enqueueing the waiter before releasing the mutex closes the SRFI-18
  lost-wakeup window.

`extensions/threads/prim_threads_test.go` covers the two primitives that do not
share the `#f` convention:

- `TestThreadSleepContextCancellation` — an embedder-cancelled sleep reports
  `werr.ErrOperationCancelled`. Asserted against the sentinel with `errors.Is`,
  never against `!= nil`: a test named for cancellation that cannot see *which*
  cancellation is a blind guard.
- `TestWithTimeoutInterruptsParkedThreadSleep` and
  `TestWithTimeoutInterruptsParkedThreadJoin` — the composition proofs for the
  source discrimination. The sleep one runs both shapes: unguarded, the handler
  runs; guarded, the handler still runs and the guard clause is never entered,
  because nothing is raised.
- `TestTerminateUnparksUntimedThreadJoin` — terminating the thread that is
  **parked in** the join unparks it. Its `joinee` arm is a labelled control that
  cannot fail: terminating the thread being **joined** always worked, and is the
  natural misreading of the same sentence.

All four run under a watchdog, because the failure mode is a hang and a hanging
test does not report.

The eager recheck's own guards are Go-level:
`pkg/machine/call_foreign_cached_test.go`, `timer_interrupt_test.go`,
`machine_context_test.go`, `operations_call_test.go`. The SRFI-18 write-once
outcome the terminate test leans on is pinned separately, and lock-free, in
`extensions/threads/prim_threads_terminate_outcome_test.go`
(`TestThreadTerminateStoresEndException`).
