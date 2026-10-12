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
wait-groups in 1.19.0, and the `rw-mutex-*` family in the removal that produced
this revision (`git log` for the history).

**Code:** `pkg/values/cond_wait.go` (`waitOnCondCtx`, the one ctx-to-cond bridge),
`pkg/values/mutex.go` (`Mutex.LockContext`, the SRFI-18 mutex;
`Mutex.UnlockContext`, the atomic unlock-and-wait),
`pkg/values/condition_variable.go` (`registerWaiter`/`blockOnWaiter`, the cv wait's
own ctx arm), `pkg/machine/call_foreign_cached.go` (`callForeignCached`, the eager
timer recheck this design leans on; `applyForeign` in
`pkg/machine/machine_context_apply.go` repeats it for unfused foreign calls),
`pkg/values/thread.go` (`Thread.Terminate`,
the SRFI-18 ctx; `Thread.setOutcome`, the write-once rule),
`extensions/threads/prim_threads.go` (`waitCancelled`, the one rule for how a
cancelled wait surfaces).

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
nothing is half-held. The cancellation machinery never force-unlocks a lock that
is already **held**: handing it to the next waiter as if it were free would expose
the guarded resource mid-transition, out of serialization order, which is the exact
race the lock exists to prevent. What a terminated holder's lock does instead depends on
ownership. `Thread.Terminate` runs SRFI-18 abandonment (`AbandonOwnedMutexes`, then
`MarkAbandoned` per mutex) on every mutex the thread *owns*: the next `mutex-lock!`
acquires it and raises `abandoned-mutex-exception`, so the acquirer is told the
invariant may be broken. A mutex the thread locked with owner `#f` is not tracked
and stays held.

How a cancelled wait surfaces to Scheme is one rule for all four blocking waits
(`mutex-lock!`, `(mutex-unlock! m cv)`, `thread-sleep!` and `thread-join!`), and
the wait does not ask what cancelled it: it raises `werr.ErrOperationCancelled`
carrying the raw ctx cause (`waitCancelled`, `extensions/threads/prim_threads.go`),
so an embedder matches a sentinel and can still tell `context.Canceled` from
`context.DeadlineExceeded`. It is a committed contract.

What cancelled the wait decides what happens to that failure next, and the VM,
which owns the timers, decides it (`supersedingTimerInterrupt`,
`pkg/machine/timer_interrupt.go`):

- **A `with-timeout` timer fired, and a context on the waiting chain owns it.**
  That context turns the failure into the timer's `ErrTimerInterrupt` before any
  exception handler on the chain sees it, and its driver runs the handler. Nothing
  after the wait executes, and no `guard` or `with-exception-handler` between the
  wait and the timer's owner, or outside it, sees the failure. (A context that is
  not linked to the chain sees it first: a macro transformer at expansion time, a
  library body being instantiated, and the roots listed below. The owner still
  turns it into the timeout when it comes back.) In the timer's own context this happens at the foreign-call
  bridge. A sub-context below the owner (`eval`, `load`, a winding thunk) has no
  timer of its own: it passes the failure up undispatched,
  and the owner converts it when it comes back through whatever started the
  sub-context, whether a foreign call's return, a host function's return, or an
  escape's winding reconcile. The owner does not need to recognise the failure,
  so a host function that replaces a callback's error with its own changes
  nothing, and any other failure that reaches the owner after its timer fired is
  the timeout too: compute or macro expansion inside `eval` stopped by the timer
  runs the handler. Not converted: a control transfer already in flight, and a
  security denial, which must reach the embedder however the program is written.
- **Anything else**: an embedder cancel or deadline, `thread-terminate!`, or the
  end of the `with-timeout` thunk a thread was started in, whether the thunk
  returned or its timer fired. A started thread's root has no synchronous parent,
  so with no `with-timeout` of its own no context on its chain owns a timer. The
  failure is an ordinary condition: a `guard` around the wait can catch it, and a
  program that catches it and finishes returns normally even though its context is
  cancelled (see *Boundaries*). So is a procedure a host function runs through
  `Engine.Call`, whose root is not linked to the caller; its failure becomes the
  timeout when the host function returns it to the caller's context.

  For `thread-join!` this is a *fourth* outcome alongside the three SRFI-18
  conditions. The other three describe how the JOINEE ended; this one says the
  joiner stopped waiting, and SRFI-18 has no condition for it, so it reaches a
  `guard` as an ordinary error object. A joinee that died of its own cancelled
  wait is a joinee outcome, an uncaught exception whose reason is that wait's
  error, not this one. (A joinee stopped mid-computation by the VM's poll is not
  yet reported the same way; see *Boundaries*.)

A `with-timeout`'s extent is a frame on the winding stack, above its boundary
frame. A normal return pops it; any other way out (an escape, an abort, a guard's
exit, the end of the run) unwinds through it, which cancels the timer and restores
the context outside it (`exitTimer`). Re-entering the extent arms nothing: a timer
once left is over.

A `dynamic-wind`'s thunks run in the dynamic environment of the `dynamic-wind` call
(R7RS §6.10), and the winding order makes the timer part of it by position: a
reconcile runs the thunks of frames above the timer's frame before it exits the
timer, and those below it after. So an escape that leaves a `with-timeout` through
an enclosing `dynamic-wind` runs the after thunk outside the timer, and a full
continuation captured outside a `with-timeout` and invoked inside it leaves the
extent before it re-runs the frame's before thunk. A frame spliced in above the
timer's frame (by a composable continuation, or a continuation reinstated at a
prompt inside the `with-timeout`) runs its thunks under the timer, and so does one
entered inside it, including inside `eval` or `load` below its owner. One
exception is filed in `TODO.md`: an escape from a `with-timeout` inside `eval` that
the top driver reconciles runs the thunks above it on the top context's ctx.

The expiry names its timer (`timerExpiry`, still `errors.Is` `ErrTimerExpired`),
so the `with-timeout` whose timer fired answers it, on the context that arms that
timer: an outer timer's expiry runs the outer handler, past an armed inner one,
and a starting thread's expiry reaches a started thread, which does not arm that
timer, as an ordinary cancellation, even inside the started thread's own
`with-timeout`.

The lock waits used to answer `#f` for every source, because their timed forms
already spend `#f` on "did not acquire", and under a timer all four returned
error-free and relied on the eager recheck below, which runs only in the timer's
own context. That made a cancelled wait indistinguishable from a failed timed
acquire; when the wait was a program's last form the embedder got `#f` and a nil
error after its own cancellation; and a thread started inside a timed thunk, or a
wait inside `eval`, carried on past the wait into the critical section when the
timer fired. `thread-sleep!` and `thread-join!` already raised for every source
but the timer.

`Mutex.LockContext` and `Mutex.UnlockContext` still report a cancelled wait as a
plain `false`; the primitive reads the ctx and decides. A timed acquire that times
out while its ctx is also cancelled reports the cancellation. A cv signal claimed
at the cancellation boundary still answers `#t`: `blockOnWaiter` deregisters the
waiter on the ctx arm unless `Signal`/`Broadcast` already claimed it.

## Who cancels the ctx: three sources, three safety arguments

The ctx a blocking op selects on is `mc.Context()`. Three distinct events cancel
it, and correctness holds for a *different reason* in each case.

| Source | Cause on ctx | Why the handling is safe |
|---|---|---|
| `with-timeout` | `timerExpiry`, naming the timer (`WithTimeoutCause`; `errors.Is` `ErrTimerExpired`) | **Eager recheck.** `callForeignCached` does a non-blocking `ctx.Done()` check *after every foreign return under an active timer* (`mc.timer != nil`) and, when the expiry names a timer this context arms, returns that timer's `ErrTimerInterrupt` — but only on the *error-free* return path (`if err != nil` returns first). That recheck serves a primitive that returns normally after the timer fired. A failure takes the other path: the owner's bridge and driver turn it into the interrupt (`supersedingTimerInterrupt`), which also covers a failure returning from a sub-context, where the recheck cannot look. |
| `thread-terminate!` | `context.Canceled` (plain `cancel()`) | **The outcome is claimed, not raced.** The eager recheck is `ErrTimerExpired`-only, so it does not fire here; the parked wait raises `ErrOperationCancelled` in the terminated thread. Safety comes from `Terminate` holding the thread's own lock across both `cancel()` and the store of the SRFI-18 terminated-thread exception, so a goroutine unparked by that cancellation cannot slip its own outcome in between, plus the outcome being write-once (`Thread.setOutcome`): neither that raise nor any returned value can become the thread's result, whichever path ends the goroutine. |
| Embedder deadline or cancel | `context.DeadlineExceeded` or `context.Canceled`, `mc.timer == nil` | **Raised at the wait.** No timer interrupt and no thread teardown; the parked wait raises `ErrOperationCancelled` carrying the cause, so an unguarded wait reaches the embedder as an error even when it is the program's last form. Code that does not park meets the VM's own top-of-loop ctx check instead, at the next poll (every 1024 ops; `contextCheckMask` is the 1023 mask), when the program runs that long. Either replaces the old **infinite hang**. Three shapes still finish normally: a `guard` that catches the raise and finishes before the next poll, a timed `mutex-lock!` whose mutex frees within its timeout, and work that ends before the next poll, including an uncontended `mutex-lock!`, which acquires without waiting (see *Boundaries*). |

The mechanism that makes the first row work for a parked wait is split: the wait
raises, and the VM's owner-side rule turns the raise into the timeout.
`TestWithTimeoutInterruptsParkedMutexLock`, `TestWithTimeoutInterruptsParkedThreadSleep`
and `TestWithTimeoutInterruptsParkedThreadJoin` prove the composition end to end,
but not which path did the work: in the timer's own context the eager recheck after
a guard's own foreign calls reaches the handler too. The paths are pinned
separately, each by a test that fails when that path is removed:
`TestTimerCancelledWaitIsNeverCaught` (the bridge in the timer's own context, and
the pass-up below it through one or two sub-contexts),
`TestAfterThunkOutsideAFiredTimerKeepsItsGuard` (a fired timer claims only a
failure a timer's cancellation caused: the cause check in `expiredTimer`, read by
`firedTimer` and `timerAbove`),
`TestWindingThunkFailureOutsideAFiredTimerIsItsOwn` (nor a winding thunk's failure
outside it), `TestUnclaimedFailureIsClaimedByAnotherOwner` (a failure reaching
another context whose timer fired is the timeout there),
`TestTimerClaimsAnApplySpreadFailureBelowIt`
(`apply`'s own spread failures go through the bridge),
`TestTimerCancelledWaitInRootlessCallCCRunsHandler` (the resume
reconcile route), `TestTimerCancelledWaitInAbortAfterThunkRunsHandler` (the abort
route, in `RunResumable` and in a sub-context's `RunWithinBoundary`),
`TestTimerCancelledWaitInHostCallbackRunsHandler` (an uncaught-condition escape is
claimed), `TestTimerSupersedesFailureButNotDenial` (the denial exclusion),
`TestTimerExpiryNamesTheTimerThatFired` (the timer the expiry names answers it:
`firedTimer`, at the poll, the rechecks and the claim), `TestEscapeFromATimerEndsIt`
(an escape ends the timer: `exitTimer`), `TestReentryInsideATimerStaysUnderIt` (a
re-entry inside it composes onto the timer's frame), `TestThreadExitLeavesTheStartersExtents`
and `TestEscapeOutOfASubContextTimerKeepsTheOwners` (a thread's exit and an escape
out of `eval`'s timer leave the owner's armed), `TestEscapedTimerDoesNotHideALiveOne`
and `TestEscapedTimerLeavesTheHandlersContinuation` (an inner `with-timeout` left by
an escape leaves the outer timer to claim a wait, for a returned failure or one
`FailureClaimedByTimer` hands back),
`TestTimerDoesNotClaimAControlTransfer`
(an escape or abort out of `eval` after the timer fired still arrives), and
`TestTimerClaimsAHandlerFailure` (a handler the drivers apply themselves). A wait that returned a value instead of raising is caught
wherever no recheck can reach: the sub-context, tail-position `call/cc`, host-callback,
never-caught and child-thread tests below.

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

- **Unowned locks are not released.** A mutex a terminated thread locked with
  owner `#f` stays locked; only owned mutexes go through abandonment (see the
  policy section).
- **The timed `mutex-lock!` path wakes within its timeout, not immediately.** Only
  the *untimed* `Mutex.LockContext` slow path was made ctx-aware; a thread in
  `(mutex-lock! m T)` under any cancellation wakes within `T` (bounded, never an
  infinite stall), which was judged sufficient. If the lock frees up within `T`
  the acquire succeeds: under an embedder cancel it answers `#t` with no error.
  Under a fired `with-timeout`, in the timer's own context, the handler runs
  instead (the eager recheck); below it, in `eval` or `load`, the acquire answers
  `#t` and the code after it runs until that sub-context returns or reaches its
  next poll or wait. Either way the mutex the abandoned thunk acquired stays held,
  like any lock held when a timer interrupts.
- **A cancellation can still end in a normal return.** `ErrOperationCancelled`
  is an ordinary condition, so a `guard` around a wait can catch it, and a program
  that catches it and then finishes before the VM's next poll returns normally to
  an embedder whose context is cancelled. The same holds for work that never
  parks and ends before the next poll, such as an uncontended `mutex-lock!`, which
  takes its fast path without reading the ctx. An embedder that must know checks
  `ctx.Err()` after `Eval`. Closing this generally would mean an end-of-run ctx
  check in the VM, which would also report a computation that genuinely finished
  at the boundary as failed; it is not built.
- **A joinee stopped at the VM's poll is reported differently from one stopped
  in a wait.** A thread cancelled while computing dies at the poll with the raw
  ctx error, which reaches its joiner as an ordinary error object, the same shape
  as the joiner's own cancellation; one cancelled while parked reaches it as an
  uncaught exception. Unifying them means mapping every joinee error to an uncaught
  exception, and that carrier does not yet expose its reason to `errors.Is`, so the
  host would lose the cause. Filed in TODO.md.
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
  `with-timeout` runs the handler (returns its value). This is the end-to-end
  composition proof that a timer-cancelled acquire runs the handler and raises no
  ordinary error; it cannot tell which VM path did it, and no Go-level test of
  either half covers the join.
- `TestTerminateUnparksCVWait` — the cv-path analogue of the first: a thread parked
  in an untimed `(mutex-unlock! m cv)` is reaped rather than leaking its goroutine.
  Its sibling `TestUnlockCVDeliversSignalAcrossThreads` pins the other half of that
  path, that enqueueing the waiter before releasing the mutex closes the SRFI-18
  lost-wakeup window.

`extensions/threads/prim_threads_test.go` covers the rest of the rule:

- `TestThreadSleepContextCancellation` — an embedder-cancelled sleep reports
  `werr.ErrOperationCancelled`. Asserted against the sentinel with `errors.Is`,
  never against `!= nil`: a test named for cancellation that cannot see *which*
  cancellation is a blind guard.
- `TestWithTimeoutInterruptsParkedThreadSleep` and
  `TestWithTimeoutInterruptsParkedThreadJoin`: the end-to-end composition proofs
  for the other two waits. The sleep one runs both shapes: unguarded, the handler
  runs; guarded, the handler still runs and the guard clause is never entered,
  because the failure becomes the interrupt before any guard sees it.
- `TestCancelledLockWaitRaisesOperationCancelled`: both lock waits, parked as the
  program's last form, raise `werr.ErrOperationCancelled` under an embedder cancel
  and under a cancel whose cause is `DeadlineExceeded`, with the cause reachable by
  `errors.Is`. This is the row that answered `#f` with a nil error before. Three
  more rows: a timed acquire, which only `PrimMutexLock`'s own check turns into a
  raise; a timed cv wait; and an acquire inside an armed but unexpired
  `with-timeout`, which must raise rather than run the handler (only an
  `ErrTimerExpired` cause belongs to a timer). `TestEmbedderDeadlineInsideArmedTimerRaises`
  does the last with a real embedder deadline, whose `Err()` matches an expired
  timer's. `TestCaughtCancellationCleanupUnlockReturns` pins the other side: a
  plain `mutex-unlock!` and an uncontended `mutex-lock!` under a cancelled ctx are
  not waits and answer `#t`; it relies on its few cleanup ops finishing before the
  VM's next poll.
- `TestTimerCancelledWaitInSubContextRunsHandler`: each of the four waits inside
  `eval` under a `with-timeout` runs the handler, and the eval'd code after the
  wait never runs. The timer is on the sub-context's parent, out of the eager
  recheck's reach.
- `TestTimerCancelledWaitIsNeverCaught`: a `with-exception-handler` around the wait
  in the timer's own context, a `guard` around it inside `eval`, and a `guard`
  outside the `with-timeout` altogether never see the failure; the handler runs.
- `TestTimerCancelledWaitInRootlessCallCCRunsHandler`: a wait in a tail-position
  `call/cc` inside an after thunk that an escape runs, the longest route back to
  the timer's owner (the thunk's sub-context, then the escape's winding reconcile).
- `TestTimerCancelledWaitInHostCallbackRunsHandler`: a wait in a `RegisterFunc`
  callback, and in a procedure a host function runs through `Engine.Call`, with a
  host that replaces the failure by an error wrapping nothing.
- `TestTimerExpiryDuringEvalComputeRunsHandler`: compute inside `eval` stopped by
  the timer runs the handler; the sub-context's poll error is converted by the
  owner like a wait's failure.
- `TestTimerSupersedesFailureButNotDenial`: a primitive that outlives the timer and
  then fails becomes the timeout, unless its failure is a security denial.
- `TestInnerTimerExpiryRunsTheInnerHandler`: with two timers on one context, and with
  timers owned by two contexts of one chain, the inner one's handler runs when its
  timer fires. `TestTimerExpiryNamesTheTimerThatFired`: the outer one's runs when
  the outer timer fires, and a starting thread's expiry does not run a started
  thread's own handler.
- `TestEscapeFromATimerEndsIt`: a guard, `call/cc` or abort leaving a
  `with-timeout` ends its timer, so later work in the same evaluation runs to
  completion. `TestReentryInsideATimerStaysUnderIt`: a composable continuation, or
  a `call/cc` one reinstated at a prompt inside the `with-timeout`, keeps the timer.
  `TestThreadExitLeavesTheStartersExtents`, `TestEscapeOutOfASubContextTimerKeepsTheOwners`:
  a thread exiting, or an escape out of a `with-timeout` inside `eval`, leaves the
  enclosing timer armed.
- `TestTimerCancelledWaitInAbortAfterThunkRunsHandler`: a guard's abort escape runs
  a dynamic-wind after thunk that waits; the handler runs with the `with-timeout` at
  top level and inside `eval`.
- `TestOrdinaryErrorUnderArmedTimerIsCaught`, `TestEmbedderDeadlineInsideArmedTimerRaises`:
  what the rule does not claim.
- `TestCancelledWaitInChildThreadIsCatchableThere`: a started thread's cancelled
  wait is an ordinary condition in that thread.
- `TestTimerClaimsAHandlerFailure`: a handler the drivers apply themselves (a
  `call-with-continuation-prompt` handler an abort runs, an inner `with-timeout`'s
  handler) that fails after a timer fired; the drivers dispatch the resulting
  interrupt instead of returning it.
- `TestTimerDoesNotClaimAControlTransfer`: an escape and an abort out of `eval`
  after the timer fired arrive at their continuation and prompt.
- `TestEscapePastATimerLeavesItsExtent`, `TestReentryUnderATimerRewindsOutsideIt`,
  `TestAfterThunkOutsideAFiredTimerKeepsItsGuard`,
  `TestWindingThunkFailureOutsideAFiredTimerIsItsOwn`: a `dynamic-wind` that encloses
  a `with-timeout` runs its thunks outside the timer, past one timer or two, on an
  escape and on a re-entry, and a failure there is the thunk's own.
  `TestUnclaimedFailureIsClaimedByAnotherOwner`: that failure is still the timeout
  for a joiner, or a host function's caller, whose timer fired.
  `TestAfterThunkEnteredBelowTheTimerStaysInsideIt`: one entered inside `eval` or
  `load` below the timer's owner stays inside it, one inside a `with-timeout` of
  `eval`'s own steps out of that one only, and one under the top timer but outside
  a later inner one stays inside the outer one.
  `TestReentryIntoAnEndedTimerKeepsTheEnclosingOne`: one entered inside a
  `with-timeout` that has ended re-enters under the live one outside it.
- `TestEscapedTimerDoesNotHideALiveOne`, `TestEscapedTimerLeavesTheHandlersContinuation`:
  after an inner `with-timeout` is left by an escape, the outer timer claims the
  wait, runs its handler once, and hands it the continuation it gets without the
  inner one.
- `TestTimerClaimsAnApplySpreadFailureBelowIt`: `apply`'s not-a-list and
  improper-list failures inside `eval` after the timer fired pass up like a
  primitive's.
- `TestTimerClaimsAnSRFI18ConditionBelowIt`: `thread-join!`'s joinee condition and
  `mutex-lock!`'s abandoned-mutex condition, which those primitives raise in place,
  are handed back to the bridge when the timer claims them
  (`MachineContext.FailureClaimedByTimer`), so a guard inside `eval` does not see them.
- `TestTimerCancelledWaitInChildThreadRaises`: each of the four waits in a thread
  started inside a timed thunk raises when the timer fires; the join reports an
  uncaught exception whose reason names the wait, and the main thread's lock stays
  held. The thread signals just before its wait, and the thunk waits for that
  signal before sleeping into the timer, so the cancellation reaches the wait and
  not the thread's first VM poll, which would report differently (see
  *Boundaries*).
- `TestJoinReportsJoineeCancellationAsUncaught`: a joinee that died of its own
  cancelled wait is reported as an uncaught exception, not as the joiner's
  cancellation. Same handshake.
- `TestTerminateUnparksUntimedThreadJoin` — terminating the thread that is
  **parked in** the join unparks it. Its `joinee` arm is a labelled control that
  cannot fail: terminating the thread being **joined** always worked, and is the
  natural misreading of the same sentence.

All of them run under a watchdog, because the failure mode is a hang and a hanging
test does not report.

The eager recheck's own guards are Go-level:
`pkg/machine/call_foreign_cached_test.go`, `timer_interrupt_test.go`,
`machine_context_test.go`. The SRFI-18 write-once
outcome the terminate test leans on is pinned separately, and lock-free, in
`extensions/threads/prim_threads_terminate_outcome_test.go`
(`TestThreadTerminateStoresEndException`).
