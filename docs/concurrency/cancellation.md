# Channel lifecycle and VM cancellation

How Scheme-visible Go channels (`make-channel`, `channel-send!`,
`channel-receive`, …) couple to the VM's context-cancellation machinery, so a
blocking channel operation observes the VM deadline / `thread-terminate!` instead
of parking a goroutine forever.

**Status:** as-built, merged to master 2026-07-16 (`3441c8ed`, from branch
`fix/channel-lifecycle-ctx`). It replaced a `sync.RWMutex` + `close(ch)` design
under which the TOCTOU host-panic and the ctx goroutine-leak described below were
live. Writing this document surfaced a further defect it had missed — a
terminated thread could report a cancelled channel op's laundered value as its
result — fixed 2026-07-16 in `pkg/values/thread.go`; see *The tail-position
hole*. This is the design record for a coupling that was retrofitted onto a
5-month-old channel subsystem; see *History* for why it is younger than
everything it touches, and *Open decisions* for the parts still deliberately
unsettled.

The section marked as a gap (*Boundaries*) describes work that is **not**
implemented.

**Code:** `pkg/values/channel.go` (the `Channel` type, `SendOutcome` /
`RecvOutcome`), `extensions/gointerop/prim_gointerop.go` (the primitive layer's
Option A policy), `pkg/machine/call_foreign_cached.go` (`callForeignCached`, the
eager timer recheck this design leans on), `pkg/machine/run_body_under_timer.go`
(`RunBodyUnderTimer`, the `with-timeout` ctx), `pkg/values/thread.go`
(`Thread.Terminate`, the SRFI-18 ctx; `Thread.setOutcome`, the write-once rule
that keeps a laundered value from becoming a terminated thread's result).

---

## What problem this solves

A blocking Go channel operation parks a goroutine. Before this design, the
Scheme primitives called `ch.Send(v)` / `ch.Receive()` on a bare Go channel with
no escape: a `channel-receive` on an empty channel, or a `channel-send!` on a
full one with no peer, blocked its goroutine **forever**, ignoring both the VM's
`with-timeout` deadline and `thread-terminate!`. Channels were the one blocking
primitive family that ignored `ctx`; `thread-sleep!` had always honored it.

Two concrete failures followed, both Scheme-reachable whenever `gointerop` is
loaded:

- **Goroutine leak.** `(thread-terminate! t)` on a thread parked in
  `channel-receive` cancelled the thread's ctx, marked it terminated, and left
  the goroutine parked. Nothing in the repo detected it.
- **Host panic (TOCTOU).** The old `Close` did `close(ch)` while a concurrent
  `Send` was mid-flight, producing a `send on closed channel` fatal panic that
  the VM's recover boundary cannot catch. Reachable from two SRFI-18 threads,
  one in `(channel-send! ch v)`, one in `(channel-close! ch)`.

This design fixes both with one structural change plus one wiring.

## The structural change: never close the data channel

The invariant that makes concurrent close panic-free:

> **The underlying data channel `ch` is never closed.** Closure is signalled by
> closing a separate `done chan struct{}` exactly once, guarded by `closeOnce`,
> with an `atomic.Bool closed` set *before* `done` closes.

Because `ch` is never closed, `close(ch)`-during-`send` is structurally
impossible, so the TOCTOU panic is *unreachable*, not merely recovered (a
`recover`-only fix could not pass `-race`; this design does). Every blocking
operation becomes a three-arm select:

```
select {
case ch <- v / v := <-ch:   // the data operation
case <-done:                 // a concurrent Close woke us
case <-ctx.Done():           // the VM cancelled us  ← the coupling
}
```

The `<-ctx.Done()` arm is the coupling. `Send`, `Receive`, `TrySend` (no ctx
arm; non-blocking), and `TryReceive` all share this shape. Closed-wins-over-
buffer ordering is enforced by a leading `closed.Load()` check on the send path;
a receive on a closed channel drains buffered stragglers on the `done` arm before
reporting closed, matching Go's own drain-then-zero close semantics.

Status (`closed`) is a lock-free atomic; there is no mutex. This type carries no
transactional guarantee across operations — concurrent senders/receivers observe
standard Go channel semantics.

## The outcome seam: cause is preserved at the Go boundary

`Send` returns a `SendOutcome`, `Receive` a `RecvOutcome`, rather than a bool.
The cancellation cause is a distinct value, not folded into "closed":

| `SendOutcome` | meaning | `RecvOutcome` | meaning |
|---|---|---|---|
| `SendSent` | delivered | `RecvReceived` | value produced |
| `SendClosed` | channel closed | `RecvClosed` | closed and drained |
| `SendWouldBlock` | `TrySend` only | `RecvWouldBlock` | `TryReceive` only |
| `SendCancelled` | ctx cancelled mid-op | `RecvCancelled` | ctx cancelled mid-op |

This is the **seam**: the channel layer *keeps* the distinction between "closed"
and "cancelled" so the primitive layer can decide how Scheme sees it, without any
change to the lifecycle below. Whether the seam is *used* is a separate decision
(see Option A).

## The policy: Option A for send, Option B for receive (updated 2026-07-17)

`PrimChannelSend` keeps the historical two-state surface; `PrimChannelReceive`
surfaces cancellation distinctly:

- `channel-send!` (Option A): any outcome that is not `SendSent` raises
  `ErrChannelClosed` (so `SendCancelled` is reported *as* a closed channel).
- `channel-receive` (Option B): `RecvReceived` returns the value; `RecvClosed`
  (and a nil value) return `Void`; `RecvCancelled` raises `ErrChannelCancelled`,
  a distinct catchable condition — EXCEPT when the ctx cause is `ErrTimerExpired`
  (with-timeout), which keeps the `Void` surface. That exception is load-bearing:
  `callForeignCached`'s eager timer recheck fires only on the error-free return
  path (its `if err != nil` return precedes the recheck), so returning an error
  on the `ErrTimerExpired` source would bypass the recheck and defeat the timeout
  handler.

**Rationale.** Send keeps Option A because a cancelled send is at least an error
already. Receive took Option B because its laundered outcome was a silent `Void`,
an ordinary value indistinguishable from a drained channel that could flow into
program logic unnoticed. This was NOT the "one-line change" an earlier draft
claimed: the `callForeignCached` return-before-recheck ordering forces receive to
keep `Void` on the `ErrTimerExpired` source. Send remains free to adopt Option B
the same way whenever a consumer needs `guard` to see `SendCancelled`.

**This rationale is load-bearing and incomplete on its own.** "The thread is
unwound anyway" is true for `thread-terminate!` but not for the other two
cancellation sources. Correctness in those cases rests on *non-local* VM
invariants, documented next. A future editor must not weaken those invariants
believing the channel layer is self-protecting.

## Who cancels the ctx: three sources, three safety arguments

The ctx a channel op selects on is `mc.Context()`. Three distinct events cancel
it, and the "cancelled ≡ closed" laundering is safe for *different reasons* in
each case. This is the crux of the coupling.

| Source | Cause on ctx | Why Option A is safe |
|---|---|---|
| `with-timeout` | `ErrTimerExpired` (`WithTimeoutCause`) | **Eager recheck.** `callForeignCached` does a non-blocking `ctx.Done()` check *after every foreign return* and, on `ErrTimerExpired`, returns `ErrTimerInterrupt` before the laundered value is consumed. The timeout handler runs; the bogus `Void`/closed-error is discarded. |
| `thread-terminate!` | `context.Canceled` (plain `cancel()`) | **The outcome is claimed, not raced.** The eager recheck is `ErrTimerExpired`-only, so it does *not* fire here. Safety comes instead from `Terminate` storing the SRFI-18 terminated-thread exception *before* it cancels, plus the outcome being write-once (`Thread.setOutcome`, `pkg/values/thread.go`): the laundered value cannot become the thread's result no matter which path ends the goroutine. Side effects the thread performs in the ≤ `contextCheckMask` (≈1024) ops before the VM loop's top-of-loop `ctx.Done()` check unwinds it remain observable — terminate's already-accepted latency, unchanged by channels. |
| Embedder deadline | `context.DeadlineExceeded`, `mc.timer == nil` | **Neither protection; bounded and strictly better than before.** No eager recheck (not a timer) and no thread teardown. The body runs up to ≈1024 ops with a laundered `Void` before `DeadlineExceeded` propagates. This is the one path where "a cancelled receive looks exactly like a closed channel" is observable — but it is bounded by the same VM-wide cancellation latency every primitive has, and it replaces the old **infinite hang** with a bounded return. |

The single mechanism that makes the first row work — the eager `ErrTimerExpired`
recheck in `callForeignCached` — is the invariant the Option A comment must name.
Remove or narrow it and the `with-timeout` composition regresses to the
embedder-deadline behavior (bounded laundering);
`TestWithTimeoutInterruptsParkedReceive` is the one test that fails when it does.

### The tail-position hole (found and fixed 2026-07-16)

The `thread-terminate!` row above originally claimed the ≈1024-op unwind window
was itself the protection: the terminated thread would reach the top-of-loop ctx
check and the laundered value would be "discarded on that unwind". It is not,
when the parked `channel-receive` sits in **tail position of the thread thunk** —
nothing follows it, so no op ever triggers the check, and the laundered `Void`
returns as the thunk's ordinary result. `Thread.Start`'s goroutine then
overwrote the exception `Terminate` had stored, and `(thread-join! t)` reported a
terminated thread as having *succeeded* with `Void` — indistinguishable from a
receive on a closed channel, and a fourth laundering escape in the row the doc
called safe.

The defect was in `thread.go`, not in the channel layer; the channel fix only
made it reachable (before it, that thread hung forever). SRFI-18 gives
`thread-terminate!` an outcome, not merely an effect: a terminated-thread
exception stored in the end-exception field, which `thread-join!` raises. The
outcome is now write-once, so the first writer (`Terminate`) wins over the
goroutine's completion path, and *no* laundered value can be reported as a
terminated thread's result. Guarded channel-free by
`TestThreadTerminateStoresEndException`; the two thunk shapes are pinned as
indistinguishable by `TestTerminateUnparksBlockedThread`.

### Empirical confirmation

`(with-timeout T handler (lambda () (channel-receive empty-ch)))`, run against a
built binary: the handler runs 60/60, and a side effect placed immediately after
the timed-out receive executes 0/40. The VM services the timer interrupt before
the laundered `Void` is consumed. The hazard the seam guards against does not
manifest for `with-timeout` today; it is prevented by `callForeignCached`, not by
the primitive layer.

## Boundaries and non-goals

- **No transactional guarantee.** A send racing a concurrent close may still land
  in the buffer and be drained by a receiver — a legitimate send-before-close
  ordering, never a panic. `closed.Load()` gives closed-wins ordering only at op
  entry, not across the whole operation.
- **`WaitGroup` / `RWMutex` still ignore ctx.** This design covers channels only.
  `wait-group-wait!` and `rw-mutex-*-lock!` park on bare Go operations and remain
  a separate follow-up; `thread-terminate!` on a thread blocked in one still
  leaks its goroutine.
- **There is no multiplexed select.** `values.ChannelSelect` was removed
  2026-07-16 (see *History*): it built its `reflect.SelectCase` set on the
  never-closed `ch`, so a peer close mid-block was invisible to a blocked select,
  and it took no ctx where `Send`/`Receive` both do. No `channel-select` primitive
  ever existed, and nothing consumed the Go symbol. Reintroducing one is a bounded
  job, not a redesign: `reflect.Select` takes a case set built at runtime, so each
  channel contributes a `done` arm and the whole set one ctx arm (2N+1), the `done`
  arm draining stragglers before reporting closed exactly as `Receive` does. It
  needs an arity guard — `reflect.Select` panics past 65536 cases, and a Scheme
  caller supplies the list.

## History — why the coupling is younger than the subsystem

The channel-cancellation link is the youngest part of the channel subsystem by
~5 months, and was not co-designed with channel exposure.

| Design | Introduced | Source |
|---|---|---|
| Go channel infrastructure exposed to Scheme | ~Feb 2026 (PR #224) | git only |
| `SelectCaseKind` enum refinement | 2026-03-04 (PR #415) | git only (removed 2026-07-16) |
| `channel-select` multiplex surface | 2026-06-08 (draft, unshipped) | untracked local design note, quoted below |
| **Channel ops ↔ VM cancellation** | **2026-07-15 (`3441c8ed`)** | untracked review-remediation plan, §T1.2/T1.3 |
| `values.ChannelSelect` removed | 2026-07-16 | git only |

For the subsystem's first ~5 months, blocking channel ops ignored `ctx`
entirely. The 2026-06-08 `channel-select` design **explicitly declined**
cancellation ("No existing channel primitive threads a context for cancellation,
so v1 does not either — it follows the established pattern"). The coupling was
then introduced as a bug-fix for the leak that design had chosen to live with,
derived from the 2026-07-13 review §4 (items T1.2 host-panic, T1.3 ctx-leak).

The rows above are history, not inventory: `ChannelSelect` and its `SelectCase`
types no longer exist. Their fate is the clearest illustration of the section's
point. The select code was written under the pre-cancellation lifecycle and was
never revisited when that lifecycle changed beneath it, because nothing called
it — the compiler cannot fail an unused function that has quietly become wrong.
The 2026-06-08 draft declined cancellation to "follow the established pattern";
by the time the pattern moved, the code following it was already unreachable.

The cancellation *mechanism* is older and was borrowed, not invented here: the VM
ctx-check cadence (`contextCheckMask`), `SetContext`, timer interrupts
(`with-timeout`, PR #659, 2026-04-16), and the SRFI-18 thread ctx
(`thread.go` `WithCancel` + `thread-terminate!`) all predate 2026-07-15. This
design *wired the channel primitives into that pre-existing VM ctx* — making
channels conform to the `thread-sleep!` pattern the rest of the VM already
followed. That the coupling was retrofitted, rather than co-designed, is why the
outcome seam reads as bolted-on: it is new, and Option A discards it at the
primitive boundary as a deliberate minimal-change decision.

## Open decisions

These are genuinely unsettled; the code commits to a default but the design does
not close them.

1. ~~**Is Option A a committed contract, or a placeholder?**~~ **Partially
   resolved 2026-07-17: receive uses the seam (Option B), send stays Option A.**
   `channel-receive` now surfaces `RecvCancelled` as `ErrChannelCancelled` on
   every source except `ErrTimerExpired`, so the seam is used, not wired to
   nothing. `channel-send!` still launders `SendCancelled` to `ErrChannelClosed`;
   adopting Option B there is the same change whenever a consumer needs to
   distinguish a cancelled send from a closed channel.

2. ~~**`channel-select`: delete or wire?**~~ **Resolved 2026-07-16: deleted.**
   Nothing consumed it, so its cost was carrying a function written against a
   lifecycle that no longer existed. The decision was never technical — a
   prototype confirmed `done` and ctx arms work — it was that `channel-select`
   had no consumer to justify the surface. Reinstating it is a fresh, bounded
   piece of work whenever one appears; see *Boundaries*.

3. ~~**Embedder-deadline observability.**~~ **Resolved 2026-07-17 via Option B for
   receive.** A `channel-receive` cancelled by an embedder deadline no longer
   returns `Void`; because the cause is not `ErrTimerExpired`, it raises
   `ErrChannelCancelled`, so a cancelled receive is catchable and cannot be
   confused with a drained channel. The residual ≈1024-op laundering window
   remains only on the send side and on the deliberately-`Void` `ErrTimerExpired`
   receive path. The original open question follows, for the record:

   Is it acceptable that a timed-out
   `channel-receive` under an embedder `WithTimeout` returns `Void` (looks
   closed) for up to ≈1024 ops before `DeadlineExceeded` surfaces? It is
   VM-consistent and strictly better than the prior infinite hang, but it is the
   one observable manifestation of the laundering and deserves an explicit
   decision rather than an accident of the eager recheck being timer-only.

## Test coverage

`pkg/values/channel_lifecycle_test.go` proves the Go-level contract: a blocking
`Send`/`Receive` returns on a raw `context.WithCancel`, and a parked op is woken
by a concurrent `Close`. `TestChannel_ConcurrentSendClose_NoPanic` (20000 trials,
ungated, `-race`) is the permanent TOCTOU guard.

The Go layer cannot reach the two integrations that make Option A safe, because
both live above `pkg/values`. `extensions/gointerop/channel_cancellation_test.go`
drives them from Scheme:

- `TestWithTimeoutInterruptsParkedReceive` — asserts the *handler's* value, not
  `Void`. The regression guard for the eager `ErrTimerExpired` recheck; with that
  recheck disabled it fails with the laundered `Void` reaching the program.
- `TestTerminateUnparksBlockedThread` — parks a thread in `channel-receive`,
  terminates it, and joins. `thread-join!` *is* the goroutine-exit handshake
  (`Thread.Join` blocks on the `done` channel closed by `Start`'s
  first-registered, therefore last-run, defer), so a leaked goroutine surfaces as
  `JoinTimeoutException`. Both thunk shapes (receive in tail position, and ops
  following) assert the same terminated-thread exception — the mechanism that
  ends the thread must not be visible from Scheme.

The SRFI-18 contract the second test leans on is pinned separately, and
channel-free, in `extensions/threads/prim_threads_terminate_outcome_test.go`
(`TestThreadTerminateStoresEndException` and its already-completed-thread
mirror). Asserting merely that *some* exception is raised does not guard it: the
overwritten outcome was the thunk's own `context.Canceled`, which raises too, so
only the exception's identity separates correct from broken.
