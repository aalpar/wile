# Channel lifecycle and VM cancellation

How Scheme-visible Go channels (`make-channel`, `channel-send!`,
`channel-receive`, …) couple to the VM's context-cancellation machinery, so a
blocking channel operation observes the VM deadline / `thread-terminate!` instead
of parking a goroutine forever.

**Status:** as-built, merged to master 2026-07-16 (`3441c8ed`, from branch
`fix/channel-lifecycle-ctx`). It replaced a `sync.RWMutex` + `close(ch)` design
under which the TOCTOU host-panic and the ctx goroutine-leak described below were
live. This is the design record for a coupling that was retrofitted onto a
5-month-old channel subsystem; see *History* for why it is younger than
everything it touches, and *Open decisions* for the parts still deliberately
unsettled.

Sections marked as gaps (*Boundaries*, *Test coverage*) describe work that is
**not** implemented.

**Code:** `pkg/values/channel.go` (the `Channel` type, `SendOutcome` /
`RecvOutcome`), `extensions/gointerop/prim_gointerop.go` (the primitive layer's
Option A policy), `pkg/machine/call_foreign_cached.go` (`callForeignCached`, the
eager timer recheck this design leans on), `pkg/machine/run_body_under_timer.go`
(`RunBodyUnderTimer`, the `with-timeout` ctx), `pkg/values/thread.go`
(`Thread.Terminate`, the SRFI-18 ctx).

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

## The policy: Option A — cancelled is surfaced as closed

`PrimChannelSend` / `PrimChannelReceive` collapse the outcome back to the
historical two-state surface:

- `channel-send!`: any outcome that is not `SendSent` raises `ErrChannelClosed`
  (so `SendCancelled` is reported *as* a closed channel).
- `channel-receive`: only `RecvReceived` with a non-nil value returns that value;
  closed, cancelled, or a nil value all return `Void`.

**Rationale.** A ctx-cancelled operation is, in the common case, a thread being
unwound; surfacing the cancellation as the ordinary closed-channel contract keeps
the Scheme surface two-state and avoids a new condition type. The seam means
**Option B** — a distinct catchable cancellation condition — is a one-line change
per primitive whenever a consumer needs `guard` to see it. No lifecycle rewrite.

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
| `thread-terminate!` | `context.Canceled` (plain `cancel()`) | **Bounded latency.** The eager recheck is `ErrTimerExpired`-only, so it does *not* fire here. Safety instead comes from the pre-existing property that a terminated thread runs at most `contextCheckMask` (≈1024) more ops before the VM loop's top-of-loop `ctx.Done()` check unwinds it to `TerminatedThreadException`. The laundered value is discarded on that unwind. Channels add no new hazard beyond terminate's already-accepted latency. |
| Embedder deadline | `context.DeadlineExceeded`, `mc.timer == nil` | **Neither protection; bounded and strictly better than before.** No eager recheck (not a timer) and no thread teardown. The body runs up to ≈1024 ops with a laundered `Void` before `DeadlineExceeded` propagates. This is the one path where "a cancelled receive looks exactly like a closed channel" is observable — but it is bounded by the same VM-wide cancellation latency every primitive has, and it replaces the old **infinite hang** with a bounded return. |

The single mechanism that makes the first row work — the eager `ErrTimerExpired`
recheck in `callForeignCached` — is the invariant the Option A comment must name.
Remove or narrow it and the `with-timeout` composition regresses to the
embedder-deadline behavior (bounded laundering), and no channel or timer test
would catch the change (see *Test coverage*).

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
- **`ChannelSelect` is not wired to `done` or ctx.** `values.ChannelSelect`
  builds `reflect.SelectCase` on the never-closed `ch` directly, so a peer closing
  a channel mid-block is invisible to a blocked select. It is also registered
  nowhere (no `channel-select` primitive). Lifting the limitation means adding
  each channel's `done` arm to the `reflect.Select` set. See *Open decisions*.

## History — why the coupling is younger than the subsystem

The channel-cancellation link is the youngest part of the channel subsystem by
~5 months, and was not co-designed with channel exposure.

| Design | Introduced | Source |
|---|---|---|
| Go channel infrastructure exposed to Scheme | ~Feb 2026 (PR #224) | git only |
| `SelectCaseKind` enum refinement | 2026-03-04 (PR #415) | git only |
| `channel-select` multiplex surface | 2026-06-08 (draft, unshipped) | `memory/2026-06-08-channel-select-design.local.md` |
| **Channel ops ↔ VM cancellation** | **2026-07-15 (`3441c8ed`)** | `plans/2026-07-15-review-2026-07-13-sec4-remediation.md` §T1.2/T1.3 |

For the subsystem's first ~5 months, blocking channel ops ignored `ctx`
entirely. The 2026-06-08 `channel-select` design **explicitly declined**
cancellation ("No existing channel primitive threads a context for cancellation,
so v1 does not either — it follows the established pattern"). The coupling was
then introduced as a bug-fix for the leak that design had chosen to live with,
derived from the 2026-07-13 review §4 (items T1.2 host-panic, T1.3 ctx-leak).

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

1. **Is Option A a committed contract, or a placeholder?** The `SendOutcome` doc
   treats "distinct condition" as a live future option. If Option A is committed,
   the seam's forward-looking language should be deleted and the outcome types
   could collapse toward a bool. If it is a placeholder, the seam should be
   *used* — surface `RecvCancelled` / `SendCancelled` distinctly so a cancelled
   op cannot be confused with close (Option B). Today the seam is built,
   documented as load-bearing, and wired to nothing.

2. **`channel-select`: delete or wire?** `ChannelSelect` is complete, tested, and
   CHANGELOG-cited, but registered nowhere and drifted from the done-channel
   model (it can block forever on a peer close). Either delete it, or wire it as
   `channel-select` *with* the per-channel `done` arms added — half-migrated dead
   code is the worst of the three.

3. **Embedder-deadline observability.** Is it acceptable that a timed-out
   `channel-receive` under an embedder `WithTimeout` returns `Void` (looks
   closed) for up to ≈1024 ops before `DeadlineExceeded` surfaces? It is
   VM-consistent and strictly better than the prior infinite hang, but it is the
   one observable manifestation of the laundering and deserves an explicit
   decision rather than an accident of the eager recheck being timer-only.

## Test coverage (and its gap)

`pkg/values/channel_lifecycle_test.go` proves the Go-level contract: a blocking
`Send`/`Receive` returns on a raw `context.WithCancel`, and a parked op is woken
by a concurrent `Close`. `TestChannel_ConcurrentSendClose_NoPanic` (20000 trials,
ungated, `-race`) is the permanent TOCTOU guard.

**Gap:** nothing exercises the coupling *through the Scheme integration* that
makes it safe. No test drives `thread-terminate!` or `with-timeout` into a parked
channel op. In particular, the `with-timeout ∘ channel-receive` path is safe only
because of the eager `ErrTimerExpired` recheck in `callForeignCached`; no channel
or timer test would fail if that recheck regressed. Two tests close the gap:

- a Scheme `(with-timeout T handler (lambda () (channel-receive empty-ch)))`
  asserting the *handler* value, not `Void` — the regression guard for the eager
  recheck;
- a cross-thread rendezvous: park a thread in `channel-receive`,
  `thread-terminate!` it, assert the goroutine exits and the joiner sees
  `TerminatedThreadException`.
