# Machine Finding 7 — Timer-cluster sub-record (Stage 2 of 3)

Implementation plan for the second cluster of **Finding 7** of
`plans/2026-05-06-machine-structural-reduction.md`. Stage 1
(Expansion cluster) shipped in PR #742. This is Stage 2.

## Decision

Cluster the `timerHandler` + `timerCancel` fields on `MachineContext`
into a single `*timerState` sub-record. Both fields are nil in the
common case (no timer installed); both are set together as a unit
when `(with-timeout …)` installs an interrupt handler.

Unlike Stage 1 — where the parent plan's
`(expanderCtx == nil) ⇒ (syntaxCase == nil)` claim turned out *not*
to be enforced by code — the Timer cluster's invariant
`(handler == nil) ⇔ (cancel == nil)` **is** load-bearing. The
existing code already maintains it:

- Both fields are written together at `registry/core/prim_timer.go:90-91`
  (the only production writer).
- Both fields are cleared together at `machine_context.go:1373-1374`
  (the only production clearer, after calling `cancel()` first).

Clustering makes the load-bearing invariant structural — there is
no API to set one without the other.

## API shape

The current API exposes two independent setters
(`SetTimerHandler`, `SetTimerCancel`) and one composite reader path.
The new API has one composite setter, one composite clearer, and one
reader:

```go
type timerState struct {
    handler values.Callable
    cancel  context.CancelFunc
}

// SetTimer installs the timer interrupt handler and cancel function
// as an atomic unit. Both must be non-nil.
func (p *MachineContext) SetTimer(h values.Callable, cancel context.CancelFunc)

// ClearTimer cancels the active timer (calling cancel) and removes
// the handler. Safe when no timer is active.
func (p *MachineContext) ClearTimer()

// TimerHandler returns the active timer handler, or nil if none.
func (p *MachineContext) TimerHandler() values.Callable
```

`SetTimerHandler` and `SetTimerCancel` are deleted — neither has
external callers post-refactor.

## Hot-path access pattern

Three hot-path sites read `mc.timerHandler != nil`:

| Site | Frequency | Current | Post-refactor |
|------|-----------|---------|---------------|
| `Run():306` (timer-expiry branch inside ctx.Done() inside `OpsExecuted&mask == 0` sample) | Once per ~1024 ops, only when ctx done | `mc.timerHandler != nil` (1 nil-check) | `mc.timer != nil` (1 nil-check) |
| `applyForeign:153` (post-call latency check) | Every foreign-call return | `p.timerHandler != nil` (1 nil-check) | `p.timer != nil` (1 nil-check) |
| `callForeignCached:91` (cached/peephole post-call check) | Every cached foreign-call return | `mc.timerHandler != nil` (1 nil-check) | `mc.timer != nil` (1 nil-check) |

Op count is preserved at all three sites. Inside each `if` body,
`Handler: mc.timerHandler` becomes `Handler: mc.timer.handler` —
a single deref of the already-checked-non-nil pointer, no extra
runtime check.

This is a strictly less risky bench-gate change than Stage 1: Stage
1 added nil-checks in cold compile-time paths; Stage 2 changes hot
reads from one nil-check to a different one nil-check.

## Struct size

- `timerHandler values.Callable` — interface = 16 bytes
- `timerCancel context.CancelFunc` — func value = 8 bytes
- Total pre-refactor: **24 bytes**

Post-refactor: one `timer *timerState` = **8 bytes**.

**Net: −16 bytes per `MachineContext`.** Combined with Stage 1's
−24 bytes, total Finding-7 struct-size reduction so far: −40 bytes
on every VM context.

## Bench gate

Per parent plan and per-stage convention: **geomean ±0.5%, per-bench
±0.3%** over 6+ pinned interleaved runs against `master`. Same
methodology as Stage 1: pinned at max frequency (NOT taskpolicy),
caffeinate -di for thermal stability, SHA-verified binaries, 3
blocks × 3 runs per binary, pooled.

If either threshold fails, revert and document. The hot-path access
analysis above predicts ≤0.1% noise; Stage 1 cleared the gate with
0.03% headroom on a strictly harder change.

## Scope

| File | Change |
|------|--------|
| `machine/machine_context.go` | Define `timerState` struct. Replace two fields (`timerHandler`, `timerCancel`) with one `timer *timerState`. Replace `SetTimerHandler`/`SetTimerCancel`/`TimerHandler`/`TimerCancel` with `SetTimer`/`ClearTimer`/`TimerHandler`. Update `Run():306-307` to read `mc.timer` instead of `mc.timerHandler`. Update the clear sequence at `1370-1374` to call `ClearTimer()`. |
| `machine/machine_context_apply.go:153-157` | Read `p.timer` instead of `p.timerHandler`; access handler via `p.timer.handler`. |
| `machine/call_foreign_cached.go:91-95` | Same: read `mc.timer`; access handler via `mc.timer.handler`. |
| `registry/core/prim_timer.go:90-91` | Replace two-line setter with `sub.SetTimer(handlerVal, timerCancel)`. |

Tests touch only comments (`machine_context_test.go:2379, 2406`),
no direct field reads. Pool reset zeroes `*mc = MachineContext{}`
which sets `timer = nil` — equivalent to current behavior.

Net: ~15 LOC added (struct + 3 accessors + composite update),
~10 LOC removed (4 old accessors), behavior preserved.

## Phases

1. **Plan commit.** This file.
2. **Define `timerState` + new accessors.** Add struct,
   `SetTimer`, `ClearTimer`, rewrite `TimerHandler`. Delete the
   four old accessors. Update internal reads (Run loop + clear
   sequence).
3. **Migrate `applyForeign` and `callForeignCached` reads.**
   Two sites.
4. **Migrate `prim_timer.go` writer.** One site.
5. **Verify.** `make lint && make covercheck && make ci`.
6. **Bench gate.** Interleaved 6-run head-to-head vs `master`
   per the Stage 1 methodology.

## Risk

- **Pool reset**. `*mc = MachineContext{}` sets the `timer` pointer
  to nil — releasing the sub-record for GC. Equivalent to the
  current behavior (which also nils both timer fields on reset).
- **Cancel-before-clear discipline.** The current
  `machine_context.go:1370-1374` sequence calls `p.timerCancel()`
  before nilling. `ClearTimer()` encapsulates that ordering — the
  refactor *removes* a footgun rather than adding one.

## Closes

This PR closes **the second of three clusters** in Finding 7.

If bench-gate passes:
- Next PR: SubContextState cluster (`parentMC` + `escapeCont`
  + `barrierValid` + `isolatedMarks`).

If bench-gate fails:
- Revert this PR.
- Re-evaluate whether the SubContextState cluster is worth
  attempting (its hot-path interaction is heavier still — those
  four fields are read on sub-context creation, call/cc, and
  exception handling).

## Commit cadence

1. `docs(plans): impl plan for machine SR finding 7 timer cluster (stage 2)`
2. `refactor(machine): cluster timerHandler + timerCancel into timerState sub-record`

Single implementation commit — the changes are tightly coupled
(field deletion happens exactly when accessors stop reading the
old fields, exactly when the writer site adopts the new
composite setter).
