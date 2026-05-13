# Finding 7 (MachineContext field clustering) — Outcomes

`plans/2026-05-06-machine-structural-reduction.md` Finding 7
proposed three sub-record clusters on `MachineContext` to enforce
representation invariants and reduce struct size. Executed staged,
bench-gated. Closed 2026-05-12.

## Summary

| Stage | Cluster | PR | Outcome | Struct delta | Bench (geomean) |
|-------|---------|-----|---------|-------------:|-----------------|
| 1 | ExpansionState (`expanderCtx`, `syntaxCase`) | #742 | Shipped | −24 B | +0.47% within ±0.5% gate (0.03% headroom) |
| 2 | TimerState (`timerHandler`, `timerCancel`) | #743 | Shipped | −16 B | −0.316% (branch faster) within ±0.5% gate |
| 3 | SubContextState (`parentMC`, `escapeCont`, `barrierValid`, `isolatedMarks`) | — | Declined | — | not run |

**Total**: −40 bytes per `MachineContext`.

## Stage 1 — ExpansionState

**Invariant claim, post-implementation**: relaxed in plan. The
parent plan said `(expanderCtx == nil) ⇒ (syntaxCase == nil)`.
Copilot review of PR #742 caught that this isn't actually enforced
— syntax-case unit tests set `SyntaxCaseState` without an expander
context, and the clustered sub-record allows both slots to be
written independently. Plan revised to claim only the weaker (and
true) invariant: both slots share an allocation lifecycle.

**Refactor was still net-positive**: −24 B per context, no
hot-path interaction (4 accessor callers in `machine/compilation/`
cold path), and reading is now `mc.expansion != nil && mc.expansion.field != nil`
vs the old single nil-check — but only on the cold path.

## Stage 2 — TimerState

**Invariant**: `(timerHandler == nil) ⇔ (timerCancel == nil)` is
**load-bearing**. One production writer (`prim_timer.go`) sets both,
one production clearer (`machine_context.go`) clears both after
calling cancel. The new `SetTimer(h, c)` + `ClearTimer()` API makes
this structural.

**Hot-path access pattern preserved**: `Run():306`, `applyForeign:153`,
`callForeignCached:91` each do exactly one nil-check on the timer
pointer — same op count as the old `mc.timerHandler != nil`. Handler
deref inside the if-body is a single field access of an already-checked
pointer.

**Bench result**: −0.316% geomean (branch *faster* than master),
11/16 benches faster — consistent with the 16-byte struct shrink
improving cache locality on the dispatch loop. Sign distribution
+5/−11. Per-bench outliers all favorable (`ctak` −2.89% biggest
mover, still net-faster).

## Stage 3 — SubContextState — declined

Inspected before plan-writing. **No co-variance invariant exists**:

| Field | Writers | Lifecycle owner |
|-------|---------|-----------------|
| `parentMC` | `NewSubContext():51` (single site) | Sub-context creation |
| `escapeCont` | `NewSubContext():52` (inherited) + `SetEscapeCont` in `registry/core/prim_control.go:258` | Mixed: inherited or set by call/cc setup |
| `barrierValid` | `NewSubContext():57` (inherited) + `SetBarrierValid` in `registry/core/prim_barrier.go:60` | Mixed: inherited or set by barrier setup |
| `isolatedMarks` | `captured_continuation.go:106` (single site, sets to true) | Captured-continuation application |

These fields don't go nil/non-nil together — `escapeCont` and
`barrierValid` can each be set on a top-level context (parentMC == nil)
by their dedicated Scheme primitives. The parent plan's listed
invariant `(parentMC == nil) ⇒ ¬is-sub-context` is trivially true
(parentMC IS the discriminator) but doesn't constrain the other
three.

**Cost of clustering anyway**: every read becomes
`mc.subCtx != nil && mc.subCtx.field`. `isolatedMarks` today reads
as a single bool op (`if mc.isolatedMarks`) — clustering adds a
pointer-deref-plus-nil-check to a hot-ish path (called from
`findParameterInMarks` walk). Same penalty hits `barrierValid`
reads in `applyComposableContinuation:419` (every composable
continuation invocation).

**Decision rationale**: a cluster whose value is only `−24 B
struct shrink` without an invariant payoff is just a struct rename
with read overhead added. Stage 1 had no real invariant but
clustered cold-path fields (low cost). Stage 2 had a real
invariant. Stage 3 has neither — pure thematic grouping. Declining
preserves a clean rule: **cluster when fields co-vary**.

## Methodology lessons

- **Inspect field writers before writing the plan.** Stage 1's
  plan inherited a parent-plan invariant claim that didn't survive
  Copilot review. Stage 2's plan was rewritten after grepping
  writers and confirming both fields are set/cleared in one
  place each. Stage 3 was inspected before drafting and the
  decline came from that inspection.
- **Bench gate per stage worked.** Both shipped stages cleared the
  gate. Per-bench ±0.3% threshold turned out to be advisory below
  the per-block noise floor (~1–5%); geomean is the load-bearing
  number.
- **−40 bytes total** translates to a slightly-faster VM context
  (Stage 2 measured −0.316% geomean speedup). Net win.

## Future-attempt guidance

If a future structural-reduction pass revisits Stage 3:

1. Don't cluster all four. Either decline again, or split.
2. The most defensible split would be `parentMC + escapeCont`
   (both inherited at sub-context creation), leaving `barrierValid`
   and `isolatedMarks` as inline fields. But `SetEscapeCont` from
   `prim_control.go:258` would still mutate one slot of the
   sub-record on a non-sub-context — weakens the invariant.
3. A larger redesign (e.g., **separate types for sub-context VM
   vs top-level VM**) is the genuinely-correct fix to the underlying
   product-type observation in the parent plan's Finding 7 body.
   That's a research project, not a sub-record cluster.
