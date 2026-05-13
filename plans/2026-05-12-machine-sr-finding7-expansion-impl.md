# Machine Finding 7 — Expansion-cluster sub-record (Stage 1 of 3)

Implementation plan for the first cluster of **Finding 7** of
`plans/2026-05-06-machine-structural-reduction.md`. Per the parent
plan's recommended phasing ("research project, not a one-PR change")
and user direction this session, Finding 7 is executed **staged: one
cluster per PR, bench-gated**.

## Decision

Cluster the `expanderCtx` + `syntaxCase` fields on `MachineContext`
into a single `*expansionState` sub-record. Both fields are nil
during VM execution (the hot path); both are set during macro
expansion (cold path). The invariant
`(expanderCtx == nil) ⇒ (syntaxCase == nil)` becomes structural: the
sub-record is allocated lazily on first write, freed on pool reset.

**Why Expansion first** (of the three Finding 7 clusters):

- **Coldest hot-path interaction.** All four callers of the accessors
  (`ExpanderContext()` × 2 in `operation_syntax_rules_transform.go`;
  `SyntaxCaseState()` × 3 in `operation_syntax_case.go`) live in
  `machine/compilation/` — the compile-time path. Zero callers in
  the bytecode `Run()` loop. Adding a nil-check on accessor reads
  is invisible to bench numbers.

- **Struct size win.** Pre-refactor `MachineContext` carries:
  - `expanderCtx ExpanderCtx` — interface = 16 bytes
  - `syntaxCase any` — interface = 16 bytes
  Post-refactor: one `expansion *expansionState` = 8 bytes.
  **Net: -24 bytes per `MachineContext`.** Improves cache locality
  on the hot bytecode loop.

- **Bench precedent.** Two recent VM hot-path refactors failed
  bench-gating (PR-pre-history Phase 2: −1.5%; PR #737 Finding 2:
  −2.5%). Both touched the bytecode dispatch path. This change does
  not touch dispatch — it touches accessor methods called from
  cold paths.

## Bench gate

Per parent plan: **geomean ±0.5%, per-bench ±0.3%** over 6+ pinned
interleaved runs against `master`. If either threshold fails, revert
and document as "Considered and declined" — Finding 7 closes at
"first cluster attempted, declined."

## Scope

| File | Change |
|------|--------|
| `machine/machine_context.go` | Define `expansionState` struct. Replace two fields (`expanderCtx`, `syntaxCase`) with one `expansion *expansionState`. Rewrite 4 accessors (`ExpanderContext`, `SetExpanderContext`, `SyntaxCaseState`, `SetSyntaxCaseState`) to lazy-allocate the sub-record on first write and return nil for reads when unset. |
| `machine/pool_test.go` | 3 direct field reads (`mc.expanderCtx`) migrate to the public `ExpanderContext()` accessor. |

Net: ~5 LOC added (struct + lazy-alloc), ~2 LOC removed (field declarations + comment), behavior preserved.

## Phases

1. **Plan commit.** This file.
2. **Define `expansionState` + replace fields.** Add the sub-record struct, lazy-allocating accessors, delete the old fields. Build verifies.
3. **Migrate `pool_test.go` direct field reads.** 3 sites.
4. **Verify.** `make lint && make covercheck && make ci`.
5. **Bench gate.** Interleaved 6-run head-to-head vs `master`. Per
   `memory/finding5-bench-methodology.md`: pinned CPU at max freq
   (not taskpolicy — see `memory/feedback-no-taskpolicy-for-benches.md`),
   verify `--version` SHA matches commit before benching, separate
   binary paths.

## Risk

- **Lazy-allocation cost on first SetExpanderContext call.** One
  `&expansionState{}` per macro-transformer invocation. macro
  expansion already allocates extensively (syntax objects, scopes,
  expansion contexts), so one more 24-byte struct allocation is
  negligible.

- **Pool zeroing.** `subContextPool`'s release function does
  `*mc = MachineContext{}`. After the refactor, the `expansion`
  pointer field becomes nil — releasing the sub-record for GC.
  Equivalent to current behavior (both fields are zeroed today).

## Closes

This PR closes **one of three clusters** in Finding 7. If bench-gate
passes:
- Next PR: TimerState cluster (`timerHandler` + `timerCancel`)
- Next PR after that: SubContextState cluster
  (`parentMC` + `escapeCont` + `barrierValid` + `isolatedMarks`)

If bench-gate fails:
- Revert this PR.
- Document Finding 7 as "Considered and declined — first cluster
  failed bench gate."
- Close the machine plan at 5 shipped + 3 declined / 7.

## Commit cadence

1. `docs(plans): impl plan for machine SR finding 7 expansion cluster (stage 1)`
2. `refactor(machine): cluster expanderCtx + syntaxCase into expansionState sub-record`

Single implementation commit — the changes are tightly coupled (field
deletion happens exactly when accessors stop reading the fields).
