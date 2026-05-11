# Finding 3 — Consolidate value-register accessors on `vmState`

Implementation plan for Finding 3 of
`plans/2026-05-06-machine-structural-reduction.md`.

Parent finding: **State Tightness — Split value register: documented invariant,
unenforced by types** (`plans/2026-05-06-machine-structural-reduction.md:194-243`).

## Decision

Per the parent plan: **leave the split, tighten encapsulation.**

The `singleValue` / `multiValues` pair on `vmState` is a deliberate
precision-for-throughput trade-off (parent plan lines 223-229) — the split
saved ~20% of allocations on call-heavy benchmarks and a true sum type using
`values.Value`-as-interface-case would re-introduce that cost. The remediation
is **not** to change the representation, but to enforce the documented
"at most one field is active at a time" invariant by **co-locating all reads
and writes on `vmState`**.

After this change, the grep canary at parent plan line 238
(`grep -n 'singleValue\|multiValues' machine/*.go | grep -v vm_state.go`)
returns zero non-test hits.

Per user input on this session: the enforcement mechanism is a **ruleguard
rule** (matching the `noFmtErrorf` / `noBareSentinelPanic` shape in
`ruleguard/rules.go`). The doc-comment + grep approach was considered; the
ruleguard rule was chosen because the project already loads ruleguard via
gocritic at lint time and the existing rule-test harness
(`ruleguard/rules_test.go`) makes regression-testing trivial.

## Scope

| Item                                | Count |
|-------------------------------------|-------|
| Helper methods moved to `vmState`   | 5 (SetValue, SetValues, GetValue, GetValues, PushValues) |
| New helper methods on `vmState`     | 3 (pushValueRegisterTo, copyValueRegisterFrom, cloneValueRegisterFrom) |
| Production call-sites rewritten     | 6 |
| Test files updated                  | 2 |
| Ruleguard rules added               | 1 (+ fixture + golden test entries) |
| LOC delta                           | net ≈ 0 (move + a few helpers) |

Pure refactor: zero behavior change, zero allocation change on the hot path.

## Phases

### Phase 1 — Move accessors to `vmState`, add new helpers

**Move from `machine_context.go` to `vm_state.go`** (receiver
`*MachineContext` → `*vmState`). Both `MachineContext` and
`MachineContinuation` embed `vmState`, so method promotion keeps all existing
call sites valid:

- `SetValues(vs ...values.Value)`     (`machine_context.go:199-207`)
- `SetValue(v values.Value)`          (`machine_context.go:212-215`)
- `GetValue() values.Value`           (`machine_context.go:219-230`)
- `GetValues() MultipleValues`        (`machine_context.go:235-243`)

**Move from `machine_continuation.go` to `vm_state.go`** (receiver
`*MachineContinuation` → `*vmState`):

- `PushValues(v ...values.Value)`     (`machine_continuation.go:140-146`)

**Add to `vm_state.go`** (new helpers, all unexported — internal-only):

```go
// pushValueRegisterTo pushes the live half of the value register onto s.
// Used by OpPush. Preserves the single-value fast path: no MultipleValues
// wrap, no allocation when only singleValue is live.
func (p *vmState) pushValueRegisterTo(s *Stack) {
    if p.multiValues != nil {
        s.PushAll(p.multiValues)
        return
    }
    if p.singleValue != nil {
        s.Push(p.singleValue)
    }
}

// copyValueRegisterFrom copies both halves of the value register from src.
// Shallow: multiValues is shared (slice header copy). Used by
// SaveContinuation, PopContinuation, and NewMachineContext initialization
// — all sites where the source and destination represent the same logical
// register state across a save/restore boundary, never re-invoked.
func (p *vmState) copyValueRegisterFrom(src *vmState) {
    p.singleValue = src.singleValue
    p.multiValues = src.multiValues
}

// cloneValueRegisterFrom copies the value register from src with the
// multiValues slice deep-copied via slices.Clone. Used by
// MachineContinuation.Copy() to support re-invocable continuations (call/cc):
// each invocation must see an independent slice so PushValues append doesn't
// corrupt the original.
func (p *vmState) cloneValueRegisterFrom(src *vmState) {
    p.singleValue = src.singleValue
    p.multiValues = slices.Clone(src.multiValues)
}
```

Why three helpers, not one with a `deep bool` flag: the call sites have
different semantics, not different parameters. Save/restore moves the same
register through space; Copy creates an independent register. A boolean flag
would obscure that distinction at call sites and invite mistakes.

### Phase 2 — Rewrite production call-sites

| File:Line | Current | After |
|-----------|---------|-------|
| `machine_context.go:120-121` | `singleValue: cont.singleValue, multiValues: cont.multiValues,` (inside `vmState{}` literal) | Remove those two field initializers from the literal; after the struct is constructed, call `q.copyValueRegisterFrom(&cont.vmState)`. |
| `machine_context.go:371-374` | `if mc.multiValues != nil { mc.evals.PushAll(mc.multiValues) } else if mc.singleValue != nil { mc.evals.Push(mc.singleValue) }` | `mc.pushValueRegisterTo(mc.evals)` |
| `machine_context_continuation.go:171-172` (inside `PopContinuation`) | `p.singleValue = q.singleValue; p.multiValues = q.multiValues` | `p.copyValueRegisterFrom(&q.vmState)` |
| `machine_continuation.go:104-105` (inside `NewMachineContinuationFromMachineContext`) | `q.singleValue = mc.singleValue; q.multiValues = mc.multiValues` | `q.copyValueRegisterFrom(&mc.vmState)` |
| `machine_continuation.go:161-162` (inside `Copy`) | `q.singleValue = p.singleValue; q.multiValues = slices.Clone(p.multiValues)` | `q.cloneValueRegisterFrom(&p.vmState)` |

After Phase 2 the production canary returns zero hits:
```
grep -n 'singleValue\|multiValues' machine/*.go \
  | grep -v vm_state.go | grep -v _test.go
# (no output)
```

### Phase 3 — Migrate test fixtures

Two test files reference the fields by name:

- `machine/machine_context_test.go:42` — inside a `vmState{}` literal in
  `TestNewMachineContext`. Replace with construction-then-setter:
  ```go
  cont := &MachineContinuation{
      vmState: vmState{ env: env, template: tpl, evals: NewStack(), pc: 5 },
      parent:  parentCont,
  }
  cont.SetValue(values.NewInteger(42))
  ```

- `machine/operation_test.go:31-32` (struct-field names in test-case
  type) — leave as-is; these are local test-case fields, not `vmState`
  references. Lines 247-248 (`mc.multiValues = tc.multiValues; mc.singleValue
  = tc.singleValue`) DO reference `vmState`; rewrite as:
  ```go
  switch {
  case tc.multiValues != nil:
      mc.SetValues(tc.multiValues...)
  case tc.singleValue != nil:
      mc.SetValue(tc.singleValue)
  }
  ```
  Both cases preserve the existing test semantics (one of the two fixture
  fields is non-nil in any given test case).

After Phase 3 the full canary including tests passes; the only places the
literal field names appear in `machine/` are:
- `vm_state.go` (definition + helper bodies)
- `operation_test.go` test-case struct field names (not `vmState` accesses)

### Phase 4 — Add ruleguard rule + golden test

`ruleguard/rules.go`: add `noDirectValueRegisterAccess`, modelled on
`noBareSentinelPanic`:

```go
// noDirectValueRegisterAccess flags direct reads or writes of the
// machine/vmState value-register fields (singleValue, multiValues) outside
// machine/vm_state.go. The fields form a split-representation register with
// a documented mutual-exclusion invariant ("at most one field is active at
// any time", vm_state.go) that is unenforced by the type system. All access
// must go through the SetValue / SetValues / GetValue / GetValues /
// PushValues / pushValueRegisterTo / copyValueRegisterFrom /
// cloneValueRegisterFrom helpers on *vmState.
//
// See plans/2026-05-06-machine-structural-reduction.md (Finding 3) and
// plans/2026-05-11-machine-sr-finding3-impl.md.
//
//	// Wrong:
//	mc.singleValue = v
//	mc.multiValues = nil
//
//	// Right:
//	mc.SetValue(v)
func noDirectValueRegisterAccess(m dsl.Matcher) { //nolint:unused
    m.Match(
        `$x.singleValue`,
        `$x.multiValues`,
    ).
        Where(!m.File().Name.Matches(`vm_state\.go$`) &&
            !m.File().Name.Matches(`_test\.go$`)).
        Report(`direct value-register access: use SetValue/SetValues/GetValue/GetValues or the *ValueRegisterFrom helpers on *vmState (Finding 3)`)
}
```

`ruleguard/rules_test.go`: add fixture functions and expectation entries.
Positive: a function that does `mc.singleValue = v`; negative: a function
that calls `mc.SetValue(v)`. Test-file fixture verifies the
`!_test.go$` exemption.

The rule exempts `_test.go` files because the test fixture in
`machine_context_test.go` initializes the `vmState{}` struct literal
directly (legitimate test setup); the production canary in Phase 2 already
verifies no production accesses remain.

### Phase 5 — Verify

```
make lint && make covercheck && make ci
```

Re-run the parent-plan canary to confirm zero non-vm_state.go,
non-test hits remain in production code.

## Risk

- **Allocation regression**: All three new helpers preserve the
  zero-allocation fast paths. `pushValueRegisterTo` keeps the same two
  branches as the inlined OpPush code; `copyValueRegisterFrom` is a 2-field
  assignment (the Go compiler will likely inline it). Run
  `make bench-extended` and confirm geomean ≤ baseline ±0.5%.
- **Method promotion semantics**: `MachineContext` and `MachineContinuation`
  embed `vmState` by value. Methods with `*vmState` receivers are promoted
  to `*MachineContext` / `*MachineContinuation`. Existing call sites like
  `mc.SetValue(v)` continue to type-check and dispatch identically.
- **Ruleguard false positives**: The rule matches `$x.singleValue` /
  `$x.multiValues` syntactically. The `vmState` struct also has fields with
  those names; access patterns within `vm_state.go` are exempted by the
  filename guard. No other Wile type has fields with these names (verified
  by `grep -rn 'singleValue\|multiValues' .` outside `machine/`).

## Commit cadence

Following `feedback_commit_cadence.md` (progressive commits):

1. `docs(plans): finding 3 impl plan for machine value-register encapsulation`
2. `refactor(machine): move value-register accessors to vmState`
3. `refactor(machine): route production sites through vmState helpers`
4. `test(machine): migrate value-register test fixtures to accessors`
5. `lint(ruleguard): add noDirectValueRegisterAccess`

Each commit builds and passes its own tests independently.
