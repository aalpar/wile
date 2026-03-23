# Unboxed Float Pipeline

**Status:** Proposed
**Requires `unsafe`:** No. Pure Go struct changes and VM dispatch.
**Thread safety:** Preserved. All modified state is per-MachineContext (one per SRFI-18 thread).

## Motivation

The `sumfp` benchmark (1M-iteration floating-point summation loop) spends **85% of CPU
time in GC**, not in computation. CPU profile (2026-03-22, M4 Max):

| Category | % of CPU | Source |
|----------|----------|--------|
| GC coordination (`kevent`, `pthread_cond_wait`) | 49% | Stop-the-world pauses |
| GC scanning/sweeping (`madvise`, `scanObjectsSmall`) | 15% | Object graph traversal |
| VM execution (`Run`, `execPromoted`, `drainAndApply`) | 15% | Actual computation |
| Allocation (`mallocgc`) | 3% | `NewFloat` in `Float.Add` |
| Other runtime | 18% | Scheduling, stack management |

The root cause: each `+` operation on two `*Float` values allocates a new `*Float` via
`NewFloat`. In `sumfp`, that's **2 allocations per iteration x 1M iterations x 7 runs =
14M short-lived heap objects**. Each `*Float` is 8 bytes of payload + Go object header.
The GC spends 6x more time collecting these objects than the VM spends computing.

The integer `sum` benchmark doesn't have this problem because `Integer` values in the
range -32768..32767 are cached singletons. `Float` has no analogous cache — every
arithmetic result allocates.

## Design Overview

Eliminate `*Float` heap allocations in tight arithmetic loops by propagating raw `float64`
values through three layers of VM infrastructure:

```
                     Currently                      With Unboxed Pipeline
                     ─────────                      ────────────────────

  OpAdd result:      NewFloat(a+b) → *Float         mc.floatVal = a+b
                          │ (ALLOC)                       │ (no alloc)
                          ▼                               ▼
  OpPush:            evals.Push(*Float)              evals.PushFloat(f64)
                          │ (interface box)                │ (no alloc)
                          ▼                               ▼
  Drain/Apply:       []values.Value → bindArgs       []stackEntry → bindArgs
                          │                               │
                          ▼                               ▼
  Binding:           bd.value = *Float               bd.floatVal = f64
                          (already exists)                (no alloc)
```

**Per-iteration allocations**: 2 → 0 for sumfp.
**Estimated impact**: 5-10x improvement on `sumfp` (from ~1.1s to ~0.1-0.2s).

## Detailed Design

### Layer 1: Value Register

Add unboxed float storage to `vmState`. The value register already has a split
representation (`singleValue`/`multiValues`); this adds a third path.

```go
// vm_state.go
type vmState struct {
    // ... existing fields ...
    singleValue values.Value
    multiValues MultipleValues
    floatVal    float64  // NEW: unboxed float result
    hasFloat    bool     // NEW: true when floatVal is active
    // ...
}
```

**Invariant**: At most one of `{singleValue, multiValues, floatVal}` is active.
- `SetValue(v)` → sets `singleValue`, clears `hasFloat`, nils `multiValues`
- `SetValues(vs)` → sets `multiValues`, clears `hasFloat`, nils `singleValue`
- `setFloatResult(f)` → sets `floatVal` + `hasFloat`, nils both others
- `GetValue()` → if `hasFloat`, materializes via `NewFloat(floatVal)`; otherwise existing logic

The `setFloatResult` method is unexported — only promoted ops use it. External code
sees the same `GetValue()` API with lazy materialization.

**Continuation impact**: `SaveContinuation` copies `floatVal`/`hasFloat` into the
continuation's embedded `vmState`. `RestoreAndRelease` / `PopContinuation` restore them.
This is already how `singleValue`/`multiValues` work — the float register follows the
same pattern.

### Layer 2: Tagged Eval Stack

Replace the stack's element type from `values.Value` to a tagged entry that can hold
either a boxed value or an unboxed float.

```go
// stack.go
type stackEntry struct {
    val      values.Value  // 16 bytes — boxed value (nil when isFloat)
    floatVal float64       // 8 bytes  — unboxed float
    isFloat  bool          // 1 byte   — tag
    // 7 bytes padding (alignment to 8)
}
// Total: 32 bytes per entry (up from 16 bytes)

type Stack []stackEntry
```

**Tag semantics**: When `isFloat` is true, `val` is nil and `floatVal` holds the data.
When `isFloat` is false, `val` holds the data and `floatVal` is ignored.

**Why not nil-as-sentinel**: `OpPushLocal` can push nil values from uninitialized bindings.
A separate `isFloat` flag is correct regardless of value content.

**API changes to Stack**:

| Method | Current | New |
|--------|---------|-----|
| `Push(v Value)` | append value | append `stackEntry{val: v}` |
| `PushFloat(f float64)` | — | append `stackEntry{floatVal: f, isFloat: true}` |
| `Pop() Value` | return top | if `isFloat`, materialize via `NewFloat`; else return `val` |
| `popEntry() stackEntry` | — | return raw entry (internal, for float-aware consumers) |
| `Pull() Value` | return bottom | same materialization logic as `Pop` |
| `PeekK(i) Value` | return kth | same materialization logic |
| `PopN(n) []Value` | allocate + copy | materialize all entries |
| `Drain() []Value` | zero-copy view | **removed** — replaced by `drainEntries()` |
| `drainEntries() []stackEntry` | — | zero-copy view of entries (internal) |
| `DrainMaterialized() []Value` | — | allocate + materialize (for generic dispatch) |
| `PopAll() []Value` | allocate + copy | materialize all entries |
| `Copy() *Stack` | clone slice | clone entry slice |
| `AsList() Tuple` | build list | materialize all entries, build list |

**Memory impact**: Stack entries grow from 16 to 32 bytes. For typical stack depths
(2-10 entries), this is 32-160 extra bytes. Negligible.

**Inline continuation storage**: `inlineEvals` changes from `[2]values.Value` (32 bytes)
to `[2]stackEntry` (64 bytes). 32 extra bytes per continuation frame. Continuations are
pooled, so this is a one-time cost per pool slot.

### Layer 3: Binding Unboxing

Add unboxed float storage to `Binding` to avoid materialization at the binding boundary.

```go
// environment/binding.go
type Binding struct {
    value       values.Value  // 16 bytes
    floatVal    float64       // 8 bytes  — NEW
    bindingType BindingType   // 8 bytes
    meta        *BindingMeta  // 8 bytes
    hasFloat    bool          // 1 byte   — NEW (+7 padding)
}
// Total: 48 bytes (up from 32 bytes)
```

**API changes to Binding**:

| Method | Behavior |
|--------|----------|
| `Value()` | if `hasFloat`, return `NewFloat(floatVal)`; else return `value` |
| `FloatValue() (float64, bool)` | if `hasFloat`, return `(floatVal, true)`; else try type-assert `value` to `*Float` |
| `SetValue(v)` | set `value = v`, clear `hasFloat` |
| `SetFloatValue(f)` | set `floatVal = f`, `hasFloat = true`, `value = nil` |
| `SetEntry(e stackEntry)` | if `e.isFloat`, call `SetFloatValue`; else call `SetValue` |

**Direct field access sites** that must also clear `hasFloat`:

| File | Line | Code |
|------|------|------|
| `environment_frame.go` | 681 | `env.local.bindings[li[0]].value = v` |
| `environment_frame.go` | 699 | `env.local.bindings[slot].value = v` |
| `global_environment_frame.go` | 236 | `ge.bindings[i].value = v` |
| `local_environment_frame.go` | 105 | `p.bindings[li[0]].value = v` |

All four must be updated to also set `hasFloat = false`. Alternatively, convert these
direct field writes to `SetValue()` calls. The latter is cleaner and prevents future
regressions.

**Memory impact**: Binding grows from 32 to 48 bytes (50%). For closures with 1-3
parameters, this is 16-48 extra bytes per frame. The GC savings from eliminating 14M
Float allocations (~112 MB of transient heap pressure) vastly outweigh this.

**copyForApplyInto impact**: This method copies bindings from source to destination
frame. With larger bindings, each copy moves 48 bytes instead of 32. The copy count is
unchanged (equal to parameter count, typically 1-3). Impact: negligible.

### Promoted Op Changes

The promoted arithmetic ops (`inlineAdd`, `inlineNumGe`, etc.) get a `*Float` fast path
that reads unboxed floats from the stack and writes unboxed floats to the value register.

```go
// call_promoted_arithmetic.go

// popTwoFloats attempts to pop two Float values from the stack without
// materializing. Returns raw float64 values if both entries are unboxed
// floats or boxed *Float. Falls back to popTwoNumbers for other types.
func popTwoFloats(mc *MachineContext) (float64, float64, bool) {
    eb := mc.evals.popEntry()
    ea := mc.evals.popEntry()
    mc.counters.StackDrains++
    mc.counters.StackElementsDrained += 2
    mc.counters.ForeignCalls++

    af, aOk := entryAsFloat(ea)
    bf, bOk := entryAsFloat(eb)
    if aOk && bOk {
        return af, bf, true
    }
    // Push entries back for popTwoNumbers fallback
    mc.evals.pushEntry(ea)
    mc.evals.pushEntry(eb)
    return 0, 0, false
}

func entryAsFloat(e stackEntry) (float64, bool) {
    if e.isFloat {
        return e.floatVal, true
    }
    f, ok := e.val.(*values.Float)
    if ok {
        return f.Value, true
    }
    return 0, false
}

func inlineAdd(mc *MachineContext) error {
    a, b, ok := popTwoFloats(mc)
    if ok {
        mc.setFloatResult(a + b)
        return nil
    }
    // Slow path: general Number dispatch
    an, bn, err := popTwoNumbers(mc, "+")
    if err != nil {
        return err
    }
    mc.SetValue(an.Add(bn))
    return nil
}
```

Similarly for `inlineNumGe`:

```go
func inlineNumGe(mc *MachineContext) error {
    a, b, ok := popTwoFloats(mc)
    if ok {
        // IEEE 754: NaN fails all comparisons
        mc.SetValue(values.BoolToBoolean(a >= b))
        return nil
    }
    // Slow path: general Number dispatch with complex/NaN checks
    // ... existing code ...
}
```

**Note on `>=` semantics**: The fast path uses Go's `>=` on `float64`, which correctly
handles NaN per IEEE 754 (NaN >= anything = false). The `isNonRealComplex` check is
skipped because a `float64` is never complex. This is semantically identical to the
current code but eliminates 2 type assertions + 2 method calls.

### Run() Loop Changes

| Opcode | Current | New |
|--------|---------|-----|
| `OpPush` | `evals.Push(mc.singleValue)` | if `hasFloat`, `evals.PushFloat(mc.floatVal)`; else existing |
| `OpPushLocal` | `evals.Push(bd.Value())` | if `bd.hasFloat`, `evals.PushFloat(bd.floatVal)`; else existing |
| `OpLoadLocal` | `mc.SetValue(bd.Value())` | if `bd.hasFloat`, `mc.setFloatResult(bd.floatVal)`; else existing |
| `OpLoadLiteral` | `mc.SetValue(literal)` | check if literal is `*Float`, set float register |
| `OpPull` | `mc.SetValue(evals.Pull())` | materialize (Pull returns `values.Value`) |
| `OpPeekK` | `mc.SetValue(evals.PeekK(i))` | materialize |
| `OpDrop` | `evals.Pop()` | `evals.popEntry()` (discard without materializing) |

**OpPushCachedBinding**: The cached binding is a `*environment.Binding`. Use the same
float-aware path as `OpPushLocal`.

**OpStoreLocal**: Currently pops from stack and sets binding. With tagged stack:
pop entry, set binding via `SetEntry(e)`. Avoids materialization when storing an unboxed
float into a binding.

### Apply / Tail Call Path

The tail call path for `sumfp` goes through `drainAndApply` → `ApplyCallable` → `Apply`.

**Fast path** (known `MachineClosure`, non-variadic — covers sumfp):

```go
func (p *MachineContext) drainAndApply(callable values.Value) (*MachineContext, error) {
    mcls, ok := callable.(*MachineClosure)
    if ok && !mcls.Template().IsVariadic() {
        return p.applyFromEntries(mcls)
    }
    // Slow path: materialize all entries, generic dispatch
    vs := p.evals.DrainMaterialized()
    // ... existing ApplyCallable code ...
}

func (p *MachineContext) applyFromEntries(mcls *MachineClosure) (*MachineContext, error) {
    entries := p.evals.drainEntries()
    // ... arity check, env frame acquisition (same as Apply) ...
    bindArgsFromEntries(bnds, entries, paramCount)
    // ... set template, env, pc=0 ...
}

func bindArgsFromEntries(bnds []environment.Binding, entries []stackEntry, n int) {
    for i := range bnds[:n] {
        bnds[i].SetEntry(entries[i])
    }
}
```

**Slow path** (variadic, CaseLambda, ForeignClosure, Parameter, etc.): call
`DrainMaterialized()` which returns `[]values.Value` with all floats boxed. These paths
are not performance-critical for tight arithmetic loops.

## Phase Plan

### Phase 1: Value Register (small, foundational)

Add `floatVal`/`hasFloat` to `vmState`. Update `SetValue`, `GetValue`, `SetValues`.
Update `SaveContinuation`/`RestoreAndRelease`/`PopContinuation` to preserve float
register. No benefit yet for sumfp (immediate Push materializes), but establishes the
three-way value register invariant. Independently testable.

**Files**: `machine/vm_state.go`, `machine/machine_context.go` (SetValue/GetValue),
`machine/machine_context_continuation.go`

**Tests**: Verify float register round-trips through continuation save/restore.

### Phase 2: Tagged Eval Stack (core change, largest)

Replace `Stack = []values.Value` with `Stack = []stackEntry`. Update all Stack methods.
Update Run() opcodes (OpPush, OpDrop, OpPull, OpPeekK). Update continuation inline
storage. Update promoted arithmetic ops with `popTwoFloats` fast path that writes results
via `setFloatResult`. Update `OpPush` to read float register.

This phase alone (combined with Phase 1) eliminates Float allocations inside promoted ops.
Floats still materialize at the `bindArgs` boundary when draining the stack into bindings.

**Files**: `machine/stack.go`, `machine/machine_context.go` (Run loop),
`machine/machine_continuation.go` (inline evals), `machine/machine_context_continuation.go`,
`machine/call_promoted_arithmetic.go`, `machine/call_promoted.go`

**Tests**: Stack unit tests for Push/PushFloat/Pop/popEntry/Copy/Drain. Integration tests
with sumfp (same result, fewer allocations). Benchmark comparison.

### Phase 3: Binding Unboxing (completes the pipeline)

Add `floatVal`/`hasFloat` to `Binding`. Update `Value()` for lazy materialization.
Add `SetFloatValue`, `FloatValue`, `SetEntry` methods. Convert all direct `.value =`
field writes to `SetValue()` calls. Update `OpPushLocal`/`OpLoadLocal` to read unboxed.
Update `OpStoreLocal` to write unboxed. Update `bindArgs` with `bindArgsFromEntries`
variant. Update `drainAndApply` with fast path for non-variadic `MachineClosure`.

This phase eliminates all remaining Float allocations in tight arithmetic loops.

**Files**: `environment/binding.go`, `environment/environment_frame.go`,
`environment/local_environment_frame.go`, `environment/global_environment_frame.go`,
`machine/machine_context.go` (OpPushLocal, OpLoadLocal, OpStoreLocal),
`machine/machine_context_apply.go` (drainAndApply, applyFromEntries),
`machine/arity.go` (bindArgsFromEntries)

**Tests**: Binding unit tests for float path. Full test suite regression. Benchmark
comparison showing zero Float allocations in sumfp.

### Phase 4: Validation

Run full benchmark suite (`make bench-gabriel`, `make bench-extended`). Profile to verify
GC time drops. Verify no regressions in non-float benchmarks. Run SRFI-18 thread tests
to confirm thread safety.

## Thread Safety Analysis

All modified state is per-`MachineContext`:

| Component | Shared? | Why safe |
|-----------|---------|----------|
| `vmState.floatVal/hasFloat` | No | Embedded in MachineContext, one per SRFI-18 thread |
| `Stack` (eval stack) | No | Each MachineContext has its own `*Stack` |
| `Binding` (in frame) | No | `Apply` acquires fresh frame from pool per call to prevent races |
| `MachineContinuation` | Shared when captured | `Copy()` deep-copies, including new float fields |

The `Apply` method already acquires a fresh environment frame from the pool on every call
to prevent SRFI-18 threads from racing on shared binding slots. With the float fields
added, this protection extends automatically — the fresh frame's bindings have their own
`floatVal`/`hasFloat` storage.

The `Stack.Copy()` used in `SaveContinuation` (shared continuations) will copy
`[]stackEntry` including the float fields, preserving isolation.

`sync.Pool` is not used in this design. No shared mutable state is introduced.

## Memory Impact

| Component | Current size | New size | Delta | Quantity |
|-----------|-------------|----------|-------|----------|
| Stack entry | 16 bytes | 32 bytes | +16 | 2-10 per stack |
| Binding | 32 bytes | 48 bytes | +16 | 1-3 per frame |
| Inline evals | 32 bytes | 64 bytes | +32 | Per continuation |
| vmState | +0 | +9 bytes | +9 | Per MachineContext |

Total per-MachineContext increase: ~100-200 bytes. This is constant overhead regardless
of program size.

GC pressure reduction: **14M `*Float` allocations (~112 MB of transient heap)** eliminated
for `sumfp`. The constant overhead is recouped many orders of magnitude over.

## Risks and Mitigations

| Risk | Severity | Mitigation |
|------|----------|------------|
| Missing a stack access site | High | `Stack` type change makes all sites fail to compile |
| Missing a binding write site | Medium | Grep for `.value =` and convert to `SetValue()` |
| Incorrect materialization | Medium | All `Pop()/PeekK()/Pull()` paths must materialize |
| Performance regression in non-float paths | Low | Non-float paths unchanged (same branch, fall through) |
| Continuation corruption | High | Test save/restore round-trip with mixed float/non-float stacks |
| `Drain()` callers assume `[]values.Value` | High | Compiler enforces: return type changes to `[]stackEntry` |

**Mechanical safety**: Changing `Stack` from `[]values.Value` to `[]stackEntry` causes
every call site that assumes `values.Value` elements to fail at compile time. This is a
feature — the compiler finds every site that needs updating.

## Success Criteria

1. `sumfp` runtime drops below 0.2s (from 1.1s) — verified by `make bench-gabriel`
2. GC fraction drops below 20% of CPU — verified by CPU profile
3. Zero `Float` allocations in promoted `+` ops during sumfp — verified by memory profile
4. All existing tests pass — `make test`
5. No regressions > 5% on other Gabriel benchmarks
6. `make lint && make covercheck` pass

## Files Changed (complete list)

### Phase 1
- `machine/vm_state.go` — add `floatVal`, `hasFloat` fields
- `machine/machine_context.go` — `SetValue`, `GetValue`, `SetValues` clear/check float
- `machine/machine_context_continuation.go` — save/restore float register

### Phase 2
- `machine/stack.go` — `stackEntry` type, all methods
- `machine/machine_context.go` — `OpPush`, `OpDrop`, `OpPull`, `OpPeekK`
- `machine/machine_continuation.go` — `inlineEvals` type, `Copy()`, `NewFromMC()`
- `machine/machine_context_continuation.go` — inline evals save/restore
- `machine/call_promoted_arithmetic.go` — `popTwoFloats`, fast paths
- `machine/call_promoted.go` — non-arithmetic promoted ops (no float path needed)

### Phase 3
- `environment/binding.go` — add fields, methods
- `environment/environment_frame.go` — convert `.value =` to `SetValue()`
- `environment/local_environment_frame.go` — convert `.value =` to `SetValue()`
- `environment/global_environment_frame.go` — convert `.value =` to `SetValue()`
- `machine/machine_context.go` — `OpPushLocal`, `OpLoadLocal`, `OpStoreLocal`
- `machine/machine_context_apply.go` — `drainAndApply`, `applyFromEntries`
- `machine/arity.go` — `bindArgsFromEntries`

### Tests (all phases)
- `machine/stack_test.go` — tagged stack operations
- `machine/call_promoted_arithmetic_test.go` — float fast paths
- `environment/binding_test.go` — float binding methods
- `machine/machine_context_test.go` — integration
- Existing test suite — regression

## Non-Goals

- **Integer unboxing**: Integer already has a cache for -32768..32767. Unboxing integers
  would require a second tag path. Not justified by current benchmark data.
- **General Number unboxing**: Only `float64` gets unboxed. Rational, Complex, BigInteger,
  BigFloat remain boxed. The dispatch table architecture handles cross-type promotion;
  unboxing is a fast path that bypasses it for the common case.
- **NaN-boxing**: Requires `unsafe`. Blocked by project constraint.
- **JIT compilation**: Out of scope. This is a VM-level optimization.
- **Compiler-level type inference**: The VM doesn't know types at compile time. The fast
  path is checked at runtime via `isFloat` flags. A future type-specializing compiler
  could emit dedicated float opcodes, but that's a separate design.

## References

- CPU profile: `/tmp/sumfp.prof` (2026-03-22, `fix/modernize-lint-and-dead-code` branch)
- Benchmark data: `examples/benchmarks/canonical-results-20260322-185638.csv`
- Opcode promotion design: `plans/OPCODE-PROMOTION.md`
- Failed optimization attempts: MEMORY.md (callStack, flat closures, inline bindings)
