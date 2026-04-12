# Structural Reduction Analysis — April 2026

**Status**: **Complete** (PRs #610-612). Phase 2 intentionally rejected (1.5% regression).
**Date**: 2026-04-05
**Method**: Full-codebase structural analysis (40 packages, Martin's instability metrics, type precision audit)
**Related**: `TECH-DEBT-2026-04.md` (Phases 5, 8), `2026-03-30-machine-decomposition-design.md`

## Executive Summary

Three high-impact structural improvements, ordered by dependency-surface reduction:

1. **CallContext interface** — narrows extension coupling from 30+ methods to 7 (Phase 1)
2. **Promoted op table** — replaces 24 hand-unrolled switch cases with data (Phase 2)
3. **Thread outcome sum type** — eliminates impossible `result AND exception` states (Phase 3)

Five medium-impact improvements documented but deferred to opportunistic execution.

---

## Stability Landscape

Instability I = Ce/(Ca+Ce). Lower = more stable (many dependents, few dependencies).

| Tier | Packages | I Range | Role |
|------|----------|---------|------|
| Foundation | `werr` (0.00), `values` (0.03), `stdlib` (0.00) | 0.00-0.03 | Types, errors |
| Core | `syntax` (0.10), `registry` (0.10), `security` (0.12), `machine` (0.18), `environment` (0.21) | 0.10-0.21 | VM, bindings |
| Pipeline | `parser` (0.42), `compilation` (0.55), `forms`/`tokenizer` (0.60) | 0.42-0.60 | Parse/compile |
| Leaf | extensions (0.60-0.77), `bootstrap` (0.94), `cmd/wile` (1.00) | 0.60-1.00 | Consumers |

### SDP Violations

| Stable Pkg | I | Depends On | I | Assessment |
|---|---|---|---|---|
| `machine` | 0.18 (Ca=30) | `internal/match` | 0.66 | Real but bounded — match deps are all stable |
| `wile` (root) | 0.07 | `compilation` | 0.55 | Architecturally necessary (facade pattern) |
| `wile` (root) | 0.07 | `registry/core` | 0.77 | One-time bootstrap init |

No action needed on SDP violations — all are either architecturally necessary or bounded
by stable transitive deps.

---

## Phase 1: CallContext Interface (High impact, medium effort)

### Problem

Every extension and registry helper depends on `*machine.MachineContext` (30+ exported
methods). Actual usage across all 7 public extensions:

| Method | Extensions using it |
|---|---|
| `Arg(int)` | ALL 7 |
| `SetValue(v)` | ALL 7 |
| `SetValues(vs...)` | threads, introspection, math |
| `Authorizer()` | files, process |
| `Context()` | files, process, system |
| `EnvironmentFrame()` | introspection |
| `Thread()` | threads, gointerop |

7 of 30+ methods = 23% utilization. Extensions carry 77% phantom coupling to VM internals.

### Theory

Interface Segregation Principle (Martin, *Clean Architecture*): depend on the narrowest
possible interface. A dependency on `*MachineContext` is a product type of 30+ method
contracts. Projecting to 7 methods reduces the coupling surface by 77%.

### Fix

**Step 1**: Define `CallContext` interface in `machine/`:

```go
// CallContext is the extension-facing subset of MachineContext.
// Extensions and ForeignFunctions should depend on this interface,
// not on *MachineContext directly.
type CallContext interface {
    Arg(index int) values.Value
    SetValue(v values.Value)
    SetValues(vs ...values.Value)
    Authorizer() security.Authorizer
    Context() context.Context
    EnvironmentFrame() *environment.EnvironmentFrame
    Thread() *values.Thread
}
```

`*MachineContext` already satisfies this — zero implementation cost.

**Step 2**: Change `ForeignFunction` signature:

```go
// Before:
type ForeignFunction func(mc *MachineContext) error

// After:
type ForeignFunction func(cc CallContext) error
```

**Step 3**: Update all extension call sites (`mc *machine.MachineContext` -> `cc machine.CallContext`
in function signatures). The `mc.Arg()`, `mc.SetValue()` calls don't change — same methods.

**Step 4**: Update `registry/helpers` to accept `CallContext`.

### Scope Assessment

Files affected: Every extension file, every `registry/helpers/*.go`, `ForeignClosure.Apply`.
Method bodies don't change — only parameter types in signatures.

**Risk**: If any extension secretly uses a `MachineContext`-specific method not on the
interface, the compiler catches it immediately. This is a mechanical refactor with
zero behavior change.

**Open question**: Some internal extensions (`internal/extensions/eval`) call
`compilation.Compile()` via `MachineContext.ExpanderContext()`. These need the full
`*MachineContext`, not `CallContext`. Audit before executing:

```
internal/extensions/eval  — uses ExpanderContext(), compilation pipeline
internal/extensions/io    — uses parser, tokenizer (read/write)
internal/extensions/namespace — uses compilation.NewCompiledLibrary
```

**Decision**: Internal extensions may keep `*MachineContext`. Only public extensions
and `registry/helpers` migrate to `CallContext`. This preserves the internal/public
boundary the codebase already maintains.

### Verify

```
make lint && make test ./machine/... ./registry/... ./extensions/...
```

### Effort: M

---

## Phase 2: Table-Driven Promoted Ops — REJECTED (2026-04-06)

### Problem

34 case branches (17 ops × tail/non-tail) in the VM dispatch loop, each identical
except for data passed to `execPromoted`. Hand-unrolled loop per Bird & de Moor.

### Experiment

Replaced 34 switch cases with `promotedOpTable` array (17 entries) + range check
in the `default:` branch. All tests passed. Benchmarked with `make bench-gabriel`
(6 runs averaged, Apple M4 Max).

### Results

| Benchmark  | Baseline | Table  | Change |
|------------|----------|--------|--------|
| tak        | 0.1150   | 0.1172 | +1.9%  |
| takl       | 1.0691   | 1.0743 | +0.5%  |
| ctak       | 1.6177   | 1.6475 | +1.8%  |
| cpstak     | 0.1868   | 0.1890 | +1.2%  |
| fib        | 0.3793   | 0.3861 | +1.8%  |
| triangl    | 0.0404   | 0.0407 | +0.7%  |
| sum        | 0.0319   | 0.0317 | -0.6%  |
| sumfp      | 1.0607   | 1.0813 | +1.9%  |
| diviter    | 2.3788   | 2.4065 | +1.2%  |
| divrec     | 0.8847   | 0.8931 | +0.9%  |
| deriv      | 0.1064   | 0.1081 | +1.6%  |
| ackermann  | 0.4602   | 0.4757 | +3.4%  |
| sieve      | 0.0877   | 0.0892 | +1.7%  |
| nqueens    | 1.6474   | 1.6808 | +2.0%  |
| primes     | 0.2315   | 0.2359 | +1.9%  |
| peval      | 0.0816   | 0.0817 | +0.1%  |

Geo mean regression: ~1.5%. 15/16 benchmarks slower.

### Why

Go compiles a contiguous-integer switch to a jump table (single indexed indirect
jump). Moving promoted ops to the `default:` branch replaces this with: range check,
two integer divisions, pointer-chased array load, indirect function call through the
loaded entry. The jump table avoids all of these.

### Decision

**Keep the hand-unrolled switch.** The ~1.5% regression is real and consistent.
The maintenance cost (4 edit sites per new promoted op) is acceptable given the
low frequency of adding new promoted ops. Accept the trade-off: readability cost
is the price of hot-path performance.

---

## Phase 3: Thread Outcome Sum Type (Medium impact, small effort) ✅

**Completed**: Replaced `result Value` + `exception error` with `*threadOutcome` pointer.
Nil until terminated, then `err != nil` discriminates success/failure. Eliminates
impossible `result AND exception` state. 4 write sites, 1 read site, all internal.

### Problem

`values/thread.go:96-130` — `Thread` has independent fields `state ThreadState` (4 values),
`result Value`, `exception error`:

```
Representable: 4 x (nil|value) x (nil|error) = 4 x 2 x 2 = 16 combinations
Valid:         4 states (New, Runnable, Blocked: result=nil exception=nil;
               Terminated: exactly one of result or exception set)
Precision:     4/16 = 25%
```

The invariant `NOT(result != nil AND exception != nil)` is enforced only by runtime
guards in `Join()`, not by the type system. This is a textbook "make illegal states
unrepresentable" opportunity (Minsky, "Effective ML").

### Fix

**Step 1**: Define outcome type in `values/thread.go`:

```go
type threadOutcome struct {
    kind  outcomeKind
    value Value     // valid iff kind == outcomeSucceeded
    err   error     // valid iff kind == outcomeFailed
}

type outcomeKind uint8
const (
    outcomePending   outcomeKind = iota  // thread not yet terminated
    outcomeSucceeded                      // result available
    outcomeFailed                         // exception available
)
```

**Step 2**: Replace `result Value` + `exception error` fields with single `outcome threadOutcome`.

**Step 3**: Update `Join()` to switch on `outcome.kind`.

**Step 4**: Update `Start()` goroutine to set outcome atomically.

### Scope Assessment

`Thread` is only mutated inside `values/thread.go` itself (Start, Join, Terminate methods).
No external code directly reads `result` or `exception` — they go through `Join()`.
This is a purely internal refactor.

### Verify

```
make lint && make test ./values/... ./extensions/threads/...
```

### Effort: S

---

## Deferred Findings (Opportunistic)

These are documented for reference. Execute when working in the affected area.

### D1: PrimitiveSpec Dead Fields — STALE (removed 2026-04-06)

Extension contracts Phase 1 (PRs #577-578) populated `ParamTypes` (170 specs) and
`ReturnType` (129 specs) broadly. These fields are alive. No action.

### D2: ForeignClosure Redundant Fields

**Where**: `machine/foreign_closure.go:35-95`
**Issue**: `doc` is duplicated between ForeignClosure and PrimitiveSpec (denormalization).
`validate` is a per-closure callback that belongs at registration time.
**Fix**: Remove `doc` (look up from registry by name). Evaluate `validate` usage — if
registration-time only, remove.
**Impact**: ForeignClosure shrinks from 7 to 5 fields.
**Cross-ref**: `TECH-DEBT-2026-04.md` Task 5.1 adds `Name()`/`Doc()` to Closure interface.
If Doc() delegates to registry lookup, the `doc` field becomes dead automatically.

### D3: Namespace Root/Child State Waste

**Where**: `environment/namespace.go:27-100`
**Issue**: 16-field struct where child namespaces (from `Derive()`) use ~8 fields. The
unused fields are nil but representable — type precision ~50%.
**Recommendation**: Document which fields are valid in root vs. child mode as a
representation invariant comment. Don't split the type unless allocation pressure
is measured. The type is internal (not public API), so documentation suffices.

### D4: LocalIndex / BindingID Overlap — AUDITED (2026-04-06), No action

**Where**: `environment/local_index.go:19-33`, `environment/binding_id.go:19-33`
**Issue**: Two representations of "which binding?"
**Audit result**: `BindingID` is used in `internal/validate` (mutation tracking, capture
analysis B1, escape analysis B2) and `machine/compilation` (inline candidate registry,
recursion guard). These are map-key use cases requiring *stable identity* — a binding
must produce the same key regardless of which frame references it.
**Conclusion**: Not replaceable. `LocalIndex` is relative (slot+depth from a reference
frame — same binding, different keys from different depths). `BindingID` is absolute
(frame pointer + slot — same binding, same key always). Both needed; overlap is superficial.

### D5: Opcode Metadata Consolidation — DONE (2026-04-06)

Added `operandKind OperandKind` to `opcodeInfo` (7 categories: None, Raw, LiteralIdx,
LocalIdx, BranchOffset, CachedBinding, SideTable). `Disassemble()` and
`instructionToOperation()` now switch on `opcodeTable[op].operandKind` instead of
per-opcode case branches. Run() untouched (hot path). Adding a new promoted op now
requires only 3 files (opcode.go, machine_context.go, call_promoted.go) instead of 5.

### D6: Number Interface ISP

**Where**: `values/values.go:307-326`
**Issue**: 18-method interface. Violates ISP — consumers wanting only comparison are coupled
to arithmetic.
**Recommendation**: **Do not split.** The 18 methods form a coherent algebra (partially
ordered field with exactness). Splitting would break the 41 dispatch tables that give O(1)
cross-type arithmetic. This is a deliberate trade-off: ISP violation is the *cost* of the
dispatch architecture. Document the trade-off; don't hide it.

### D7: environment `os` Import

**Where**: `environment/resolve.go:16-82`
**Issue**: `ResolveFile()` calls `os.Stat()` directly, coupling a core package (I=0.21,
Ca=15) to the OS filesystem.
**Cross-ref**: `TECH-DEBT-2026-04.md` Task 2.1 investigated this and found it is *not* an
issue — `ResolveFile` has exactly one caller (`OSFileResolver.ResolveAndOpen`), which is
the correct place for OS filesystem access. `FSFileResolver` has its own resolution logic.
**Recommendation**: No action needed. The dependency is properly scoped.

---

## Summary

| Phase | Task | Effort | Impact | Metric |
|-------|------|--------|--------|--------|
| 1 | CallContext interface | M | High | 77% coupling surface reduction across 12 packages |
| 2 | Promoted op table | S-M | Medium | **REJECTED** — ~1.5% geo mean regression from jump table → default branch |
| 3 | Thread outcome sum type | S | Medium | ✅ Eliminates impossible `result AND exception` states |
| D1 | PrimitiveSpec dead fields | — | — | **STALE** — fields populated by extension contracts Phase 1 |
| D2 | ForeignClosure denorm | S | Low | 7 -> 5 fields |
| D3 | Namespace root/child docs | S | Low | Documentation only |
| D4 | LocalIndex/BindingID audit | S | — | Audited: both needed (relative vs absolute identity) |
| D5 | Opcode operandKind metadata | S | Medium | ✅ Disassemble + instructionToOperation metadata-driven; promoted ops 5→3 edit sites |
| D6 | Number interface ISP | — | — | Deliberately not fixed (trade-off documented) |
| D7 | environment os import | — | — | Already investigated (TECH-DEBT Task 2.1) |

**Recommended order**: Phase 1 (highest coupling reduction) -> Phase 3 (smallest, quick win)
-> Phase 2 (profile-dependent, do with benchmarks available).
