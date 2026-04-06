# Structural Reduction Analysis — April 2026

**Status**: Phase 1 Complete
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

## Phase 2: Table-Driven Promoted Ops (Medium impact, small effort)

### Problem

`machine/machine_context.go:566-741` has 24 case branches in the VM dispatch loop,
each identical except for data:

```go
case OpEqQ:
    mc, err = execPromoted(mc, instr, "eq?", 2, false, inlineEq)
case OpEqQTail:
    mc, err = execPromoted(mc, instr, "eq?", 2, true, inlineEq)
// ... 22 more identical patterns
```

This is a hand-unrolled loop: N blocks differing only in `(name, arity, inlineFunc)` x
`{tail=false, tail=true}`. See Bird & de Moor, *Algebra of Programming* — the transition
from enumeration to induction.

Adding a new promoted op requires editing 4 locations: opcode enum, Run() (2 cases),
disassembly, and operationToInstruction.

### Fix

**Step 1**: Add metadata to `opcodeInfo` in `machine/opcode.go`:

```go
type opcodeInfo struct {
    name        string
    writesValue bool
    isBranch    bool
    operandKind OperandKind   // NEW: none, literalIdx, localIdx, branchOffset, cachedBinding, promoted
}

type OperandKind uint8
const (
    OperandNone OperandKind = iota
    OperandLiteralIdx
    OperandLocalIdx       // bit-packed slot+depth
    OperandBranchOffset
    OperandCachedBinding
    OperandPromoted
)
```

**Step 2**: Add promoted op lookup table in `machine/call_promoted.go`:

```go
type promotedOpInfo struct {
    name   string
    arity  int
    inline func(*MachineContext, Instruction) (*MachineContext, error)
}

// Indexed by (OpCode - firstPromotedOp) / 2 — each promoted op has non-tail + tail variant.
var promotedOps = [...]promotedOpInfo{
    {"eq?", 2, inlineEq},
    {"vector?", 1, inlineVectorP},
    {"vector-ref", 2, inlineVectorRef},
    // ... 12 entries total
}
```

**Step 3**: Replace 24 case branches in `Run()` with:

```go
default:
    if instr.Op >= firstPromotedOp && instr.Op < opCount {
        idx := (instr.Op - firstPromotedOp) / 2
        tail := (instr.Op-firstPromotedOp)%2 == 1
        info := &promotedOps[idx]
        mc, err = execPromoted(mc, instr, info.name, info.arity, tail, info.inline)
        if err != nil {
            return err
        }
    }
```

**Caveat**: The VM dispatch loop is the hot path. The Go compiler optimizes the current
switch to a jump table. A table lookup adds one indirection. **Profile before and after**
using `make bench-gabriel` to verify no regression. If the table approach is measurably
slower, keep the switch but generate it from the table at code-gen time (or accept the
maintenance cost).

**Step 4**: Simplify `Disassemble()` using `operandKind` metadata instead of case branches.

**Step 5**: Simplify `instructionToOperation()` similarly.

### Verify

```
make bench-gabriel  # before and after
make lint && make test ./machine/...
```

### Effort: S-M

---

## Phase 3: Thread Outcome Sum Type (Medium impact, small effort)

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

### D1: PrimitiveSpec Dead Fields

**Where**: `registry/registry.go:23-34`
**Issue**: `ParamTypes` (5% usage across 115 specs) and `ReturnType` (2% usage) are nearly
dead. They suggest a contract system that doesn't exist at runtime.
**Fix**: Remove both fields. If contract checking is needed later, implement as a separate
opt-in `PrimitiveContract` type.
**Impact**: Spec shrinks from 9 to 7 fields. Removes misleading API surface.
**Cross-ref**: Relates to `2026-03-26-extension-contracts-design.md` — if contract system
moves forward, these fields may be revived in a different form. Coordinate.

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

### D4: LocalIndex / BindingID Overlap

**Where**: `environment/local_index.go:19-33`, `environment/binding_id.go:19-33`
**Issue**: Two representations of "which binding?" — `LocalIndex` (relative, compile-time)
and `BindingID` (absolute, runtime). They're different views of the same concept.
**Recommendation**: Audit whether `BindingID` is used outside environment/. If internal-only,
consider replacing with `LocalIndex`. If both are needed, document which phase uses which.

### D5: Opcode Metadata Consolidation

**Where**: `machine/opcode.go:96-170`
**Issue**: Three dispatch loops over OpCode (Run, instructionToOperation, Disassemble)
each re-derive operand semantics via case branches. Adding a new opcode means updating
3 switch statements.
**Fix**: Add `operandKind OperandKind` to `opcodeInfo`. `Disassemble()` and
`instructionToOperation()` use metadata instead of case branches. Run() stays as switch
(hot path — don't add indirection).
**Note**: Partially addressed by Phase 2. If Phase 2 is implemented, D5 covers the
remaining non-promoted opcodes.

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
| 2 | Promoted op table | S-M | Medium | 24 cases -> 1 loop + 12-entry table; O(1) new-op cost |
| 3 | Thread outcome sum type | S | Medium | Eliminates impossible `result AND exception` states |
| D1 | PrimitiveSpec dead fields | S | Low | 9 -> 7 fields |
| D2 | ForeignClosure denorm | S | Low | 7 -> 5 fields |
| D3 | Namespace root/child docs | S | Low | Documentation only |
| D4 | LocalIndex/BindingID audit | S | Low | Potential type elimination |
| D5 | Opcode operandKind metadata | S | Medium | 3 dispatch loops -> metadata-driven |
| D6 | Number interface ISP | — | — | Deliberately not fixed (trade-off documented) |
| D7 | environment os import | — | — | Already investigated (TECH-DEBT Task 2.1) |

**Recommended order**: Phase 1 (highest coupling reduction) -> Phase 3 (smallest, quick win)
-> Phase 2 (profile-dependent, do with benchmarks available).
