# Wile Codebase: Structural Analysis

**Date**: 2026-02-14
**Scope**: Recently changed files (HEAD~10) — machine/, internal/match/, values/
**Method**: Dependency metrics (Martin), type precision (Pierce), composability analysis

## Dependency Map

```
                           ┌─────────────────────┐
                           │      values/         │  Ca=25  Ce=0  I=0.00
                           │  (maximally stable)  │
                           └─────────┬────────────┘
                                     │
              ┌──────────────────────┼──────────────────────────┐
              │                      │                          │
   ┌──────────▼──────┐   ┌──────────▼──────────┐   ┌──────────▼──────┐
   │ internal/syntax  │   │   environment/      │   │ internal/forms  │
   │ Ca=14 Ce=1 I=.07 │   │ Ca=14 Ce=2 I=.13   │   │ Ca=2 Ce=0 I=0  │
   └──────────┬───────┘   └──────────┬──────────┘   └────────┬───────┘
              │                      │                        │
   ┌──────────▼─────────────────────▼────────────────────────▼──────┐
   │                        machine/                                 │
   │                Ca=17  Ce=8  I=0.32                              │
   │  Imports: values, environment, syntax, parser, forms,           │
   │           schemeutil, validate, match                           │
   └──────────┬──────────────────────────────────────────────────────┘
              │
   ┌──────────▼──────────┐    ┌──────────────────────┐
   │    registry/        │    │   registry/helpers/   │
   │ Ca=12 Ce=3 I=.20   │    │ Ca=10 Ce=3 I=.23     │
   └──────────┬──────────┘    └──────────────────────┘
              │
   ┌──────────▼──────────┐
   │   registry/core/    │    extensions/*, runtime/, bootstrap/
   │ Ca=2 Ce=7 I=.78    │    (high instability, leaf consumers)
   └─────────────────────┘
```

**Key metrics**: 103 internal dependency edges, 33 packages, 0 cycles (clean DAG), 0 SDP violations. Instability range: [0.00, 1.00].

## Package Coupling Metrics

```
Package                                                                    Ca   Ce      I
------------------------------------------------------------------------------------------
values/                                                                    25    0  0.000
internal/forms                                                              2    0  0.000
internal/syntax                                                            14    1  0.067
environment/                                                               14    2  0.125
internal/schemeutil                                                        11    2  0.154
registry/                                                                  12    3  0.200
registry/helpers/                                                          10    3  0.231
machine/                                                                   17    8  0.320
internal/parser                                                             8    4  0.333
internal/tokenizer                                                          2    2  0.500
internal/extensions/exceptions                                              3    5  0.625
registry/core/                                                              2    7  0.778
internal/match                                                              1    3  0.750
runtime/                                                                    2    5  0.714
internal/repl                                                               1    5  0.833
internal/bootstrap                                                          1   15  0.938
internal/extensions/all                                                     1   14  0.933
cmd/scheme                                                                  0    7  1.000
```

## Line Counts (non-test .go files)

| Package | Files | Lines | Top contributor |
|---------|-------|-------|-----------------|
| machine/ | 73 | 14,383 | compile_time_continuation.go (2,376) |
| values/ | 52 | 10,583 | big_complex.go (715) |
| internal/match/ | 18 | 2,415 | syntax_adapter.go (854) |
| **Total** | **143** | **27,381** | |

---

## Findings

### 1. MachineContext: 22 Fields, ~4 Orthogonal Concerns

**Principle**: State Tightness
**Where**: `machine/machine_context.go:56-70`
**Theory**: Product type explosion (Pierce, *TAPL* §11). `MachineContext` is a struct of ~14 fields (embedded `vmState` + direct). The representable state space is the Cartesian product of all field domains. Most combinations are meaningless.

**Current state**: `MachineContext` combines four orthogonal concerns:

| Concern | Fields |
|---------|--------|
| Core VM execution | vmState (env, template, value, evals, pc, callDepth), cont, maxCallDepth |
| Sub-context tracking | parentMC, escapeCont, barrierValid |
| Execution context add-ons | expanderCtx, exceptionHandler, debugger, thread, syntaxCase, counters |
| Thread identity | vmState.threadID, thread |

**Note**: The `pendingEscape` field was removed in a prior refactor. The escape tracking is now simpler: `parentMC` tracks the parent context for sub-contexts, and `escapeCont` tracks where to continue after a sub-context completes. `barrierValid` was added for `with-continuation-barrier` support.

**Proposed direction**: The escape state is now cleaner than when this analysis was written. The remaining fields (`parentMC`, `escapeCont`) serve distinct, well-defined roles. No extraction needed.

**Impact**: Low — the prior refactor already addressed the main concern.

---

### 2. Thread.RunFunc / Thread.CleanupFunc: Dependency Inversion via Function Injection

**Principle**: Dependency Minimization
**Where**: `values/thread.go` (`RunFunc`/`CleanupFunc` fields)
**Theory**: Dependency Inversion Principle (Martin) implemented via first-class functions. `Thread` lives in `values/` (I=0.00) but needs `machine/` (I=0.32) to run thunks. Rather than importing `machine/`, `Thread` holds `RunFunc func(ctx, thunk) (Value, error)` and `CleanupFunc func()` — function-typed fields injected at construction time.

**Current state**: This is a correct application of DIP. `values/` remains a pure leaf, and the injection is done at thread creation time in `machine/`.

**Problem**: `RunFunc` and `CleanupFunc` are implicitly mandatory (the thread panics or silently does nothing if they're nil), but this isn't enforced by the type. A `Thread` with nil `RunFunc` is representable but invalid.

In Hoare triple terms: `{t.RunFunc != nil ∧ t.CleanupFunc != nil} t.Start() {thread runs correctly}`. The precondition is a human obligation, not a compiler obligation.

**Proposed direction**: Fine as-is for a single injection site. If injection grows beyond 2 functions, consider an interface.

**Impact**: No change needed. Deliberate design decision.

---

### 3. OperationBase + 34 Operation Types: Successful Embedding Refactor

**Principle**: Composability
**Where**: `machine/operation_helpers.go:100+`, `machine/operation_*.go`
**Theory**: Template Method pattern (GoF) via Go struct embedding. The base provides `SchemeString()`, `IsVoid()`, `String()`, and each operation overrides `Apply()` and `EqualTo()`.

**Current state**: 34 operation types all embed `OperationBase`. Four generic helper functions (`sameType`, `fieldMatches`, `fieldMethodMatches`, `sliceMatches`) eliminate repetitive `EqualTo` boilerplate. The generics are well-constrained.

The `EqualTo` helpers form a family of morphisms parameterized by field accessor — a clean factoring. The `sameType` ← `fieldMatches` ← `fieldMethodMatches` chain progresses from zero to one field to method-compared field — a refinement tower.

**Problem**: Minor — operations with fields override `SchemeString()` to include the field value, but the override decision is implicit. No compile-time signal that an operation with fields forgot to override.

**Impact**: Well-executed refactor. No action needed.

---

### 4. Operation Interface Embeds values.Value: Deliberate Subtyping Choice

**Principle**: Dependency Minimization / Composability
**Where**: `machine/operation.go:28-30` (Operation), `machine/operation.go:35-38` (InlinedOperation)
**Theory**: Interface embedding as subtyping (Liskov & Wing, 1994).

**Current state** (post integer-opcode-dispatch refactor):
```go
type Operation interface {
    values.Value
}

type InlinedOperation interface {
    Operation
    Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error)
}
```

The `Operation` base interface now only embeds `values.Value` (for operations inlined into the `Run()` switch). `InlinedOperation` extends it with `Apply` for complex operations dispatched via the `OpComplex` side table.

**Assessment**: The integer-opcode-dispatch refactor split the interface appropriately. Simple operations (Push, Pop, Branch, etc.) implement only `Operation` and have their logic inlined in the `Run()` switch. Complex operations (ForeignFunctionCall, SyntaxRulesTransform, etc.) implement `InlinedOperation` with their own `Apply`.

**Proposed direction**: Leave as-is. The interface split is clean.

**Impact**: None needed.

---

### 5. captureContext: Recursive Tree Without Typed Keys

**Principle**: State Tightness
**Where**: `internal/match/syntax_compiler.go:73-76`

**Current state**:
```go
type captureContext struct {
    children map[int][]*captureContext
    bindings map[string]syntax.SyntaxValue
}
```

**Problem**: The `children` map uses `int` keys (ellipsis IDs) — the key space is `int` (2^64 states) but the semantic space is "small non-negative integers assigned sequentially by the compiler" (effectively 0..N). Type precision: N/2^64 ≈ 0%.

**Proposed direction**: `type ellipsisID int` prevents accidental key confusion. The unbounded recursion is inherent to the domain (nested ellipsis) and doesn't need a depth bound.

**Impact**: Low. A type alias would add clarity without changing behavior.

---

### 6. ThreadState: Correct Sum Type, Missing Transition Guard

**Principle**: State Tightness
**Where**: `values/thread.go:77-85`

**Current state**: `ThreadState` is `int` with 4 named constants. State machine:
```
ThreadNew → ThreadRunnable → ThreadBlocked ⇌ ThreadRunnable → ThreadTerminated
```

Type precision: 4/2^64 ≈ 0% (Go typed-int enum with unbounded underlying type).

**Problem**: Transitions are enforced only by `Thread` methods — no compile-time prevention of illegal transitions like `ThreadTerminated → ThreadNew`. This is a Go language limitation, not a design flaw.

**Proposed direction**: Current approach (mutex + runtime state checks) is idiomatic Go. No change needed.

**Impact**: None. Current design is correct for Go.

---

### 7. IsVoid: 46 Implementations, All Return False (Except One)

**Principle**: Composability
**Where**: All 46 `IsVoid()` methods across `values/`
**Theory**: Degenerate predicate — a boolean method that returns the same value for 45/46 types. In information-theoretic terms, `IsVoid()` carries log₂(46/45) ≈ 0.03 bits of information per call on non-void values.

**Current state**: Every pointer-receiver `IsVoid()` uses the nil-receiver convention: `p == nil` returns true (nil pointer = void), otherwise false. `OperationBase.IsVoid()` deviates — value receiver, always returns false (documented as intentional).

**Problem**: The 46 implementations are hand-unrolled but identical (`return p == nil`). Go's interface dispatch requires each concrete type to have its own method. The `OperationBase` embedding handles it for operations. For `values/`, the nil-receiver convention is documented and consistent.

**Proposed direction**: Accept as a Go tax. No change needed.

**Impact**: None.

---

## Opportunities

### Opportunity: EscapeState Extraction — SUPERSEDED

The `pendingEscape` field was removed in a prior refactor. The escape tracking is now two fields (`parentMC`, `escapeCont`) which serve distinct roles: `parentMC` tracks the parent context for sub-contexts, and `escapeCont` tracks where to continue after a sub-context completes. `barrierValid` (added for `with-continuation-barrier`) is orthogonal. The original 3-pointer state machine concern no longer applies.

---

## Summary

### State-Space Summary

| Type | Representable States | Semantic States | Precision |
|------|---------------------|-----------------|-----------|
| `MachineContext` (sub-context fields) | 4 (2 nullable pointers) | 2 | 50% |
| `ThreadState` | 2^64 (Go int) | 4 | ≈ 0% (language limitation) |
| `captureContext.children` key | 2^64 (int) | 0..N (small) | ≈ 0% |
| `Thread.RunFunc` | nil \| func | func only | 50% |

### Dependency Health

- 103 internal dependency edges across 33 packages
- 0 cycles (clean DAG)
- 0 SDP violations
- Instability range: [0.00, 1.00]
- Critical path: `values/ (0.00) → environment/ (0.13) → machine/ (0.32) → registry/ (0.20) → registry/core/ (0.78) → extensions (0.93)`

### Top 2 Highest-Impact Changes (Ranked)

1. **EscapeState grouping** (Finding #1) — SUPERSEDED: `pendingEscape` was removed; the escape tracking is now simpler (2 fields, not 3). No further grouping needed.

2. **ellipsisID type alias** (Finding #5) — `type ellipsisID int` prevents key-type confusion in `captureContext.children` and `SyntaxCompiler.ellipsisVars`. Impact: one-line change, improved documentation-in-type.

### Overall Assessment

This is a structurally sound codebase. The dependency graph is a clean DAG with no cycles and no SDP violations. `values/` is a textbook stable foundation (I=0.00, 25 dependents). The recent OperationBase refactor demonstrates good instincts — factoring repetitive patterns into generic helpers. The main area for tightening is implicit state machines in the VM. These are refinement opportunities, not structural problems.

Strong design decisions: DIP via function injection for `Thread`, clean layered architecture respecting stability ordering, consistent error convention (sentinel + wrap). The 27,000+ lines across the three core packages are well-organized for a project of this complexity.
