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
**Where**: `machine/machine_context.go:61-75`
**Theory**: Product type explosion (Pierce, *TAPL* §11). `MachineContext` is a struct of 22 fields (9 from embedded `vmState` + 13 direct). The representable state space is the Cartesian product of all field domains. Most combinations are meaningless.

**Current state**: `MachineContext` combines four orthogonal concerns:

| Concern | Fields |
|---------|--------|
| Core VM execution | vmState (env, template, value, evals, pc, callDepth), cont, maxCallDepth |
| Continuation escape tracking | parentMC, pendingEscape, escapeCont |
| Execution context add-ons | expanderCtx, exceptionHandler, debugger, thread, syntaxCase, counters |
| Thread identity | vmState.threadID, thread |

**Problem**: The continuation escape fields (`parentMC`, `pendingEscape`, `escapeCont`) form a micro-state machine embedded inside the struct without explicit states. `pendingEscape` is only used in 2 files (18 references across `machine_context.go` and one test). These fields are temporal — they describe a lifecycle phase (normal execution → escaping → resumed) — but the phase is encoded implicitly across three pointer fields rather than as a discriminated state.

In type-algebraic terms: `parentMC *MachineContext × pendingEscape *MachineContinuation × escapeCont *MachineContinuation` represents `(ptr|nil)³ = 8 states`. Semantically there are ~3 states: *no escape in progress*, *escaping*, *resumed after escape*. Type precision: 3/8 = 37.5%.

**Proposed direction**: Extract a `type escapeState struct { parent *MachineContext; pending, escape *MachineContinuation }` or consider an explicit `escapePhase` enum to constrain transitions. Even just grouping these fields with a comment block would make the state machine visible.

**Impact**: Reduces cognitive load when reading the VM loop. Clarifies invariants for call/cc escape handling — currently the most complex control flow path in the VM.

---

### 2. Thread.RunFunc / Thread.CleanupFunc: Dependency Inversion via Function Injection

**Principle**: Dependency Minimization
**Where**: `values/thread.go:124-133`
**Theory**: Dependency Inversion Principle (Martin) implemented via first-class functions. `Thread` lives in `values/` (I=0.00) but needs `machine/` (I=0.32) to run thunks. Rather than importing `machine/`, `Thread` holds `RunFunc func(ctx, thunk) (Value, error)` and `CleanupFunc func()` — function-typed fields injected at construction time.

**Current state**: This is a correct application of DIP. `values/` remains a pure leaf, and the injection is done at thread creation time in `machine/`.

**Problem**: `RunFunc` and `CleanupFunc` are implicitly mandatory (the thread panics or silently does nothing if they're nil), but this isn't enforced by the type. A `Thread` with nil `RunFunc` is representable but invalid.

In Hoare triple terms: `{t.RunFunc != nil ∧ t.CleanupFunc != nil} t.Start() {thread runs correctly}`. The precondition is a human obligation, not a compiler obligation.

**Proposed direction**: Fine as-is for a single injection site. If injection grows beyond 2 functions, consider an interface.

**Impact**: No change needed. Deliberate design decision.

---

### 3. OperationBase + 34 Operation Types: Successful Embedding Refactor

**Principle**: Composability
**Where**: `machine/operation_helpers.go:94-135`, `machine/operation_*.go`
**Theory**: Template Method pattern (GoF) via Go struct embedding. The base provides `SchemeString()`, `IsVoid()`, `String()`, and each operation overrides `Apply()` and `EqualTo()`.

**Current state**: 34 operation types all embed `OperationBase`. Four generic helper functions (`sameType`, `fieldMatches`, `fieldMethodMatches`, `sliceMatches`) eliminate repetitive `EqualTo` boilerplate. The generics are well-constrained.

The `EqualTo` helpers form a family of morphisms parameterized by field accessor — a clean factoring. The `sameType` ← `fieldMatches` ← `fieldMethodMatches` chain progresses from zero to one field to method-compared field — a refinement tower.

**Problem**: Minor — operations with fields override `SchemeString()` to include the field value, but the override decision is implicit. No compile-time signal that an operation with fields forgot to override.

**Impact**: Well-executed refactor. No action needed.

---

### 4. Operation Interface Embeds values.Value: Deliberate Subtyping Choice

**Principle**: Dependency Minimization / Composability
**Where**: `machine/operation.go:22-25`
**Theory**: Interface embedding as subtyping (Liskov & Wing, 1994).

**Current state**:
```go
type Operation interface {
    values.Value
    Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error)
}
```

**Problem**: Operations carry 3 methods (`SchemeString`, `IsVoid`, `EqualTo`) that serve debugging/testing but aren't part of the core `Apply` contract. By the Interface Segregation Principle, this is a product-type interface where a sum might be more appropriate. However, `values.Value` is the project's universal type — everything in the VM is a `Value`. Breaking this would require a parallel type hierarchy.

**Proposed direction**: Leave as-is. The cost (implementing `EqualTo` on every operation) was amortized by the generic helpers. The trade-off is net positive.

**Impact**: None needed.

---

### ~~5. SyntaxMatcher: Parameter Accretion in Expand Methods~~ — RESOLVED

**Resolution**: PR #235 (`refactor/expand-options`) consolidated 6 `Expand*` methods into a single `Expand(template, ExpandOptions)` method with an `ExpandOptions` struct. The recursive core now passes `*ExpandOptions` instead of 5 separate parameters. API surface reduced from 6 public methods to 1.

---

### 6. captureContext: Recursive Tree Without Typed Keys

**Principle**: State Tightness
**Where**: `internal/match/syntax_compiler.go:72-75`

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

### 7. ThreadState: Correct Sum Type, Missing Transition Guard

**Principle**: State Tightness
**Where**: `values/thread.go:76-85`

**Current state**: `ThreadState` is `int` with 4 named constants. State machine:
```
ThreadNew → ThreadRunnable → ThreadBlocked ⇌ ThreadRunnable → ThreadTerminated
```

Type precision: 4/2^64 ≈ 0% (Go typed-int enum with unbounded underlying type).

**Problem**: Transitions are enforced only by `Thread` methods — no compile-time prevention of illegal transitions like `ThreadTerminated → ThreadNew`. This is a Go language limitation, not a design flaw.

**Proposed direction**: Current approach (mutex + runtime state checks) is idiomatic Go. No change needed.

**Impact**: None. Current design is correct for Go.

---

### 8. IsVoid: 46 Implementations, All Return False (Except One)

**Principle**: Composability
**Where**: All 46 `IsVoid()` methods across `values/`
**Theory**: Degenerate predicate — a boolean method that returns the same value for 45/46 types. In information-theoretic terms, `IsVoid()` carries log₂(46/45) ≈ 0.03 bits of information per call on non-void values.

**Current state**: Every pointer-receiver `IsVoid()` uses the nil-receiver convention: `p == nil` returns true (nil pointer = void), otherwise false. `OperationBase.IsVoid()` deviates — value receiver, always returns false (documented as intentional).

**Problem**: The 46 implementations are hand-unrolled but identical (`return p == nil`). Go's interface dispatch requires each concrete type to have its own method. The `OperationBase` embedding handles it for operations. For `values/`, the nil-receiver convention is documented and consistent.

**Proposed direction**: Accept as a Go tax. No change needed.

**Impact**: None.

---

## Opportunities

### ~~Opportunity: ExpandOptions Struct~~ — RESOLVED

**Resolution**: PR #235 (`refactor/expand-options`) implemented this exactly as proposed. Single `Expand(template, ExpandOptions)` method replaced 6 delegation methods.

### Opportunity: EscapeState Extraction

**Replaces**: 3 loose fields on `MachineContext` (`parentMC`, `pendingEscape`, `escapeCont`)
**Core operation**: Track the lifecycle of a continuation escape across sub-context boundaries
**Algebraic structure**: State machine with 3 states (none, escaping, handled). Currently encoded as a product of 3 nullable pointers (8 representable states, 3 semantic).
**Proposed shape**:
```go
type escapeTracking struct {
    parentMC      *MachineContext
    pendingEscape *MachineContinuation
    escapeCont    *MachineContinuation
}
```
**Reuse sites**: `NewSubContext()` (field copy), `RunWithEscapeHandling()` (state transitions), continuation capture in `call/cc`.

---

## Summary

### State-Space Summary

| Type | Representable States | Semantic States | Precision |
|------|---------------------|-----------------|-----------|
| `MachineContext` (escape fields) | 8 (3 nullable pointers) | 3 | 37.5% |
| `ThreadState` | 2^64 (Go int) | 4 | ≈ 0% (language limitation) |
| `captureContext.children` key | 2^64 (int) | 0..N (small) | ≈ 0% |
| `Thread.RunFunc` | nil \| func | func only | 50% |

### Dependency Health

- 103 internal dependency edges across 33 packages
- 0 cycles (clean DAG)
- 0 SDP violations
- Instability range: [0.00, 1.00]
- Critical path: `values/ (0.00) → environment/ (0.13) → machine/ (0.32) → registry/ (0.20) → registry/core/ (0.78) → extensions (0.93)`

### Top 3 Highest-Impact Changes (Ranked)

1. ~~**ExpandOptions struct** (Finding #5)~~ — **RESOLVED** (PR #235). 4 delegation methods removed, recursive core simplified.

2. **EscapeState grouping** (Finding #1) — Raises type precision from 37.5% to ~100% for escape tracking. Makes the call/cc escape lifecycle explicit. Impact: clarifies the most complex control flow in the VM.

3. **ellipsisID type alias** (Finding #6) — `type ellipsisID int` prevents key-type confusion in `captureContext.children` and `SyntaxCompiler.ellipsisVars`. Impact: one-line change, improved documentation-in-type.

### Overall Assessment

This is a structurally sound codebase. The dependency graph is a clean DAG with no cycles and no SDP violations. `values/` is a textbook stable foundation (I=0.00, 25 dependents). The recent OperationBase refactor demonstrates good instincts — factoring repetitive patterns into generic helpers. The main area for tightening is implicit state machines in the VM (parameter accretion in match was resolved via ExpandOptions, PR #235). These are refinement opportunities, not structural problems.

Strong design decisions: DIP via function injection for `Thread`, clean layered architecture respecting stability ordering, consistent error convention (sentinel + wrap). The 27,000+ lines across the three core packages are well-organized for a project of this complexity.
