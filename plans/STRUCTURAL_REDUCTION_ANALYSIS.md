# Wile Codebase — Structural Reduction Analysis

**Date:** 2026-02-17
**Type:** Full codebase review — dependency minimization, state tightness, composability, semantic contracts

## Package Dependency Map

```
                             ┌──────────────────────────┐
                             │        values/            │
                             │  Ca=151  Ce=0   I=0.00   │
                             │  (maximally stable leaf)  │
                             └──────────────┬───────────┘
                                            ▲
                    ┌───────────────────────┬┴───────────────────────┐
                    │                       │                        │
          ┌─────────┴──────────┐  ┌────────┴──────────┐  ┌─────────┴─────────┐
          │  internal/syntax   │  │   environment/     │  │ registry/helpers  │
          │  Ca=67 Ce=1 I=.01  │  │  Ca=53 Ce=2 I=.03 │  │ Ca=26 Ce=2 I=.07 │
          │  (very stable)     │  │  (very stable)     │  │ (stable)          │
          └───────┬────────────┘  └────────┬───────────┘  └──────────┬────────┘
                  ▲                         ▲                         ▲
     ┌────────────┤            ┌───────────┤              ┌──────────┘
     │            │            │           │              │
┌────┴────┐ ┌─────┴──────┐ ┌──┴───────┐ ┌─┴──────────┐ ┌┴───────────┐
│tokenizer│ │  parser    │ │  match   │ │  validate  │ │  registry  │
│Ca=4 I=33│ │Ca=11 I=.26 │ │Ca=4 I=42│ │Ca=3 I=.57  │ │Ca=13 I=.18 │
└─────────┘ └────────────┘ └─────────┘ └────────────┘ └──────┬──────┘
                  ▲                          ▲                 ▲
                  │                          │                 │
                  └─────────────┬────────────┘                 │
                                │                              │
                      ┌─────────┴────────────┐                 │
                      │      machine/        │─────────────────┘
                      │  Ca=48  Ce=8  I=.14  │
                      │  (stable hub)        │
                      └──────────┬───────────┘
                                 ▲
        ┌────────────────────────┤
        │                        │
┌───────┴────────┐    ┌──────────┴──────────┐
│ registry/core  │    │    extensions/*     │
│ Ca=2  Ce=6     │    │  (6 packages)       │
│ I=0.75         │    │  I=0.50-0.80        │
│ (unstable)     │    │  (unstable leaf)    │
└────────────────┘    └─────────────────────┘
        ▲                        ▲
        │                        │
        └────────────┬───────────┘
                     │
          ┌──────────┴─────────────┐
          │  internal/bootstrap    │
          │  (wiring, unstable)    │
          └──────────┬─────────────┘
                     ▲
          ┌──────────┴─────────────┐        ┌─────────────┐
          │      root (wile/)      │        │   runtime    │
          │  (public API, stable)  │        │ Ca=2 I=.71   │
          └────────────────────────┘        └─────────────┘
```

### Dependency Health Summary

| Package | Ca | Ce | I | Role |
|---------|----|----|---|------|
| `values` | 151 | 0 | 0.00 | Leaf: all types, zero deps. **Maximally stable.** |
| `internal/syntax` | 67 | 1 | 0.01 | Near-leaf: depends only on `values`. Very stable. |
| `environment` | 53 | 2 | 0.03 | Depends on `values`, `internal/syntax`. Very stable. |
| `registry/helpers` | 26 | 2 | 0.07 | Depends on `machine`, `values`. Stable utility. |
| `machine` | 48 | 8 | 0.14 | Central hub: 8 outgoing, 48 incoming. Stable despite fan-out. |
| `registry` | 13 | 3 | 0.18 | Thin facade: `environment` + `machine` + `values`. |
| `internal/parser` | 11 | 4 | 0.26 | Mid-stability, used by many internal packages. |
| `internal/tokenizer` | 4 | 2 | 0.33 | Relatively isolated. |
| `internal/match` | 4 | 3 | 0.42 | Pattern matcher: `environment` + `syntax` + `values`. |
| `internal/validate` | 3 | 4 | 0.57 | Consumes more than it provides — expected for a validation phase. |
| `runtime` | 2 | 5 | 0.71 | Unstable: convenience wrapper with few consumers. |
| `registry/core` | 2 | 6 | 0.75 | Unstable: primitives register into stable packages. Correct SDP direction. |

**ADP check (Acyclic Dependencies Principle):** No package-level import cycles. The dependency graph is a clean DAG. `internal/forms` serves as the cycle-breaker between `validate` and `machine` — it's a shared registry of function pointers typed as `any`, allowing both packages to register/lookup without importing each other. The DAG structure is sound.

---

## Findings

### 1. OperationBase: Dead Field `goName`

**Principle**: State Tightness
**Where**: `machine/operation_helpers.go:100-102`
**Theory**: **Dead field detection** (pigeonhole). If M fields exist and N < M are used, M-N are waste. `goName` is set by only 10 of 35 operation types (via `NewOperationBaseWithGoName`), while 25 types call `NewOperationBase` which leaves `goName` as `""`.

**Current state**: `OperationBase` has two `string` fields: `opName` (always set) and `goName` (set by 10 types, empty for 25). The `String()` method checks `goName != ""` and falls back to `opName`.

```go
type OperationBase struct {
    opName string   // always set — 35/35 types
    goName string   // set by 10/35 types, empty for 25
}
```

**Problem**: Every operation struct pays 16 bytes (string header) for a field that's empty 71% of the time. With ~35 operation types, each instantiated many times during compilation, this is wasted memory across every `NativeTemplate`. More importantly, it splits the "name" concept into two redundant representations.

**Type precision**: `opName × goName` = ∞ × ∞ states. But semantically there are only two cases: "has both names" or "has one name". A single string field with an optional override would be precise.

**Proposed direction**: Eliminate `goName`. The 10 types that use it (all syntax-case and build-syntax ops) should use `opName` for both Scheme and Go representations, or derive one from the other. The `String()` method becomes trivial: `return p.opName`.

**Impact**: 16 bytes saved per operation instance. More importantly: one fewer field to maintain, one fewer nil-path in `String()`.

---

### 2. `internal/forms`: Type Erasure via `any`

**Principle**: State Tightness / Dependency
**Where**: `internal/forms/form_spec.go:32-41`
**Theory**: **Type precision ≈ 0%**. `ValidatorFunc func(ctx, any, any, any) any` has a state space of (all possible Go values)³ — effectively infinite — while the semantic domain is exactly `(*EnvironmentFrame, *SyntaxPair, *ValidationResult) → ValidatedExpr`. This is **boolean blindness** (Harper) applied to function signatures: the type discards all information about what the function operates on.

**Current state**:
```go
type ValidatorFunc func(ctx context.Context, env any, pair any, result any) any
type CompilerFunc  func(ctc any, ctctx any, expr any) error
```

**Problem**: This exists to break the `validate` ↔ `machine` import cycle. It succeeds at that goal. But the cost is zero compile-time type safety — every call site must perform runtime type assertions. The **Parnas criterion** says modules should hide design decisions likely to change; here the "hidden" decision is the types themselves, which are among the most stable things in the codebase.

**Proposed direction**: This is a known trade-off that's working. A future alternative (if desired) would be Go generics or a dedicated interface in a shared types package. But the `forms` package is 98 lines and stable — the cost of the `any` escape hatch is contained. **Not high-priority.**

**Impact**: Documentation value only — call out the runtime assertion contract in the type docs.

---

### 3. `SyntaxSymbol.ResolvedBinding any` — Escaped Type

**Principle**: State Tightness
**Where**: `internal/syntax/syntax_symbol.go:42`
**Theory**: Same pattern as finding #2. `ResolvedBinding` is documented as `*environment.GlobalIndex` but typed as `any` to avoid a circular import (`internal/syntax` → `environment` is fine, but `environment` already imports `internal/syntax`). Wait — checking the imports: `environment` imports `internal/syntax`, and `internal/syntax` imports `values`. So `internal/syntax` cannot import `environment` without creating a cycle.

**Current state**:
```go
// Type: *environment.GlobalIndex (stored as any to avoid circular import).
ResolvedBinding any
```

**Problem**: `any` representable states = ∞. Semantic states = `{nil, *GlobalIndex}` = 2. Type precision ≈ 0%. Every consumer must assert `rb.(*environment.GlobalIndex)`.

**Proposed direction**: Define a `ResolvedBinding` interface in `internal/syntax` (or `values`) with the single method that consumers actually call. `GlobalIndex` already implements `SchemeString()`. If consumers only need the value, an interface like `type ResolvedRef interface { SchemeString() string }` would restore type safety without the cycle. However, this requires auditing all use sites to determine the minimal interface.

**Impact**: Moderate. Wrong type assertion is a runtime panic in the compiler — low-frequency but high-severity if it happens.

---

### 4. `TopLevelEnvironment.libraryRegistry any` — Escaped Type

**Principle**: State Tightness
**Where**: `environment/top_level_environment.go:76`
**Theory**: Same circular-import escape as #3. `libraryRegistry` is `*machine.LibraryRegistry` but typed `any` because `environment` cannot import `machine`.

**Current state**: There's a TODO: `// TODO: consider defining an interface for library registries.`

**Problem**: The TODO is correct. `any` representable states = ∞. Semantic states = `{nil, *LibraryRegistry}` = 2.

**Proposed direction**: Define `LibraryRegistry` interface in `environment` (or `values`). This is the **Dependency Inversion Principle** (Martin): the stable package (`environment`, I=0.03) should own the interface, and the unstable package (`machine`, I=0.14) should implement it. The interface likely needs only `Lookup(name) -> library` and `Register(name, library)`.

**Impact**: Eliminates a class of "stored wrong type in libraryRegistry" bugs. Enables compile-time checking of the contract between `environment` and `machine`.

---

### 5. Operation Struct Proliferation — 35 Types × ~4 Methods Each

**Principle**: Composability
**Where**: `machine/operation_*.go` (31 non-test files, ~2500 lines)
**Theory**: **Hand-unrolled structure**. Each operation type is a struct with the same embedded `OperationBase`, the same `SchemeString()` pattern, the same `EqualTo()` pattern (via generics in `operation_helpers.go` — already factored), and a unique `Apply()`. The 20+ simple operations (zero or one field) follow an identical structural template.

**Current state**: 35 operation types across 31 files. Each file: license header (13 lines) + import block (~4 lines) + struct definition (~4 lines) + constructor (~5 lines) + `SchemeString()` (~3 lines) + `EqualTo()` (~3 lines) + `Apply()` (varies, 5-50 lines) = minimum ~37 lines per operation.

Zero-field operations like `Push`, `Pop`, `Pull` contribute ~42 lines each for behavior that's 3 lines of `Apply()` logic.

**Problem**: The **Parnas criterion** asks: what's the design decision being hidden? For simple ops, it's the `Apply` body — 3-5 lines. The remaining ~35 lines per file are structural boilerplate. This is the transition from **enumeration to induction** — 20+ structurally identical blocks that differ only in their `Apply` body.

This pattern was addressed by the integer-opcode-dispatch refactor (now completed and merged). Simple ops are inlined in the `Run()` switch on `OpCode`, each case 3-5 lines. The remaining ~13 complex operations keep their struct-based form (as `InlinedOperation`, dispatched via `OpComplex` side table).

**Status**: **COMPLETED.** The `Instruction{Op OpCode, Arg int32}` encoding with inlined switch dispatch is now the production architecture. The `Operation` base interface embeds only `values.Value`; complex ops implement `InlinedOperation` with `Apply`.

**Impact**: ~700 lines of boilerplate eliminated. Cache locality improvement from contiguous `[]Instruction`. The composability gain is that adding a new simple operation becomes adding a `case` to the switch, not creating a new file with 40 lines of boilerplate.

---

### 6. `OperationBase.opName` Strings Are Compile-Time Constants Used at Runtime

**Principle**: State Tightness
**Where**: `machine/operation_*.go` constructors, `machine/operation_helpers.go:107`
**Theory**: **Information-theoretic waste**. Each operation instance stores a `string` (`opName`) that is determined entirely by the operation's Go type. For `OperationPush`, `opName` is always `"machine-operation-push"`. This is **invariant information** embedded as variable state. The type *is* the name — storing it separately is redundant.

**Current state**: 35 string values, each allocated per-instance, each derivable from the concrete type via reflection or a type-to-name map.

```go
// Every OperationPush instance stores the same string:
OperationBase: NewOperationBase("machine-operation-push")
```

**Problem**: With N instances of `OperationPush` across all templates, N copies of the same `OperationBase{opName: "machine-operation-push"}` exist. Go interns string literals, so the underlying bytes are shared, but each `OperationBase` struct still occupies 32 bytes (two string headers) per instance.

**Proposed direction**: The integer-opcode-dispatch refactor (now completed) resolved the per-instance name overhead for inlined ops. For the remaining complex operations, consider making `opName` a method on `OperationBase` that looks up a package-level `map[reflect.Type]string`, or simply use a package-level `var` per type.

**Impact**: 32 bytes × instances saved. More importantly, it eliminates the *possibility* of two instances of the same type having different names (currently representable but never valid).

---

### 7. `vmState` Shared Between `MachineContext` and `MachineContinuation`

**Principle**: Composability / State Tightness
**Where**: `machine/vm_state.go:62-86`
**Theory**: `vmState` is a **product type** embedded in both `MachineContext` and `MachineContinuation`. Both types share the same fields but use them differently. This is good factoring — it extracts the common state that must be saved/restored across continuation boundaries.

**Current state**:
```go
type vmState struct {
    env, template, singleValue, multiValues, evals,
    pc, windingStack, promptTag, threadID, callDepth
}
```

11 fields. `MachineContext` adds 9 more fields. `MachineContinuation` adds 2 (`parent`, `promptHandler`).

**Assessment**: This is **correct factoring**. The `vmState` struct is the minimal state that must be snapshot for a continuation and restored on return. The additional fields on `MachineContext` (debugger, expander context, exception handler, escape tracking, counters) are execution infrastructure that doesn't participate in continuation semantics. The `vmState` boundary is the continuation save/restore contract.

**Semantic contract** (Hoare triple):
```
{mc.vmState = S}   SaveContinuation   {cont.vmState = S ∧ mc unchanged}
{cont.vmState = S} RestoreContinuation {mc.vmState = S}
```

**No action needed.** This is well-designed. Documenting the contract (as above) would be valuable inline.

---

### 8. `MachineContext` — `singleValue` / `multiValues` Dual Value Register

**Principle**: State Tightness
**Where**: `machine/vm_state.go:65-66`
**Theory**: **Product type encoding a sum type.** The value register is either one value (`singleValue`) or multiple values (`multiValues`), never both simultaneously.

**Current state**:
```go
singleValue  values.Value    // fast path: one value
multiValues  MultipleValues  // rare path: R7RS values/call-with-values
```

`MultipleValues` is `[]values.Value`. When there's a single value, `singleValue` holds it and `multiValues` is nil. When there are multiple values, `multiValues` holds them and `singleValue` holds `multiValues[0]`.

**Type precision**: The representable state is `Value × []Value` = ∞². The semantic state is `Value + []Value` (union). Precision ≈ 0% in theory, but in practice the `SetValue`/`SetValues`/`Value`/`Values` methods enforce the invariant.

**Assessment**: This is a **deliberate performance optimization** — the common case (single value) avoids allocating a `[]Value` slice. The dual-field encoding trades type precision for allocation reduction. Given that the value register is read/written on *every* VM instruction, this trade-off is justified.

**No action needed.** The accessor methods (`SetValue`, `Value`, `Values`) correctly maintain the invariant. The only risk is direct field access bypassing the accessors — which would be caught by review.

---

### 9. Extension Registration Pattern — 6× Identical Structure

**Principle**: Composability
**Where**: `extensions/*/register.go` (6 files, ~100 lines each)
**Theory**: **Hand-unrolled structure across packages.** Each extension package follows the exact same pattern:

```go
var Extension = registry.NewExtension("name", AddToRegistry)
var Builder = registry.NewRegistryBuilder(addPrimitives)
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
    r.AddPrimitives([]registry.PrimitiveSpec{ ... })
    return nil
}
```

**Assessment**: This is borderline. The pattern *is* repetitive, but each extension's `addPrimitives` function has genuinely different content (different primitive specs). The boilerplate is ~5 lines per package. The `registry.NewExtension` + `registry.NewRegistryBuilder` already factor out the common structure.

**No action needed.** The boilerplate is minimal and the pattern is clear. Trying to factor it further (e.g., code generation) would add complexity for negligible gain.

---

### 10. Error Type Proliferation — 10 Error Structs

**Principle**: Composability / State Tightness
**Where**: Across `values/`, `machine/`, `internal/*/`, root

**Current error types**:
| Type | Package | Purpose |
|------|---------|---------|
| `StaticError` | values | Sentinel errors (`errors.Is`) |
| `ForeignError` | values | Wrapped Go errors with Scheme context |
| `ForeignFileError` | values | File-related foreign errors |
| `ForeignReadError` | values | Read-related foreign errors |
| `NativeError` | values | R7RS `error` objects (condition type) |
| `SchemeError` | machine | Runtime error with source + stack trace |
| `CompilationError` | root | Compilation error with source |
| `RuntimeError` | root | Runtime error wrapping `SchemeError` |
| `ValidationError` | validate | Validation phase error |
| `TokenizerError` | tokenizer | Tokenizer phase error |
| `ParserError` | parser | Parser phase error |

**Theory**: Each phase of the pipeline has its own error type. This is the **phase-separation pattern** — errors at different phases carry different metadata (tokenizer errors have position, compiler errors have source context, runtime errors have stack traces). Phase-specific types enable `errors.As` dispatch.

**Assessment**: The pipeline error types (`TokenizerError`, `ParserError`, `ValidationError`, `CompilationError`, `RuntimeError`) form a natural progression. Each phase adds information. The `values` error types (`StaticError`, `ForeignError`, `NativeError`) serve different purposes (sentinels, wrapping, R7RS conditions).

`ForeignFileError` and `ForeignReadError` are specializations of `ForeignError` — these could potentially be merged into `ForeignError` with a category tag, but they're small and rarely touched.

**Proposed direction**: Low-priority. The current structure follows the two-layer convention (`values.NewStaticError` for sentinels, `values.WrapForeignErrorf` at return sites) consistently. If anything, the root-level `CompilationError` and `RuntimeError` could be consolidated — they're thin wrappers.

**Impact**: Minor. The error type hierarchy is already factored around the pipeline phases.

---

### 11. `CompileTimeCallContext` — Clean Product Type

**Principle**: State Tightness
**Where**: `machine/compile_time_call_context.go:60-64`

```go
type CompileTimeCallContext struct {
    ctx          context.Context
    inTail       bool
    inExpression bool
}
```

**Assessment**: 3 fields, all necessary, all used. `context.Context × bool × bool` = ∞ × 2 × 2 = ∞ × 4. The two booleans are independent (a form can be in tail position regardless of expression/definition mode), so this is a correctly minimal product type. No impossible state combinations.

**No action needed.** This is a well-designed type.

---

### 12. `Pair [2]Value` and `ArrayList []Value` — Dual List Representation

**Principle**: Composability
**Where**: `values/pair.go:32`, `values/array_list.go:51`
**Theory**: **Sum type** — a Scheme list is either a chain of `Pair` cells (O(1) cons, O(n) index) or a contiguous `ArrayList` (O(1) index, O(n) cons). The `Tuple` interface unifies read-only operations. Write operations (`SetCar`, `SetCdr`) are `Pair`-only.

**Assessment**: The `Tuple` interface is a correct **projection** — it exposes the read-only contract that all list consumers need. The CLAUDE.md correctly documents when to use `Tuple` vs `*Pair`. The dual representation is semantically necessary (cons cells vs contiguous arrays have different performance characteristics for different workloads).

**No action needed.** The `Tuple` interface is the right abstraction.

---

## Opportunities

### Opportunity: Opcode-Based Operation Encoding

**Replaces**: 20+ simple operation structs (Push, Pop, LoadLocal, Branch, etc.)
**Core operation**: `Apply(mc) → mc` with behavior determined by opcode enum + immediate operand
**Algebraic structure**: **Enumeration to induction** — 20+ structurally identical types → a single dispatched type with a discriminant
**Status**: **COMPLETED.** The `Instruction{Op OpCode, Arg int32}` encoding with `OpComplex` side table dispatch is now implemented.
**Reuse sites**: Peephole optimizer (Phase 5) operates on the instruction stream; integer opcodes are simpler to pattern-match than interface types.

---

### Opportunity: Interface for `libraryRegistry`

**Replaces**: `any` field + runtime type assertions in `environment/top_level_environment.go`
**Core operation**: Library lookup by name, library registration
**Algebraic structure**: **Map** (name → library). The interface is a **functor** from names to libraries.
**Proposed shape**:
```go
// In environment/ or values/
type LibraryRegistry interface {
    LookupLibrary(name []string) (any, bool)
    RegisterLibrary(name []string, lib any) error
}
```
**Reuse sites**: Any future extension that needs to interact with the library system gets compile-time type safety instead of casting.

---

### Opportunity: OperationBase Name as Type-Level Constant

**Replaces**: Per-instance `opName string` field in 35 operation types
**Core operation**: Mapping concrete type → Scheme display name
**Algebraic structure**: **Bijection** from types to names. Currently encoded as a value (string field) rather than a type-level property.
**Proposed shape**: With integer opcodes, the name becomes `OpCode.String()` — a method on an enum, not a field on an instance. For remaining complex ops, a package-level `var` or `const` per type.
**Reuse sites**: Bytecode disassembler, debugger display, error messages.

---

## Closing

### State-Space Summary

Key types analyzed:

| Type | Representable states | Semantic states | Precision |
|------|---------------------|-----------------|-----------|
| `OperationBase` | ∞² (two strings) | 35 fixed name pairs | ≈ 0% |
| `vmState.singleValue/multiValues` | ∞² (Value × []Value) | ∞ + ∞ (sum, not product) | ≈ 50% (deliberate) |
| `SyntaxSymbol.ResolvedBinding` | ∞ (any) | 2 ({nil, *GlobalIndex}) | ≈ 0% |
| `TopLevelEnvironment.libraryRegistry` | ∞ (any) | 2 ({nil, *LibraryRegistry}) | ≈ 0% |
| `CompileTimeCallContext` | ∞ × 4 | ∞ × 4 | 100% |
| `forms.ValidatorFunc` args | ∞³ | 1 valid type triple | ≈ 0% |
| `BindingType` | ∞ (int) | 4 values | ≈ 0% |

The three `any`-typed fields (`libraryRegistry`, `ResolvedBinding`, `forms` function params) account for the worst precision ratios. These are all circular-import workarounds.

### Dependency Count

14 core packages. 0 import cycles (clean DAG). No SDP violations — unstable packages (`registry/core`, `extensions/*`, `runtime`) depend on stable packages (`values`, `environment`, `machine`). The dependency direction is consistently correct.

The `internal/forms` package exists solely to break a `validate` ↔ `machine` cycle. It's 98 lines and serves its purpose, though at the cost of type erasure.

### Top 3 Highest-Impact Changes

1. **Integer Opcode Dispatch** (**COMPLETED**): Eliminated operation boilerplate, improved cache locality. Finding #5, #6. Now uses `Instruction{Op OpCode, Arg int32}` with 17 inlined ops in the `Run()` switch and 13 complex ops via `OpComplex` side table.

2. **`LibraryRegistry` interface in `environment`**: Restores compile-time type safety for the library system contract. Small change, eliminates an `any` field, follows Dependency Inversion. Finding #4.

3. **Eliminate `OperationBase.goName`**: 10/35 types use it, 25 don't. Merge into `opName` or make the 10 types override `String()` directly. Removes a dead field from 71% of operation instances. Finding #1.
