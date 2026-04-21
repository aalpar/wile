# Type Constraint Extension — Design

**Parent**: `plans/2026-03-26-extension-contracts-impl.md` (Extension Contracts Phase 2+).
**Precedent evidence**: `plans/2026-04-20-axis-b-annotation-bugs.md` §4 (28 return-side gaps), `plans/2026-04-20-paramtypes-annotation-bugs.md` §2.A (~85 param-side gaps), `plans/2026-04-20-paramtypes-axis-c-findings.md` §3 (~25 refinement-type candidates, excluded from scope).
**Status**: Design draft. Implementation deferred to follow-up plan file(s).

---

## 1. Motivation

The four-axis primitive annotation audit (Phases 1–5) surfaced a structural constraint on the current type system: the 28-entry `ValueType` enum in `values/value_type.go:52–81` does not cover the Go types that appear across ~100+ primitive signatures. Affected clusters:

- **Opaque container types**: `*values.Box`, `*values.Promise`, `*values.OpaqueValue`.
- **Concurrency types**: `*values.Thread`, `*values.Mutex`, `*values.Channel`, `*values.ConditionVariable`, `*values.RWMutex`, `*values.WaitGroup`, `*values.Once`, `*values.AtomicBox`, `*values.AtomicInt64`, `*values.Time`.
- **Record infrastructure**: `*values.Record`, `*values.RecordType` (beyond the existing user-defined `RecordTypeConstraint`).
- **Syntax and macro internals**: `*syntax.SyntaxSymbol`, `syntax.SyntaxValue`, `*values.CompileTimeValue`.
- **Error and control plumbing**: `*values.NativeError`, `*machine.PromptTag`, `*machine.ErrorContext`, `*machine.ContinuationMarkSet`.
- **Namespace and process**: `*environment.Namespace`, `*values.SchemeEnvironment`, `*values.Process`.

Primitives parameterized over these types currently declare `TypeAny`, losing both documentation value and (when Phase-2 runtime validation ships) the ability to reject wrong types at the contract boundary.

Additionally, the `TypeConstraint` interface lacks a compile-time subtype operation. The audit analyzer (`audit/wile-axis-b-params.scm:521–540`) hardcodes subtype edges (port hierarchy, pair-vs-list) because the Go-side interface can't answer "does X accept Y?" — the lattice exists implicitly but is not first-class.

This design addresses the vocabulary gap and introduces the missing primary operation.

---

## 2. Design principles

Established through conversation on 2026-04-21. These principles constrain and explain every downstream decision.

### 2.1 Invertibility

A valid `TypeConstraint` must be verifiable by **structural inspection alone**: given a value `v`, determining `v ∈ T` requires only Go-type identity or interface satisfaction — never interpretation of the expression that produced `v`, path-sensitive analysis, or predicate evaluation.

Consequence: refinement types (`exact-non-negative-integer`, `byte-in-range`) are excluded. Proving a return value satisfies a refinement on the callee side is unbounded (cf. `(read)` returning an integer of unknown sign), and the only sound responses are SMT solvers (heavy + incomplete) or runtime checks (duplicates primitive logic).

### 2.2 No duplication with primitive logic

The type system may only carry information that is useful **beyond** what primitives must already check internally. Any type-system claim that primitives end up re-checking is duplication — two authorities for the same claim, which either duplicate runtime work or drift apart.

Consequence: compound types (unions, nullable unions as type-system constructs) are excluded. `Union[T, #f]` would force the framework to check `#f` at the boundary and the primitive to check `#f` again for dispatch. Nominal types don't have this problem because the framework's check replaces the primitive's entry check rather than paralleling the primitive's dispatch.

### 2.3 `Subtype` as the primary operation

The binary predicate a static analyzer consults at a call site is "does the expression's inferred type satisfy this parameter's declared type?" — i.e., `Subtype(inferred, declared)`. This must be decidable, invertible, and O(depth-of-lattice). `Check(Value)` becomes a derived operation (useful for runtime validation of dynamic values).

### 2.4 Orthogonal dimensions for advisory metadata

Information that belongs to the **primitive's contract** but not to the type system's formal reasoning (nullable returns, failure sentinels, refinement hints) lives in separate advisory fields. Consumers: documentation, MCP, IDE tooling, static call-site analysis. Non-consumers: the type-system compiler / Phase-2 runtime validator.

This mirrors `@Nullable` in Java, `may be nil` prose in Go's standard library docstrings, and Julia's docstring conventions.

---

## 3. Non-goals

Explicitly excluded from this design and its follow-ups:

| Excluded | Rationale |
|---|---|
| Refinement types (predicate-based, range-based) | Violates §2.1 invertibility on the return side. |
| Union types (`Union[T1, T2, ...]`) | Violates §2.2 duplication — every compound type forces parallel runtime checks at the contract boundary and inside the primitive. |
| Method dispatch by type | Wile dispatches through the VM via primitive entry. Julia-style multiple dispatch doesn't apply. |
| Parametric generics (`Vector{T}`) | R7RS collections are heterogeneous; no value is asking for element-typed containers. |
| `where`-bounds | Useful only with parametric types. |
| Full Julia-scale type system | Goal is a minimal subset of the Julia-subtype model, not a port. |

The 13 union-bucket entries from 5.C §3 (`angle`, `denominator`, `imag-part`, …) are NOT evidence for union types — they are already correctly declared at abstract supertypes (`TypeNumber`, `TypeReal`). The analyzer's "Union" label reports impl dispatch branches, not a type-system gap. This is fixed by the lattice formalization (Phase 2 of this design), not by introducing unions.

The ~25 refinement-type candidates from 5.E §3.1 (exact-non-negative-integer indices, byte-in-range, Unicode codepoints) are handled by prose in `Doc:` fields, not type-system constructs.

---

## 4. Design

### 4.1 Interface extension: `Subtype` operation

`TypeConstraint` gains one method:

```go
type TypeConstraint interface {
    Name() string
    Description() string
    Check(Value) (any, bool, error)
    Supertype() TypeConstraint    // NEW: direct parent in the lattice; nil for Any itself
}
```

`Subtype(a, b TypeConstraint) bool` is a free function in `values/`:

```go
func Subtype(a, b TypeConstraint) bool {
    for t := a; t != nil; t = t.Supertype() {
        if t == b {
            return true
        }
    }
    return b == AnyType  // Any is top
}
```

Walks the supertype chain from `a` looking for `b`. O(depth). Invertible (no interpretation). Decidable.

Identity comparison (`t == b`) works because types are package-level singletons (see §4.3).

### 4.2 `OpaqueTypeConstraint`

A new `TypeConstraint` implementation parallel to `RecordTypeConstraint`:

```go
type OpaqueTypeConstraint struct {
    goType      reflect.Type
    name        string
    description string
    supertype   TypeConstraint
}

func NewOpaqueType(goType reflect.Type, name, description string, supertype TypeConstraint) *OpaqueTypeConstraint {
    // ...
}

func (p *OpaqueTypeConstraint) Name() string             { return p.name }
func (p *OpaqueTypeConstraint) Description() string      { return p.description }
func (p *OpaqueTypeConstraint) Supertype() TypeConstraint { return p.supertype }
func (p *OpaqueTypeConstraint) Check(v Value) (any, bool, error) {
    if reflect.TypeOf(v).AssignableTo(p.goType) {
        return v, true, nil
    }
    return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
        "expected %s, got %s", p.name, SchemeTypeName(v))
}
```

**Storage decision (confirmed 2026-04-21):** `reflect.Type` field. Simple, trivially printable/serializable, works for any Go type (concrete or interface) without closure machinery. Slight perf cost on `Check` via reflection — acceptable because `Check` is not in the hot path (primitives already gate internally). Closure-based storage is a **roadmap item** (§5.3) for extension authors who need it.

### 4.3 Package singletons

One `OpaqueTypeConstraint` singleton per opaque Go type, defined at package level in `values/` (or the package owning the type). Singletons are the canonical identity — `Subtype` compares via pointer equality.

Initial singletons (Phase 1 of this design):

```go
// values/opaque_types.go (new file)

var (
    BoxType             = NewOpaqueType(reflect.TypeOf((*Box)(nil)), "box", "mutable single-value container", AnyType)
    PromiseType         = NewOpaqueType(reflect.TypeOf((*Promise)(nil)), "promise", "R7RS lazy evaluation", AnyType)
    RecordType_         = NewOpaqueType(reflect.TypeOf((*Record)(nil)), "record", "user-defined record instance", AnyType)
    RecordTypeType      = NewOpaqueType(reflect.TypeOf((*RecordType)(nil)), "record-type", "record type descriptor", AnyType)
    OpaqueValueType     = NewOpaqueType(reflect.TypeOf((*OpaqueValue)(nil)), "opaque", "opaque record instance", AnyType)
    NativeErrorType     = NewOpaqueType(reflect.TypeOf((*NativeError)(nil)), "error-object", "R7RS error object", AnyType)
    CompileTimeValueType = NewOpaqueType(reflect.TypeOf((*CompileTimeValue)(nil)), "compile-time-value", "macro-expansion compile-time value", AnyType)
    SchemeEnvironmentType = NewOpaqueType(reflect.TypeOf((*SchemeEnvironment)(nil)), "environment", "first-class environment", AnyType)
    // Concurrency
    ThreadType          = NewOpaqueType(reflect.TypeOf((*Thread)(nil)), "thread", "SRFI-18 thread", AnyType)
    MutexType           = NewOpaqueType(reflect.TypeOf((*Mutex)(nil)), "mutex", "SRFI-18 mutex", AnyType)
    ChannelType         = NewOpaqueType(reflect.TypeOf((*Channel)(nil)), "channel", "Go channel wrapper", AnyType)
    ConditionVariableType = NewOpaqueType(reflect.TypeOf((*ConditionVariable)(nil)), "condition-variable", "SRFI-18 condition variable", AnyType)
    RWMutexType         = NewOpaqueType(reflect.TypeOf((*RWMutex)(nil)), "rw-mutex", "Go sync.RWMutex wrapper", AnyType)
    WaitGroupType       = NewOpaqueType(reflect.TypeOf((*WaitGroup)(nil)), "wait-group", "Go sync.WaitGroup wrapper", AnyType)
    OnceType            = NewOpaqueType(reflect.TypeOf((*Once)(nil)), "once", "Go sync.Once wrapper", AnyType)
    AtomicBoxType       = NewOpaqueType(reflect.TypeOf((*AtomicBox)(nil)), "atomic", "atomic value container", AnyType)
    AtomicInt64Type     = NewOpaqueType(reflect.TypeOf((*AtomicInt64)(nil)), "atomic-int64", "atomic int64", AnyType)
    TimeType            = NewOpaqueType(reflect.TypeOf((*Time)(nil)), "time", "SRFI-18 time", AnyType)
    ProcessType         = NewOpaqueType(reflect.TypeOf((*Process)(nil)), "process", "spawned subprocess", AnyType)
)

// Types owned by other packages get their singletons in those packages:
// - environment.NamespaceType
// - machine.PromptTagType, ErrorContextType, ContinuationMarkSetType
// - syntax.SyntaxSymbolType, SyntaxValueType
```

Every opaque singleton is **Any-parented** in Phase 1 (per user direction 2026-04-21). Hierarchy formalization (e.g., SyntaxSymbol ⊑ SyntaxValue, Thread ⊑ some abstract ConcurrencyHandle) is roadmap-deferred (§5.2).

### 4.4 RecordTypeConstraint retrofit

The existing `RecordTypeConstraint` becomes a refinement (subtype) of `RecordType_` (the singleton for `*values.Record`):

```go
func (p *RecordTypeConstraint) Supertype() TypeConstraint {
    return RecordType_
}
```

Subtype relation then resolves cleanly:
- `RecordTypeConstraint(point) <: RecordTypeConstraint(shape)` iff `point` inherits from `shape` (walks the `rtd` parent chain via existing `Check` logic — already structural).
- `RecordTypeConstraint(point) <: RecordType_` — always true (all records).
- `RecordType_ <: AnyType` — always true (lattice root).

No change to `RecordTypeConstraint.Check` logic; just one new `Supertype()` method.

### 4.5 ValueType enum retrofit (Phase 1 minimal)

Every existing `ValueType` entry gets `Supertype()` returning `AnyType`. No hierarchy wired up in Phase 1. This preserves existing behavior — `Subtype(TypeInteger, TypeNumber)` returns `false` even though semantically it should be `true`. The analyzer script's hardcoded subtype table continues to compensate.

Phase 2 (§5.2) wires up the real supertype edges.

### 4.6 `AnyType` singleton

The root of the lattice:

```go
var AnyType = &anyTypeImpl{}

type anyTypeImpl struct{}

func (p *anyTypeImpl) Name() string                       { return "any" }
func (p *anyTypeImpl) Description() string                { return "any value" }
func (p *anyTypeImpl) Supertype() TypeConstraint          { return nil }
func (p *anyTypeImpl) Check(v Value) (any, bool, error)   { return v, true, nil }
```

`TypeAny` (the enum entry at `value_type.go:53`) becomes a deprecated alias for `AnyType` — two spellings during migration, then consolidate.

### 4.7 Advisory metadata: `ReturnNullable`

New field on `PrimitiveSpec` in the **advisory cluster** (alongside `Doc`, `Keywords`, `Category`, `ParamNames`), NOT next to `ReturnType`:

```go
type PrimitiveSpec struct {
    Name       string
    ParamCount int
    IsVariadic bool
    Impl       machine.ForeignFunction

    // Enforced (when Extension Contracts Phase 2 lands): type-system fields.
    ParamTypes []values.TypeConstraint
    ReturnType values.TypeConstraint

    // Advisory (documentation + static call-site analysis only — NOT runtime enforced):
    Doc            string
    ParamNames     []string
    Category       string
    Keywords       []string
    ReturnNullable bool  // may return #f as failure sentinel (R7RS idiom)
}
```

Consumers:
- **Documentation**: "returns Number, or #f if input is not a valid number"
- **MCP / IDE**: structured return-shape info
- **Static call-site analyzer**: forces discrimination on use — if caller uses the result as a `Number` without an `if`/`cond` check, flag it
- **NOT Phase-2 runtime validator**: never consulted

If additional sentinel patterns emerge (e.g., `#!eof` for read primitives), they follow the same shape: one orthogonal bool per fixed sentinel (`ReturnMayBeEOF`). The vocabulary is bounded by Scheme's conventional sentinel set (`#f`, `#!eof`, `()`, `#!void`) — not open-ended.

### 4.8 Primitive declaration migration

~85 primitives currently declare `TypeAny` on parameter slots where the impl requires an opaque type. Post-Phase 1, they update to the appropriate singleton:

```go
// Before
{Name: "unbox", ParamCount: 1, Impl: PrimUnbox,
    ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},

// After
{Name: "unbox", ParamCount: 1, Impl: PrimUnbox,
    ParamTypes: []values.TypeConstraint{values.BoxType}, ReturnType: values.AnyType},
```

Categories (see `plans/2026-04-20-paramtypes-annotation-bugs.md` §2.A for the full list):

| Singleton | Affected primitives |
|---|---|
| `BoxType` | `set-box!`, `unbox` |
| `PromiseType` | `make-promise` |
| `RecordType_` | `record-type` |
| `RecordTypeType` | `record-predicate` |
| `OpaqueValueType` | `opaque-tag` |
| `NativeErrorType` | `error-object-irritants`, `error-object-message`, `error-object-source`, `error-object-stack-trace` |
| `ChannelType` | 8 primitives |
| `MutexType` | 6 primitives |
| `ConditionVariableType` | 5 primitives |
| `ThreadType` | 6 primitives |
| `AtomicBoxType` | 4 primitives |
| `RWMutexType` | 6 primitives |
| `WaitGroupType` | 3 primitives |
| `OnceType` | 2 primitives |
| `TimeType` | `time->seconds` |
| `ProcessType` | 5 primitives |
| `machine.PromptTagType` | `abort-current-continuation`, `continuation-prompt-available?` |
| `machine.ErrorContextType` | 3 primitives |
| `syntax.SyntaxSymbolType` | `bound-identifier=?` ×2, `free-identifier=?` ×2, `identifier?` |
| `syntax.SyntaxValueType` | `datum->syntax` slot 0, `syntax->datum`, `compile` |

Predicate primitives (`pair?`, `error-object?`, `real?`, `integer?`, etc.) **keep `TypeAny`** — domain is all values per R7RS §2.B.1 of the 5.C sidecar.

Return-type updates: ~28 return sites currently declare the narrower Go type via `TypeAny` and should update to the opaque singletons (see `plans/2026-04-20-axis-b-annotation-bugs.md` §4).

### 4.9 Analyzer rewiring

`audit/wile-axis-b-params.scm:521–540` currently hardcodes:

```scheme
;; pair < list, {textual,binary}-{input,output}-port < {input,output}-port < port
(define subtype-edges
  '(("textual-output-port" . "output-port")
    ("binary-output-port" . "output-port")
    ...))
```

Post-Phase 2 (lattice hierarchy formalization, §5.2), the analyzer consults the Go-side `Subtype` via wile-goast's `go_eval` mechanism or by exporting the lattice as a manifest. The duplicated `subtype-edges` table is deleted.

In Phase 1 the analyzer stays as-is since the Go-side lattice is flat (Any-parented).

---

## 5. Roadmap

**Gating principle (added 2026-04-21):** The specification mechanism lands *independently* of primitive declaration updates, and further type-system development is gated on the primitive re-audit completing. This forces the mechanism to stand on its own design merits — uncoupled from migration-convenience pressure — and ensures the re-audit produces clean input to any subsequent lattice work.

```
Phase 1a — Specification only  (mechanism, no primitive changes)
    ↓  (gate: mechanism exists, singletons exist, tests pass)
Phase 1b — Primitive re-audit  (apply new vocabulary to registry)
    ↓  (gate: primitives correctly annotated under new vocabulary)
Phase 2+ — Further development (hierarchy, enforcement, closure-based storage, …)
```

No Phase 2 work begins until Phase 1b closes.

### 5.1 Phase 1a — Specification mechanism (no primitive changes)

**Scope**: introduce the new type-system vocabulary as inert specification. Nothing consumes it yet — not Phase-2 runtime validation, not the compiler, not primitive declarations. The mechanism exists, is testable in isolation, and is ready for the audit pass to begin using.

- Add `Supertype()` to `TypeConstraint` interface.
- Implement `Subtype(a, b)` free function with unit tests.
- Declare `AnyType` singleton.
- Implement `OpaqueTypeConstraint` with `reflect.Type` storage; unit tests for `Check` and `Supertype`.
- Declare ~24 opaque singletons in their owning packages (`values/`, `machine/`, `syntax/`, `environment/`).
- Retrofit `RecordTypeConstraint` with `Supertype()` returning `RecordType_`.
- Wire every existing `ValueType` entry with `Supertype()` returning `AnyType` (flat lattice — no hierarchy yet).
- Add `ReturnNullable bool` field to `PrimitiveSpec` advisory cluster (declared but unused in 1a).
- **No primitive declarations are changed in 1a.** Registry remains exactly as it is today.

**Exit criteria**:
- `Subtype(BoxType, AnyType) == true`, `Subtype(AnyType, BoxType) == false`, all singleton pairs round-trip correctly.
- `RecordTypeConstraint(specific-rtd).Supertype() == RecordType_`.
- All existing primitive-level tests pass unchanged (no declaration mutations).
- Manifest/inventory regeneration produces byte-identical output (nothing's been re-declared).

**Deliverable**: a separate implementation plan file (e.g., `plans/2026-04-22-type-system-phase-1a-impl.md`) to be drafted when work begins. Estimated ~300–500 LOC of pure Go additions in `values/`, `machine/`, `syntax/`, `environment/`, plus tests.

### 5.2 Phase 1b — Primitive re-audit under the new vocabulary

**Scope**: apply the new vocabulary to the registry. This is a **fresh audit pass**, not a mechanical find-replace of `TypeAny → OpaqueSingleton`. Each primitive gets reconsidered under the richer vocabulary:

- Does the existing declaration still reflect intent, or does the new vocabulary make a better annotation available?
- Are there primitives whose declared type should now be tightened beyond the obvious opaque singleton (e.g., to a `RecordTypeConstraint` for a specific record type)?
- Does `ReturnNullable: true` apply to this primitive's return contract?
- Are there declarations the old 28-entry vocabulary forced into `TypeAny` that the audit never surfaced because the enum couldn't express the right thing?

**Working from the audit artifacts**:
- `plans/2026-04-20-paramtypes-annotation-bugs.md` §2.A (~85 param-side opaque-type gaps) — candidate list for opaque-singleton migration.
- `plans/2026-04-20-axis-b-annotation-bugs.md` §4 (~28 return-side gaps) — candidate list for return-type tightening.
- `plans/2026-04-20-paramtypes-axis-c-findings.md` §3 — nullable-return R7RS patterns feeding `ReturnNullable` assignments.

**Expected shape of work**:
- Commit-by-cluster migration (box/promise, concurrency, syntax, error/prompt/mark, …) per `plans/2026-04-20-paramtypes-annotation-bugs.md` §6 Tier 1 structure.
- Each cluster: audit the primitives → write the PR → regenerate manifest + inventory → verify bucket shift → commit.
- The audit is expected to produce **some edits that aren't in the candidate list** (primitives whose current `TypeAny` turns out to have a better declaration once opaque singletons exist) and **some declined migrations** (primitives where the mechanical mapping is wrong and `TypeAny` was correct).

**Exit criteria**:
- All affected primitives in the candidate lists have been reconsidered; each has either (a) been migrated with justification or (b) been marked `keep-as-is` with rationale in a follow-up sidecar.
- Inventory regenerated: Declared-too-wide bucket drops substantially (exact target depends on audit outcomes).
- Bucket counts published in a 1b-completion sidecar (analogous to `2026-04-20-paramtypes-annotation-bugs.md`).

**Deliverable**: a separate implementation plan (`plans/2026-04-23-type-system-phase-1b-audit.md`) plus per-cluster PRs.

### 5.3 Phase 2 — Hierarchy formalization (gated on 1b)

Wire explicit supertype edges for the existing `ValueType` enum:

- **Numeric tower**: `TypeInteger.Supertype() → TypeRational → TypeReal → TypeNumber → TypeAny`; `TypeFlonum → TypeReal`; `TypeComplex → TypeNumber`; `TypeExactInteger` aliased to `TypeInteger`.
- **Port hierarchy**: `TypeBinaryOutputPort → TypeOutputPort → TypePort → TypeAny` (and parallel for input/textual variants).
- **Pair/list**: `TypePair → TypeList → TypeAny`. (Edge case: `TypeList` also includes `EmptyList`; `TypePair` does not. Consider whether this needs a `TypeEmptyList` leaf or lives as an analyzer-side refinement.)

Consequence: analyzer's hardcoded `subtype-edges` deleted. `Subtype(TypeInteger, TypeNumber)` returns `true`. 5.C §2.D list-vs-pair analyzer artifact class resolves.

**Gate**: 1b complete. Rationale — the re-audit may surface hierarchy edges the current design didn't anticipate; decoupling Phase 2 lets that influence the edge list without blocking 1b progress.

### 5.4 Phase 3 — Closure-based `OpaqueTypeConstraint` storage (gated on 1b)

For extension authors whose custom types don't fit `reflect.Type` (rare, but worth supporting):

```go
type ClosureOpaqueType struct {
    name        string
    description string
    supertype   TypeConstraint
    checkFn     func(Value) (any, bool, error)
}
```

Same `TypeConstraint` interface. Orthogonal to reflect-based singletons. Enables extension packages to declare their own opaque types without importing `reflect`.

### 5.5 Phase 4 — Enforcement wiring (gated on 2 or later; see §6.1)

Once the lattice is in place and the re-audit has produced a correct set of declarations, Phase-2 runtime validation consumes `ParamTypes` at call sites. The compiler consults `Subtype(expr-inferred, param-declared)` for static validation. `ReturnNullable` stays advisory.

---

## 6. Open questions

### 6.1 Enforcement phasing

When do Phase-2 runtime validation and compile-time static checking turn on against the new lattice? Three sub-questions:

1. **Ship the whole mechanism (lattice + singletons + re-declarations) with only documentation as the live consumer first**, then turn on enforcement in a follow-up? This is the lowest-risk path — the lattice gets exercised by static analyzers and docs tooling before runtime enforcement begins, so any declaration bugs surface in docs long before they reject valid programs.

2. **Ship enforcement simultaneously with the lattice?** Faster time-to-value but higher blast radius if any opaque-singleton declaration is subtly wrong.

3. **Ship lattice first, enforcement later, in phases matching the validation categories** (opaque types → numeric tower → ports → compound predicates)? Fine-grained rollout; more coordination overhead.

**Current stance**: unresolved. Documented for resolution before Phase 4 begins.

### 6.2 Supertype chain cycle safety

The `Subtype` walk assumes acyclic supertype chains. With singletons constructed at package init time, cycles aren't possible unless explicitly declared. Enforcement: `NewOpaqueType` could detect cycles by walking from the proposed supertype to see if the new type would appear. Cheap, optional safety.

### 6.3 Cross-package singleton visibility

Some opaque types live in packages that currently don't import `values/` (e.g., `machine/` owns `PromptTag`). Singletons for those types must live in their owning packages, which means the opaque-type mechanism needs to work across packages. The `TypeConstraint` interface already supports this — singletons are just values of the interface type.

Possible concern: discoverability. If `machine.PromptTagType` lives in `machine/` and `registry/core/` needs to declare a primitive using it, `registry/core/` must import `machine`. Check import layering — if it creates a cycle, the singleton may need to move.

---

## 7. Implementation sequencing

Phase 1a and 1b are separate PRs (separate branches, separate reviews). Phase 1a **must ship** before 1b begins.

### 7.1 Phase 1a — Specification mechanism (no primitive changes)

Suggested commit structure, one commit per logical group:

1. **Interface extension**: `Supertype()` on `TypeConstraint` interface; stub returning `AnyType` on existing `ValueType`, `NamedTypeConstraint`.
2. **`Subtype` free function**: implementation + unit tests covering identity, chain-walk, root-termination.
3. **`AnyType` singleton**: new file or addition to `values/value_type.go`; tests.
4. **`OpaqueTypeConstraint`**: new type + `NewOpaqueType` constructor; tests for `Check`, `Supertype`, `Name`, `Description`.
5. **Singletons (values package)**: ~20 entries in `values/opaque_types.go` (Box, Promise, Record, RecordType, OpaqueValue, NativeError, CompileTimeValue, SchemeEnvironment, Thread, Mutex, Channel, CondVar, RWMutex, WaitGroup, Once, AtomicBox, AtomicInt64, Time, Process).
6. **Singletons (other packages)**: `machine.PromptTagType`, `machine.ErrorContextType`, `machine.ContinuationMarkSetType`; `syntax.SyntaxSymbolType`, `syntax.SyntaxValueType`; `environment.NamespaceType`. Import layering check (see §6.3).
7. **`RecordTypeConstraint` retrofit**: one `Supertype()` method returning `RecordType_`; verify existing record-subtype tests still pass.
8. **`PrimitiveSpec.ReturnNullable`**: field addition with explicit advisory comment; no primitive sets it yet.

Expected Phase 1a size: ~300–500 LOC of pure additions. **No changes to any primitive registration, no manifest regeneration, no inventory update** — that's Phase 1b.

**Exit test**: build + test the full tree. All existing tests pass unchanged. Manifest regeneration produces byte-identical output.

### 7.2 Phase 1b — Primitive re-audit (gated on 1a)

Structure per design §5.2. Work begins only after 1a ships. Each cluster is its own PR:

1. Box + opaque + promise family
2. Record + record-type family
3. Error-object + error-context + native-error family
4. Continuation / prompt / mark family (PromptTag, ErrorContext, ContinuationMarkSet)
5. Syntax family (SyntaxSymbol, SyntaxValue, CompileTimeValue)
6. Concurrency cluster 1: Mutex + CondVar + Thread + Time
7. Concurrency cluster 2: Channel + WaitGroup + Once + RWMutex + AtomicBox + AtomicInt64
8. Process + Namespace + SchemeEnvironment
9. ReturnNullable pass across ~15 nullable-returning primitives
10. Inventory regeneration + 1b-completion sidecar

Per-cluster deliverable: audit notes (any primitives declined migration, with rationale) + updated declarations + regenerated manifest segment.

**Exit test**: manifest matches expected bucket shifts. Sidecar published summarizing final audit state.

---

## 8. Cross-references

- Audit evidence:
  - `plans/2026-04-20-paramtypes-annotation-bugs.md` §2.A (~85 param-side gaps)
  - `plans/2026-04-20-axis-b-annotation-bugs.md` §4 (~28 return-side gaps)
  - `plans/2026-04-20-paramtypes-axis-c-findings.md` §3 (~25 refinement candidates — out of scope)
- Parent plan: `plans/2026-03-26-extension-contracts-impl.md`.
- Existing TypeConstraint infrastructure: `values/value_type.go` (28-entry enum, `NamedTypeConstraint`, `RecordTypeConstraint`).
- Analyzer with hardcoded lattice: `audit/wile-axis-b-params.scm:521–540`.
- Design doc §7.3 (parent audit design): `plans/2026-04-20-paramtypes-audit-design.md` §7.3 — union/refinement cost-profile discussion now resolved (both excluded).
