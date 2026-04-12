# Extensible Type Constraints

**Status:** Implemented (PR #629)
**Date:** 2026-04-09
**Branch:** feat/extensible-type-constraints (merged)

## Problem

Wile's type system (`ValueType` enum in `values/value_type.go`) is a closed set of
27 built-in types. This prevents:

1. **Documentation** from expressing record-specific types (e.g., "this parameter
   takes a `<point>` record" — today it can only say "record" or "any").
2. **Parameter validation** for Go extension authors who want to assert that a
   foreign closure receives a specific record type.
3. **SRFI-99/131 record inheritance** where `point3d` ISA `point` — the type
   system has no mechanism for subtype relationships.

## Design

### Core Abstraction: TypeConstraint Interface

Replace `ValueType` with an interface everywhere types are expressed:

```go
// TypeConstraint describes a type expectation for documentation and validation.
type TypeConstraint interface {
    // Name returns the Scheme-facing type name (e.g., "integer", "point").
    Name() string
    // Description returns a human-readable description.
    Description() string
    // Check tests whether v satisfies this constraint.
    // Returns (narrowed value, matched, error).
    Check(Value) (any, bool, error)
}
```

`ValueType` implements `TypeConstraint` — all 27 built-in types work as before.

### Concrete Implementations

**`ValueType`** (existing) — built-in types. `Name()` aliases existing `String()`.
`Description()` and `Check()` already exist.

**`NamedTypeConstraint`** (new) — an unresolved type name from a docstring.
Documentation-only; `Check()` always fails. Created by `docparse.ParseValueType`
for unknown names instead of collapsing to `TypeAny`.

```go
type NamedTypeConstraint struct {
    name string
}
```

**`RecordTypeConstraint`** (new) — wraps a `*RecordType`. `Check()` verifies the
record's type matches, walking the parent chain for subtype relationships.

```go
type RecordTypeConstraint struct {
    rtd *RecordType
}
```

The parent-chain walk in `Check()`:
```go
for rt := rec.RecordType(); rt != nil; rt = rt.Parent() {
    if rt == p.rtd {
        return rec, true, nil
    }
}
```

### Nil vs TypeAny

- `nil` TypeConstraint means **unspecified** (no type info declared).
- `TypeAny` means **explicitly any** (the parameter accepts any value).

This matches how `ParamTypes` already works: an empty slice means "no type info,"
not "all params are any."

### RecordType Gets a Parent Pointer

```go
type RecordType struct {
    name       *Symbol
    fieldNames []*Symbol
    parent     *RecordType  // nil = no parent (flat R7RS record)
}
```

`fieldNames` holds only the new fields declared by this type, not inherited ones.
To enumerate all fields, walk up the parent chain.

New constructor: `NewDerivedRecordType(name, parent, fieldNames)`. Nothing calls it
yet — `define-record-type` doesn't support a parent clause. The data structure is
forward-compatible with SRFI-99/131.

### Lazy Resolution for Docstrings

Scheme docstrings like `p : point` produce a `NamedTypeConstraint("point")` at
parse time. Resolution against a live environment (to find a `*RecordType`) happens
lazily at documentation rendering or validation time — not at parse time. This is
order-independent and works with forward references.

### Backward Compatibility

Changing `PrimitiveSpec.ParamTypes` from `[]ValueType` to `[]TypeConstraint`
requires updating every slice literal in `registry/core/*.go`. The values inside
the slices don't change — `ValueType` constants implement `TypeConstraint`. This is
mechanical churn, done once.

## Change Inventory

| Layer | Files | Nature of change |
|-------|-------|-----------------|
| `values/value_type.go` | 1 | Define `TypeConstraint`, `NamedTypeConstraint`, `RecordTypeConstraint`. Add `Name()` to `ValueType`. |
| `values/record_type.go` | 1 | Add `parent` field, `Parent()`, `NewDerivedRecordType` |
| `docparse/docparse.go` | 1 | Return type → `TypeConstraint`, unknown names → `NamedTypeConstraint` |
| `docparse/docparse_test.go` | 1 | Update assertions |
| `registry/registry.go` | 1 | `PrimitiveSpec` field types → `TypeConstraint` |
| `registry/registry_test.go` | 1 | Update test literals |
| `registry/core/*.go` | ~15 | Mechanical: `[]values.ValueType{` → `[]values.TypeConstraint{` |
| `repl/doc_provider.go` | 1 | `DocInfo` field types → `TypeConstraint` |
| `repl/meta.go` | 1 | `paramTypeForDoc` return type, nil checks |
| `repl/meta_test.go` | 1 | Update assertions |
| `repl/registry_doc_provider*.go` | 2 | Adapt to new types |
| `values/value_type_test.go` | 1 | Add tests for new types |

**Not touched:** `machine/`, `SchemeTypeName()`, `cmd/typeswitchlint/`,
Scheme-level syntax, any existing behavior.

## Growth Path

1. **This change:** TypeConstraint interface + NamedTypeConstraint + RecordTypeConstraint + parent pointer on RecordType. Documentation can express custom types.
2. **Parameter validation:** `PrimitiveSpec.ParamTypes` constraints checked at call time in the VM or foreign closure wrapper. `RecordTypeConstraint.Check()` already works.
3. **SRFI-99/131 inheritance:** `define-record-type` gains a parent clause. `record-predicate` walks the parent chain. `NewDerivedRecordType` gets called. The type constraint system already handles subtype checking via parent-chain walk.
4. **Lazy resolution:** `NamedTypeConstraint` gains a `Resolve(env)` method that looks up the record type by name in the current environment, upgrading to `RecordTypeConstraint`.

## Decisions Made

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Interface vs enum extension | TypeConstraint interface | Generic, extensible, records plug in naturally |
| Migration strategy | Accept churn, change all sites at once | One-time cost, no confusing dual-field API |
| Unknown docstring types | `NamedTypeConstraint` (lazy resolution) | Order-independent, forward-reference friendly |
| nil vs TypeAny | nil = unspecified, TypeAny = explicitly any | Matches existing empty-slice semantics |
| Record field storage | New fields only, not inherited | Matches SRFI-99/136 where child doesn't redeclare parent fields |
