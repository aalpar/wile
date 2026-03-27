# Extension API Contract System Design

**Date**: 2026-03-26
**Status**: Approved
**Motivation**: LLM reliability feedback identified "plausible but wrong" as the key risk
for Scheme code. Stronger type declarations on extension APIs make types at boundaries
explicit for callers (including LLMs), feed the documentation system, and lay groundwork
for a future linter.

## Overview

Add a contract system to `PrimitiveSpec` that declares parameter types and return types
for foreign functions. Contracts serve three consumers:

1. **Documentation** — `,doc` REPL command, MCP `describe-primitive` tool
2. **Runtime enforcement** — dispatch layer validates arguments before calling `Impl`
3. **Future linter** — static analysis can check call sites against declared contracts

## Design Decisions

| Decision | Choice | Alternatives Considered |
|----------|--------|------------------------|
| Primary consumer | Both docs and runtime (layered) | Docs-only; runtime-only |
| Type vocabulary | Typed enum (`ValueType uint8`) | Go-type strings; Scheme-type strings |
| Union types | Named predicates, `TypeAny` escape | Per-position union sets |
| Check narrowing | Narrow to contract's interface level | Pass through concrete type |
| Spec structure | Extend PrimitiveSpec directly | Separate Contract struct; unified metadata struct |
| Return types | Included (expandable later) | Deferred |

## ValueType Enum

Defined in `values/` and referenced from `PrimitiveSpec` in `registry/` to avoid import cycles. A `uint8` backed by `[typeCount]T`
lookup tables for constant-time access.

```go
type ValueType uint8

const (
    TypeAny          ValueType = iota // no constraint
    TypeVoid                          // void

    // Booleans
    TypeBoolean                       // *values.Boolean

    // Numeric tower
    TypeNumber                        // values.Number (any numeric)
    TypeComplex                       // values.ComplexNumber
    TypeReal                          // values.RealNumber
    TypeRational                      // *values.Rational
    TypeInteger                       // exact integer: *values.Integer | *values.BigInteger
    TypeExactInteger                  // alias for TypeInteger (clarity)
    TypeFlonum                        // inexact real: *values.Float | *values.BigFloat

    // Text
    TypeString                        // *values.String
    TypeCharacter                     // *values.Character
    TypeSymbol                        // *values.Symbol
    TypeByte                          // *values.Byte

    // Collections
    TypePair                          // *values.Pair
    TypeList                          // values.Tuple (pair or empty list)
    TypeVector                        // *values.Vector
    TypeByteVector                    // *values.ByteVector
    TypeHashtable                     // *values.Hashtable

    // Procedures
    TypeProcedure                     // values.Callable

    // Ports
    TypePort                          // values.Port
    TypeInputPort                     // values.InputPort
    TypeOutputPort                    // values.OutputPort
    TypeTextualInputPort              // values.TextualReader
    TypeTextualOutputPort             // values.TextualWriter
    TypeBinaryInputPort               // values.BinaryReader
    TypeBinaryOutputPort              // values.BinaryWriter

    typeCount                         // must be last — used to size lookup tables
)
```

~28 entries covering the R7RS type hierarchy. Extensible by adding constants before
`typeCount`.

### ValueType as Named Predicate

Each `ValueType` constant is a **named predicate** — a scalar that identifies a
type-checking concept. It is not a 1:1 mapping to Go types or Scheme types. A single
`ValueType` may match multiple Go concrete types (e.g., `TypeInteger` matches both
`*values.Integer` and `*values.BigInteger`), and a single Go interface may correspond
to multiple `ValueType` entries (e.g., `values.Number` is `TypeNumber`, but
`values.RealNumber` is `TypeReal`).

The scalar **names the concept**. Three methods give it meaning:

| Method | Signature | Purpose |
|--------|-----------|---------|
| `String()` | `string` | Scheme-facing name: `"string"`, `"exact-integer"` |
| `Description()` | `string` | Longer help text: `"An exact integer (fixnum or bignum)"` |
| `Check()` | `(values.Value) (any, bool, error)` | Predicate + narrowing + error |

All three are backed by `[typeCount]T` arrays — constant-time, no map allocation.

### Check Signature

```go
// Check validates v against this type contract.
//
// Returns:
//   - narrowed: the value narrowed to the contract's interface level (any on Go
//     side, values.Value on Scheme side). For TypeString this is *values.String;
//     for TypeNumber this is values.Number (the interface).
//   - ok: whether the value matched the predicate.
//   - err: on failure, a descriptive error ("expected exact integer, got string").
//     On success, nil.
func (vt ValueType) Check(v values.Value) (any, bool, error)
```

**Narrowing rule**: `Check` narrows to the contract's interface level, not the
concrete type. If the contract says `TypeNumber` and the value is `*values.Integer`,
`Check` returns `values.Number`. The caller can further narrow if needed.

**Go vs Scheme boundary**: On the Scheme side, everything is `values.Value`. On the
Go side, the `any` return gives extension authors the narrowed type without
re-asserting. A Go caller does: `n := result.(values.Number)`.

## PrimitiveSpec Changes

Two new optional fields:

```go
type PrimitiveSpec struct {
    Name       string
    ParamCount int
    IsVariadic bool
    Impl       machine.ForeignFunction
    Doc        string
    ParamNames []string
    Category   string
    ParamTypes []ValueType  // NEW: type contract per parameter position
    ReturnType ValueType    // NEW: return type (zero value = TypeAny = no constraint)
}
```

### ParamTypes Semantics

- `nil` or empty: no contract — legacy behavior, zero overhead.
- Length matches `ParamCount` for non-variadic primitives.
- For variadic primitives, the last entry applies to all rest arguments.
- Each position checked independently via `ValueType.Check()`.

### ReturnType Semantics

- Zero value is `TypeAny` (iota 0) — backwards-compatible, no constraint.
- Used for documentation initially. Runtime return-type checking is a future opt-in.

### Example Registration

Before:
```go
{Name: "string-ref", ParamCount: 2, Impl: PrimStringRef,
    Doc: "Returns the kth character of string.",
    ParamNames: []string{"string", "k"}, Category: "strings"}
```

After:
```go
{Name: "string-ref", ParamCount: 2, Impl: PrimStringRef,
    Doc: "Returns the kth character of string.",
    ParamNames: []string{"string", "k"}, Category: "strings",
    ParamTypes: []ValueType{TypeString, TypeExactInteger},
    ReturnType: TypeCharacter}
```

## Contract Validation

```go
// ValidateArgs checks arguments against the spec's ParamTypes contract.
// Returns nil if no contract or all checks pass.
func ValidateArgs(spec PrimitiveSpec, mc *machine.MachineContext) error {
    if len(spec.ParamTypes) == 0 {
        return nil // no contract — fast path
    }
    argc := mc.ArgCount()
    for i := 0; i < argc; i++ {
        vt := paramTypeAt(spec, i)
        if vt == TypeAny {
            continue
        }
        _, ok, err := vt.Check(mc.Arg(i))
        if !ok {
            return err
        }
    }
    return nil
}

// paramTypeAt returns the ValueType for parameter position i.
// For variadic primitives, positions beyond len(ParamTypes)-1 use the last entry.
func paramTypeAt(spec PrimitiveSpec, i int) ValueType {
    if i < len(spec.ParamTypes) {
        return spec.ParamTypes[i]
    }
    return spec.ParamTypes[len(spec.ParamTypes)-1]
}
```

### Enforcement Site

The dispatch layer in the VM (`callForeignCached` / `applyForeign` in
`machine/machine_context.go`) calls `ValidateArgs` before `Impl`. Gated on
`len(spec.ParamTypes) > 0` so uncontracted primitives pay zero cost.

Enforcement is opt-in at the engine level initially (e.g., `WithContractEnforcement()`
engine option) to allow incremental rollout without risking breakage in existing
embedder code.

## Documentation Integration

The existing `,doc` REPL command works through `DocProvider` → `RegistryDocProvider`.
Contract types enhance it with no new plumbing:

```
> ,doc string-ref
(string-ref string k) → character

  Returns the kth character of string.

  Parameters:
    string : string
    k      : exact integer
  Returns: character
  Category: strings
```

Parameter type names come from `ValueType.String()`, return type from
`ReturnType.String()`. The existing `formatPrimitiveDoc` in `internal/repl/meta.go`
gets a small update to include type annotations when `ParamTypes` is populated.

The MCP server's `describe-primitive` tool (v2 roadmap) reads `PrimitiveSpec`
directly — contract data is available with no additional work.

## RequireArg Evolution

`helpers.RequireArg[T]` performs type validation and extraction. With contracts,
validation moves to the dispatch layer. However:

- **RequireArg is not deprecated when contracts ship.** It remains correct and
  provides defense-in-depth for contracted primitives.
- For contracted primitives, `RequireArg` becomes redundant (belt-and-suspenders) —
  the contract already validated the type, so `RequireArg` will never fail.
- New primitives written against contracts can use bare type assertions
  (`mc.Arg(0).(*values.String)`) since the contract guarantees safety.
- The bare assertion panics on misconfiguration rather than returning a Scheme error.
  `RequireArg` fails gracefully. This is an acceptable trade-off for contracted
  primitives where the contract is the source of truth.

### Deprecation Timeline

| Phase | RequireArg Status | Rationale |
|-------|-------------------|-----------|
| Contracts ship (metadata only) | Active, unchanged | Contracts are docs-only initially |
| Enforcement enabled | Redundant for contracted primitives | 305 primitives use it — noisy to deprecate |
| All primitives contracted | Candidate for deprecation | Only then is there no code that needs it |
| Deprecated | Replaced by extraction-only helper or bare assertions | Future decision |

A natural successor would be `helpers.ExtractArg[T](mc, i) T` — same generic
extraction, panics instead of returning error (contract already validated). This is
out of scope for the current design.

## Migration Strategy

### Phase 1: Infrastructure

Ship the `ValueType` enum, `PrimitiveSpec` field additions, `ValidateArgs` function,
and `,doc` formatting updates. Zero primitives contracted. All existing code unchanged.
This phase alone enables documentation improvements for any primitive that adds types.

### Phase 2: Core Primitives

Add `ParamTypes` and `ReturnType` to `registry/core/` primitives (~172). These are
the most regular, table-driven registrations. One file at a time, mechanical work.

### Phase 3: Extension Primitives

Add contracts to `extensions/` primitives (~133). Same mechanical process.

### Phase 4: Enforcement

Enable contract enforcement in the dispatch layer. Initially gated behind
`WithContractEnforcement()` engine option. Once confidence is high, enable by default.

Each phase is independently shippable. Phase 1 alone delivers documentation value.

## Future Directions (Out of Scope)

- **Linter integration**: Static analysis checking call sites against declared contracts.
- **Return-type enforcement**: Runtime checking of return values.
- **Scheme-defined procedure contracts**: Extending the system beyond foreign functions
  to `define`-based procedures (ties into the documentation system design).
- **Custom ValueType registration**: Extensions registering domain-specific types.
- **RequireArg deprecation**: Gated on all primitives having contracts.
