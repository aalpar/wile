# Opaque Values Design

**Date**: 2026-03-24
**Status**: Approved
**Unblocks**: wile-goast Track A1 (shared sessions)

## Problem

Embedders need to pass arbitrary Go objects through Scheme code without Scheme
knowing about the internals. Today, each Go-backed type (Channel, WaitGroup, etc.)
requires a dedicated `values/` struct. OpaqueValue generalizes this pattern.

## Design

### Layer 1: Interface

```go
// values/opaque.go

// Opaque marks a Value as opaque to Scheme code.
// Any Value type can opt in by implementing this single method.
type Opaque interface {
    OpaqueTag() string
}
```

- `opaque?` predicate checks this interface (capability check, not type check)
- `opaque-tag` calls `OpaqueTag()`, returns a symbol
- Any existing or future Value type can satisfy `Opaque`

### Layer 2: Convenience Struct

```go
// values/opaque_value.go

type OpaqueValue struct {
    tag string
    id  uint64
    val any
}

func NewOpaqueValue(tag string, val any) *OpaqueValue
func (p *OpaqueValue) OpaqueTag() string     // returns tag
func (p *OpaqueValue) Unwrap() any           // Go-only access to inner value
func (p *OpaqueValue) SchemeString() string   // #<tag:id>
func (p *OpaqueValue) IsVoid() bool           // nil check
func (p *OpaqueValue) EqualTo(v Value) bool   // identity (pointer equality)
```

- Atomic counter for IDs (same pattern as Channel)
- Construction is Go-only (`NewOpaqueValue`)
- Inner value accessible only from Go (`Unwrap`)
- SchemeString format: `#<tag:id>` (e.g., `#<db-conn:42>`)

### Layer 3: Predicates

Registered in `registry/core/` (type introspection, not extension behavior):

| Primitive | Behavior |
|-----------|----------|
| `(opaque? v)` | `#t` if `v` satisfies `Opaque` interface |
| `(opaque-tag v)` | Returns tag as symbol; error if not opaque |

### Not Included

- No Scheme-side construction or unwrap
- No `opaque-id` accessor (ID is display-only, in SchemeString)
- No custom equality — identity only for the convenience struct; interface
  implementors control their own `EqualTo`

## Usage Pattern

**Simple embedding** (convenience struct):
```go
db, _ := sql.Open("postgres", connStr)
eng.Eval(ctx, eng.MustParse(ctx, `(do-something db)`),
    wile.WithBinding("db", values.NewOpaqueValue("db-conn", db)))
```

**Rich Go type** (direct interface implementation):
```go
// In wile-goast: GoSession implements Value + Opaque directly
type GoSession struct { ... }
func (p *GoSession) OpaqueTag() string    { return "go-session" }
func (p *GoSession) SchemeString() string { return fmt.Sprintf("#<go-session:%d>", p.id) }
// ... other Value methods + session-specific Go methods
```

Both satisfy `(opaque? v)` and `(opaque-tag v)`.
