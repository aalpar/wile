# Algebraic Reductions

Structural simplifications identified by reading every Go source file in the repository. Each entry describes code that can be **reduced** in the mathematical sense: multiple expressions of the same underlying operation collapsed into one.

The organizing principle is Occam's razor — prefer the formulation with fewer entities. Elegance here means the code reveals its algebraic structure rather than hiding it behind repetition.

See also `REFACTORING_OPPORTUNITIES.md` for previously identified reductions (form-checking predicates, CxR factory, compile-time argument extraction, eval-at-compile-time, type assertion helpers). This document covers what that file does not.

---

## I. Values Package: The Numeric Tower Dispatch

**Severity**: Critical (1000+ lines of redundant dispatch)

### Problem

Seven numeric types (`Integer`, `BigInteger`, `Float`, `BigFloat`, `Rational`, `Complex`, `BigComplex`) each implement `Add`, `Subtract`, `Multiply`, `Divide`, `Compare` with 7-way type switches. Every method on every type contains the same switch structure:

```go
func (p *Integer) Add(o Number) Number {
    switch v := o.(type) {
    case *Integer:    return NewInteger(p.Value + v.Value)
    case *BigInteger: return &BigInteger{value: new(big.Int).Add(...)}
    case *Float:      return NewFloat(float64(p.Value) + v.Value)
    case *BigFloat:   return &BigFloat{value: new(big.Float).Add(...)}
    case *Rational:   return &Rational{value: new(big.Rat).Add(...)}
    case *Complex:    return NewComplex(...)
    case *BigComplex: return v.Add(p)
    }
    panic(ErrNotANumber)
}
```

This is 5 methods x 7 types = 35 switch statements, each with 7 cases. The `numeric_tower.go` file already contains `BinaryOp`, `Promote`, and `Simplify` — the correct abstraction — but it is **not used** by the arithmetic methods. The types implement their own dispatch.

### The Algebraic Structure

The numeric tower forms a lattice:

```
Integer < BigInteger < Rational < BigFloat
Integer < Float < Complex < BigComplex
BigInteger < BigFloat
Float < BigFloat
BigFloat < BigComplex
```

Every binary operation follows the same law: promote both operands to their least upper bound in the lattice, apply the same-type operation, simplify the result. This is a single algorithm parameterized by the operation, not 35 independent implementations.

### Reduction

Each type keeps only its `*Same` methods (`addSame`, `subtractSame`, etc.). All public arithmetic methods delegate to the tower:

```go
func (p *Integer) Add(o Number) Number {
    return numericBinaryOp(p, o, func(a, b Number) Number {
        // same-type dispatch only — guaranteed by promotion
        switch x := a.(type) {
        case *Integer:    return NewInteger(x.Value + b.(*Integer).Value)
        case *BigInteger: ...
        // ...
        }
    })
}
```

Or better — define the operation table once and index into it. The `*Same` methods already exist on every type. The tower's `Promote` already handles coercion. The pieces exist; they just need to be connected.

### Files

| File | Current | After |
|------|---------|-------|
| `values/integer.go` | 5 switch methods (~150 lines) | 5 one-liners delegating to tower |
| `values/big_integer.go` | same | same |
| `values/float.go` | same | same |
| `values/big_float.go` | same | same |
| `values/rational.go` | same | same |
| `values/complex.go` | same | same |
| `values/big_complex.go` | same | same |
| `values/numeric_tower.go` | Has tower infrastructure, unused | Becomes the single dispatch point |

### Estimated reduction: ~900 lines removed, ~50 lines added.

---

## II. Values Package: Port Base Type

**Severity**: High (50+ repeated closed-guard checks, 10 duplicate Close methods)

### Problem

Ten port types share identical fields and methods:

| Field/Method | Occurrences | Identical? |
|-------------|-------------|------------|
| `closed bool` | 10 types | Yes |
| `clsr io.Closer` | 10 types | Yes |
| `Close()` | 10 types | Structurally (flush variant for output ports) |
| `IsClosed()` | 10 types | Yes |
| `IsVoid()` | 10 types | Yes (`return p == nil`) |
| `if p.closed { return ErrPortClosed }` | 50+ sites | Yes |

### The Algebraic Structure

Every port is a product type: `Port = PortBase x SpecificIO`. The `PortBase` component (`closed`, `clsr`, `Close`, `IsClosed`, closed-guard) is invariant across all ports. The `SpecificIO` component varies (reader, writer, buffer).

### Reduction

Embed a `PortBase` struct:

```go
type PortBase struct {
    closed bool
    clsr   io.Closer
}

func (p *PortBase) IsClosed() bool { return p.closed }

func (p *PortBase) guardClosed() error {
    if p.closed {
        return ErrPortClosed
    }
    return nil
}

func (p *PortBase) Close() error {
    defer func() { p.closed = true }()
    if p.clsr != nil {
        return p.clsr.Close()
    }
    return nil
}
```

Output ports override `Close` to flush first:

```go
type OutputPortBase struct {
    PortBase
    wrt *bufio.Writer
}

func (p *OutputPortBase) Close() error {
    defer func() { p.closed = true }()
    if p.wrt != nil {
        _ = p.wrt.Flush()
    }
    if p.clsr != nil {
        return p.clsr.Close()
    }
    return nil
}
```

Each port method replaces `if p.closed { return ... }` with `if err := p.guardClosed(); err != nil { return ..., err }`.

### Files

All 10 port files in `values/`:
- `character_input_port.go`, `character_output_port.go`
- `binary_input_port.go`, `binary_output_port.go`
- `byte_vector_input_port.go`, `byte_vector_output_port.go`
- `byte_vector_buffered_output_port.go`, `byte_vector_input_output_port.go`
- `string_input_port.go`, `string_output_port.go`

### Estimated reduction: ~200 lines removed, ~30 lines added.

---

## III. Values Package: Structural Equality Cycle Detection

**Severity**: Medium (85 lines of near-identical traversal)

### Problem

`values/utils.go` contains three cycle-detecting equality functions:

- `pairEqualToDeep()` (lines 116-150) — walks cons pairs
- `vectorEqualToDeep()` (lines 153-171) — walks vector elements
- `arrayListEqualToDeep()` (lines 174-198) — walks array list elements

All three:
1. Check for nil
2. Check structural compatibility (length, shape)
3. Build a visited-set key (`equalPairKey{a, b}`)
4. Guard against cycles
5. Recurse via `equalToDeep()`

### The Algebraic Structure

All three are instances of: `equalSequences(a, b []Value, visited)`. The only difference is how elements are extracted: `Car/Cdr` for pairs, index for vectors/arrays.

### Reduction

```go
func equalElements(as, bs []values.Value, visited map[equalPairKey]bool) bool {
    if len(as) != len(bs) {
        return false
    }
    for i := range as {
        if !equalToDeep(as[i], bs[i], visited) {
            return false
        }
    }
    return true
}
```

Pairs need special handling for improper lists (tail element), but vectors and array lists can share the element-wise comparison directly. The pair walker converts to a slice or uses the same helper with a streaming interface.

### Files

| File | Change |
|------|--------|
| `values/utils.go` | Unify vector/array list equality into `equalElements`, simplify pair equality |

### Estimated reduction: ~40 lines removed.

---

## IV. Syntax Package: Parallel ForEach / SyntaxForEach

**Severity**: High (duplicated iteration logic)

### Problem

`syntax/syntax_pair.go` contains two nearly identical iteration methods:

- `ForEach` (lines 251-273): iterates yielding `values.Value`
- `SyntaxForEach` (lines 276-298): iterates yielding `SyntaxValue`

The loop body — checking `IsEmptyList()`, tracking `hasNext`, incrementing `i`, walking the cdr — is identical. Only the callback type and return type differ.

Similarly, `Append` (lines 169-192) and `SyntaxAppend` (lines 195-222) duplicate list concatenation logic.

### The Algebraic Structure

Both methods are the same fold over the list spine. The iteration skeleton is type-independent; only the element accessor and callback signature vary.

### Reduction

Extract the loop skeleton into a private method that both `ForEach` and `SyntaxForEach` call, or use a generic internal iterator. Since Go generics support this:

```go
func forEachImpl[T any](p *SyntaxPair, extract func(SyntaxValue) T, fn func(int, bool, T) error) (SyntaxValue, error) {
    // single implementation of the loop
}
```

### Files

| File | Change |
|------|--------|
| `syntax/syntax_pair.go` | Unify ForEach/SyntaxForEach via generic or shared skeleton |

### Estimated reduction: ~40 lines removed.

---

## V. Syntax Package: Interface Method Boilerplate

**Severity**: Medium (8 types x 6 methods = 48 trivial methods)

### Problem

Eight syntax types implement identical versions of `SyntaxValue` interface methods:

| Method | Pattern | Occurrences |
|--------|---------|-------------|
| `IsVoid()` | `return p == nil` | 8 types |
| `SourceContext()` | `return p.sourceContext` | 8 types |
| `UnwrapAll()` | `return UnwrapAllShared(p, make(...))` | 6 types |
| `EqualTo()` | pointer identity check | 5 types (3 have custom logic) |

### The Algebraic Structure

These are default implementations: every syntax type that embeds a `sourceContext` field has the same `SourceContext()` method. This is the definition of an embedded struct.

### Reduction

```go
type syntaxBase struct {
    sourceContext *SourceContext
}

func (p *syntaxBase) SourceContext() *SourceContext { return p.sourceContext }
```

Embed `syntaxBase` in all syntax types. `IsVoid()` can't be defaulted this way (nil receiver on embedded struct doesn't work the same), but `SourceContext()` and `UnwrapAll()` can.

### Files

All syntax type files:
- `syntax_symbol.go`, `syntax_pair.go`, `syntax_vector.go`, `syntax_value.go`
- `syntax_comment.go`, `syntax_datum_comment.go`, `syntax_datum_label.go`, `syntax_directive.go`

### Estimated reduction: ~30 lines removed.

---

## VI. Machine Package: Operation Interface Boilerplate

**Severity**: High (32 operation types x ~16 lines each = ~512 lines)

### Problem

Every VM operation implements `values.Value` interface methods identically:

```go
func (p *OperationX) IsVoid() bool          { return p == nil }
func (p *OperationX) SchemeString() string   { return "#<machine-operation-X>" }
func (p *OperationX) EqualTo(o values.Value) bool {
    v, ok := o.(*OperationX)
    return sameType(p, v, ok)
}
```

32 operation types repeat this pattern.

### The Algebraic Structure

Operations embed a constant (their name) and a field-comparison function. The interface methods are determined entirely by these two values.

### Reduction

Embed a base struct that carries the operation name:

```go
type operationBase struct {
    opName string
}

func (p *operationBase) SchemeString() string { return "#<machine-operation-" + p.opName + ">" }
func (p *operationBase) String() string       { return p.SchemeString() }
```

`IsVoid()` and `EqualTo()` require per-type receivers (nil check and type assertion), so they can't be fully eliminated, but `SchemeString()` and `String()` can. The `sameType` / `fieldMatches` helpers in `operation_helpers.go` already abstract `EqualTo` partially.

### Files

All 32 `operation_*.go` files in `machine/`.

### Estimated reduction: ~200 lines removed.

---

## VII. Environment Package: Binding Lookup Duplication

**Severity**: High (4 near-identical lookup paths)

### Problem

`environment/environment_frame.go` contains four binding lookup methods that walk parent chains:

1. `GetBinding()` (line 232) — no scopes
2. `GetBindingWithScopes()` (line 318) — with scope matching
3. `GetLocalIndex()` (line 430) — local only, no scopes
4. `GetLocalIndexWithScopes()` (line 455) — local only, with scopes

The scope-aware variants duplicate the parent-chain walk with the addition of `syntax.ScopesMatch()` calls.

### The Algebraic Structure

All four are instances of: `walk(startFrame, direction, filter)` where:
- `direction` is "locals only" or "locals then globals"
- `filter` is "any" or "scopes match"

### Reduction

```go
func (p *EnvironmentFrame) resolveBinding(key string, scopes []*syntax.Scope, includeGlobals bool) *Binding {
    // single parent-chain walk with optional scope filter
}
```

The four public methods become thin wrappers that call `resolveBinding` with appropriate parameters.

### Files

| File | Change |
|------|--------|
| `environment/environment_frame.go` | Extract `resolveBinding`, simplify 4 methods to wrappers |

### Estimated reduction: ~80 lines removed.

---

## VIII. Machine Package: Scope-Aware Symbol Resolution

**Severity**: Medium (duplicated in compiler and expander)

### Problem

`compile_time_continuation.go` (lines 86-143) and `expander_time_continuation.go` both contain:

```go
if len(symbolScopes) == 0 {
    // try local then global (no scopes)
} else {
    // try local with scopes, then global with scopes
}
```

The compiler and expander duplicate the same binding resolution logic.

### Reduction

If reduction VII (environment binding lookup) is done first, this becomes a single call to `env.ResolveSymbol(sym, scopes)` that handles both paths internally. The compiler and expander each call the same method.

### Files

| File | Change |
|------|--------|
| `machine/compile_time_continuation.go` | Replace dual-path symbol resolution with single env method call |
| `machine/expander_time_continuation.go` | Same |

### Estimated reduction: ~30 lines removed (depends on VII).

---

## IX. Registry: Optional Range Argument Parsing

**Severity**: High (7 identical implementations)

### Problem

Seven primitives parse optional `[start [end]]` arguments with identical boilerplate (~20 lines each):

- `vector->list`, `vector-copy`, `vector-fill!` (`prim_vectors.go`)
- `bytevector-copy`, `bytevector-copy!` (`prim_byte_vectors.go`)
- `string->list`, `string-copy` (`prim_strings.go`)

All extract two optional integers from the rest parameter with the same type checking and error handling.

### Reduction

```go
// helpers/range.go
func ParseOptionalRange(rest values.Value, length int64, name string) (start, end int64, err error) {
    start, end = 0, length
    if values.IsEmptyList(rest) {
        return
    }
    // single implementation of the extraction logic
}
```

### Files

| File | Change |
|------|--------|
| `registry/helpers/range.go` | New: `ParseOptionalRange` |
| `registry/core/prim_vectors.go` | 3 sites use helper |
| `registry/core/prim_byte_vectors.go` | 2 sites use helper |
| `registry/core/prim_strings.go` | 2 sites use helper |

### Estimated reduction: ~120 lines removed, ~25 lines added.

---

## X. Registry: Chain Equality Predicates

**Severity**: Low-Medium (2 identical implementations)

### Problem

`boolean=?` and `symbol=?` in `prim_equality.go` (lines 73-138) implement identical variadic chain comparison. Both:
1. Validate first argument type
2. Loop through rest comparing each to first
3. Short-circuit on mismatch

### Reduction

```go
func chainEquality(mc *machine.MachineContext, name string, typeCheck func(Value) bool, eq func(a, b Value) bool) error {
    // single implementation
}
```

### Files

| File | Change |
|------|--------|
| `registry/core/prim_equality.go` | Extract `chainEquality`, reduce `boolean=?` and `symbol=?` to wrappers |

### Estimated reduction: ~40 lines removed.

---

## XI. Syntax Package: Scope Propagation Asymmetry

**Severity**: Low-Medium

### Problem

Two different mechanisms propagate scopes through syntax trees:

1. `SyntaxPair.AddScope` (syntax_pair.go:48-75) — uses interface type assertion for `AddScope` method
2. `FlipScope` (scope_utils.go:79-93) — uses switch on concrete types

Both recursively traverse the tree but with different dispatch mechanisms.

### The Algebraic Structure

Both are maps over the syntax tree: `mapSyntax(tree, f)` where `f` transforms each node. `AddScope` maps `node -> node.addScope(s)`. `FlipScope` maps `node -> node.flipScope(s)`. The traversal is the same; only the per-node operation differs.

### Reduction

If `AddScope` and `FlipScope` are both interface methods on `SyntaxValue`, the traversal can be shared. Alternatively, define a generic tree-map:

```go
func mapSyntaxTree(root SyntaxValue, f func(SyntaxValue) SyntaxValue) SyntaxValue
```

### Files

| File | Change |
|------|--------|
| `syntax/scope_utils.go` | Unify traversal with `AddScope` pattern |
| `syntax/syntax_pair.go` | Delegate to shared traversal |

### Estimated reduction: ~30 lines removed.

---

## XII. Runtime Package: Environment Initialization

**Severity**: Low

### Problem

`runtime/environment_tiny.go` contains `NewTopLevelEnvironmentFrameTiny()` and `NewLibraryEnvironmentFrame()` which share 7 sequential initialization steps (registry creation, extension loading, compiler/expander registration, bootstrap macro loading). Only the initial environment creation differs.

### Reduction

```go
func initializeRuntime(ctx context.Context, env *environment.EnvironmentFrame, extensions ...Extension) error {
    // shared: registry, extensions, compilers, expanders, bootstrap
}
```

### Files

| File | Change |
|------|--------|
| `runtime/environment_tiny.go` | Extract `initializeRuntime` |

### Estimated reduction: ~40 lines removed.

---

## Summary: Reduction Lattice

Ordered by algebraic depth — deeper reductions are more fundamental and may enable surface-level ones.

| # | Reduction | Lines Saved | Enables |
|---|-----------|-------------|---------|
| I | Numeric tower dispatch | ~900 | — |
| II | Port base type | ~200 | — |
| VI | Operation base type | ~200 | — |
| IX | Optional range parsing | ~120 | — |
| VII | Binding lookup unification | ~80 | VIII |
| III | Equality cycle detection | ~40 | — |
| IV | ForEach/SyntaxForEach | ~40 | — |
| XII | Runtime initialization | ~40 | — |
| X | Chain equality | ~40 | — |
| V | Syntax interface boilerplate | ~30 | — |
| VIII | Symbol resolution | ~30 | — |
| XI | Scope propagation | ~30 | — |

**Total potential reduction: ~1750 lines**, replacing mechanical repetition with algebraic structure.

---

## Implementation Notes

**Independence**: Most reductions are independent. Exceptions: VIII depends on VII. The numeric tower (I) is the single highest-value target.

**Risk ordering**: Start with leaf reductions (IX, X, III) that touch few files and have obvious correctness. The numeric tower (I) is highest-value but highest-risk — it changes the core arithmetic dispatch. Port base type (II) and operation base type (VI) are medium-risk embedding changes.

**Testing**: Every reduction must pass `cd go && go test ./... -count=1`. The numeric tower reduction should additionally run the R7RS numeric test suite to verify exactness preservation and tower promotion semantics.
