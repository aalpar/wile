# Higher-Risk Architectural Consolidation Plan

## Executive Summary

This document explores higher-risk architectural changes that could save significantly more code than the low-risk helpers approach. These changes involve introducing new abstractions, interfaces, or code generation that would require more extensive testing and carry higher implementation risk.

**Estimated total savings: ~8,000-12,000 bytes**
**Risk level: MEDIUM to HIGH**
**Recommended approach: Implement incrementally with comprehensive testing**

---

## Architectural Change 1: Indexable Container Interface

**Risk: MEDIUM**
**Estimated savings: ~1,500 bytes**
**Files affected: 6 primitive files, 1 new interface file**

### Problem

Three container types (String, Vector, ByteVector) have nearly identical operations:

| Operation | String | Vector | ByteVector |
|-----------|--------|--------|------------|
| length | string-length | vector-length | bytevector-length |
| ref | string-ref | vector-ref | bytevector-u8-ref |
| set! | string-set! | vector-set! | bytevector-u8-set! |
| make | make-string | make-vector | make-bytevector |
| ->list | string->list | vector->list | (n/a) |

Each implementation has ~17-25 lines of nearly identical code:
- Extract container from arg 0
- Extract index from arg 1
- Bounds check
- Perform operation
- Return result

### Proposed Solution

Create an `Indexable` interface in `go/values/indexable.go`:

```go
// Indexable is implemented by types supporting indexed access.
type Indexable interface {
    Value
    Len() int
    IndexRef(idx int) (Value, error)
    IndexSet(idx int, v Value) error
}

// Ensure types implement Indexable
var (
    _ Indexable = (*String)(nil)
    _ Indexable = (*Vector)(nil)
    _ Indexable = (*ByteVector)(nil)
)
```

Then implement methods on each type:

```go
// In string.go
func (p *String) Len() int { return utf8.RuneCountInString(p.Value) }
func (p *String) IndexRef(idx int) (Value, error) {
    runes := []rune(p.Value)
    if idx < 0 || idx >= len(runes) {
        return nil, errors.New("index out of bounds")
    }
    return NewCharacter(runes[idx]), nil
}
func (p *String) IndexSet(idx int, v Value) error {
    ch, ok := v.(*Character)
    if !ok {
        return errors.New("expected character")
    }
    p.SetChar(idx, ch.Value)
    return nil
}

// Similar for Vector and ByteVector
```

Create generic helpers in `go/registry/helpers/indexable.go`:

```go
// IndexableLength implements the length operation for any indexable container.
func IndexableLength[T Indexable](mc *machine.MachineContext, name string) error {
    o := mc.Arg(0)
    container, ok := o.(T)
    if !ok {
        return values.WrapForeignErrorf(values.ErrNotA..., "%s: expected ... but got %T", name, o)
    }
    mc.SetValue(values.NewInteger(int64(container.Len())))
    return nil
}

// IndexableRef implements the ref operation for any indexable container.
func IndexableRef[T Indexable](mc *machine.MachineContext, name string, errType error) error {
    o := mc.Arg(0)
    k := mc.Arg(1)
    container, ok := o.(T)
    if !ok {
        return values.WrapForeignErrorf(errType, "%s: expected appropriate type but got %T", name, o)
    }
    idx, ok := k.(*values.Integer)
    if !ok {
        return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %T", name, k)
    }
    result, err := container.IndexRef(int(idx.Value))
    if err != nil {
        return values.NewForeignError(fmt.Sprintf("%s: %v", name, err))
    }
    mc.SetValue(result)
    return nil
}

// IndexableSet implements the set! operation for any indexable container.
func IndexableSet[T Indexable](mc *machine.MachineContext, name string, errType error) error {
    o := mc.Arg(0)
    k := mc.Arg(1)
    v := mc.Arg(2)
    container, ok := o.(T)
    if !ok {
        return values.WrapForeignErrorf(errType, "%s: expected appropriate type but got %T", name, o)
    }
    idx, ok := k.(*values.Integer)
    if !ok {
        return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %T", name, k)
    }
    err := container.IndexSet(int(idx.Value), v)
    if err != nil {
        return values.NewForeignError(fmt.Sprintf("%s: %v", name, err))
    }
    mc.SetValues()
    return nil
}
```

### Primitive Transformations

**Before** (`prim_vectors.go` lines 59-87, 28 lines):
```go
func PrimVectorLength(_ context.Context, mc *machine.MachineContext) error {
    o := mc.Arg(0)
    v, ok := o.(*values.Vector)
    if !ok {
        return values.WrapForeignErrorf(values.ErrNotAVector, "vector-length: expected a vector but got %T", o)
    }
    mc.SetValue(values.NewInteger(int64(len(*v))))
    return nil
}

func PrimVectorRef(_ context.Context, mc *machine.MachineContext) error {
    o := mc.Arg(0)
    k := mc.Arg(1)
    v, ok := o.(*values.Vector)
    if !ok {
        return values.WrapForeignErrorf(values.ErrNotAVector, "vector-ref: expected a vector but got %T", o)
    }
    idx, ok := k.(*values.Integer)
    if !ok {
        return values.WrapForeignErrorf(values.ErrNotANumber, "vector-ref: expected an integer but got %T", k)
    }
    if idx.Value < 0 || idx.Value >= int64(len(*v)) {
        return values.NewForeignError("vector-ref: index out of bounds")
    }
    mc.SetValue((*v)[idx.Value])
    return nil
}
// ... and PrimVectorSet (19 lines)
```

**After** (3 lines each):
```go
func PrimVectorLength(_ context.Context, mc *machine.MachineContext) error {
    return helpers.IndexableLength[*values.Vector](mc, "vector-length")
}

func PrimVectorRef(_ context.Context, mc *machine.MachineContext) error {
    return helpers.IndexableRef[*values.Vector](mc, "vector-ref", values.ErrNotAVector)
}

func PrimVectorSet(_ context.Context, mc *machine.MachineContext) error {
    return helpers.IndexableSet[*values.Vector](mc, "vector-set!", values.ErrNotAVector)
}
```

### Files to Create/Modify

| File | Action |
|------|--------|
| `go/values/indexable.go` | Create interface (~30 lines) |
| `go/values/string.go` | Add Indexable methods (~25 lines) |
| `go/values/vector.go` | Add Indexable methods (~20 lines) |
| `go/values/bytevector.go` | Add Indexable methods (~25 lines) |
| `go/registry/helpers/indexable.go` | Create generic helpers (~80 lines) |
| `go/registry/core/prim_strings.go` | Simplify ref/set/length (~45 lines removed) |
| `go/registry/core/prim_vectors.go` | Simplify ref/set/length (~45 lines removed) |
| `go/registry/core/prim_bytevectors.go` | Simplify ref/set/length (~50 lines removed) |

**Net change**: +180 lines infrastructure, -140 lines primitives = ~40 lines added
**But**: Dramatically reduced complexity and duplication, consistent behavior guaranteed

### Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Performance overhead from interface | Profile before/after; Go interface dispatch is fast |
| ByteVector special cases (byte range validation) | Keep validation in ByteVector.IndexSet method |
| String Unicode handling | Encapsulate in String.IndexRef/IndexSet methods |

---

## Architectural Change 2: Operation Code Generation

**Risk: HIGH**
**Estimated savings: ~2,000-3,000 bytes**
**Files affected: 20+ operation files**

### Problem

35 operation files follow repetitive patterns. Example categories:

**Zero-field operations** (6 files, ~270 lines total):
- `operation_pop.go` (47 lines)
- `operation_pop_all.go` (46 lines)
- `operation_push.go` (46 lines)
- `operation_brk.go` (46 lines)
- `operation_pull.go` (48 lines)
- `operation_drop.go` (40 lines)

Each has identical structure:
```go
type OperationXxx struct{}
func NewOperationXxx() *OperationXxx { return &OperationXxx{} }
func (p *OperationXxx) SchemeString() string { return "#<machine-operation-xxx>" }
func (p *OperationXxx) IsVoid() bool { return p == nil }
func (p *OperationXxx) EqualTo(o values.Value) bool { v, ok := o.(*OperationXxx); return sameType(p, v, ok) }
func (p *OperationXxx) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) { /* unique */ }
```

**Single-field operations** (8+ files, ~400 lines total):
- `operation_branch_offset_immediate.go` (51 lines)
- `operation_branch_on_false_offset_immediate.go` (55 lines)
- `operation_branch_on_not_false_offset_immediate.go` (55 lines)
- `operation_load_literal_integer.go` (55 lines)
- `operation_load_void.go` (47 lines)
- etc.

### Proposed Solution: Code Generation

Create `go/machine/gen_operations.go` with `//go:generate`:

```go
//go:generate go run gen_operations_main.go

package machine

// Generator definitions - the actual code generation happens in gen_operations_main.go
```

Create `go/machine/gen_operations_main.go` (build-ignored):

```go
//go:build ignore

package main

import (
    "os"
    "text/template"
)

type ZeroFieldOp struct {
    Name       string   // e.g., "Pop"
    ApplyBody  string   // e.g., "mc.value = []values.Value{mc.evals.Pop()}\nmc.pc++"
}

type SingleFieldOp struct {
    Name       string   // e.g., "BranchOffsetImmediate"
    FieldName  string   // e.g., "Offset"
    FieldType  string   // e.g., "int"
    ApplyBody  string   // e.g., "mc.pc += p.Offset"
}

var zeroFieldOps = []ZeroFieldOp{
    {"Pop", "mc.value = []values.Value{mc.evals.Pop()}\nmc.pc++"},
    {"PopAll", "mc.value = mc.evals.PopAll()\nmc.pc++"},
    {"Push", "mc.evals.Push(mc.value[0])\nmc.pc++"},
    {"Brk", "return mc, ErrMachineHalt"},
    {"Pull", "mc.value = mc.evals.values[len(mc.evals.values)-1:]\nmc.pc++"},
    {"Drop", "mc.evals.values = mc.evals.values[:len(mc.evals.values)-1]\nmc.pc++"},
}

var singleFieldOps = []SingleFieldOp{
    {"BranchOffsetImmediate", "Offset", "int", "mc.pc += p.Offset"},
    {"BranchOnFalseOffsetImmediate", "Offset", "int",
        "v := mc.value[0]\nif values.EqualTo(v, values.FalseValue) {\n\tmc.pc += p.Offset\n} else {\n\tmc.pc++\n}"},
    {"BranchOnNotFalseOffsetImmediate", "Offset", "int",
        "v := mc.value[0]\nif !values.EqualTo(v, values.FalseValue) {\n\tmc.pc += p.Offset\n} else {\n\tmc.pc++\n}"},
    {"LoadLiteralInteger", "Value", "int64", "mc.value = []values.Value{values.NewInteger(p.Value)}\nmc.pc++"},
    {"LoadVoid", "unused", "struct{}", "mc.value = []values.Value{values.Void}\nmc.pc++"},
}

const zeroFieldTemplate = `// Code generated by gen_operations_main.go; DO NOT EDIT.

package machine

import (
    "context"
    "wile/values"
)

{{range .}}
type Operation{{.Name}} struct{}

func NewOperation{{.Name}}() *Operation{{.Name}} {
    return &Operation{{.Name}}{}
}

func (p *Operation{{.Name}}) SchemeString() string {
    return "#<machine-operation-{{.Name | lower}}>"
}

func (p *Operation{{.Name}}) IsVoid() bool {
    return p == nil
}

func (p *Operation{{.Name}}) EqualTo(o values.Value) bool {
    v, ok := o.(*Operation{{.Name}})
    return sameType(p, v, ok)
}

func (p *Operation{{.Name}}) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
    {{.ApplyBody}}
    return mc, nil
}
{{end}}
`

func main() {
    // Generate zero-field operations
    // Generate single-field operations
    // Write to operation_generated.go
}
```

### Impact Analysis

**Files that could be generated**:

| Category | Files | Lines Removed | Generated Lines |
|----------|-------|---------------|-----------------|
| Zero-field | 6 | 273 | 180 (in one file) |
| Single-field (simple) | 5 | 255 | 150 (in one file) |
| Branch variants | 3 | 161 | 90 (parameterized) |
| **Total** | **14** | **689** | **420** |

**Net savings**: ~270 lines (~2,700 bytes)

### Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Generated code harder to debug | Keep Apply bodies in readable format; good comments |
| Build dependency on generator | Generator is simple Go code, easy to maintain |
| Loss of individual file structure | Group related operations in generated file sections |
| Complex operations don't fit template | Keep complex operations (Apply, MakeClosure, SyntaxCase) as manual files |

### Alternative: Embedding with Generics (Lower Risk)

Instead of code generation, use embedding:

```go
// In operation_base.go
type zeroFieldOperation[T any] struct{}

func (p *zeroFieldOperation[T]) IsVoid() bool { return p == nil }
func (p *zeroFieldOperation[T]) EqualTo(o values.Value) bool {
    _, ok := o.(*T)
    return ok
}

// Then in operation_pop.go:
type OperationPop struct {
    zeroFieldOperation[OperationPop]
}
```

This is lower risk but saves less code (~100 lines).

---

## Architectural Change 3: Numeric Predicate Consolidation

**Risk: MEDIUM**
**Estimated savings: ~800 bytes**
**Files affected: prim_predicates.go**

### Problem

Numeric predicates like `positive?`, `negative?`, `odd?`, `even?` have repetitive switch statements over numeric types:

```go
// PrimPositiveQ - 15 lines
switch v := o.(type) {
case *values.Integer:
    mc.SetValue(utils.BoolToBoolean(v.Value > 0))
case *values.BigInteger:
    mc.SetValue(utils.BoolToBoolean(v.IsPositive()))
case *values.BigFloat:
    mc.SetValue(utils.BoolToBoolean(v.IsPositive()))
case *values.Float:
    mc.SetValue(utils.BoolToBoolean(v.Value > 0))
case *values.Rational:
    mc.SetValue(utils.BoolToBoolean(v.Rat().Sign() > 0))
default:
    return error
}

// PrimNegativeQ - nearly identical, just changes > 0 to < 0
// PrimOddQ - 30 lines, similar structure
// PrimEvenQ - 30 lines, similar structure
```

### Proposed Solution: Add Methods to Number Interface

Extend the `Number` interface in `go/values/number.go`:

```go
type Number interface {
    Value
    Add(Number) Number
    Subtract(Number) Number
    Multiply(Number) Number
    Divide(Number) Number
    IsZero() bool
    LessThan(Number) bool

    // New methods for predicate consolidation
    Sign() int        // Returns -1, 0, or 1
    IsOdd() (bool, error)   // Returns error for non-integers
    IsEven() (bool, error)  // Returns error for non-integers
}
```

Implement on each numeric type:

```go
// In integer.go
func (p *Integer) Sign() int {
    if p.Value < 0 { return -1 }
    if p.Value > 0 { return 1 }
    return 0
}

func (p *Integer) IsOdd() (bool, error) {
    return p.Value%2 != 0, nil
}

func (p *Integer) IsEven() (bool, error) {
    return p.Value%2 == 0, nil
}

// Similar for BigInteger, Float (with integer check), etc.
```

Then simplify predicates:

```go
// Before: 15 lines
func PrimPositiveQ(_ context.Context, mc *machine.MachineContext) error {
    o := mc.Arg(0)
    n, ok := o.(values.Number)
    if !ok {
        return values.WrapForeignErrorf(values.ErrNotANumber, "positive?: expected a real number but got %T", o)
    }
    mc.SetValue(utils.BoolToBoolean(n.Sign() > 0))
    return nil
}

// After: 8 lines (same for negative?, using Sign() < 0)
```

```go
// Before: 30 lines for odd?
// After: 10 lines
func PrimOddQ(_ context.Context, mc *machine.MachineContext) error {
    o := mc.Arg(0)
    n, ok := o.(values.Number)
    if !ok {
        return values.WrapForeignErrorf(values.ErrNotANumber, "odd?: expected an integer but got %T", o)
    }
    isOdd, err := n.IsOdd()
    if err != nil {
        return values.WrapForeignErrorf(values.ErrNotANumber, "odd?: %v", err)
    }
    mc.SetValue(utils.BoolToBoolean(isOdd))
    return nil
}
```

### Impact

| Predicate | Before | After | Savings |
|-----------|--------|-------|---------|
| positive? | 15 lines | 8 lines | 7 lines |
| negative? | 17 lines | 8 lines | 9 lines |
| odd? | 30 lines | 10 lines | 20 lines |
| even? | 30 lines | 10 lines | 20 lines |
| **Total** | 92 lines | 36 lines | **56 lines** |

Plus interface methods (~60 lines added across 7 numeric types) = Net ~0 but much cleaner code.

### Risks

| Risk | Mitigation |
|------|------------|
| Interface change affects all Number implementations | All implementations in same package; single PR |
| Complex types (BigComplex) need special handling | Add appropriate methods with clear semantics |

---

## Architectural Change 4: Declarative Primitive Registration

**Risk: HIGH**
**Estimated savings: ~1,500-2,000 bytes**
**Files affected: All prim_*.go files, registry system**

### Problem

Primitive implementations have repetitive boilerplate:
1. Type extraction from arguments
2. Error formatting with primitive name
3. Result setting

### Proposed Solution: Declarative Specifications

Create a new primitive specification format:

```go
// In go/registry/declarative.go
type ArgSpec struct {
    Type     reflect.Type  // e.g., reflect.TypeOf((*values.String)(nil))
    Name     string        // For error messages
    Optional bool
}

type DeclarativePrimitive struct {
    Name     string
    Args     []ArgSpec
    Rest     *ArgSpec      // For variadic
    Impl     interface{}   // func(extracted args...) (values.Value, error)
}

// Usage example:
var stringLength = DeclarativePrimitive{
    Name: "string-length",
    Args: []ArgSpec{{Type: stringType, Name: "string"}},
    Impl: func(s *values.String) (values.Value, error) {
        return values.NewInteger(int64(utf8.RuneCountInString(s.Value))), nil
    },
}

var stringRef = DeclarativePrimitive{
    Name: "string-ref",
    Args: []ArgSpec{
        {Type: stringType, Name: "string"},
        {Type: integerType, Name: "index"},
    },
    Impl: func(s *values.String, idx *values.Integer) (values.Value, error) {
        runes := []rune(s.Value)
        if idx.Value < 0 || idx.Value >= int64(len(runes)) {
            return nil, errors.New("index out of bounds")
        }
        return values.NewCharacter(runes[idx.Value]), nil
    },
}
```

The registry generates wrapper functions:

```go
func (r *Registry) AddDeclarativePrimitive(spec DeclarativePrimitive, phases Phase) {
    wrapper := func(_ context.Context, mc *machine.MachineContext) error {
        // Extract and validate args based on spec.Args
        // Call spec.Impl with extracted args
        // Set result or return error
    }
    r.AddPrimitives([]PrimitiveSpec{{spec.Name, len(spec.Args), spec.Rest != nil, wrapper}}, phases)
}
```

### Impact

**Current** (`prim_strings.go` string-length, 9 lines):
```go
func PrimStringLength(_ context.Context, mc *machine.MachineContext) error {
    o := mc.Arg(0)
    s, ok := o.(*values.String)
    if !ok {
        return values.WrapForeignErrorf(values.ErrNotAString, "string-length: expected a string but got %T", o)
    }
    mc.SetValue(values.NewInteger(int64(utf8.RuneCountInString(s.Value))))
    return nil
}
```

**Declarative** (4 lines):
```go
var stringLength = DeclarativePrimitive{
    Name: "string-length",
    Args: []ArgSpec{{Type: stringType, Name: "string"}},
    Impl: func(s *values.String) (values.Value, error) {
        return values.NewInteger(int64(utf8.RuneCountInString(s.Value))), nil
    },
}
```

### Risks

| Risk | Severity | Mitigation |
|------|----------|------------|
| Reflection overhead | MEDIUM | Cache type checks; profile |
| Complex error handling | HIGH | Keep manual impl for complex cases |
| Loss of IDE navigation | MEDIUM | Good naming conventions |
| Debugging harder | MEDIUM | Clear error messages with stack |

### Recommendation

This is the highest-risk change. **Do not implement** unless:
1. The lower-risk changes have been completed
2. Profiling shows no performance regression
3. A subset of primitives is migrated first as proof-of-concept

---

## Implementation Roadmap

### Phase A: Indexable Interface (MEDIUM risk, ~2 weeks)

1. Add `Indexable` interface to values package
2. Implement on String, Vector, ByteVector
3. Create generic helpers
4. Migrate primitives one type at a time
5. Comprehensive testing after each type

### Phase B: Operation Embedding (LOW risk, ~1 week)

1. Create base types with generic embedding
2. Migrate zero-field operations
3. Migrate single-field operations
4. Test thoroughly

### Phase C: Numeric Methods (MEDIUM risk, ~1 week)

1. Add Sign(), IsOdd(), IsEven() to Number interface
2. Implement on all numeric types
3. Simplify predicates
4. Test with edge cases (NaN, Inf, BigInteger limits)

### Phase D: Code Generation (HIGH risk, ~2 weeks)

1. Create generator for simple operations
2. Generate zero-field operations
3. Generate single-field operations
4. Validate generated code matches behavior
5. Remove manual files only after validation

### Phase E: Declarative Primitives (HIGH risk, defer)

1. Prototype with 5-10 simple primitives
2. Measure performance impact
3. Decide whether to proceed based on results

---

## Summary

| Change | Risk | Savings | Recommended? |
|--------|------|---------|--------------|
| Indexable Interface | MEDIUM | ~1,500 bytes | Yes |
| Operation Embedding | LOW | ~500 bytes | Yes |
| Numeric Methods | MEDIUM | ~800 bytes | Yes |
| Operation Code Gen | HIGH | ~2,700 bytes | Maybe (after testing) |
| Declarative Primitives | HIGH | ~2,000 bytes | No (defer) |

**Total recommended savings: ~2,800 bytes**
**Total if all implemented: ~7,500 bytes**

---

## Verification Strategy

For each architectural change:

1. **Before**: Full test suite passes (`make test`)
2. **Implementation**: Create feature branch
3. **Unit tests**: Add tests for new abstractions
4. **Integration**: Verify primitive behavior unchanged
5. **Performance**: Profile hot paths (arithmetic, list operations)
6. **Review**: Code review focusing on edge cases
7. **Merge**: Only after all tests pass

---

## Files Summary

### New Files to Create

| File | Purpose | Lines |
|------|---------|-------|
| `go/values/indexable.go` | Indexable interface | ~30 |
| `go/registry/helpers/indexable.go` | Generic helpers | ~80 |
| `go/machine/operation_base.go` | Operation embedding | ~40 |
| `go/machine/gen_operations_main.go` | Code generator | ~200 |

### Files to Modify Significantly

| File | Change |
|------|--------|
| `go/values/string.go` | Add Indexable methods |
| `go/values/vector.go` | Add Indexable methods |
| `go/values/bytevector.go` | Add Indexable methods |
| `go/values/integer.go` | Add Sign/IsOdd/IsEven |
| `go/values/float.go` | Add Sign/IsOdd/IsEven |
| `go/values/big_integer.go` | Add Sign/IsOdd/IsEven |
| `go/values/rational.go` | Add Sign |
| `go/registry/core/prim_strings.go` | Use Indexable helpers |
| `go/registry/core/prim_vectors.go` | Use Indexable helpers |
| `go/registry/core/prim_bytevectors.go` | Use Indexable helpers |
| `go/registry/core/prim_predicates.go` | Use Number methods |
