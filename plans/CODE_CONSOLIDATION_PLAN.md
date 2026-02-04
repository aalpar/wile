# Code Consolidation Plan: Reducing Code Volume Through Parameterization

## Executive Summary

This plan identifies opportunities to reduce code volume in the Wile Scheme interpreter through parameterization techniques. Analysis identified **~3,700 bytes** of consolidation opportunities across primitives, machine, and extension packages.

**Approach**: Extend existing successful patterns (`NumericChainCompare()`, `MakeTypePredicate()`) rather than risky architectural changes.

**Not recommended**: Values package arithmetic methods (high risk, R7RS exactness semantics require type-specific behavior).

---

## Analysis Summary

| Area | Potential Savings | Risk | Recommendation |
|------|------------------|------|----------------|
| Primitive type extraction | ~2,000 bytes | LOW | Implement |
| Optional argument parsing | ~650 bytes | LOW | Implement |
| Compile-time execution pattern | ~400 bytes | MEDIUM | Implement |
| Operation EqualTo helpers | ~300 bytes | LOW | Implement (documented in TODO.md) |
| Duplicate math helpers | ~250 bytes | LOW | Implement |
| Index bounds checking | ~100 bytes | LOW | Implement |
| Values arithmetic methods | ~1,600 bytes | HIGH | **Do not implement** |
| IsVoid boilerplate | ~50 bytes | LOW | **Do not implement** (idiomatic Go) |

---

## Phase 1: Remove Duplicate Math Helpers

**Effort**: 30 minutes
**Savings**: ~250 bytes
**Risk**: LOW

### Problem

Functions in `go/extensions/math/prim_math.go` lines 29-91 are duplicates of `go/registry/helpers/value_conv.go` lines 26-79:

| Function | extensions/math location | helpers location |
|----------|------------------------|------------------|
| `ToComplex128` | lines 29-42 | value_conv.go:26-39 |
| `ComplexOrFloat` | lines 44-56 | value_conv.go:41-53 |
| `ToFloat64` | lines 58-75 | value_conv.go:55-72 |
| `FloorDivide` | lines 77-91 | integer.go:23-37 |

### Solution

Delete duplicate functions from `go/extensions/math/prim_math.go` and import from `registry/helpers`.

### Files to Modify

| File | Action |
|------|--------|
| `go/extensions/math/prim_math.go` | Delete lines 29-91, add import `"github.com/aalpar/wile/registry/helpers"` |

### Verification

```bash
cd go && make test
```

---

## Phase 2: Type Extraction Helpers

**Effort**: 4 hours
**Savings**: ~2,000 bytes
**Risk**: LOW

### Problem

95+ occurrences of this 7-line pattern across primitive files:

```go
o := mc.Arg(0)
s, ok := o.(*values.String)
if !ok {
    return values.WrapForeignErrorf(values.ErrNotAString, "string-length: expected a string but got %T", o)
}
```

### Solution

Create `go/registry/helpers/extract.go` with typed extraction helpers:

```go
// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// ...

package helpers

import (
    "github.com/aalpar/wile/machine"
    "github.com/aalpar/wile/values"
)

// ExtractString extracts a *String from the argument at idx.
// Returns an error with the primitive name if the type doesn't match.
func ExtractString(mc *machine.MachineContext, idx int, name string) (*values.String, error) {
    o := mc.Arg(idx)
    s, ok := o.(*values.String)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrNotAString, "%s: expected a string but got %T", name, o)
    }
    return s, nil
}

// ExtractInteger extracts a *Integer from the argument at idx.
func ExtractInteger(mc *machine.MachineContext, idx int, name string) (*values.Integer, error) {
    o := mc.Arg(idx)
    v, ok := o.(*values.Integer)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %T", name, o)
    }
    return v, nil
}

// ExtractVector extracts a *Vector from the argument at idx.
func ExtractVector(mc *machine.MachineContext, idx int, name string) (*values.Vector, error) {
    o := mc.Arg(idx)
    v, ok := o.(*values.Vector)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrNotAVector, "%s: expected a vector but got %T", name, o)
    }
    return v, nil
}

// ExtractByteVector extracts a *ByteVector from the argument at idx.
func ExtractByteVector(mc *machine.MachineContext, idx int, name string) (*values.ByteVector, error) {
    o := mc.Arg(idx)
    v, ok := o.(*values.ByteVector)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrNotAByteVector, "%s: expected a bytevector but got %T", name, o)
    }
    return v, nil
}

// ExtractCharacter extracts a *Character from the argument at idx.
func ExtractCharacter(mc *machine.MachineContext, idx int, name string) (*values.Character, error) {
    o := mc.Arg(idx)
    c, ok := o.(*values.Character)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrNotACharacter, "%s: expected a character but got %T", name, o)
    }
    return c, nil
}

// ExtractNumber extracts a Number interface from the argument at idx.
func ExtractNumber(mc *machine.MachineContext, idx int, name string) (values.Number, error) {
    o := mc.Arg(idx)
    n, ok := o.(values.Number)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, o)
    }
    return n, nil
}

// ExtractPair extracts a *Pair from the argument at idx.
func ExtractPair(mc *machine.MachineContext, idx int, name string) (*values.Pair, error) {
    o := mc.Arg(idx)
    p, ok := o.(*values.Pair)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrNotAPair, "%s: expected a pair but got %T", name, o)
    }
    return p, nil
}

// ExtractProcedure extracts a procedure (MachineClosure) from the argument at idx.
func ExtractProcedure(mc *machine.MachineContext, idx int, name string) (*machine.MachineClosure, error) {
    o := mc.Arg(idx)
    p, ok := o.(*machine.MachineClosure)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrNotAProcedure, "%s: expected a procedure but got %T", name, o)
    }
    return p, nil
}
```

### Example Transformation

**Before** (7 lines):
```go
func PrimStringLength(_ context.Context, mc *machine.MachineContext) error {
    o := mc.Arg(0)
    s, ok := o.(*values.String)
    if !ok {
        return values.WrapForeignErrorf(values.ErrNotAString, "string-length: expected a string but got %T", o)
    }
    mc.SetValue(values.NewInteger(int64(len(s.Value))))
    return nil
}
```

**After** (4 lines):
```go
func PrimStringLength(_ context.Context, mc *machine.MachineContext) error {
    s, err := helpers.ExtractString(mc, 0, "string-length")
    if err != nil {
        return err
    }
    mc.SetValue(values.NewInteger(int64(len(s.Value))))
    return nil
}
```

### Files to Modify

| File | Occurrences | Lines Saved |
|------|-------------|-------------|
| `go/registry/core/prim_strings.go` | 29 | ~87 |
| `go/registry/core/prim_bytevectors.go` | 26 | ~78 |
| `go/registry/core/prim_lists.go` | 17 | ~51 |
| `go/registry/core/prim_control.go` | 16 | ~48 |
| `go/registry/core/prim_vectors.go` | 7 | ~21 |
| `go/registry/core/prim_pairs.go` | ~10 | ~30 |
| `go/registry/core/prim_characters.go` | ~8 | ~24 |

### New File

Create `go/registry/helpers/extract.go` (~80 lines)

### Verification

```bash
cd go && make test
cd go && make lint
```

---

## Phase 3: Index Bounds Checking Helper

**Effort**: 30 minutes
**Savings**: ~100 bytes
**Risk**: LOW

### Problem

6 identical bounds-check patterns in ref/set operations:

```go
if idx.Value < 0 || idx.Value >= int64(len(*v)) {
    return values.NewForeignError("string-ref: index out of bounds")
}
```

### Solution

Add to `go/registry/helpers/extract.go`:

```go
// CheckIndexBounds validates that idx is in range [0, length).
// Returns an error with the primitive name if out of bounds.
func CheckIndexBounds(idx int64, length int, name string) error {
    if idx < 0 || idx >= int64(length) {
        return values.NewForeignError(fmt.Sprintf("%s: index %d out of bounds for length %d", name, idx, length))
    }
    return nil
}
```

### Files to Modify

| File | Lines | Pattern |
|------|-------|---------|
| `go/registry/core/prim_strings.go` | 108-110, 135-137 | string-ref, string-set! |
| `go/registry/core/prim_vectors.go` | 82-84, 103-105 | vector-ref, vector-set! |
| `go/registry/core/prim_bytevectors.go` | 120-122, 141-143 | bytevector-u8-ref, bytevector-u8-set! |

### Verification

```bash
cd go && go test -v -run "TestPrimString|TestPrimVector|TestPrimByte" ./registry/core/...
```

---

## Phase 4: Optional Start/End Argument Parser

**Effort**: 2 hours
**Savings**: ~650 bytes
**Risk**: LOW

### Problem

8 occurrences of this 26-line pattern for parsing optional `[start [end]]` arguments:

```go
rest := mc.Arg(1)
start := 0
end := len(s.Value)
if rest != values.EmptyList {
    pair, ok := rest.(*values.Pair)
    if !ok {
        return values.WrapForeignErrorf(values.ErrNotAList, "string->list: improper argument list")
    }
    startVal, ok := pair.Car().(*values.Integer)
    if !ok {
        return values.WrapForeignErrorf(values.ErrNotANumber, "string->list: expected an integer for start but got %T", pair.Car())
    }
    start = int(startVal.Value)

    if pair.Cdr() != values.EmptyList {
        pair2, ok := pair.Cdr().(*values.Pair)
        if !ok {
            return values.WrapForeignErrorf(values.ErrNotAList, "string->list: improper argument list")
        }
        endVal, ok := pair2.Car().(*values.Integer)
        if !ok {
            return values.WrapForeignErrorf(values.ErrNotANumber, "string->list: expected an integer for end but got %T", pair2.Car())
        }
        end = int(endVal.Value)
    }
}
if start < 0 || end > len(s.Value) || start > end {
    return values.NewForeignError("string->list: invalid indices")
}
```

### Solution

Add to `go/registry/helpers/extract.go`:

```go
// StartEndIndices holds parsed optional [start [end]] indices.
type StartEndIndices struct {
    Start int
    End   int
}

// ParseOptionalStartEnd parses optional [start [end]] arguments from a rest list.
// If rest is empty, returns (0, length). Validates indices are in bounds.
func ParseOptionalStartEnd(rest values.Value, length int, name string) (StartEndIndices, error) {
    start := 0
    end := length

    if rest == values.EmptyList {
        return StartEndIndices{Start: start, End: end}, nil
    }

    pair, ok := rest.(*values.Pair)
    if !ok {
        return StartEndIndices{}, values.WrapForeignErrorf(values.ErrNotAList, "%s: improper argument list", name)
    }

    // Parse start
    startVal, ok := pair.Car().(*values.Integer)
    if !ok {
        return StartEndIndices{}, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer for start but got %T", name, pair.Car())
    }
    start = int(startVal.Value)

    // Check for optional end argument
    if pair.Cdr() != values.EmptyList {
        pair2, ok := pair.Cdr().(*values.Pair)
        if !ok {
            return StartEndIndices{}, values.WrapForeignErrorf(values.ErrNotAList, "%s: improper argument list", name)
        }
        endVal, ok := pair2.Car().(*values.Integer)
        if !ok {
            return StartEndIndices{}, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer for end but got %T", name, pair2.Car())
        }
        end = int(endVal.Value)
    }

    // Validate indices
    if start < 0 || end > length || start > end {
        return StartEndIndices{}, values.NewForeignError(fmt.Sprintf("%s: invalid indices start=%d end=%d length=%d", name, start, end, length))
    }

    return StartEndIndices{Start: start, End: end}, nil
}
```

### Example Transformation

**Before** (26 lines):
```go
func PrimStringToList(_ context.Context, mc *machine.MachineContext) error {
    o := mc.Arg(0)
    s, ok := o.(*values.String)
    if !ok {
        return values.WrapForeignErrorf(values.ErrNotAString, "string->list: expected a string but got %T", o)
    }
    rest := mc.Arg(1)
    start := 0
    end := len(s.Value)
    // ... 20 more lines of argument parsing ...
    // actual logic
}
```

**After** (8 lines):
```go
func PrimStringToList(_ context.Context, mc *machine.MachineContext) error {
    s, err := helpers.ExtractString(mc, 0, "string->list")
    if err != nil {
        return err
    }
    indices, err := helpers.ParseOptionalStartEnd(mc.Arg(1), len(s.Value), "string->list")
    if err != nil {
        return err
    }
    // actual logic using indices.Start, indices.End
}
```

### Files to Modify

| File | Lines | Primitive |
|------|-------|-----------|
| `go/registry/core/prim_strings.go` | 161-191 | string->list |
| `go/registry/core/prim_strings.go` | 336-365 | string-copy |
| `go/registry/core/prim_bytevectors.go` | 169-199 | bytevector-copy |
| `go/registry/core/prim_bytevectors.go` | 232-254 | utf8->string |
| `go/registry/core/prim_bytevectors.go` | 322-344 | string->utf8 |
| `go/registry/core/prim_bytevectors.go` | 377-399 | bytevector-copy! |

### Verification

```bash
cd go && go test -v -run "TestPrimString|TestPrimByte" ./registry/core/...
```

---

## Phase 5: Compile-Time Execution Helper

**Effort**: 2 hours
**Savings**: ~400 bytes
**Risk**: MEDIUM

### Problem

3 files share identical 30-line expand-compile-execute pattern:

| File | Lines | Form |
|------|-------|------|
| `go/machine/compile_begin_for_syntax.go` | 56-87 | begin-for-syntax |
| `go/machine/compile_define_for_syntax.go` | 98-124 | define-for-syntax |
| `go/machine/compile_eval_when.go` | 155-192 | eval-when |

Common pattern:
```go
expandEnv := p.env.Expand()
ectx := NewExpandTimeCallContext()
expander := NewExpanderTimeContinuation(p.env)

expandedExpr, err := expander.ExpandExpression(ectx, stxVal)
if err != nil {
    return values.WrapForeignErrorf(err, "[form]: expansion failed")
}

tmpTpl := NewNativeTemplate(0, 0, false)
tmpCcnt := NewCompiletimeContinuation(tmpTpl, expandEnv)
err = tmpCcnt.CompileExpression(ctctx, expandedExpr)
if err != nil {
    return values.WrapForeignErrorf(err, "[form]: compilation failed")
}

cont := NewMachineContinuation(nil, tmpTpl, expandEnv)
mc := NewMachineContext(context.Background(), cont)
err = mc.Run()
if err != nil {
    if !errors.Is(err, ErrMachineHalt) {
        return values.WrapForeignErrorf(err, "[form]: evaluation failed")
    }
}
```

### Solution

Create `go/machine/compile_helpers.go`:

```go
// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// ...

package machine

import (
    "context"
    "errors"

    "github.com/aalpar/wile/syntax"
    "github.com/aalpar/wile/values"
)

// CompileTimeExecuteResult holds the result of compile-time execution.
type CompileTimeExecuteResult struct {
    Value values.Value
    MC    *MachineContext
}

// ExecuteAtCompileTime expands, compiles, and executes an expression at compile time.
// This consolidates the common pattern used by begin-for-syntax, define-for-syntax,
// and eval-when forms.
//
// Parameters:
//   - ctctx: The compile-time call context
//   - expr: The syntax expression to execute
//   - errPrefix: Prefix for error messages (e.g., "begin-for-syntax")
//
// Returns the execution result or an error.
func (p *CompileTimeContinuation) ExecuteAtCompileTime(
    ctctx CompileTimeCallContext,
    expr syntax.SyntaxValue,
    errPrefix string,
) (*CompileTimeExecuteResult, error) {
    expandEnv := p.env.Expand()

    ectx := NewExpandTimeCallContext()
    expander := NewExpanderTimeContinuation(p.env)

    expandedExpr, err := expander.ExpandExpression(ectx, expr)
    if err != nil {
        return nil, values.WrapForeignErrorf(err, "%s: expansion failed", errPrefix)
    }

    tmpTpl := NewNativeTemplate(0, 0, false)
    tmpCcnt := NewCompiletimeContinuation(tmpTpl, expandEnv)

    err = tmpCcnt.CompileExpression(ctctx, expandedExpr)
    if err != nil {
        return nil, values.WrapForeignErrorf(err, "%s: compilation failed", errPrefix)
    }

    cont := NewMachineContinuation(nil, tmpTpl, expandEnv)
    mc := NewMachineContext(context.Background(), cont)

    err = mc.Run()
    if err != nil {
        if !errors.Is(err, ErrMachineHalt) {
            return nil, values.WrapForeignErrorf(err, "%s: evaluation failed", errPrefix)
        }
    }

    return &CompileTimeExecuteResult{Value: mc.GetValue(), MC: mc}, nil
}
```

### Files to Modify

| File | Change |
|------|--------|
| `go/machine/compile_helpers.go` | Create new file |
| `go/machine/compile_begin_for_syntax.go` | Replace lines 56-87 with helper call |
| `go/machine/compile_define_for_syntax.go` | Replace lines 98-124 with helper call |
| `go/machine/compile_eval_when.go` | Replace lines 155-192 with helper call |

### Verification

```bash
cd go && go test -v -run "TestMacro|TestSyntax|TestBeginForSyntax|TestDefineForSyntax|TestEvalWhen" ./machine/...
```

---

## Phase 6: Operation EqualTo Migration

**Effort**: 2 hours
**Savings**: ~300 bytes
**Risk**: LOW

### Problem

Already documented in TODO.md line 258:
> Add `go/machine/operation_helpers.go` - EqualTo helper functions (~300 lines saved)

The file `go/machine/operation_helpers.go` already exists with:
- `sameType[T any]()` - for zero-field operations
- `fieldMatches[T comparable, Op any]()` - for single-field operations

These helpers need to be applied to ~27 operation files.

### Solution

Migrate operation files to use existing helpers. Example transformation:

**Before** (`operation_pop.go`):
```go
func (p *OperationPop) EqualTo(o values.Value) bool {
    _, ok := o.(*OperationPop)
    return ok
}
```

**After**:
```go
func (p *OperationPop) EqualTo(o values.Value) bool {
    return sameType[*OperationPop](o)
}
```

**Before** (`operation_branch_offset_immediate.go`):
```go
func (p *OperationBranchOffsetImmediate) EqualTo(o values.Value) bool {
    v, ok := o.(*OperationBranchOffsetImmediate)
    if !ok {
        return false
    }
    return p.Offset == v.Offset
}
```

**After**:
```go
func (p *OperationBranchOffsetImmediate) EqualTo(o values.Value) bool {
    return fieldMatches(p, o, func(op *OperationBranchOffsetImmediate) int { return op.Offset })
}
```

### Files to Modify

Zero-field operations (use `sameType`):
- `go/machine/operation_pop.go`
- `go/machine/operation_pop_all.go`
- `go/machine/operation_push.go`
- `go/machine/operation_brk.go`
- `go/machine/operation_pull.go`
- `go/machine/operation_clear_syntax_case_input.go`

Single-field operations (use `fieldMatches`):
- `go/machine/operation_branch_offset_immediate.go`
- `go/machine/operation_branch_on_false_offset_immediate.go`
- `go/machine/operation_branch_on_not_false_offset_immediate.go`
- `go/machine/operation_load_literal_integer.go`
- `go/machine/operation_load_void.go`
- (and ~16 more)

### Verification

```bash
cd go && go test -v ./machine/...
```

---

## What NOT to Consolidate

### Values Package Arithmetic Methods

**Analysis**: 28 methods (Add/Subtract/Multiply/Divide across 7 numeric types) have similar switch/case structure totaling ~1,600 lines.

**Why NOT to consolidate**:
1. **R7RS exactness semantics**: Each type has specific exactness contagion rules
2. **Performance**: Arithmetic is a hot path; generic dispatch adds overhead
3. **Type promotion rules**: Integer→Float→Complex promotion varies by operation
4. **Precision handling**: BigInteger/BigFloat have special precision requirements
5. **Working code**: The existing implementation is correct and well-tested

**Recommendation**: Document the pattern in CODING_STYLE.md for consistency, but do not attempt abstraction.

### IsVoid Boilerplate

**Pattern**: `func (p *Type) IsVoid() bool { return p == nil }` appears 7+ times.

**Why NOT to consolidate**:
1. Go interfaces require explicit method definitions
2. Each implementation is one line
3. No mechanism to share implementation without embedding

**Recommendation**: This is idiomatic Go; leave as-is.

### Tokenizer Refactoring

Already documented in TODO.md lines 485-494 as a separate initiative:
- `readRadixPrefix` - consolidate #b/#o/#d/#x handling
- `readBooleanLiteral` - consolidate #t/#true and #f/#false
- `readInfNan` - consolidate inf.0/nan.0 parsing

**Recommendation**: Implement as a separate project per existing TODO documentation.

---

## Implementation Summary

| Phase | Description | Savings | Risk | Effort |
|-------|-------------|---------|------|--------|
| 1 | Remove duplicate math helpers | 250 bytes | LOW | 30 min |
| 2 | Type extraction helpers | 2,000 bytes | LOW | 4 hours |
| 3 | Index bounds checking | 100 bytes | LOW | 30 min |
| 4 | Optional start/end parser | 650 bytes | LOW | 2 hours |
| 5 | Compile-time execution helper | 400 bytes | MEDIUM | 2 hours |
| 6 | Operation EqualTo migration | 300 bytes | LOW | 2 hours |
| **Total** | | **~3,700 bytes** | | **~11 hours** |

---

## New Files to Create

1. `go/registry/helpers/extract.go` - Type extraction and argument parsing helpers (~120 lines)
2. `go/machine/compile_helpers.go` - Compile-time execution helper (~60 lines)

---

## Verification Strategy

After each phase:

1. **Unit tests**: `cd go && make test`
2. **Linting**: `cd go && make lint`
3. **Specific tests for Phase 5**: `go test -v -run "TestMacro|TestSyntax" ./machine/...`

---

## References

- TODO.md line 257: "Add `go/registry/helpers/args.go` - helper functions for argument extraction (~600 lines saved)"
- TODO.md line 258: "Add `go/machine/operation_helpers.go` - EqualTo helper functions (~300 lines saved)"
- TODO.md lines 485-494: Tokenizer refactoring notes (separate initiative)
- Existing helpers: `go/registry/helpers/` demonstrates successful consolidation patterns
