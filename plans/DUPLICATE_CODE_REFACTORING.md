# Duplicate Code Refactoring Plan

**Status**: In Progress
**Created**: 2026-01-31
**Last Updated**: 2026-01-31
**Initial Issues**: 39 duplicate code blocks
**Remaining Issues**: 26 duplicate code blocks
**Eliminated**: 13 duplicates (33% reduction)

## Overview

The linter identified 39 duplicate code blocks across 6 packages. These duplications fall into clear patterns that can be consolidated using Go generics, helper functions, and extracted common patterns.

## Completed Refactorings

### 1. Output Port Helpers (✅ Complete)
- **Eliminated**: 3 duplicates
- **File**: `extensions/io/prim_read_write.go`
- **Solution**: Created `getOptionalOutputPort()` helper
- **Impact**: Refactored `PrimWrite`, `PrimDisplay`, `PrimWriteShared`
- **Lines reduced**: ~87 → ~36 (plus 19-line helper)

### 2. Parity Check Predicates (✅ Complete)
- **Eliminated**: 2 duplicates
- **File**: `registry/core/prim_predicates.go`
- **Solution**: Created `parityCheck()` helper
- **Impact**: Refactored `PrimOddQ`, `PrimEvenQ`
- **Lines reduced**: ~64 → ~8 (plus 32-line helper)

### 3. Integer Division Operations (✅ Complete)
- **Eliminated**: 2 duplicates
- **File**: `registry/core/prim_arithmetic.go`
- **Solution**: Created `integerDivisionOp()` helper
- **Impact**: Refactored `PrimQuotient`, `PrimRemainder`
- **Lines reduced**: ~106 → ~14 (plus 52-line helper)

### 4. Variadic Type Comparisons (✅ Complete)
- **Eliminated**: 2 duplicates
- **File**: `extensions/all/prim_all.go`
- **Solution**: Created generic `variadicCompare[T, V]()` helper
- **Impact**: Refactored `stringCompareVariadic`, `charCompareVariadic`
- **Lines reduced**: ~60 → ~12 (plus 38-line generic helper)
- **Note**: Uses Go generics for type safety

### 5. Optional Position Extraction (✅ Complete)
- **Eliminated**: 2 duplicates
- **File**: `extensions/io/prim_read_write.go`
- **Solution**: Created `extractOptionalPositions()` helper
- **Impact**: Refactored bytevector I/O start/end argument parsing in `read-bytevector!` and `write-bytevector`
- **Lines reduced**: ~54 → ~6 (plus 28-line helper)

### 6. Registry Helper Comparisons (✅ Complete)
- **Eliminated**: 2 duplicates
- **Files**: `registry/helpers/char.go`, `registry/helpers/string.go`
- **Solution**: Created generic `variadicCompare[T, V]()` helper (similar to extensions/all pattern)
- **Impact**: Refactored `CharCompareVariadic`, `StringCompareVariadic`
- **Lines reduced**: ~60 → ~14 (plus 44-line generic helper in char.go)
- **Note**: Uses Go generics for type safety

**Total Progress**: 13 duplicates eliminated, 26 remaining (33% reduction)

## Pattern Categories

### 1. Variadic Type Comparison Helpers (2 duplicates)

**Location**: `extensions/all/prim_all.go`

**Duplicates**:
- `stringCompareVariadic` (lines 401-430)
- `charCompareVariadic` (lines 819-848)

**Pattern**: Both functions implement identical logic:
1. Extract first value with type assertion
2. Iterate through rest list comparing adjacent pairs
3. Short-circuit on first failure
4. Return boolean result

**Solution**: Create a generic helper that abstracts over the element type:

```go
// Generic variadic comparison helper
func variadicCompare[T any](
    mc *machine.MachineContext,
    name string,
    extractFirst func(values.Value) (T, bool),
    extractNext func(values.Value) (T, bool),
    cmp func(T, T) bool,
    errType error,
) error {
    // Common logic extracted
}

// Wrappers become one-liners
func stringCompareVariadic(mc *machine.MachineContext, name string, cmp func(a, b string) bool) error {
    return variadicCompare(mc, name,
        func(v values.Value) (string, bool) { /* extract */ },
        func(v values.Value) (string, bool) { /* extract */ },
        cmp,
        values.ErrNotAString)
}
```

**Files to modify**:
- `extensions/all/prim_all.go`

**Impact**: Reduces ~60 lines to ~15 lines + generic helper

---

### 2. Output Port Helper Pattern (3 duplicates)

**Location**: `extensions/io/prim_read_write.go`

**Duplicates**:
- `PrimWrite` (lines 159-187)
- `PrimDisplay` (lines 228-256)
- `PrimWriteShared` (lines 327-355)

**Pattern**: All three functions:
1. Extract object to write
2. Extract optional port argument from rest list
3. Validate port is a Tuple/IsList
4. Get OutputPort from tuple or default to current-output-port
5. Write using different formatters
6. Flush and set void result

**Solution**: Extract port resolution into a helper:

```go
// Extract optional output port from variadic args
func getOutputPort(mc *machine.MachineContext, argIndex int) (values.OutputPort, error) {
    o := mc.Arg(argIndex)
    tuple, ok := o.(values.Tuple)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %T", o)
    }
    if !tuple.IsList() {
        return nil, values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", tuple.SchemeString())
    }
    if tuple.IsEmptyList() {
        return GetCurrentOutputPort(), nil
    }
    p, ok := tuple.Car().(values.OutputPort)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrNotAnOutputPort, "expected an output port but got %T", tuple.Car())
    }
    return p, nil
}

// Then each function becomes:
func PrimWrite(_ context.Context, mc *machine.MachineContext) error {
    obj := mc.Arg(0)
    writer, err := getOutputPort(mc, 1)
    if err != nil {
        return err
    }
    _, err = writer.Write([]byte(values.WriteValueToString(obj)))
    if err != nil {
        return values.WrapForeignErrorf(err, "error writing to output port")
    }
    writer.Flush()
    mc.SetValues()
    return nil
}
```

**Files to modify**:
- `extensions/io/prim_read_write.go`

**Impact**: Reduces ~87 lines to ~36 lines + helper

---

### 3. Optional Port Argument Extraction (2 duplicates)

**Location**: `extensions/io/prim_read_write.go`

**Duplicates**:
- Lines 855-881 (in one function)
- Lines 950-976 (in another function)

**Pattern**: Extract optional start/end positions from bytevector I/O arguments

**Solution**: Similar to #2, extract helper for optional position arguments:

```go
func extractOptionalPositions(tuple values.Tuple) (start, end int, hasEnd bool, err error) {
    // Common extraction logic
}
```

**Files to modify**:
- `extensions/io/prim_read_write.go`

**Impact**: Reduces ~54 lines to ~20 lines + helper

---

### 4. Integer Division Helpers (2 duplicates)

**Location**: `registry/core/prim_arithmetic.go`

**Duplicates**:
- `PrimQuotient` (lines 284-334)
- `PrimRemainder` (lines 339-389)

**Pattern**: Both functions:
1. Extract two integer arguments with exactness tracking
2. Handle BigInteger and regular integer cases
3. Check for division by zero
4. Apply different operations (Quo vs Rem)
5. Handle inexact result conversion

**Solution**: Extract integer division helper:

```go
// Generic integer division operation
func integerDivisionOp(
    mc *machine.MachineContext,
    name string,
    regularOp func(int64, int64) int64,
    bigOp func(*big.Int, *big.Int, *big.Int) *big.Int,
) error {
    o0, o1 := mc.Arg(0), mc.Arg(1)
    v0, big0, inexact0, err := extractInteger(o0, name)
    if err != nil {
        return err
    }
    v1, big1, inexact1, err := extractInteger(o1, name)
    if err != nil {
        return err
    }

    inexact := inexact0 || inexact1

    if big0 != nil || big1 != nil {
        // BigInteger path
        b0 := big0
        if b0 == nil {
            b0 = big.NewInt(v0)
        }
        b1 := big1
        if b1 == nil {
            b1 = big.NewInt(v1)
        }
        if b1.Sign() == 0 {
            return values.NewForeignError(name + ": division by zero")
        }
        result := bigOp(new(big.Int), b0, b1)
        if inexact {
            f, _ := new(big.Float).SetInt(result).Float64()
            mc.SetValue(values.NewFloat(f))
        } else {
            mc.SetValue(values.NewBigInteger(result))
        }
        return nil
    }

    // Regular integer path
    if v1 == 0 {
        return values.NewForeignError(name + ": division by zero")
    }
    result := regularOp(v0, v1)
    if inexact {
        mc.SetValue(values.NewFloat(float64(result)))
    } else {
        mc.SetValue(values.NewInteger(result))
    }
    return nil
}

// Usage becomes:
func PrimQuotient(_ context.Context, mc *machine.MachineContext) error {
    return integerDivisionOp(mc, "quotient",
        func(a, b int64) int64 { return a / b },
        (*big.Int).Quo)
}

func PrimRemainder(_ context.Context, mc *machine.MachineContext) error {
    return integerDivisionOp(mc, "remainder",
        func(a, b int64) int64 { return a % b },
        (*big.Int).Rem)
}
```

**Files to modify**:
- `registry/core/prim_arithmetic.go`

**Impact**: Reduces ~106 lines to ~60 lines + helper

---

### 5. Parity Check Predicates (2 duplicates)

**Location**: `registry/core/prim_predicates.go`

**Duplicates**:
- `PrimOddQ` (lines 377-408)
- `PrimEvenQ` (lines 414-445)

**Pattern**: Both functions:
1. Extract integer value
2. Check if BigInteger or regular integer
3. Apply different parity tests
4. Return boolean result

**Solution**: Extract parity check helper:

```go
func parityCheck(
    mc *machine.MachineContext,
    name string,
    regularTest func(int64) bool,
    bigTest func(*big.Int) bool,
) error {
    o := mc.Arg(0)

    switch v := o.(type) {
    case *values.Integer:
        mc.SetValue(utils.BoolToBoolean(regularTest(v.Value)))
        return nil
    case *values.BigInteger:
        mc.SetValue(utils.BoolToBoolean(bigTest(v.Value)))
        return nil
    case *values.BigFloat, *values.Float:
        return values.WrapForeignErrorf(values.ErrNotAnInteger, "%s: expected an integer but got %T", name, o)
    default:
        return values.WrapForeignErrorf(values.ErrNotAnInteger, "%s: expected an integer but got %T", name, o)
    }
}

func PrimOddQ(_ context.Context, mc *machine.MachineContext) error {
    return parityCheck(mc, "odd?",
        func(n int64) bool { return n%2 != 0 },
        func(n *big.Int) bool { return n.Bit(0) == 1 })
}

func PrimEvenQ(_ context.Context, mc *machine.MachineContext) error {
    return parityCheck(mc, "even?",
        func(n int64) bool { return n%2 == 0 },
        func(n *big.Int) bool { return n.Bit(0) == 0 })
}
```

**Files to modify**:
- `registry/core/prim_predicates.go`

**Impact**: Reduces ~64 lines to ~30 lines + helper

---

### 6. Registry Helper Duplicates (2 duplicates)

**Location**: `registry/helpers/char.go` and `registry/helpers/string.go`

**Duplicates**:
- Lines 15-71 in both files

**Pattern**: Both files likely have identical helper functions for variadic comparison registration

**Solution**: Need to read these files to understand the duplication, then either:
- Consolidate into a generic helper if the pattern is similar to #1
- Or extract shared registration logic

**Files to modify**:
- `registry/helpers/char.go`
- `registry/helpers/string.go`

**Action**: Requires investigation

---

### 7. Number Type Switch Duplicates (Multiple)

**Location**: `values/big_complex.go`, `values/big_float.go`

**Duplicates**:
- `big_complex.go`: lines 321-347, 359-385
- `big_float.go`: lines 102-123, 132-153, 165-186, 195-216

**Pattern**: Type switches over numeric types with similar case handling

**Solution**: Extract common numeric type conversion/comparison logic

**Files to modify**:
- `values/big_complex.go`
- `values/big_float.go`

**Action**: Requires investigation to understand the specific operations

---

### 8. Match Package Duplicates (2 duplicates)

**Location**: `match/match.go`

**Duplicates**:
- Lines 162-224
- Lines 392-454

**Pattern**: Bytecode execution logic

**Solution**: Extract common bytecode handling into a helper function

**Files to modify**:
- `match/match.go`

**Action**: Requires investigation

---

## Implementation Strategy

### Phase 1: High-Value, Low-Risk (Start Here)

1. **Output port helpers** (#2, #3)
   - Clear pattern
   - Low risk
   - Significant line reduction
   - Files: `extensions/io/prim_read_write.go`

2. **Parity checks** (#5)
   - Simple pattern
   - Easy to verify correctness
   - Files: `registry/core/prim_predicates.go`

### Phase 2: Generic Helpers

3. **Variadic comparison** (#1)
   - Uses Go generics
   - Clear pattern
   - Files: `extensions/all/prim_all.go`

4. **Integer division** (#4)
   - More complex but well-defined
   - Files: `registry/core/prim_arithmetic.go`

### Phase 3: Investigation Required

5. **Registry helpers** (#6)
   - Need to read files first
   - Files: `registry/helpers/*.go`

6. **Numeric type switches** (#7)
   - Need to understand operations
   - Files: `values/big_*.go`

7. **Match bytecode** (#8)
   - Lowest priority (complex VM logic)
   - Files: `match/match.go`

## Testing Strategy

For each refactoring:

1. Run existing tests before changes: `go test ./...`
2. Make refactoring changes
3. Run tests again to verify no regressions
4. Run `make lint` to verify duplication eliminated
5. Manually verify primitives still work via REPL testing

## Success Criteria

- All 39 duplicate code warnings resolved
- All existing tests pass
- No new linter warnings introduced
- Code is more maintainable (fewer lines, clearer patterns)

## Risks

- **Generics complexity**: May reduce readability if overused
- **Performance**: Generic helpers may have slight overhead (negligible for Scheme interpreter)
- **Testing coverage**: Some primitives may not have comprehensive tests

## Remaining Duplicates (26 total)

### Values Package Arithmetic Operations (24 duplicates)

These are similar type-switch patterns in arithmetic methods across numeric types:

- **integer.go**: 6 duplicates in Add/Subtract/Multiply (3 pairs)
- **big_integer.go**: 6 duplicates in Add/Subtract/Multiply (3 pairs)
- **float.go**: 2 duplicates in Add/Subtract
- **big_float.go**: 6 duplicates (3 pairs)
- **rational.go**: 6 duplicates (3 pairs)
- **complex.go**: 2 duplicates in Subtract/Multiply
- **big_complex.go**: 2 duplicates

**Pattern**: Each numeric type has Add, Subtract, Multiply, Divide methods that follow similar patterns:
```go
func (p *Integer) Add(o Number) Number {
    if o.IsZero() { return p }
    switch v := o.(type) {
    case *Integer: return NewInteger(p.Value + v.Value)
    case *BigInteger: // promote to BigInteger
    case *Float: // promote to Float
    // ... other cases
    }
}
```

**Complexity**: High - requires understanding the numeric tower promotion rules and exactness preservation

### Match Package (2 duplicates)

- **match/match.go**: Lines 162-224 duplicate of 392-454 (bytecode execution in pattern matcher VM)

**Complexity**: High - VM bytecode execution logic, requires deep understanding of the pattern matcher

## Next Steps

These remaining duplicates are significantly more complex than those already completed. Recommendations:

1. **Values arithmetic**: Would benefit from a comprehensive refactoring of the numeric tower, possibly extracting common promotion logic
2. **Match bytecode**: Requires deep VM knowledge; defer until pattern matcher is better understood
3. **Cost/benefit**: The 13 duplicates eliminated (33% reduction) covered the simpler, high-value cases. Remaining duplicates are in performance-critical paths and may not benefit as much from extraction
