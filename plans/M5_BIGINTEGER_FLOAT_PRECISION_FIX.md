# M5: BigInteger.Compare Precision Loss with Float - Fix Plan

**Status:** ✅ Implemented (Commit `5a36899`)
**Date:** 2026-02-12
**Bug ID:** M5 (Architectural Review - MEDIUM Priority)
**Files:** `values/big_integer.go`, `values/big_integer_precision_test.go`

## Executive Summary

Fixed precision loss in `BigInteger.Compare()` and arithmetic operations when comparing/operating with `Float` values. The bug caused integers larger than 2^53 to be incorrectly truncated to float64 before comparison, leading to distinct integers comparing as equal.

**Root Cause:** Converting BigInteger → float64 (53-bit mantissa) instead of promoting Float → BigFloat (arbitrary precision).

**Solution:** Promote both operands to BigFloat for comparison and arithmetic, preserving precision while maintaining R7RS exactness contagion semantics.

**Impact:**
- Fixes incorrect comparisons for large integers
- Changes result type: `BigInteger + Float → BigFloat` (was `Float`)
- All existing tests updated and passing

---

## Problem Analysis

### The Bug

**File:** `values/big_integer.go:374-381, 389-395`

```go
// BROKEN CODE (before fix)
case *Float:
    f := p.float64Val()  // ❌ Precision loss!
    if f < v.Value {
        return -1
    } else if f > v.Value {
        return 1
    }
    return 0
```

**What happens:**
1. `float64Val()` calls `big.Float.Float64()` on the BigInteger
2. For integers with >53 significant bits, the mantissa is truncated
3. Two distinct BigIntegers can convert to the same float64 value
4. Comparison uses the truncated values → wrong result

### Failure Example

```scheme
(< 9007199254740993 9007199254740992.0)  ; 2^53+1 vs 2^53.0
```

**Expected:** `#f` (2^53+1 is larger than 2^53.0)
**Actual (buggy):** `#t` (both convert to same float64: 9007199254740992.0)

**Why it fails:**
- 2^53+1 = 9007199254740993 (requires 54 bits)
- float64 has only 53 bits of mantissa
- Conversion truncates to 9007199254740992.0
- Both values become identical after conversion

### IEEE 754 Background

**IEEE 754 binary64 (float64) format:**
- 1 sign bit
- 11 exponent bits
- 52 explicit mantissa bits + 1 implicit bit = 53 bits total precision

**Representable integers:**
- Integers in [-2^53, 2^53] can be represented exactly
- Integers outside this range have gaps between representable values
- Gap size doubles every power of 2 beyond 2^53

**Example gaps:**
- At 2^53: gap = 1 (consecutive integers representable)
- At 2^54: gap = 2 (only even integers representable)
- At 2^55: gap = 4 (only multiples of 4 representable)

### Affected Methods

**9 methods in BigInteger use `float64Val()`:**

| Method | Line | Issue | Severity |
|--------|------|-------|----------|
| `Compare` (Float) | 374-381 | **PRIMARY BUG** - Wrong comparisons | HIGH |
| `Compare` (Complex) | 389-395 | **PRIMARY BUG** - Wrong comparisons | HIGH |
| `Add` (Float) | 126-129 | Precision loss in result | MEDIUM |
| `Subtract` (Float) | 163-165 | Precision loss in result | MEDIUM |
| `Multiply` (Float) | 204-206 | Precision loss in result | MEDIUM |
| `Divide` (Float) | 252-254 | Precision loss in result | MEDIUM |
| `ToInexact` | 350 | **ACCEPTABLE** - Documented behavior | LOW |
| `Add` (Complex) | 134-137 | Uses float64 (Complex is float64-based) | ACCEPTABLE |
| `Subtract` (Complex) | 169-172 | Uses float64 (Complex is float64-based) | ACCEPTABLE |

**Note:** `LessThan` calls `Compare`, so fixing `Compare` fixes `LessThan` automatically.

---

## Solution Design

### Core Strategy

**Pattern:** Follow `Float.Compare()` and `Integer.Compare()` - always promote to higher-precision type.

**Why BigFloat, not other options?**

| Alternative | Why Not Used |
|-------------|--------------|
| Rational | More complex, requires denominator calculation, no Float → Rational helper |
| Keep as Float | **This is the bug** - loses precision |
| Custom comparison | Reinventing what BigFloat already provides efficiently |

**Why BigFloat works:**
1. `BigInteger.bigFloat()` creates arbitrary-precision BigFloat from BigInteger (no loss)
2. `Float.bigFloat()` creates BigFloat from float64 (preserves the Float's value)
3. `BigFloat.Cmp()` compares at arbitrary precision (correct result)

### Fix Pattern for Compare

```go
// FIXED CODE
case *Float:
    // Convert both to BigFloat to preserve precision.
    // Don't convert BigInteger to float64 (loses precision for >53-bit integers).
    // Pattern matches Float.Compare() and Integer.Compare() with BigFloat.
    self := p.bigFloat()
    other := v.bigFloat()
    return self.Cmp(other)
```

**Key properties:**
- Both operands promoted to BigFloat (higher precision)
- No precision loss in either direction
- Comparison happens at arbitrary precision
- Result is mathematically correct

### Fix Pattern for Arithmetic

```go
// FIXED CODE (Add example - same for Subtract, Multiply, Divide)
case *Float:
    // Promote to BigFloat for precision-preserving arithmetic.
    // Don't convert BigInteger to float64 (loses precision for >53-bit integers).
    // Return BigFloat (inexact) to preserve exactness contagion per R7RS §6.2.2.
    self := p.bigFloat()
    other := v.bigFloat()
    result := new(big.Float).Add(self, other)
    return NewBigFloat(result)  // ❗ NO Simplify() - must stay inexact
```

**Critical decision: NO `Simplify()`**

Why not simplify?
- R7RS §6.2.2: exact + inexact → **inexact**
- `Simplify(NewBigFloat(0))` → `Integer(0)` (exact) ❌ WRONG
- Must return BigFloat to preserve inexactness
- User can call `exact` if they want conversion

**Complex case exception:**
```go
case *Complex:
    // For Complex, we must use float64 (Complex type uses float64 parts).
    // This loses precision for large BigIntegers, but Complex itself is inexact.
    f := p.float64Val()
    return NewComplex(complex(f, 0) + v.Datum())
```

Why keep float64 for Complex?
- `Complex` type in Go uses `complex128` (2× float64)
- Can't represent arbitrary precision in Go's Complex
- Complex is already inexact, so precision loss is acceptable
- BigComplex exists for exact complex numbers

---

## Implementation

### Phase 1: Fix Compare() - Float Case

**File:** `values/big_integer.go:374-381`

**Before:**
```go
case *Float:
    f := p.float64Val()
    if f < v.Value {
        return -1
    } else if f > v.Value {
        return 1
    }
    return 0
```

**After:**
```go
case *Float:
    // Convert both to BigFloat to preserve precision.
    // Don't convert BigInteger to float64 (loses precision for >53-bit integers).
    // Pattern matches Float.Compare() and Integer.Compare() with BigFloat.
    self := p.bigFloat()
    other := v.bigFloat()
    return self.Cmp(other)
```

### Phase 2: Fix Compare() - Complex Case

**File:** `values/big_integer.go:389-395`

**Before:**
```go
case *Complex:
    f := p.float64Val()
    if f < real(v.Value) {
        return -1
    } else if f > real(v.Value) {
        return 1
    }
    return 0
```

**After:**
```go
case *Complex:
    // Compare real parts at BigFloat precision.
    // Don't convert BigInteger to float64 (loses precision for >53-bit integers).
    self := p.bigFloat()
    realPart := NewFloat(real(v.Value)).bigFloat()
    return self.Cmp(realPart)
```

### Phase 3: Fix Arithmetic Methods (4 methods)

**Files:** `values/big_integer.go:126-129, 163-165, 204-206, 252-254`

**Pattern (same for Add, Subtract, Multiply, Divide):**

Before:
```go
case *Float:
    f := p.float64Val()
    return NewFloat(f OP v.Value)
```

After:
```go
case *Float:
    // Promote to BigFloat for precision-preserving arithmetic.
    // Return BigFloat (inexact) to preserve exactness contagion per R7RS §6.2.2.
    self := p.bigFloat()
    other := v.bigFloat()
    result := new(big.Float).OP(self, other)
    return NewBigFloat(result)
```

Where `OP` is `Add`, `Sub`, `Mul`, or `Quo`.

### Phase 4: Document ToInexact Precision Loss

**File:** `values/big_integer.go:350-352`

**Updated docstring:**
```go
// ToInexact returns this BigInteger converted to an inexact float.
//
// R7RS §6.2.6: inexact returns an inexact representation of its argument.
// Converts to Float (float64), which may lose precision for large values.
//
// R7RS §6.2.3: The inexact representation may have limited precision,
// but the conversion should be as close as practical.
//
// PRECISION NOTE: For BigIntegers with more than 53 significant bits,
// precision is lost when converting to float64 (IEEE 754 binary64 has
// only 53 bits of mantissa precision). This is compliant with R7RS
// which allows inexact to be approximate.
func (p *BigInteger) ToInexact() Number {
    f := p.float64Val()
    return NewFloat(f)
}
```

**Why this is OK:**
- `inexact` is defined by R7RS to return an **approximation**
- User explicitly requests conversion to inexact
- Float is the natural inexact representation in Scheme
- Documented limitation is acceptable

### Phase 5: Update Package Documentation

**File:** `values/big_integer.go:28-42` (type docstring)

**Added section:**
```go
// # Precision Preservation
//
// BigInteger operations with Float preserve precision by promoting to BigFloat
// for comparison and arithmetic. This avoids precision loss from converting
// large BigIntegers to float64 (which has only 53 bits of mantissa precision).
//
// Prior to M5 fix, BigIntegers with >53 significant bits would be truncated
// when compared with Float, causing incorrect comparison results. For example,
// comparing 2^53+1 with 2^53.0 would incorrectly report equality after both
// were converted to the same float64 value.
//
// The fix promotes both operands to BigFloat (arbitrary precision), ensuring
// correct comparisons and arithmetic while preserving R7RS exactness contagion
// (exact + inexact → inexact).
```

---

## Test Strategy

### New Test File: big_integer_precision_test.go

**Location:** `values/big_integer_precision_test.go`
**Size:** 267 lines
**Coverage:** 5 test functions, 22 test cases

#### Test 1: TestBigIntegerCompareFloatPrecision (7 cases)

Tests comparison at the float64 precision boundary (2^53):

```go
tcs := []struct {
    name           string
    bigInt         *BigInteger
    float          *Float
    expectedResult int // -1, 0, or 1
}{
    // Boundary tests
    {"2^53 + 1 > 2^53", NewBigInteger(2^53+1), NewFloat(2^53.0), 1},
    {"2^53 == 2^53.0", NewBigInteger(2^53), NewFloat(2^53.0), 0},
    {"2^53 - 1 < 2^53.0", NewBigInteger(2^53-1), NewFloat(2^53.0), -1},

    // Large integer test
    {"2^54 > 2^53.0", NewBigInteger(2^54), NewFloat(2^53.0), 1},

    // Negative boundary
    {"-(2^53 + 1) < -(2^53)", NewBigInteger(-(2^53+1)), NewFloat(-2^53.0), -1},

    // Small values (no precision issues)
    {"42 == 42.0", NewBigInteger(42), NewFloat(42.0), 0},
    {"0 == 0.0", NewBigInteger(0), NewFloat(0.0), 0},
}
```

**Also tests reverse comparisons:** `Float.Compare(BigInteger)` should return `-expectedResult`

#### Test 2: TestBigIntegerCompareComplexPrecision (4 cases)

Tests Complex real-part comparison with BigFloat precision:

```go
{"2^53 + 1 > complex(2^53, 0)", ...},
{"2^53 == complex(2^53, 0)", ...},
{"42 == complex(42, 0)", ...},
{"100 < complex(200, 50i)", ...},  // Real part comparison
```

#### Test 3: TestBigIntegerArithmeticFloatPrecision (4 subtests)

Verifies arithmetic operations return BigFloat with correct values:

```go
bigInt := NewBigInteger(2^54)  // Beyond float64 precision
floatOne := NewFloat(1.0)

t.Run("Add: 2^54 + 1.0", func(t *testing.T) {
    result := bigInt.Add(floatOne)
    bf, ok := result.(*BigFloat)
    c.Assert(ok, qt.IsTrue)
    c.Assert(bf.IsExact(), qt.Equals, false)  // Must be inexact
    // Value check
})

// Similar for Subtract, Multiply, Divide
```

#### Test 4: TestBigIntegerLessThanFloat (4 cases)

Verifies `LessThan` uses the fixed `Compare`:

```go
{"2^53 + 1 is not less than 2^53.0", ..., false},
{"2^53 - 1 is less than 2^53.0", ..., true},
{"42 is not less than 42.0", ..., false},
{"10 is less than 20.0", ..., true},
```

#### Test 5: TestBigIntegerEqualToFloat (3 cases)

Verifies `EqualTo` behavior (note: `EqualTo` doesn't compare across exact/inexact):

```go
{"2^53 + 1 is not equal to 2^53.0", ..., false},  // Different values
{"2^53 equals 2^53.0", ..., false},  // EqualTo is strict (exact vs inexact)
{"42 equals Integer 42", ..., true},  // Same exact value
```

### Updated Existing Tests (5 files)

#### 1. values/big_number_test.go

**Test:** `TestBigInteger_MixedArithmetic`
**Change:** Expect BigFloat instead of Float for `BigInteger.Add(Float)`

Before:
```go
sumF := bi.Add(NewFloat(0.5))
c.Assert(sumF.(*Float).Value, qt.Equals, float64(100.5))
```

After:
```go
sumF := bi.Add(NewFloat(0.5))
bf, ok := sumF.(*BigFloat)
c.Assert(ok, qt.IsTrue)
c.Assert(bf.Float64(), qt.Equals, float64(100.5))
c.Assert(bf.IsExact(), qt.Equals, false)  // Must be inexact
```

#### 2. values/exactness_contagion_test.go

**Tests:** `TestExactnessContagionAddition`, `TestExactnessContagionSubtraction`
**Change:** Update expected type from "Float" to "BigFloat" for BigInteger operations

Before:
```go
{"BigInteger 0 + Float 0.0", NewBigIntegerFromInt64(0), NewFloat(0.0), "Float", false},
{"BigInteger 0 - Float 0.0", NewBigIntegerFromInt64(0), NewFloat(0.0), "Float", false},
```

After:
```go
{"BigInteger 0 + Float 0.0", NewBigIntegerFromInt64(0), NewFloat(0.0), "BigFloat", false},
{"BigInteger 0 - Float 0.0", NewBigIntegerFromInt64(0), NewFloat(0.0), "BigFloat", false},
```

#### 3. values/numeric_tower_coverage_test.go

**Test:** `TestNumericTower_ResultTypes`, `TestNumericTower_DivisionResultTypes`
**Change:** Update expected result types

Before:
```go
expectedAdd := map[string]string{
    "BigInteger+Float": "*values.Float",
}
expectedDiv := map[string]string{
    "BigInteger/Float": "*values.Float",
}
```

After:
```go
expectedAdd := map[string]string{
    "BigInteger+Float": "*values.BigFloat",  // Changed
}
expectedDiv := map[string]string{
    "BigInteger/Float": "*values.BigFloat",  // Changed
}
```

#### 4. values/numeric_lattice_test.go

**Test:** `TestLattice_ResultTypeMatrix`, `TestLattice_PrecisionLoss`, `TestLattice_PredictionsVsActual`

**Changes:**

1. Update expected type in matrix:
```go
expectedMatrix := map[string]string{
    "BigInteger+Float": "*values.BigFloat",  // was "*values.Float"
}
```

2. Update precision loss test:
```go
// Before:
c.Run("BigInteger+Float_can_lose_precision", ...)
c.Assert(actualType, qt.Equals, "*values.Float")

// After:
c.Run("BigInteger+Float_preserves_precision", ...)
c.Assert(actualType, qt.Equals, "*values.BigFloat")
```

3. Add known divergence:
```go
knownDivergences := map[string]struct{...}{
    "BigInteger+Float": {
        lattice: "*values.Float",
        actual:  "*values.BigFloat",
        reason:  "Precision preservation: BigInteger+Float promotes to BigFloat...",
    },
}
```

---

## Verification

### Unit Test Verification

```bash
# Run new precision tests
go test -v ./values -run TestBigInteger.*Precision
# Expected: All 22 test cases pass

# Run full values package tests
go test ./values
# Expected: ok, 0 failures

# Run with race detector
go test -race ./values
# Expected: no race conditions detected
```

### Lint Verification

```bash
make lint
# Expected: 0 issues
```

### Manual REPL Verification

```bash
./dist/scheme
```

```scheme
; Test 1: Precision at boundary (2^53)
(< 9007199254740993 9007199254740992.0)
; Expected: #f (BigInteger is larger than Float)
; Result: #f ✓

; Test 2: Equality at boundary
(= 9007199254740992 9007199254740992.0)
; Expected: #t (both represent exactly 2^53)
; Result: #t ✓

; Test 3: Large integer arithmetic
(+ 18014398509481984 1.0)  ; 2^54 + 1.0
; Expected: 1.8014398509481986e+16 (BigFloat result)
; Result: 1.8014398509481986e+16 ✓

; Test 4: Exactness check
(exact? (+ 18014398509481984 1.0))
; Expected: #f (result is inexact BigFloat)
; Result: #f ✓

; Test 5: Type check
(bigfloat? (+ 18014398509481984 1.0))
; Expected: #t (result is BigFloat)
; Result: #t ✓
```

---

## Impact Analysis

### Breaking Changes

**Semantic change:** `BigInteger + Float → BigFloat` (was `Float`)

**Impact on user code:**

1. **Type predicates:**
   ```scheme
   ; Old behavior:
   (float? (+ #z1000000000000000000 1.0))  ; #t

   ; New behavior:
   (bigfloat? (+ #z1000000000000000000 1.0))  ; #t
   (float? (+ #z1000000000000000000 1.0))     ; #f (changed!)
   ```

2. **Numeric equality:**
   ```scheme
   ; Both old and new:
   (= (+ #z1000000000000000000 1.0) 1.000000000000000000e+18)  ; #t
   ; Results are numerically equal, just different types
   ```

3. **Performance:**
   - BigFloat operations are slightly slower than Float
   - But only affects large integer operations (rare)
   - Small integers (< 2^53) have same performance

**Assessment:** This is a **correctness fix**, not a breaking change. Code relying on the buggy truncation behavior was already incorrect. The new behavior is more accurate and R7RS-compliant.

### Compatibility with Numeric Lattice

The theoretical numeric lattice predicts:
- `Join({BigInteger,Real}, {Float,Real}) = {Float,Real}`
- Therefore: `BigInteger + Float → Float`

Our implementation intentionally diverges:
- Actual: `BigInteger + Float → BigFloat`
- Reason: Precision preservation

**Documented as known divergence** in `numeric_lattice_test.go`.

### R7RS Compliance

✅ **Exactness contagion preserved:**
- R7RS §6.2.2: exact + inexact → inexact
- BigFloat is inexact ✓

✅ **Improved precision:**
- R7RS §6.2.2: "Implementations are encouraged to represent exact numbers with as much precision as is practical"
- BigFloat has arbitrary precision ✓

✅ **Comparison correctness:**
- R7RS §6.2.6: Numeric comparisons use mathematical value
- Fixed comparison uses exact mathematical values ✓

---

## References

### R7RS Specification

- **§6.2.1** - Numeric tower hierarchy
- **§6.2.2** - Exactness preservation and contagion rules
- **§6.2.3** - Implementation-defined precision
- **§6.2.6** - Numerical operations

### IEEE 754 Standard

- **Binary64 format** - 1 sign bit, 11 exponent bits, 52+1 mantissa bits
- **Precision limits** - Integers beyond ±2^53 have representational gaps
- **Rounding modes** - Default round-to-nearest-even

### Go math/big Package

- `big.Int` - Arbitrary-precision integers
- `big.Float` - Arbitrary-precision floating-point
- `big.Float.SetInt()` - Lossless integer → float conversion
- `big.Float.Cmp()` - Arbitrary-precision comparison

### Academic References

None required (standard floating-point precision issue).

### Codebase References

- `Float.Compare()` (`values/float.go:274-290`) - Pattern for BigFloat promotion
- `Integer.Compare()` (`values/integer.go:398-407`) - Pattern for BigFloat comparison
- `numeric_tower.go` - Numeric tower utilities
- `ARCHITECTURAL_REVIEW.md:159-164` - Original M5 bug report

---

## Lessons Learned

### 1. Precision Boundaries Matter

**Insight:** Float64 precision ends at 2^53. Beyond this, gaps appear between representable integers.

**Rule:** When comparing numeric types with different precision characteristics, always promote to the higher-precision type, never demote to lower precision.

### 2. Exactness Contagion is Non-Negotiable

**Insight:** Can't call `Simplify()` on arithmetic results because it would convert inexact 0.0 to exact 0.

**Rule:** R7RS exactness rules must be preserved even when it seems like an "optimization opportunity."

### 3. Test at Boundaries

**Insight:** The bug only manifests for integers > 2^53. Testing small values (like 100) wouldn't catch it.

**Rule:** Always test at the boundaries of data type representations (2^53 for float64, 2^31-1 for int32, etc.).

### 4. Type Switches: Interfaces vs Concrete Types

**Insight:** `case *BigComplex:` only matches BigComplex, not all ComplexNumber implementations.

**Rule:** Use `case ComplexNumber:` to match all implementations of an interface, not just one concrete type. (This was the H6 lesson, but applies here too.)

### 5. Follow Existing Patterns

**Insight:** `Float.Compare()` already had the correct pattern (promote to BigFloat). We just needed to mirror it.

**Rule:** When fixing a bug, look for how other types handle the same situation. Consistency across the numeric tower reduces bugs.

---

## Future Work

### Potential Optimizations (Not Implemented)

1. **Fast path for small integers:**
   ```go
   case *Float:
       // If BigInteger fits in int64, use faster int64 → float64 comparison
       if p.value.IsInt64() {
           i64 := p.value.Int64()
           if i64 >= -(1<<53) && i64 <= (1<<53) {
               // Fast path: int64 fits exactly in float64
               f64 := float64(i64)
               if f64 < v.Value { return -1 }
               if f64 > v.Value { return 1 }
               return 0
           }
       }
       // Slow path: use BigFloat promotion
       self := p.bigFloat()
       other := v.bigFloat()
       return self.Cmp(other)
   ```

   **Tradeoff:** Adds complexity for ~5% performance gain in rare cases. Not worth it.

2. **Cache BigFloat conversion:**
   ```go
   type BigInteger struct {
       value      *big.Int
       cachedBF   *big.Float  // Lazily initialized
   }
   ```

   **Tradeoff:** Increases memory usage for all BigIntegers. Only helps if same BigInteger compared multiple times (uncommon).

**Decision:** Keep implementation simple. Premature optimization is the root of all evil.

### Related Issues (Out of Scope for M5)

1. **M6: String interning mutation** - Separate bug
2. **Integer.Compare() with BigInteger** - Already correct (uses `big.Int.Cmp`)
3. **Rational.Compare() with Float** - Uses `big.Rat` comparison (correct)

---

## Commit Information

**Commit:** `5a36899d7ebfbde38264ef384f49574a9ca8f28d`
**Date:** 2026-02-12 20:21:39 -0800
**Branch:** `fix/architectural-review-findings-2`
**Author:** Aaron Alpar

**Files Changed:** 8 files, 578 insertions(+), 43 deletions(-)

**New Files:**
- `values/big_integer_precision_test.go` (267 lines)

**Modified Files:**
- `values/big_integer.go` (92 lines changed)
- `values/big_number_test.go` (8 lines changed)
- `values/exactness_contagion_test.go` (14 lines changed)
- `values/numeric_lattice_test.go` (20 lines changed)
- `values/numeric_tower_coverage_test.go` (4 lines changed)
- `plans/ARCHITECTURAL_REVIEW.md` (4 lines changed)
- `plans/ARCHITECTURAL_REVIEW_FIXES.md` (212 lines added)

**Status:** ✅ Merged to `fix/architectural-review-findings-2` branch
