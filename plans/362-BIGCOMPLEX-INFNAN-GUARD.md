# Issue #362: Complex.Add/Sub/Mul/Div with Float(Inf/NaN) drops imaginary part when LUB is BigComplex

## Bug

When a `Float` holding `+inf.0`, `-inf.0`, or `+nan.0` is combined arithmetically with a `BigComplex`, the dispatch guard in `makeArithmeticDispatch` short-circuits via `NewFloat(float64Op(...))`, silently discarding the imaginary component.

```scheme
(+ +inf.0 (make-rectangular 3 4))
; Expected: +inf.0+4i
; Actual:   +inf.0     (imaginary part lost)
```

## Root Cause

The promotion table says `Float × BigComplex → KindBigComplex`. Since BigFloat/BigComplex can't represent Inf/NaN, a guard in `makeArithmeticDispatch` (`values/promotion.go:345-368`) short-circuits when a Float operand has a special value:

```go
return NewFloat(float64Op(numberToFloat64(p), numberToFloat64(o)))
```

`numberToFloat64` on a `BigComplex` extracts only `v.real` (`promotion.go:285-286`), discarding the imaginary part. The result is also the wrong type — `Float` instead of `Complex`.

### Which dispatch slots are affected?

Only one LUB combination triggers this: **`Float × BigComplex → KindBigComplex`** (and its symmetric counterpart when BigComplex is the receiver).

- `Float × Complex → KindComplex` — `lubNeedsGuard = false`, guard never fires. OK.
- `Float × BigFloat → KindBigFloat` — real-only, no imaginary to lose. OK.
- `Float × BigComplex → KindBigComplex` — **BUG**.

### Why the fix isn't trivial

The obvious thought is "apply `float64Op` component-wise to real and imaginary parts." That works for addition/subtraction but fails for multiplication:

- `(inf+0i) * (3+4i)` = `(inf·3 - 0·4) + (inf·4 + 0·3)i` = `inf + inf·i`
- Component-wise `float64Op`: real = `inf*3 = inf`, imag = `0*4 = 0` → `inf+0i` — **wrong**.

Complex multiplication isn't component-wise, so we can't derive the correct complex128 operation from `float64Op` alone.

## Design

**Only `values/promotion.go` is modified.** No other files.

### Step 1: Add `numberToComplex128` helper

Place immediately after `numberToFloat64` (after line 288). Analogous to `numberToFloat64`. Converts any Number to complex128, extracting both real and imaginary parts. Real-only types get imaginary = 0.

```go
// numberToComplex128 converts any Number to complex128 for IEEE 754 special-value
// guard paths. See plans/PRECISION-GUARANTEES.md Tier 4 for precision justification.
func numberToComplex128(n Number) complex128 {
	switch v := n.(type) {
	case *Integer:
		return complex(float64(v.Value), 0)
	case *BigInteger:
		return complex(float64FromBigInt(v.value), 0)
	case *Float:
		return complex(v.Value, 0)
	case *BigFloat:
		return complex(v.Float64(), 0)
	case *Rational:
		return complex(v.Float64(), 0)
	case *Complex:
		return v.Value
	case *BigComplex:
		return complex(toBigFloat(v.real).Float64(), toBigFloat(v.imag).Float64())
	}
	panic(ErrNotANumber)
}
```

**Precision notes for each case (all acceptable — Tier 4):**

| Case | Conversion | Precision loss |
|------|-----------|----------------|
| `*Integer` | `float64(v.Value)` | Truncates int64 > 2^53. Acceptable: Inf/NaN dominates. |
| `*BigInteger` | `float64FromBigInt(v.value)` | Arbitrary precision → 53-bit mantissa. Same helper as `numberToFloat64`. |
| `*Float` | `v.Value` | No loss — already float64. Preserves Inf/NaN. |
| `*BigFloat` | `v.Float64()` | `BigFloat.Float64()` returns single `float64` (wraps `big.Float.Float64()`, discards `big.Accuracy`). Defined at `values/big_float.go:63-66`. |
| `*Rational` | `v.Float64()` | `Rational.Float64()` returns single `float64` (wraps `big.Rat.Float64()`, discards `big.Accuracy`). Defined at `values/rational.go:86-89`. |
| `*Complex` | `v.Value` | No loss — already complex128. |
| `*BigComplex` | `toBigFloat(v.real).Float64()` for each part | Both real and imag truncated to 53-bit float64. `toBigFloat` (at `values/big_complex.go:94-106`) handles `*BigFloat`, `*BigInteger`, `*Rational` — these are the only types that `BigComplex.real`/`.imag` can hold. `BigFloat.Float64()` returns a single `float64`. |

### Step 2: Add `complex128Op` parameter to `makeArithmeticDispatch`

Change the function signature at line 323. Add one new parameter after `float64Op`:

**Before:**
```go
func makeArithmeticDispatch[T Number](
	srcKind NumericKind,
	sameTypeOp func(T, Number) Number,
	applyOp func(Number, Number) Number,
	float64Op func(float64, float64) float64,
) [numKinds]func(T, Number) Number {
```

**After:**
```go
func makeArithmeticDispatch[T Number](
	srcKind NumericKind,
	sameTypeOp func(T, Number) Number,
	applyOp func(Number, Number) Number,
	float64Op func(float64, float64) float64,
	complex128Op func(complex128, complex128) complex128,
) [numKinds]func(T, Number) Number {
```

### Step 3: Modify both guard branches to check `lubKind`

When the Inf/NaN guard fires and the LUB is `KindBigComplex`, use `complex128Op` with `numberToComplex128` instead of `float64Op` with `numberToFloat64`. The result type changes from `NewFloat` to `NewComplex`.

**Why only `KindBigComplex`?** `lubNeedsGuard` is `true` when `lubKind != KindFloat && lubKind != KindComplex`. The complex LUB kinds are `KindComplex` and `KindBigComplex`. Since `KindComplex` makes `lubNeedsGuard = false`, only `KindBigComplex` reaches the guard AND needs complex arithmetic. Checking `lubKind == KindBigComplex` is the complete and exact condition.

**Closure variable capture:** `lubIsComplex` is declared as a local `bool` in the switch case body (outside the closure). Go closures capture variables by reference, but since `lubKind` is computed fresh per loop iteration (`lubKind := promotionTable[srcKind][dstKind]`), and `lubIsComplex` is a new local per iteration, each closure captures its own correct value.

Replace the entire switch block (lines ~345-368) with:

```go
		switch {
		case srcKind == KindFloat && lubNeedsGuard:
			// Receiver is Float, might have Inf/NaN.
			lubIsComplex := lubKind == KindBigComplex
			table[dstKind] = func(p T, o Number) Number {
				if isSpecialFloat(any(p).(*Float)) {
					if lubIsComplex {
						return NewComplex(complex128Op(
							numberToComplex128(p),
							numberToComplex128(o),
						))
					}
					return NewFloat(float64Op(numberToFloat64(p), numberToFloat64(o)))
				}
				return applyOp(promSrc(p), promDst(o))
			}
		case dstKind == KindFloat && lubNeedsGuard:
			// Operand is Float, might have Inf/NaN.
			lubIsComplex := lubKind == KindBigComplex
			table[dstKind] = func(p T, o Number) Number {
				if isSpecialFloat(o.(*Float)) {
					if lubIsComplex {
						return NewComplex(complex128Op(
							numberToComplex128(p),
							numberToComplex128(o),
						))
					}
					return NewFloat(float64Op(numberToFloat64(p), numberToFloat64(o)))
				}
				return applyOp(promSrc(p), promDst(o))
			}
		default:
			table[dstKind] = func(p T, o Number) Number {
				return applyOp(promSrc(p), promDst(o))
			}
		}
```

**Key difference between the two guard branches:**
- `srcKind == KindFloat`: The receiver `p` is Float → check `isSpecialFloat(any(p).(*Float))`
- `dstKind == KindFloat`: The operand `o` is Float → check `isSpecialFloat(o.(*Float))`

The `any(p)` cast is needed in the `srcKind` branch because `p` is generic type `T`; the `o` cast is direct because `o` is already `Number`.

### Step 4: Update the 4 arithmetic dispatch callers

Each `makeXxxDispatch` currently passes 3 arguments to `makeArithmeticDispatch` (after `srcKind` and `sameTypeOp`). Add a 4th argument: the `complex128Op` closure.

**All 4 closures must use multi-line function bodies** per project convention (CLAUDE.md: "NEVER write single-line function definitions — applies to ALL function forms including closures").

#### `makeAddDispatch` (lines 372-381)

**Before:**
```go
func makeAddDispatch[T Number](srcKind NumericKind, sameTypeAdd func(T, Number) Number) [numKinds]func(T, Number) Number {
	return makeArithmeticDispatch(srcKind, sameTypeAdd,
		func(a, b Number) Number {
			return a.Add(b)
		},
		func(a, b float64) float64 {
			return a + b
		},
	)
}
```

**After:**
```go
func makeAddDispatch[T Number](srcKind NumericKind, sameTypeAdd func(T, Number) Number) [numKinds]func(T, Number) Number {
	return makeArithmeticDispatch(srcKind, sameTypeAdd,
		func(a, b Number) Number {
			return a.Add(b)
		},
		func(a, b float64) float64 {
			return a + b
		},
		func(a, b complex128) complex128 {
			return a + b
		},
	)
}
```

#### `makeSubtractDispatch` (lines 384-393)

Add after the `float64` closure:
```go
		func(a, b complex128) complex128 {
			return a - b
		},
```

#### `makeMultiplyDispatch` (lines 396-405)

Add after the `float64` closure:
```go
		func(a, b complex128) complex128 {
			return a * b
		},
```

#### `makeDivideDispatch` (lines 408-417)

Add after the `float64` closure:
```go
		func(a, b complex128) complex128 {
			return a / b
		},
```

### Step 5: No changes to LessThan/Compare dispatches

`makeLessThanDispatch` (line 425) and `makeCompareDispatch` (line 472) have their own signatures — they return `bool`/`int`, not `Number`, and do NOT call `makeArithmeticDispatch`. They have their own guard logic using `numberToFloat64` for float64 comparisons.

These guards are fine as-is:
- R7RS doesn't define `<` on complex numbers.
- If someone tries `(< +inf.0 (make-rectangular 3 4))`, comparing only real parts (via `numberToFloat64`) is the existing behavior and is acceptable for an undefined operation.

**Do NOT modify `makeLessThanDispatch` or `makeCompareDispatch`.**

## Result Type: `NewComplex` not `NewBigComplex`

The guard produces a `*Complex` (complex128-based) value, NOT `*BigComplex`. This is correct because:

1. BigComplex can't represent Inf/NaN — that's why the guard exists.
2. Go's `complex128` handles Inf/NaN natively.
3. The result faithfully represents `+inf.0+4.0i`.
4. The tower "drops" from BigComplex to Complex on the Inf/NaN path — unavoidable and correct.

No downstream concerns: Complex implements the same `Number` interface as BigComplex. The `Simplify` function (in `numeric_tower.go`) handles Complex→Float demotion when the imaginary part is zero, which doesn't apply here (we're preserving a non-zero imaginary part).

## Testing

Add **both** unit tests and integration tests.

### Unit tests in `values/promotion_test.go`

**If this file doesn't exist, check for existing Inf/NaN guard tests.** Search for `isSpecialFloat` in test files. Add tests in whichever file currently tests numeric dispatch (likely `values/numeric_tower_coverage_test.go` or a new `values/promotion_test.go`).

Test all 4 operations × 2 directions × 3 special values:

```go
// Test cases for Float(Inf/NaN) × BigComplex guard fix (#362)
//
// BigComplex operand: (make-rectangular 3 4) → BigComplex(BigInteger(3), BigInteger(4))
// Construct as: NewBigComplex(NewBigInteger(big.NewInt(3)), NewBigInteger(big.NewInt(4)))
//
// Expected results use Go complex128 arithmetic as the oracle:
//   complex(math.Inf(1), 0) + complex(3, 4) = complex(+Inf, 4)
//   complex(math.Inf(1), 0) * complex(3, 4) = complex(+Inf, +Inf)  // NOT component-wise!
```

Specific test cases to include:

| Op | Float | BigComplex | Expected | Notes |
|----|-------|-----------|----------|-------|
| Add | `+inf.0` | `3+4i` | `+inf.0+4.0i` | Inf dominates real, imag preserved |
| Add | `-inf.0` | `3+4i` | `-inf.0+4.0i` | |
| Add | `+nan.0` | `3+4i` | `+nan.0+4.0i` | NaN dominates real |
| Sub | `+inf.0` | `3+4i` | `+inf.0-4.0i` | Note: sign flip on imag |
| Mul | `+inf.0` | `3+4i` | `+inf.0+inf.0i` | Complex mul, NOT component-wise |
| Mul | `+nan.0` | `3+4i` | `+nan.0+nan.0i` | NaN propagates through complex mul |
| Div | `+inf.0` | `3+4i` | complex128 result | Use Go `complex(+Inf,0)/complex(3,4)` as oracle |
| Add (reversed) | `3+4i` | `+inf.0` | `+inf.0+4.0i` | BigComplex is receiver, Float is operand |

For each test case:
1. Construct `Float` via `NewFloat(math.Inf(1))` etc.
2. Construct `BigComplex` via the appropriate constructor (ensure parts are `*BigInteger`, not `*Integer`, to guarantee the value is `BigComplex` not `Complex`).
3. Call `floatVal.Add(bigComplexVal)` (and reverse: `bigComplexVal.Add(floatVal)`).
4. Assert result type is `*Complex` (not `*Float`, not `*BigComplex`).
5. Assert `result.(*Complex).Value == expected` using the Go complex128 computation as the oracle.

### Integration test

Add to the Scheme-level test file (likely `integration/testdata/r7rs-tests.scm` or a separate file):

```scheme
;; #362: Float(Inf/NaN) + BigComplex preserves imaginary part
(test-equal "+inf.0+4.0i" (number->string (+ +inf.0 (make-rectangular 3 4))))
(test-equal "+inf.0+4.0i" (number->string (+ (make-rectangular 3 4) +inf.0)))
(test-equal "+nan.0+4.0i" (number->string (+ +nan.0 (make-rectangular 3 4))))
```

**Note:** Use `number->string` comparison to verify both the real and imaginary parts, including their types (inexact). The exact string format depends on Wile's `SchemeString()` for `Complex` — verify by checking how `Complex.SchemeString()` formats `+inf.0` and `+nan.0` values.

## Precision Impact Analysis

This fix introduces one new precision loss site: `numberToComplex128`. All existing precision loss sites in `numberToFloat64` remain unchanged.

### What this fix does NOT make worse

The guard already called `numberToFloat64`, which truncates the BigComplex's real part to float64. The fix changes the guard to call `numberToComplex128`, which truncates BOTH parts to float64 — but previously the imaginary part was silently discarded (returned 0), so any finite float64 approximation is strictly better than total loss.

### Unavoidable precision loss in this fix

When `(+ +inf.0 (make-rectangular 3 (expt 2 100)))`:
- Real part: `+inf.0` (correct — Inf dominates)
- Imaginary part: `1.2676506002282294e+30` (53-bit truncation of 2^100)
- Ideal: `+inf.0+1267650600228229401496703205376i` (exact)

This is unavoidable because Complex (complex128) is the only complex type that can hold Inf/NaN. BigComplex cannot. There is no hybrid "exact-imaginary, inexact-real" complex type.

### Possible future mitigation

A future `InfComplex` type with BigFloat/BigInteger imaginary parts and special-value-capable real parts could eliminate this loss. This is out of scope for #362 but tracked as a consideration in `plans/PRECISION-GUARANTEES.md`.

## Verification Checklist

After implementation, verify:

1. `make build` passes
2. `make test` passes (all existing tests still green)
3. `make lint` passes (no formatting or import issues)
4. `make covercheck` passes
5. New tests cover both directions (Float as receiver, Float as operand)
6. Multiplication test confirms complex semantics (not component-wise)

## Summary of Changes

| What | Where | Lines (approx) |
|------|-------|-----------------|
| Add `numberToComplex128` helper | After `numberToFloat64` (line 288) | +15 lines |
| Add `complex128Op` param to `makeArithmeticDispatch` | Line 323 (signature) | +1 line |
| Modify `srcKind == KindFloat` guard branch | Lines ~348-355 | +5 lines |
| Modify `dstKind == KindFloat` guard branch | Lines ~356-363 | +5 lines |
| Update `makeAddDispatch` | Lines 372-381 | +3 lines |
| Update `makeSubtractDispatch` | Lines 384-393 | +3 lines |
| Update `makeMultiplyDispatch` | Lines 396-405 | +3 lines |
| Update `makeDivideDispatch` | Lines 408-417 | +3 lines |
| **Total** | **values/promotion.go only** | **~+38 lines** |
