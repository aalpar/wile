# Issue #362: Complex.Add/Sub/Mul/Div with Float(Inf/NaN) drops imaginary part when LUB is BigComplex

## Status: DONE (PR #363, 2026-02-28)

**Approach diverged from this plan's Step 2c.** The guard in `makeArithmeticDispatch`
was NOT removed. Instead it was modified: the `Float×BigComplex` case now wraps the
`complex128Op` result in `BigComplex` (via `NewBigComplexFromBigFloats`) rather than
returning `Float`. `numberToComplex128` still exists and is used by this path.
The original bug is fixed. The full guard-removal refactor described in Steps 2c–2e
remains possible but was not done.

Verified completed:
- Step 1 (BigFloat Inf/NaN-capable): DONE — `nan bool` field, `NewBigFloatNaN()`, `recoverNaN()`, all predicates and arithmetic methods updated
- Step 2a (Float→BigFloat promoter NaN fix): DONE — `NewBigFloatFromFloat64` guards `math.IsNaN`
- Step 2b (Float→BigComplex promoter): DONE — delegates to fixed `NewBigFloatFromFloat64`
- Step 2c (Remove arithmetic guard): NOT DONE — guard kept, BigComplex case patched instead
- Step 2d (Simplify makeArithmeticDispatch signature): NOT DONE — still takes `float64Op`, `complex128Op`
- Step 2e (Remove numberToComplex128): NOT DONE — still used by patched BigComplex case
- Step 2f (Keep comparison guards): DONE — untouched
- Step 2g (Update isSpecialFloat comment): NOT DONE — comment unchanged
- Step 3a (BigComplex.IsFinite): DONE
- Step 3b (BigComplex.IsNaN): DONE
- Step 3c (BigComplex.IsRational): DONE

Note: `values/CLAUDE.local.md` still says `BigFloat | #t | #f | #f | big.Float has no Inf/NaN`
and `BigComplex | #t | #f | #f | Always finite` — these are stale and should be updated.

## Bug

When a `Float` holding `+inf.0`, `-inf.0`, or `+nan.0` is combined arithmetically with a `BigComplex`, the dispatch guard in `makeArithmeticDispatch` short-circuits via `NewFloat(float64Op(...))`, silently discarding the imaginary component.

```scheme
(+ +inf.0 (make-rectangular 3 4))
; Expected: +inf.0+4i
; Actual:   +inf.0     (imaginary part lost)
```

## Root Cause

The promotion table says `Float × BigComplex → KindBigComplex`. A guard in `makeArithmeticDispatch` (`values/promotion.go:~370-410`) prevents promotion when a Float operand has an IEEE 754 special value (Inf/NaN), because the guard assumes BigFloat/BigComplex cannot represent these values. The guard short-circuits with `float64Op` or `complex128Op`, discarding the imaginary part (in the float64 path) or truncating arbitrary-precision parts to float64 (in the complex128 path).

### Which dispatch slots are affected?

Only one LUB combination triggers the bug: **`Float × BigComplex → KindBigComplex`** (and its symmetric counterpart).

- `Float × Complex → KindComplex` — `lubNeedsGuard = false`, guard never fires. OK.
- `Float × BigFloat → KindBigFloat` — real-only, no imaginary to lose. Guard gives correct result but is unnecessary.
- `Float × BigComplex → KindBigComplex` — **BUG**: imaginary part lost via `numberToFloat64`, or truncated to float64 via `numberToComplex128`.

### The original plan's incorrect assumption

The previous version of this plan stated: *"This is unavoidable because Complex (complex128) is the only complex type that can hold Inf/NaN. BigComplex cannot."*

**This is factually wrong for Inf.** Go's `big.Float` natively supports ±Inf:
- `SetInf(signbit bool)` — sets a `big.Float` to +Inf or -Inf
- `IsInf() bool` — tests for infinity
- Arithmetic works: `Inf + finite = Inf`, `Inf * positive = Inf`, etc.
- `SetFloat64(math.Inf(1))` produces a valid `big.Float` Inf

**NaN is genuinely unsupported.** `big.Float` has no NaN representation — operations that would produce NaN under IEEE 754 rules (e.g., `Inf + (-Inf)`, `0 * Inf`, `Inf / Inf`) panic with `big.ErrNaN`. `SetFloat64(math.NaN())` also panics.

### The real fix

Since `big.Float` supports Inf natively, and NaN can be represented via an out-of-band flag on `BigFloat`, the correct fix is to **make BigFloat Inf/NaN-capable** and **remove the arithmetic guard entirely**. Promotion from Float to BigFloat/BigComplex then works naturally for all values, preserving arbitrary-precision imaginary parts without any fallback to complex128.

## Design

Three files modified: `values/big_float.go`, `values/promotion.go`, `values/big_complex.go`.

### Step 1: Make BigFloat Inf/NaN-capable (`values/big_float.go`)

#### 1a. Add `nan` flag to struct

```go
type BigFloat struct {
	value *big.Float
	nan   bool
}
```

`big.Float` supports Inf natively (via `SetInf`), so no flag is needed for Inf — it is stored in `value` directly. Only NaN requires the flag because `big.Float` panics rather than storing NaN.

When `nan` is true, `value` MUST be a valid (zero-valued) `*big.Float` — never nil — to prevent nil-pointer panics if code accidentally reads it without checking the flag.

#### 1b. Add NaN-aware constructors

**`NewBigFloatNaN()`** — new constructor:
```go
func NewBigFloatNaN() *BigFloat {
	return &BigFloat{value: new(big.Float), nan: true}
}
```

**`NewBigFloatFromFloat64`** — fix NaN panic:
```go
func NewBigFloatFromFloat64(v float64) *BigFloat {
	if math.IsNaN(v) {
		return NewBigFloatNaN()
	}
	return &BigFloat{value: big.NewFloat(v).SetPrec(DefaultBigFloatPrecision)}
}
```

No change needed for `NewBigFloat(v *big.Float)` — a `big.Float` value can never be NaN (the library panics before creating one), so the Copy is always safe. Inf values are preserved by `Copy`.

No change needed for `NewBigFloatFromString` — parsing doesn't produce Inf or NaN.

#### 1c. Add `recoverNaN` helper

Wraps any `big.Float` operation that might panic with `ErrNaN`. Returns a BigFloat NaN if the operation panics.

```go
func recoverNaN(op func() *BigFloat) (result *BigFloat) {
	defer func() {
		if r := recover(); r != nil {
			if _, ok := r.(big.ErrNaN); ok {
				result = NewBigFloatNaN()
			} else {
				panic(r) // re-panic non-ErrNaN panics
			}
		}
	}()
	return op()
}
```

#### 1d. Fix predicate methods

All six predicates need updating. The `big.Float`-supports-Inf fact means Inf handling is automatic for some (`IsZero`, `IsInteger`), but predicates that previously hard-coded answers need fixing:

| Method | Before | After | Why |
|--------|--------|-------|-----|
| `IsRational()` | `return true` | `return !p.nan && !p.value.IsInf()` | Inf and NaN are not rational (R7RS §6.2.6) |
| `IsFinite()` | `return true` | `return !p.nan && !p.value.IsInf()` | Inf and NaN are not finite |
| `IsNaN()` | `return false` | `return p.nan` | NaN flag |
| `IsInteger()` | `return p.value.IsInt()` | `return !p.nan && p.value.IsInt()` | NaN BigFloat has zero `value` which `IsInt` reports as true |
| `IsZero()` | `return p.value.Sign() == 0` | `return !p.nan && p.value.Sign() == 0` | Same issue: NaN's zero `value` has Sign() == 0 |
| `IsExact()` | `return false` | unchanged | NaN and Inf are still inexact |

**Why Inf doesn't need explicit checks in `IsInteger`/`IsZero`:**
- `big.Float.IsInt()` returns `false` for Inf (form is `inf`, not `finite`)
- `big.Float.Sign()` returns ±1 for Inf, never 0

#### 1e. Fix arithmetic methods

Every arithmetic method needs two additions:
1. **NaN propagation**: if either operand is NaN, return NaN immediately (IEEE 754 rule).
2. **ErrNaN recovery**: wrap `big.Float` operations in `recoverNaN` for edge cases like `Inf + (-Inf)`, `0 * Inf`.

**Pattern for each arithmetic method (Add shown as example):**

```go
func (p *BigFloat) Add(o Number) Number {
	v, ok := o.(*BigFloat)
	if ok {
		if p.nan || v.nan {
			return NewBigFloatNaN()
		}
		return recoverNaN(func() *BigFloat {
			return &BigFloat{value: new(big.Float).Add(p.value, v.value)}
		})
	}
	return bigFloatAdd[o.Kind()](p, o)
}
```

Apply the same pattern to `Subtract`, `Multiply`, `Divide`.

**`Multiply` special case — zero × Inf:**

The existing `Multiply` has a fast path `if o.IsZero() { return multiplyResultForZero(o, p) }`. After our fix, BigFloat can hold Inf, so `BigFloat(0) * BigFloat(+Inf)` would hit this path and return zero instead of NaN. Fix by adding a finiteness guard:

```go
func (p *BigFloat) Multiply(o Number) Number {
	if p.nan || o.IsNaN() {
		return NewBigFloatNaN()
	}
	if o.IsZero() {
		if !p.IsFinite() {
			return NewBigFloatNaN() // 0 * Inf = NaN
		}
		return multiplyResultForZero(o, p)
	}
	if p.IsZero() && o.IsFinite() {
		return multiplyResultForZero(p, o)
	}
	v, ok := o.(*BigFloat)
	if ok {
		return recoverNaN(func() *BigFloat {
			return &BigFloat{value: new(big.Float).Mul(p.value, v.value)}
		})
	}
	return bigFloatMultiply[o.Kind()](p, o)
}
```

The second check `p.IsZero() && o.IsFinite()` already guards against `0 * Inf` from the other direction (o is Inf → `o.IsFinite()` is false → falls through to `big.Float.Mul(0, Inf)` → ErrNaN → recovered to NaN). Correct.

**`Divide` special case — NaN before zero-check:**

```go
func (p *BigFloat) Divide(o Number) Number {
	if p.nan || o.IsNaN() {
		return NewBigFloatNaN() // NaN / x = NaN, x / NaN = NaN
	}
	if o.IsZero() {
		panic(ErrDivisionByZero)
	}
	v, ok := o.(*BigFloat)
	if ok {
		return recoverNaN(func() *BigFloat {
			return &BigFloat{value: new(big.Float).Quo(p.value, v.value)}
		})
	}
	return bigFloatDivide[o.Kind()](p, o)
}
```

NaN check MUST precede `o.IsZero()`: IEEE 754 says `NaN / 0 = NaN`, not division-by-zero error.

#### 1f. Fix same-type dispatch closures

The `init()` closures for `bigFloatAdd`, `bigFloatSubtract`, `bigFloatMultiply`, `bigFloatDivide` also need NaN/ErrNaN handling. Although these are currently dead code for same-type operations (BigFloat.Add fast-paths past the dispatch table), they should be consistent for safety:

```go
bigFloatAdd = makeAddDispatch(KindBigFloat, func(p *BigFloat, o Number) Number {
	v := o.(*BigFloat)
	if p.nan || v.nan {
		return NewBigFloatNaN()
	}
	return recoverNaN(func() *BigFloat {
		return &BigFloat{value: new(big.Float).Add(p.value, v.value)}
	})
})
```

Same pattern for subtract, multiply, divide closures (multiply and divide need the zero/Inf edge-case checks matching their method bodies).

#### 1g. Fix other methods

| Method | Change needed |
|--------|--------------|
| `Negate()` | If `p.nan`, return `NewBigFloatNaN()`. Inf: `big.Float.Neg` handles natively. |
| `Abs()` | If `p.nan`, return `NewBigFloatNaN()`. Inf: `big.Float.Abs` handles natively. |
| `LessThan()` | If either is NaN, return `false` (IEEE 754). Inf: `big.Float.Cmp` handles natively. |
| `Compare()` | If either is NaN, return `0` (no valid ordering; least-wrong value). Inf: `big.Float.Cmp` handles natively. |
| `Float64()` | If `p.nan`, return `math.NaN()`. Inf: `big.Float.Float64()` returns `math.Inf(sign)` natively. |
| `Sign()` | No change needed. NaN BigFloat returns 0 (Sign of zero value). Acceptable — NaN has no sign. |
| `IsNegative()` | No change needed. Returns false for NaN (Sign=0). Acceptable. |
| `IsPositive()` | No change needed. Returns false for NaN (Sign=0). Acceptable. |
| `ToInexact()` | No change needed. Returns self. |

#### 1h. Fix `SchemeString`

`big.Float.Text('g', -1)` returns `"Inf"` / `"-Inf"` for Inf values. Scheme requires `"+inf.0"` / `"-inf.0"`.

```go
func (p *BigFloat) SchemeString() string {
	if p.nan {
		return "+nan.0"
	}
	if p.value.IsInf() {
		if p.value.Sign() < 0 {
			return "-inf.0"
		}
		return "+inf.0"
	}
	return p.value.Text('g', -1)
}
```

#### 1i. Fix `HashCode`

`Float(+Inf)` hashes via `hashUint64(0x5, math.Float64bits(+Inf))`. BigFloat(+Inf) must produce the same hash (since they are `EqualTo`). The current `hashInexactNumeric(p.value)` would produce a different hash for Inf (string-based vs bits-based). Fix:

```go
func (p *BigFloat) HashCode() uint64 {
	if p.nan {
		return hashUint64(0x5, math.Float64bits(math.NaN()))
	}
	if p.value.IsInf() {
		return hashUint64(0x5, math.Float64bits(p.Float64()))
	}
	return hashInexactNumeric(p.value)
}
```

This matches `Float.HashCode()` for Inf/NaN values (see `values/float.go:46-50`).

#### 1j. Fix `EqualTo`

NaN != anything (IEEE 754). Also guard against `SetFloat64(NaN)` panic when comparing with `*Float`:

```go
func (p *BigFloat) EqualTo(o Value) bool {
	v, ok := o.(*BigFloat)
	if ok {
		if p.nan || v.nan {
			return false
		}
		return p.value.Cmp(v.value) == 0
	}
	f, ok := o.(*Float)
	if ok {
		if p.nan || math.IsNaN(f.Value) {
			return false
		}
		vf := new(big.Float).SetFloat64(f.Value)
		return p.value.Cmp(vf) == 0
	}
	return false
}
```

Inf comparison works natively: `big.Float.Cmp(+Inf, +Inf) = 0`, and `SetFloat64(+Inf)` produces a valid `big.Float` Inf.

#### 1k. Fix `ToExact`

R7RS: `(exact +inf.0)` and `(exact +nan.0)` raise errors. Currently `ToExact` calls `p.value.Rat(nil)` which returns nil for Inf, falling through to `NewRational(0, 1)` — silently wrong.

```go
func (p *BigFloat) ToExact() Number {
	if p.nan {
		panic(errors.New("no exact equivalent for +nan.0"))
	}
	if p.value.IsInf() {
		panic(errors.New("no exact equivalent for infinity"))
	}
	r, _ := p.value.Rat(nil)
	if r == nil {
		return NewRational(0, 1)
	}
	return NewRationalFromRat(r)
}
```

Use project error types if available; this shows the intent.

#### 1l. Update comments

Remove all incorrect comments claiming `big.Float` has no Inf representation:
- `big_float.go:207` — `IsRational` comment: remove "big.Float has no Inf/NaN"
- `big_float.go:214` — `IsFinite` comment: remove "big.Float has no Inf or NaN representation"
- `big_float.go:221-223` — `IsNaN` comment: remove "big.Float has no NaN representation"

### Step 2: Fix promoters (`values/promotion.go`)

#### 2a. Fix Float → BigFloat promoter

Currently at line ~203. `SetFloat64(NaN)` panics. Fix:

```go
promoter[KindFloat][KindBigFloat] = func(n Number) Number {
	p := n.(*Float)
	if math.IsNaN(p.Value) {
		return NewBigFloatNaN()
	}
	return &BigFloat{value: new(big.Float).SetPrec(DefaultBigFloatPrecision).SetFloat64(p.Value)}
}
```

`SetFloat64(Inf)` works natively — no change needed for Inf.

#### 2b. Fix Float → BigComplex promoter

Currently at line ~213. Delegates to `NewBigFloatFromFloat64` which we fixed in Step 1b, so this works automatically:

```go
promoter[KindFloat][KindBigComplex] = func(n Number) Number {
	p := n.(*Float)
	return NewBigComplexFromBigFloats(
		NewBigFloatFromFloat64(p.Value), // NaN handled by Step 1b
		NewBigFloatFromFloat64(0),
	)
}
```

No code change needed here — the `NewBigFloatFromFloat64` fix propagates.

#### 2c. Remove arithmetic guard from `makeArithmeticDispatch`

Since BigFloat and BigComplex now handle Inf/NaN natively, the `isSpecialFloat` guard in `makeArithmeticDispatch` is unnecessary. Remove:
- The `lubNeedsGuard` variable
- The `srcKind == KindFloat && lubNeedsGuard` and `dstKind == KindFloat && lubNeedsGuard` switch cases
- The `float64Op` parameter
- The `complex128Op` parameter

**Simplified `makeArithmeticDispatch`:**

```go
func makeArithmeticDispatch[T Number](
	srcKind NumericKind,
	sameTypeOp func(T, Number) Number,
	applyOp func(Number, Number) Number,
) [numKinds]func(T, Number) Number {
	ensurePromotionInit()
	var table [numKinds]func(T, Number) Number
	table[srcKind] = sameTypeOp
	for dstKind := range numKinds {
		if dstKind == srcKind {
			continue
		}
		lubKind := promotionTable[srcKind][dstKind]
		promSrc := promoter[srcKind][lubKind]
		promDst := promoter[dstKind][lubKind]

		table[dstKind] = func(p T, o Number) Number {
			return applyOp(promSrc(p), promDst(o))
		}
	}
	return table
}
```

#### 2d. Update 4 arithmetic dispatch callers

Remove the `float64Op` and `complex128Op` closure arguments from `makeAddDispatch`, `makeSubtractDispatch`, `makeMultiplyDispatch`, `makeDivideDispatch`:

**Before (each caller has 3 closure args):**
```go
func makeAddDispatch[T Number](srcKind NumericKind, sameTypeAdd func(T, Number) Number) [numKinds]func(T, Number) Number {
	return makeArithmeticDispatch(srcKind, sameTypeAdd,
		func(a, b Number) Number { return a.Add(b) },
		func(a, b float64) float64 { return a + b },
		func(a, b complex128) complex128 { return a + b },
	)
}
```

**After (only 1 closure arg):**
```go
func makeAddDispatch[T Number](srcKind NumericKind, sameTypeAdd func(T, Number) Number) [numKinds]func(T, Number) Number {
	return makeArithmeticDispatch(srcKind, sameTypeAdd,
		func(a, b Number) Number {
			return a.Add(b)
		},
	)
}
```

Same pattern for subtract (`a.Subtract(b)`), multiply (`a.Multiply(b)`), divide (`a.Divide(b)`).

#### 2e. Remove `numberToComplex128` helper

This was added for the previous complex128 guard path and is no longer needed. Remove entirely.

#### 2f. Keep `isSpecialFloat`, `numberToFloat64`, comparison guards

`makeLessThanDispatch` and `makeCompareDispatch` still have their own guards using `isSpecialFloat` and `numberToFloat64`. These guards serve a different purpose: they handle Float NaN comparison semantics (`NaN < x` → false, `NaN = x` → false) by falling back to float64 comparisons. Keep them unchanged.

**Do NOT modify `makeLessThanDispatch` or `makeCompareDispatch`.**

#### 2g. Update `isSpecialFloat` comment

The comment above `isSpecialFloat` (line ~263) says: *"BigFloat cannot represent these values (SetFloat64 panics on NaN, and arithmetic on BigFloat Inf is undefined)."* This is now incorrect. Update to:

```go
// isSpecialFloat reports whether a Float holds IEEE 754 Inf or NaN.
// Used by comparison dispatches (LessThan, Compare) to fall back to
// float64 comparisons when the LUB is BigFloat/BigComplex.
```

### Step 3: Fix BigComplex predicates (`values/big_complex.go`)

#### 3a. Fix `IsFinite`

Currently hard-codes `true`. Delegate to parts:

```go
func (p *BigComplex) IsFinite() bool {
	return p.real.IsFinite() && p.imag.IsFinite()
}
```

This works because `Number.IsFinite()` is an interface method. For `*BigInteger` and `*Rational` parts, `IsFinite()` is always true. For `*BigFloat` parts, our new implementation checks `!p.nan && !p.value.IsInf()`.

#### 3b. Fix `IsNaN`

Currently hard-codes `false`. Delegate to parts:

```go
func (p *BigComplex) IsNaN() bool {
	return p.real.IsNaN() || p.imag.IsNaN()
}
```

A complex number is NaN if either component is NaN.

#### 3c. Fix `IsRational`

Currently returns `p.IsReal()`. With Inf/NaN parts, a BigComplex can be real (imag=0) but not rational (real=Inf):

```go
func (p *BigComplex) IsRational() bool {
	return p.IsReal() && p.real.IsRational()
}
```

Previously `p.real.IsRational()` was always true for all valid BigComplex part types, so the extra check was redundant. Now it matters.

#### 3d. Update comments

Remove incorrect comments:
- `big_complex.go:~443` — `IsFinite` comment: remove "BigComplex parts are always finite"
- `big_complex.go:~453` — `IsNaN` comment: remove "BigComplex parts cannot represent NaN"

### Step 4: BigComplex arithmetic (no changes needed)

BigComplex same-type operations (`bigComplexAdd`, `bigComplexMultiply`, etc.) use `addParts`, `multiplyParts`, `subtractParts`, `divideParts`. These delegate to BigFloat methods (via type switch). Since BigFloat methods now handle Inf/NaN correctly (Step 1e), BigComplex arithmetic works naturally:

- `BigFloat(+Inf).Add(BigFloat(3))` → `BigFloat(+Inf)` ✓
- `BigFloat(0).Multiply(BigFloat(+Inf))` → `BigFloat(NaN)` via ErrNaN recovery ✓
- NaN parts propagate through all intermediate operations ✓

The `ErrNaN` panics from `big.Float` operations (e.g., complex multiplication `(Inf+0i) * (0+1i)` produces intermediate `Inf*0`) are caught by BigFloat's `recoverNaN` wrapper at the BigFloat method level.

**No changes to BigComplex arithmetic code.**

## Result Type

With this fix, `(+ +inf.0 (make-rectangular 3 4))` produces a `*BigComplex` with:
- Real part: `BigFloat(+Inf)` (native `big.Float` Inf)
- Imag part: `BigInteger(4)` (exact, preserved at full precision)

This is **strictly better** than the previous plan's approach, which would have produced `*Complex(complex128)` with both parts truncated to float64. The arbitrary-precision imaginary part is fully preserved.

## `big.Float` Inf Arithmetic — Edge Cases and ErrNaN Panics

For reference, the `big.Float` operations that panic with `ErrNaN` instead of producing a result:

| Operation | Panics? | IEEE 754 result | Our recovery |
|-----------|---------|-----------------|--------------|
| `Inf + finite` | No | ±Inf | n/a |
| `Inf + Inf` (same sign) | No | ±Inf | n/a |
| `Inf + (-Inf)` | **Yes** | NaN | `recoverNaN` → BigFloat NaN |
| `Inf - Inf` (same sign) | **Yes** | NaN | `recoverNaN` → BigFloat NaN |
| `Inf * positive` | No | ±Inf | n/a |
| `Inf * 0` | **Yes** | NaN | `recoverNaN` → BigFloat NaN |
| `Inf / Inf` | **Yes** | NaN | `recoverNaN` → BigFloat NaN |
| `0 / 0` | **Yes** | NaN | `ErrDivisionByZero` fires first |
| `SetFloat64(NaN)` | **Yes** | n/a | Promoter catches with `math.IsNaN` check |

All ErrNaN panics correspond to IEEE 754 NaN results. The `recoverNaN` wrapper converts each to a BigFloat NaN, which is the correct semantics.

## Precision Impact Analysis

### What this fix eliminates

The previous plan introduced `numberToComplex128` which truncated both real and imaginary parts of BigComplex operands to float64. **That truncation is now eliminated.** When `(+ +inf.0 (make-rectangular 3 (expt 2 100)))`:

- **Previous plan**: imag = `1.2676506002282294e+30` (53-bit truncation)
- **This fix**: imag = `BigInteger(2^100)` (exact, preserved)

### New precision loss: none

No new precision loss is introduced. The BigFloat Inf is a native `big.Float` Inf (exact representation). The BigFloat NaN is a flag (no numeric value to lose precision on). All promotion paths preserve the original operand's precision.

### `BigFloat.Float64()` on Inf/NaN

`big.Float.Float64()` on Inf returns `math.Inf(sign)` with accuracy `Exact`. BigFloat NaN's `Float64()` returns `math.NaN()`. Both are exact — no precision loss.

## Testing

### Unit tests in `values/` (likely `values/promotion_test.go` or `values/big_float_test.go`)

#### BigFloat Inf/NaN predicate tests

```go
// BigFloat with Inf
infBF := NewBigFloat(new(big.Float).SetInf(false))
assert(infBF.IsFinite() == false)
assert(infBF.IsRational() == false)
assert(infBF.IsNaN() == false)
assert(infBF.IsInteger() == false)
assert(infBF.IsZero() == false)
assert(infBF.SchemeString() == "+inf.0")

// BigFloat with NaN
nanBF := NewBigFloatNaN()
assert(nanBF.IsFinite() == false)
assert(nanBF.IsRational() == false)
assert(nanBF.IsNaN() == true)
assert(nanBF.IsInteger() == false)
assert(nanBF.IsZero() == false)
assert(nanBF.SchemeString() == "+nan.0")
```

#### BigFloat Inf arithmetic tests

```go
inf := NewBigFloat(new(big.Float).SetInf(false))    // +Inf
three := NewBigFloatFromFloat64(3.0)

assert(inf.Add(three).IsFinite() == false)           // +Inf + 3 = +Inf
assert(inf.Multiply(three).IsFinite() == false)      // +Inf * 3 = +Inf
assert(inf.Add(inf).IsFinite() == false)             // +Inf + +Inf = +Inf
```

#### BigFloat ErrNaN recovery tests

```go
posInf := NewBigFloat(new(big.Float).SetInf(false))
negInf := NewBigFloat(new(big.Float).SetInf(true))
zero := NewBigFloatFromFloat64(0.0)

assert(posInf.Add(negInf).IsNaN() == true)            // +Inf + -Inf = NaN
assert(zero.Multiply(posInf).IsNaN() == true)          // 0 * +Inf = NaN (not panic)
assert(posInf.Divide(posInf).IsNaN() == true)          // +Inf / +Inf = NaN
```

#### BigFloat hash consistency tests

```go
// Float and BigFloat must hash identically for equal values
floatInf := NewFloat(math.Inf(1))
bigFloatInf := NewBigFloat(new(big.Float).SetInf(false))
assert(floatInf.HashCode() == bigFloatInf.HashCode())

floatNaN := NewFloat(math.NaN())
bigFloatNaN := NewBigFloatNaN()
assert(floatNaN.HashCode() == bigFloatNaN.HashCode())
```

#### Float × BigComplex dispatch tests (the original bug)

Test all 4 operations × 2 directions × 3 special values:

```go
// BigComplex operand: (make-rectangular 3 4)
// Construct with BigInteger parts to guarantee BigComplex (not Complex)
bc := NewBigComplex(NewBigInteger(big.NewInt(3)), NewBigInteger(big.NewInt(4)))
```

| Op | Float | BigComplex | Expected result type | Expected real | Expected imag |
|----|-------|-----------|---------------------|---------------|---------------|
| Add | `+inf.0` | `3+4i` | `*BigComplex` | `BigFloat(+Inf)` | `BigInteger(4)` |
| Add | `-inf.0` | `3+4i` | `*BigComplex` | `BigFloat(-Inf)` | `BigInteger(4)` |
| Add | `+nan.0` | `3+4i` | `*BigComplex` | `BigFloat(NaN)` | `BigInteger(4)` |
| Sub | `+inf.0` | `3+4i` | `*BigComplex` | `BigFloat(+Inf)` | imag part |
| Mul | `+inf.0` | `3+4i` | `*BigComplex` | check parts | check parts |
| Mul | `+nan.0` | `3+4i` | `*BigComplex` | NaN part | NaN part |
| Div | `+inf.0` | `3+4i` | `*BigComplex` | check parts | check parts |
| Add (reversed) | `3+4i` | `+inf.0` | `*BigComplex` | `BigFloat(+Inf)` | `BigInteger(4)` |

**Key assertions:**
1. Result type is `*BigComplex` (not `*Float`, not `*Complex`)
2. Imaginary part is preserved (not zero, not truncated to float64)
3. For multiplication, verify complex semantics: `(+Inf+0i) * (3+4i)` = real=Inf, imag=Inf (not component-wise)
4. For NaN cases, result parts are BigFloat NaN

### Integration tests (Scheme level)

```scheme
;; #362: Float(Inf/NaN) + BigComplex preserves imaginary part
(test-equal "+inf.0+4i" (number->string (+ +inf.0 (make-rectangular 3 4))))
(test-equal "+inf.0+4i" (number->string (+ (make-rectangular 3 4) +inf.0)))
(test-equal "+nan.0+4i" (number->string (+ +nan.0 (make-rectangular 3 4))))

;; Verify BigFloat Inf/NaN predicates
(test-assert (not (finite? (real-part (+ +inf.0 (make-rectangular 3 4))))))
(test-assert (nan? (real-part (+ +nan.0 (make-rectangular 3 4)))))
```

**Note:** Verify the exact string format by checking `BigComplex.SchemeString()` and `BigFloat.SchemeString()` render correctly for Inf/NaN parts. The `BigComplex.SchemeString()` delegates to each part's `SchemeString()`, so `BigFloat(+Inf).SchemeString()` returning `"+inf.0"` feeds directly into the complex representation.

## Verification Checklist

After implementation, verify:

1. `make build` passes
2. `make test` passes (all existing tests still green)
3. `make lint` passes
4. `make covercheck` passes
5. New tests cover BigFloat Inf predicates, NaN predicates, Inf arithmetic, ErrNaN recovery, hash consistency
6. New tests cover both directions (Float as receiver, Float as operand) for BigComplex dispatch
7. Multiplication test confirms complex semantics (not component-wise)
8. `BigFloat(+Inf).HashCode() == Float(+Inf).HashCode()` (hash consistency)
9. `BigFloat(NaN).EqualTo(BigFloat(NaN)) == false` (NaN != NaN)

## Summary of Changes

| What | Where | Approx lines |
|------|-------|-------------|
| Add `nan` field to BigFloat struct | `values/big_float.go:32-34` | +1 |
| Add `NewBigFloatNaN` constructor | `values/big_float.go` | +3 |
| Fix `NewBigFloatFromFloat64` for NaN | `values/big_float.go:~44` | +3 |
| Add `recoverNaN` helper | `values/big_float.go` | +12 |
| Fix 6 predicate methods | `values/big_float.go` | ~6 lines changed |
| Fix 4 arithmetic methods + closures | `values/big_float.go` | ~40 lines changed |
| Fix `Negate`, `Abs`, `LessThan`, `Compare` | `values/big_float.go` | ~12 lines changed |
| Fix `SchemeString` | `values/big_float.go` | +6 |
| Fix `HashCode` | `values/big_float.go` | +6 |
| Fix `EqualTo` | `values/big_float.go` | +4 |
| Fix `Float64` | `values/big_float.go` | +3 |
| Fix `ToExact` | `values/big_float.go` | +6 |
| Fix Float→BigFloat promoter | `values/promotion.go:~203` | +3 |
| Remove arithmetic guard | `values/promotion.go:~345-410` | -40 lines |
| Simplify `makeArithmeticDispatch` | `values/promotion.go` | -2 params |
| Simplify 4 dispatch callers | `values/promotion.go` | -12 lines |
| Remove `numberToComplex128` | `values/promotion.go` | -20 lines |
| Fix `isSpecialFloat` comment | `values/promotion.go` | comment only |
| Fix `BigComplex.IsFinite` | `values/big_complex.go` | 1 line changed |
| Fix `BigComplex.IsNaN` | `values/big_complex.go` | 1 line changed |
| Fix `BigComplex.IsRational` | `values/big_complex.go` | 1 line changed |
| Fix comments (3 files) | throughout | comments only |
| **Net** | **3 files** | **~+30 lines** |

## Notes for Implementer

These clarifications address ambiguities that could lead to incorrect implementation.

### 1. SameTypeOp closures: delegate, don't duplicate

Step 1f says to update the `init()` closures for `bigFloatAdd`, `bigFloatSubtract`, `bigFloatMultiply`, `bigFloatDivide`. **Do NOT duplicate the method body's zero/Inf edge-case logic into these closures.** These closures are dead code for same-type dispatch (the BigFloat method fast-paths past the dispatch table), but for consistency, simply delegate to the method:

```go
bigFloatAdd = makeAddDispatch(KindBigFloat, func(p *BigFloat, o Number) Number {
	return p.Add(o)
})
bigFloatSubtract = makeSubtractDispatch(KindBigFloat, func(p *BigFloat, o Number) Number {
	return p.Subtract(o)
})
bigFloatMultiply = makeMultiplyDispatch(KindBigFloat, func(p *BigFloat, o Number) Number {
	return p.Multiply(o)
})
bigFloatDivide = makeDivideDispatch(KindBigFloat, func(p *BigFloat, o Number) Number {
	return p.Divide(o)
})
```

This is not circular: `BigFloat.Add(o)` checks `o.(*BigFloat)` → true → takes the inline fast path → never calls back into the dispatch table.

### 2. Error types for `ToExact`

For `ToExact` panics on Inf/NaN, use the same pattern as `ErrDivisionByZero`. Check how `ErrDivisionByZero` is defined (likely a sentinel `errors.New(...)` in `values/errors.go` or similar). Define analogous sentinels:

```go
var ErrNoExactEquivalent = errors.New("no exact equivalent")
```

Then in `ToExact`:
```go
if p.nan || p.value.IsInf() {
	panic(ErrNoExactEquivalent)
}
```

Search for `ErrDivisionByZero` to find where error sentinels are declared and follow that pattern.

### 3. `Compare` with NaN — verify `=` routing

The plan says `Compare` returns `0` for NaN (no valid ordering). This is safe **only if** the Scheme `=` predicate routes through `EqualTo` (which returns `false` for NaN), NOT through `Compare`. Verify this before implementing:

- Search for how `=` is dispatched in the evaluator. If it uses `EqualTo`, returning `0` from `Compare` for NaN is acceptable (comparison predicates like `<` use `LessThan`, not `Compare`).
- If `=` uses `Compare`, then `Compare` must NOT return `0` for NaN. In that case, `Compare` should panic or the caller must check `IsNaN` first.

**Check before coding.** Do not assume.

### 4. Existing `&BigFloat{value: ...}` struct literals are safe

Go zero-values the `nan` bool field to `false`. All existing code that constructs `BigFloat` via struct literal (e.g., `&BigFloat{value: new(big.Float).Add(a, b)}`) correctly produces a non-NaN BigFloat. **Do NOT add `nan: false` to existing struct literals** — it's redundant noise.

The only places that set `nan: true` are `NewBigFloatNaN()` and `NewBigFloatFromFloat64(math.NaN())`.

### 5. NaN-before-IsZero ordering is critical

In arithmetic methods, NaN checks MUST precede `IsZero()` / `IsFinite()` checks. The general principle:

```
NaN check first  →  produces NaN (IEEE 754: NaN dominates everything)
IsZero check     →  short-circuit for zero (only valid if NaN is excluded)
IsFinite check   →  short-circuit for Inf edge cases
Normal operation →  may panic with ErrNaN → recovered to NaN
```

Example of WRONG ordering in Divide:
```go
// WRONG: NaN / 0 would panic with DivisionByZero instead of returning NaN
if o.IsZero() { panic(ErrDivisionByZero) }
if p.nan || o.IsNaN() { return NewBigFloatNaN() }
```

Example of CORRECT ordering:
```go
// CORRECT: NaN dominates, then check zero
if p.nan || o.IsNaN() { return NewBigFloatNaN() }
if o.IsZero() { panic(ErrDivisionByZero) }
```

Apply this principle to ALL four arithmetic methods. The plan's code examples show the correct ordering — follow them exactly.
