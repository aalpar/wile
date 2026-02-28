# Issue #362: Complex.Add/Sub/Mul/Div with Float(Inf/NaN) drops imaginary part when LUB is BigComplex

## Bug

When a `Float` holding `+inf.0`, `-inf.0`, or `+nan.0` is combined arithmetically with a `BigComplex`, the dispatch guard in `makeArithmeticDispatch` short-circuits via `NewFloat(float64Op(...))`, silently discarding the imaginary component.

```scheme
(+ +inf.0 (make-rectangular 3 4))
; Expected: +inf.0+4i  (or BigComplex with Inf real part and exact 4 imaginary)
; Actual:   +inf.0     (imaginary part lost)
```

## Design Context

Scheme numerics are biased toward algebra — symbolic manipulation and exact representation. Wile maps this onto Go's concrete types but follows a key invariant: **operations stay in their domain**. `float64 × float64 → float64`. `BigFloat × BigFloat → BigFloat`. The promotion lattice determines the result type; special values (Inf, NaN) are never a reason to switch domains.

This means `values.BigFloat` and `values.BigComplex` **must** represent both Inf and NaN, following IEEE 754 semantics uniformly across all inexact types. See `docs/dev/NUMERIC_TOWER.md` § "IEEE 754 Semantic Uniformity".

### Why the guard exists (and why it should be removed)

The guard was introduced because Go's `math/big.Float` has no NaN representation (it supports Inf via `SetInf` but NaN is out of band). Rather than extending `values.BigFloat` to handle NaN, the guard demotes the result to `float64`/`complex128`.

The correct fix is the opposite: extend `BigFloat` and `BigComplex` to represent Inf and NaN internally, then **remove the guard entirely**. The promotion lattice already produces the correct result type (`Float × BigComplex → BigComplex`); the guard was a workaround for a missing capability, not a design feature.

## Root Cause

The promotion table says `Float × BigComplex → KindBigComplex`. The guard in `makeArithmeticDispatch` (`values/promotion.go:376-407`) short-circuits when a Float operand has a special value:

```go
return NewFloat(float64Op(numberToFloat64(p), numberToFloat64(o)))
```

`numberToFloat64` on a `BigComplex` extracts only `v.real` (`promotion.go:285-286`), discarding the imaginary part. The result is also the wrong type — `Float` instead of `BigComplex`.

### Which dispatch slots are affected?

The `lubNeedsGuard` flag is `true` when `lubKind != KindFloat && lubKind != KindComplex`. The affected slots:

- `Float × BigFloat → KindBigFloat` — demotes to Float, losing BigFloat precision
- `Float × BigComplex → KindBigComplex` — demotes to Float, losing imaginary part entirely
- `BigFloat × Float → KindBigFloat` — same issue, symmetric
- `BigComplex × Float → KindBigComplex` — same issue, symmetric

The same guard pattern exists in `makeLessThanDispatch` (`promotion.go:493-514`) and `makeCompareDispatch` (`promotion.go:540-563`). All three dispatch generators have identical guard logic that must be removed.

Once BigFloat/BigComplex support Inf/NaN, **all** of these guards become unnecessary. The lattice promotion handles them correctly.

## Fix Design

The fix has four phases. Phase 1 is the core work; phases 2-4 are mechanical follow-ups.

---

### Phase 1: Extend BigFloat to support Inf and NaN

**File: `values/big_float.go`**

`values.BigFloat` currently wraps `*big.Float` (line 33-35). It must be extended to represent Inf and NaN internally.

**Go's `math/big.Float` status:**
- **Inf**: Supported via `big.Float.SetInf(signbit bool)`. `big.Float.IsInf()` detects it. `SetInf(false)` = +Inf, `SetInf(true)` = -Inf.
- **NaN**: NOT supported. No `SetNaN`, no `IsNaN`. NaN is completely out of band for `big.Float`.
- **Panics**: `big.NewFloat(math.NaN())` panics. `big.NewFloat(math.Inf(1))` also panics. `new(big.Float).SetFloat64(math.NaN())` panics. `new(big.Float).SetFloat64(math.Inf(1))` panics.

#### 1a. Change BigFloat struct (line 33-35)

**Before:**
```go
type BigFloat struct {
    value *big.Float
}
```

**After:**
```go
type BigFloat struct {
    value *big.Float
    nan   bool // true if this value represents NaN; value is unused when nan==true
}
```

When `nan == true`, the `value` field MUST be a valid (zero-value) `*big.Float`, not nil. This avoids nil-pointer panics in code paths that access `value` before checking `nan`. Set it to `new(big.Float)` or leave as the zero `big.Float`.

When representing Inf, `nan == false` and `value` holds a `big.Float` set via `value.SetInf(signbit)`. `value.IsInf()` detects this.

#### 1b. Add constructors

Add after existing constructors (after line 55):

```go
// NewBigFloatInf creates a BigFloat representing +Inf or -Inf.
// sign > 0 or sign == 0 → +Inf, sign < 0 → -Inf.
func NewBigFloatInf(sign int) *BigFloat {
    bf := new(big.Float).SetPrec(DefaultBigFloatPrecision)
    bf.SetInf(sign < 0)
    return &BigFloat{value: bf}
}

// NewBigFloatNaN creates a BigFloat representing NaN.
func NewBigFloatNaN() *BigFloat {
    return &BigFloat{value: new(big.Float), nan: true}
}
```

#### 1c. Fix existing constructors to handle Inf/NaN float64 inputs

`NewBigFloatFromFloat64` (line 43-45) currently calls `big.NewFloat(v)` which **panics** on Inf and NaN. This is the constructor used by the promoter at `promotion.go:202-205` (`Float → BigFloat`) and `promotion.go:214-219` (`Float → BigComplex`).

**Before:**
```go
func NewBigFloatFromFloat64(v float64) *BigFloat {
    return &BigFloat{value: big.NewFloat(v).SetPrec(DefaultBigFloatPrecision)}
}
```

**After:**
```go
func NewBigFloatFromFloat64(v float64) *BigFloat {
    if math.IsNaN(v) {
        return NewBigFloatNaN()
    }
    if math.IsInf(v, 0) {
        return NewBigFloatInf(int(math.Copysign(1, v)))
    }
    return &BigFloat{value: big.NewFloat(v).SetPrec(DefaultBigFloatPrecision)}
}
```

Requires adding `"math"` to imports.

`NewBigFloat(v *big.Float)` (line 38-40) does NOT need changes — `big.Float` can already hold Inf via `SetInf`, and callers passing `*big.Float` are responsible for valid values. However, `new(big.Float).Copy(v)` on a `big.Float` set to Inf preserves the Inf — verify this.

`NewBigFloatFromString` (line 47-55) does NOT need changes — `big.ParseFloat` does not produce Inf/NaN from string input.

#### 1d. Update predicates

Each predicate method must check `nan` and Inf before accessing `value`:

| Method | Line | Before | After |
|--------|------|--------|-------|
| `IsNaN()` | 225 | `return false` | `return p.nan` |
| `IsFinite()` | 218 | `return true` | `return !p.nan && !p.value.IsInf()` |
| `IsRational()` | 211 | `return true` | `return !p.nan && !p.value.IsInf()` |
| `IsInteger()` | 204 | `return p.value.IsInt()` | `return !p.nan && !p.value.IsInf() && p.value.IsInt()` |
| `IsZero()` | 173 | `return p.value.Sign() == 0` | `return !p.nan && !p.value.IsInf() && p.value.Sign() == 0` |
| `IsNegative()` | 187 | `return p.value.Sign() < 0` | `return !p.nan && p.value.Sign() < 0` (NaN is not negative) |
| `IsPositive()` | 192 | `return p.value.Sign() > 0` | `return !p.nan && p.value.Sign() > 0` (NaN is not positive) |
| `Sign()` | 250 | `return p.value.Sign()` | NaN → `0` (no sign), Inf → `p.value.Sign()` (big.Float handles this) |

**Note on `IsNegative`/`IsPositive`/`Sign` for Inf:** `big.Float.Sign()` returns -1 for -Inf and +1 for +Inf. This is correct behavior — no special handling needed for Inf, only for NaN.

#### 1e. Update `SchemeString()` (line 264-266)

**Before:**
```go
func (p *BigFloat) SchemeString() string {
    return p.value.Text('g', -1)
}
```

**After:**
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

Verify: `big.Float.Text` on an Inf `big.Float` may return `"Inf"` or panic. Check Go docs. If it handles Inf, the explicit check is still better for Scheme-formatted output (`+inf.0` vs `Inf`).

#### 1f. Update `EqualTo()` (line 274-289)

IEEE 754: `NaN != NaN`. `Inf == Inf`. `-Inf == -Inf`. `Inf != -Inf`.

**Before:**
```go
func (p *BigFloat) EqualTo(o Value) bool {
    v, ok := o.(*BigFloat)
    if !ok {
        f, ok := o.(*Float)
        if ok {
            vf := new(big.Float).SetFloat64(f.Value)
            return p.value.Cmp(vf) == 0
        }
        return false
    }
    if v == nil || p == nil {
        return p == v
    }
    return p.value.Cmp(v.value) == 0
}
```

**After:**
```go
func (p *BigFloat) EqualTo(o Value) bool {
    v, ok := o.(*BigFloat)
    if !ok {
        f, ok := o.(*Float)
        if ok {
            // NaN != anything
            if p.nan || math.IsNaN(f.Value) {
                return false
            }
            // Inf comparison — both sides may be Inf
            if p.value.IsInf() {
                return math.IsInf(f.Value, p.value.Sign())
            }
            vf := new(big.Float).SetFloat64(f.Value)
            return p.value.Cmp(vf) == 0
        }
        return false
    }
    if v == nil || p == nil {
        return p == v
    }
    // NaN != anything (including NaN)
    if p.nan || v.nan {
        return false
    }
    return p.value.Cmp(v.value) == 0
}
```

**Note:** `big.Float.Cmp` on two Inf values of the same sign returns 0 (equal). On Inf vs -Inf returns ±1. This is correct. Verify in Go docs.

#### 1g. Update `Float64()` (line 63-66)

**Before:**
```go
func (p *BigFloat) Float64() float64 {
    f, _ := p.value.Float64()
    return f
}
```

**After:**
```go
func (p *BigFloat) Float64() float64 {
    if p.nan {
        return math.NaN()
    }
    if p.value.IsInf() {
        return math.Inf(p.value.Sign())
    }
    f, _ := p.value.Float64()
    return f
}
```

**Note:** `big.Float.Float64()` on an Inf `big.Float` may return `math.Inf` correctly — verify. If so, the explicit check is redundant but safer and clearer.

#### 1h. Update `HashCode()` (line 71-73)

`hashInexactNumeric` (in `values/hash.go:67-74`) calls `f.MinPrec()` and `f.Text('g', -1)` on the `*big.Float`. Both may behave unexpectedly for Inf, and `value` is unused for NaN.

**After:**
```go
func (p *BigFloat) HashCode() uint64 {
    if p.nan {
        return hashString(0x5, "+nan.0")
    }
    if p.value.IsInf() {
        if p.value.Sign() < 0 {
            return hashString(0x5, "-inf.0")
        }
        return hashString(0x5, "+inf.0")
    }
    return hashInexactNumeric(p.value)
}
```

**Cross-type hash consistency:** `Float(+inf.0).HashCode()` must equal `BigFloat(+inf.0).HashCode()`. Check `Float.HashCode()` to ensure it uses the same seed byte (0x5) and string format. If `Float.HashCode()` hashes `math.Inf(1)` differently, align them.

#### 1i. Update `ToExact()` (line 230-237)

R7RS §6.2.6: `(exact +inf.0)` and `(exact +nan.0)` MUST raise an error.

**Before:**
```go
func (p *BigFloat) ToExact() Number {
    r, _ := p.value.Rat(nil)
    if r == nil {
        return NewRational(0, 1)
    }
    return NewRationalFromRat(r)
}
```

**After:**
```go
func (p *BigFloat) ToExact() Number {
    if p.nan || p.value.IsInf() {
        panic(WrapForeignErrorf(ErrExactnessConversion, "toExact: cannot convert %s to exact", p.SchemeString()))
    }
    r, _ := p.value.Rat(nil)
    if r == nil {
        return NewRational(0, 1)
    }
    return NewRationalFromRat(r)
}
```

**Note:** The existing code returns `NewRational(0, 1)` when `Rat` returns nil — this silently converts Inf to 0, which is wrong. The Inf case is now caught above.

#### 1j. Update `Abs()` (line 245-247)

**Before:**
```go
func (p *BigFloat) Abs() Number {
    return NewBigFloat(new(big.Float).Abs(p.value))
}
```

**After:**
```go
func (p *BigFloat) Abs() Number {
    if p.nan {
        return p
    }
    if p.value.IsInf() {
        return NewBigFloatInf(1) // |±Inf| = +Inf
    }
    return NewBigFloat(new(big.Float).Abs(p.value))
}
```

**Note:** `big.Float.Abs` on an Inf value should work (produces +Inf) — verify. If confirmed, the explicit check is unnecessary but clearer.

#### 1k. Update `Negate()` (line 168-170)

**Before:**
```go
func (p *BigFloat) Negate() Number {
    return &BigFloat{value: new(big.Float).Neg(p.value)}
}
```

**After:**
```go
func (p *BigFloat) Negate() Number {
    if p.nan {
        return p // -NaN = NaN
    }
    return &BigFloat{value: new(big.Float).Neg(p.value)}
}
```

**Note:** `big.Float.Neg` on Inf correctly produces -Inf (and vice versa). No special handling needed for Inf.

#### 1l. Update arithmetic methods: Add, Subtract, Multiply, Divide

All four arithmetic methods have the same pattern: a same-type fast path that directly accesses `p.value` and `o.(*BigFloat).value`, then falls through to the dispatch table.

The same-type fast paths (and the `init()` dispatch closures at lines 90-112) will panic if `p.value` or `o.value` is a zero `*big.Float` (NaN sentinel) and `big.Float.Add/Sub/Mul/Quo` is called on it. They also need IEEE 754 Inf arithmetic semantics.

**Pattern for same-type BigFloat arithmetic (all four ops follow this):**

```go
func (p *BigFloat) Add(o Number) Number {
    v, ok := o.(*BigFloat)
    if ok {
        // NaN propagation: any op with NaN → NaN
        if p.nan || v.nan {
            return NewBigFloatNaN()
        }
        // Inf + Inf rules (IEEE 754):
        //   +Inf + +Inf = +Inf
        //   +Inf + -Inf = NaN
        //   -Inf + -Inf = -Inf
        //   Inf + finite = Inf
        if p.value.IsInf() || v.value.IsInf() {
            // Delegate to float64 for IEEE 754 Inf arithmetic
            return NewBigFloatFromFloat64(p.Float64() + v.Float64())
        }
        return &BigFloat{value: new(big.Float).Add(p.value, v.value)}
    }
    return bigFloatAdd[o.Kind()](p, o)
}
```

**The `float64` delegation pattern:** For Inf cases, converting both operands to float64 and performing the operation in float64 is the simplest way to get correct IEEE 754 Inf semantics (e.g., `Inf + -Inf = NaN`, `Inf * 0 = NaN`). The result is then re-wrapped via `NewBigFloatFromFloat64` which handles the NaN/Inf result correctly (see 1c). This works because:
- Inf operands have no precision to lose
- The finite operand's value doesn't affect the result (Inf dominates, or NaN is produced)
- `NewBigFloatFromFloat64` correctly routes NaN → `NewBigFloatNaN()`, Inf → `NewBigFloatInf()`

**Apply the same pattern to:**

| Method | Line | Inf-specific rules |
|--------|------|--------------------|
| `Add` | 118-124 | `Inf + -Inf = NaN`, `Inf + finite = Inf` |
| `Subtract` | 130-136 | `Inf - Inf = NaN`, `Inf - finite = Inf` |
| `Multiply` | 141-153 | `Inf * 0 = NaN`, `Inf * finite = ±Inf` (sign rule) |
| `Divide` | 156-165 | `Inf / Inf = NaN`, `Inf / 0 = ±Inf`, `finite / Inf = 0`, `Inf / finite = ±Inf` |

**Multiply has existing zero-check guards (lines 142-147):**

```go
if o.IsZero() {
    return multiplyResultForZero(o, p)
}
if p.IsZero() && o.IsFinite() {
    return multiplyResultForZero(p, o)
}
```

With BigFloat supporting Inf/NaN, `multiplyResultForZero` (`numeric_tower.go:32-37`) is called when `o.IsZero()` is true. But IEEE 754 says `0 * Inf = NaN` and `0 * NaN = NaN`. Currently `IsZero()` returns false for Inf and NaN (correct — 1d update), so the `o.IsZero()` guard only fires when `o` is actually zero. But then `p` might be Inf. The guard returns `NewInteger(0)` or `zero` — wrong for `Inf * 0`.

**Fix:** The NaN/Inf check in the same-type fast path MUST come before the `o.IsZero()` check. Restructure:

```go
func (p *BigFloat) Multiply(o Number) Number {
    v, ok := o.(*BigFloat)
    if ok {
        if p.nan || v.nan {
            return NewBigFloatNaN()
        }
        if p.value.IsInf() || v.value.IsInf() {
            return NewBigFloatFromFloat64(p.Float64() * v.Float64())
        }
        return &BigFloat{value: new(big.Float).Mul(p.value, v.value)}
    }
    // Cross-type: existing guards + dispatch table
    if o.IsZero() {
        return multiplyResultForZero(o, p)
    }
    if p.IsZero() && o.IsFinite() {
        return multiplyResultForZero(p, o)
    }
    return bigFloatMultiply[o.Kind()](p, o)
}
```

**Note on Divide:** The existing `if o.IsZero() { panic(ErrDivisionByZero) }` guard (line 157-159) fires before the same-type check. IEEE 754 says `Inf / 0 = Inf` and `NaN / 0 = NaN`. Restructure so the NaN/Inf check comes first for same-type, and the zero-check only applies to finite operands. But for cross-type (where `o` is not `*BigFloat`), the zero-check is still needed. Restructure:

```go
func (p *BigFloat) Divide(o Number) Number {
    v, ok := o.(*BigFloat)
    if ok {
        if p.nan || v.nan {
            return NewBigFloatNaN()
        }
        if p.value.IsInf() || v.value.IsInf() {
            return NewBigFloatFromFloat64(p.Float64() / v.Float64())
        }
        if v.IsZero() {
            panic(ErrDivisionByZero)
        }
        return &BigFloat{value: new(big.Float).Quo(p.value, v.value)}
    }
    if o.IsZero() {
        panic(ErrDivisionByZero)
    }
    return bigFloatDivide[o.Kind()](p, o)
}
```

#### 1m. Update `init()` same-type dispatch closures (lines 89-113)

The six dispatch closures registered in `init()` are the same-type fast paths used by the dispatch table. They access `p.value` and `o.(*BigFloat).value` directly. They MUST also handle NaN/Inf.

**Apply the same NaN → early return, Inf → float64 delegation pattern to each closure:**

```go
func init() {
    bigFloatAdd = makeAddDispatch(KindBigFloat, func(p *BigFloat, o Number) Number {
        v := o.(*BigFloat)
        if p.nan || v.nan {
            return NewBigFloatNaN()
        }
        if p.value.IsInf() || v.value.IsInf() {
            return NewBigFloatFromFloat64(p.Float64() + v.Float64())
        }
        return &BigFloat{value: new(big.Float).Add(p.value, v.value)}
    })
    // ... same pattern for Subtract, Multiply, Divide
```

**LessThan and Compare closures (lines 98-104):**

```go
    bigFloatLessThan = makeLessThanDispatch(KindBigFloat, func(p *BigFloat, o Number) bool {
        v := o.(*BigFloat)
        // NaN comparisons always return false (IEEE 754)
        if p.nan || v.nan {
            return false
        }
        return p.value.Cmp(v.value) < 0
    })

    bigFloatCompare = makeCompareDispatch(KindBigFloat, func(p *BigFloat, o Number) int {
        v := o.(*BigFloat)
        // NaN is unordered — return 0 (neither less nor greater)
        // This matches Float.Compare behavior for NaN
        if p.nan || v.nan {
            return 0
        }
        return p.value.Cmp(v.value)
    })
```

**Note on Compare for NaN:** Returning 0 means `NaN == NaN` in Compare but `NaN != NaN` in EqualTo. This is the same behavior as `Float.Compare` — verify by checking `Float.Compare` for NaN handling. If Float returns a different value for NaN, match it.

#### 1n. Update `LessThan()` method (line 178-184)

Same-type fast path needs NaN guard:

```go
func (p *BigFloat) LessThan(o Number) bool {
    v, ok := o.(*BigFloat)
    if ok {
        if p.nan || v.nan {
            return false // NaN is not less than anything
        }
        return p.value.Cmp(v.value) < 0
    }
    return bigFloatLessThan[o.Kind()](p, o)
}
```

#### 1o. Update `Compare()` method (line 255-261)

Same-type fast path needs NaN guard:

```go
func (p *BigFloat) Compare(o Number) int {
    v, ok := o.(*BigFloat)
    if ok {
        if p.nan || v.nan {
            return 0 // NaN is unordered
        }
        return p.value.Cmp(v.value)
    }
    return bigFloatCompare[o.Kind()](p, o)
}
```

---

### Phase 2: Extend BigComplex to support Inf/NaN parts

**File: `values/big_complex.go`**

`BigComplex` holds two `Number` parts (real, imag) that can be `*BigInteger`, `*Rational`, or `*BigFloat`. Once BigFloat supports Inf/NaN, BigComplex automatically gains Inf/NaN support through its parts — no structural changes needed to the BigComplex struct.

#### 2a. Update predicates (lines 440-454)

| Method | Line | Before | After |
|--------|------|--------|-------|
| `IsFinite()` | 444 | `return true` | `return p.real.IsFinite() && p.imag.IsFinite()` |
| `IsNaN()` | 452 | `return false` | `return p.real.IsNaN() \|\| p.imag.IsNaN()` |
| `IsRational()` | 436 | `return p.IsReal()` | `return p.IsReal() && p.real.IsRational()` |

Update doc comments to remove "none of which support Inf or NaN" (lines 441-443, 449-451).

**Note:** `IsFinite()` calls `p.real.IsFinite()`. For `*BigInteger` and `*Rational` parts, `IsFinite()` always returns true (they can't be Inf/NaN). For `*BigFloat` parts, `IsFinite()` now correctly checks. Similarly for `IsNaN()`.

#### 2b. Update `SchemeString()` (line 573-590)

The existing implementation delegates to `p.real.SchemeString()` and `p.imag.SchemeString()`. Since BigFloat's `SchemeString()` now handles Inf/NaN (Phase 1e), this should work correctly without changes.

**However:** the `isNeg` check (lines 577-585) calls `v.IsNegative()` on the imag part. For BigFloat NaN, `IsNegative()` returns false (Phase 1d), so NaN imag gets a `+` prefix: `"+inf.0+nan.0i"`. This is correct per Scheme conventions.

**Verify:** For BigFloat Inf imag, `IsNegative()` returns true for -Inf and false for +Inf. The sign prefix will be correct.

No changes needed. Verify with test cases.

#### 2c. Verify BigComplex arithmetic propagates Inf/NaN correctly

BigComplex arithmetic uses `addParts`, `subtractParts`, `multiplyParts`, `divideParts` (lines 134-252). These helper functions dispatch to BigFloat arithmetic when any part is BigFloat. Since BigFloat now handles Inf/NaN (Phase 1l), the helpers propagate correctly.

**Verify for Multiply:** BigComplex multiply uses `(a+bi)(c+di) = (ac-bd) + (ad+bc)i`. With `a=Inf, b=0, c=3, d=4`:
- `ac = Inf*3 = Inf`, `bd = 0*4 = 0`, `ad = Inf*4 = Inf`, `bc = 0*3 = 0`
- `real = Inf - 0 = Inf`, `imag = Inf + 0 = Inf`
- Result: `Inf + Inf*i` ✓

But the parts flow through `promoteToBigComplexPart` (line 118-132), which calls `NewBigFloatFromFloat64(v.Value)` for `*Float` inputs (line 129). Phase 1c ensures this handles Inf/NaN correctly.

**Potential issue with `multiplyParts`:** When `a=BigFloat(Inf)` and `b=BigInteger(4)`, `multiplyParts` dispatches to the `*BigFloat` case (line 219-220): `va.Multiply(toBigFloat(b))`. `toBigFloat` converts `BigInteger(4)` to `BigFloat(4)`. Then `BigFloat(Inf).Multiply(BigFloat(4))` is called — this is handled by Phase 1l.

**Potential issue with `maybeSimplify`:** (line 110-115) calls `iam.IsZero()`. BigFloat NaN's `IsZero()` returns false (Phase 1d). BigFloat Inf's `IsZero()` returns false. Both are correct — a BigComplex with NaN or Inf imaginary part should NOT simplify to a real.

**Demotion via Multiply's zero-check guards** (lines 349-354): `Integer(0) * BigComplex(Inf, 4)` hits `o.IsZero()`, calls `multiplyResultForZero(Integer(0), BigComplex(Inf, 4))`, returns `NewInteger(0)` — demoting BigComplex to Integer. Same demotion pattern as the dispatch guard, same root cause: skipping the dispatch table that would keep the result in the correct domain.

**Fix:** Add `&& p.IsFinite() && !p.IsNaN()` to the first zero-check guard, matching the pattern `Float.Multiply` already uses (`float.go:138`):

```go
func (p *BigComplex) Multiply(o Number) Number {
    if o.IsZero() && p.IsFinite() && !p.IsNaN() {
        return multiplyResultForZero(o, p)
    }
    if p.IsZero() && o.IsFinite() {
        return multiplyResultForZero(p, o)
    }
    return bigComplexMultiply[o.Kind()](p, o)
}
```

When `p` is BigComplex with Inf/NaN parts, the zero-check is skipped. Dispatch promotes `Integer(0) → BigComplex(BigInteger(0), BigInteger(0))`, same-type closure runs `multiplyParts` which delegates to BigFloat arithmetic: `BigFloat(Inf) * BigFloat(0) → BigFloat(NaN)`. No demotion.

Apply the same fix to `BigFloat.Multiply` (line 141-153):

```go
func (p *BigFloat) Multiply(o Number) Number {
    v, ok := o.(*BigFloat)
    if ok {
        if p.nan || v.nan {
            return NewBigFloatNaN()
        }
        if p.value.IsInf() || v.value.IsInf() {
            return NewBigFloatFromFloat64(p.Float64() * v.Float64())
        }
        return &BigFloat{value: new(big.Float).Mul(p.value, v.value)}
    }
    if o.IsZero() && p.IsFinite() && !p.IsNaN() {
        return multiplyResultForZero(o, p)
    }
    if p.IsZero() && o.IsFinite() {
        return multiplyResultForZero(p, o)
    }
    return bigFloatMultiply[o.Kind()](p, o)
}
```

#### 2d. Update `toExactPart` (line 486-506)

BigFloat parts may now be Inf or NaN. `toExactPart` is called by `ToExact()`. BigFloat's `ToExact()` already panics for Inf/NaN (Phase 1i), so the delegation via `toExactPart → BigFloat case → v.value.Float64() → big.Rat.SetFloat64()` is no longer needed — BigFloat.ToExact handles it.

But wait — `toExactPart` doesn't call `BigFloat.ToExact()`. It has its own conversion (line 490-503). This conversion will fail on Inf/NaN because `v.value.Float64()` returns Inf/NaN, and `big.Rat.SetFloat64(Inf)` returns nil, triggering `panic(ErrExactnessConversion)` at line 497. This is actually correct behavior — `(exact +inf.0)` should error.

**No changes needed.** The existing panic at line 497 produces the right behavior for Inf. For NaN, `v.value` is a zero-value `big.Float`, so `v.value.Float64()` returns 0, and `big.Rat.SetFloat64(0)` returns `0/1`. This is WRONG — it silently converts NaN to 0.

**Fix:** Add NaN check at the top of the BigFloat case:

```go
case *BigFloat:
    if v.nan {
        panic(WrapForeignErrorf(ErrExactnessConversion, "toExactPart: cannot convert NaN to exact"))
    }
    // existing code handles Inf correctly (SetFloat64 returns nil → panic)
```

#### 2e. Update `toBigFloat` helper (line 94-107)

`toBigFloat` is called extensively by BigComplex methods (`Magnitude`, `Phase`, `EqualTo`, `HashCode`, `Compare`). When BigFloat now has NaN, `toBigFloat` must pass it through. For `*BigInteger` and `*Rational` inputs, no change needed. For `*BigFloat` input, it's already identity — no change.

**No changes needed.**

#### 2f. Update `Magnitude()` (line 547-555) and `Phase()` (line 559-565)

`Magnitude()` calls `toBigFloat(p.real)` and `toBigFloat(p.imag)`, then does `big.Float.Mul` and `big.Float.Sqrt`. If either part is NaN BigFloat, `part.value` is a zero big.Float (not reflecting NaN). The result would be wrong (it would compute `sqrt(0+0)=0` instead of NaN).

**Fix:** Add NaN/Inf guard:

```go
func (p *BigComplex) Magnitude() *BigFloat {
    if p.IsNaN() {
        return NewBigFloatNaN()
    }
    a := toBigFloat(p.real)
    b := toBigFloat(p.imag)
    if a.value.IsInf() || b.value.IsInf() {
        return NewBigFloatInf(1) // |anything with Inf| = +Inf
    }
    // existing code
}
```

`Phase()` converts to float64 and uses `math.Atan2`. Float64 conversion handles NaN/Inf (Phase 1g). `math.Atan2(NaN, x) = NaN`, `math.Atan2(y, Inf) = 0 or ±π`. These are correct IEEE 754 behaviors. No changes needed.

#### 2g. Update `HashCode()` (line 637-641)

Calls `toBigFloat(p.real).value` and `toBigFloat(p.imag).value`, passing to `hashInexactNumeric`. For NaN BigFloat, `value` is a zero big.Float — the hash would be wrong (hashes as 0 instead of NaN).

**Fix:** Delegate to the parts' HashCode methods:

```go
func (p *BigComplex) HashCode() uint64 {
    r := toBigFloat(p.real).HashCode()
    i := toBigFloat(p.imag).HashCode()
    return r ^ (i * 0x9e3779b97f4a7c15)
}
```

This works because BigFloat.HashCode now handles NaN/Inf (Phase 1h).

#### 2h. Update `EqualTo()` (line 600-630)

The BigFloat comparison path (`toBigFloat(p.real).Compare(v.real)`) needs NaN awareness. `toBigFloat` on a NaN BigFloat returns the NaN BigFloat itself. `Compare` on NaN returns 0 (Phase 1o) — but this means `NaN == NaN` in BigComplex.EqualTo, which violates IEEE 754.

**Fix:** Add NaN check:

```go
func (p *BigComplex) EqualTo(o Value) bool {
    v, ok := o.(*BigComplex)
    if !ok {
        c, ok := o.(*Complex)
        if ok {
            // NaN != anything
            if p.IsNaN() || math.IsNaN(real(c.Value)) || math.IsNaN(imag(c.Value)) {
                return false
            }
            pReal := toBigFloat(p.real).Float64()
            pImag := toBigFloat(p.imag).Float64()
            return pReal == real(c.Value) && pImag == imag(c.Value)
        }
        return false
    }
    if v == nil || p == nil {
        return p == v
    }
    // NaN != anything (including NaN)
    if p.IsNaN() || v.IsNaN() {
        return false
    }
    // existing comparison code
```

---

### Phase 3: Remove the Inf/NaN guard from dispatch generators

**File: `values/promotion.go`**

#### 3a. Simplify `makeArithmeticDispatch` (lines 350-410)

Remove `lubNeedsGuard`, the two guard branches, the `float64Op` parameter, and the `complex128Op` parameter. The function simplifies to:

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
        promSrc := promoter[srcKind][promotionTable[srcKind][dstKind]]
        promDst := promoter[dstKind][promotionTable[srcKind][dstKind]]
        table[dstKind] = func(p T, o Number) Number {
            return applyOp(promSrc(p), promDst(o))
        }
    }
    return table
}
```

**Note:** Keep `lubKind` as a local for the closure capture (same pattern as existing code — `promSrc` and `promDst` are captured per iteration).

#### 3b. Simplify `makeLessThanDispatch` (lines 478-517)

Remove `lubNeedsGuard` and guard branches. Same simplification:

```go
func makeLessThanDispatch[T Number](
    srcKind NumericKind,
    sameTypeLT func(T, Number) bool,
) [numKinds]func(T, Number) bool {
    ensurePromotionInit()
    var table [numKinds]func(T, Number) bool
    table[srcKind] = sameTypeLT
    for dstKind := range numKinds {
        if dstKind == srcKind {
            continue
        }
        promSrc := promoter[srcKind][promotionTable[srcKind][dstKind]]
        promDst := promoter[dstKind][promotionTable[srcKind][dstKind]]
        table[dstKind] = func(p T, o Number) bool {
            return promSrc(p).LessThan(promDst(o))
        }
    }
    return table
}
```

#### 3c. Simplify `makeCompareDispatch` (lines 525-564)

Same pattern. Remove guard.

#### 3d. Update dispatch caller signatures

The four callers (`makeAddDispatch`, `makeSubtractDispatch`, `makeMultiplyDispatch`, `makeDivideDispatch`) at lines 412-470 each pass `float64Op` and `complex128Op` closures. Remove those arguments:

**Before (e.g., makeAddDispatch):**
```go
func makeAddDispatch[T Number](...) [numKinds]func(T, Number) Number {
    return makeArithmeticDispatch(srcKind, sameTypeAdd,
        func(a, b Number) Number { return a.Add(b) },
        func(a, b float64) float64 { return a + b },
        func(a, b complex128) complex128 { return a + b },
    )
}
```

**After:**
```go
func makeAddDispatch[T Number](...) [numKinds]func(T, Number) Number {
    return makeArithmeticDispatch(srcKind, sameTypeAdd,
        func(a, b Number) Number {
            return a.Add(b)
        },
    )
}
```

Apply to all four callers.

#### 3e. Remove dead code

Delete the following functions from `promotion.go`:

| Function | Lines | Why |
|----------|-------|-----|
| `isSpecialFloat` | 260-265 | No longer used — guard removed |
| `numberToFloat64` | 267-289 | No longer used by dispatch — only used by guard paths. Check if anything else calls it. If only the guard used it, delete. If FFI or other code uses it, keep. |
| `numberToComplex128` | 296-315 | Added by the earlier #362 branch fix — no longer needed |
| `cmpFloat64` | 317-326 | Only used by `makeCompareDispatch` guard — delete if guard is removed |

**Before deleting `numberToFloat64`:** Search for callers outside `promotion.go`:
```
grep -rn 'numberToFloat64' values/
```
If it's only called in the guard paths, delete it. If other code calls it, keep it.

#### 3f. Update comments

Remove all "IEEE 754 guard" comments from `makeArithmeticDispatch`, `makeLessThanDispatch`, `makeCompareDispatch`, and the `makeXxxDispatch` callers. Update the doc comments to reflect that BigFloat/BigComplex now handle Inf/NaN natively.

---

### Phase 4: Update promoters for Float(Inf/NaN) → BigFloat

**File: `values/promotion.go`**

#### 4a. Float → BigFloat promoter (lines 202-205)

**Before:**
```go
promoter[KindFloat][KindBigFloat] = func(n Number) Number {
    p := n.(*Float)
    return &BigFloat{value: new(big.Float).SetPrec(DefaultBigFloatPrecision).SetFloat64(p.Value)}
}
```

`SetFloat64` panics on Inf and NaN. Replace with `NewBigFloatFromFloat64` (which now handles Inf/NaN — Phase 1c):

**After:**
```go
promoter[KindFloat][KindBigFloat] = func(n Number) Number {
    p := n.(*Float)
    return NewBigFloatFromFloat64(p.Value)
}
```

#### 4b. Float → BigComplex promoter (lines 214-220)

Already uses `NewBigFloatFromFloat64(p.Value)` (line 217). Phase 1c makes this Inf/NaN safe. **No changes needed.**

#### 4c. Complex → BigComplex promoter (lines 229-234)

Uses `NewBigFloatFromFloat64(real(p.Value))` and `NewBigFloatFromFloat64(imag(p.Value))`. Phase 1c makes this Inf/NaN safe. **No changes needed.**

---

## Cleanup: Existing #362 Branch

The branch `fix/362-bigcomplex-infnan-guard` has a partial fix that added `numberToComplex128` and `complex128Op` to the dispatch. This work is superseded by this plan. The branch should be:

1. Reset to master (or create a new branch)
2. The `complex128Op` parameter and `numberToComplex128` function are deleted as part of Phase 3e
3. The plan and doc file changes from this conversation should be preserved (cherry-pick or re-apply)

---

## Result Type After Fix

With the guard removed, `Float(+inf.0) + BigComplex(3, 4)` follows the normal lattice:

1. Promotion: `Float(+inf.0)` → `BigFloat(+inf.0)` via `NewBigFloatFromFloat64` (handles Inf)
2. LUB: `BigFloat × BigComplex → BigComplex`
3. Both promoted to BigComplex: `BigComplex(BigFloat(+inf.0), BigFloat(0)) + BigComplex(BigInteger(3), BigInteger(4))`
4. `addParts` dispatches to BigFloat arithmetic for each component
5. Real: `BigFloat(+inf.0) + toBigFloat(BigInteger(3))` = `BigFloat(+inf.0)` (Inf + finite = Inf)
6. Imag: `BigFloat(0) + toBigFloat(BigInteger(4))` = `BigFloat(4)`
7. Result: `BigComplex(BigFloat(+inf.0), BigFloat(4))` — **Inf real represented natively, imaginary preserved**

No precision loss. No domain switching. The lattice works.

---

## Testing

### Unit tests for BigFloat Inf/NaN (Phase 1)

**File: `values/big_float_test.go`**

Test IEEE 754 compliance for BigFloat arithmetic with special values:

| Category | Test | Expected |
|----------|------|----------|
| Constructors | `NewBigFloatInf(1).IsFinite()` | `false` |
| Constructors | `NewBigFloatInf(1).IsNaN()` | `false` |
| Constructors | `NewBigFloatNaN().IsNaN()` | `true` |
| Constructors | `NewBigFloatNaN().IsFinite()` | `false` |
| Constructors | `NewBigFloatFromFloat64(math.Inf(1)).IsFinite()` | `false` |
| Constructors | `NewBigFloatFromFloat64(math.NaN()).IsNaN()` | `true` |
| Predicates | `NewBigFloatInf(1).IsZero()` | `false` |
| Predicates | `NewBigFloatNaN().IsZero()` | `false` |
| Predicates | `NewBigFloatInf(1).IsRational()` | `false` |
| Predicates | `NewBigFloatNaN().IsRational()` | `false` |
| Predicates | `NewBigFloatInf(1).IsInteger()` | `false` |
| SchemeString | `NewBigFloatInf(1).SchemeString()` | `"+inf.0"` |
| SchemeString | `NewBigFloatInf(-1).SchemeString()` | `"-inf.0"` |
| SchemeString | `NewBigFloatNaN().SchemeString()` | `"+nan.0"` |
| Float64 | `NewBigFloatInf(1).Float64()` | `math.Inf(1)` |
| Float64 | `NewBigFloatNaN().Float64()` | `math.NaN()` |
| EqualTo | `NewBigFloatNaN().EqualTo(NewBigFloatNaN())` | `false` |
| EqualTo | `NewBigFloatInf(1).EqualTo(NewBigFloatInf(1))` | `true` |
| EqualTo | `NewBigFloatInf(1).EqualTo(NewBigFloatInf(-1))` | `false` |
| Add | `BigFloat(Inf) + BigFloat(1)` | `BigFloat(Inf)` |
| Add | `BigFloat(Inf) + BigFloat(-Inf)` | `BigFloat(NaN)` |
| Add | `BigFloat(NaN) + BigFloat(1)` | `BigFloat(NaN)` |
| Multiply | `BigFloat(Inf) * BigFloat(0)` | `BigFloat(NaN)` |
| Multiply | `BigFloat(Inf) * BigFloat(2)` | `BigFloat(Inf)` |
| Multiply | `BigFloat(Inf) * BigFloat(-1)` | `BigFloat(-Inf)` |
| Divide | `BigFloat(Inf) / BigFloat(Inf)` | `BigFloat(NaN)` |
| Divide | `BigFloat(1) / BigFloat(Inf)` | `BigFloat(0)` |
| Divide | `BigFloat(Inf) / BigFloat(2)` | `BigFloat(Inf)` |
| LessThan | `BigFloat(NaN) < BigFloat(1)` | `false` |
| LessThan | `BigFloat(1) < BigFloat(NaN)` | `false` |
| LessThan | `BigFloat(Inf) < BigFloat(1)` | `false` |
| LessThan | `BigFloat(1) < BigFloat(Inf)` | `true` |
| Negate | `-BigFloat(NaN)` is NaN | `true` |
| Negate | `-BigFloat(Inf)` | `BigFloat(-Inf)` |
| ToExact | `BigFloat(Inf).ToExact()` | panic (ErrExactnessConversion) |
| ToExact | `BigFloat(NaN).ToExact()` | panic (ErrExactnessConversion) |
| Hash | `BigFloat(Inf).HashCode() == BigFloat(Inf).HashCode()` | `true` |
| Hash | `Float(Inf).HashCode() == BigFloat(Inf).HashCode()` | `true` (cross-type consistency) |

### Unit tests for #362 (the original bug)

**File: `values/promotion_test.go` or `values/big_complex_test.go`**

Test all 4 operations × 2 directions × 3 special values:

| Op | Float | BigComplex | Expected result type | Expected value | Notes |
|----|-------|-----------|---------------------|----------------|-------|
| Add | `+inf.0` | `3+4i` | `*BigComplex` | Inf real, 4 imag | |
| Add | `-inf.0` | `3+4i` | `*BigComplex` | -Inf real, 4 imag | |
| Add | `+nan.0` | `3+4i` | `*BigComplex` | NaN real, 4 imag | |
| Sub | `+inf.0` | `3+4i` | `*BigComplex` | Inf real, -4 imag | |
| Mul | `+inf.0` | `3+4i` | `*BigComplex` | Inf real, Inf imag | Complex mul |
| Div | `+inf.0` | `3+4i` | `*BigComplex` | Per IEEE 754 | |
| Add (rev) | `3+4i` (BigComplex) | `+inf.0` (Float) | `*BigComplex` | Inf real, 4 imag | Commutativity |
| Mul (rev) | `3+4i` (BigComplex) | `+inf.0` (Float) | `*BigComplex` | Inf real, Inf imag | |

**Critical assertion:** Result type is `*BigComplex`, NOT `*Complex` or `*Float`.

Construct BigComplex with `*BigInteger` parts to ensure it's truly BigComplex:
```go
bc := NewBigComplex(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(4))
f := NewFloat(math.Inf(1))
result := f.Add(bc)
// Assert: result is *BigComplex
// Assert: result.(*BigComplex).Real() is BigFloat with Inf
```

### Integration tests

**File: `integration/testdata/r7rs-tests.scm`**

```scheme
;; #362: Float(Inf/NaN) + BigComplex preserves imaginary part
(test-equal "+inf.0+4i" (number->string (+ +inf.0 (make-rectangular 3 4))))
(test-equal "+inf.0+4i" (number->string (+ (make-rectangular 3 4) +inf.0)))
(test-equal "+nan.0+4i" (number->string (+ +nan.0 (make-rectangular 3 4))))

;; BigFloat Inf/NaN predicates
(test-assert (infinite? +inf.0))
(test-assert (nan? +nan.0))
(test-assert (not (nan? +inf.0)))
(test-assert (not (finite? +inf.0)))
```

**Note:** The exact string format for BigComplex with Inf/NaN BigFloat parts depends on `BigComplex.SchemeString()` → `BigFloat.SchemeString()`. Verify the expected strings after implementation. The format might be `+inf.0+4i` or `+inf.0+4.0i` depending on whether the imaginary part is BigInteger or BigFloat.

---

## Verification Checklist

After implementation, verify:

1. `make build` passes
2. `make test` passes (all existing tests still green)
3. `make lint` passes
4. `make covercheck` passes
5. BigFloat Inf/NaN tests cover all IEEE 754 arithmetic rules
6. BigFloat cross-type hash consistency (Float(Inf) == BigFloat(Inf) hash)
7. #362 tests cover both directions (Float as receiver, Float as operand)
8. Multiplication test confirms complex semantics (not component-wise)
9. `isSpecialFloat` guard is removed from all three dispatch generators
10. `float64Op`, `complex128Op` parameters removed from `makeArithmeticDispatch`
11. `numberToFloat64`, `numberToComplex128`, `cmpFloat64` removed (if no other callers)
12. No Tier 4 precision loss — result stays in BigComplex domain
13. `NewBigFloatFromFloat64(math.Inf(1))` does not panic
14. `NewBigFloatFromFloat64(math.NaN())` does not panic
15. Float → BigFloat promoter does not panic on Inf/NaN
16. `BigFloat(Inf) * Integer(0)` returns `BigFloat(NaN)`, not `Integer(0)`
17. `BigComplex(Inf, 4) * Integer(0)` returns `BigComplex(NaN, NaN)`, not `Integer(0)`
18. No code path in BigFloat or BigComplex produces `NewFloat`, `NewInteger`, or `NewComplex` as a result of Inf/NaN

## Summary of Changes

| What | Where | Phase |
|------|-------|-------|
| Add `nan bool` field to BigFloat struct | `values/big_float.go:33` | 1a |
| Add `NewBigFloatInf`, `NewBigFloatNaN` constructors | `values/big_float.go` (after line 55) | 1b |
| Fix `NewBigFloatFromFloat64` for Inf/NaN inputs | `values/big_float.go:43` | 1c |
| Update all BigFloat predicates (6 methods) | `values/big_float.go` | 1d |
| Update `SchemeString` for Inf/NaN | `values/big_float.go:264` | 1e |
| Update `EqualTo` for NaN != NaN | `values/big_float.go:274` | 1f |
| Update `Float64` for Inf/NaN | `values/big_float.go:63` | 1g |
| Update `HashCode` for Inf/NaN | `values/big_float.go:71` | 1h |
| Update `ToExact` to panic on Inf/NaN | `values/big_float.go:230` | 1i |
| Update `Abs` for Inf/NaN | `values/big_float.go:245` | 1j |
| Update `Negate` for NaN | `values/big_float.go:168` | 1k |
| Update Add/Sub/Mul/Div for Inf/NaN | `values/big_float.go:118-165` | 1l |
| Update `init()` dispatch closures (6 closures) | `values/big_float.go:89-113` | 1m |
| Update `LessThan` for NaN | `values/big_float.go:178` | 1n |
| Update `Compare` for NaN | `values/big_float.go:255` | 1o |
| Fix BigComplex.Multiply zero-check guard (add `p.IsFinite()`) | `values/big_complex.go:349` | 2c |
| Fix BigFloat.Multiply zero-check guard (add `p.IsFinite()`) | `values/big_float.go:142` | 1l |
| Update BigComplex `IsFinite`, `IsNaN`, `IsRational` | `values/big_complex.go:436-454` | 2a |
| Update `toExactPart` NaN guard | `values/big_complex.go:486` | 2d |
| Update `Magnitude` for Inf/NaN | `values/big_complex.go:547` | 2f |
| Update `HashCode` to use BigFloat.HashCode | `values/big_complex.go:637` | 2g |
| Update `EqualTo` NaN guard | `values/big_complex.go:600` | 2h |
| Simplify `makeArithmeticDispatch` (remove guard) | `values/promotion.go:350-410` | 3a |
| Simplify `makeLessThanDispatch` (remove guard) | `values/promotion.go:478-517` | 3b |
| Simplify `makeCompareDispatch` (remove guard) | `values/promotion.go:525-564` | 3c |
| Remove `float64Op`/`complex128Op` from 4 callers | `values/promotion.go:412-470` | 3d |
| Delete `isSpecialFloat`, `numberToComplex128`, `cmpFloat64` | `values/promotion.go` | 3e |
| Delete or keep `numberToFloat64` (check callers) | `values/promotion.go:267` | 3e |
| Fix Float→BigFloat promoter | `values/promotion.go:202` | 4a |
| Tests for BigFloat Inf/NaN | `values/big_float_test.go` | — |
| Tests for #362 | `values/big_complex_test.go` or `values/promotion_test.go` | — |
| Integration tests | `integration/testdata/r7rs-tests.scm` | — |
