# N4: PrimExpt & PrimMakeRectangular Refactoring

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Eliminate hand-unrolled type dispatch in PrimExpt (~157 lines → ~70) and PrimMakeRectangular (~85 lines → ~55) by extracting helpers and reusing existing conversion functions.

**Architecture:** Export `NumberToFloat64` and `NumberToComplex128` from `values/promotion.go` (already exist as unexported functions). Extract `exptExact` helper for the exact-integer-exponent path using a unified rational representation. Delete the duplicate `numberToFloat64` from `extensions/math/prim_math.go`.

**Tech Stack:** Go, `math/big`, `math/cmplx`, existing `values.Simplify`

**Reference:** `plans/STAFF_ENGINEER_REVIEW.md` finding N4

---

## Task 1: Export NumberToFloat64 and NumberToComplex128

**Files:**
- Modify: `values/promotion.go` (rename functions)
- Modify: all internal callers of the unexported names

**Step 1: Rename `numberToFloat64` → `NumberToFloat64` in `values/promotion.go`**

Use `go_rename_symbol` on `numberToFloat64` in `values/promotion.go` to `NumberToFloat64`. This automatically updates all internal callers within the `values` package.

**Step 2: Rename `numberToComplex128` → `NumberToComplex128` in `values/promotion.go`**

Same approach: `go_rename_symbol` on `numberToComplex128` to `NumberToComplex128`.

**Step 3: Run diagnostics**

Run: `go_diagnostics` on `values/promotion.go`
Expected: No errors (rename is mechanical).

**Step 4: Run tests**

Run: `go test -count=1 ./values/...`
Expected: All pass. This is a pure rename within the package.

**Step 5: Commit**

```
refactor(values): export NumberToFloat64 and NumberToComplex128

These conversion functions already existed as unexported helpers in
promotion.go. Exporting them enables extensions to use them instead
of hand-rolling type switches.
```

---

## Task 2: Delete duplicate `numberToFloat64` from extensions/math

**Files:**
- Modify: `extensions/math/prim_math.go` (delete function, update call site)

**Step 1: Update the one call site**

Line 213 in `prim_math.go` calls `numberToFloat64(v)`. Change it to `values.NumberToFloat64(v)`.

The local version returns NaN for unknown types; the values/ version panics. At this call site, `v` is a validated real number (it's inside a `*values.Float` / `*values.Integer` / etc. type switch), so neither fallback is reachable.

**Step 2: Delete the local `numberToFloat64` function**

Delete lines 1110-1127 (the function definition and its comment).

**Step 3: Run diagnostics**

Run: `go_diagnostics` on `extensions/math/prim_math.go`
Expected: No errors.

**Step 4: Run tests**

Run: `go test -count=1 ./extensions/math/...`
Expected: All pass.

**Step 5: Commit**

```
refactor(math): remove duplicate numberToFloat64

Use the newly-exported values.NumberToFloat64 instead of the
package-local duplicate. One call site updated (complexSqrtR7RS
fallback path).
```

---

## Task 3: Extract `exptExact` helper

**Files:**
- Modify: `extensions/math/prim_math.go` (add helper, refactor PrimExpt exact path)

**Step 1: Write the failing test**

Add a test to `prim_math_test.go` that exercises exact exponentiation edge cases not covered by existing tests. Specifically: `(expt 0 0)` returns exact 1, `(expt 1/2 -2)` returns exact 4, and `(expt 0 5)` returns exact 0.

```go
// In TestExptAdditionalCases or new test function:
{"expt 1/2 neg exp", `(= (expt 1/2 -2) 4)`, values.TrueValue},
{"expt 1/2 neg exp exact", `(exact? (expt 1/2 -2))`, values.TrueValue},
{"expt 0 positive", `(= (expt 0 5) 0)`, values.TrueValue},
```

**Step 2: Run tests to verify they pass (baseline)**

Run: `go test -count=1 -run TestExptAdditionalCases ./extensions/math/...`
Expected: PASS (these should work with current code; they're regression guards).

**Step 3: Add the `exptExact` helper**

Add this function to `prim_math.go`, near the existing `PrimExpt`:

```go
// exptExact computes (num/denom)^exp exactly.
// For integer bases, pass denom as 1 (use bigOne).
// Result is always simplified via values.Simplify.
func exptExact(num, denom *big.Int, exp int64) values.Number {
	if exp >= 0 {
		e := big.NewInt(exp)
		n := new(big.Int).Exp(num, e, nil)
		d := new(big.Int).Exp(denom, e, nil)
		return values.Simplify(values.NewRationalFromBigInt(n, d))
	}
	absE := big.NewInt(-exp)
	// Invert: (num/denom)^(-e) = (denom^e)/(num^e)
	n := new(big.Int).Exp(denom, absE, nil)
	d := new(big.Int).Exp(num, absE, nil)
	return values.Simplify(values.NewRationalFromBigInt(n, d))
}
```

Also add a package-level `var bigOne = big.NewInt(1)` if one doesn't already exist.

**Step 4: Replace PrimExpt's exact path with `exptExact`**

Replace lines 298-360 (the `expInt, ok := expNum.(*values.Integer)` block with its three nested base-type checks) AND lines 403-413 (the L17 BigInteger special case in the default branch) with a single block using `values.ExactInteger`:

```go
if e, ok := values.ExactInteger(expNum); ok {
	switch b := baseNum.(type) {
	case *values.Integer:
		mc.SetValue(exptExact(big.NewInt(b.Value), bigOne, e))
		return nil
	case *values.BigInteger:
		mc.SetValue(exptExact(b.BigInt(), bigOne, e))
		return nil
	case *values.Rational:
		mc.SetValue(exptExact(b.Num(), b.Denom(), e))
		return nil
	}
	// Non-exact base types (Float, Complex, etc.) fall through
	// to inexact paths below.
}
```

This replaces ~75 lines with ~12.

**Step 5: Run tests**

Run: `go test -count=1 ./extensions/math/...`
Expected: All pass. The `exptExact` helper produces identical results for all existing test cases.

**Step 6: Commit**

```
refactor(math): extract exptExact helper for exact integer exponentiation

Unifies Integer, BigInteger, and Rational base handling into a single
function using rational representation (num/denom). Replaces ~75 lines
of hand-unrolled type dispatch with a 12-line dispatcher and a 15-line
helper. Uses values.ExactInteger instead of *Integer type assertion,
which also subsumes the L17 BigInteger special case.
```

---

## Task 4: Collapse PrimExpt's complex and float paths

**Files:**
- Modify: `extensions/math/prim_math.go` (refactor PrimExpt remaining paths)

**Step 1: Write regression test for complex expt**

Add tests for complex exponentiation (currently untested):

```go
// In a new TestExptComplex function or added to TestExptAdditionalCases:
{"complex base integer exp", `(< (abs (- (expt 1+1i 2) 0+2i)) 1e-10)`, values.TrueValue},
{"complex base float exp", `(number? (expt 1+1i 0.5))`, values.TrueValue},
{"bigcomplex base", `(number? (expt (make-rectangular 1 1) 2))`, values.TrueValue},
{"float base complex exp", `(number? (expt 2.0 1+1i))`, values.TrueValue},
```

**Step 2: Run to verify they pass (baseline)**

Run: `go test -count=1 -run TestExptComplex ./extensions/math/...`
Expected: PASS (existing code handles these).

**Step 3: Replace complex path**

Replace the `case *values.Complex` and `case *values.BigComplex` switch arms (lines ~362-401, with their nested 11-case type switches) with:

```go
case *values.Complex, *values.BigComplex:
	mc.SetValue(values.NewComplex(cmplx.Pow(
		values.NumberToComplex128(baseNum),
		values.NumberToComplex128(expNum))))
	return nil
```

This replaces ~40 lines with 4.

**Step 4: Replace float fallback**

Replace the `default` arm's remaining float-conversion code (the two type switches converting base and exp to float64, plus the complex-exp special case at line 437) with:

```go
default:
	// Complex exponent with real base
	if _, isComplex := expNum.(*values.Complex); isComplex {
		mc.SetValue(values.NewComplex(cmplx.Pow(
			complex(values.NumberToFloat64(baseNum), 0),
			values.NumberToComplex128(expNum))))
		return nil
	}
	if _, isBigComplex := expNum.(*values.BigComplex); isBigComplex {
		mc.SetValue(values.NewComplex(cmplx.Pow(
			complex(values.NumberToFloat64(baseNum), 0),
			values.NumberToComplex128(expNum))))
		return nil
	}
	mc.SetValue(values.NewFloat(math.Pow(
		values.NumberToFloat64(baseNum),
		values.NumberToFloat64(expNum))))
	return nil
```

Note: The complex-exponent check must stay in default because the outer switch matched on `baseNum`, not `expNum`. A real base with a complex exponent falls to default.

**Step 5: Run tests**

Run: `go test -count=1 ./extensions/math/...`
Expected: All pass.

**Step 6: Commit**

```
refactor(math): collapse PrimExpt complex and float paths

Complex path: 11 cmplx.Pow calls across 2 nested type switches
replaced by single call using values.NumberToComplex128.

Float path: 2 type switches for float64 conversion replaced by
values.NumberToFloat64 calls.

PrimExpt is now ~55 lines (down from 157).
```

---

## Task 5: Simplify PrimMakeRectangular

**Files:**
- Modify: `extensions/math/prim_math.go` (refactor PrimMakeRectangular)

**Step 1: Verify existing test coverage**

Run: `go test -count=1 -run "TestComplexOps|TestMakeRectangularExactTypes" ./extensions/math/...`
Expected: All pass (baseline).

**Step 2: Replace inline type switches with NumberToFloat64**

In `PrimMakeRectangular`, replace the two 4-case type switches at the bottom (the `// Use regular Complex for inexact numbers` section, lines ~1050-1068) with:

```go
	// Use regular Complex for inexact numbers
	realPart := values.NumberToFloat64(rNum)
	imagPart := values.NumberToFloat64(iNum)
	mc.SetValue(values.NewComplexFromParts(realPart, imagPart))
	return nil
```

Remove the two `default` error branches — they're unreachable because `isRealNumber()` validation at the top already rejected non-numeric types.

**Step 3: Run tests**

Run: `go test -count=1 ./extensions/math/...`
Expected: All pass.

**Step 4: Run full lint and covercheck**

Run: `make lint && make covercheck`
Expected: Clean.

**Step 5: Commit**

```
refactor(math): simplify PrimMakeRectangular float conversion

Replace two 4-case type switches with values.NumberToFloat64 calls.
The switches were hand-unrolled versions of the same conversion.
```

---

## Task 6: Update plans and verify

**Files:**
- Modify: `plans/STAFF_ENGINEER_REVIEW.md` (mark N4 complete)

**Step 1: Mark N4 as COMPLETE in STAFF_ENGINEER_REVIEW.md**

Add `[COMPLETE]` to the N4 heading, matching the format of N1, N2, N3, N5.

**Step 2: Final verification**

Run: `make lint && make covercheck`
Run: `go test -count=1 ./values/... ./extensions/math/...`
Expected: All clean.

---

## Summary

| Change | Lines removed | Lines added | Net |
|--------|--------------|-------------|-----|
| Export NumberToFloat64/NumberToComplex128 | 0 | 0 (rename only) |  0 |
| Delete duplicate numberToFloat64 | 17 | 1 | -16 |
| Extract exptExact + refactor exact path | ~75 | ~27 | -48 |
| Collapse complex + float paths | ~55 | ~18 | -37 |
| Simplify PrimMakeRectangular | ~20 | ~4 | -16 |
| **Total** | **~167** | **~50** | **-117** |

## Risks

- **`values.NumberToFloat64` handles Complex by returning real part**: Not a problem — all call sites have already filtered out complex types before reaching the float64 conversion. But worth noting in case someone calls it expecting an error for Complex input.
- **`exptExact` extra Rational allocation for integer bases**: Negligible next to `big.Int.Exp`. Verified by existing benchmarks if concern arises.
- **Complex expt test gap**: Current tests don't exercise `(expt complex complex)`. Task 4 adds basic regression tests, but deeper coverage (branch cuts, special values) is out of scope.
