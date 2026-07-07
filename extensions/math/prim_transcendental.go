// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package math

import (
	"math"
	"math/big"
	"math/cmplx"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// makeComplexPrimitive returns a ForeignFunction that converts its argument
// to complex128, applies fn, and returns the result (as Float when imaginary
// part is zero, otherwise Complex). Used for the six unary transcendental
// functions that share identical structure.
func makeComplexPrimitive(name string, fn func(complex128) complex128) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		z, err := helpers.ToComplex128(mc.Arg(0))
		if err != nil {
			return werr.WrapForeignErrorf(err, "%s: %v", name, err)
		}
		mc.SetValue(helpers.ComplexOrFloat(fn(z)))
		return nil
	}
}

// Unary transcendental primitives (R7RS §6.2.6).
var (
	PrimExp  = makeComplexPrimitive("exp", cmplx.Exp)
	PrimSin  = makeComplexPrimitive("sin", cmplx.Sin)
	PrimCos  = makeComplexPrimitive("cos", cmplx.Cos)
	PrimTan  = makeComplexPrimitive("tan", cmplx.Tan)
	PrimAsin = makeComplexPrimitive("asin", cmplx.Asin)
	PrimAcos = makeComplexPrimitive("acos", cmplx.Acos)
)

// PrimLog implements the (log z) and (log z1 z2) primitives.
func PrimLog(mc machine.CallContext) error {
	o := mc.Arg(0)
	rest := mc.Arg(1)
	z, err := helpers.ToComplex128(o)
	if err != nil {
		return werr.WrapForeignErrorf(err, "log: %v", err)
	}
	if values.IsEmptyList(rest) {
		mc.SetValue(helpers.ComplexOrFloat(cmplx.Log(z)))
	} else {
		baseVal, restAfter, err := helpers.Uncons(rest, "log", "base argument")
		if err != nil {
			return err
		}
		if !values.IsEmptyList(restAfter) {
			return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments, "log: expected 1 or 2 arguments")
		}
		base, err := helpers.ToComplex128(baseVal)
		if err != nil {
			return werr.WrapForeignErrorf(err, "log: %v", err)
		}
		mc.SetValue(helpers.ComplexOrFloat(cmplx.Log(z) / cmplx.Log(base)))
	}
	return nil
}

// PrimAtan implements the (atan z) and (atan y x) primitives.
func PrimAtan(mc machine.CallContext) error {
	o := mc.Arg(0)
	rest := mc.Arg(1)

	if values.IsEmptyList(rest) {
		// A real big operand keeps big precision via values.BigAtan; complex
		// operands stay on the float64 complex path (big complex atan needs a
		// big complex log — out of scope).
		if isBigReal(o) {
			mc.SetValue(values.NewBigFloat(values.BigAtan(numberToBigFloat(o), values.DefaultBigFloatPrecision)))
			return nil
		}
		z, err := helpers.ToComplex128(o)
		if err != nil {
			return werr.WrapForeignErrorf(err, "atan: %v", err)
		}
		mc.SetValue(helpers.ComplexOrFloat(cmplx.Atan(z)))
		return nil
	}

	xVal, restAfter, err := helpers.Uncons(rest, "atan", "x argument")
	if err != nil {
		return err
	}
	if !values.IsEmptyList(restAfter) {
		return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments, "atan: expected 1 or 2 arguments")
	}

	// A big operand on either side takes the big-precision atan2 path, so
	// large-magnitude operands with a finite ratio keep their true angle
	// instead of both saturating to ±Inf under float64 truncation.
	if isBigReal(o) || isBigReal(xVal) {
		y := numberToBigFloat(o)
		x := numberToBigFloat(xVal)
		if y == nil || x == nil {
			return werr.WrapForeignErrorf(werr.ErrNotANumber, "atan: expected real arguments")
		}
		mc.SetValue(values.NewBigFloat(values.BigAtan2(y, x, values.DefaultBigFloatPrecision)))
		return nil
	}

	// atan2 inherently returns an inexact result, so silent lossy conversion of
	// *Rational operands is load-bearing for R7RS §6.2.6 semantics — hence
	// ToFloat64Lossy, not the strict ToFloat64. See
	// memory/2026-05-14-numeric-loss-signals-design.md §R8.
	y, err := helpers.ToFloat64Lossy(o)
	if err != nil {
		return werr.WrapForeignErrorf(err, "atan: %v", err)
	}
	x, err := helpers.ToFloat64Lossy(xVal)
	if err != nil {
		return werr.WrapForeignErrorf(err, "atan: %v", err)
	}
	mc.SetValue(values.NewFloat(math.Atan2(y, x)))
	return nil
}

// isBigReal reports whether v is a real number carried at a precision or range
// beyond float64 — the trigger for the big-precision atan paths.
func isBigReal(v values.Value) bool {
	switch v.(type) {
	case *values.BigFloat, *values.BigInteger:
		return true
	default:
		return false
	}
}

// numberToBigFloat converts a real numeric value to *big.Float at default
// precision, or returns nil for non-real / non-numeric values.
func numberToBigFloat(v values.Value) *big.Float {
	prec := uint(values.DefaultBigFloatPrecision)
	switch n := v.(type) {
	case *values.BigFloat:
		return new(big.Float).Set(n.BigFloatValue())
	case *values.BigInteger:
		return new(big.Float).SetPrec(prec).SetInt(n.BigInt())
	case *values.Integer:
		return new(big.Float).SetPrec(prec).SetInt64(n.Value)
	case *values.Float:
		return new(big.Float).SetPrec(prec).SetFloat64(n.Value)
	case *values.Rational:
		return new(big.Float).SetPrec(prec).SetRat(n.Rat())
	default:
		return nil
	}
}

// PrimSqrt implements the (sqrt) primitive.
//
// R7RS §6.2.6: The branch cut for sqrt lies along the negative real axis,
// continuous with quadrant II. This means for values on the negative real axis
// (including those with -0.0 imaginary part), sqrt returns positive imaginary.
func PrimSqrt(mc machine.CallContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer:
		if v.Value < 0 {
			// Check for negative perfect square: result is exact imaginary
			absVal := -v.Value
			root, ok := exactIntegerSqrt(absVal)
			if ok {
				mc.SetValue(values.NewBigComplex(values.NewBigIntegerFromInt64(0), values.NewBigIntegerFromInt64(root)))
				return nil
			}
			mc.SetValue(values.NewComplex(cmplx.Sqrt(complex(float64(v.Value), 0))))
		} else {
			// Check for perfect square: result is exact integer
			root, ok := exactIntegerSqrt(v.Value)
			if ok {
				mc.SetValue(values.NewInteger(root))
				return nil
			}
			mc.SetValue(values.NewFloat(math.Sqrt(float64(v.Value))))
		}
	case *values.BigInteger:
		bi := v.BigInt()
		root := new(big.Int).Sqrt(new(big.Int).Abs(bi))
		check := new(big.Int).Mul(root, root)
		absBI := new(big.Int).Abs(bi)
		if check.Cmp(absBI) == 0 {
			// Perfect square BigInteger
			if bi.Sign() < 0 {
				mc.SetValue(values.NewBigComplex(
					values.NewBigInteger(new(big.Int)),
					values.NewBigInteger(root),
				))
			} else {
				mc.SetValue(values.NewBigInteger(root))
			}
			return nil
		}
		f := new(big.Float).SetInt(bi)
		if bi.Sign() < 0 {
			mc.SetValue(values.NewComplex(cmplx.Sqrt(complex(values.NumberToFloat64(v), 0))))
		} else {
			mc.SetValue(values.NewBigFloat(new(big.Float).Sqrt(f)))
		}
	case *values.Float:
		if v.Value < 0 {
			mc.SetValue(values.NewComplex(cmplx.Sqrt(complex(v.Value, 0))))
		} else {
			mc.SetValue(values.NewFloat(math.Sqrt(v.Value)))
		}
	case *values.Rational:
		// Check if numerator and denominator are both perfect squares
		num := v.Num()
		denom := v.Denom()
		numAbs := new(big.Int).Abs(num)
		numRoot := new(big.Int).Sqrt(numAbs)
		denomRoot := new(big.Int).Sqrt(denom)
		if new(big.Int).Mul(numRoot, numRoot).Cmp(numAbs) == 0 &&
			new(big.Int).Mul(denomRoot, denomRoot).Cmp(denom) == 0 {
			// Perfect square rational
			if num.Sign() < 0 {
				mc.SetValue(values.NewBigComplex(
					values.NewBigInteger(new(big.Int)),
					values.NewRationalFromBigInt(numRoot, denomRoot),
				))
			} else {
				mc.SetValue(values.NewRationalFromBigInt(numRoot, denomRoot))
			}
			return nil
		}
		f := v.Float64Truncated()
		if f < 0 {
			mc.SetValue(values.NewComplex(cmplx.Sqrt(complex(f, 0))))
		} else {
			mc.SetValue(values.NewFloat(math.Sqrt(f)))
		}
	case *values.Complex:
		mc.SetValue(values.NewComplex(complexSqrtR7RS(v.Value)))
	case *values.BigComplex:
		// Compute at big.Float precision; truncating to complex128 first
		// overflows to +inf when a component exceeds the float64 range.
		mc.SetValue(v.Sqrt())
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "sqrt: expected a number but got %T", o)
	}
	return nil
}

// complexSqrtR7RS computes square root with R7RS branch cut semantics.
// R7RS §6.2.6 specifies the branch cut lies along the negative real axis,
// continuous with quadrant II. This means values on the negative real axis
// (real < 0, imag == 0) should return positive imaginary, regardless of
// whether the imaginary part is +0.0 or -0.0.
//
// Go's cmplx.Sqrt follows IEEE 754 conventions where -0.0 imaginary means
// "below the real axis", returning negative imaginary. We correct for this
// by treating -0.0 as +0.0 for branch cut purposes.
func complexSqrtR7RS(z complex128) complex128 {
	re := real(z)
	im := imag(z)

	// If on the negative real axis (real < 0, imag is zero regardless of sign),
	// ensure we return positive imaginary by using +0.0 for the imaginary part.
	if re < 0 && im == 0 {
		// Use positive zero to get the correct branch cut behavior
		return cmplx.Sqrt(complex(re, 0))
	}

	return cmplx.Sqrt(z)
}

var bigOne = big.NewInt(1)

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
	// Use big.Int.Abs to avoid int64 overflow when exp == math.MinInt64.
	absE := new(big.Int).Abs(big.NewInt(exp))
	// Invert: (num/denom)^(-e) = (denom^e)/(num^e)
	n := new(big.Int).Exp(denom, absE, nil)
	d := new(big.Int).Exp(num, absE, nil)
	return values.Simplify(values.NewRationalFromBigInt(n, d))
}

// PrimExpt implements the (expt) primitive.
func PrimExpt(mc machine.CallContext) error {
	base := mc.Arg(0)
	exp := mc.Arg(1)
	baseNum, ok := base.(values.Number)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "expt: expected a number but got %T", base)
	}
	expNum, ok := exp.(values.Number)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "expt: expected a number but got %T", exp)
	}

	e, ok := values.ExactInteger(expNum)
	if ok {
		// Exact zero base with a negative exponent is a true division by zero
		// (1/0^|e|); exptExact would build big.Rat.SetFrac(_, 0) and panic,
		// surfacing as a recovered "internal error" (C7). An INEXACT zero base
		// is left to the IEEE path below, where 0.0^-1 = +inf.0 per R7RS §6.2.6.
		if e < 0 && baseNum.IsExact() && baseNum.IsZero() {
			return werr.WrapForeignErrorf(werr.ErrDivisionByZero,
				"expt: zero base with negative exponent %d", e)
		}
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

	switch baseNum.(type) {
	case *values.Complex, *values.BigComplex:
		mc.SetValue(values.NewComplex(cmplx.Pow(
			values.NumberToComplex128Lossy(baseNum),
			values.NumberToComplex128Lossy(expNum))))
	default:
		// Complex exponent with real base
		switch expNum.(type) {
		case *values.Complex, *values.BigComplex:
			mc.SetValue(values.NewComplex(cmplx.Pow(
				complex(values.NumberToFloat64(baseNum), 0),
				values.NumberToComplex128Lossy(expNum))))
			return nil
		}
		baseF := values.NumberToFloat64(baseNum)
		expF := values.NumberToFloat64(expNum)
		// A negative real base raised to a NON-integer power has no real value
		// (R7RS §6.2.6 — it is complex); math.Pow returns +nan.0 there (C6).
		// An integer-valued exponent (e.g. (expt -2 3.0)) stays real, so guard
		// on the exponent being non-integral, not merely on a negative base.
		// A NaN exponent is excluded (math.Trunc(NaN)!=NaN would otherwise route
		// it to the complex path); leave it on the real IEEE path, matching the
		// prior real +nan.0 result for a degenerate input like (expt -2 +nan.0).
		if baseF < 0 && !math.IsNaN(expF) && expF != math.Trunc(expF) {
			mc.SetValue(values.NewComplex(cmplx.Pow(complex(baseF, 0), complex(expF, 0))))
			return nil
		}
		mc.SetValue(values.NewFloat(math.Pow(baseF, expF)))
	}
	return nil
}

// exactIntegerSqrt checks if n (non-negative) is a perfect square.
// For n <= 2^53 the float64 sqrt is exact; for larger values we verify
// that root*root == n. Returns (root, true) on success.
func exactIntegerSqrt(n int64) (int64, bool) {
	if n == 0 {
		return 0, true
	}
	root := int64(math.Sqrt(float64(n)))
	// Correct for float64 rounding near the boundary of large integers
	for root*root > n {
		root--
	}
	for (root+1)*(root+1) <= n {
		root++
	}
	if root*root == n {
		return root, true
	}
	return 0, false
}
