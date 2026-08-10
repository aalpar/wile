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

// bigComplexFn is a big-precision complex kernel returning (real, imag) parts.
type bigComplexFn func(re, im *big.Float, prec uint) (*big.Float, *big.Float)

// cmplxNonFinite reports whether a complex128 has a non-finite component.
func cmplxNonFinite(c complex128) bool {
	return math.IsInf(real(c), 0) || math.IsInf(imag(c), 0) ||
		math.IsNaN(real(c)) || math.IsNaN(imag(c))
}

// complexComponents extracts a numeric operand's real/imaginary parts as
// *big.Float. Real numbers yield (value, 0). Returns ok=false for non-numeric or
// non-finite operands (so genuine infinities are not "rescued").
func complexComponents(v values.Value) (re, im *big.Float, ok bool) {
	switch c := v.(type) {
	case *values.BigComplex:
		re = new(big.Float).Set(c.RealAsBigFloat().BigFloatValue())
		im = new(big.Float).Set(c.ImagAsBigFloat().BigFloatValue())
	case *values.Complex:
		cr, ci := real(c.Value), imag(c.Value)
		if math.IsNaN(cr) || math.IsNaN(ci) || math.IsInf(cr, 0) || math.IsInf(ci, 0) {
			return nil, nil, false
		}
		re = big.NewFloat(cr)
		im = big.NewFloat(ci)
	default:
		r := numberToBigFloat(v)
		if r == nil {
			return nil, nil, false
		}
		re = r
		im = new(big.Float)
	}
	if re.IsInf() || im.IsInf() {
		return nil, nil, false
	}
	return re, im, true
}

// rescueBigComplex recomputes bigFn at big precision when the float64 result is
// non-finite on a finite operand (overflow-gating). It sets the machine value and
// returns true on rescue; otherwise returns false. The result simplifies to a
// *BigFloat when the imaginary part is zero.
func rescueBigComplex(mc machine.CallContext, arg values.Value, result complex128, bigFn bigComplexFn) bool {
	if bigFn == nil || !cmplxNonFinite(result) {
		return false
	}
	re, im, ok := complexComponents(arg)
	if !ok {
		return false
	}
	ro, io := bigFn(re, im, values.DefaultBigFloatPrecision)
	if ro.IsInf() || io.IsInf() {
		// Genuinely infinite (e.g. log(0) = −∞), not a float64 overflow of a finite
		// value — keep the float64 result rather than reboxing an infinity.
		return false
	}
	if io.Sign() == 0 {
		mc.SetValue(values.NewBigFloat(ro))
	} else {
		mc.SetValue(values.NewBigComplexFromBigFloats(values.NewBigFloat(ro), values.NewBigFloat(io)))
	}
	return true
}

// makeComplexPrimitive builds a unary transcendental. An unbounded-tier real
// operand (BigFloat/BigInteger/Rational) takes bigRealFn at big precision when one
// is supplied; any operand whose float64 result comes back non-finite (±Inf/NaN)
// takes bigCplxFn, underflow-to-zero is not rescued here; everything else truncates
// to complex128 and applies fn.
func makeComplexPrimitive(name string, fn func(complex128) complex128, bigRealFn func(*big.Float, uint) *big.Float, bigCplxFn bigComplexFn) machine.ForeignFunction {
	return func(mc machine.CallContext) error {
		arg := mc.Arg(0)
		if bigRealFn != nil && isUnboundedReal(arg) {
			// A nil return declines (e.g. asin/acos of |x|>1 is complex); fall through.
			q := bigRealFn(numberToBigFloat(arg), values.DefaultBigFloatPrecision)
			if q != nil {
				mc.SetValue(values.NewBigFloat(q))
				return nil
			}
		}
		z, err := helpers.ToComplex128(arg)
		if err != nil {
			return werr.WrapForeignErrorf(err, "%s: %v", name, err)
		}
		result := fn(z)
		if rescueBigComplex(mc, arg, result, bigCplxFn) {
			return nil
		}
		mc.SetValue(helpers.ComplexOrFloat(result))
		return nil
	}
}

// Unary transcendental primitives (R7RS §6.2.6). Big-real kernels are wired in as
// they land. exp is custom (below) for the bounded-real overflow rescue.
var (
	PrimSin  = makeComplexPrimitive("sin", cmplx.Sin, values.BigSin, values.BigComplexSin)
	PrimCos  = makeComplexPrimitive("cos", cmplx.Cos, values.BigCos, values.BigComplexCos)
	PrimTan  = makeComplexPrimitive("tan", cmplx.Tan, values.BigTan, values.BigComplexTan)
	PrimAsin = makeComplexPrimitive("asin", cmplx.Asin, values.BigAsin, values.BigComplexAsin)
	PrimAcos = makeComplexPrimitive("acos", cmplx.Acos, values.BigAcos, values.BigComplexAcos)
)

// PrimExp implements (exp z). Unlike the other unary transcendentals, exp
// overflows float64 at ~709 — inside the bounded range — so besides the
// big-precision path for unbounded-tier operands it also rescues a bounded real
// operand whose exp over/underflows float64 to a finite BigFloat (e^1000 ≈
// 1.97e434 is representable as a bignum even though math.Exp returns +Inf).
func PrimExp(mc machine.CallContext) error {
	arg := mc.Arg(0)
	if isUnboundedReal(arg) {
		mc.SetValue(values.NewBigFloat(values.BigExp(numberToBigFloat(arg), values.DefaultBigFloatPrecision)))
		return nil
	}
	z, err := helpers.ToComplex128(arg)
	if err != nil {
		return werr.WrapForeignErrorf(err, "exp: %v", err)
	}
	r := real(z)
	if imag(z) == 0 && !math.IsInf(r, 0) {
		ez := math.Exp(r)
		if math.IsInf(ez, 1) || (ez == 0 && r != 0) {
			mc.SetValue(values.NewBigFloat(values.BigExp(big.NewFloat(r), values.DefaultBigFloatPrecision)))
			return nil
		}
	}
	result := cmplx.Exp(z)
	if rescueBigComplex(mc, arg, result, values.BigComplexExp) {
		return nil
	}
	mc.SetValue(helpers.ComplexOrFloat(result))
	return nil
}

// PrimLog implements the (log z) and (log z1 z2) primitives. A positive,
// unbounded-tier real argument takes the big-precision log (so a value beyond the
// float64 range no longer overflows its input to +Inf); non-positive or complex
// arguments keep the branch-correct complex path.
func PrimLog(mc machine.CallContext) error {
	o := mc.Arg(0)
	rest := mc.Arg(1)

	if values.IsEmptyList(rest) {
		if isUnboundedReal(o) {
			ob := numberToBigFloat(o)
			if ob.Sign() > 0 {
				mc.SetValue(values.NewBigFloat(values.BigLog(ob, values.DefaultBigFloatPrecision)))
				return nil
			}
		}
		z, err := helpers.ToComplex128(o)
		if err != nil {
			return werr.WrapForeignErrorf(err, "log: %v", err)
		}
		result := cmplx.Log(z)
		if rescueBigComplex(mc, o, result, values.BigComplexLog) {
			return nil
		}
		mc.SetValue(helpers.ComplexOrFloat(result))
		return nil
	}

	baseVal, restAfter, err := helpers.Uncons(rest, "log", "base argument")
	if err != nil {
		return err
	}
	if !values.IsEmptyList(restAfter) {
		return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments, "log: expected 1 or 2 arguments")
	}
	// log base b: big path when the argument is unbounded-positive-real and the
	// base is positive-real (a bounded base is converted exactly).
	if isUnboundedReal(o) {
		ob := numberToBigFloat(o)
		bb := numberToBigFloat(baseVal)
		if ob.Sign() > 0 && bb != nil && bb.Sign() > 0 {
			q := new(big.Float).SetPrec(values.DefaultBigFloatPrecision).Quo(
				values.BigLog(ob, values.DefaultBigFloatPrecision),
				values.BigLog(bb, values.DefaultBigFloatPrecision))
			mc.SetValue(values.NewBigFloat(q))
			return nil
		}
	}
	base, err := helpers.ToComplex128(baseVal)
	if err != nil {
		return werr.WrapForeignErrorf(err, "log: %v", err)
	}
	z, err := helpers.ToComplex128(o)
	if err != nil {
		return werr.WrapForeignErrorf(err, "log: %v", err)
	}
	mc.SetValue(helpers.ComplexOrFloat(cmplx.Log(z) / cmplx.Log(base)))
	return nil
}

// PrimAtan implements the (atan z) and (atan y x) primitives.
func PrimAtan(mc machine.CallContext) error {
	o := mc.Arg(0)
	rest := mc.Arg(1)

	if values.IsEmptyList(rest) {
		// Big-tier real operands keep big precision via values.BigAtan; an
		// out-of-float64-range BigComplex uses values.BigComplexAtan (see below).
		if isUnboundedReal(o) {
			mc.SetValue(values.NewBigFloat(values.BigAtan(numberToBigFloat(o), values.DefaultBigFloatPrecision)))
			return nil
		}
		bc, isBigComplex := o.(*values.BigComplex)
		if isBigComplex {
			reBig := bc.RealAsBigFloat().BigFloatValue()
			imBig := bc.ImagAsBigFloat().BigFloatValue()
			// cmplx.Atan is branch-cut-correct (R7RS) in the float64 range, so only
			// divert to the big-precision complex atan when a component overflows
			// float64 — the case where the truncating path returns NaN.
			reF, _ := reBig.Float64()
			imF, _ := imBig.Float64()
			if math.IsInf(reF, 0) || math.IsInf(imF, 0) {
				reOut, imOut := values.BigComplexAtan(reBig, imBig, values.DefaultBigFloatPrecision)
				if imOut.Sign() == 0 {
					mc.SetValue(values.NewBigFloat(reOut))
				} else {
					mc.SetValue(values.NewBigComplexFromBigFloats(values.NewBigFloat(reOut), values.NewBigFloat(imOut)))
				}
				return nil
			}
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

	// An unbounded-range/precision operand on either side takes the big-precision
	// atan2 path, so large-magnitude operands with a finite ratio keep their true
	// angle instead of both saturating to ±Inf under float64 truncation.
	if isUnboundedReal(o) || isUnboundedReal(xVal) {
		y := numberToBigFloat(o)
		x := numberToBigFloat(xVal)
		if y == nil || x == nil {
			return werr.WrapForeignErrorf(werr.ErrNotANumber, "atan: expected real arguments")
		}
		mc.SetValue(values.NewBigFloat(values.BigAtan2(y, x, values.DefaultBigFloatPrecision)))
		return nil
	}

	// Bounded operands (Integer, Float) cannot overflow float64, so atan2 runs on
	// the fast path. atan2 is inherently inexact, so ToFloat64Lossy (not strict
	// ToFloat64) is correct here per memory/2026-05-14-numeric-loss-signals-design.md §R8.
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

// isUnboundedReal reports whether v is a real number whose range or precision
// exceeds float64 — *BigFloat (unbounded precision), *BigInteger and *Rational
// (unbounded range). These take the big-precision kernel in whichever
// transcendental is calling, so a value outside the float64 range is not
// truncated before the computation. Bounded *Integer (int64) and *Float (float64) stay
// on the fast float64 path.
func isUnboundedReal(v values.Value) bool {
	switch v.(type) {
	case *values.BigFloat, *values.BigInteger, *values.Rational:
		return true
	default:
		return false
	}
}

// numberToBigFloat converts a real numeric value to *big.Float: at default
// precision, except a *BigFloat which keeps its own. Returns nil for non-numeric
// or complex values, and for a NaN/±Inf float, which math/big cannot represent.
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
		if math.IsNaN(n.Value) || math.IsInf(n.Value, 0) {
			return nil // math/big has no NaN; ±Inf is not a value the big paths handle
		}
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
			// Check for negative perfect square: result is exact imaginary.
			//
			// MinInt64 negates to ITSELF, so `-v.Value` used to hand
			// exactIntegerSqrt a negative magnitude that its correction loop
			// could never satisfy: (sqrt -9223372036854775808) never returned.
			// 2^63 is not a perfect square, so excluding it forfeits no exact
			// answer — the inexact branch below is the right one for it. Same
			// hazard, same reason, as exptExact's big.Int.Abs guard below.
			root, ok := int64(0), false
			if v.Value != math.MinInt64 {
				root, ok = exactIntegerSqrt(-v.Value)
			}
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

// exactIntegerSqrt checks if n is a perfect square, returning (root, true) on
// success and (0, false) otherwise, including for every negative n.
//
// It computes in big.Int rather than correcting a float64 seed in int64. The
// correction loops this replaced were unconditionally infinite at the top of
// the range: with n = MaxInt64 the guard `(root+1)*(root+1) <= n` compares a
// WRAPPED product, and no int64 exceeds MaxInt64, so the condition held for
// every value of root and (sqrt 9223372036854775807) never returned. The
// negative direction hung for a different reason — see PrimSqrt's MinInt64
// guard — and the doc comment's "non-negative" precondition was declared but
// never enforced, which is what let that reach here at all.
//
// Same computation as the *values.BigInteger arm of PrimSqrt just above, which
// was always correct at these magnitudes; this makes the two agree by running
// the same code rather than by keeping two implementations in step.
func exactIntegerSqrt(n int64) (int64, bool) {
	if n < 0 {
		return 0, false
	}
	if n == 0 {
		return 0, true
	}
	bn := big.NewInt(n)
	root := new(big.Int).Sqrt(bn)
	if new(big.Int).Mul(root, root).Cmp(bn) != 0 {
		return 0, false
	}
	return root.Int64(), true
}
