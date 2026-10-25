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
	"math/bits"
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
		if c.IsNaN() {
			return nil, nil, false
		}
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
	ro, io := bigFn(re, im, values.OperandPrecision(arg))
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
		special, ok := bigFloatSpecial(arg, fn)
		if ok {
			mc.SetValue(special)
			return nil
		}
		// An infinity reaches here only when its special result is complex (asin,
		// acos of ±inf); the kernels must not see it.
		bf := numberToBigFloat(arg)
		if bigRealFn != nil && isUnboundedReal(arg) && bf != nil && !bf.IsInf() {
			// A nil return declines (e.g. asin/acos of |x|>1 is complex); fall through.
			q := bigRealFn(bf, values.OperandPrecision(arg))
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
	special, ok := bigFloatSpecial(arg, cmplx.Exp)
	if ok {
		mc.SetValue(special)
		return nil
	}
	if isUnboundedReal(arg) {
		mc.SetValue(values.NewBigFloat(values.BigExp(numberToBigFloat(arg), values.OperandPrecision(arg))))
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
		special, ok := bigFloatSpecial(o, cmplx.Log)
		if ok {
			mc.SetValue(special)
			return nil
		}
		if isUnboundedReal(o) {
			ob := numberToBigFloat(o)
			if ob.Sign() > 0 {
				mc.SetValue(values.NewBigFloat(values.BigLog(ob, values.OperandPrecision(o))))
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
	// log base b: big path when either operand is unbounded and both are
	// positive reals, at the operands' precision.
	if isUnboundedReal(o) || isUnboundedReal(baseVal) {
		prec := values.OperandPrecision(o, baseVal)
		ob := numberToBigFloatAt(o, prec)
		bb := numberToBigFloatAt(baseVal, prec)
		if ob != nil && ob.Sign() > 0 && bb != nil && bb.Sign() > 0 {
			lo := bigLogOfPositive(ob, prec)
			lb := bigLogOfPositive(bb, prec)
			zeroOverZero := lo.Sign() == 0 && lb.Sign() == 0 // (log 1 1)
			if zeroOverZero || (lo.IsInf() && lb.IsInf()) {
				// big.Float.Quo panics on exactly these two; IEEE answers NaN.
				mc.SetValue(values.NewBigFloatNaN())
				return nil
			}
			mc.SetValue(values.NewBigFloat(new(big.Float).SetPrec(prec).Quo(lo, lb)))
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
	lz, lb := cmplx.Log(z), cmplx.Log(base)
	result := lz / lb
	if imag(lz) == 0 && imag(lb) == 0 {
		// Go's complex division answers Inf/x and x/0 with a NaN imaginary part.
		result = complex(real(lz)/real(lb), 0)
	}
	if isUnboundedReal(o) || isUnboundedReal(baseVal) {
		special, ok := realBigFloat(result)
		if ok {
			mc.SetValue(special)
			return nil
		}
	}
	mc.SetValue(helpers.ComplexOrFloat(result))
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
			bf := numberToBigFloat(o)
			// Not bigFloatSpecial: BigAtan answers ±Inf at full precision (±π/2),
			// where the float64 arm would truncate it. Only a NaN is left over.
			if bf == nil {
				mc.SetValue(values.NewBigFloatNaN())
				return nil
			}
			mc.SetValue(values.NewBigFloat(values.BigAtan(bf, values.OperandPrecision(o))))
			return nil
		}
		bc, isBigComplex := o.(*values.BigComplex)
		if isBigComplex && !bc.IsNaN() { // a NaN part reads as zero; cmplx.Atan answers NaN
			reBig := bc.RealAsBigFloat().BigFloatValue()
			imBig := bc.ImagAsBigFloat().BigFloatValue()
			// cmplx.Atan is branch-cut-correct (R7RS) in the float64 range, so only
			// divert to the big-precision complex atan when a component overflows
			// float64 — the case where the truncating path returns NaN.
			reF, _ := reBig.Float64()
			imF, _ := imBig.Float64()
			if math.IsInf(reF, 0) || math.IsInf(imF, 0) {
				reOut, imOut := values.BigComplexAtan(reBig, imBig, values.OperandPrecision(bc))
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
	// A NaN on either side (nil here) takes the float64 path, which answers NaN
	// or raises for a non-real operand.
	if isUnboundedReal(o) || isUnboundedReal(xVal) {
		prec := values.OperandPrecision(o, xVal)
		y := numberToBigFloatAt(o, prec)
		x := numberToBigFloatAt(xVal, prec)
		if y != nil && x != nil {
			mc.SetValue(values.NewBigFloat(values.BigAtan2(y, x, prec)))
			return nil
		}
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
	if isUnboundedReal(o) || isUnboundedReal(xVal) {
		mc.SetValue(values.NewBigFloatFromFloat64(math.Atan2(y, x)))
		return nil
	}
	mc.SetValue(values.NewFloat(math.Atan2(y, x)))
	return nil
}

// bigFloatSpecial answers fn at a NaN or infinite *BigFloat operand, which the
// big kernels cannot reduce (BigExp and the trig range reduction dereference a
// nil integer part; BigLog panics on Inf/Inf): fn's IEEE result, boxed as a
// BigFloat so the result keeps the operand's tier, as sqrt's does. ok is false
// for any other operand, and for a complex result (asin of +inf.0), which the
// caller's complex path answers.
func bigFloatSpecial(arg values.Value, fn func(complex128) complex128) (values.Value, bool) {
	bf, isBig := arg.(*values.BigFloat)
	if !isBig || bf.IsFinite() {
		return nil, false
	}
	return realBigFloat(fn(complex(bf.Float64Truncated(), 0)))
}

// realBigFloat boxes a real-valued complex128 result as a BigFloat. "Real" is
// ComplexOrFloat's rule: a zero imaginary part, or NaN in both parts.
func realBigFloat(r complex128) (values.Value, bool) {
	re, im := real(r), imag(r)
	bothNaN := math.IsNaN(re) && math.IsNaN(im)
	if im != 0 && !bothNaN {
		return nil, false
	}
	return values.NewBigFloatFromFloat64(re), true
}

// bigLogOfPositive is BigLog at prec extended to +Inf, which logAt cannot
// reduce (its mantissa split divides Inf by Inf): ln(+Inf) = +Inf.
func bigLogOfPositive(x *big.Float, prec uint) *big.Float {
	if x.IsInf() {
		return new(big.Float).SetPrec(prec).SetInf(false)
	}
	return values.BigLog(x, prec)
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
// or complex values and for a NaN (Float or BigFloat), which math/big cannot
// represent. An infinity (Float or BigFloat) converts to a big.Float infinity;
// callers whose kernel cannot reduce one test IsInf first.
func numberToBigFloat(v values.Value) *big.Float {
	return numberToBigFloatAt(v, values.DefaultBigFloatPrecision)
}

// numberToBigFloatAt is numberToBigFloat converting a non-BigFloat operand at
// prec bits, for a caller whose working precision exceeds the default.
func numberToBigFloatAt(v values.Value, prec uint) *big.Float {
	switch n := v.(type) {
	case *values.BigFloat:
		if n.IsNaN() {
			return nil // a NaN BigFloat's value is a zero under the nan flag
		}
		return new(big.Float).Set(n.BigFloatValue())
	case *values.BigInteger:
		return new(big.Float).SetPrec(prec).SetInt(n.BigInt())
	case *values.Integer:
		return new(big.Float).SetPrec(prec).SetInt64(n.Value)
	case *values.Float:
		if math.IsNaN(n.Value) {
			return nil // math/big has no NaN
		}
		if math.IsInf(n.Value, 0) {
			return new(big.Float).SetPrec(prec).SetInf(n.Value < 0)
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
			//
			// Belt and braces: exactIntegerSqrt now enforces its own n >= 0
			// precondition, so removing this guard would not reintroduce the
			// hang. It stays because the hazard belongs to the negation, which
			// is here, not to the callee.
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
		// SetInt at zero precision is exact. The root rounds at the default, or
		// at the operand's bit length when that is wider, so a wide operand
		// keeps its digits; both signs share the rule and so the digits.
		f := new(big.Float).SetInt(bi)
		mc.SetValue(values.BigSqrt(f, max(f.Prec(), values.DefaultBigFloatPrecision)))
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
		// The quotient carries guard bits so the root rounds once, at the default.
		f := new(big.Float).SetPrec(values.DefaultBigFloatPrecision + 64).SetRat(v.Rat())
		mc.SetValue(values.BigSqrt(f, values.DefaultBigFloatPrecision))
	case *values.Complex:
		mc.SetValue(values.NewComplex(complexSqrtR7RS(v.Value)))
	case *values.BigFloat:
		mc.SetValue(v.Sqrt())
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

	if isRealBigFloatExpt(baseNum, expNum) {
		mc.SetValue(bigFloatExpt(baseNum, expNum))
		return nil
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

// isRealBigFloatExpt reports whether expt takes the big-precision real path: a
// *BigFloat operand is present and both operands are real. The trigger is a
// BigFloat operand, not any unbounded-tier one, so (expt 2.0 (expt 10 20)) and
// (expt 2/3 0.5) keep their Float kinds.
func isRealBigFloatExpt(base, exp values.Number) bool {
	_, baseBig := base.(*values.BigFloat)
	_, expBig := exp.(*values.BigFloat)
	if !baseBig && !expBig {
		return false
	}
	return isRealKind(base) && isRealKind(exp)
}

// isRealKind reports whether n is one of the real numeric kinds. Unlike
// isRealNumber it refuses a Complex with a zero imaginary part, which
// numberToBigFloat cannot convert.
func isRealKind(n values.Number) bool {
	switch n.(type) {
	case *values.Integer, *values.BigInteger, *values.Rational, *values.Float, *values.BigFloat:
		return true
	default:
		return false
	}
}

// bigFloatExpt computes base^exp in big.Float for real operands at least one of
// which is a BigFloat, rounded to the widest BigFloat operand's precision.
//
// The IEEE special cases are answered first, in big.Float, because the kernels
// cannot take them: big.Float has no NaN, BigExp cannot reduce an infinity, and
// 0·Inf panics. Answering them through float64 instead would be wrong where the
// operand does not survive the rounding: (expt #m1.000…01 +inf.0) is +inf.0,
// but its base rounds to 1.0, where pow(1, +inf) is 1.
//
// A negative base with a non-integer exponent has a complex value and keeps the
// float64 complex path, as on the Float arm.
func bigFloatExpt(base, exp values.Number) values.Number {
	prec := values.OperandPrecision(base, exp)
	wp := prec + 64
	x := numberToBigFloatAt(base, wp)
	y := exponentToBigFloat(exp, wp)
	// pow(x, 0) and pow(1, y) are 1 even for a NaN partner. A Rational is never
	// 1, though one near it can round to 1 at wp.
	_, baseRational := base.(*values.Rational)
	if (y != nil && y.Sign() == 0) || (x != nil && !baseRational && x.Cmp(big.NewFloat(1)) == 0) {
		return values.NewBigFloat(new(big.Float).SetPrec(prec).SetInt64(1))
	}
	if x == nil || y == nil {
		return values.NewBigFloatNaN()
	}
	if x.Sign() < 0 && !y.IsInt() && !y.IsInf() {
		return values.NewComplex(cmplx.Pow(
			complex(values.NumberToFloat64(base), 0),
			complex(values.NumberToFloat64(exp), 0)))
	}
	q := bigPowAbs(base, new(big.Float).Abs(x), y, prec, wp)
	if x.Signbit() && isOddInteger(y) {
		q.Neg(q)
	}
	return values.NewBigFloat(q)
}

// exponentToBigFloat converts expt's exponent: an exact integer exactly, so
// the parity that decides a negative base's sign is the exponent's own (an odd
// BigInteger rounded to wp bits reads as even), anything else as
// numberToBigFloatAt does.
func exponentToBigFloat(v values.Number, wp uint) *big.Float {
	switch n := v.(type) {
	case *values.Integer:
		return new(big.Float).SetInt64(n.Value)
	case *values.BigInteger:
		return new(big.Float).SetInt(n.BigInt())
	default:
		return numberToBigFloatAt(v, wp)
	}
}

// bigPowAbs returns |base|^y rounded to prec bits, for y ≠ 0 and neither
// operand NaN. a is |base| converted at wp, the working precision of the log
// path; base itself is reconverted where wp is not enough.
func bigPowAbs(base values.Number, a, y *big.Float, prec, wp uint) *big.Float {
	switch {
	case y.IsInf():
		// |x| < 1 vanishes under +inf and diverges under -inf; |x| > 1 the
		// reverse. |x| = 1 is 1: a base of -1 reaches here as a = 1, and
		// pow(-1, ±inf) is 1, as on the Float arm.
		c := cmpAbsOne(base, a)
		if c == 0 {
			return new(big.Float).SetPrec(prec).SetInt64(1)
		}
		return zeroOrInf(prec, (c < 0) == y.Signbit())
	case a.Sign() == 0 || a.IsInf():
		return zeroOrInf(prec, a.IsInf() != y.Signbit())
	}
	// |x| = 1 is 1 under any finite y; the caller applies the sign. Answering
	// it here keeps a wide y off the log refinement below, which would grow
	// with y's exponent and find ln 1 = 0 anyway.
	if cmpAbsOne(base, a) == 0 {
		return new(big.Float).SetPrec(prec).SetInt64(1)
	}
	n, acc := y.Int64()
	if acc == big.Exact {
		return bigPowInt(a, n, prec)
	}
	// t = y·ln a, and its absolute error is |y| times ln a's. A t of 2^32 or
	// more over- or underflows big.Float, which wp answers as well as any
	// precision. Below that, a wide y means a near 1, where ln a computed at wp
	// carries an absolute error of about 2^-wp that y magnifies: ln a then
	// needs y's binary exponent in extra bits, from the base reconverted at
	// that precision (a Rational base is not exact at wp).
	t := new(big.Float).SetPrec(wp).Mul(y, values.BigLog(a, wp))
	r, isRational := base.(*values.Rational)
	if t.Sign() == 0 && isRational {
		// Only a Rational base rounds to 1 at wp; every other a ≠ 1 is exact
		// there. ln|r| ≈ |r|-1, exactly known, bounds t without a log: from
		// 2^32 on it over- or underflows, and below that y's exponent, and so
		// the refinement's precision, is bounded by the Rational's own size.
		d := new(big.Rat).Sub(new(big.Rat).Abs(r.Rat()), big.NewRat(1, 1))
		t0 := new(big.Float).SetPrec(64).Mul(y, new(big.Float).SetPrec(64).SetRat(d))
		if t0.MantExp(nil) > 32 {
			return zeroOrInf(prec, t0.Sign() > 0)
		}
	}
	yExp := y.MantExp(nil)
	if yExp > 0 && t.MantExp(nil) <= 32 {
		lp := wp + uint(yExp)
		ln := values.BigLog(new(big.Float).Abs(numberToBigFloatAt(base, lp)), lp)
		t = new(big.Float).SetPrec(lp).Mul(y, ln)
	}
	return new(big.Float).SetPrec(prec).Set(values.BigExp(t, wp))
}

// cmpAbsOne compares |base| with 1, exactly for a Rational base, whose a (its
// magnitude at the working precision) can round to 1; otherwise from a, which
// is exact.
func cmpAbsOne(base values.Number, a *big.Float) int {
	r, ok := base.(*values.Rational)
	if !ok {
		return a.Cmp(big.NewFloat(1))
	}
	return new(big.Rat).Abs(r.Rat()).Cmp(big.NewRat(1, 1))
}

// zeroOrInf returns +Inf when inf is true, else +0, at prec bits.
func zeroOrInf(prec uint, inf bool) *big.Float {
	q := new(big.Float).SetPrec(prec)
	if inf {
		q.SetInf(false)
	}
	return q
}

// bigPowInt returns a^n rounded to prec bits for a finite a > 0 and n ≠ 0, by
// repeated squaring rather than exp(n·ln a), so an exactly representable power
// such as 2.5^2 comes out exact. Each squaring doubles the relative error carried
// so far, which the bits.Len64(|n|) guard bits absorb. big.Float saturates an
// overflowing exponent to Inf and an underflowing one to 0, and a > 1 never
// meets a < 1, so no step multiplies 0 by Inf.
func bigPowInt(a *big.Float, n int64, prec uint) *big.Float {
	m := uint64(n)
	if n < 0 {
		m = -m // two's complement: |MinInt64| = 2^63 survives in uint64
	}
	wp := prec + uint(bits.Len64(m)) + 16
	q := new(big.Float).SetPrec(wp).SetInt64(1)
	sq := new(big.Float).SetPrec(wp).Set(a)
	for ; m > 0; m >>= 1 {
		if m&1 == 1 {
			q.Mul(q, sq)
		}
		if m > 1 {
			sq.Mul(sq, sq)
		}
	}
	if n < 0 {
		q.Quo(new(big.Float).SetPrec(wp).SetInt64(1), q)
	}
	return new(big.Float).SetPrec(prec).Set(q)
}

// isOddInteger reports whether y is an odd integer. An integer's lowest set bit
// is its units bit exactly when its binary exponent equals its significant-bit
// count, which avoids materializing a huge y as a big.Int.
func isOddInteger(y *big.Float) bool {
	if !y.IsInt() || y.Sign() == 0 {
		return false
	}
	return y.MantExp(nil) == int(y.MinPrec())
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
