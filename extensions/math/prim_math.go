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
	"strconv"
	"strings"

	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// ensureInexactDecimal ensures a float string has a decimal point, even in
// scientific notation. "5e-324" becomes "5.0e-324", "1" becomes "1.0".
func ensureInexactDecimal(s string) string {
	eIdx := strings.IndexAny(s, "eE")
	if eIdx >= 0 {
		mantissa := s[:eIdx]
		if !strings.ContainsRune(mantissa, '.') {
			return mantissa + ".0" + s[eIdx:]
		}
		return s
	}
	if !strings.ContainsRune(s, '.') {
		return s + ".0"
	}
	return s
}

// makeComplexPrimitive returns a ForeignFunction that converts its argument
// to complex128, applies fn, and returns the result (as Float when imaginary
// part is zero, otherwise Complex). Used for the six unary transcendental
// functions that share identical structure.
func makeComplexPrimitive(name string, fn func(complex128) complex128) machine.ForeignFunction {
	return func(mc *machine.MachineContext) error {
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
func PrimLog(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	rest := mc.Arg(1)
	z, err := helpers.ToComplex128(o)
	if err != nil {
		return werr.WrapForeignErrorf(err, "log: %v", err)
	}
	if values.IsEmptyList(rest) {
		mc.SetValue(helpers.ComplexOrFloat(cmplx.Log(z)))
	} else {
		baseArg, ok := rest.(values.Tuple)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAList, "log: expected a list for rest arguments")
		}
		base, err := helpers.ToComplex128(baseArg.Car())
		if err != nil {
			return werr.WrapForeignErrorf(err, "log: %v", err)
		}
		mc.SetValue(helpers.ComplexOrFloat(cmplx.Log(z) / cmplx.Log(base)))
	}
	return nil
}

// PrimAtan implements the (atan z) and (atan y x) primitives.
func PrimAtan(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	rest := mc.Arg(1)

	if values.IsEmptyList(rest) {
		z, err := helpers.ToComplex128(o)
		if err != nil {
			return werr.WrapForeignErrorf(err, "atan: %v", err)
		}
		mc.SetValue(helpers.ComplexOrFloat(cmplx.Atan(z)))
	} else {
		y, err := helpers.ToFloat64(o)
		if err != nil {
			return werr.WrapForeignErrorf(err, "atan: %v", err)
		}
		xArg, ok := rest.(values.Tuple)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAList, "atan: expected a list for rest arguments")
		}
		x, err := helpers.ToFloat64(xArg.Car())
		if err != nil {
			return werr.WrapForeignErrorf(err, "atan: %v", err)
		}
		mc.SetValue(values.NewFloat(math.Atan2(y, x)))
	}
	return nil
}

// PrimSqrt implements the (sqrt) primitive.
//
// R7RS §6.2.6: The branch cut for sqrt lies along the negative real axis,
// continuous with quadrant II. This means for values on the negative real axis
// (including those with -0.0 imaginary part), sqrt returns positive imaginary.
func PrimSqrt(mc *machine.MachineContext) error {
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
		f := v.Float64()
		if f < 0 {
			mc.SetValue(values.NewComplex(cmplx.Sqrt(complex(f, 0))))
		} else {
			mc.SetValue(values.NewFloat(math.Sqrt(f)))
		}
	case *values.Complex:
		mc.SetValue(values.NewComplex(complexSqrtR7RS(v.Value)))
	case *values.BigComplex:
		// Convert BigComplex to complex128 and compute sqrt
		realF := v.RealAsBigFloat().Float64()
		imagF := v.ImagAsBigFloat().Float64()
		mc.SetValue(values.NewComplex(complexSqrtR7RS(complex(realF, imagF))))
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
func PrimExpt(mc *machine.MachineContext) error {
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
			values.NumberToComplex128(baseNum),
			values.NumberToComplex128(expNum))))
	default:
		// Complex exponent with real base
		switch expNum.(type) {
		case *values.Complex, *values.BigComplex:
			mc.SetValue(values.NewComplex(cmplx.Pow(
				complex(values.NumberToFloat64(baseNum), 0),
				values.NumberToComplex128(expNum))))
			return nil
		}
		mc.SetValue(values.NewFloat(math.Pow(
			values.NumberToFloat64(baseNum),
			values.NumberToFloat64(expNum))))
	}
	return nil
}

// PrimSquare implements the (square) primitive.
func PrimSquare(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	n, ok := o.(values.Number)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "square: expected a number but got %T", o)
	}
	mc.SetValue(n.Multiply(n))
	return nil
}

// realNumberOp and makeRealNumberPrimitive for floor/ceiling/truncate/round
type realNumberOp struct {
	name         string
	integerOp    func(*values.Integer) values.Value
	bigIntegerOp func(*values.BigInteger) values.Value
	floatOp      func(float64) float64
	rationalOp   func(*values.Rational) values.Value
}

func makeRealNumberPrimitive(op realNumberOp) func(*machine.MachineContext) error {
	return func(mc *machine.MachineContext) error {
		o := mc.Arg(0)
		switch v := o.(type) {
		case *values.Integer:
			mc.SetValue(op.integerOp(v))
		case *values.BigInteger:
			if op.bigIntegerOp != nil {
				mc.SetValue(op.bigIntegerOp(v))
			} else {
				mc.SetValue(values.NewFloat(op.floatOp(v.ToInexact().(*values.Float).Datum())))
			}
		case *values.Float:
			mc.SetValue(values.NewFloat(op.floatOp(v.Value)))
		case *values.BigFloat:
			mc.SetValue(values.NewBigFloatFromFloat64(op.floatOp(v.Float64())))
		case *values.Rational:
			mc.SetValue(op.rationalOp(v))
		default:
			return werr.WrapForeignErrorf(werr.ErrNotANumber, "%s: expected a real number but got %T", op.name, o)
		}
		return nil
	}
}

func integerPassthrough(v *values.Integer) values.Value {
	return v
}

// rationalToInteger returns an exact integer for exact rational inputs.
// R7RS §6.2.6: floor, ceiling, truncate, round return integers.
// When the input is exact (like a rational), the result must also be exact.
func rationalToInteger(f func(float64) float64) func(*values.Rational) values.Value {
	return func(v *values.Rational) values.Value {
		return values.NewInteger(int64(f(v.Float64())))
	}
}

var PrimCeiling = makeRealNumberPrimitive(realNumberOp{
	name:       "ceiling",
	integerOp:  integerPassthrough,
	floatOp:    math.Ceil,
	rationalOp: rationalToInteger(math.Ceil),
})

var PrimFloor = makeRealNumberPrimitive(realNumberOp{
	name:       "floor",
	integerOp:  integerPassthrough,
	floatOp:    math.Floor,
	rationalOp: rationalToInteger(math.Floor),
})

var PrimTruncate = makeRealNumberPrimitive(realNumberOp{
	name:       "truncate",
	integerOp:  integerPassthrough,
	floatOp:    math.Trunc,
	rationalOp: rationalToInteger(math.Trunc),
})

var PrimRound = makeRealNumberPrimitive(realNumberOp{
	name:       "round",
	integerOp:  integerPassthrough,
	floatOp:    math.RoundToEven,
	rationalOp: rationalToInteger(math.RoundToEven),
})

// divResult selects which values a real division primitive returns.
type divResult int

const (
	divBoth      divResult = iota // return quotient and remainder as multiple values
	divQuotient                   // return quotient only
	divRemainder                  // return remainder only
)

// realDivision implements the shared logic for floor and truncate division
// families (R7RS §6.2.6). The roundFn parameter selects the rounding mode
// (math.Floor or math.Trunc), and result selects which values to return.
func realDivision(mc *machine.MachineContext, name string, roundFn func(float64) float64, result divResult) error {
	n0, exact0, err := helpers.ExtractReal(mc.Arg(0), name)
	if err != nil {
		return err
	}
	n1, exact1, err := helpers.ExtractReal(mc.Arg(1), name)
	if err != nil {
		return err
	}

	if n1 == 0 {
		return werr.WrapForeignErrorf(werr.ErrDivisionByZero, "%s: division by zero", name)
	}

	q := roundFn(n0 / n1)
	exact := exact0 && exact1

	switch result {
	case divBoth:
		r := n0 - q*n1
		if exact {
			mc.SetValues(values.NewInteger(int64(q)), values.NewInteger(int64(r)))
		} else {
			mc.SetValues(values.NewFloat(q), values.NewFloat(r))
		}
	case divQuotient:
		if exact {
			mc.SetValue(values.NewInteger(int64(q)))
		} else {
			mc.SetValue(values.NewFloat(q))
		}
	case divRemainder:
		r := n0 - q*n1
		if exact {
			mc.SetValue(values.NewInteger(int64(r)))
		} else {
			mc.SetValue(values.NewFloat(r))
		}
	}
	return nil
}

// PrimFloorDiv implements the (floor/) primitive.
//
// R7RS §6.2.6: Returns two values: floor quotient and floor remainder.
func PrimFloorDiv(mc *machine.MachineContext) error {
	return realDivision(mc, "floor/", math.Floor, divBoth)
}

// PrimFloorQuotient implements the (floor-quotient) primitive.
//
// R7RS §6.2.6: Returns the floor quotient for any real numbers.
func PrimFloorQuotient(mc *machine.MachineContext) error {
	return realDivision(mc, "floor-quotient", math.Floor, divQuotient)
}

// PrimFloorRemainder implements the (floor-remainder) primitive.
//
// R7RS §6.2.6: Returns the floor remainder for any real numbers.
func PrimFloorRemainder(mc *machine.MachineContext) error {
	return realDivision(mc, "floor-remainder", math.Floor, divRemainder)
}

// PrimTruncateDiv implements the truncate/ primitive.
//
// R7RS §6.2.6: Returns two values: truncate quotient and truncate remainder.
func PrimTruncateDiv(mc *machine.MachineContext) error {
	return realDivision(mc, "truncate/", math.Trunc, divBoth)
}

// PrimTruncateQuotient implements the truncate-quotient primitive.
//
// R7RS §6.2.6: Returns the truncate quotient for any real numbers.
func PrimTruncateQuotient(mc *machine.MachineContext) error {
	return realDivision(mc, "truncate-quotient", math.Trunc, divQuotient)
}

// PrimTruncateRemainder implements the truncate-remainder primitive.
//
// R7RS §6.2.6: Returns the truncate remainder for any real numbers.
func PrimTruncateRemainder(mc *machine.MachineContext) error {
	return realDivision(mc, "truncate-remainder", math.Trunc, divRemainder)
}

// PrimFiniteQ implements the (finite?) primitive.
func PrimFiniteQ(mc *machine.MachineContext) error {
	n, ok := mc.Arg(0).(values.Number)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "finite?: expected a number but got %T", mc.Arg(0))
	}
	mc.SetValue(values.BoolToBoolean(n.IsFinite()))
	return nil
}

// PrimInfiniteQ implements the (infinite?) primitive.
func PrimInfiniteQ(mc *machine.MachineContext) error {
	n, ok := mc.Arg(0).(values.Number)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "infinite?: expected a number but got %T", mc.Arg(0))
	}
	mc.SetValue(values.BoolToBoolean(!n.IsFinite() && !n.IsNaN()))
	return nil
}

// PrimNanQ implements the nan? primitive.
func PrimNanQ(mc *machine.MachineContext) error {
	n, ok := mc.Arg(0).(values.Number)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "nan?: expected a number but got %T", mc.Arg(0))
	}
	mc.SetValue(values.BoolToBoolean(n.IsNaN()))
	return nil
}

// PrimNumerator implements the numerator primitive.
func PrimNumerator(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(v)
	case *values.Rational:
		num := v.Num()
		if num.IsInt64() {
			mc.SetValue(values.NewInteger(num.Int64()))
		} else {
			mc.SetValue(values.NewRationalFromBigInt(num, big.NewInt(1)))
		}
	case *values.Float:
		// R7RS §6.2.6: inexact input → inexact output
		r := new(big.Rat).SetFloat64(v.Value)
		if r == nil {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "numerator: cannot get numerator of infinity or NaN")
		}
		num := r.Num()
		f, _ := new(big.Float).SetInt(num).Float64()
		mc.SetValue(values.NewFloat(f))
	case *values.BigFloat:
		// L18: Handle BigFloat from Rational.ToInexact()
		// R7RS §6.2.6: inexact input → inexact output
		r, _ := v.BigFloatValue().Rat(nil)
		if r == nil {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "numerator: cannot get numerator of infinity or NaN")
		}
		num := r.Num()
		f := new(big.Float).SetInt(num)
		mc.SetValue(values.NewBigFloat(f))
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "numerator: expected a rational number but got %T", o)
	}
	return nil
}

// PrimDenominator implements the (denominator) primitive.
func PrimDenominator(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(values.NewInteger(1))
	case *values.Rational:
		denom := v.Denom()
		if denom.IsInt64() {
			mc.SetValue(values.NewInteger(denom.Int64()))
		} else {
			mc.SetValue(values.NewRationalFromBigInt(denom, big.NewInt(1)))
		}
	case *values.Float:
		// R7RS §6.2.6: inexact input → inexact output
		r := new(big.Rat).SetFloat64(v.Value)
		if r == nil {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "denominator: cannot get denominator of infinity or NaN")
		}
		denom := r.Denom()
		f, _ := new(big.Float).SetInt(denom).Float64()
		mc.SetValue(values.NewFloat(f))
	case *values.BigFloat:
		// L18: Handle BigFloat from Rational.ToInexact()
		// R7RS §6.2.6: inexact input → inexact output
		r, _ := v.BigFloatValue().Rat(nil)
		if r == nil {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "denominator: cannot get denominator of infinity or NaN")
		}
		denom := r.Denom()
		f := new(big.Float).SetInt(denom)
		mc.SetValue(values.NewBigFloat(f))
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "denominator: expected a rational number but got %T", o)
	}
	return nil
}

// PrimRationalize implements the (rationalize) primitive.
func PrimRationalize(mc *machine.MachineContext) error {
	xArg := mc.Arg(0)
	yArg := mc.Arg(1)

	var x, y *big.Rat
	var xExact, yExact bool

	switch v := xArg.(type) {
	case *values.Integer:
		x = big.NewRat(v.Value, 1)
		xExact = true
	case *values.Rational:
		x = v.Rat()
		xExact = true
	case *values.Float:
		x = new(big.Rat).SetFloat64(v.Value)
		if x == nil {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "rationalize: x cannot be infinity or NaN")
		}
		xExact = false
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "rationalize: expected a real number for x but got %T", xArg)
	}

	switch v := yArg.(type) {
	case *values.Integer:
		y = big.NewRat(v.Value, 1)
		yExact = true
	case *values.Rational:
		y = v.Rat()
		yExact = true
	case *values.Float:
		y = new(big.Rat).SetFloat64(v.Value)
		if y == nil {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "rationalize: y cannot be infinity or NaN")
		}
		yExact = false
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "rationalize: expected a real number for y but got %T", yArg)
	}

	if y.Sign() < 0 {
		y = new(big.Rat).Abs(y)
	}

	result := rationalizeSternBrocot(x, y)

	if !xExact || !yExact {
		f, _ := result.Float64()
		mc.SetValue(values.NewFloat(f))
		return nil
	}
	if !result.IsInt() {
		mc.SetValue(values.NewRationalFromRat(result))
		return nil
	}
	num := result.Num()
	if num.IsInt64() {
		mc.SetValue(values.NewInteger(num.Int64()))
		return nil
	}
	mc.SetValue(values.NewRationalFromRat(result))
	return nil
}

func rationalizeSternBrocot(x, y *big.Rat) *big.Rat {
	if y.Sign() == 0 {
		return new(big.Rat).Set(x)
	}

	lo := new(big.Rat).Sub(x, y)
	hi := new(big.Rat).Add(x, y)

	if lo.Sign() > 0 {
		return rationalizePositive(lo, hi)
	}
	if hi.Sign() < 0 {
		negHi := new(big.Rat).Neg(hi)
		negLo := new(big.Rat).Neg(lo)
		result := rationalizePositive(negHi, negLo)
		return new(big.Rat).Neg(result)
	}
	return big.NewRat(0, 1)
}

func rationalizePositive(lo, hi *big.Rat) *big.Rat {
	aNum, aDenom := big.NewInt(0), big.NewInt(1)
	bNum, bDenom := big.NewInt(1), big.NewInt(0)

	one := big.NewRat(1, 1)
	for {
		if lo.Cmp(one) > 0 {
			k := floorRat(lo)
			lo.Sub(lo, k)
			hi.Sub(hi, k)
			kInt := k.Num()
			aNum.Add(aNum, new(big.Int).Mul(kInt, bNum))
			aDenom.Add(aDenom, new(big.Int).Mul(kInt, bDenom))
		}

		if hi.Cmp(one) >= 0 {
			result := new(big.Rat)
			num := new(big.Int).Add(aNum, bNum)
			denom := new(big.Int).Add(aDenom, bDenom)
			result.SetFrac(num, denom)
			return result
		}

		lo.Inv(lo)
		hi.Inv(hi)
		lo, hi = hi, lo
		aNum, bNum = bNum, aNum
		aDenom, bDenom = bDenom, aDenom
	}
}

func floorRat(r *big.Rat) *big.Rat {
	q := new(big.Int).Div(r.Num(), r.Denom())
	return new(big.Rat).SetInt(q)
}

// PrimExactIntegerSqrt implements the (exact-integer-sqrt) primitive.
//
// R7RS §6.2.6: Returns two non-negative exact integers s and r where
// n = s² + r and n < (s+1)².
func PrimExactIntegerSqrt(mc *machine.MachineContext) error {
	o := mc.Arg(0)

	switch v := o.(type) {
	case *values.Integer:
		if v.Value < 0 {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "exact-integer-sqrt: expected a non-negative integer")
		}
		s := int64(math.Sqrt(float64(v.Value)))
		for s*s > v.Value {
			s--
		}
		for (s+1)*(s+1) <= v.Value {
			s++
		}
		r := v.Value - s*s
		mc.SetValues(values.NewInteger(s), values.NewInteger(r))
		return nil

	case *values.BigInteger:
		if v.BigInt().Sign() < 0 {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "exact-integer-sqrt: expected a non-negative integer")
		}
		// Use big.Int.Sqrt which computes floor(sqrt(n))
		s := new(big.Int).Sqrt(v.BigInt())
		// Compute remainder: r = n - s²
		sSquared := new(big.Int).Mul(s, s)
		r := new(big.Int).Sub(v.BigInt(), sSquared)
		mc.SetValues(values.NewBigInteger(s), values.NewBigInteger(r))
		return nil

	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "exact-integer-sqrt: expected an exact integer but got %T", o)
	}
}

// PrimMakeRectangular implements make-rectangular.
// R7RS §6.2.6: If both arguments are exact, the result is exact.
func PrimMakeRectangular(mc *machine.MachineContext) error {
	r := mc.Arg(0)
	i := mc.Arg(1)

	// Check if both arguments are real numbers (not complex)
	rNum, rOk := r.(values.Number)
	iNum, iOk := i.(values.Number)
	if !rOk {
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "make-rectangular: expected a real number but got %T", r)
	}
	if !iOk {
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "make-rectangular: expected a real number but got %T", i)
	}

	// Reject complex numbers - make-rectangular requires real number arguments
	if !isRealNumber(r) {
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "make-rectangular: expected a real number but got complex %T", r)
	}
	if !isRealNumber(i) {
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "make-rectangular: expected a real number but got complex %T", i)
	}

	bothExact := values.ExactnessOf(rNum) == values.Exact && values.ExactnessOf(iNum) == values.Exact

	if bothExact {
		// Create exact BigComplex
		realPart := toExactBigComplexPart(rNum)
		imagPart := toExactBigComplexPart(iNum)
		if imagPart.IsZero() {
			mc.SetValue(realPart)
			return nil
		}
		mc.SetValue(values.NewBigComplex(realPart, imagPart))
		return nil
	}

	// At least one argument is inexact - check if we need BigFloat precision
	_, rIsBigFloat := r.(*values.BigFloat)
	_, iIsBigFloat := i.(*values.BigFloat)

	if rIsBigFloat || iIsBigFloat {
		realPart, err := toBigComplexPart(r, "make-rectangular")
		if err != nil {
			return err
		}
		imagPart, err := toBigComplexPart(i, "make-rectangular")
		if err != nil {
			return err
		}
		if imagPart.IsZero() {
			mc.SetValue(realPart)
			return nil
		}
		mc.SetValue(values.NewBigComplex(realPart, imagPart))
		return nil
	}

	// Use regular Complex for inexact numbers
	mc.SetValue(values.NewComplexFromParts(
		values.NumberToFloat64(rNum),
		values.NumberToFloat64(iNum)))
	return nil
}

// toExactBigComplexPart converts an exact number to a BigInteger or Rational
// suitable for use as a BigComplex part.
func toExactBigComplexPart(n values.Number) values.Number {
	switch v := n.(type) {
	case *values.Integer:
		return values.NewBigIntegerFromInt64(v.Value)
	case *values.BigInteger:
		return v
	case *values.Rational:
		return v
	default:
		panic("toExactBigComplexPart: expected exact number")
	}
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

// isRealNumber returns true if the value is a real number (not complex).
// Real numbers include Integer, BigInteger, Float, BigFloat, and Rational.
// Complex and BigComplex are only considered real if their imaginary part is zero.
func isRealNumber(v values.Value) bool {
	switch n := v.(type) {
	case *values.Integer, *values.BigInteger, *values.Float, *values.BigFloat, *values.Rational:
		return true
	case *values.Complex:
		return n.IsReal()
	case *values.BigComplex:
		return n.IsReal()
	default:
		return false
	}
}

func toBigComplexPart(v values.Value, name string) (values.Number, error) {
	switch n := v.(type) {
	case *values.BigInteger:
		return n, nil
	case *values.BigFloat:
		return n, nil
	case *values.Integer:
		return values.NewBigIntegerFromInt64(n.Value), nil
	case *values.Float:
		return values.NewBigFloatFromFloat64(n.Value), nil
	case *values.Rational:
		return values.NewBigFloatFromString(n.Rat().FloatString(256)), nil
	default:
		return nil, werr.WrapForeignErrorf(werr.ErrNotANumber, "%s: expected a real number but got %T", name, v)
	}
}

// PrimMakePolar implements the (make-polar) primitive.
func PrimMakePolar(mc *machine.MachineContext) error {
	r := mc.Arg(0)
	theta := mc.Arg(1)
	var mag, angle float64
	switch v := r.(type) {
	case *values.Integer:
		mag = float64(v.Value)
	case *values.Float:
		mag = v.Value
	case *values.Rational:
		mag = v.Float64()
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "make-polar: expected a real number but got %T", r)
	}
	switch v := theta.(type) {
	case *values.Integer:
		angle = float64(v.Value)
	case *values.Float:
		angle = v.Value
	case *values.Rational:
		angle = v.Float64()
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "make-polar: expected a real number but got %T", theta)
	}
	realPart := mag * math.Cos(angle)
	imagPart := mag * math.Sin(angle)
	mc.SetValue(values.NewComplexFromParts(realPart, imagPart))
	return nil
}

// PrimRealPart implements the (real-part) primitive.
func PrimRealPart(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	c, ok := o.(values.ComplexNumber)
	if ok {
		mc.SetValue(c.RealPart())
		return nil
	}
	switch o.(type) {
	case *values.Integer, *values.BigInteger, *values.Float, *values.BigFloat, *values.Rational:
		mc.SetValue(o)
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "real-part: expected a number but got %T", o)
	}
	return nil
}

// PrimImagPart implements the (imag-part) primitive.
func PrimImagPart(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	c, ok := o.(values.ComplexNumber)
	if ok {
		mc.SetValue(c.ImagPart())
		return nil
	}
	switch o.(type) {
	case *values.Integer, *values.BigInteger, *values.Rational:
		mc.SetValue(values.NewInteger(0))
	case *values.Float, *values.BigFloat:
		mc.SetValue(values.NewFloat(0.0))
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "imag-part: expected a number but got %T", o)
	}
	return nil
}

// PrimMagnitude implements the (magnitude) primitive.
func PrimMagnitude(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Complex:
		mc.SetValue(values.NewFloat(cmplx.Abs(v.Value)))
	case *values.BigComplex:
		// Convert to float64 for magnitude calculation (transcendental operation via sqrt)
		realF := v.RealAsBigFloat().Float64()
		imagF := v.ImagAsBigFloat().Float64()
		mc.SetValue(values.NewFloat(cmplx.Abs(complex(realF, imagF))))
	case *values.Integer:
		mc.SetValue(v.Abs())
	case *values.BigInteger:
		mc.SetValue(v.Abs())
	case *values.Float:
		mc.SetValue(values.NewFloat(math.Abs(v.Value)))
	case *values.BigFloat:
		bf := v.BigFloatValue()
		if bf.Sign() < 0 {
			bf = new(big.Float).Neg(bf)
		}
		mc.SetValue(values.NewBigFloat(bf))
	case *values.Rational:
		mc.SetValue(v.Abs())
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "magnitude: expected a number but got %T", o)
	}
	return nil
}

// PrimAngle implements the angle primitive.
func PrimAngle(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Complex:
		mc.SetValue(values.NewFloat(cmplx.Phase(v.Value)))
	case *values.BigComplex:
		// Convert to float64 for phase calculation (transcendental operation)
		realF := v.RealAsBigFloat().Float64()
		imagF := v.ImagAsBigFloat().Float64()
		mc.SetValue(values.NewFloat(cmplx.Phase(complex(realF, imagF))))
	case *values.Integer:
		if v.Value >= 0 {
			mc.SetValue(values.NewFloat(0))
		} else {
			mc.SetValue(values.NewFloat(math.Pi))
		}
	case *values.BigInteger:
		if v.BigInt().Sign() >= 0 {
			mc.SetValue(values.NewBigFloatFromFloat64(0))
		} else {
			mc.SetValue(values.NewBigFloatFromFloat64(math.Pi))
		}
	case *values.Float:
		if v.Value >= 0 {
			mc.SetValue(values.NewFloat(0))
		} else {
			mc.SetValue(values.NewFloat(math.Pi))
		}
	case *values.BigFloat:
		if v.BigFloatValue().Sign() >= 0 {
			mc.SetValue(values.NewBigFloatFromFloat64(0))
		} else {
			mc.SetValue(values.NewBigFloatFromFloat64(math.Pi))
		}
	case *values.Rational:
		if v.Rat().Sign() >= 0 {
			mc.SetValue(values.NewFloat(0))
		} else {
			mc.SetValue(values.NewFloat(math.Pi))
		}
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "angle: expected a number but got %T", o)
	}
	return nil
}

// PrimNumberToString implements the number->string primitive.
func PrimNumberToString(mc *machine.MachineContext) error {
	n := mc.Arg(0)
	rest := mc.Arg(1)
	radix := 10
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(values.Tuple)
		if ok && !pr.IsEmptyList() {
			r, ok := pr.Car().(*values.Integer)
			if !ok {
				return werr.WrapForeignErrorf(werr.ErrNotANumber, "number->string: expected an integer radix but got %T", pr.Car())
			}
			radix = int(r.Value)
			if radix != 2 && radix != 8 && radix != 10 && radix != 16 {
				return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "number->string: radix must be 2, 8, 10, or 16")
			}
		}
	}
	switch v := n.(type) {
	case *values.Integer:
		mc.SetValue(values.NewString(strconv.FormatInt(v.Value, radix)))
	case *values.Float:
		switch {
		case math.IsInf(v.Value, 1):
			mc.SetValue(values.NewString("+inf.0"))
		case math.IsInf(v.Value, -1):
			mc.SetValue(values.NewString("-inf.0"))
		case math.IsNaN(v.Value):
			mc.SetValue(values.NewString("+nan.0"))
		default:
			s := strconv.FormatFloat(v.Value, 'g', -1, 64)
			s = ensureInexactDecimal(s)
			mc.SetValue(values.NewString(s))
		}
	case *values.Rational:
		mc.SetValue(values.NewString(v.SchemeString()))
	case *values.Complex:
		mc.SetValue(values.NewString(v.SchemeString()))
	case *values.BigComplex:
		mc.SetValue(values.NewString(v.SchemeString()))
	case *values.BigInteger:
		mc.SetValue(values.NewString(v.BigInt().Text(radix)))
	case *values.BigFloat:
		mc.SetValue(values.NewString(v.SchemeString()))
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "number->string: expected a number but got %T", n)
	}
	return nil
}

// PrimStringToNumber implements the string->number primitive.
//
// R7RS §6.2.7: string->number returns a number of the maximally precise
// representation expressed by the given string. It is an error if radix
// is not 2, 8, 10, or 16.
//
// R7RS §7.1.1: The string may contain prefix directives #b, #o, #d, #x
// (radix) and #e, #i (exactness), in either order, up to one of each.
// A radix prefix in the string overrides the radix argument.
func PrimStringToNumber(mc *machine.MachineContext) error {
	s := mc.Arg(0)
	rest := mc.Arg(1)
	str, ok := s.(*values.String)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAString, "string->number: expected a string but got %T", s)
	}
	radix := 10
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(values.Tuple)
		if ok && !pr.IsEmptyList() {
			r, ok := pr.Car().(*values.Integer)
			if !ok {
				return werr.WrapForeignErrorf(werr.ErrNotANumber, "string->number: expected an integer radix but got %T", pr.Car())
			}
			radix = int(r.Value)
		}
	}

	input := str.Value
	exactness := 0 // 0 = unspecified, 1 = exact (#e), -1 = inexact (#i)

	// Parse up to two R7RS prefix directives.
	for range 2 {
		if len(input) < 2 || input[0] != '#' {
			break
		}
		switch input[1] {
		case 'b', 'B':
			radix = 2
			input = input[2:]
		case 'o', 'O':
			radix = 8
			input = input[2:]
		case 'd', 'D':
			radix = 10
			input = input[2:]
		case 'x', 'X':
			radix = 16
			input = input[2:]
		case 'e', 'E':
			exactness = 1
			input = input[2:]
		case 'i', 'I':
			exactness = -1
			input = input[2:]
		default:
			// Unknown prefix — not a valid number.
			mc.SetValue(values.FalseValue)
			return nil
		}
	}

	result := parseStringToNumber(input, radix)
	if result == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}

	// Apply exactness conversion if a prefix was specified.
	switch exactness {
	case 1:
		result = stringToNumberMakeExact(result)
	case -1:
		result = stringToNumberMakeInexact(result)
	}

	mc.SetValue(result)
	return nil
}

// parseStringToNumber parses a numeric string in the given radix.
// Returns nil if the string is not a valid number.
// R7RS §6.2.7: handles integers, rationals, floats, complex, and special values.
func parseStringToNumber(input string, radix int) values.Value {
	if len(input) == 0 {
		return nil
	}

	// Special float values (+inf.0, -inf.0, +nan.0, -nan.0) are always decimal.
	sf, ok := parser.ParseSpecialFloat(input)
	if ok {
		return sf
	}

	// Complex and imaginary numbers (only for radix 10).
	if radix == 10 && len(input) > 1 && (input[len(input)-1] == 'i' || input[len(input)-1] == 'I') {
		// Try pure imaginary first (no real part separator).
		n, imagOK := parser.ParseImaginaryStringNumber(input)
		if imagOK {
			return n
		}
		// Try full complex (real + imaginary parts).
		n, complexOK := parser.ParseComplexStringNumber(input)
		if complexOK {
			return n
		}
	}

	// Try integer first.
	i, err := strconv.ParseInt(input, radix, 64)
	if err == nil {
		return values.NewInteger(i)
	}

	// Try big integer for overflow.
	bi := new(big.Int)
	_, bigOK := bi.SetString(input, radix)
	if bigOK {
		return values.NewBigInteger(bi)
	}

	// Try rational (only if radix applies to both parts).
	idx := strings.Index(input, "/")
	if idx > 0 && idx < len(input)-1 {
		numStr := input[:idx]
		denStr := input[idx+1:]
		num := new(big.Int)
		den := new(big.Int)
		_, ok := num.SetString(numStr, radix)
		if ok {
			_, ok := den.SetString(denStr, radix)
			if ok {
				if den.Sign() == 0 {
					return nil
				}
				r := new(big.Rat).SetFrac(num, den)
				return values.Simplify(values.NewRationalFromRat(r))
			}
		}
	}

	// Float and scientific notation only for radix 10.
	if radix == 10 {
		f, err := strconv.ParseFloat(normalizeExponentMarker(input), 64)
		if err == nil {
			return values.NewFloat(f)
		}
	}

	return nil
}

// stringToNumberMakeExact converts a number to its exact representation.
//
// R7RS §6.2.6: exact returns an exact representation of z.
func stringToNumberMakeExact(n values.Value) values.Value {
	switch v := n.(type) {
	case *values.Integer, *values.BigInteger, *values.Rational:
		return v
	case *values.Float:
		f := v.Value
		if f == math.Trunc(f) && f >= math.MinInt64 && f <= math.MaxInt64 {
			return values.NewInteger(int64(f))
		}
		r := new(big.Rat).SetFloat64(f)
		if r == nil {
			return n // inf/nan — cannot convert
		}
		return values.Simplify(values.NewRationalFromRat(r))
	default:
		return n
	}
}

// stringToNumberMakeInexact converts a number to its inexact representation.
//
// R7RS §6.2.6: inexact returns an inexact representation of z.
func stringToNumberMakeInexact(n values.Value) values.Value {
	switch v := n.(type) {
	case *values.Float:
		return v
	case *values.Integer:
		return values.NewFloat(float64(v.Value))
	case *values.BigInteger:
		f, _ := new(big.Float).SetInt(v.BigInt()).Float64()
		return values.NewFloat(f)
	case *values.Rational:
		f, _ := v.Rat().Float64()
		return values.NewFloat(f)
	default:
		return n
	}
}

// normalizeExponentMarker replaces R7RS short float exponent suffixes
// (s, S, f, F, d, D, l, L) with 'e' so strconv.ParseFloat can parse them.
// R7RS §7.1.1: All exponent markers have the same meaning in Wile.
func normalizeExponentMarker(s string) string {
	idx := strings.IndexAny(s, "sSfFdDlL")
	if idx == -1 {
		return s
	}
	q := []byte(s)
	q[idx] = 'e'
	return string(q)
}
