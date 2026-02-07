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
	"context"
	"math"
	"math/big"
	"math/cmplx"
	"strconv"
	"strings"

	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
)

// ToComplex128 converts a Scheme number to a Go complex128.
func ToComplex128(v values.Value) (complex128, error) {
	switch n := v.(type) {
	case *values.Integer:
		return complex(float64(n.Value), 0), nil
	case *values.BigInteger:
		f, _ := n.BigInt().Float64()
		return complex(f, 0), nil
	case *values.Float:
		return complex(n.Value, 0), nil
	case *values.BigFloat:
		f, _ := n.BigFloatValue().Float64()
		return complex(f, 0), nil
	case *values.Rational:
		f, _ := n.Rat().Float64()
		return complex(f, 0), nil
	case *values.Complex:
		return n.Value, nil
	case *values.BigComplex:
		r := n.RealAsBigFloat().Float64()
		i := n.ImagAsBigFloat().Float64()
		return complex(r, i), nil
	default:
		return 0, values.WrapForeignErrorf(values.ErrNotANumber, "expected a number but got %T", v)
	}
}

// ComplexOrFloat returns a Float if the imaginary part is zero,
// otherwise returns a Complex.
func ComplexOrFloat(c complex128) values.Value {
	r := real(c)
	i := imag(c)
	if i == 0 || (math.IsNaN(r) && math.IsNaN(i)) {
		return values.NewFloat(r)
	}
	return values.NewComplex(c)
}

// ToFloat64 converts a Scheme number to a Go float64.
func ToFloat64(v values.Value) (float64, error) {
	switch n := v.(type) {
	case *values.Integer:
		return float64(n.Value), nil
	case *values.Float:
		return n.Value, nil
	case *values.Rational:
		f, _ := n.Rat().Float64()
		return f, nil
	default:
		return 0, values.WrapForeignErrorf(values.ErrNotANumber, "expected a real number but got %T", v)
	}
}

// extractReal extracts a float64 from a real number for division operations.
// Returns the float64 value, whether the input was exact, and any error.
//
// R7RS §6.2.6: Division procedures work on all real numbers.
func extractReal(v values.Value, name string) (float64, bool, error) {
	switch n := v.(type) {
	case *values.Integer:
		return float64(n.Value), true, nil
	case *values.BigInteger:
		f, _ := new(big.Float).SetInt(n.BigInt()).Float64()
		return f, true, nil
	case *values.Float:
		return n.Value, false, nil
	case *values.Rational:
		f, _ := n.Rat().Float64()
		return f, true, nil
	case *values.BigFloat:
		f, _ := n.BigFloatValue().Float64()
		return f, false, nil
	default:
		return 0, false, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a real number but got %T", name, v)
	}
}

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

// FloorDivide performs floor division, returning quotient and remainder.
func FloorDivide(n0, n1 int64) (q, r int64) {
	q = n0 / n1
	r = n0 % n1
	if r != 0 && (n0 < 0) != (n1 < 0) {
		q--
		r += n1
	}
	return q, r
}

// PrimExp implements the (exp z) primitive.
func PrimExp(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	z, err := ToComplex128(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "exp: %v", err)
	}
	mc.SetValue(ComplexOrFloat(cmplx.Exp(z)))
	return nil
}

// PrimLog implements the (log z) and (log z1 z2) primitives.
func PrimLog(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	rest := mc.Arg(1)
	z, err := ToComplex128(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "log: %v", err)
	}
	if values.IsEmptyList(rest) {
		mc.SetValue(ComplexOrFloat(cmplx.Log(z)))
	} else {
		baseArg, ok := rest.(*values.Pair)
		if !ok {
			return values.NewForeignError("log: expected a list for rest arguments")
		}
		base, err := ToComplex128(baseArg.Car())
		if err != nil {
			return values.WrapForeignErrorf(err, "log: %v", err)
		}
		mc.SetValue(ComplexOrFloat(cmplx.Log(z) / cmplx.Log(base)))
	}
	return nil
}

// PrimSin implements the (sin z) primitive.
func PrimSin(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	z, err := ToComplex128(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "sin: %v", err)
	}
	mc.SetValue(ComplexOrFloat(cmplx.Sin(z)))
	return nil
}

// PrimCos implements the (cos z) primitive.
func PrimCos(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	z, err := ToComplex128(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "cos: %v", err)
	}
	mc.SetValue(ComplexOrFloat(cmplx.Cos(z)))
	return nil
}

// PrimTan implements the (tan z) primitive.
func PrimTan(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	z, err := ToComplex128(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "tan: %v", err)
	}
	mc.SetValue(ComplexOrFloat(cmplx.Tan(z)))
	return nil
}

// PrimAsin implements the (asin z) primitive.
func PrimAsin(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	z, err := ToComplex128(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "asin: %v", err)
	}
	mc.SetValue(ComplexOrFloat(cmplx.Asin(z)))
	return nil
}

// PrimAcos implements the (acos z) primitive.
func PrimAcos(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	z, err := ToComplex128(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "acos: %v", err)
	}
	mc.SetValue(ComplexOrFloat(cmplx.Acos(z)))
	return nil
}

// PrimAtan implements the (atan z) and (atan y x) primitives.
func PrimAtan(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	rest := mc.Arg(1)

	if values.IsEmptyList(rest) {
		z, err := ToComplex128(o)
		if err != nil {
			return values.WrapForeignErrorf(err, "atan: %v", err)
		}
		mc.SetValue(ComplexOrFloat(cmplx.Atan(z)))
	} else {
		y, err := ToFloat64(o)
		if err != nil {
			return values.WrapForeignErrorf(err, "atan: %v", err)
		}
		xArg, ok := rest.(*values.Pair)
		if !ok {
			return values.NewForeignError("atan: expected a list for rest arguments")
		}
		x, err := ToFloat64(xArg.Car())
		if err != nil {
			return values.WrapForeignErrorf(err, "atan: %v", err)
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
func PrimSqrt(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer:
		if v.Value < 0 {
			mc.SetValue(values.NewComplex(cmplx.Sqrt(complex(float64(v.Value), 0))))
		} else {
			mc.SetValue(values.NewFloat(math.Sqrt(float64(v.Value))))
		}
	case *values.Float:
		if v.Value < 0 {
			mc.SetValue(values.NewComplex(cmplx.Sqrt(complex(v.Value, 0))))
		} else {
			mc.SetValue(values.NewFloat(math.Sqrt(v.Value)))
		}
	case *values.Rational:
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
		return values.WrapForeignErrorf(values.ErrNotANumber, "sqrt: expected a number but got %T", o)
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

// PrimExpt implements the (expt) primitive.
func PrimExpt(_ context.Context, mc *machine.MachineContext) error {
	base := mc.Arg(0)
	exp := mc.Arg(1)
	baseNum, ok := base.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "expt: expected a number but got %T", base)
	}
	expNum, ok := exp.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "expt: expected a number but got %T", exp)
	}

	if expInt, ok := expNum.(*values.Integer); ok {
		e := expInt.Value

		if baseInt, ok := baseNum.(*values.Integer); ok {
			if e >= 0 {
				// Use big.Int to avoid overflow, then simplify if possible
				baseBig := big.NewInt(baseInt.Value)
				result := new(big.Int).Exp(baseBig, big.NewInt(e), nil)
				// Try to fit in int64, otherwise return BigInteger
				if result.IsInt64() {
					mc.SetValue(values.NewInteger(result.Int64()))
				} else {
					mc.SetValue(values.NewBigInteger(result))
				}
				return nil
			}
			// Negative exponent: compute 1 / base^|e|
			absE := -e
			baseBig := big.NewInt(baseInt.Value)
			denom := new(big.Int).Exp(baseBig, big.NewInt(absE), nil)
			mc.SetValue(values.NewRationalFromBigInt(big.NewInt(1), denom))
			return nil
		}

		if baseBig, ok := baseNum.(*values.BigInteger); ok {
			if e >= 0 {
				result := new(big.Int).Exp(baseBig.BigInt(), big.NewInt(e), nil)
				mc.SetValue(values.NewBigInteger(result))
				return nil
			}
			absE := -e
			denom := new(big.Int).Exp(baseBig.BigInt(), big.NewInt(absE), nil)
			mc.SetValue(values.NewRationalFromBigInt(big.NewInt(1), denom))
			return nil
		}

		if baseRat, ok := baseNum.(*values.Rational); ok {
			num := baseRat.Num()
			denom := baseRat.Denom()
			if e >= 0 {
				numResult := new(big.Int).Exp(num, big.NewInt(e), nil)
				denomResult := new(big.Int).Exp(denom, big.NewInt(e), nil)
				result := values.NewRationalFromBigInt(numResult, denomResult)
				if result.IsInteger() {
					mc.SetValue(values.NewInteger(result.NumInt64()))
					return nil
				}
				mc.SetValue(result)
				return nil
			}
			absE := -e
			numResult := new(big.Int).Exp(denom, big.NewInt(absE), nil)
			denomResult := new(big.Int).Exp(num, big.NewInt(absE), nil)
			result := values.NewRationalFromBigInt(numResult, denomResult)
			if result.IsInteger() {
				mc.SetValue(values.NewInteger(result.NumInt64()))
				return nil
			}
			mc.SetValue(result)
			return nil
		}
	}

	switch b := baseNum.(type) {
	case *values.Complex:
		switch e := expNum.(type) {
		case *values.Complex:
			mc.SetValue(values.NewComplex(cmplx.Pow(b.Value, e.Value)))
		case *values.Float:
			mc.SetValue(values.NewComplex(cmplx.Pow(b.Value, complex(e.Value, 0))))
		case *values.Integer:
			mc.SetValue(values.NewComplex(cmplx.Pow(b.Value, complex(float64(e.Value), 0))))
		case *values.Rational:
			mc.SetValue(values.NewComplex(cmplx.Pow(b.Value, complex(e.Float64(), 0))))
		case *values.BigComplex:
			eReal := e.RealAsBigFloat().Float64()
			eImag := e.ImagAsBigFloat().Float64()
			mc.SetValue(values.NewComplex(cmplx.Pow(b.Value, complex(eReal, eImag))))
		}
	case *values.BigComplex:
		bReal := b.RealAsBigFloat().Float64()
		bImag := b.ImagAsBigFloat().Float64()
		bComplex := complex(bReal, bImag)
		switch e := expNum.(type) {
		case *values.Complex:
			mc.SetValue(values.NewComplex(cmplx.Pow(bComplex, e.Value)))
		case *values.Float:
			mc.SetValue(values.NewComplex(cmplx.Pow(bComplex, complex(e.Value, 0))))
		case *values.Integer:
			mc.SetValue(values.NewComplex(cmplx.Pow(bComplex, complex(float64(e.Value), 0))))
		case *values.Rational:
			mc.SetValue(values.NewComplex(cmplx.Pow(bComplex, complex(e.Float64(), 0))))
		case *values.BigComplex:
			eReal := e.RealAsBigFloat().Float64()
			eImag := e.ImagAsBigFloat().Float64()
			mc.SetValue(values.NewComplex(cmplx.Pow(bComplex, complex(eReal, eImag))))
		case *values.BigInteger:
			ef, _ := e.BigInt().Float64()
			mc.SetValue(values.NewComplex(cmplx.Pow(bComplex, complex(ef, 0))))
		}
	default:
		var bf float64
		switch v := baseNum.(type) {
		case *values.Integer:
			bf = float64(v.Value)
		case *values.BigInteger:
			bf, _ = new(big.Float).SetInt(v.BigInt()).Float64()
		case *values.Float:
			bf = v.Value
		case *values.Rational:
			bf = v.Float64()
		}
		var ef float64
		switch v := expNum.(type) {
		case *values.Integer:
			ef = float64(v.Value)
		case *values.Float:
			ef = v.Value
		case *values.Rational:
			ef = v.Float64()
		case *values.Complex:
			mc.SetValue(values.NewComplex(cmplx.Pow(complex(bf, 0), v.Value)))
			return nil
		}
		mc.SetValue(values.NewFloat(math.Pow(bf, ef)))
	}
	return nil
}

// PrimSquare implements the (square) primitive.
func PrimSquare(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	n, ok := o.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "square: expected a number but got %T", o)
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

func makeRealNumberPrimitive(op realNumberOp) func(context.Context, *machine.MachineContext) error {
	return func(_ context.Context, mc *machine.MachineContext) error {
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
		case *values.Rational:
			mc.SetValue(op.rationalOp(v))
		default:
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a real number but got %T", op.name, o)
		}
		return nil
	}
}

func integerPassthrough(v *values.Integer) values.Value { return v }

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

// PrimFloorDiv implements the (floor/) primitive.
//
// R7RS §6.2.6: Returns two values: floor quotient and floor remainder.
// Works on any real numbers, returning exact results when both inputs are exact.
func PrimFloorDiv(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)

	n0, exact0, err := extractReal(o0, "floor/")
	if err != nil {
		return err
	}
	n1, exact1, err := extractReal(o1, "floor/")
	if err != nil {
		return err
	}

	if n1 == 0 {
		return values.NewForeignError("floor/: division by zero")
	}

	q := math.Floor(n0 / n1)
	r := n0 - q*n1

	if exact0 && exact1 {
		mc.SetValues(values.NewInteger(int64(q)), values.NewInteger(int64(r)))
	} else {
		mc.SetValues(values.NewFloat(q), values.NewFloat(r))
	}
	return nil
}

// PrimFloorQuotient implements the (floor-quotient) primitive.
//
// R7RS §6.2.6: Returns the floor quotient for any real numbers.
func PrimFloorQuotient(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)

	n0, exact0, err := extractReal(o0, "floor-quotient")
	if err != nil {
		return err
	}
	n1, exact1, err := extractReal(o1, "floor-quotient")
	if err != nil {
		return err
	}

	if n1 == 0 {
		return values.NewForeignError("floor-quotient: division by zero")
	}

	q := math.Floor(n0 / n1)

	if exact0 && exact1 {
		mc.SetValue(values.NewInteger(int64(q)))
	} else {
		mc.SetValue(values.NewFloat(q))
	}
	return nil
}

// PrimFloorRemainder implements the (floor-remainder) primitive.
//
// R7RS §6.2.6: Returns the floor remainder for any real numbers.
func PrimFloorRemainder(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)

	n0, exact0, err := extractReal(o0, "floor-remainder")
	if err != nil {
		return err
	}
	n1, exact1, err := extractReal(o1, "floor-remainder")
	if err != nil {
		return err
	}

	if n1 == 0 {
		return values.NewForeignError("floor-remainder: division by zero")
	}

	q := math.Floor(n0 / n1)
	r := n0 - q*n1

	if exact0 && exact1 {
		mc.SetValue(values.NewInteger(int64(r)))
	} else {
		mc.SetValue(values.NewFloat(r))
	}
	return nil
}

// PrimTruncateDiv implements the truncate/ primitive.
//
// R7RS §6.2.6: Returns two values: truncate quotient and truncate remainder.
// Works on any real numbers, returning exact results when both inputs are exact.
func PrimTruncateDiv(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)

	n0, exact0, err := extractReal(o0, "truncate/")
	if err != nil {
		return err
	}
	n1, exact1, err := extractReal(o1, "truncate/")
	if err != nil {
		return err
	}

	if n1 == 0 {
		return values.NewForeignError("truncate/: division by zero")
	}

	q := math.Trunc(n0 / n1)
	r := n0 - q*n1

	if exact0 && exact1 {
		mc.SetValues(values.NewInteger(int64(q)), values.NewInteger(int64(r)))
	} else {
		mc.SetValues(values.NewFloat(q), values.NewFloat(r))
	}
	return nil
}

// PrimTruncateQuotient implements the truncate-quotient primitive.
//
// R7RS §6.2.6: Returns the truncate quotient for any real numbers.
func PrimTruncateQuotient(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)

	n0, exact0, err := extractReal(o0, "truncate-quotient")
	if err != nil {
		return err
	}
	n1, exact1, err := extractReal(o1, "truncate-quotient")
	if err != nil {
		return err
	}

	if n1 == 0 {
		return values.NewForeignError("truncate-quotient: division by zero")
	}

	q := math.Trunc(n0 / n1)

	if exact0 && exact1 {
		mc.SetValue(values.NewInteger(int64(q)))
	} else {
		mc.SetValue(values.NewFloat(q))
	}
	return nil
}

// PrimTruncateRemainder implements the truncate-remainder primitive.
//
// R7RS §6.2.6: Returns the truncate remainder for any real numbers.
func PrimTruncateRemainder(_ context.Context, mc *machine.MachineContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)

	n0, exact0, err := extractReal(o0, "truncate-remainder")
	if err != nil {
		return err
	}
	n1, exact1, err := extractReal(o1, "truncate-remainder")
	if err != nil {
		return err
	}

	if n1 == 0 {
		return values.NewForeignError("truncate-remainder: division by zero")
	}

	q := math.Trunc(n0 / n1)
	r := n0 - q*n1

	if exact0 && exact1 {
		mc.SetValue(values.NewInteger(int64(r)))
	} else {
		mc.SetValue(values.NewFloat(r))
	}
	return nil
}

// PrimFiniteQ implements the (finite?) primitive.
func PrimFiniteQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer, *values.BigInteger, *values.BigFloat, *values.Rational, *values.BigComplex:
		// BigInteger, Rational, BigComplex (exact) are always finite
		// BigFloat uses math/big which doesn't support infinity/NaN
		mc.SetValue(values.TrueValue)
	case *values.Float:
		mc.SetValue(schemeutil.BoolToBoolean(!math.IsInf(v.Value, 0) && !math.IsNaN(v.Value)))
	case *values.Complex:
		rel := real(v.Value)
		iam := imag(v.Value)
		isFinite := !math.IsInf(rel, 0) && !math.IsNaN(rel) && !math.IsInf(iam, 0) && !math.IsNaN(iam)
		mc.SetValue(schemeutil.BoolToBoolean(isFinite))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "finite?: expected a number but got %T", o)
	}
	return nil
}

// PrimInfiniteQ implements the (infinite?) primitive.
func PrimInfiniteQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer, *values.BigInteger, *values.BigFloat, *values.Rational, *values.BigComplex:
		// BigInteger, Rational, BigComplex (exact) are never infinite
		// BigFloat uses math/big which doesn't support infinity
		mc.SetValue(values.FalseValue)
	case *values.Float:
		mc.SetValue(schemeutil.BoolToBoolean(math.IsInf(v.Value, 0)))
	case *values.Complex:
		rel := real(v.Value)
		iam := imag(v.Value)
		isInfinite := math.IsInf(rel, 0) || math.IsInf(iam, 0)
		mc.SetValue(schemeutil.BoolToBoolean(isInfinite))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "infinite?: expected a number but got %T", o)
	}
	return nil
}

// PrimNanQ implements the nan? primitive.
func PrimNanQ(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Integer, *values.BigInteger, *values.BigFloat, *values.Rational, *values.BigComplex:
		// BigInteger, Rational, BigComplex (exact) are never NaN
		// BigFloat uses math/big which doesn't support NaN
		mc.SetValue(values.FalseValue)
	case *values.Float:
		mc.SetValue(schemeutil.BoolToBoolean(math.IsNaN(v.Value)))
	case *values.Complex:
		rel := real(v.Value)
		iam := imag(v.Value)
		isNaN := math.IsNaN(rel) || math.IsNaN(iam)
		mc.SetValue(schemeutil.BoolToBoolean(isNaN))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "nan?: expected a number but got %T", o)
	}
	return nil
}

// PrimNumerator implements the numerator primitive.
func PrimNumerator(_ context.Context, mc *machine.MachineContext) error {
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
			return values.NewForeignError("numerator: cannot get numerator of infinity or NaN")
		}
		num := r.Num()
		f, _ := new(big.Float).SetInt(num).Float64()
		mc.SetValue(values.NewFloat(f))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "numerator: expected a rational number but got %T", o)
	}
	return nil
}

// PrimDenominator implements the (denominator) primitive.
func PrimDenominator(_ context.Context, mc *machine.MachineContext) error {
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
			return values.NewForeignError("denominator: cannot get denominator of infinity or NaN")
		}
		denom := r.Denom()
		f, _ := new(big.Float).SetInt(denom).Float64()
		mc.SetValue(values.NewFloat(f))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "denominator: expected a rational number but got %T", o)
	}
	return nil
}

// PrimRationalize implements the (rationalize) primitive.
func PrimRationalize(_ context.Context, mc *machine.MachineContext) error {
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
			return values.NewForeignError("rationalize: x cannot be infinity or NaN")
		}
		xExact = false
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "rationalize: expected a real number for x but got %T", xArg)
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
			return values.NewForeignError("rationalize: y cannot be infinity or NaN")
		}
		yExact = false
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "rationalize: expected a real number for y but got %T", yArg)
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
func PrimExactIntegerSqrt(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)

	switch v := o.(type) {
	case *values.Integer:
		if v.Value < 0 {
			return values.NewForeignError("exact-integer-sqrt: expected a non-negative integer")
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
			return values.NewForeignError("exact-integer-sqrt: expected a non-negative integer")
		}
		// Use big.Int.Sqrt which computes floor(sqrt(n))
		s := new(big.Int).Sqrt(v.BigInt())
		// Compute remainder: r = n - s²
		sSquared := new(big.Int).Mul(s, s)
		r := new(big.Int).Sub(v.BigInt(), sSquared)
		mc.SetValues(values.NewBigInteger(s), values.NewBigInteger(r))
		return nil

	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "exact-integer-sqrt: expected an exact integer but got %T", o)
	}
}

// PrimMakeRectangular implements make-rectangular.
// R7RS §6.2.6: If both arguments are exact, the result is exact.
func PrimMakeRectangular(_ context.Context, mc *machine.MachineContext) error {
	r := mc.Arg(0)
	i := mc.Arg(1)

	// Check if both arguments are real numbers (not complex)
	rNum, rOk := r.(values.Number)
	iNum, iOk := i.(values.Number)
	if !rOk {
		return values.WrapForeignErrorf(values.ErrNotANumber, "make-rectangular: expected a real number but got %T", r)
	}
	if !iOk {
		return values.WrapForeignErrorf(values.ErrNotANumber, "make-rectangular: expected a real number but got %T", i)
	}

	// Reject complex numbers - make-rectangular requires real number arguments
	if !isRealNumber(r) {
		return values.WrapForeignErrorf(values.ErrNotANumber, "make-rectangular: expected a real number but got complex %T", r)
	}
	if !isRealNumber(i) {
		return values.WrapForeignErrorf(values.ErrNotANumber, "make-rectangular: expected a real number but got complex %T", i)
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
	var realPart, imagPart float64
	switch v := r.(type) {
	case *values.Integer:
		realPart = float64(v.Value)
	case *values.Float:
		realPart = v.Value
	case *values.Rational:
		realPart = v.Float64()
	case *values.BigInteger:
		realPart, _ = new(big.Float).SetInt(v.BigInt()).Float64()
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "make-rectangular: expected a real number but got %T", r)
	}
	switch v := i.(type) {
	case *values.Integer:
		imagPart = float64(v.Value)
	case *values.Float:
		imagPart = v.Value
	case *values.Rational:
		imagPart = v.Float64()
	case *values.BigInteger:
		imagPart, _ = new(big.Float).SetInt(v.BigInt()).Float64()
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "make-rectangular: expected a real number but got %T", i)
	}
	mc.SetValue(values.NewComplexFromParts(realPart, imagPart))
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
		return nil, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a real number but got %T", name, v)
	}
}

// PrimMakePolar implements the (make-polar) primitive.
func PrimMakePolar(_ context.Context, mc *machine.MachineContext) error {
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
		return values.WrapForeignErrorf(values.ErrNotANumber, "make-polar: expected a real number but got %T", r)
	}
	switch v := theta.(type) {
	case *values.Integer:
		angle = float64(v.Value)
	case *values.Float:
		angle = v.Value
	case *values.Rational:
		angle = v.Float64()
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "make-polar: expected a real number but got %T", theta)
	}
	realPart := mag * math.Cos(angle)
	imagPart := mag * math.Sin(angle)
	mc.SetValue(values.NewComplexFromParts(realPart, imagPart))
	return nil
}

// PrimRealPart implements the (real-part) primitive.
func PrimRealPart(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	if c, ok := o.(values.ComplexNumber); ok {
		mc.SetValue(c.RealPart())
		return nil
	}
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(values.NewFloat(float64(v.Value)))
	case *values.BigInteger:
		mc.SetValue(v)
	case *values.Float:
		mc.SetValue(v)
	case *values.BigFloat:
		mc.SetValue(v)
	case *values.Rational:
		mc.SetValue(values.NewFloat(v.Float64()))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "real-part: expected a number but got %T", o)
	}
	return nil
}

// PrimImagPart implements the (imag-part) primitive.
func PrimImagPart(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	if c, ok := o.(values.ComplexNumber); ok {
		mc.SetValue(c.ImagPart())
		return nil
	}
	switch o.(type) {
	case *values.Integer, *values.Float, *values.Rational:
		mc.SetValue(values.NewFloat(0))
	case *values.BigInteger:
		mc.SetValue(values.NewBigIntegerFromInt64(0))
	case *values.BigFloat:
		mc.SetValue(values.NewBigFloatFromFloat64(0))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "imag-part: expected a number but got %T", o)
	}
	return nil
}

// PrimMagnitude implements the (magnitude) primitive.
func PrimMagnitude(_ context.Context, mc *machine.MachineContext) error {
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
		mc.SetValue(values.NewFloat(math.Abs(float64(v.Value))))
	case *values.BigInteger:
		bi := v.BigInt()
		if bi.Sign() < 0 {
			bi = new(big.Int).Neg(bi)
		}
		mc.SetValue(values.NewBigFloat(new(big.Float).SetInt(bi)))
	case *values.Float:
		mc.SetValue(values.NewFloat(math.Abs(v.Value)))
	case *values.BigFloat:
		bf := v.BigFloatValue()
		if bf.Sign() < 0 {
			bf = new(big.Float).Neg(bf)
		}
		mc.SetValue(values.NewBigFloat(bf))
	case *values.Rational:
		mc.SetValue(values.NewFloat(math.Abs(v.Float64())))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "magnitude: expected a number but got %T", o)
	}
	return nil
}

// PrimAngle implements the angle primitive.
func PrimAngle(_ context.Context, mc *machine.MachineContext) error {
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
		return values.WrapForeignErrorf(values.ErrNotANumber, "angle: expected a number but got %T", o)
	}
	return nil
}

// PrimNumberToString implements the number->string primitive.
func PrimNumberToString(_ context.Context, mc *machine.MachineContext) error {
	n := mc.Arg(0)
	rest := mc.Arg(1)
	radix := 10
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(*values.Pair)
		if ok && !values.IsEmptyList(pr) {
			r, ok := pr.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotANumber, "number->string: expected an integer radix but got %T", pr.Car())
			}
			radix = int(r.Value)
			if radix != 2 && radix != 8 && radix != 10 && radix != 16 {
				return values.NewForeignError("number->string: radix must be 2, 8, 10, or 16")
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
		mc.SetValue(values.NewString(v.SchemeString()))
	case *values.BigFloat:
		mc.SetValue(values.NewString(v.SchemeString()))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "number->string: expected a number but got %T", n)
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
func PrimStringToNumber(_ context.Context, mc *machine.MachineContext) error {
	s := mc.Arg(0)
	rest := mc.Arg(1)
	str, ok := s.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string->number: expected a string but got %T", s)
	}
	radix := 10
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(*values.Pair)
		if ok && !values.IsEmptyList(pr) {
			r, ok := pr.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotANumber, "string->number: expected an integer radix but got %T", pr.Car())
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
func parseStringToNumber(input string, radix int) values.Value {
	if len(input) == 0 {
		return nil
	}

	// Try integer first.
	if i, err := strconv.ParseInt(input, radix, 64); err == nil {
		return values.NewInteger(i)
	}

	// Try big integer for overflow.
	bi := new(big.Int)
	if _, ok := bi.SetString(input, radix); ok {
		return values.NewBigInteger(bi)
	}

	// Try rational (only if radix applies to both parts).
	if idx := strings.Index(input, "/"); idx > 0 && idx < len(input)-1 {
		numStr := input[:idx]
		denStr := input[idx+1:]
		num := new(big.Int)
		den := new(big.Int)
		if _, ok := num.SetString(numStr, radix); ok {
			if _, ok := den.SetString(denStr, radix); ok {
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
		if f, err := strconv.ParseFloat(normalizeExponentMarker(input), 64); err == nil {
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
