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
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// PrimMakeRectangular implements make-rectangular.
// R7RS §6.2.6: If both arguments are exact, the result is exact.
func PrimMakeRectangular(mc machine.CallContext) error {
	r := mc.Arg(0)
	i := mc.Arg(1)

	// Check if both arguments are real numbers (not complex)
	rNum, rOk := r.(values.Number)
	iNum, iOk := i.(values.Number)
	if !rOk {
		return werr.WrapForeignErrorf(werr.ErrNotAReal, "make-rectangular: expected a real number but got %T", r)
	}
	if !iOk {
		return werr.WrapForeignErrorf(werr.ErrNotAReal, "make-rectangular: expected a real number but got %T", i)
	}

	// Reject complex numbers - make-rectangular requires real number arguments
	if !isRealNumber(r) {
		return werr.WrapForeignErrorf(werr.ErrNotAReal, "make-rectangular: expected a real number but got complex %T", r)
	}
	if !isRealNumber(i) {
		return werr.WrapForeignErrorf(werr.ErrNotAReal, "make-rectangular: expected a real number but got complex %T", i)
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
		panic(werr.WrapForeignErrorf(werr.ErrNotANumber, "toExactBigComplexPart: expected exact number but got %T", n))
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
		return nil, werr.WrapForeignErrorf(werr.ErrNotAReal, "%s: expected a real number but got %T", name, v)
	}
}

// PrimMakePolar implements the (make-polar) primitive.
func PrimMakePolar(mc machine.CallContext) error {
	r := mc.Arg(0)
	theta := mc.Arg(1)
	rNum, rOk := r.(values.Number)
	if !rOk || !isRealNumber(r) {
		return werr.WrapForeignErrorf(werr.ErrNotANumber,
			"make-polar: expected a real number but got %T", r)
	}
	tNum, tOk := theta.(values.Number)
	if !tOk || !isRealNumber(theta) {
		return werr.WrapForeignErrorf(werr.ErrNotANumber,
			"make-polar: expected a real number but got %T", theta)
	}
	mag := values.NumberToFloat64(rNum)
	angle := values.NumberToFloat64(tNum)
	realPart := mag * math.Cos(angle)
	imagPart := mag * math.Sin(angle)
	mc.SetValue(values.NewComplexFromParts(realPart, imagPart))
	return nil
}

// PrimRealPart implements the (real-part) primitive.
func PrimRealPart(mc machine.CallContext) error {
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
func PrimImagPart(mc machine.CallContext) error {
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
func PrimMagnitude(mc machine.CallContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Complex:
		mc.SetValue(values.NewFloat(cmplx.Abs(v.Value)))
	case *values.BigComplex:
		// Compute at big.Float precision; truncating to float64 first overflows
		// to +inf when a component exceeds the float64 range.
		mc.SetValue(v.Magnitude())
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

// PrimAngle implements R7RS §6.2.6 angle.
//
// The five real kinds used to be hand-unrolled here, one arm each, and every arm
// carried the same three bugs -- which is what a hand-unrolled dispatch buys you: the
// same mistake, five times, fixed one at a time forever. They ask ONE question, so
// they get one answer: angleOfReal.
func PrimAngle(mc machine.CallContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Complex:
		mc.SetValue(values.NewFloat(cmplx.Phase(v.Value)))
	case *values.BigComplex:
		// Big-precision atan2 (see values.BigAtan2); truncating to float64 first
		// collapses large-magnitude components with a finite ratio to a wrong angle.
		mc.SetValue(v.Phase())
	case values.RealNumber:
		q, err := angleOfReal(v)
		if err != nil {
			return err
		}
		mc.SetValue(q)
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "angle: expected a number but got %T", o)
	}
	return nil
}

// angleOfReal returns the angle of a real number: the direction of the point (x, 0).
//
// Three axes, each of which the per-kind arms got wrong:
//
// SIGN. A negative zero lies on the NEGATIVE real axis, so (angle -0.0) is π, not 0.
// IsNegative() cannot see that (-0.0 < 0 is false) and neither can Sign() (0 for both
// ±0); SignBit() can. Getting this wrong put the answer in the wrong QUADRANT, not
// merely off by a sign.
//
// EXACTNESS. The angle of any positive real is EXACTLY 0, whatever the operand's
// exactness -- a strong update, like the exact-zero rule, licensed because the result
// is known exactly. The arms all returned an inexact 0.0. Both oracles: (angle 1.0)
// is 0 and (exact? (angle 1.0)) is #t.
//
// DOMAIN. An EXACT zero is a mathematical zero: it has no direction, so its angle is
// undefined and this raises. An INEXACT ±0.0 is an IEEE value that does sit on one
// side of the axis, so it does not raise -- (angle 0.0) is 0 and (angle -0.0) is π.
// The exact-vs-inexact-zero distinction, one more time.
func angleOfReal(v values.RealNumber) (values.Value, error) {
	if v.IsZero() && v.IsExact() {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"angle: undefined for an exact zero, which has no direction")
	}
	// NaN has no side of the axis, so it has no angle: the answer is NaN, which is what
	// both oracles give. It must be checked BEFORE the sign test, because SignBit() is
	// false for a NaN, so it would otherwise fall through to the positive-real arm and
	// come back as an EXACT 0 -- a claim that the angle is known exactly, for an input
	// that carries no value at all. That is a worse answer than the π it used to give.
	if v.IsNaN() {
		return values.NewFloat(math.NaN()), nil
	}
	if v.SignBit() {
		return values.NewFloat(math.Pi), nil
	}
	return values.NewInteger(0), nil
}
