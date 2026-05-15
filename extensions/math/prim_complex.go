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

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
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
		// Convert to float64 for magnitude calculation (transcendental operation via sqrt)
		realF := v.RealAsBigFloat().Float64Truncated()
		imagF := v.ImagAsBigFloat().Float64Truncated()
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
func PrimAngle(mc machine.CallContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *values.Complex:
		mc.SetValue(values.NewFloat(cmplx.Phase(v.Value)))
	case *values.BigComplex:
		// Convert to float64 for phase calculation (transcendental operation)
		realF := v.RealAsBigFloat().Float64Truncated()
		imagF := v.ImagAsBigFloat().Float64Truncated()
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
