// Copyright 2025 Aaron Alpar
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

package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimMakeRectangular implements make-rectangular.
// Creates a complex number from real and imaginary parts: x+yi
// If either part is a BigInteger or BigFloat, returns a BigComplex.
func PrimMakeRectangular(_ context.Context, mc *machine.MachineContext) error {
	r := mc.Arg(0)
	i := mc.Arg(1)

	// Check if either part requires BigComplex
	_, rIsBigInt := r.(*values.BigInteger)
	_, rIsBigFloat := r.(*values.BigFloat)
	_, iIsBigInt := i.(*values.BigInteger)
	_, iIsBigFloat := i.(*values.BigFloat)

	if rIsBigInt || rIsBigFloat || iIsBigInt || iIsBigFloat {
		// Create BigComplex
		realPart, err := toBigComplexPart(r, "make-rectangular")
		if err != nil {
			return err
		}
		imagPart, err := toBigComplexPart(i, "make-rectangular")
		if err != nil {
			return err
		}
		// If both parts are BigInteger with imag=0, return BigInteger
		if imagPart.IsZero() {
			mc.SetValue(realPart)
			return nil
		}
		mc.SetValue(values.NewBigComplex(realPart, imagPart))
		return nil
	}

	// Standard Complex path
	var realPart, imagPart float64
	switch v := r.(type) {
	case *values.Integer:
		realPart = float64(v.Value)
	case *values.Float:
		realPart = v.Value
	case *values.Rational:
		realPart = v.Float64()
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
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "make-rectangular: expected a real number but got %T", i)
	}
	mc.SetValue(values.NewComplexFromParts(realPart, imagPart))
	return nil
}

// toBigComplexPart converts a real number to a BigComplex-compatible part.
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
		// Convert to BigFloat for inexact representation
		return values.NewBigFloatFromString(n.Rat().FloatString(256)), nil
	default:
		return nil, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a real number but got %T", name, v)
	}
}
