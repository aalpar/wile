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

package helpers

import (
	"math"
	"math/big"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// ToComplex128 converts a Scheme number to a Go complex128.
// This supports all numeric types: Integer, BigInteger, Float,
// BigFloat, Rational, Complex, and BigComplex.
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
		return 0, werr.WrapForeignErrorf(werr.ErrNotANumber, "expected a number but got %T", v)
	}
}

// ComplexOrFloat returns a Float if the imaginary part is zero,
// otherwise returns a Complex. This follows R7RS behavior where
// real results are returned as real numbers.
// Special case: If both parts are NaN, returns Float(NaN) since
// this typically results from operations on real NaN inputs.
func ComplexOrFloat(c complex128) values.Value {
	r := real(c)
	i := imag(c)
	if i == 0 || (math.IsNaN(r) && math.IsNaN(i)) {
		return values.NewFloat(r)
	}
	return values.NewComplex(c)
}

// ToFloat64 converts a Scheme real number to a Go float64, covering the full
// real numeric tower: Integer, BigInteger, Float, BigFloat, and Rational.
// Complex types are excluded — they cannot be reduced to a single float64
// without information loss. Use ToComplex128 for complex values.
func ToFloat64(v values.Value) (float64, error) {
	switch n := v.(type) {
	case *values.Integer:
		return float64(n.Value), nil
	case *values.BigInteger:
		f, _ := new(big.Float).SetInt(n.BigInt()).Float64()
		return f, nil
	case *values.Float:
		return n.Value, nil
	case *values.BigFloat:
		f, _ := n.BigFloatValue().Float64()
		return f, nil
	case *values.Rational:
		f, _ := n.Rat().Float64()
		return f, nil
	default:
		return 0, werr.WrapForeignErrorf(werr.ErrNotANumber, "expected a real number but got %T", v)
	}
}

// ExtractReal extracts a float64 from a real number, tracking exactness.
// Returns the float64 value, whether the input was exact, and any error.
//
// R7RS §6.2.6: Division procedures work on all real numbers.
func ExtractReal(v values.Value, name string) (float64, bool, error) {
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
		return 0, false, werr.WrapForeignErrorf(werr.ErrNotANumber, "%s: expected a real number but got %T", name, v)
	}
}
