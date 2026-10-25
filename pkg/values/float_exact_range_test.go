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

package values_test

import (
	"math"
	"math/big"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// TestFiniteFloatOutOfRangeExactPartner pins a finite Float meeting an exact
// operand outside float64's normal range: the operation is computed exactly and
// rounded once, where rounding the exact operand first lost it to +-Inf or zero.
// A zero Float keeps IEEE's answer and sign (Chez s/5_3.ss: exact-inexact* and
// its siblings). The complex rows reach the same kernel through the part-wise
// arithmetic of meetsFloat64. petite and racket agree on every row, and on all
// 752 rows of the grid {+ - * /} x both orders x {1e-300 -1e-300 1e300 1.5 -2.5
// 0.0 -0.0 +inf.0 -inf.0 +nan.0} x {10^400 -10^400 10^-400 -10^-400 10^-310
// 10^400/3 7/10^330}, plus the complex grid around the last rows below.
func TestFiniteFloatOutOfRangeExactPartner(t *testing.T) {
	pow10 := func(n int64) *big.Int {
		return new(big.Int).Exp(big.NewInt(10), big.NewInt(n), nil)
	}
	huge := values.NewBigInteger(pow10(400))
	negHuge := values.NewBigInteger(new(big.Int).Neg(pow10(400)))
	tiny := values.NewRationalFromBigInt(big.NewInt(1), pow10(400))
	negTiny := values.NewRationalFromBigInt(big.NewInt(-1), pow10(400))
	sub := values.NewRationalFromBigInt(big.NewInt(1), pow10(310)) // a float64 subnormal
	one := values.NewBigIntegerFromInt64(1)
	f := func(x float64) values.Number {
		return values.NewFloat(x)
	}
	negZero := math.Copysign(0, -1)
	add := func(a, b values.Number) (values.Number, error) {
		return a.Add(b), nil
	}
	subtract := func(a, b values.Number) (values.Number, error) {
		return a.Subtract(b), nil
	}
	mul := func(a, b values.Number) (values.Number, error) {
		return a.Multiply(b), nil
	}
	div := func(a, b values.Number) (values.Number, error) {
		return a.Divide(b)
	}
	tcs := []struct {
		name   string
		op     func(a, b values.Number) (values.Number, error)
		a, b   values.Number
		re, im float64
		cplx   bool
	}{
		{"(* 1e-300 10^400)", mul, f(1e-300), huge, 1e100, 0, false},
		{"(* 10^400 1e-300)", mul, huge, f(1e-300), 1e100, 0, false},
		{"(* 1e300 10^-400)", mul, f(1e300), tiny, 1e-100, 0, false},
		{"(* 1e300 10^-310)", mul, f(1e300), sub, 1e-10, 0, false},
		{"(* 1.5 10^-310)", mul, f(1.5), sub, 1.5e-310, 0, false},
		{"(/ 10^400 1e300)", div, huge, f(1e300), 1e100, 0, false},
		{"(/ 1e300 10^400)", div, f(1e300), huge, 1e-100, 0, false},
		{"(+ 1e-300 10^-400)", add, f(1e-300), tiny, 1e-300, 0, false},
		{"(+ 1e-300 10^-310)", add, f(1e-300), sub, 1.0000000001e-300, 0, false},
		// A zero Float: * and / keep IEEE's zero or infinity, signed.
		{"(* 10^400 0.0)", mul, huge, f(0), 0, 0, false},
		{"(* -10^400 0.0)", mul, negHuge, f(0), negZero, 0, false},
		{"(* -0.0 10^400)", mul, f(negZero), huge, negZero, 0, false},
		{"(/ 10^-400 0.0)", div, tiny, f(0), math.Inf(1), 0, false},
		{"(/ 0.0 -10^400)", div, f(0), negHuge, negZero, 0, false},
		// ... and + and - round the exact operand, as IEEE adds a zero.
		{"(+ -10^-400 0.0)", add, negTiny, f(0), 0, 0, false},
		{"(- -10^-400 0.0)", subtract, negTiny, f(0), negZero, 0, false},
		{"(+ 0.0 10^400)", add, f(0), huge, math.Inf(1), 0, false},
		// The result is out of range too: still +inf.0.
		{"(- 10^400 1e300)", subtract, huge, f(1e300), math.Inf(1), 0, false},
		// Complex: the parts take the same kernel.
		{"(* 1e-300 10^400+1i)", mul, f(1e-300), values.NewBigComplex(huge, one), 1e100, 1e-300, true},
		{"(* 1+10^400i 0.0)", mul, values.NewBigComplex(one, huge), f(0), 0, 0, true},
		{"(* 1+10^400i 0.0+0.0i)", mul, values.NewBigComplex(one, huge), values.NewComplex(0), 0, 0, true},
		{"(/ 1+10^400i 1e300+0.0i)", div, values.NewBigComplex(one, huge), values.NewComplex(1e300), 1e-300, math.Inf(1), true},
		{"(/ 10^400+1i 1e300)", div, values.NewBigComplex(huge, one), f(1e300), 1e100, 1e-300, true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			got, err := tc.op(tc.a, tc.b)
			c.Assert(err, qt.IsNil)
			if !tc.cplx {
				x, ok := got.(*values.Float)
				c.Assert(ok, qt.IsTrue, qt.Commentf("got %T %s", got, got.SchemeString()))
				assertSameFloat64(c, x.Value, tc.re, "value")
				return
			}
			z, ok := got.(*values.Complex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("got %T %s", got, got.SchemeString()))
			assertSameFloat64(c, real(z.Value), tc.re, "real part")
			assertSameFloat64(c, imag(z.Value), tc.im, "imaginary part")
		})
	}
}
