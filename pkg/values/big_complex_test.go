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
	"github.com/aalpar/wile/pkg/values/valuestest"
)

func TestBigComplex_Constructors(t *testing.T) {
	c := qt.New(t)

	// From BigIntegers (exact)
	bc1 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	c.Assert(bc1.Real().(*values.BigInteger).Int64(), qt.Equals, int64(3))
	c.Assert(bc1.Imag().(*values.BigInteger).Int64(), qt.Equals, int64(4))
	c.Assert(bc1.IsExact(), qt.IsTrue)

	// From BigFloats (inexact)
	bc2 := values.NewBigComplexFromBigFloats(
		values.NewBigFloatFromFloat64(1.5),
		values.NewBigFloatFromFloat64(2.5),
	)
	c.Assert(bc2.RealAsBigFloat().Float64Truncated(), qt.Equals, 1.5)
	c.Assert(bc2.ImagAsBigFloat().Float64Truncated(), qt.Equals, 2.5)
	c.Assert(bc2.IsExact(), qt.IsFalse)

	// Mixed parts (inexact due to BigFloat)
	bc3 := values.NewBigComplex(
		values.NewBigIntegerFromInt64(1),
		values.NewBigFloatFromFloat64(2.0),
	)
	c.Assert(bc3.IsExact(), qt.IsFalse)
}

func TestBigComplex_Arithmetic(t *testing.T) {
	c := qt.New(t)

	bc1 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	bc2 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(1),
		values.NewBigIntegerFromInt64(2),
	)

	// Add: (3+4i) + (1+2i) = (4+6i)
	sum := bc1.Add(bc2)
	c.Assert(sum, qt.IsNotNil)
	bc := sum.(*values.BigComplex)
	c.Assert(bc.Real().(*values.BigInteger).Int64(), qt.Equals, int64(4))
	c.Assert(bc.Imag().(*values.BigInteger).Int64(), qt.Equals, int64(6))

	// Subtract: (3+4i) - (1+2i) = (2+2i)
	diff := bc1.Subtract(bc2)
	c.Assert(diff, qt.IsNotNil)
	bc = diff.(*values.BigComplex)
	c.Assert(bc.Real().(*values.BigInteger).Int64(), qt.Equals, int64(2))
	c.Assert(bc.Imag().(*values.BigInteger).Int64(), qt.Equals, int64(2))

	// Multiply: (3+4i) * (1+2i) = (3*1 - 4*2) + (3*2 + 4*1)i = -5 + 10i
	prod := bc1.Multiply(bc2)
	c.Assert(prod, qt.IsNotNil)
	bc = prod.(*values.BigComplex)
	c.Assert(bc.Real().(*values.BigInteger).Int64(), qt.Equals, int64(-5))
	c.Assert(bc.Imag().(*values.BigInteger).Int64(), qt.Equals, int64(10))

	// Negate
	neg := bc1.Negate()
	c.Assert(neg, qt.IsNotNil)
	bc = neg.(*values.BigComplex)
	c.Assert(bc.Real().(*values.BigInteger).Int64(), qt.Equals, int64(-3))
	c.Assert(bc.Imag().(*values.BigInteger).Int64(), qt.Equals, int64(-4))
}

// TestBigComplex_Division pins exact complex division.
//
// Dividing two EXACT complex numbers yields an EXACT result: the parts divide
// through the numeric tower, so an Integer/Integer quotient becomes a Rational
// rather than collapsing to a float. Both oracles agree:
//
//	petite -q <<< '(display (list (/ 3+4i 1+2i) (/ 2+2i 1+1i) (/ 0+1i 0+1i)))'
//	racket   -e '(display (list (/ 3+4i 1+2i) (/ 2+2i 1+1i) (/ 0+1i 0+1i)))'
//	# both => (11/5-2/5i 2 1)
//
// Before 2026-07-12 this test asserted the opposite ("division always produces
// BigFloat parts", quotient 2.2-0.4i). That was the bug, not the contract: the
// implementation coerced both parts to BigFloat before dividing, destroying
// exactness unconditionally. The assertions below are the R7RS §6.2.2 contract.
func TestBigComplex_Division(t *testing.T) {
	c := qt.New(t)

	// (3+4i) / (1+2i) = ((3*1+4*2) + (4*1-3*2)i) / (1+4) = (11 - 2i) / 5
	//                 = 11/5 - 2/5i, EXACTLY — not 2.2 - 0.4i.
	bc1 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	bc2 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(1),
		values.NewBigIntegerFromInt64(2),
	)

	quot, err := bc1.Divide(bc2)
	c.Assert(err, qt.IsNil)
	c.Assert(quot, qt.IsNotNil)
	c.Assert(quot.SchemeString(), qt.Equals, "11/5-2/5i")
	c.Assert(quot.IsExact(), qt.IsTrue,
		qt.Commentf("exact operands must divide exactly; a float quotient means the parts were coerced"))

	// Division by zero returns error
	zero := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(0),
		values.NewBigIntegerFromInt64(0),
	)
	_, err = bc1.Divide(zero)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Matches, ".*division by zero")

	// A quotient whose imaginary part is a mathematically-exact zero demotes to a
	// real. (0+1i)/(0+1i): numerImag = (0*0) - (1*1)... = exact Integer 0, so
	// maybeSimplify demotes and the result is the exact integer 1, not 1.0 and not
	// a complex 1+0i stranded by an inexact zero imag.
	bcPureImag := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(0),
		values.NewBigIntegerFromInt64(1),
	)
	quotImag, err := bcPureImag.Divide(bcPureImag)
	c.Assert(err, qt.IsNil)
	c.Assert(quotImag, qt.IsNotNil)
	c.Assert(quotImag.SchemeString(), qt.Equals, "1")
	c.Assert(quotImag.IsExact(), qt.IsTrue)

	// The mirror case, and the reason maybeSimplify must test exactness rather
	// than magnitude: an INEXACT zero imaginary part does NOT make a number real
	// (R7RS §6.2.6). (/ 1.0+1.0i 1.0+1.0i) stays complex at 1.0+0.0i in both
	// oracles — demoting it to a bare 1.0 would discard an observable component.
	inexact := values.NewBigComplex(
		values.NewBigFloatFromFloat64(1.0),
		values.NewBigFloatFromFloat64(1.0),
	)
	quotInexact, err := inexact.Divide(inexact)
	c.Assert(err, qt.IsNil)
	c.Assert(quotInexact.SchemeString(), qt.Equals, "1.0+0.0i")
	c.Assert(quotInexact.IsExact(), qt.IsFalse)

	// Dividing by an INEXACT zero complex does NOT raise: 0.0+0.0i is an IEEE
	// value, so the quotient is NaN under IEEE rather than a division-by-zero
	// error. Only an EXACT zero divisor raises (asserted above).
	//
	// The COMPLEX zero and the REAL zero are different divisors and the oracles
	// give different answers — this pair is the reason BigComplex.Divide must
	// branch on the divisor's KIND before promotion rather than on its imaginary
	// part's exactness after it (promotion makes a real 0.0 look like 0.0+0.0i):
	//
	//	petite -q <<< '(display (list (/ 1+1i 0.0+0.0i) (/ 1+1i 0.0)))'
	//	racket   -e '(display (list (/ 1+1i 0.0+0.0i) (/ 1+1i 0.0)))'
	//	# both => (+nan.0+nan.0i +inf.0+inf.0i)
	//
	// The complex zero drives the general formula's denominator (c²+d²) to 0.0, so
	// 0/0 => NaN. The real zero divides each part by c, so x/0.0 => a signed
	// infinity. TestBigComplex_DivideByRealZero pins the other half of the pair.
	inexactZero := values.NewBigComplex(
		values.NewBigFloatFromFloat64(0.0),
		values.NewBigFloatFromFloat64(0.0),
	)
	one := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(1),
		values.NewBigIntegerFromInt64(1),
	)
	quotNaN, err := one.Divide(inexactZero)
	c.Assert(err, qt.IsNil,
		qt.Commentf("an inexact zero divisor is an IEEE value, not a division-by-zero error"))
	c.Assert(quotNaN.SchemeString(), qt.Equals, "+nan.0+nan.0i")
}

// TestBigComplex_DivideByRealZero pins the regression half of the divisor pair:
// dividing by a REAL inexact zero yields a signed infinity, NOT NaN.
//
// This is the case an exactness test on the divisor's imaginary part cannot see.
// Dispatch promotes a real divisor into the complex LUB, manufacturing an inexact
// zero imaginary part that is byte-identical to a user-written 0.0+0.0i — so the
// decision must be made on the divisor's KIND, before promotion. Both oracles:
//
//	petite -q <<< '(display (list (/ 1+2i 0.0) (/ 1+2i -0.0) (/ 1+2i 0.0+0.0i)))'
//	racket   -e '(display (list (/ 1+2i 0.0) (/ 1+2i -0.0) (/ 1+2i 0.0+0.0i)))'
//	# both => (+inf.0+inf.0i -inf.0-inf.0i +nan.0+nan.0i)
func TestBigComplex_DivideByRealZero(t *testing.T) {
	oneTwo := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(1),
		values.NewBigIntegerFromInt64(2),
	)

	tcs := []struct {
		name    string
		divisor values.Number
		want    string
	}{
		{"Float +0.0", values.NewFloat(0.0), "+inf.0+inf.0i"},
		{"Float -0.0", values.NewFloat(math.Copysign(0, -1)), "-inf.0-inf.0i"},
		{"BigFloat +0.0", values.NewBigFloatFromFloat64(0.0), "+inf.0+inf.0i"},
		// A nonzero real divisor takes the same part-wise path; pin that it is
		// still correct, so the fast path is not merely "right on zeros".
		{"Float 2.0", values.NewFloat(2.0), "0.5+1.0i"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			got, err := oneTwo.Divide(tc.divisor)
			c.Assert(err, qt.IsNil)
			c.Assert(got.SchemeString(), qt.Equals, tc.want,
				qt.Commentf("a REAL zero divisor must give a signed infinity, not the NaN a COMPLEX zero gives"))
		})
	}
}

// TestBigComplex_SchemeStringSign pins the "+" separator against imaginary parts
// that already carry their own sign.
//
// The separator must be chosen on the RENDERED text, not on IsNegative(). Three
// values are not negative yet print with a leading sign, and an IsNegative() test
// misses all three, emitting a double sign:
//
//	-0.0    is not < 0        printed "5.0+-0.0i"     want "5.0-0.0i"
//	+inf.0  is not < 0        printed "5.0++inf.0i"   want "5.0+inf.0i"
//	+nan.0  is not < 0        printed "5.0++nan.0i"   want "5.0+nan.0i"
//
// (*Complex).SchemeString already tested the rendered text; BigComplex did not.
//
// The five all-inexact rows are pinned to both oracles, which render these exact
// shapes:
//
//	petite -q <<< '(display (list (make-rectangular 5.0 +inf.0) (make-rectangular 5.0 -0.0)))'
//	racket   -e '(display (list (make-rectangular 5.0 +inf.0) (make-rectangular 5.0 -0.0)))'
//	# both => (5.0+inf.0i 5.0-0.0i)
//
// The three exact-imag rows are NOT oracle-pinned, and deliberately so: Chez and
// Racket normalize a mixed-exactness complex to all-inexact parts, so
// (make-rectangular 5.0 -3) renders as 5.0-3.0i there and the shape under test —
// an inexact real beside an exact imag — is not constructible in either. They are
// unit tests of the separator branch on a wile-internal value, which is what this
// function actually decides.
func TestBigComplex_SchemeStringSign(t *testing.T) {
	negZero := values.NewBigFloatFromFloat64(math.Copysign(0, -1))

	tcs := []struct {
		name string
		imag values.Number
		want string
	}{
		// Oracle-pinned (all-inexact parts).
		{"negative zero", negZero, "5.0-0.0i"},
		{"positive zero", values.NewBigFloatFromFloat64(0.0), "5.0+0.0i"},
		{"positive infinity", values.NewBigFloatFromFloat64(math.Inf(1)), "5.0+inf.0i"},
		{"negative infinity", values.NewBigFloatFromFloat64(math.Inf(-1)), "5.0-inf.0i"},
		{"NaN", values.NewBigFloatNaN(), "5.0+nan.0i"},
		// Separator-branch only; see the note above on why these are not oracle-pinned.
		{"negative integer", values.NewBigIntegerFromInt64(-3), "5.0-3i"},
		{"positive integer", values.NewBigIntegerFromInt64(3), "5.0+3i"},
		{"negative rational", values.NewRational(-2, 5), "5.0-2/5i"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			bc := values.NewBigComplex(values.NewBigFloatFromFloat64(5), tc.imag)
			c.Assert(bc.SchemeString(), qt.Equals, tc.want,
				qt.Commentf("imaginary part rendered as %q", tc.imag.SchemeString()))
		})
	}
}

func TestBigComplex_MixedArithmetic(t *testing.T) {
	c := qt.New(t)

	bc := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)

	// Add with BigInteger: (3+4i) + 5 = (8+4i)
	sum1 := bc.Add(values.NewBigIntegerFromInt64(5))
	c.Assert(sum1.(*values.BigComplex).Real().(*values.BigInteger).Int64(), qt.Equals, int64(8))
	c.Assert(sum1.(*values.BigComplex).Imag().(*values.BigInteger).Int64(), qt.Equals, int64(4))

	// Add with BigFloat: (3+4i) + 1.5 = (4.5+4i) - becomes inexact
	sum2 := bc.Add(values.NewBigFloatFromFloat64(1.5))
	c.Assert(sum2.(*values.BigComplex).IsExact(), qt.IsFalse)
	c.Assert(sum2.(*values.BigComplex).RealAsBigFloat().Float64Truncated(), qt.Equals, 4.5)

	// Add with Integer: promotes Integer to BigInteger
	sum3 := bc.Add(values.NewInteger(10))
	c.Assert(sum3.(*values.BigComplex).Real().(*values.BigInteger).Int64(), qt.Equals, int64(13))

	// Add with Float: becomes inexact
	sum4 := bc.Add(values.NewFloat(2.5))
	c.Assert(sum4.(*values.BigComplex).IsExact(), qt.IsFalse)

	// Add with Complex
	cplx := values.NewComplexFromParts(1.0, 1.0)
	sum5 := bc.Add(cplx)
	c.Assert(sum5.(*values.BigComplex).RealAsBigFloat().Float64Truncated(), qt.Equals, 4.0)
	c.Assert(sum5.(*values.BigComplex).ImagAsBigFloat().Float64Truncated(), qt.Equals, 5.0)
}

func TestBigComplex_Exactness(t *testing.T) {
	c := qt.New(t)

	// Exact complex (both parts BigInteger)
	exact := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	c.Assert(exact.IsExact(), qt.IsTrue)

	// Inexact complex (both parts BigFloat)
	inexact := values.NewBigComplexFromBigFloats(
		values.NewBigFloatFromFloat64(3.0),
		values.NewBigFloatFromFloat64(4.0),
	)
	c.Assert(inexact.IsExact(), qt.IsFalse)

	// Mixed (one BigInteger, one BigFloat) - inexact
	mixed := values.NewBigComplex(
		values.NewBigIntegerFromInt64(3),
		values.NewBigFloatFromFloat64(4.0),
	)
	c.Assert(mixed.IsExact(), qt.IsFalse)

	// ToInexact
	inexactFromExact := exact.ToInexact()
	c.Assert(inexactFromExact.(*values.BigComplex).IsExact(), qt.IsFalse)

	// ToExact
	exactFromInexact, err := inexact.ToExact()
	c.Assert(err, qt.IsNil)
	c.Assert(exactFromInexact.(*values.BigComplex).IsExact(), qt.IsTrue)
}

// TestBigComplex_ToExactFractionalParts tests H4 regression:
// BigComplex.ToExact() must convert fractional BigFloat parts to Rational,
// not truncate to BigInteger.
//
// Architectural Review H4: toExactPart converts BigFloat to integer by
// truncation instead of Rational. (exact 1.5+0i) produces 1 instead of 3/2.
func TestBigComplex_ToExactFractionalParts(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name             string
		real             float64
		imag             float64
		simplifiesToReal bool   // If true, result should be a Number (not BigComplex)
		wantRealType     string // "Rational", "BigInteger", or "Integer"
		wantRealValue    float64
		wantImagType     string
		wantImagValue    float64
	}{
		{
			name:             "fractional real, zero imag - simplifies to real",
			real:             1.5,
			imag:             0.0,
			simplifiesToReal: true,
			wantRealType:     "Rational",
			wantRealValue:    1.5,
		},
		{
			name:             "zero real, fractional imag - stays complex",
			real:             0.0,
			imag:             2.5,
			simplifiesToReal: false,
			wantRealType:     "zero",
			wantRealValue:    0.0,
			wantImagType:     "Rational",
			wantImagValue:    2.5,
		},
		{
			name:             "both parts fractional",
			real:             1.5,
			imag:             2.5,
			simplifiesToReal: false,
			wantRealType:     "Rational",
			wantRealValue:    1.5,
			wantImagType:     "Rational",
			wantImagValue:    2.5,
		},
		{
			name:             "integer-valued float and fractional",
			real:             3.0,
			imag:             2.5,
			simplifiesToReal: false,
			wantRealType:     "Integer", // 3.0 simplifies to integer
			wantRealValue:    3.0,
			wantImagType:     "Rational",
			wantImagValue:    2.5,
		},
		{
			name:             "fractional real, integer-valued imag",
			real:             1.5,
			imag:             4.0,
			simplifiesToReal: false,
			wantRealType:     "Rational",
			wantRealValue:    1.5,
			wantImagType:     "Integer",
			wantImagValue:    4.0,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			bc := values.NewBigComplexFromBigFloats(
				values.NewBigFloatFromFloat64(tc.real),
				values.NewBigFloatFromFloat64(tc.imag),
			)
			exact, err := bc.ToExact()
			c.Assert(err, qt.IsNil)
			c.Assert(exact.IsExact(), qt.IsTrue)

			// If result simplifies to real (imag == 0), check the real value directly
			if tc.simplifiesToReal {
				switch tc.wantRealType {
				case "Rational":
					rat, ok := exact.(*values.Rational)
					c.Assert(ok, qt.IsTrue, qt.Commentf("Result should be Rational, got %T: %v", exact, exact.SchemeString()))
					c.Assert(rat.Float64Truncated(), qt.Equals, tc.wantRealValue)
				case "Integer":
					switch v := exact.(type) {
					case *values.Integer:
						c.Assert(float64(v.Value), qt.Equals, tc.wantRealValue)
					case *values.BigInteger:
						c.Assert(float64(v.Int64()), qt.Equals, tc.wantRealValue)
					default:
						t.Fatalf("Result should be Integer or BigInteger, got %T: %v", exact, exact.SchemeString())
					}
				}
				return
			}

			// Otherwise, result should be BigComplex
			bcExact, ok := exact.(*values.BigComplex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("Result should be BigComplex, got %T: %v", exact, exact.SchemeString()))

			// Check real part
			realPart := bcExact.Real()
			switch tc.wantRealType {
			case "zero":
				c.Assert(realPart.IsZero(), qt.IsTrue, qt.Commentf("Real part should be zero"))
			case "Rational":
				rat, ok := realPart.(*values.Rational)
				c.Assert(ok, qt.IsTrue, qt.Commentf("Real part should be Rational, got %T: %v", realPart, realPart.SchemeString()))
				c.Assert(rat.Float64Truncated(), qt.Equals, tc.wantRealValue)
			case "Integer":
				// Could be Integer or BigInteger depending on value
				switch v := realPart.(type) {
				case *values.Integer:
					c.Assert(float64(v.Value), qt.Equals, tc.wantRealValue)
				case *values.BigInteger:
					c.Assert(float64(v.Int64()), qt.Equals, tc.wantRealValue)
				default:
					t.Fatalf("Real part should be Integer or BigInteger, got %T: %v", realPart, realPart.SchemeString())
				}
			}

			// Check imaginary part
			imagPart := bcExact.Imag()
			switch tc.wantImagType {
			case "zero":
				c.Assert(imagPart.IsZero(), qt.IsTrue, qt.Commentf("Imaginary part should be zero"))
			case "Rational":
				rat, ok := imagPart.(*values.Rational)
				c.Assert(ok, qt.IsTrue, qt.Commentf("Imaginary part should be Rational, got %T: %v", imagPart, imagPart.SchemeString()))
				c.Assert(rat.Float64Truncated(), qt.Equals, tc.wantImagValue)
			case "Integer":
				switch v := imagPart.(type) {
				case *values.Integer:
					c.Assert(float64(v.Value), qt.Equals, tc.wantImagValue)
				case *values.BigInteger:
					c.Assert(float64(v.Int64()), qt.Equals, tc.wantImagValue)
				default:
					t.Fatalf("Imaginary part should be Integer or BigInteger, got %T: %v", imagPart, imagPart.SchemeString())
				}
			}
		})
	}
}

func TestBigComplex_Simplification(t *testing.T) {
	c := qt.New(t)

	// When imaginary part becomes 0, should simplify to real number
	bc1 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(2),
	)
	bc2 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(0),
		values.NewBigIntegerFromInt64(2),
	)

	// (3+2i) - (0+2i) = 3 (should simplify to BigInteger)
	result := bc1.Subtract(bc2)
	_, isBigInt := result.(*values.BigInteger)
	c.Assert(isBigInt, qt.IsTrue)
	c.Assert(result.(*values.BigInteger).Int64(), qt.Equals, int64(3))
}

func TestBigComplex_MagnitudePhase(t *testing.T) {
	c := qt.New(t)

	// 3+4i has magnitude 5 and phase atan2(4, 3)
	bc := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)

	mag := bc.Magnitude()
	c.Assert(mag.Float64Truncated(), qt.Equals, 5.0)

	phase := bc.Phase()
	expected := math.Atan2(4, 3)
	c.Assert(math.Abs(phase.Float64Truncated()-expected) < 0.0001, qt.IsTrue)

	// Conjugate: (3+4i)* = 3-4i
	conj := bc.Conjugate()
	c.Assert(conj.Real().(*values.BigInteger).Int64(), qt.Equals, int64(3))
	c.Assert(conj.Imag().(*values.BigInteger).Int64(), qt.Equals, int64(-4))
}

func TestBigComplex_PhaseHugeComponentsNoOverflow(t *testing.T) {
	c := qt.New(t)

	// 1e400 + 1e401 i : both components exceed float64 range, but the ratio is
	// finite. True angle = atan2(1e401, 1e400) = atan(10) ≈ 1.4711. The prior
	// float64-truncating Phase() returned π/4 (both saturated to +Inf).
	z := values.NewBigComplexFromBigFloats(
		values.NewBigFloatFromString("1e400"),
		values.NewBigFloatFromString("1e401"),
	)

	got, _ := z.Phase().BigFloatValue().Float64()
	c.Assert(math.Abs(got-math.Atan(10)) < 1e-12, qt.IsTrue)
}

func TestBigComplex_EqualTo(t *testing.T) {
	c := qt.New(t)

	bc1 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	bc2 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	bc3 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(1),
		values.NewBigIntegerFromInt64(2),
	)

	c.Assert(bc1.EqualTo(bc2), qt.IsTrue)
	c.Assert(bc1.EqualTo(bc3), qt.IsFalse)

	// NOT equal to a regular Complex with the "same" values, and the reason is
	// EXACTNESS, not precision: bc1 has BigInteger components, so it is an EXACT
	// complex, while Complex is float64-backed and therefore inexact. R7RS 6.1:
	// eqv? is #f when "one of obj1 and obj2 is an exact number but the other is an
	// inexact number", and equal? returns the same as eqv? on numbers.
	//
	// This asserted IsTrue until equal? was made to agree with eqv?. It was the same
	// bug as (equal? 3 3.0) answering #t — just wearing a complex costume.
	cplx := values.NewComplexFromParts(3.0, 4.0)
	c.Assert(bc1.EqualTo(cplx), qt.IsFalse)
	c.Assert(cplx.EqualTo(bc1), qt.IsFalse)

	// Not equal to different type
	c.Assert(bc1.EqualTo(values.NewInteger(3)), qt.IsFalse)
}

// TestBigComplex_EqualTo_StrictBeyondFloat64 guards the BigComplex<->Complex
// comparison against the pre-existing bug where BigComplex.EqualTo truncated its
// own components to float64 before comparing, so two values differing only below
// float64 precision compared equal. The real part 1 + 2^-60 rounds to exactly
// 1.0 in float64 (float64 ULP near 1 is 2^-52), so the truncating comparison
// wrongly returned true. The fix promotes the Complex components to big.Float
// and compares at full precision. See TODO site (5).
func TestBigComplex_EqualTo_StrictBeyondFloat64(t *testing.T) {
	c := qt.New(t)

	// realNear = (2^60 + 1) / 2^60 = 1 + 2^-60, indistinguishable from 1.0 in
	// float64 but distinct as an exact rational.
	den := new(big.Int).Lsh(big.NewInt(1), 60)
	num := new(big.Int).Add(den, big.NewInt(1))
	realNear := values.NewRationalFromBigInt(num, den)
	bcNear := values.NewBigComplex(realNear, values.NewBigIntegerFromInt64(0))
	cplxOne := values.NewComplexFromParts(1.0, 0.0)

	// Differ below float64 precision: must be unequal in both directions.
	c.Assert(bcNear.EqualTo(cplxOne), qt.IsFalse)
	c.Assert(cplxOne.EqualTo(bcNear), qt.IsFalse)

	// An exactly-representable value is STILL not equal — and this is the assertion
	// that changed. bcOne has BigInteger components (exact); cplxOne is float64-backed
	// (inexact). R7RS 6.1 makes an exact and an inexact number unequal under eqv?,
	// whatever their values, and equal? follows eqv? on numbers.
	//
	// The guard this test exists for is UNAFFECTED and is strengthened: the point was
	// that BigComplex must not truncate its components to float64 before comparing.
	// It now does not compare across the exact/inexact boundary at all.
	bcOne := values.NewBigComplex(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(0))
	c.Assert(bcOne.EqualTo(cplxOne), qt.IsFalse)
	c.Assert(cplxOne.EqualTo(bcOne), qt.IsFalse)

	// A huge but finite BigComplex component (>float64 range) must NOT compare
	// equal to an infinite Complex — regression guard for the Inf branch that
	// previously truncated the big component to +Inf before comparing.
	big10e400 := new(big.Int).Exp(big.NewInt(10), big.NewInt(400), nil)
	bcHuge := values.NewBigComplexFromBigIntegers(values.NewBigInteger(big10e400), values.NewBigIntegerFromInt64(0))
	cplxInf := values.NewComplexFromParts(math.Inf(1), 0.0)
	c.Assert(bcHuge.EqualTo(cplxInf), qt.IsFalse)
	c.Assert(cplxInf.EqualTo(bcHuge), qt.IsFalse)

	// NaN never equals anything, in either direction.
	cplxNaN := values.NewComplexFromParts(math.NaN(), 0.0)
	c.Assert(bcOne.EqualTo(cplxNaN), qt.IsFalse)
	c.Assert(cplxNaN.EqualTo(bcOne), qt.IsFalse)
}

// TestBigComplex_Sqrt guards (*BigComplex).Sqrt across every branch of the
// closed form: the four quadrant sign combinations of (a, b) and the two b==0
// axis cases (including the R7RS §6.2.6 branch cut that maps the negative real
// axis to the positive imaginary axis). The b==0 cases are unreachable from
// Scheme because make-rectangular collapses a zero-imaginary value to a real,
// so only a values-level test exercises them. See TODO site (2).
func TestBigComplex_Sqrt(t *testing.T) {
	c := qt.New(t)

	// z given as (real, imag) integer parts; sqrt(z) expected as (wantRe, wantIm).
	// Each squares back to z: (3±4i)^2 = -7±24i, (4±3i)^2 = 7±24i.
	tcs := []struct {
		name           string
		re, im         int64
		wantRe, wantIm float64
	}{
		{"a>=0 b>0: 7+24i -> 4+3i", 7, 24, 4, 3},
		{"a>=0 b<0: 7-24i -> 4-3i", 7, -24, 4, -3},
		{"a<0 b>0: -7+24i -> 3+4i", -7, 24, 3, 4},
		{"a<0 b<0: -7-24i -> 3-4i", -7, -24, 3, -4},
		{"b==0 a>=0: 9+0i -> 3+0i", 9, 0, 3, 0},
		// Branch cut: sqrt of a negative real is +i*sqrt(|a|), not -i.
		{"b==0 a<0 (branch cut): -4+0i -> 0+2i", -4, 0, 0, 2},
		{"zero: 0+0i -> 0+0i", 0, 0, 0, 0},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			z := values.NewBigComplexFromBigIntegers(
				values.NewBigIntegerFromInt64(tc.re),
				values.NewBigIntegerFromInt64(tc.im),
			)
			root := z.Sqrt()
			c.Assert(math.Abs(root.RealAsBigFloat().Float64Truncated()-tc.wantRe) < 1e-9, qt.IsTrue)
			c.Assert(math.Abs(root.ImagAsBigFloat().Float64Truncated()-tc.wantIm) < 1e-9, qt.IsTrue)
		})
	}

	// Components beyond float64 range: result stays finite (no +inf overflow),
	// and squares back to the input (relative error tiny at big precision).
	big10e400 := new(big.Int).Exp(big.NewInt(10), big.NewInt(400), nil)
	huge := values.NewBigComplexFromBigIntegers(
		values.NewBigInteger(big10e400),
		values.NewBigInteger(big10e400),
	)
	hugeRoot := huge.Sqrt()
	c.Assert(math.IsInf(hugeRoot.RealAsBigFloat().Float64Truncated(), 0), qt.IsFalse)
	c.Assert(math.IsInf(hugeRoot.ImagAsBigFloat().Float64Truncated(), 0), qt.IsFalse)
	// Value check at a scale where Float64Truncated would overflow: sqrt(z)^2
	// must reproduce z. Assert per-component relative error is negligible, so a
	// wrong-but-finite result cannot pass (unlike the !IsInf checks above).
	sq := hugeRoot.Multiply(hugeRoot).(*values.BigComplex)
	relErr := func(got, want *values.BigFloat) *big.Float {
		d := new(big.Float).Sub(got.BigFloatValue(), want.BigFloatValue())
		d.Abs(d)
		return d.Quo(d, new(big.Float).Abs(want.BigFloatValue()))
	}
	threshold := big.NewFloat(1e-50)
	c.Assert(relErr(sq.RealAsBigFloat(), huge.RealAsBigFloat()).Cmp(threshold) < 0, qt.IsTrue)
	c.Assert(relErr(sq.ImagAsBigFloat(), huge.ImagAsBigFloat()).Cmp(threshold) < 0, qt.IsTrue)
}

// TestComplex_EqualTo_SymmetricWithBigComplex pins R7RS equality symmetry across
// the Complex/BigComplex kinds: (equal? a b) must not flip with operand order.
// Complex.EqualTo once matched only *Complex and returned false for any
// *BigComplex, while BigComplex.EqualTo cross-compared to *Complex — so the two
// directions disagreed.
//
// Symmetry is now STRUCTURAL rather than asserted: both directions delegate to the
// same values.EqvNumber, which cannot be asymmetric because it is one function. The
// test is kept as a guard against anyone re-introducing a hand-written cross-kind
// arm in one type's EqualTo and not the other's — the exact way the original bug
// arose.
//
// The expected ANSWER changed with exactness contagion: a BigComplex with
// BigInteger components is an EXACT complex, and a Complex is float64-backed and
// so inexact. R7RS §6.1 makes an exact and an inexact number unequal under eqv?,
// and equal? returns the same as eqv? on numbers. So both directions are now #f —
// still symmetric, which is what this test is for.
func TestComplex_EqualTo_SymmetricWithBigComplex(t *testing.T) {
	c := qt.New(t)

	bc := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	cplx := values.NewComplexFromParts(3.0, 4.0)

	// Numerically equal, but exact vs inexact: unequal, and symmetrically so.
	c.Assert(bc.EqualTo(cplx), qt.IsFalse)
	c.Assert(cplx.EqualTo(bc), qt.IsFalse)

	// Unequal values: both directions must agree.
	bcDiff := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(1),
		values.NewBigIntegerFromInt64(2),
	)
	c.Assert(bcDiff.EqualTo(cplx), qt.IsFalse)
	c.Assert(cplx.EqualTo(bcDiff), qt.IsFalse)

	// Same kind, same value: equal in both directions. The symmetry guarantee is not
	// vacuous — it still has a true case to be symmetric about.
	cplxSame := values.NewComplexFromParts(3.0, 4.0)
	c.Assert(cplx.EqualTo(cplxSame), qt.IsTrue)
	c.Assert(cplxSame.EqualTo(cplx), qt.IsTrue)

	bcSame := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	c.Assert(bc.EqualTo(bcSame), qt.IsTrue)
	c.Assert(bcSame.EqualTo(bc), qt.IsTrue)
}

func TestBigComplex_SchemeString(t *testing.T) {
	c := qt.New(t)

	// Positive imaginary
	bc1 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	c.Assert(bc1.SchemeString(), qt.Equals, "3+4i")

	// Negative imaginary
	bc2 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(-4),
	)
	c.Assert(bc2.SchemeString(), qt.Equals, "3-4i")

	// With BigFloat parts
	bc3 := values.NewBigComplexFromBigFloats(
		values.NewBigFloatFromFloat64(1.5),
		values.NewBigFloatFromFloat64(2.5),
	)
	str := bc3.SchemeString()
	c.Assert(str, qt.Contains, "1.5")
	c.Assert(str, qt.Contains, "2.5")
	c.Assert(str, qt.Contains, "i")
}

func TestBigComplex_Properties(t *testing.T) {
	c := qt.New(t)

	bc := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	zero := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(0),
		values.NewBigIntegerFromInt64(0),
	)
	realOnly := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(5),
		values.NewBigIntegerFromInt64(0),
	)

	c.Assert(bc.IsZero(), qt.IsFalse)
	c.Assert(zero.IsZero(), qt.IsTrue)
	c.Assert(bc.IsReal(), qt.IsFalse)
	c.Assert(realOnly.IsReal(), qt.IsTrue)
	c.Assert(bc.IsVoid(), qt.IsFalse)
	c.Assert((*values.BigComplex)(nil).IsVoid(), qt.IsTrue)
}

func TestBigComplex_Comparison(t *testing.T) {
	c := qt.New(t)

	bc1 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	bc2 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(5),
		values.NewBigIntegerFromInt64(1),
	)
	bc3 := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(100),
	)

	// Compare by real parts only
	c.Assert(bc1.LessThan(bc2), qt.IsTrue)  // 3 < 5
	c.Assert(bc2.LessThan(bc1), qt.IsFalse) // 5 > 3
	c.Assert(bc1.LessThan(bc3), qt.IsFalse) // 3 == 3 (imaginary ignored)

	// Compare with other numeric types
	c.Assert(bc1.Compare(values.NewBigIntegerFromInt64(5)), qt.Equals, -1) // 3 < 5
	c.Assert(bc1.Compare(values.NewBigIntegerFromInt64(2)), qt.Equals, 1)  // 3 > 2
	c.Assert(bc1.Compare(values.NewBigIntegerFromInt64(3)), qt.Equals, 0)  // 3 == 3
}

func TestBigComplex_ZeroOptimizations(t *testing.T) {
	c := qt.New(t)

	bc := values.NewBigComplexFromBigIntegers(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)
	zero := values.NewBigIntegerFromInt64(0)

	// Add zero returns equal value (not necessarily same pointer)
	result := bc.Add(zero)
	c.Assert(result, valuestest.SchemeEquals, bc)

	// Subtract zero returns equal value (not necessarily same pointer)
	result = bc.Subtract(zero)
	c.Assert(result, valuestest.SchemeEquals, bc)

	// Multiply by zero returns zero
	result = bc.Multiply(zero)
	c.Assert(result.IsZero(), qt.IsTrue)
}

func TestBigComplex_RationalParts(t *testing.T) {
	c := qt.New(t)

	// Create BigComplex with Rational parts (exact)
	bc := values.NewBigComplex(
		values.NewRational(3, 2), // 3/2
		values.NewRational(1, 2), // 1/2
	)

	// Should be exact
	c.Assert(bc.IsExact(), qt.IsTrue)
	c.Assert(bc.Real().(*values.Rational).Float64Truncated(), qt.Equals, 1.5)
	c.Assert(bc.Imag().(*values.Rational).Float64Truncated(), qt.Equals, 0.5)

	// SchemeString should format correctly
	c.Assert(bc.SchemeString(), qt.Equals, "3/2+1/2i")

	// Negative imaginary
	bcNeg := values.NewBigComplex(
		values.NewRational(3, 2),
		values.NewRational(-1, 2),
	)
	c.Assert(bcNeg.SchemeString(), qt.Equals, "3/2-1/2i")
}

// TestBigComplex_RationalArithmetic asserts the VALUE of each result, not its
// concrete Go type.
//
// It used to cast every component to *values.Rational — including the ones its own
// comments call 2 and 1. That only held because rational arithmetic returned
// non-canonical denominator-1 rationals; once the tower canonicalizes (an exact
// value has ONE representation), an integral component descends to *Integer and the
// cast panics. An assertion that pins the representation of a result cannot notice
// the representation being wrong, which is exactly what it was hiding here.
func TestBigComplex_RationalArithmetic(t *testing.T) {
	c := qt.New(t)

	bc1 := values.NewBigComplex(
		values.NewRational(3, 2), // 3/2
		values.NewRational(1, 2), // 1/2
	)
	bc2 := values.NewBigComplex(
		values.NewRational(1, 2), // 1/2
		values.NewRational(1, 4), // 1/4
	)

	// Add: (3/2 + 1/2i) + (1/2 + 1/4i) = (2 + 3/4i). The real part is integral, so
	// it is an exact integer, not a 2/1 rational.
	sum := bc1.Add(bc2)
	c.Assert(sum.(*values.BigComplex).IsExact(), qt.IsTrue)
	c.Assert(sum.SchemeString(), qt.Equals, "2+3/4i")

	// Subtract: (3/2 + 1/2i) - (1/2 + 1/4i) = (1 + 1/4i)
	diff := bc1.Subtract(bc2)
	c.Assert(diff.(*values.BigComplex).IsExact(), qt.IsTrue)
	c.Assert(diff.SchemeString(), qt.Equals, "1+1/4i")

	// Multiply: (3/2 + 1/2i) * (1/2 + 1/4i) = (3/4 - 1/8) + (3/8 + 1/4)i = 5/8 + 5/8i.
	// Neither component is integral, so both stay rational.
	prod := bc1.Multiply(bc2)
	c.Assert(prod.(*values.BigComplex).IsExact(), qt.IsTrue)
	c.Assert(prod.SchemeString(), qt.Equals, "5/8+5/8i")
	_, realIsRational := prod.(*values.BigComplex).Real().(*values.Rational)
	c.Assert(realIsRational, qt.IsTrue,
		qt.Commentf("a non-integral exact component must STAY a *Rational; "+
			"canonicalization descends integral values only"))
}

func TestBigComplex_RationalWithScalar(t *testing.T) {
	c := qt.New(t)

	bc := values.NewBigComplex(
		values.NewRational(3, 2), // 3/2
		values.NewRational(1, 2), // 1/2
	)

	// Add Rational: (3/2 + 1/2i) + 1/4 = (7/4 + 1/2i) — both components fractional.
	sum := bc.Add(values.NewRational(1, 4))
	c.Assert(sum.(*values.BigComplex).IsExact(), qt.IsTrue)
	c.Assert(sum.SchemeString(), qt.Equals, "7/4+1/2i")

	// Multiply Rational: (3/2 + 1/2i) * 2 = (3 + 1i) — both components integral.
	prod := bc.Multiply(values.NewRational(2, 1))
	c.Assert(prod.(*values.BigComplex).IsExact(), qt.IsTrue)
	c.Assert(prod.SchemeString(), qt.Equals, "3+1i")

	// Divide by Rational: (3/2 + 1/2i) / (1/2) = (3 + 1i)
	quot, err := bc.Divide(values.NewRational(1, 2))
	c.Assert(err, qt.IsNil)
	c.Assert(quot.(*values.BigComplex).IsExact(), qt.IsTrue)
	c.Assert(quot.SchemeString(), qt.Equals, "3+1i")
}

func TestBigComplex_RationalExactnessContagion(t *testing.T) {
	c := qt.New(t)

	exact := values.NewBigComplex(
		values.NewRational(3, 2),
		values.NewRational(1, 2),
	)

	// Adding a Float makes it inexact
	sumFloat := exact.Add(values.NewFloat(1.0))
	c.Assert(sumFloat.(*values.BigComplex).IsExact(), qt.IsFalse)

	// Adding a BigFloat makes it inexact
	sumBigFloat := exact.Add(values.NewBigFloatFromFloat64(1.0))
	c.Assert(sumBigFloat.(*values.BigComplex).IsExact(), qt.IsFalse)

	// Adding an Integer keeps it exact (Integer promotes to BigInteger)
	sumInt := exact.Add(values.NewInteger(1))
	c.Assert(sumInt.(*values.BigComplex).IsExact(), qt.IsTrue)

	// Adding a BigInteger keeps it exact
	sumBigInt := exact.Add(values.NewBigIntegerFromInt64(1))
	c.Assert(sumBigInt.(*values.BigComplex).IsExact(), qt.IsTrue)

	// Adding another exact Rational keeps it exact
	sumRat := exact.Add(values.NewRational(1, 4))
	c.Assert(sumRat.(*values.BigComplex).IsExact(), qt.IsTrue)
}

func TestBigComplex_MixedRationalBigInteger(t *testing.T) {
	c := qt.New(t)

	// Mixed: Rational real, BigInteger imag
	bc := values.NewBigComplex(
		values.NewRational(3, 2),
		values.NewBigIntegerFromInt64(4),
	)

	// Should still be exact
	c.Assert(bc.IsExact(), qt.IsTrue)

	// Add another mixed
	bc2 := values.NewBigComplex(
		values.NewBigIntegerFromInt64(1),
		values.NewRational(1, 2),
	)
	sum := bc.Add(bc2)
	c.Assert(sum.(*values.BigComplex).IsExact(), qt.IsTrue)
}

func TestBigComplex_HashCode(t *testing.T) {
	c := qt.New(t)

	// Stability: same value produces same hash.
	bc1 := values.NewBigComplexFromBigFloats(
		values.NewBigFloatFromFloat64(3.0),
		values.NewBigFloatFromFloat64(4.0),
	)
	bc2 := values.NewBigComplexFromBigFloats(
		values.NewBigFloatFromFloat64(3.0),
		values.NewBigFloatFromFloat64(4.0),
	)
	c.Assert(bc1.HashCode(), qt.Equals, bc2.HashCode())

	// Exact (BigInteger) parts: stability.
	be1 := values.NewBigComplex(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(2))
	be2 := values.NewBigComplex(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(2))
	c.Assert(be1.HashCode(), qt.Equals, be2.HashCode())

	// Distinctness: different values hash differently.
	h1 := values.NewBigComplexFromBigFloats(values.NewBigFloatFromFloat64(1), values.NewBigFloatFromFloat64(2)).HashCode()
	h2 := values.NewBigComplexFromBigFloats(values.NewBigFloatFromFloat64(2), values.NewBigFloatFromFloat64(1)).HashCode()
	c.Assert(h1, qt.Not(qt.Equals), h2)
}

// TestBigComplex_FloatInfDispatch is the regression test for issue #362.
// Float(Inf/NaN) combined with BigComplex must preserve the imaginary part.
func TestBigComplex_FloatInfDispatch(t *testing.T) {
	c := qt.New(t)

	// BigComplex(3+4i) with exact BigInteger parts.
	bc := values.NewBigComplex(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)

	tcs := []struct {
		name     string
		result   values.Number
		realInf  bool
		realNaN  bool
		imagZero bool
	}{
		{
			name:     "+inf.0 + (3+4i)",
			result:   values.NewFloat(math.Inf(1)).Add(bc),
			realInf:  true,
			realNaN:  false,
			imagZero: false,
		},
		{
			name:     "(3+4i) + +inf.0",
			result:   bc.Add(values.NewFloat(math.Inf(1))),
			realInf:  true,
			realNaN:  false,
			imagZero: false,
		},
		{
			name:     "-inf.0 + (3+4i)",
			result:   values.NewFloat(math.Inf(-1)).Add(bc),
			realInf:  true,
			realNaN:  false,
			imagZero: false,
		},
		{
			name:     "+nan.0 + (3+4i)",
			result:   values.NewFloat(math.NaN()).Add(bc),
			realInf:  false,
			realNaN:  true,
			imagZero: false,
		},
		{
			name:     "+inf.0 - (3+4i)",
			result:   values.NewFloat(math.Inf(1)).Subtract(bc),
			realInf:  true,
			realNaN:  false,
			imagZero: false,
		},
		{
			name:     "(3+4i) - +inf.0",
			result:   bc.Subtract(values.NewFloat(math.Inf(1))),
			realInf:  true,
			realNaN:  false,
			imagZero: false,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// Result must be *BigComplex (not *Float or *Complex).
			bResult, ok := tc.result.(*values.BigComplex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected *BigComplex, got %T", tc.result))

			realPart := bResult.Real()
			imagPart := bResult.Imag()

			// Imaginary part must not be zero (the original bug: imaginary lost).
			c.Assert(imagPart.IsZero(), qt.IsFalse)

			if tc.realInf {
				c.Assert(realPart.IsFinite(), qt.IsFalse)
				c.Assert(realPart.IsNaN(), qt.IsFalse)
			}
			if tc.realNaN {
				c.Assert(realPart.IsNaN(), qt.IsTrue)
			}
		})
	}
}

func TestBigComplex_InfNaNPredicates(t *testing.T) {
	c := qt.New(t)

	infReal := values.NewBigComplex(
		values.NewBigFloat(new(big.Float).SetInf(false)),
		values.NewBigIntegerFromInt64(4),
	)
	nanReal := values.NewBigComplex(
		values.NewBigFloatNaN(),
		values.NewBigIntegerFromInt64(4),
	)
	finite := values.NewBigComplex(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)

	// BigComplex with Inf real part.
	c.Assert(infReal.IsFinite(), qt.IsFalse)
	c.Assert(infReal.IsNaN(), qt.IsFalse)
	c.Assert(infReal.IsRational(), qt.IsFalse)

	// BigComplex with NaN real part.
	c.Assert(nanReal.IsFinite(), qt.IsFalse)
	c.Assert(nanReal.IsNaN(), qt.IsTrue)
	c.Assert(nanReal.IsRational(), qt.IsFalse)

	// Ordinary finite BigComplex.
	c.Assert(finite.IsFinite(), qt.IsTrue)
	c.Assert(finite.IsNaN(), qt.IsFalse)
}

// TestBigComplex_NaNEquality verifies IEEE 754: NaN is never equal to
// anything, including itself. This is a regression test for a bug where
// BigComplex.EqualTo fell through to Compare (which returns 0 for NaN),
// causing NaN == NaN to return true.
func TestBigComplex_NaNEquality(t *testing.T) {
	c := qt.New(t)

	nanReal := values.NewBigComplex(
		values.NewBigFloatNaN(),
		values.NewBigIntegerFromInt64(4),
	)
	nanImag := values.NewBigComplex(
		values.NewBigIntegerFromInt64(3),
		values.NewBigFloatNaN(),
	)
	nanBoth := values.NewBigComplex(
		values.NewBigFloatNaN(),
		values.NewBigFloatNaN(),
	)
	finite := values.NewBigComplex(
		values.NewBigIntegerFromInt64(3),
		values.NewBigIntegerFromInt64(4),
	)

	// Reflexive: EqualTo backs the equivalence predicates (eqv?/equal?), not numeric
	// =, and an equivalence relation holds between an object and itself whatever its
	// components carry. R7RS §6.1 requires eq? ⊆ eqv? ⊆ equal?. See
	// TestFloat_NaNEquality for the full argument.
	c.Assert(nanReal.EqualTo(nanReal), qt.IsTrue)
	c.Assert(nanImag.EqualTo(nanImag), qt.IsTrue)
	c.Assert(nanBoth.EqualTo(nanBoth), qt.IsTrue)

	// NaN is equivalent to no finite value: a NaN component makes the whole complex
	// INEXACT, and finite here is exact (BigInteger parts), so exactness alone
	// separates them — before the NaN rule is even consulted.
	c.Assert(nanReal.EqualTo(finite), qt.IsFalse)
	c.Assert(finite.EqualTo(nanReal), qt.IsFalse)

	// Two distinct NaN-carrying BigComplexes ARE equivalent, component-wise: the NaN
	// reals are eqv? (Chez-matching), and the exact imaginary parts are equal. See
	// TestFloat_NaNEquality.
	nanReal2 := values.NewBigComplex(
		values.NewBigFloatNaN(),
		values.NewBigIntegerFromInt64(4),
	)
	c.Assert(nanReal.EqualTo(nanReal2), qt.IsTrue)
	c.Assert(nanReal2.EqualTo(nanReal), qt.IsTrue)

	// A NaN in the REAL slot is still not the same value as a NaN in the IMAGINARY
	// slot — the rule is component-wise, not "contains a NaN somewhere".
	c.Assert(nanReal.EqualTo(nanImag), qt.IsFalse)
	c.Assert(nanBoth.EqualTo(nanReal), qt.IsFalse)
}
