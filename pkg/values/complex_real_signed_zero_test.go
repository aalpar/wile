package values

import (
	"math"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestComplexTimesRealPreservesSignedZero pins the sign of a zero component through
// arithmetic with a REAL operand.
//
// A real number has NO imaginary component. Promotion used to manufacture one -- an
// INEXACT 0.0 -- and IEEE then ate the sign: -0.0 + 0.0 is +0.0, and -0.0*r + 0.0*s
// is +0.0. So (* 5.0-0.0i 2.0) came back 10.0+0.0i.
//
// The EXACT reals were always correct, and that is the tell: their promoted imaginary
// part is an EXACT zero, so the exact-zero identity returns the operand untouched and
// the annihilation rule kills the cross terms. The rule was already doing the job for
// half the inputs. A real's imaginary part is a mathematical zero, not an IEEE one.
//
//	petite -q <<< '(display (list (* 5.0-0.0i 2.0) (+ 5.0-0.0i 2.0) (/ -0.0+5.0i 2.0)))'
//	racket   -e '(display (list (* 5.0-0.0i 2.0) (+ 5.0-0.0i 2.0) (/ -0.0+5.0i 2.0)))'
//	# both => (10.0-0.0i 7.0-0.0i -0.0+2.5i)
func TestComplexTimesRealPreservesSignedZero(t *testing.T) {
	negZeroImag := NewComplex(complex(5, math.Copysign(0, -1))) // 5.0-0.0i
	negZeroReal := NewComplex(complex(math.Copysign(0, -1), 5)) // -0.0+5.0i

	tcs := []struct {
		name string
		call func() (Number, error)
		want string
	}{
		// MULTIPLY: the imaginary part is SCALED, so its sign survives.
		{"complex * inexact real", func() (Number, error) {
			return negZeroImag.Multiply(NewFloat(2.0)), nil
		}, "10.0-0.0i"},
		{"inexact real * complex (other order)", func() (Number, error) {
			return NewFloat(2.0).Multiply(negZeroImag), nil
		}, "10.0-0.0i"},

		// ADD/SUBTRACT: the imaginary part is UNTOUCHED. A real operand touches only
		// the real part; it has nothing to add to the imaginary one.
		{"complex + inexact real", func() (Number, error) {
			return negZeroImag.Add(NewFloat(2.0)), nil
		}, "7.0-0.0i"},
		{"inexact real + complex (other order)", func() (Number, error) {
			return NewFloat(2.0).Add(negZeroImag), nil
		}, "7.0-0.0i"},
		{"complex - inexact real", func() (Number, error) {
			return negZeroImag.Subtract(NewFloat(2.0)), nil
		}, "3.0-0.0i"},

		// DIVIDE: the imaginary part is scaled. And the REAL part's sign survives too.
		{"complex / inexact real", func() (Number, error) {
			return negZeroReal.Divide(NewFloat(2.0))
		}, "-0.0+2.5i"},
		{"inexact real / complex (other order)", func() (Number, error) {
			return NewFloat(2.0).Divide(negZeroReal)
		}, "-0.0-0.4i"},

		// The EXACT reals must KEEP working -- they were never broken.
		{"complex * exact real", func() (Number, error) {
			return negZeroImag.Multiply(NewInteger(2)), nil
		}, "10.0-0.0i"},
		{"complex + exact real", func() (Number, error) {
			return negZeroImag.Add(NewInteger(0)), nil
		}, "5.0-0.0i"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			got, err := tc.call()
			c.Assert(err, qt.IsNil)
			c.Assert(got.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestRealPromotionKeepsContagion is the guard on the fix, and it pins the exact trap
// a reviewer flagged when this approach was first proposed: promoting a real with an
// EXACT zero imaginary part must not leak that exactness into the RESULT.
//
// (+ 2.0 1+1i) is 3.0+1.0i. If the promoted 2.0 carried an exact-0 imaginary part and
// nothing re-imposed contagion, the imaginary part would come back EXACT -- 3.0+1i --
// an exact component inside an inexact number, which is the very defect
// contagionOverParts exists to prevent. Both oracles: 3.0+1.0i, and exact? is #f.
func TestRealPromotionKeepsContagion(t *testing.T) {
	exactComplex := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(1), NewBigIntegerFromInt64(1)) // 1+1i, exact

	tcs := []struct {
		name string
		got  Number
		want string
	}{
		{"inexact real + exact complex", NewFloat(2.0).Add(exactComplex), "3.0+1.0i"},
		{"exact complex + inexact real", exactComplex.Add(NewFloat(2.0)), "3.0+1.0i"},
		{"inexact real * exact complex", NewFloat(2.0).Multiply(exactComplex), "2.0+2.0i"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(tc.got.SchemeString(), qt.Equals, tc.want)
			c.Assert(tc.got.IsExact(), qt.IsFalse,
				qt.Commentf("an inexact operand makes the whole result inexact"))

			bc, ok := tc.got.(*BigComplex)
			if !ok {
				return
			}
			c.Assert(bc.Real().IsExact(), qt.IsFalse,
				qt.Commentf("EXACT real part %s inside an inexact number", bc.Real().SchemeString()))
			c.Assert(bc.Imag().IsExact(), qt.IsFalse,
				qt.Commentf("EXACT imag part %s inside an inexact number -- the promoted "+
					"exact zero leaked", bc.Imag().SchemeString()))
		})
	}
}

// TestRealDivideByComplexKeepsRange pins the NUMERIC RANGE of a real divided by a
// complex -- the property the signed-zero tests above structurally cannot see, since
// every one of them uses small magnitudes. A formula that is correct on the sign and
// catastrophic on the range passes all of them, and one did.
//
// The naive conjugate form -- a*c/(c²+d²), -a*d/(c²+d²) -- is that formula. c²+d²
// overflows to +Inf above ~1.3e154 and flushes to zero below ~1e-154, so it collapses
// ordinary finite divisions to 0, ±inf, or NaN. Go's own complex division uses Smith's
// algorithm (1962) precisely to avoid it, dividing through by the LARGER component.
//
// Asserted on the VALUES, not on SchemeString: wile prints floats in full decimal
// rather than scientific notation (as master does), so a string comparison here would
// be testing the formatter, not the arithmetic.
//
//	petite -q <<< '(display (list (/ 1.0 1e200+1e200i) (/ 2.0 1e-200+0.0i)))'
//	racket   -e '(display (list (/ 1.0 1e200+1e200i) (/ 2.0 1e-200+0.0i)))'
//	# both => (5e-201-5e-201i 2e200-0.0i)
func TestRealDivideByComplexKeepsRange(t *testing.T) {
	tcs := []struct {
		name           string
		dividend       float64
		divisor        *Complex
		wantRe, wantIm float64
	}{
		// c²+d² would overflow to +Inf and drive the quotient to zero.
		{"huge components", 1.0, NewComplex(complex(1e200, 1e200)), 5e-201, -5e-201},
		// c²+d² would flush to zero and drive the quotient to ±inf.
		{"tiny components", 2.0, NewComplex(complex(1e-170, 1e-170)), 1e170, -1e170},
		// The naive form manufactured a NaN out of an ordinary finite division.
		{"tiny real component", 2.0, NewComplex(complex(1e-200, 0)), 2e200, math.Copysign(0, -1)},
		// A perfectly ordinary case must still be exact.
		{"ordinary", 2.0, NewComplex(complex(1, 1)), 1.0, -1.0},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			got, err := NewFloat(tc.dividend).Divide(tc.divisor)
			c.Assert(err, qt.IsNil)

			q, ok := got.(*Complex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected *Complex, got %T", got))
			re, im := real(q.Value), imag(q.Value)

			c.Assert(math.IsNaN(re) || math.IsInf(re, 0), qt.IsFalse,
				qt.Commentf("real part collapsed to %v -- the intermediate overflowed", re))
			c.Assert(math.IsNaN(im) || math.IsInf(im, 0), qt.IsFalse,
				qt.Commentf("imag part collapsed to %v -- the intermediate overflowed", im))

			c.Assert(closeEnough(re, tc.wantRe), qt.IsTrue,
				qt.Commentf("real: want %v, got %v", tc.wantRe, re))
			c.Assert(closeEnough(im, tc.wantIm), qt.IsTrue,
				qt.Commentf("imag: want %v, got %v", tc.wantIm, im))
		})
	}

	// The round trip is the sharpest statement of the property: (1/z)·z is 1, and it is
	// 0 under the naive formula because the reciprocal underflowed to zero entirely.
	c := qt.New(t)
	z := NewComplex(complex(1e200, 1e200))
	inv, err := NewFloat(1.0).Divide(z)
	c.Assert(err, qt.IsNil)
	rt, ok := inv.Multiply(z).(*Complex)
	c.Assert(ok, qt.IsTrue)
	c.Assert(closeEnough(real(rt.Value), 1.0), qt.IsTrue,
		qt.Commentf("(1/z)*z must be ~1; the naive conjugate formula gives 0"))
}

// closeEnough compares two float64s by RELATIVE error, so it works across the ~600
// orders of magnitude these cases span. A negative zero must match a negative zero.
func closeEnough(got, want float64) bool {
	if want == 0 {
		return got == 0 && math.Signbit(got) == math.Signbit(want)
	}
	return math.Abs(got-want)/math.Abs(want) < 1e-12
}

// TestRealOpComplexKeepsContagionOnEveryPath extends the contagion guard to the paths
// TestRealPromotionKeepsContagion missed -- which is how a leak shipped.
//
// That test covered Add and Multiply, so it never reached bigComplexDivide's GENERAL
// branch, and contagionOverParts had been added everywhere except there. A promoted
// real dividend carries an EXACT zero imaginary part, and when the divisor is exact
// and pure-imaginary BOTH terms of numerReal = a*c + b*d annihilate to an exact zero:
// b because promotion mints it exact, c because it is zero. The quotient then holds an
// exact 0 real part inside an inexact number.
//
//	petite -q <<< '(display (list (/ 2.0 0+1i) (exact? (real-part (/ 2.0 0+1i)))))'
//	# => (0.0-2.0i #f)
func TestRealOpComplexKeepsContagionOnEveryPath(t *testing.T) {
	pureImagExact := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(0), NewBigIntegerFromInt64(1)) // 0+1i, exact
	negPureImag := NewBigComplexFromBigIntegers(
		NewBigIntegerFromInt64(0), NewBigIntegerFromInt64(-1)) // 0-1i, exact

	ops := []struct {
		name string
		call func(a Number, b Number) (Number, error)
	}{
		{"Add", func(a, b Number) (Number, error) {
			return a.Add(b), nil
		}},
		{"Subtract", func(a, b Number) (Number, error) {
			return a.Subtract(b), nil
		}},
		{"Multiply", func(a, b Number) (Number, error) {
			return a.Multiply(b), nil
		}},
		{"Divide", func(a, b Number) (Number, error) {
			return a.Divide(b)
		}},
	}

	for _, op := range ops {
		for _, z := range []Number{pureImagExact, negPureImag} {
			// Both operand orders: the promoted real can be either side.
			for _, pair := range [][2]Number{{NewFloat(2.0), z}, {z, NewFloat(2.0)}} {
				name := op.name + " " + pair[0].SchemeString() + " " + pair[1].SchemeString()
				t.Run(name, func(t *testing.T) {
					c := qt.New(t)
					got, err := op.call(pair[0], pair[1])
					c.Assert(err, qt.IsNil)
					c.Assert(got.IsExact(), qt.IsFalse,
						qt.Commentf("an inexact operand makes the result inexact"))

					bc, ok := got.(*BigComplex)
					if !ok {
						return
					}
					c.Assert(bc.Real().IsExact(), qt.IsFalse,
						qt.Commentf("EXACT real part %s inside the inexact number %s -- the "+
							"promoted exact zero leaked", bc.Real().SchemeString(), got.SchemeString()))
					c.Assert(bc.Imag().IsExact(), qt.IsFalse,
						qt.Commentf("EXACT imag part %s inside the inexact number %s",
							bc.Imag().SchemeString(), got.SchemeString()))
				})
			}
		}
	}
}
