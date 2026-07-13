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
