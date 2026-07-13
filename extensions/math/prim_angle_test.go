package math_test

import (
	gomath "math"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
)

// TestAngleOfReal pins the angle of a REAL number, on all three axes the previous
// implementation got wrong.
//
// The contract, verified against Chez and Racket:
//
//	x is an EXACT zero   -> RAISE       a mathematical zero has no direction
//	SignBit(x)           -> inexact π   INCLUDING -0.0: it lies on the negative axis
//	otherwise            -> EXACT 0     including +0.0
//
// That is the exact-zero distinction one more time. (angle 0) raises, but
// (angle 0.0) is 0 and (angle -0.0) is π -- an exact 0 is a mathematical zero, while
// an inexact ±0.0 is an IEEE value that sits on one side of the axis or the other.
//
//	petite -q <<< '(display (list (angle -0.0) (angle 0.0) (angle 1.0) (exact? (angle 1.0))))'
//	racket   -e '(display (list (angle -0.0) (angle 0.0) (angle 1.0) (exact? (angle 1.0))))'
//	# both => (3.141592653589793 0 0 #t)
func TestAngleOfReal(t *testing.T) {
	pi := "3.141592653589793"

	tcs := []testhelpers.SchemeCodeTestCase{}
	_ = tcs

	cases := []struct {
		name string
		code string
		want string
	}{
		// THE SIGN AXIS. -0.0 is on the NEGATIVE real axis, so its angle is π.
		// IsNegative() cannot see this (-0.0 < 0 is false), which is why the old
		// code returned 0.0 -- the wrong quadrant, not merely the wrong sign.
		{"negative zero float", "(angle -0.0)", pi},
		{"positive zero float", "(angle 0.0)", "0"},

		// THE EXACTNESS AXIS. The angle of ANY positive real is exactly 0, whatever
		// the operand's exactness. It is a strong update: the result is known
		// exactly. The old code returned an inexact 0.0.
		{"positive integer", "(angle 1)", "0"},
		{"positive float", "(angle 1.0)", "0"},
		{"positive rational", "(angle 1/2)", "0"},
		{"exact? of a positive angle", "(exact? (angle 1.0))", "#t"},

		// Negative reals give an INEXACT π, whatever the operand's exactness.
		{"negative integer", "(angle -1)", pi},
		{"negative float", "(angle -1.0)", pi},
		{"negative rational", "(angle -1/2)", pi},
		{"exact? of a negative angle", "(exact? (angle -1.0))", "#f"},

		// Infinities keep their side of the axis.
		{"positive infinity", "(angle +inf.0)", "0"},
		{"negative infinity", "(angle -inf.0)", pi},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestAngleOfExactZeroRaises pins the DOMAIN axis: a mathematical zero has no
// direction, so (angle 0) is undefined and must raise. Both oracles do:
//
//	petite: Exception in angle: undefined for 0
//	racket: angle: division by zero
//
// An INEXACT zero does not raise -- (angle 0.0) is 0 and (angle -0.0) is π -- because
// ±0.0 is an IEEE value sitting on one side of the axis, not a mathematical zero.
func TestAngleOfExactZeroRaises(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "exact zero integer", Code: "(angle 0)"},
		{Name: "exact zero rational", Code: "(angle 0/1)"},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil,
				qt.Commentf("the angle of a mathematical zero is undefined"))
		})
	}
}

// TestAngleOfComplexSignedZero pins the imaginary part's sign through the big-float
// atan2 kernel, which is where the Sign()-vs-Signbit() trap actually bites.
//
//	petite -q <<< '(display (list (angle (/ 10 2.0+0.0i)) (angle (make-rectangular 1.0 -0.0))))'
//	# => (-0.0 -0.0)
func TestAngleOfComplexSignedZero(t *testing.T) {
	c := qt.New(t)
	_ = gomath.Pi

	// A NEGATIVE zero imaginary part with a positive real part: the angle is -0.0,
	// not +0.0. atanAt's zero early-return used to drop the sign.
	result, err := testhelpers.RunSchemeCode(t, "(angle (make-rectangular 1.0 -0.0))")
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "-0.0")

	// And a positive zero imaginary part still gives +0.0.
	result, err = testhelpers.RunSchemeCode(t, "(angle (make-rectangular 1.0 0.0))")
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "0.0")
}
