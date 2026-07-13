package values

import (
	"math"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestSignBit pins the predicate that IsNegative cannot express.
//
// IsNegative asks "is n < 0", which is FALSE for a negative zero: -0.0 is not less
// than zero. Sign() is worse -- it returns 0 for BOTH +0 and -0, so the sign is
// invisible to it. That trap is already documented at big_float.go ("Sign() returns
// 0 for ±0 ... but Signbit() sees it") and code fell into it anyway.
//
// SignBit asks the IEEE question, and it is load-bearing wherever a zero's sign
// picks a branch: (angle -0.0) is π, not 0, because -0.0 lies on the NEGATIVE real
// axis. Both oracles agree.
func TestSignBit(t *testing.T) {
	negZero := math.Copysign(0, -1)

	tcs := []struct {
		name string
		n    RealNumber
		want bool
	}{
		// The whole reason this predicate exists.
		{"Float -0.0", NewFloat(negZero), true},
		{"Float +0.0", NewFloat(0), false},
		{"BigFloat -0.0", NewBigFloatFromFloat64(negZero), true},
		{"BigFloat +0.0", NewBigFloatFromFloat64(0), false},

		// Ordinary signs, for every real kind.
		{"Float -2.5", NewFloat(-2.5), true},
		{"Float 2.5", NewFloat(2.5), false},
		{"Float -inf", NewFloat(math.Inf(-1)), true},
		{"Float +inf", NewFloat(math.Inf(1)), false},
		{"BigFloat -2.5", NewBigFloatFromFloat64(-2.5), true},
		{"BigFloat 2.5", NewBigFloatFromFloat64(2.5), false},
		{"Integer -5", NewInteger(-5), true},
		{"Integer 5", NewInteger(5), false},
		{"Integer 0", NewInteger(0), false},
		{"BigInteger -5", NewBigIntegerFromInt64(-5), true},
		{"BigInteger 5", NewBigIntegerFromInt64(5), false},
		{"BigInteger 0", NewBigIntegerFromInt64(0), false},
		{"Rational -1/2", NewRational(-1, 2), true},
		{"Rational 1/2", NewRational(1, 2), false},
		{"Rational 0", NewRational(0, 1), false},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(tc.n.SignBit(), qt.Equals, tc.want)
		})
	}
}

// TestSignBitDivergesFromIsNegative pins the ONE case the two predicates disagree on,
// which is the entire point of adding SignBit. If they ever agree everywhere, SignBit
// is redundant and should go.
func TestSignBitDivergesFromIsNegative(t *testing.T) {
	c := qt.New(t)
	negZero := NewFloat(math.Copysign(0, -1))

	c.Assert(negZero.SignBit(), qt.IsTrue, qt.Commentf("a negative zero HAS its sign bit set"))
	c.Assert(negZero.IsNegative(), qt.IsFalse, qt.Commentf("...but is NOT less than zero"))
	c.Assert(negZero.IsPositive(), qt.IsFalse, qt.Commentf("...and is not greater than zero either"))
	c.Assert(negZero.Sign(), qt.Equals, 0, qt.Commentf("...and Sign() cannot see it at all"))
}
