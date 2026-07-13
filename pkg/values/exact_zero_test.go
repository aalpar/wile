package values

import (
	"errors"
	"math"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
)

// TestExactZeroRule is the conformance grid for the exact-zero rule.
//
// The premise: exactness, not magnitude, decides. An exact 0 is a MATHEMATICAL
// zero -- the annihilator of *, the identity of + -- with no sign, and IEEE's
// rules do not govern it. An inexact 0.0 is an IEEE float that merely compares
// equal to zero: it carries a sign, and IEEE 754 does govern it.
//
// Every want below is the verbatim output of both oracles:
//
//	petite -q <<< '(display (list (* 0 +inf.0) (+ 0 -0.0) (- 0 -0.0) (/ 0 +nan.0)))'
//	racket   -e '(display (list (* 0 +inf.0) (+ 0 -0.0) (- 0 -0.0) (/ 0 +nan.0)))'
//	# both => (0 -0.0 0.0 0)
//
// Two properties here are NOT guessable, and are the reason this is a table
// rather than four hand-written guards:
//
//  1. The / dividend rule is UNCONDITIONAL, exactly like the * annihilation.
//     (/ 0 0.0) is 0 in both oracles, NOT NaN -- an exact 0 divided by anything
//     is exactly zero, and that strong update overrides IEEE's 0/0 => NaN.
//
//  2. Ordering is load-bearing. (/ 0 0) RAISES in both oracles; it does not
//     return 0. So the divisor rule must beat the dividend rule, which is why
//     the applicator consults the RIGHT operand first.
func TestExactZeroRule(t *testing.T) {
	negZero := NewFloat(math.Copysign(0, -1))
	posInf := NewFloat(math.Inf(1))
	nan := NewFloat(math.NaN())

	tcs := []struct {
		name string
		op   zeroOp
		a    Number
		b    Number
		// want is the SchemeString of the result; empty means the rule must not
		// fire (fall through to normal dispatch).
		want string
		// raises means the rule fires and yields a division-by-zero error.
		raises bool
	}{
		// ---- * : an exact zero annihilates, UNCONDITIONALLY ----
		// Not "finite operand" -- the exactness of the zero alone licenses it,
		// because an exact 0 is not an IEEE value. (* 0 +inf.0) => 0, not NaN.
		{"* exact0 * inf", zeroMul, NewInteger(0), posInf, "0", false},
		{"* exact0 * nan", zeroMul, NewInteger(0), nan, "0", false},
		{"* inf * exact0", zeroMul, posInf, NewInteger(0), "0", false},
		{"* exact0 * float", zeroMul, NewInteger(0), NewFloat(2.5), "0", false},
		// An INEXACT zero must NOT short-circuit: IEEE governs it.
		{"* inexact0 * inf falls through", zeroMul, NewFloat(0), posInf, "", false},

		// ---- + : an exact zero is the identity; the operand is returned UNTOUCHED ----
		// "Untouched" is load-bearing: IEEE addition would give +0.0 here, but
		// we do not ADD -- we hand back the operand, sign and all.
		{"+ exact0 + negzero", zeroAdd, NewInteger(0), negZero, "-0.0", false},
		{"+ negzero + exact0", zeroAdd, negZero, NewInteger(0), "-0.0", false},
		{"+ exact0 + inf", zeroAdd, NewInteger(0), posInf, "+inf.0", false},
		{"+ inexact0 + negzero falls through", zeroAdd, NewFloat(0), negZero, "", false},

		// ---- - : ASYMMETRIC. right => identity, left => NEGATE ----
		// This asymmetry is the whole reason the rule is a table: (- x 0) is x,
		// but (- 0 x) is -x.
		{"- negzero - exact0 (identity)", zeroSub, negZero, NewInteger(0), "-0.0", false},
		{"- exact0 - negzero (negate)", zeroSub, NewInteger(0), negZero, "0.0", false},
		{"- exact0 - poszero (negate)", zeroSub, NewInteger(0), NewFloat(0), "-0.0", false},
		{"- exact0 - inf (negate)", zeroSub, NewInteger(0), posInf, "-inf.0", false},
		{"- exact0 - nan (negate)", zeroSub, NewInteger(0), nan, "+nan.0", false},
		{"- exact0 - 5 (negate)", zeroSub, NewInteger(0), NewInteger(5), "-5", false},

		// ---- / : divisor rule RAISES and beats the dividend rule ----
		{"/ 1 by exact0 raises", zeroDiv, NewInteger(1), NewInteger(0), "", true},
		{"/ exact0 by exact0 RAISES (order!)", zeroDiv, NewInteger(0), NewInteger(0), "", true},
		{"/ float by exact0 raises", zeroDiv, NewFloat(1.5), NewInteger(0), "", true},

		// ---- / : an exact zero DIVIDEND yields exact 0, UNCONDITIONALLY ----
		// Structurally the same strong update as the * annihilation: it overrides
		// IEEE even against NaN and an inexact zero.
		{"/ exact0 by float", zeroDiv, NewInteger(0), NewFloat(2.0), "0", false},
		{"/ exact0 by inf", zeroDiv, NewInteger(0), posInf, "0", false},
		{"/ exact0 by nan", zeroDiv, NewInteger(0), nan, "0", false},
		{"/ exact0 by INEXACT zero (not NaN!)", zeroDiv, NewInteger(0), NewFloat(0), "0", false},
		{"/ exact0 by negative inexact zero", zeroDiv, NewInteger(0), negZero, "0", false},

		// An inexact zero divisor does NOT raise: it is an IEEE value, so the
		// quotient is a signed infinity. Falls through to normal dispatch.
		{"/ 1 by inexact0 falls through", zeroDiv, NewInteger(1), NewFloat(0), "", false},
		// Neither operand is an exact zero: no rule.
		{"/ 6 by 2 falls through", zeroDiv, NewInteger(6), NewInteger(2), "", false},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			q, ok, err := exactZeroRule(tc.op, tc.a, tc.b)

			if tc.raises {
				c.Assert(ok, qt.IsTrue, qt.Commentf("the rule must fire and raise"))
				c.Assert(err, qt.IsNotNil)
				c.Assert(errors.Is(err, werr.ErrDivisionByZero), qt.IsTrue)
				return
			}
			c.Assert(err, qt.IsNil)

			if tc.want == "" {
				c.Assert(ok, qt.IsFalse,
					qt.Commentf("the rule must NOT fire; normal dispatch owns this case"))
				c.Assert(q, qt.IsNil)
				return
			}
			c.Assert(ok, qt.IsTrue, qt.Commentf("the rule must fire"))
			c.Assert(q.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestExactZeroRuleExactnessOfResult pins the exactness of the results the rule
// synthesises, which SchemeString alone cannot distinguish (0 and 0.0 differ, but
// an exact 0 returned by the * and / rules must be EXACT, not merely equal).
func TestExactZeroRuleExactnessOfResult(t *testing.T) {
	c := qt.New(t)

	// (* 0 2.5) => exact 0. Both oracles: (exact? (* 0 2.5)) => #t
	q, ok, _ := exactZeroRule(zeroMul, NewInteger(0), NewFloat(2.5))
	c.Assert(ok, qt.IsTrue)
	c.Assert(q.IsExact(), qt.IsTrue, qt.Commentf("the annihilated product is an EXACT zero"))

	// (/ 0 2.0) => exact 0. Both oracles: (exact? (/ 0 2.0)) => #t
	q, ok, _ = exactZeroRule(zeroDiv, NewInteger(0), NewFloat(2.0))
	c.Assert(ok, qt.IsTrue)
	c.Assert(q.IsExact(), qt.IsTrue, qt.Commentf("the annihilated quotient is an EXACT zero"))

	// The + and - identities return the OPERAND, so its exactness is preserved
	// rather than synthesised: (+ 0 2.5) is inexact.
	q, ok, _ = exactZeroRule(zeroAdd, NewInteger(0), NewFloat(2.5))
	c.Assert(ok, qt.IsTrue)
	c.Assert(q.IsExact(), qt.IsFalse, qt.Commentf("the identity hands back the operand untouched"))
}

// TestExactZeroTableIsTotal pins the design's central claim: a missing member is
// impossible. zeroFallThrough is iota's zero value, so an op added to the enum but
// not to the table would default to {zeroFallThrough, zeroFallThrough} and the rule
// would SILENTLY never fire -- which is precisely the "invisible missing member"
// failure this table exists to prevent. The init() assertion is the mechanism; this
// is the test that the mechanism is real rather than a comment.
func TestExactZeroTableIsTotal(t *testing.T) {
	c := qt.New(t)

	c.Assert(len(exactZeroTable), qt.Equals, int(numZeroOps),
		qt.Commentf("every zeroOp must have a row"))

	for op := range numZeroOps {
		r := exactZeroTable[op]
		c.Assert(r.left == zeroFallThrough && r.right == zeroFallThrough, qt.IsFalse,
			qt.Commentf("op %d declares no rule in either column; a silent gap is the bug "+
				"this table exists to prevent. Declare zeroFallThrough explicitly in BOTH "+
				"columns if the op genuinely has no exact-zero rule.", op))
	}
}
