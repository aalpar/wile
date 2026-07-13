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

// exactZeroOperands returns a representative value of every numeric kind, in both
// a zero and a non-zero flavour.
//
// Note which zeros are EXACT: Integer, BigInteger, Rational, and a BigComplex with
// exact parts. Float, BigFloat and Complex report IsExact() == false
// unconditionally, so their zeros are INEXACT and the rule must never fire on them
// -- which is exactly what the conformance test below is checking.
func exactZeroOperands() []Number {
	return []Number{
		// exact zeros
		NewInteger(0),
		NewBigIntegerFromInt64(0),
		NewRational(0, 1),
		NewBigComplexFromBigIntegers(NewBigIntegerFromInt64(0), NewBigIntegerFromInt64(0)),
		// INEXACT zeros -- the rule must NOT fire on these
		NewFloat(0),
		NewFloat(math.Copysign(0, -1)),
		NewBigFloatFromFloat64(0),
		NewComplex(0),
		// non-zeros, one per kind
		NewInteger(5),
		NewBigIntegerFromInt64(5),
		NewRational(1, 2),
		NewFloat(2.5),
		NewBigFloatFromFloat64(2.5),
		NewComplex(complex(3, 4)),
		NewBigComplexFromBigIntegers(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(4)),
		// IEEE specials, where "exact zero overrides IEEE" actually bites
		NewFloat(math.Inf(1)),
		NewFloat(math.Inf(-1)),
		NewFloat(math.NaN()),
	}
}

// TestExactZeroCallSitesMatchTheTable is the load-bearing test of this design.
//
// The seven numeric kinds hand-inline their table rows rather than calling
// exactZeroRule, because the wrapper costs ~1.5% geomean on the Gabriel suite (see
// exactZeroRule's doc). That optimisation is only safe if the hand-written guards
// provably agree with the table -- otherwise the table is decoration and we are back
// to four spellings drifting across seven files, which is the bug this whole change
// exists to end.
//
// So: drive every kind's PUBLIC Add/Subtract/Multiply/Divide over every operand
// pairing, and assert that whenever the reference rule fires, the public method
// returns exactly what the reference says. A call site that forgets a row, or
// spells the predicate wrong, or checks the operands in the wrong order, fails here.
func TestExactZeroCallSitesMatchTheTable(t *testing.T) {
	ops := []struct {
		name string
		op   zeroOp
		call func(a, b Number) (Number, error)
	}{
		{"Add", zeroAdd, func(a, b Number) (Number, error) {
			return a.Add(b), nil
		}},
		{"Subtract", zeroSub, func(a, b Number) (Number, error) {
			return a.Subtract(b), nil
		}},
		{"Multiply", zeroMul, func(a, b Number) (Number, error) {
			return a.Multiply(b), nil
		}},
		{"Divide", zeroDiv, func(a, b Number) (Number, error) {
			return a.Divide(b)
		}},
	}

	operands := exactZeroOperands()
	fired := 0

	for _, op := range ops {
		for _, a := range operands {
			for _, b := range operands {
				want, ruleFires, wantErr := exactZeroRule(op.op, a, b)
				if !ruleFires {
					continue // normal dispatch owns it; not this test's business
				}
				fired++

				name := op.name + " " + a.SchemeString() + " " + b.SchemeString()
				t.Run(name, func(t *testing.T) {
					c := qt.New(t)
					got, gotErr := op.call(a, b)

					if wantErr != nil {
						c.Assert(gotErr, qt.IsNotNil,
							qt.Commentf("the table says this raises; the call site did not"))
						c.Assert(errors.Is(gotErr, werr.ErrDivisionByZero), qt.IsTrue)
						return
					}
					c.Assert(gotErr, qt.IsNil)
					c.Assert(got.SchemeString(), qt.Equals, want.SchemeString(),
						qt.Commentf("the call site disagrees with its table row"))
					c.Assert(got.IsExact(), qt.Equals, want.IsExact(),
						qt.Commentf("value matched but exactness did not"))
				})
			}
		}
	}

	// Guard the guard: if a refactor made the rule stop firing, every assertion
	// above would vacuously pass and this test would be worthless.
	if fired < 100 {
		t.Fatalf("the rule fired only %d times across the operand matrix; "+
			"this test is not exercising what it claims to", fired)
	}
}

// TestExactZeroPredicatesAgree pins that exactZeroEither is a SPECIALISATION of
// isExactZero and not a second copy of the rule that can drift from it.
//
// exactZeroEither hand-expands the predicate to save a call on the multiply hot
// path (isExactZero is never inlined: cost 122 vs budget 80). That is a measured,
// deliberate duplication -- and duplication is exactly what this file exists to
// eliminate, so it gets a test rather than a promise.
func TestExactZeroPredicatesAgree(t *testing.T) {
	c := qt.New(t)
	operands := exactZeroOperands()

	for _, a := range operands {
		for _, b := range operands {
			want := isExactZero(a) || isExactZero(b)
			c.Assert(exactZeroEither(a, b), qt.Equals, want,
				qt.Commentf("exactZeroEither(%s, %s) drifted from isExactZero",
					a.SchemeString(), b.SchemeString()))
		}
	}
}

// TestComplexToExactDemotes pins that Complex.ToExact routes its result through the
// demotion rule, like BigComplex.ToExact already did.
//
// Converting both parts to exact makes an inexact 0.0 imaginary part an EXACT zero
// -- at which point the value IS real (R7RS §6.2.6), and must demote. Before this,
// Complex.ToExact minted a 5+0i that reported real? #t and integer? #t yet was not
// eqv? to 5, while BigComplex.ToExact demoted correctly. Two ToExacts, one applying
// the rule and one not, is the tell that the rule had no owner.
//
//	petite -q <<< '(display (list (exact 5.0+0.0i) (eqv? (exact 5.0+0.0i) 5)))'
//	# => (5 #t)
func TestComplexToExactDemotes(t *testing.T) {
	tcs := []struct {
		name string
		in   *Complex
		want string
	}{
		{"positive zero imag", NewComplex(complex(5, 0)), "5"},
		{"negative zero imag", NewComplex(complex(5, math.Copysign(0, -1))), "5"},
		{"non-integral real", NewComplex(complex(2.5, 0)), "5/2"},
		// A non-zero imaginary part must NOT demote.
		{"non-zero imag stays complex", NewComplex(complex(5, 4)), "5+4i"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			got, err := tc.in.ToExact()
			c.Assert(err, qt.IsNil)
			c.Assert(got.SchemeString(), qt.Equals, tc.want)
			c.Assert(got.IsExact(), qt.IsTrue)
		})
	}
}

// TestComplexDivideByZeroDivisor pins the real-vs-complex zero divisor distinction
// in the Complex (complex128) type -- the MIRROR of the bug BigComplex had.
//
// Both types erase the distinction by promoting to the same value, but they land on
// opposite sides: BigComplex used to give NaN for everything, Complex gives Inf for
// everything. Go's complex128 division by (0+0i) produces Inf; C99 Annex G and both
// oracles produce NaN. A REAL zero divisor genuinely IS a signed infinity, so the
// two cannot be told apart after promotion and must be decided on the divisor's KIND.
//
//	petite -q <<< '(display (list (/ 1.0+2.0i 0.0) (/ 1.0+2.0i 0.0+0.0i)))'
//	racket   -e '(display (list (/ 1.0+2.0i 0.0) (/ 1.0+2.0i 0.0+0.0i)))'
//	# both => (+inf.0+inf.0i +nan.0+nan.0i)
func TestComplexDivideByZeroDivisor(t *testing.T) {
	oneTwo := NewComplex(complex(1, 2))

	tcs := []struct {
		name    string
		divisor Number
		want    string
	}{
		{"REAL +0.0 divisor => signed infinity", NewFloat(0), "+inf.0+inf.0i"},
		{"REAL -0.0 divisor => signed infinity", NewFloat(math.Copysign(0, -1)), "-inf.0-inf.0i"},
		{"COMPLEX 0.0+0.0i divisor => NaN", NewComplex(0), "+nan.0+nan.0i"},
		{"nonzero real divisor still divides", NewFloat(2.0), "0.5+1.0i"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			got, err := oneTwo.Divide(tc.divisor)
			c.Assert(err, qt.IsNil)
			c.Assert(got.SchemeString(), qt.Equals, tc.want)
		})
	}
}
