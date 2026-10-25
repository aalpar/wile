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

package values

import (
	"math"
	"math/big"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestPromotionTable_Symmetry(t *testing.T) {
	c := qt.New(t)
	for a := range numKinds {
		for b := range numKinds {
			c.Assert(
				promotionTable[a][b], qt.Equals, promotionTable[b][a],
				qt.Commentf("promotionTable[%d][%d] != promotionTable[%d][%d]", a, b, b, a),
			)
		}
	}
}

// kindLabels gives promotion-law failures a readable name. The tables are
// indexed by NumericKind, which has no String method; a raw %d in a failing
// triple is unreadable exactly when you most need to read it.
var kindLabels = [numKinds]string{
	KindInteger:    "Integer",
	KindBigInteger: "BigInteger",
	KindFloat:      "Float",
	KindBigFloat:   "BigFloat",
	KindRational:   "Rational",
	KindComplex:    "Complex",
	KindBigComplex: "BigComplex",
}

// TestPromotionTable_Associativity pins the third of the three semilattice laws
// named in promotionTable's own doc comment. Symmetry and idempotency each had a
// test; associativity did not, and the table silently stopped satisfying it.
//
// A join must be associative or the result of an n-ary + or * depends on the fold
// order. That is not an abstract algebra complaint: the result KIND is observable
// through eqv?/equal? (R7RS §6.1 makes representation observable for inexacts), so
// a non-associative join means (+ a b c) and (+ c a b) can produce values that are
// = but not eqv?, and that print identically. See docs/numeric/tower.md.
func TestPromotionTable_Associativity(t *testing.T) {
	c := qt.New(t)
	for a := range numKinds {
		for b := range numKinds {
			for d := range numKinds {
				left := promotionTable[promotionTable[a][b]][d]
				right := promotionTable[a][promotionTable[b][d]]
				c.Assert(left, qt.Equals, right, qt.Commentf(
					"(%s ⊔ %s) ⊔ %s = %s  but  %s ⊔ (%s ⊔ %s) = %s",
					kindLabels[a], kindLabels[b], kindLabels[d], kindLabels[left],
					kindLabels[a], kindLabels[b], kindLabels[d], kindLabels[right],
				))
			}
		}
	}
}

func TestPromotionTable_Diagonal(t *testing.T) {
	c := qt.New(t)
	// Every type paired with itself should return itself.
	for k := range numKinds {
		c.Assert(
			promotionTable[k][k], qt.Equals, k,
			qt.Commentf("diagonal: kind %d should promote to itself", k),
		)
	}
}

func TestPromotionTable_ExactResults(t *testing.T) {
	c := qt.New(t)
	// Verify every entry in the no-loss result type matrix.
	// Layout: promotionTable[row][col] = expected
	type entry struct {
		a, b     NumericKind
		expected NumericKind
	}

	entries := []entry{
		// Zone 1: Exact × Exact
		{KindInteger, KindInteger, KindInteger},
		{KindInteger, KindBigInteger, KindBigInteger},
		{KindInteger, KindRational, KindRational},
		{KindBigInteger, KindBigInteger, KindBigInteger},
		{KindBigInteger, KindRational, KindRational},
		{KindRational, KindRational, KindRational},

		// Zone 2: Exact × Inexact Real → the inexact operand's kind (contagion).
		// (+ 1.5 2) is a Float 3.5, not a 256-bit BigFloat. An explicit BigFloat
		// operand still yields BigFloat — precision you ASK for is preserved.
		{KindInteger, KindFloat, KindFloat},
		{KindInteger, KindBigFloat, KindBigFloat},
		{KindBigInteger, KindFloat, KindFloat},
		{KindBigInteger, KindBigFloat, KindBigFloat},
		{KindRational, KindFloat, KindFloat},
		{KindRational, KindBigFloat, KindBigFloat},

		// Inexact × Inexact
		{KindFloat, KindFloat, KindFloat},
		{KindFloat, KindBigFloat, KindBigFloat},
		{KindBigFloat, KindBigFloat, KindBigFloat},

		// Zone 3: Anything × Complex — the SAME contagion as Zone 2. An exact operand
		// meeting an inexact Complex is absorbed into it, exactly as it is absorbed
		// into a Float. Associativity forces this: exact ⊔ Float is Float and
		// Float ⊔ Complex is Complex, so exact ⊔ Complex must be Complex or the join
		// stops being a semilattice (TestPromotionTable_Associativity).
		//
		// The exact-zero hazard is real and is handled at the OPERATION, not here:
		// real ⊕ complex is computed part-wise so no imaginary component is ever
		// manufactured, and (/ 10 2.0+0.0i) => 5.0-0.0i still holds. See the
		// real ⊕ complex helpers in complex.go.
		{KindInteger, KindComplex, KindComplex},
		{KindInteger, KindBigComplex, KindBigComplex},
		{KindBigInteger, KindComplex, KindComplex},
		{KindBigInteger, KindBigComplex, KindBigComplex},
		{KindRational, KindComplex, KindComplex},
		{KindRational, KindBigComplex, KindBigComplex},
		{KindFloat, KindComplex, KindComplex},
		{KindFloat, KindBigComplex, KindBigComplex},
		{KindBigFloat, KindComplex, KindBigComplex},
		{KindBigFloat, KindBigComplex, KindBigComplex},
		{KindComplex, KindComplex, KindComplex},
		{KindComplex, KindBigComplex, KindBigComplex},
		{KindBigComplex, KindBigComplex, KindBigComplex},
	}

	for _, e := range entries {
		c.Assert(
			PromotionResultKind(e.a, e.b), qt.Equals, e.expected,
			qt.Commentf("PromotionResultKind(%d, %d)", e.a, e.b),
		)
		// Verify symmetry inline.
		c.Assert(
			PromotionResultKind(e.b, e.a), qt.Equals, e.expected,
			qt.Commentf("PromotionResultKind(%d, %d) (symmetric)", e.b, e.a),
		)
	}
}

// TestPromotionTable_ContagionIsLossy is the other half: arithmetic MUST round the
// exact operand into the inexact one's representation. This is the positive
// statement of what the change bought — (+ 1.5 2) allocating a Float, not a 256-bit
// BigFloat that nothing ever demotes.
func TestPromotionTable_ContagionIsLossy(t *testing.T) {
	c := qt.New(t)
	for _, exact := range []NumericKind{KindInteger, KindBigInteger, KindRational} {
		c.Assert(
			promotionTable[exact][KindFloat], qt.Equals, KindFloat,
			qt.Commentf("exact kind %d + Float must contaminate to Float (R7RS 6.2.2)", exact),
		)
		// The SAME contagion on the complex axis — see the Zone 3 comment in
		// promotion.go. The exact-zero imaginary part is protected at the operation
		// (real ⊕ complex is part-wise), not by escalating the join.
		c.Assert(
			promotionTable[exact][KindComplex], qt.Equals, KindComplex,
			qt.Commentf("exact kind %d + Complex must contaminate to Complex, exactly as "+
				"it does to Float; escalating instead breaks associativity", exact),
		)
		// Precision the program ASKED for is still preserved.
		c.Assert(
			promotionTable[exact][KindBigFloat], qt.Equals, KindBigFloat,
			qt.Commentf("exact kind %d + BigFloat must stay BigFloat", exact),
		)
	}
	// The one arithmetic-side assertion that used to live in
	// TestComparisonTable_NoLossyPaths, kept when that test was rewritten as a
	// value-level property in compare_test.go: BigFloat does not fit in
	// complex128, so the SUM must not route through it either.
	c.Assert(
		promotionTable[KindBigFloat][KindComplex], qt.Not(qt.Equals), KindComplex,
		qt.Commentf("BigFloat + Complex must not round through complex128"),
	)
}

func TestPromoter_AllReachablePathsPopulated(t *testing.T) {
	c := qt.New(t)
	// For every (a, b) pair, the promoter for a→result and b→result must exist.
	for a := range numKinds {
		for b := range numKinds {
			result := promotionTable[a][b]
			c.Assert(
				promoter[a][result] != nil, qt.IsTrue,
				qt.Commentf("promoter[%d][%d] is nil (needed for pair %d+%d→%d)", a, result, a, b, result),
			)
			c.Assert(
				promoter[b][result] != nil, qt.IsTrue,
				qt.Commentf("promoter[%d][%d] is nil (needed for pair %d+%d→%d)", b, result, a, b, result),
			)
		}
	}
}

func TestPromote_IntegerToAllTargets(t *testing.T) {
	c := qt.New(t)
	n := NewInteger(42)

	// Identity.
	c.Assert(Promote(n, KindInteger), qt.Equals, n)

	// → BigInteger
	bi := Promote(n, KindBigInteger)
	c.Assert(bi.Kind(), qt.Equals, KindBigInteger)
	c.Assert(bi.(*BigInteger).value.Int64(), qt.Equals, int64(42))

	// → Rational
	r := Promote(n, KindRational)
	c.Assert(r.Kind(), qt.Equals, KindRational)
	c.Assert(r.(*Rational).NumInt64(), qt.Equals, int64(42))
	c.Assert(r.(*Rational).DenomInt64(), qt.Equals, int64(1))

	// → BigFloat
	bf := Promote(n, KindBigFloat)
	c.Assert(bf.Kind(), qt.Equals, KindBigFloat)
	f64, _ := bf.(*BigFloat).value.Float64()
	c.Assert(f64, qt.Equals, float64(42))

	// → BigComplex (exact parts)
	bc := Promote(n, KindBigComplex)
	c.Assert(bc.Kind(), qt.Equals, KindBigComplex)
	bcc := bc.(*BigComplex)
	c.Assert(bcc.real.Kind(), qt.Equals, KindBigInteger)
	c.Assert(bcc.imag.IsZero(), qt.IsTrue)
}

func TestPromote_BigIntegerToAllTargets(t *testing.T) {
	c := qt.New(t)
	n := NewBigIntegerFromInt64(1000000)

	c.Assert(Promote(n, KindBigInteger), qt.Equals, n)

	r := Promote(n, KindRational)
	c.Assert(r.Kind(), qt.Equals, KindRational)

	bf := Promote(n, KindBigFloat)
	c.Assert(bf.Kind(), qt.Equals, KindBigFloat)

	bc := Promote(n, KindBigComplex)
	c.Assert(bc.Kind(), qt.Equals, KindBigComplex)
	c.Assert(bc.(*BigComplex).real.Kind(), qt.Equals, KindBigInteger)
}

func TestPromote_RationalToAllTargets(t *testing.T) {
	c := qt.New(t)
	n := NewRational(3, 7)

	c.Assert(Promote(n, KindRational), qt.Equals, n)

	bf := Promote(n, KindBigFloat)
	c.Assert(bf.Kind(), qt.Equals, KindBigFloat)

	bc := Promote(n, KindBigComplex)
	c.Assert(bc.Kind(), qt.Equals, KindBigComplex)
}

func TestPromote_FloatToAllTargets(t *testing.T) {
	c := qt.New(t)
	n := NewFloat(3.14)

	c.Assert(Promote(n, KindFloat), qt.Equals, n)

	bf := Promote(n, KindBigFloat)
	c.Assert(bf.Kind(), qt.Equals, KindBigFloat)

	cx := Promote(n, KindComplex)
	c.Assert(cx.Kind(), qt.Equals, KindComplex)
	c.Assert(real(cx.(*Complex).Value), qt.Equals, 3.14)
	c.Assert(imag(cx.(*Complex).Value), qt.Equals, 0.0)

	bc := Promote(n, KindBigComplex)
	c.Assert(bc.Kind(), qt.Equals, KindBigComplex)
}

func TestPromote_BigFloatToAllTargets(t *testing.T) {
	c := qt.New(t)
	n := NewBigFloatFromFloat64(2.718)

	c.Assert(Promote(n, KindBigFloat), qt.Equals, n)

	bc := Promote(n, KindBigComplex)
	c.Assert(bc.Kind(), qt.Equals, KindBigComplex)
}

func TestPromote_ComplexToAllTargets(t *testing.T) {
	c := qt.New(t)
	n := NewComplex(complex(1.0, 2.0))

	c.Assert(Promote(n, KindComplex), qt.Equals, n)

	bc := Promote(n, KindBigComplex)
	c.Assert(bc.Kind(), qt.Equals, KindBigComplex)
}

func TestPromote_BigComplexIdentity(t *testing.T) {
	c := qt.New(t)
	n := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(1.0), NewBigFloatFromFloat64(2.0))
	c.Assert(Promote(n, KindBigComplex), qt.Equals, n)
}

func TestPromote_PreservesExactness(t *testing.T) {
	c := qt.New(t)
	// Integer → BigComplex should produce exact parts (BigInteger, not BigFloat).
	n := NewInteger(7)
	bc := Promote(n, KindBigComplex).(*BigComplex)
	c.Assert(bc.real.IsExact(), qt.IsTrue)
	c.Assert(bc.imag.IsExact(), qt.IsTrue)

	// BigInteger → BigComplex should preserve exact BigInteger parts.
	bi := NewBigIntegerFromInt64(999)
	bc2 := Promote(bi, KindBigComplex).(*BigComplex)
	c.Assert(bc2.real.IsExact(), qt.IsTrue)
	c.Assert(bc2.imag.IsExact(), qt.IsTrue)
}

func TestPromotionResultKind_API(t *testing.T) {
	c := qt.New(t)
	// Public API produces same results as direct table access.
	c.Assert(PromotionResultKind(KindInteger, KindFloat), qt.Equals, KindFloat)
	c.Assert(PromotionResultKind(KindRational, KindComplex), qt.Equals, KindComplex)
	c.Assert(PromotionResultKind(KindFloat, KindComplex), qt.Equals, KindComplex)
	c.Assert(PromotionResultKind(KindRational, KindBigComplex), qt.Equals, KindBigComplex)
}

// TestFloatBigComplexGuard_ImaginaryPreserved verifies fix for issue #362:
// when a Float with Inf/NaN is combined with a BigComplex, the imaginary part
// of the BigComplex operand must be preserved in the result.
//
// After the fix (making BigFloat Inf/NaN-capable and updating the guard to return
// BigComplex), the result is *BigComplex with the imaginary part fully preserved.
func TestFloatBigComplexGuard_ImaginaryPreserved(t *testing.T) {
	c := qt.New(t)

	// BigComplex(3+4i) with BigInteger parts — guarantees BigComplex kind (not Complex).
	bc := NewBigComplex(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(4),
	)

	type testCase struct {
		name    string
		floatV  *Float
		op      func(a, b Number) Number
		realInf bool
		realNaN bool
		// skipReversed skips the reversed-direction BigComplex+imag check.
		// Division is asymmetric: (3+4i)/Inf = 0 (real), not complex.
		skipReversed bool
	}

	cases := []testCase{
		{
			name:    "add +inf.0 + BigComplex(3+4i): imag preserved",
			floatV:  NewFloat(math.Inf(1)),
			op:      func(a, b Number) Number { return a.Add(b) },
			realInf: true,
		},
		{
			name:    "add -inf.0 + BigComplex(3+4i): imag preserved",
			floatV:  NewFloat(math.Inf(-1)),
			op:      func(a, b Number) Number { return a.Add(b) },
			realInf: true,
		},
		{
			name:    "add +nan.0 + BigComplex(3+4i): NaN propagates",
			floatV:  NewFloat(math.NaN()),
			op:      func(a, b Number) Number { return a.Add(b) },
			realNaN: true,
		},
		{
			name:    "sub +inf.0 - BigComplex(3+4i): imag sign flips",
			floatV:  NewFloat(math.Inf(1)),
			op:      func(a, b Number) Number { return a.Subtract(b) },
			realInf: true,
		},
		{
			name:    "mul +inf.0 * BigComplex(3+4i): complex semantics",
			floatV:  NewFloat(math.Inf(1)),
			op:      func(a, b Number) Number { return a.Multiply(b) },
			realInf: true,
		},
		{
			name:    "mul +nan.0 * BigComplex(3+4i): NaN propagates",
			floatV:  NewFloat(math.NaN()),
			op:      func(a, b Number) Number { return a.Multiply(b) },
			realNaN: true,
		},
		{
			// Inf / (3+4i) has non-zero imaginary; (3+4i) / Inf = 0 (real, not complex).
			name:   "div +inf.0 / BigComplex(3+4i): correct complex division",
			floatV: NewFloat(math.Inf(1)),
			op: func(a, b Number) Number {
				q, err := a.Divide(b)
				if err != nil {
					panic(err)
				}
				return q
			},
			realInf:      true,
			skipReversed: true,
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Float is receiver, BigComplex is operand.
			result := tc.op(tc.floatV, bc)
			bResult, ok := result.(*BigComplex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected *BigComplex, got %T", result))

			// Imaginary part must not be zero (the original bug: imaginary lost).
			c.Assert(bResult.Imag().IsZero(), qt.IsFalse)
			if tc.realInf {
				c.Assert(bResult.Real().IsFinite(), qt.IsFalse)
				c.Assert(bResult.Real().IsNaN(), qt.IsFalse)
			}
			if tc.realNaN {
				c.Assert(bResult.Real().IsNaN(), qt.IsTrue)
			}

			if tc.skipReversed {
				return
			}

			// Reversed: BigComplex is receiver, Float is operand.
			resultRev := tc.op(bc, tc.floatV)
			bResultRev, ok := resultRev.(*BigComplex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("(reversed) expected *BigComplex, got %T", resultRev))
			c.Assert(bResultRev.Imag().IsZero(), qt.IsFalse)
		})
	}
}

// TestFloatBigComplexGuard_ResultType verifies that the guard path
// produces *BigComplex for the BigComplex LUB case (fix for #362).
func TestFloatBigComplexGuard_ResultType(t *testing.T) {
	c := qt.New(t)
	bc := NewBigComplex(
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(4),
	)
	result := NewFloat(math.Inf(1)).Add(bc)
	_, isFloat := result.(*Float)
	_, isComplex := result.(*Complex)
	_, isBigComplex := result.(*BigComplex)
	c.Assert(isBigComplex, qt.IsTrue, qt.Commentf("expected *BigComplex, got float=%v complex=%v actual=%T", isFloat, isComplex, result))
}

// TestSpecialFloatBigComplex_ImaginaryDigits pins I175: an Inf/NaN Float meeting
// a BigComplex used to round BOTH components through complex128, so the
// untouched imaginary component came back at float64 precision (the result still
// printed as a BigComplex, which hides the loss). The additive rows assert the
// exact imaginary digits. The multiplicative rows pin today's complex128 route so
// a change to it is deliberate: it is right while the BigComplex parts fit in
// float64 and wrong beyond (filed in todo/tier1-security-correctness.md).
func TestSpecialFloatBigComplex_ImaginaryDigits(t *testing.T) {
	c := qt.New(t)

	const digits = "9.87654321098765432109876543210987"
	imPart := NewBigFloatFromString(digits)
	c.Assert(imPart, qt.IsNotNil)
	z := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(0), imPart)
	negZeroImag := NewBigComplexFromBigFloats(
		NewBigFloatFromFloat64(0),
		NewBigFloatFromFloat64(math.Copysign(0, -1)),
	)

	// A finite real part beyond float64 range: float64 would make it -Inf, and
	// +inf.0 + -inf.0 is NaN where the true sum is +inf.0.
	hugeNegReal := NewBigComplexFromBigFloats(
		NewBigFloatFromString("-1e400"),
		NewBigFloatFromFloat64(1),
	)

	// An exact imaginary part rounds to float64 like every other inexact-contagion
	// path; it must not be lifted to the 256-bit default precision.
	thirdImag := NewBigComplex(NewBigIntegerFromInt64(1), NewRational(1, 3))
	hugeImag := NewBigComplex(
		NewBigIntegerFromInt64(1),
		NewBigIntegerFromString("1"+strings.Repeat("0", 400), 10),
	)

	// A real part that is itself infinite is not clamped: +inf + -inf is NaN.
	infReal := NewBigComplexFromBigFloats(
		NewBigFloatFromFloat64(math.Inf(1)),
		NewBigFloatFromFloat64(1),
	)

	posZeroImag := NewBigComplexFromBigFloats(
		NewBigFloatFromFloat64(0),
		NewBigFloatFromFloat64(0),
	)

	inf := NewFloat(math.Inf(1))
	negInf := NewFloat(math.Inf(-1))
	nan := NewFloat(math.NaN())

	add := func(a, b Number) Number {
		return a.Add(b)
	}
	sub := func(a, b Number) Number {
		return a.Subtract(b)
	}

	// imagWant is the exact imaginary component the additive result must carry.
	tcs := []struct {
		name     string
		got      Number
		realWant string
		imagWant Number
	}{
		{"z + +inf.0", add(z, inf), "+inf.0", imPart},
		{"+inf.0 + z", add(inf, z), "+inf.0", imPart},
		{"z - +inf.0", sub(z, inf), "-inf.0", imPart},
		{"+inf.0 - z", sub(inf, z), "+inf.0", imPart.Negate()},
		{"z + +nan.0", add(z, nan), "+nan.0", imPart},
		{"+nan.0 + z", add(nan, z), "+nan.0", imPart},
		{"z - +nan.0", sub(z, nan), "+nan.0", imPart},
		{"+nan.0 - z", sub(nan, z), "+nan.0", imPart.Negate()},
		{"huge-real z + +inf.0 stays +inf.0", add(hugeNegReal, inf), "+inf.0", NewBigFloatFromFloat64(1)},
		{"+inf.0 - huge-real z stays +inf.0", sub(inf, hugeNegReal), "+inf.0", NewBigFloatFromFloat64(-1)},
		{"huge-real z + +nan.0 is nan", add(hugeNegReal, nan), "+nan.0", NewBigFloatFromFloat64(1)},
		{"1+1/3i + +inf.0: exact imag rounds to float64", add(thirdImag, inf), "+inf.0", NewBigFloatFromFloat64(1.0 / 3)},
		{"+inf.0 - 1+1/3i: exact imag rounds then negates", sub(inf, thirdImag), "+inf.0", NewBigFloatFromFloat64(-1.0 / 3)},
		{"1+10^400i + +inf.0: exact imag overflows to inf", add(hugeImag, inf), "+inf.0", NewBigFloatFromFloat64(math.Inf(1))},
		{"+inf-real z + -inf.0 is nan, not clamped", add(infReal, negInf), "+nan.0", NewBigFloatFromFloat64(1)},
		{"+inf-real z - +inf.0 is nan, not clamped", sub(infReal, inf), "+nan.0", NewBigFloatFromFloat64(1)},
		{"+inf.0 - (+0.0i) is -0.0i, not 0.0 - 0.0", sub(inf, posZeroImag), "+inf.0", NewBigFloatFromFloat64(math.Copysign(0, -1))},
		{"(-0.0i) + +inf.0 keeps the sign", add(negZeroImag, inf), "+inf.0", NewBigFloatFromFloat64(math.Copysign(0, -1))},
		{"+inf.0 - (-0.0i) negates it", sub(inf, negZeroImag), "+inf.0", NewBigFloatFromFloat64(0)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			bc, ok := tc.got.(*BigComplex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected *BigComplex, got %T", tc.got))
			c.Assert(bc.RealAsBigFloat().SchemeString(), qt.Equals, tc.realWant)
			c.Assert(bc.Imag().EqualTo(tc.imagWant), qt.IsTrue,
				qt.Commentf("imag = %s, want %s", bc.Imag().SchemeString(), tc.imagWant.SchemeString()))
		})
	}

	mul := func(a, b Number) Number {
		return a.Multiply(b)
	}
	div := func(a, b Number) Number {
		q, err := a.Divide(b)
		c.Assert(err, qt.IsNil)
		return q
	}
	guards := []struct {
		name string
		got  Number
		want string
	}{
		{"z * +inf.0", mul(z, inf), "+nan.0+inf.0i"},
		{"+inf.0 * z", mul(inf, z), "+nan.0+inf.0i"},
		{"z * +nan.0", mul(z, nan), "+nan.0+nan.0i"},
		{"z / +inf.0", div(z, inf), "0.0+0.0i"},
		// Agrees with Chez, Racket and the plain-Complex arm for (/ +inf.0 0+9.87i).
		{"+inf.0 / z", div(inf, z), "+nan.0-inf.0i"},
		{"z / +nan.0", div(z, nan), "+nan.0+nan.0i"},
	}
	for _, tc := range guards {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(tc.got.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestSpecialFloatBigComplex_ExactImagAgreesWithFiniteAdd pins that an exact
// imaginary part takes the same value through the Inf path as through the finite
// path (contagionOverParts), so (+ +inf.0 1+1/3i) and (+ 1.0 1+1/3i) agree on it.
func TestSpecialFloatBigComplex_ExactImagAgreesWithFiniteAdd(t *testing.T) {
	c := qt.New(t)

	z := NewBigComplex(NewBigIntegerFromInt64(1), NewRational(1, 3))
	viaInf, ok := NewFloat(math.Inf(1)).Add(z).(*BigComplex)
	c.Assert(ok, qt.IsTrue)
	viaFinite, ok := NewFloat(1).Add(z).(*BigComplex)
	c.Assert(ok, qt.IsTrue)
	c.Assert(viaInf.Imag().EqualTo(viaFinite.Imag()), qt.IsTrue,
		qt.Commentf("inf path %s, finite path %s", viaInf.Imag().SchemeString(), viaFinite.Imag().SchemeString()))
}

// TestSpecialFloatBigFloat_OutOfRangeIsFinite pins the BigFloat twin of the
// clamp: a FINITE BigFloat beyond float64 range converts to +-Inf, which made
// (+ +inf.0 #m-1e400) NaN (Inf + -Inf) where the true sum is +inf.0.
func TestSpecialFloatBigFloat_OutOfRangeIsFinite(t *testing.T) {
	inf := NewFloat(math.Inf(1))
	hugePos := NewBigFloatFromString("1e400")
	hugeNeg := NewBigFloatFromString("-1e400")

	divide := func(a, b Number) Number {
		q, err := a.Divide(b)
		if err != nil {
			panic(err)
		}
		return q
	}

	negInfBig := NewBigFloatFromFloat64(math.Inf(-1))

	tcs := []struct {
		name string
		got  Number
		want string
	}{
		// An INFINITE BigFloat partner is not clamped: +inf + -inf is NaN.
		{"+inf.0 + -inf BigFloat is nan", inf.Add(negInfBig), "+nan.0"},
		{"+inf.0 + -1e400", inf.Add(hugeNeg), "+inf.0"},
		{"-1e400 + +inf.0", hugeNeg.Add(inf), "+inf.0"},
		{"+inf.0 - 1e400", inf.Subtract(hugePos), "+inf.0"},
		{"1e400 - +inf.0", hugePos.Subtract(inf), "-inf.0"},
		{"+inf.0 / 1e400", divide(inf, hugePos), "+inf.0"},
		{"1e400 / +inf.0", divide(hugePos, inf), "0.0"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(tc.got.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestSpecialFloatExactPartner_OutOfRange pins the Inf/NaN guard on the Float x
// BigInteger and Float x Rational contagion paths. The exact partner used to be
// converted to float64 before the special Float decided the answer: 10^400 became
// +inf.0 and 10^-400 became 0.0, so (+ +inf.0 (- 10^400)) was Inf + -Inf = NaN and
// (* +inf.0 10^-400) was Inf * 0 = NaN. A finite non-zero partner now stays finite
// and non-zero. Racket and Chez agree on every row.
func TestSpecialFloatExactPartner_OutOfRange(t *testing.T) {
	inf := NewFloat(math.Inf(1))
	negInf := NewFloat(math.Inf(-1))
	nan := NewFloat(math.NaN())
	ten400 := new(big.Int).Exp(big.NewInt(10), big.NewInt(400), nil)
	huge := NewBigInteger(ten400)
	negHuge := NewBigInteger(new(big.Int).Neg(ten400))
	hugeRat := NewRationalFromBigInt(ten400, big.NewInt(3))
	tiny := NewRationalFromBigInt(big.NewInt(1), ten400)
	negTiny := NewRationalFromBigInt(big.NewInt(-1), ten400)

	divide := func(a, b Number) Number {
		q, err := a.Divide(b)
		if err != nil {
			panic(err)
		}
		return q
	}

	tcs := []struct {
		name string
		got  Number
		want string
	}{
		{"+inf.0 + -10^400", inf.Add(negHuge), "+inf.0"},
		{"-10^400 + +inf.0", negHuge.Add(inf), "+inf.0"},
		{"10^400 - +inf.0", huge.Subtract(inf), "-inf.0"},
		{"+inf.0 - 10^400", inf.Subtract(huge), "+inf.0"},
		{"-inf.0 - 10^400", negInf.Subtract(huge), "-inf.0"},
		{"+inf.0 - 10^400/3", inf.Subtract(hugeRat), "+inf.0"},
		{"+inf.0 / 10^400", divide(inf, huge), "+inf.0"},
		{"-inf.0 / 10^400", divide(negInf, huge), "-inf.0"},
		{"10^400 / +inf.0", divide(huge, inf), "0.0"},
		{"+inf.0 * 10^-400", inf.Multiply(tiny), "+inf.0"},
		{"10^-400 * +inf.0", tiny.Multiply(inf), "+inf.0"},
		{"+inf.0 * -10^-400", inf.Multiply(negTiny), "-inf.0"},
		{"-inf.0 * -10^-400", negInf.Multiply(negTiny), "+inf.0"},
		{"+inf.0 / 10^-400", divide(inf, tiny), "+inf.0"},
		{"10^-400 / -inf.0", divide(tiny, negInf), "-0.0"},
		// Controls: a NaN still wins, and a FINITE Float partner is not clamped
		// (finite contagion stays lossy: 10^400 rounds to +inf.0).
		{"+nan.0 + 10^400", nan.Add(huge), "+nan.0"},
		{"+nan.0 * 10^-400", nan.Multiply(tiny), "+nan.0"},
		{"1.0 + 10^400", NewFloat(1).Add(huge), "+inf.0"},
		{"10^400 * +inf.0", huge.Multiply(inf), "+inf.0"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(tc.got.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestSpecialFloatBigFloat_Underflow pins the underflow twin of the BigFloat
// clamp: #m1e-400 converts to 0.0, and +inf.0 * 0.0 is NaN where the product of
// +inf.0 and a positive number is +inf.0. An INEXACT zero is still zero.
func TestSpecialFloatBigFloat_Underflow(t *testing.T) {
	inf := NewFloat(math.Inf(1))
	tests := []struct {
		name string
		got  Number
		want string
	}{
		{"+inf.0 * 1e-400", inf.Multiply(NewBigFloatFromString("1e-400")), "+inf.0"},
		{"+inf.0 * -1e-400", inf.Multiply(NewBigFloatFromString("-1e-400")), "-inf.0"},
		{"1e-400 * +inf.0", NewBigFloatFromString("1e-400").Multiply(inf), "+inf.0"},
		{"+inf.0 * BigFloat 0.0 is nan", inf.Multiply(NewBigFloatFromFloat64(0)), "+nan.0"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(tc.got.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestSpecialFloatBigComplex_Multiplicative pins real x complex and real / complex
// for an Inf/NaN real and a BigComplex. Both used to go through complex128, which
// rounds out-of-range parts to +-Inf or 0 and invents a +0.0 imaginary part for the
// real, so 0 * Inf turned finite and infinite answers into NaN. An EXACT zero part
// annihilates (0.0 in the inexact result); an INEXACT zero part gives NaN. Every
// row agrees with Chez and Racket on the same value spelled with exact parts or
// flonums.
func TestSpecialFloatBigComplex_Multiplicative(t *testing.T) {
	inf := NewFloat(math.Inf(1))
	negInf := NewFloat(math.Inf(-1))
	nan := NewFloat(math.NaN())
	ten400 := new(big.Int).Exp(big.NewInt(10), big.NewInt(400), nil)
	huge := NewBigInteger(ten400)
	tiny := NewRationalFromBigInt(big.NewInt(1), ten400)
	one := NewBigIntegerFromInt64(1)

	plus2i := NewBigComplex(NewBigIntegerFromInt64(0), NewBigIntegerFromInt64(2))
	exactHugeImag := NewBigComplex(one, huge)
	exactHugeBoth := NewBigComplex(huge, huge)
	exactTinyReal := NewBigComplex(tiny, one)
	exactTinyImag := NewBigComplex(one, tiny)
	bigHugeImag := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(1), NewBigFloatFromString("1e400"))
	bigTinyReal := NewBigComplexFromBigFloats(NewBigFloatFromString("1e-400"), NewBigFloatFromFloat64(1))
	inexactZeroReal := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(0), NewBigFloatFromString("9.87"))
	inexactZero := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(0), NewBigFloatFromFloat64(0))
	infImag := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(1), NewBigFloatFromFloat64(math.Inf(1)))

	divide := func(a, b Number) Number {
		q, err := a.Divide(b)
		if err != nil {
			panic(err)
		}
		return q
	}

	tcs := []struct {
		name string
		got  Number
		want string
	}{
		{"+inf.0 * +2i", inf.Multiply(plus2i), "0.0+inf.0i"},
		{"+2i * +inf.0", plus2i.Multiply(inf), "0.0+inf.0i"},
		{"-inf.0 * +2i", negInf.Multiply(plus2i), "0.0-inf.0i"},
		{"+nan.0 * +2i", nan.Multiply(plus2i), "0.0+nan.0i"},
		{"+inf.0 * 1+10^400i", inf.Multiply(exactHugeImag), "+inf.0+inf.0i"},
		{"1+10^400i * +inf.0", exactHugeImag.Multiply(inf), "+inf.0+inf.0i"},
		{"+inf.0 * 10^-400+1i", inf.Multiply(exactTinyReal), "+inf.0+inf.0i"},
		{"+inf.0 * 1+#m1e400i", inf.Multiply(bigHugeImag), "+inf.0+inf.0i"},
		{"+inf.0 * #m1e-400+1i", inf.Multiply(bigTinyReal), "+inf.0+inf.0i"},
		{"+inf.0 * 1+inf.0i", inf.Multiply(infImag), "+inf.0+inf.0i"},
		{"+inf.0 * 0.0+9.87i: inexact zero part is nan", inf.Multiply(inexactZeroReal), "+nan.0+inf.0i"},
		{"+inf.0 * 0.0+0.0i", inf.Multiply(inexactZero), "+nan.0+nan.0i"},
		{"+inf.0 / +2i", divide(inf, plus2i), "0.0-inf.0i"},
		{"-inf.0 / +2i", divide(negInf, plus2i), "0.0+inf.0i"},
		{"+nan.0 / +2i", divide(nan, plus2i), "0.0+nan.0i"},
		{"+inf.0 / 1+10^400i", divide(inf, exactHugeImag), "+inf.0-inf.0i"},
		{"+inf.0 / 10^400+10^400i", divide(inf, exactHugeBoth), "+inf.0-inf.0i"},
		{"+inf.0 / 10^-400+1i", divide(inf, exactTinyReal), "+inf.0-inf.0i"},
		{"+inf.0 / 1+10^-400i", divide(inf, exactTinyImag), "+inf.0-inf.0i"},
		{"+inf.0 / 1+#m1e400i", divide(inf, bigHugeImag), "+inf.0-inf.0i"},
		{"+inf.0 / 0.0+9.87i: inexact zero part is nan", divide(inf, inexactZeroReal), "+nan.0-inf.0i"},
		{"+inf.0 / 0.0+0.0i", divide(inf, inexactZero), "+nan.0+nan.0i"},
		{"+inf.0 / 1+inf.0i", divide(inf, infImag), "+nan.0+nan.0i"},
		{"1+10^400i / +inf.0", divide(exactHugeImag, inf), "0.0+0.0i"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			_, ok := tc.got.(*BigComplex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected *BigComplex, got %T", tc.got))
			c.Assert(tc.got.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestSpecialFloatBigComplex_DivideTableAgreesWithMethod pins the BigComplex /
// special-Float table entry. BigComplex.Divide divides by a real part-wise before
// consulting the table, so only a direct table call reaches the entry; it must
// give the method's answer.
func TestSpecialFloatBigComplex_DivideTableAgreesWithMethod(t *testing.T) {
	ten400 := new(big.Int).Exp(big.NewInt(10), big.NewInt(400), nil)
	z := NewBigComplex(NewBigIntegerFromInt64(0), NewBigInteger(ten400))
	for _, x := range []float64{math.Inf(1), math.Inf(-1), math.NaN()} {
		t.Run(NewFloat(x).SchemeString(), func(t *testing.T) {
			c := qt.New(t)
			viaTable, err := bigComplexDivide[KindFloat](z, NewFloat(x))
			c.Assert(err, qt.IsNil)
			viaMethod, err := z.Divide(NewFloat(x))
			c.Assert(err, qt.IsNil)
			c.Assert(viaTable.SchemeString(), qt.Equals, viaMethod.SchemeString())
		})
	}
}
