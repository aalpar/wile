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
	"fmt"
	"math/big"
	"reflect"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// =============================================================================
// Lattice Type Classification
// =============================================================================

// PrecisionRank represents the precision dimension of the numeric lattice.
// This is separate from complexity (real vs complex).
type PrecisionRank int

const (
	PrecisionInteger    PrecisionRank = iota // int64
	PrecisionBigInteger                      // arbitrary-precision integer
	PrecisionRational                        // exact rational
	PrecisionFloat                           // float64 (inexact)
	PrecisionBigFloat                        // arbitrary-precision float (inexact)
)

func (p PrecisionRank) String() string {
	switch p {
	case PrecisionInteger:
		return "Integer"
	case PrecisionBigInteger:
		return "BigInteger"
	case PrecisionRational:
		return "Rational"
	case PrecisionFloat:
		return "Float"
	case PrecisionBigFloat:
		return "BigFloat"
	default:
		return fmt.Sprintf("PrecisionRank(%d)", p)
	}
}

// ComplexityRank represents the complexity dimension (real vs complex).
type ComplexityRank int

const (
	ComplexityReal ComplexityRank = iota
	ComplexityComplex
)

func (c ComplexityRank) String() string {
	if c == ComplexityReal {
		return "Real"
	}
	return "Complex"
}

// TypeClass represents a position in the numeric lattice.
// The lattice has two orthogonal dimensions: precision and complexity.
type TypeClass struct {
	Precision  PrecisionRank
	Complexity ComplexityRank
}

func (tc TypeClass) String() string {
	return fmt.Sprintf("{%s,%s}", tc.Precision, tc.Complexity)
}

// Classify returns the lattice position of a number.
func Classify(n values.Number) TypeClass {
	switch v := n.(type) {
	case *values.Integer:
		return TypeClass{PrecisionInteger, ComplexityReal}
	case *values.BigInteger:
		return TypeClass{PrecisionBigInteger, ComplexityReal}
	case *values.Rational:
		return TypeClass{PrecisionRational, ComplexityReal}
	case *values.Float:
		return TypeClass{PrecisionFloat, ComplexityReal}
	case *values.BigFloat:
		return TypeClass{PrecisionBigFloat, ComplexityReal}
	case *values.Complex:
		// complex128 uses float64 internally
		return TypeClass{PrecisionFloat, ComplexityComplex}
	case *values.BigComplex:
		// BigComplex precision depends on its components
		return TypeClass{classifyBigComplexPrecision(v), ComplexityComplex}
	}
	panic(fmt.Sprintf("unknown type: %T", n))
}

// classifyBigComplexPrecision reports the precision of the BigComplex CONTAINER,
// which is arbitrary-precision no matter what its components currently hold.
//
// It used to report max(precision(real), precision(imag)) — the precision of the
// values inside. That reads sensibly and is wrong, because the model's precision
// axis is what the JOIN is computed on, and a BigComplex operand can never demote:
// Float + BigComplex is a BigComplex, exactly as Float + BigFloat is a BigFloat.
// Reporting a BigComplex(BigInteger, BigInteger) as "BigInteger precision" made the
// join predict Complex — a complex128 — for four pairs, which would silently
// truncate the container.
//
// The old no-loss rule hid this: it forced exact × Float up to BigFloat, so the
// join happened to land on BigComplex anyway, for the wrong reason. Removing that
// rule (exactness contagion) exposed the model's real defect.
//
// Exactness is a SEPARATE axis and is unaffected: a BigComplex with exact parts is
// still exact, and TestLattice_Exactness checks that via IsExact().
func classifyBigComplexPrecision(_ *values.BigComplex) PrecisionRank {
	return PrecisionBigFloat
}

// =============================================================================
// Lattice Join (Least Upper Bound)
// =============================================================================

// Join computes the least upper bound of two type classes.
// This determines the result type of a binary operation.
//
// It is the componentwise max on a product of two independent axes — precision and
// complexity — and NOTHING ELSE. That is the whole model, and its simplicity is the
// point: max is associative and commutative, so a Join built only from max inherits
// both laws for free. The real table must satisfy the same laws, and
// TestLattice_PredictionsVsActual is what holds it to them.
//
// THIS MODEL IS THE ORACLE. It is derived from the lattice, independently of
// promotionTable, and it earns its keep only so long as that independence is real.
// It once carried a carve-out here — an escalation to BigFloat precision when an
// exact real met a float64-backed complex — added to make the model agree with a
// table that had stopped being associative. That is backwards. A model edited to
// match the implementation is not an oracle, it is a mirror, and it will report green
// on exactly the bug it exists to catch. It did: (+ 1 1.5 2.0+0.0i) and
// (+ 2.0+0.0i 1 1.5) produced values that were =, printed identically, and were not
// eqv?, and this file agreed that they should.
//
// The exact-zero problem that the carve-out was reaching for is real, but it is not a
// PROMOTION problem and it does not belong in a join. It is an OPERATION problem, and
// it is solved where operations live: real ⊕ complex is computed part-wise so that no
// imaginary component is ever manufactured. See the real ⊕ complex helpers in
// complex.go, and promotionTable Zone 3.
//
// If this function ever needs a special case again, the special case is the bug.
func Join(a, b TypeClass) TypeClass {
	return TypeClass{
		Precision:  maxPrecision(a.Precision, b.Precision),
		Complexity: maxComplexity(a.Complexity, b.Complexity),
	}
}

// maxPrecision is the join on the precision axis, and for ARITHMETIC it is a
// plain max — which is exactly what exactness contagion says (R7RS §6.2.2). The
// ranks are already ordered Integer < BigInteger < Rational < Float < BigFloat,
// so an exact operand meeting a Float simply loses to it, and the result is a
// Float. That is the contagion: the exact value is absorbed.
//
// This used to carry a "no-loss rule" carve-out forcing exact × Float up to
// BigFloat. That rule is real, but it belongs to COMPARISON, not arithmetic —
// rounding an operand is free when the result is already inexact, and fatal when
// the result is a boolean, because the rounding is what decides the boolean:
//
//	(= (- (expt 2 100) 1) (exact->inexact (expt 2 100)))  =>  #f, not #t
//
// That rule now lives in values.comparisonTable, reachable as
// values.ComparisonResultKind and exercised by TestLattice_PrecisionLoss's
// comparison subtest below.
func maxPrecision(a, b PrecisionRank) PrecisionRank {
	if a > b {
		return a
	}
	return b
}

func maxComplexity(a, b ComplexityRank) ComplexityRank {
	if a > b {
		return a
	}
	return b
}

// =============================================================================
// Result Type Determination
// =============================================================================

// ExpectedResultType returns the expected Go type from a lattice join.
// This is what SHOULD happen according to the lattice model.
func ExpectedResultType(tc TypeClass) string {
	if tc.Complexity == ComplexityComplex {
		// Complex results
		switch tc.Precision {
		case PrecisionInteger, PrecisionBigInteger, PrecisionRational:
			// Exact complex -> BigComplex with exact parts
			return "*values.BigComplex"
		case PrecisionFloat:
			// Inexact complex with float64 precision -> Complex
			return "*values.Complex"
		case PrecisionBigFloat:
			// Inexact complex with arbitrary precision -> BigComplex
			return "*values.BigComplex"
		}
	}

	// Real results
	switch tc.Precision {
	case PrecisionInteger:
		return "*values.Integer"
	case PrecisionBigInteger:
		return "*values.BigInteger"
	case PrecisionRational:
		return "*values.Rational"
	case PrecisionFloat:
		return "*values.Float"
	case PrecisionBigFloat:
		return "*values.BigFloat"
	}
	panic("unreachable")
}

// =============================================================================
// Test: Classification
// =============================================================================

func TestLattice_Classify(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name     string
		value    values.Number
		expected TypeClass
	}{
		// Real types
		{"Integer", values.NewInteger(42), TypeClass{PrecisionInteger, ComplexityReal}},
		{"BigInteger", values.NewBigIntegerFromInt64(42), TypeClass{PrecisionBigInteger, ComplexityReal}},
		{"Rational", values.NewRational(3, 4), TypeClass{PrecisionRational, ComplexityReal}},
		{"Float", values.NewFloat(3.14), TypeClass{PrecisionFloat, ComplexityReal}},
		{"BigFloat", values.NewBigFloatFromFloat64(3.14), TypeClass{PrecisionBigFloat, ComplexityReal}},

		// Complex types
		{"Complex", values.NewComplex(complex(1, 2)), TypeClass{PrecisionFloat, ComplexityComplex}},

		// BigComplex with various component types
		// The CONTAINER is arbitrary-precision, whatever its parts hold — that is
		// what the join must see. See classifyBigComplexPrecision.
		{"BigComplex(BigInteger)", values.NewBigComplex(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(2)),
			TypeClass{PrecisionBigFloat, ComplexityComplex}},
		{"BigComplex(Rational)", values.NewBigComplex(values.NewRational(1, 2), values.NewRational(3, 4)),
			TypeClass{PrecisionBigFloat, ComplexityComplex}},
		{"BigComplex(BigFloat)", values.NewBigComplex(values.NewBigFloatFromFloat64(1.0), values.NewBigFloatFromFloat64(2.0)),
			TypeClass{PrecisionBigFloat, ComplexityComplex}},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			got := Classify(tt.value)
			c.Assert(got, qt.Equals, tt.expected)
		})
	}
}

// =============================================================================
// Test: Lattice Join
// =============================================================================

func TestLattice_Join(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name     string
		a, b     TypeClass
		expected TypeClass
	}{
		// Same types
		{"Int+Int", TypeClass{PrecisionInteger, ComplexityReal}, TypeClass{PrecisionInteger, ComplexityReal},
			TypeClass{PrecisionInteger, ComplexityReal}},

		// Real precision promotion
		{"Int+BigInt", TypeClass{PrecisionInteger, ComplexityReal}, TypeClass{PrecisionBigInteger, ComplexityReal},
			TypeClass{PrecisionBigInteger, ComplexityReal}},
		{"Int+Rational", TypeClass{PrecisionInteger, ComplexityReal}, TypeClass{PrecisionRational, ComplexityReal},
			TypeClass{PrecisionRational, ComplexityReal}},
		// Contagion: the exact operand is absorbed into the Float. Plain max.
		{"Int+Float", TypeClass{PrecisionInteger, ComplexityReal}, TypeClass{PrecisionFloat, ComplexityReal},
			TypeClass{PrecisionFloat, ComplexityReal}},
		{"Rational+Float", TypeClass{PrecisionRational, ComplexityReal}, TypeClass{PrecisionFloat, ComplexityReal},
			TypeClass{PrecisionFloat, ComplexityReal}},
		{"Float+BigFloat", TypeClass{PrecisionFloat, ComplexityReal}, TypeClass{PrecisionBigFloat, ComplexityReal},
			TypeClass{PrecisionBigFloat, ComplexityReal}},

		// Real + Complex (complexity promotion). Contagion applies here exactly as it
		// does on the real axis: an exact real is absorbed into the Complex. Plain max
		// on both axes, no special case. The exact zero is protected at the operation,
		// not by escalating the join — see Join's doc comment.
		{"Int+Complex", TypeClass{PrecisionInteger, ComplexityReal}, TypeClass{PrecisionFloat, ComplexityComplex},
			TypeClass{PrecisionFloat, ComplexityComplex}},
		{"BigFloat+Complex", TypeClass{PrecisionBigFloat, ComplexityReal}, TypeClass{PrecisionFloat, ComplexityComplex},
			TypeClass{PrecisionBigFloat, ComplexityComplex}}, // BigFloat can't fit in complex128.
		{"Rational+BigComplex(exact)", TypeClass{PrecisionRational, ComplexityReal}, TypeClass{PrecisionRational, ComplexityComplex},
			TypeClass{PrecisionRational, ComplexityComplex}},

		// Complex + Complex
		{"Complex+BigComplex", TypeClass{PrecisionFloat, ComplexityComplex}, TypeClass{PrecisionBigFloat, ComplexityComplex},
			TypeClass{PrecisionBigFloat, ComplexityComplex}},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			got := Join(tt.a, tt.b)
			c.Assert(got, qt.Equals, tt.expected)
		})
	}
}

// TestLattice_JoinIsASemilattice holds the MODEL to the three semilattice laws over
// every TypeClass in the product, not just the hand-listed pairs above.
//
// A pure componentwise max satisfies all three by construction, so this test can only
// fail if someone reintroduces a special case into Join. That is exactly what it is
// for: the model's authority over promotionTable (via TestLattice_PredictionsVsActual)
// is only worth anything while the model is lawful. Guard the oracle, not just the
// implementation.
func TestLattice_JoinIsASemilattice(t *testing.T) {
	c := qt.New(t)

	var all []TypeClass
	for _, p := range []PrecisionRank{
		PrecisionInteger, PrecisionBigInteger, PrecisionRational,
		PrecisionFloat, PrecisionBigFloat,
	} {
		for _, x := range []ComplexityRank{ComplexityReal, ComplexityComplex} {
			all = append(all, TypeClass{Precision: p, Complexity: x})
		}
	}

	for _, a := range all {
		for _, b := range all {
			c.Assert(Join(a, b), qt.Equals, Join(b, a),
				qt.Commentf("commutativity: %v ⊔ %v", a, b))
			c.Assert(Join(a, a), qt.Equals, a,
				qt.Commentf("idempotency: %v ⊔ %v", a, a))
			for _, d := range all {
				c.Assert(Join(Join(a, b), d), qt.Equals, Join(a, Join(b, d)),
					qt.Commentf("associativity: %v ⊔ %v ⊔ %v", a, b, d))
			}
		}
	}
}

// =============================================================================
// Test: Lattice Predictions vs Actual Behavior
// =============================================================================

// TestLattice_PredictionsVsActual compares lattice-based predictions against
// the actual result types from direct dispatch.
func TestLattice_PredictionsVsActual(t *testing.T) {
	c := qt.New(t)

	// Test values - use different values for receiver vs operand to avoid simplification
	testValues := map[string]values.Number{
		"Integer":    values.NewInteger(5),
		"BigInteger": values.NewBigIntegerFromInt64(7),
		"Rational":   values.NewRational(3, 4),
		"Float":      values.NewFloat(2.5),
		"BigFloat":   values.NewBigFloatFromFloat64(3.5),
		"Complex":    values.NewComplex(complex(1, 2)),
		"BigComplex": values.NewBigComplex(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(2)),
	}

	// Different operand values to avoid simplification
	operandValues := map[string]values.Number{
		"Integer":    values.NewInteger(3),
		"BigInteger": values.NewBigIntegerFromInt64(11),
		"Rational":   values.NewRational(1, 3),
		"Float":      values.NewFloat(1.5),
		"BigFloat":   values.NewBigFloatFromFloat64(2.5),
		"Complex":    values.NewComplex(complex(3, 4)),
		"BigComplex": values.NewBigComplex(values.NewBigIntegerFromInt64(5), values.NewBigIntegerFromInt64(6)),
	}

	typeNames := []string{"Integer", "BigInteger", "Rational", "Float", "BigFloat", "Complex", "BigComplex"}

	// Known divergences between lattice prediction and actual behavior.
	// After the no-loss promotion table refactoring, the lattice model and
	// actual behavior are fully aligned. No divergences remain for Add.
	knownDivergences := map[string]struct {
		lattice string
		actual  string
		reason  string
	}{}

	var mismatches []string
	var matches int

	for _, aName := range typeNames {
		for _, bName := range typeNames {
			key := aName + "+" + bName

			a := testValues[aName]
			b := operandValues[bName]

			// Get lattice prediction
			classA := Classify(a)
			classB := Classify(b)
			joined := Join(classA, classB)
			latticePrediction := ExpectedResultType(joined)

			// Get actual result type (using Add as representative operation)
			result := a.Add(b)
			actualType := reflect.TypeOf(result).String()

			// Check if this is a known divergence
			div, known := knownDivergences[key]
			if known {
				c.Run(key+"_known_divergence", func(c *qt.C) {
					c.Assert(latticePrediction, qt.Equals, div.lattice, qt.Commentf("lattice prediction"))
					c.Assert(actualType, qt.Equals, div.actual, qt.Commentf("actual type"))
					c.Logf("Known divergence: %s - %s", key, div.reason)
				})
				continue
			}

			// For non-divergent cases, lattice and actual should match
			if latticePrediction == actualType {
				matches++
			} else {
				mismatches = append(mismatches, fmt.Sprintf(
					"%s: lattice=%s, actual=%s (classA=%s, classB=%s, joined=%s)",
					key, latticePrediction, actualType, classA, classB, joined,
				))
			}
		}
	}

	// Report results
	c.Logf("Matches: %d, Known divergences: %d, Unexpected mismatches: %d",
		matches, len(knownDivergences), len(mismatches))

	if len(mismatches) > 0 {
		for _, m := range mismatches {
			c.Logf("MISMATCH: %s", m)
		}
		c.Fatalf("Found %d unexpected mismatches between lattice predictions and actual behavior", len(mismatches))
	}
}

// =============================================================================
// Test: Result Type Matrix Validation
// =============================================================================

// TestLattice_ResultTypeMatrix validates the documented Result Type Matrix
// against actual implementation behavior.
func TestLattice_ResultTypeMatrix(t *testing.T) {
	c := qt.New(t)

	// The documented Result Type Matrix from NUMERIC_TOWER_REFACTOR.md
	// Key: "A+B", Value: expected result type
	expectedMatrix := map[string]string{
		// Integer row
		"Integer+Integer":    "*values.Integer",
		"Integer+BigInteger": "*values.BigInteger",
		"Integer+Rational":   "*values.Rational",
		"Integer+Float":      "*values.Float",
		"Integer+BigFloat":   "*values.BigFloat",
		"Integer+Complex":    "*values.Complex",
		"Integer+BigComplex": "*values.BigComplex",

		// BigInteger row
		"BigInteger+Integer":    "*values.BigInteger",
		"BigInteger+BigInteger": "*values.BigInteger",
		"BigInteger+Rational":   "*values.Rational",
		"BigInteger+Float":      "*values.Float",
		"BigInteger+BigFloat":   "*values.BigFloat",
		"BigInteger+Complex":    "*values.Complex",
		"BigInteger+BigComplex": "*values.BigComplex",

		// Rational row
		"Rational+Integer":    "*values.Rational",
		"Rational+BigInteger": "*values.Rational",
		"Rational+Rational":   "*values.Rational",
		"Rational+Float":      "*values.Float",
		"Rational+BigFloat":   "*values.BigFloat",
		"Rational+Complex":    "*values.Complex",
		"Rational+BigComplex": "*values.BigComplex",

		// Float row
		"Float+Integer":    "*values.Float",
		"Float+BigInteger": "*values.Float",
		"Float+Rational":   "*values.Float",
		"Float+Float":      "*values.Float",
		"Float+BigFloat":   "*values.BigFloat",
		"Float+Complex":    "*values.Complex",
		"Float+BigComplex": "*values.BigComplex",

		// BigFloat row
		"BigFloat+Integer":    "*values.BigFloat",
		"BigFloat+BigInteger": "*values.BigFloat",
		"BigFloat+Rational":   "*values.BigFloat",
		"BigFloat+Float":      "*values.BigFloat",
		"BigFloat+BigFloat":   "*values.BigFloat",
		"BigFloat+Complex":    "*values.BigComplex", // Preserves BigFloat precision
		"BigFloat+BigComplex": "*values.BigComplex",

		// Complex row
		"Complex+Integer":    "*values.Complex",
		"Complex+BigInteger": "*values.Complex",
		"Complex+Rational":   "*values.Complex",
		"Complex+Float":      "*values.Complex",
		"Complex+BigFloat":   "*values.BigComplex", // Complex.Add(BigFloat) preserves precision!
		"Complex+Complex":    "*values.Complex",
		"Complex+BigComplex": "*values.BigComplex",

		// BigComplex row
		"BigComplex+Integer":    "*values.BigComplex",
		"BigComplex+BigInteger": "*values.BigComplex",
		"BigComplex+Rational":   "*values.BigComplex",
		"BigComplex+Float":      "*values.BigComplex",
		"BigComplex+BigFloat":   "*values.BigComplex",
		"BigComplex+Complex":    "*values.BigComplex",
		"BigComplex+BigComplex": "*values.BigComplex",
	}

	// Test values
	testValues := map[string]values.Number{
		"Integer":    values.NewInteger(5),
		"BigInteger": values.NewBigIntegerFromInt64(7),
		"Rational":   values.NewRational(3, 4),
		"Float":      values.NewFloat(2.5),
		"BigFloat":   values.NewBigFloatFromFloat64(3.5),
		"Complex":    values.NewComplex(complex(1, 2)),
		"BigComplex": values.NewBigComplex(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(2)),
	}

	operandValues := map[string]values.Number{
		"Integer":    values.NewInteger(3),
		"BigInteger": values.NewBigIntegerFromInt64(11),
		"Rational":   values.NewRational(1, 3),
		"Float":      values.NewFloat(1.5),
		"BigFloat":   values.NewBigFloatFromFloat64(2.5),
		"Complex":    values.NewComplex(complex(3, 4)),
		"BigComplex": values.NewBigComplex(values.NewBigIntegerFromInt64(5), values.NewBigIntegerFromInt64(6)),
	}

	typeNames := []string{"Integer", "BigInteger", "Rational", "Float", "BigFloat", "Complex", "BigComplex"}

	for _, aName := range typeNames {
		for _, bName := range typeNames {
			key := aName + "+" + bName
			expected := expectedMatrix[key]

			c.Run(key, func(c *qt.C) {
				a := testValues[aName]
				b := operandValues[bName]
				result := a.Add(b)
				actualType := reflect.TypeOf(result).String()
				c.Assert(actualType, qt.Equals, expected)
			})
		}
	}
}

// =============================================================================
// Test: Exactness Preservation
// =============================================================================

// TestLattice_ExactnessPreservation validates that the lattice correctly
// predicts exactness preservation.
func TestLattice_ExactnessPreservation(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name        string
		a, b        values.Number
		expectExact bool
		description string
	}{
		// Exact + Exact = Exact
		{"Int+Int", values.NewInteger(1), values.NewInteger(2), true, "exact integer + exact integer"},
		{"Int+BigInt", values.NewInteger(1), values.NewBigIntegerFromInt64(2), true, "exact integer + exact big integer"},
		{"Int+Rational", values.NewInteger(1), values.NewRational(1, 2), true, "exact integer + exact rational"},
		{"BigInt+Rational", values.NewBigIntegerFromInt64(1), values.NewRational(1, 2), true, "exact big integer + exact rational"},
		{"Rational+Rational", values.NewRational(1, 2), values.NewRational(1, 3), true, "exact rational + exact rational"},

		// Exact + Inexact = Inexact
		{"Int+Float", values.NewInteger(1), values.NewFloat(2.0), false, "exact + inexact float"},
		{"Int+BigFloat", values.NewInteger(1), values.NewBigFloatFromFloat64(2.0), false, "exact + inexact big float"},
		{"Rational+Float", values.NewRational(1, 2), values.NewFloat(2.0), false, "exact rational + inexact float"},

		// Inexact + Inexact = Inexact
		{"Float+Float", values.NewFloat(1.0), values.NewFloat(2.0), false, "inexact + inexact"},
		{"Float+BigFloat", values.NewFloat(1.0), values.NewBigFloatFromFloat64(2.0), false, "float + big float"},
		{"BigFloat+BigFloat", values.NewBigFloatFromFloat64(1.0), values.NewBigFloatFromFloat64(2.0), false, "big float + big float"},

		// Complex exactness
		{"Int+ExactComplex", values.NewInteger(1),
			values.NewBigComplex(values.NewBigIntegerFromInt64(2), values.NewBigIntegerFromInt64(3)), true, "exact + exact complex"},
		{"Int+InexactComplex", values.NewInteger(1), values.NewComplex(complex(2, 3)), false, "exact + inexact complex"},
		{"Float+ExactComplex", values.NewFloat(1.0),
			values.NewBigComplex(values.NewBigIntegerFromInt64(2), values.NewBigIntegerFromInt64(3)), false, "inexact + exact complex = inexact"},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			result := tt.a.Add(tt.b)
			isExact := result.IsExact()
			c.Assert(isExact, qt.Equals, tt.expectExact, qt.Commentf(tt.description))
		})
	}
}

// =============================================================================
// Test: Lattice vs Linear Tower
// =============================================================================

// TestLattice_VsLinearTower demonstrates the difference between the correct
// lattice behavior and the broken linear tower (Tower* functions).
func TestLattice_VsLinearTower(t *testing.T) {
	c := qt.New(t)

	// This test documents the exactness bug in the linear tower
	// The linear tower forces: Integer -> BigInteger -> Rational -> Float -> BigFloat -> Complex -> BigComplex
	// This loses exactness when promoting exact reals to complex

	exactInt := values.NewInteger(3)
	exactComplex := values.NewBigComplex(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(2))

	// Direct dispatch (correct): preserves exactness
	directResult := exactInt.Add(exactComplex)
	c.Assert(directResult.IsExact(), qt.IsTrue, qt.Commentf("direct dispatch should preserve exactness"))

	// The lattice predicts: Join({Integer,Real}, {BigFloat,Complex}) = {BigFloat,Complex}
	// Result type: BigComplex — which, note, still carries EXACT parts. The
	// precision axis describes the CONTAINER's width (a BigComplex is
	// arbitrary-precision), and exactness is an independent axis, asserted above via
	// IsExact(). An arbitrary-precision container holding exact components is exact;
	// the two facts do not contradict each other.
	classInt := Classify(exactInt)
	classComplex := Classify(exactComplex)
	joined := Join(classInt, classComplex)

	c.Assert(joined.Complexity, qt.Equals, ComplexityComplex)
	c.Assert(joined.Precision, qt.Equals, PrecisionBigFloat)

	expectedType := ExpectedResultType(joined)
	c.Assert(expectedType, qt.Equals, "*values.BigComplex")

	actualType := reflect.TypeOf(directResult).String()
	c.Assert(actualType, qt.Equals, "*values.BigComplex")

	// NOTE: We don't test TowerAdd here because it has the known bug.
	// If we did: TowerAdd(exactInt, exactComplex) would return an INEXACT result
	// because the linear tower promotes Integer through Float before reaching Complex.
}

// =============================================================================
// Test: Precision Loss Documentation
// =============================================================================

// TestLattice_PrecisionLoss documents cases where precision is lost.
func TestLattice_PrecisionLoss(t *testing.T) {
	c := qt.New(t)

	// BigFloat + Complex now preserves BigFloat precision (fixed asymmetry bug)
	c.Run("BigFloat+Complex_preserves_precision", func(c *qt.C) {
		// A BigFloat with more precision than float64 can represent
		bf := values.NewBigFloatFromFloat64(1.0)
		bf.BigFloatValue().SetPrec(256)
		bf.BigFloatValue().SetString("1.123456789012345678901234567890")

		cx := values.NewComplex(complex(2, 3))

		result := bf.Add(cx)

		// The lattice says this should be BigComplex to preserve precision
		latticePrediction := ExpectedResultType(Join(Classify(bf), Classify(cx)))
		c.Assert(latticePrediction, qt.Equals, "*values.BigComplex")

		// Implementation now matches lattice prediction - returns BigComplex
		actualType := reflect.TypeOf(result).String()
		c.Assert(actualType, qt.Equals, "*values.BigComplex")

		// The BigFloat precision is preserved in the BigComplex result
		c.Logf("Precision preserved: BigFloat with 256-bit precision kept in BigComplex")
	})

	// BigInteger + Float LOSES precision, deliberately. This subtest used to assert
	// the opposite ("preserves precision via BigFloat"), and reversing it is the
	// point of the exactness-contagion fix, not collateral damage from it.
	//
	// R7RS §6.2.2: exact + inexact = inexact. "Inexact" is a promise that precision
	// MAY be lost, and contagion is how it is lost — the exact operand is absorbed
	// into the float64. Chez agrees: (+ 1.5 (expt 2 2000)) is +inf.0, not a precise
	// bignum-float.
	//
	// The old rule promoted to BigFloat "to preserve precision," on the theory that
	// Simplify would demote afterwards. Per-op demotion was never wired, so ordinary
	// float arithmetic minted 256-bit bignums that never came back down — (+ 1.5 2)
	// was a *BigFloat. A program that WANTS the precision must stay exact, or ask for
	// it with an explicit BigFloat operand (see the BigFloat subtest below, which
	// still preserves).
	c.Run("BigInteger+Float_loses_precision_by_contagion", func(c *qt.C) {
		// An integer larger than float64 can exactly represent.
		largeInt := new(big.Int)
		largeInt.SetString("9999999999999999999999999999", 10)
		bi := values.NewBigInteger(largeInt)

		result := bi.Add(values.NewFloat(1.0))

		actualType := reflect.TypeOf(result).String()
		c.Assert(actualType, qt.Equals, "*values.Float",
			qt.Commentf("exact + Float must contaminate to Float, not escalate to BigFloat"))

		// And it agrees with the model.
		latticePrediction := ExpectedResultType(Join(Classify(bi), Classify(values.NewFloat(1.0))))
		c.Assert(latticePrediction, qt.Equals, "*values.Float")
	})

	// The precision the arithmetic gives up is NOT given up by COMPARISON. This is
	// the other half of the contagion fix and the reason there are two tables: the
	// result of an arithmetic op is inexact anyway, so rounding an operand costs
	// nothing already promised. The result of a comparison is a BOOLEAN, and the
	// rounding would decide it.
	c.Run("comparison_does_not_round_the_exact_operand", func(c *qt.C) {
		// 2^100 and 2^100 - 1 round to the SAME float64. A lossy comparison would
		// call them equal. They are not, and Chez says so too.
		twoTo100 := new(big.Int).Lsh(big.NewInt(1), 100)
		exact := values.NewBigInteger(twoTo100)

		minusOne := new(big.Int).Sub(twoTo100, big.NewInt(1))
		exactMinusOne := values.NewBigInteger(minusOne)

		inexact := exact.ToInexact()

		c.Assert(exact.Compare(inexact), qt.Equals, 0,
			qt.Commentf("2^100 compares equal to its own inexact image"))
		c.Assert(exactMinusOne.Compare(inexact) < 0, qt.IsTrue,
			qt.Commentf("2^100-1 must compare LESS than (exact->inexact 2^100); "+
				"if the comparison rounded the bignum to float64 both would collapse to equal"))

		// The comparison table is what buys this, and it differs from the arithmetic
		// table on exactly this pair.
		c.Assert(values.ComparisonResultKind(values.KindBigInteger, values.KindFloat),
			qt.Equals, values.KindBigFloat)
		c.Assert(values.PromotionResultKind(values.KindBigInteger, values.KindFloat),
			qt.Equals, values.KindFloat)
	})
}
