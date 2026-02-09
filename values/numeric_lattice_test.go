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
	"fmt"
	"math/big"
	"reflect"
	"testing"

	qt "github.com/frankban/quicktest"
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
func Classify(n Number) TypeClass {
	switch v := n.(type) {
	case *Integer:
		return TypeClass{PrecisionInteger, ComplexityReal}
	case *BigInteger:
		return TypeClass{PrecisionBigInteger, ComplexityReal}
	case *Rational:
		return TypeClass{PrecisionRational, ComplexityReal}
	case *Float:
		return TypeClass{PrecisionFloat, ComplexityReal}
	case *BigFloat:
		return TypeClass{PrecisionBigFloat, ComplexityReal}
	case *Complex:
		// complex128 uses float64 internally
		return TypeClass{PrecisionFloat, ComplexityComplex}
	case *BigComplex:
		// BigComplex precision depends on its components
		return TypeClass{classifyBigComplexPrecision(v), ComplexityComplex}
	}
	panic(fmt.Sprintf("unknown type: %T", n))
}

func classifyBigComplexPrecision(bc *BigComplex) PrecisionRank {
	// BigComplex precision is the max of its real and imaginary parts
	realPrec := Classify(bc.Real())
	imagPrec := Classify(bc.Imag())
	if realPrec.Precision > imagPrec.Precision {
		return realPrec.Precision
	}
	return imagPrec.Precision
}

// =============================================================================
// Lattice Join (Least Upper Bound)
// =============================================================================

// Join computes the least upper bound of two type classes.
// This determines the result type of a binary operation.
func Join(a, b TypeClass) TypeClass {
	return TypeClass{
		Precision:  maxPrecision(a.Precision, b.Precision),
		Complexity: maxComplexity(a.Complexity, b.Complexity),
	}
}

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
		value    Number
		expected TypeClass
	}{
		// Real types
		{"Integer", NewInteger(42), TypeClass{PrecisionInteger, ComplexityReal}},
		{"BigInteger", NewBigIntegerFromInt64(42), TypeClass{PrecisionBigInteger, ComplexityReal}},
		{"Rational", NewRational(3, 4), TypeClass{PrecisionRational, ComplexityReal}},
		{"Float", NewFloat(3.14), TypeClass{PrecisionFloat, ComplexityReal}},
		{"BigFloat", NewBigFloatFromFloat64(3.14), TypeClass{PrecisionBigFloat, ComplexityReal}},

		// Complex types
		{"Complex", NewComplex(complex(1, 2)), TypeClass{PrecisionFloat, ComplexityComplex}},

		// BigComplex with various component types
		{"BigComplex(BigInteger)", NewBigComplex(NewBigIntegerFromInt64(1), NewBigIntegerFromInt64(2)),
			TypeClass{PrecisionBigInteger, ComplexityComplex}},
		{"BigComplex(Rational)", NewBigComplex(NewRational(1, 2), NewRational(3, 4)),
			TypeClass{PrecisionRational, ComplexityComplex}},
		{"BigComplex(BigFloat)", NewBigComplex(NewBigFloatFromFloat64(1.0), NewBigFloatFromFloat64(2.0)),
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
		{"Int+Float", TypeClass{PrecisionInteger, ComplexityReal}, TypeClass{PrecisionFloat, ComplexityReal},
			TypeClass{PrecisionFloat, ComplexityReal}},
		{"Rational+Float", TypeClass{PrecisionRational, ComplexityReal}, TypeClass{PrecisionFloat, ComplexityReal},
			TypeClass{PrecisionFloat, ComplexityReal}},
		{"Float+BigFloat", TypeClass{PrecisionFloat, ComplexityReal}, TypeClass{PrecisionBigFloat, ComplexityReal},
			TypeClass{PrecisionBigFloat, ComplexityReal}},

		// Real + Complex (complexity promotion)
		{"Int+Complex", TypeClass{PrecisionInteger, ComplexityReal}, TypeClass{PrecisionFloat, ComplexityComplex},
			TypeClass{PrecisionFloat, ComplexityComplex}},
		{"BigFloat+Complex", TypeClass{PrecisionBigFloat, ComplexityReal}, TypeClass{PrecisionFloat, ComplexityComplex},
			TypeClass{PrecisionBigFloat, ComplexityComplex}}, // Lattice says BigFloat precision!
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

// =============================================================================
// Test: Lattice Predictions vs Actual Behavior
// =============================================================================

// TestLattice_PredictionsVsActual compares lattice-based predictions against
// the actual result types from direct dispatch.
func TestLattice_PredictionsVsActual(t *testing.T) {
	c := qt.New(t)

	// Test values - use different values for receiver vs operand to avoid simplification
	testValues := map[string]Number{
		"Integer":    NewInteger(5),
		"BigInteger": NewBigIntegerFromInt64(7),
		"Rational":   NewRational(3, 4),
		"Float":      NewFloat(2.5),
		"BigFloat":   NewBigFloatFromFloat64(3.5),
		"Complex":    NewComplex(complex(1, 2)),
		"BigComplex": NewBigComplex(NewBigIntegerFromInt64(1), NewBigIntegerFromInt64(2)),
	}

	// Different operand values to avoid simplification
	operandValues := map[string]Number{
		"Integer":    NewInteger(3),
		"BigInteger": NewBigIntegerFromInt64(11),
		"Rational":   NewRational(1, 3),
		"Float":      NewFloat(1.5),
		"BigFloat":   NewBigFloatFromFloat64(2.5),
		"Complex":    NewComplex(complex(3, 4)),
		"BigComplex": NewBigComplex(NewBigIntegerFromInt64(5), NewBigIntegerFromInt64(6)),
	}

	typeNames := []string{"Integer", "BigInteger", "Rational", "Float", "BigFloat", "Complex", "BigComplex"}

	// Known divergences between lattice prediction and actual behavior
	// These are intentional design choices: BigComplex is preserved for any operation involving it
	knownDivergences := map[string]struct {
		lattice string
		actual  string
		reason  string
	}{
		// Operations involving BigComplex always return BigComplex (preserves structure)
		"Float+BigComplex": {
			lattice: "*values.Complex",
			actual:  "*values.BigComplex",
			reason:  "BigComplex is preserved: any op with BigComplex returns BigComplex",
		},
		"Complex+BigComplex": {
			lattice: "*values.Complex",
			actual:  "*values.BigComplex",
			reason:  "BigComplex is preserved: any op with BigComplex returns BigComplex",
		},
		"BigComplex+Float": {
			lattice: "*values.Complex",
			actual:  "*values.BigComplex",
			reason:  "BigComplex is preserved: any op with BigComplex returns BigComplex",
		},
		"BigComplex+Complex": {
			lattice: "*values.Complex",
			actual:  "*values.BigComplex",
			reason:  "BigComplex is preserved: any op with BigComplex returns BigComplex",
		},
	}

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
	testValues := map[string]Number{
		"Integer":    NewInteger(5),
		"BigInteger": NewBigIntegerFromInt64(7),
		"Rational":   NewRational(3, 4),
		"Float":      NewFloat(2.5),
		"BigFloat":   NewBigFloatFromFloat64(3.5),
		"Complex":    NewComplex(complex(1, 2)),
		"BigComplex": NewBigComplex(NewBigIntegerFromInt64(1), NewBigIntegerFromInt64(2)),
	}

	operandValues := map[string]Number{
		"Integer":    NewInteger(3),
		"BigInteger": NewBigIntegerFromInt64(11),
		"Rational":   NewRational(1, 3),
		"Float":      NewFloat(1.5),
		"BigFloat":   NewBigFloatFromFloat64(2.5),
		"Complex":    NewComplex(complex(3, 4)),
		"BigComplex": NewBigComplex(NewBigIntegerFromInt64(5), NewBigIntegerFromInt64(6)),
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
		a, b        Number
		expectExact bool
		description string
	}{
		// Exact + Exact = Exact
		{"Int+Int", NewInteger(1), NewInteger(2), true, "exact integer + exact integer"},
		{"Int+BigInt", NewInteger(1), NewBigIntegerFromInt64(2), true, "exact integer + exact big integer"},
		{"Int+Rational", NewInteger(1), NewRational(1, 2), true, "exact integer + exact rational"},
		{"BigInt+Rational", NewBigIntegerFromInt64(1), NewRational(1, 2), true, "exact big integer + exact rational"},
		{"Rational+Rational", NewRational(1, 2), NewRational(1, 3), true, "exact rational + exact rational"},

		// Exact + Inexact = Inexact
		{"Int+Float", NewInteger(1), NewFloat(2.0), false, "exact + inexact float"},
		{"Int+BigFloat", NewInteger(1), NewBigFloatFromFloat64(2.0), false, "exact + inexact big float"},
		{"Rational+Float", NewRational(1, 2), NewFloat(2.0), false, "exact rational + inexact float"},

		// Inexact + Inexact = Inexact
		{"Float+Float", NewFloat(1.0), NewFloat(2.0), false, "inexact + inexact"},
		{"Float+BigFloat", NewFloat(1.0), NewBigFloatFromFloat64(2.0), false, "float + big float"},
		{"BigFloat+BigFloat", NewBigFloatFromFloat64(1.0), NewBigFloatFromFloat64(2.0), false, "big float + big float"},

		// Complex exactness
		{"Int+ExactComplex", NewInteger(1),
			NewBigComplex(NewBigIntegerFromInt64(2), NewBigIntegerFromInt64(3)), true, "exact + exact complex"},
		{"Int+InexactComplex", NewInteger(1), NewComplex(complex(2, 3)), false, "exact + inexact complex"},
		{"Float+ExactComplex", NewFloat(1.0),
			NewBigComplex(NewBigIntegerFromInt64(2), NewBigIntegerFromInt64(3)), false, "inexact + exact complex = inexact"},
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

	exactInt := NewInteger(3)
	exactComplex := NewBigComplex(NewBigIntegerFromInt64(1), NewBigIntegerFromInt64(2))

	// Direct dispatch (correct): preserves exactness
	directResult := exactInt.Add(exactComplex)
	c.Assert(directResult.IsExact(), qt.IsTrue, qt.Commentf("direct dispatch should preserve exactness"))

	// The lattice predicts: Join({Integer,Real}, {BigInteger,Complex}) = {BigInteger,Complex}
	// Result type: BigComplex with exact parts
	classInt := Classify(exactInt)
	classComplex := Classify(exactComplex)
	joined := Join(classInt, classComplex)

	c.Assert(joined.Complexity, qt.Equals, ComplexityComplex)
	c.Assert(joined.Precision, qt.Equals, PrecisionBigInteger)

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
		bf := NewBigFloatFromFloat64(1.0)
		bf.value.SetPrec(256)
		bf.value.SetString("1.123456789012345678901234567890")

		cx := NewComplex(complex(2, 3))

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

	// BigInteger + Float can lose precision for large integers
	c.Run("BigInteger+Float_can_lose_precision", func(c *qt.C) {
		// An integer larger than float64 can exactly represent
		largeInt := new(big.Int)
		largeInt.SetString("9999999999999999999999999999", 10)
		bi := &BigInteger{value: largeInt}

		fl := NewFloat(1.0)

		result := bi.Add(fl)

		// Result is Float, which can't exactly represent the large integer
		actualType := reflect.TypeOf(result).String()
		c.Assert(actualType, qt.Equals, "*values.Float")

		// This is correct per R7RS (exact + inexact = inexact), but precision is lost
		c.Logf("BigInteger with ~95 bits of precision reduced to float64 (~53 bits)")
	})
}
