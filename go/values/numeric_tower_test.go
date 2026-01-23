// Copyright 2025 Aaron Alpar
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
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestRank(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name     string
		number   Number
		expected NumericRank
	}{
		{"Integer", NewInteger(5), RankInteger},
		{"BigInteger", NewBigIntegerFromInt64(5), RankBigInteger},
		{"Rational", NewRational(1, 2), RankRational},
		{"Float", NewFloat(5.0), RankFloat},
		{"BigFloat", NewBigFloatFromFloat64(5.0), RankBigFloat},
		{"Complex", NewComplex(complex(5, 0)), RankComplex},
		{"BigComplex", NewBigComplexFromBigFloats(NewBigFloatFromFloat64(5), NewBigFloatFromFloat64(0)), RankBigComplex},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			c.Assert(Rank(tt.number), qt.Equals, tt.expected)
		})
	}
}

func TestRank_Order(t *testing.T) {
	c := qt.New(t)

	// Verify the ordering is correct
	c.Assert(RankInteger < RankBigInteger, qt.IsTrue)
	c.Assert(RankBigInteger < RankRational, qt.IsTrue)
	c.Assert(RankRational < RankFloat, qt.IsTrue)
	c.Assert(RankFloat < RankBigFloat, qt.IsTrue)
	c.Assert(RankBigFloat < RankComplex, qt.IsTrue)
	c.Assert(RankComplex < RankBigComplex, qt.IsTrue)
}

func TestPromoteOnce(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name         string
		input        Number
		expectedRank NumericRank
	}{
		{"Integer→BigInteger", NewInteger(5), RankBigInteger},
		{"BigInteger→Rational", NewBigIntegerFromInt64(5), RankRational},
		{"Rational→Float", NewRational(1, 2), RankFloat},
		{"Float→BigFloat", NewFloat(5.0), RankBigFloat},
		{"BigFloat→Complex", NewBigFloatFromFloat64(5.0), RankComplex},
		{"Complex→BigComplex", NewComplex(complex(5, 3)), RankBigComplex},
		{"BigComplex stays", NewBigComplexFromBigFloats(NewBigFloatFromFloat64(5), NewBigFloatFromFloat64(0)), RankBigComplex},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			result := promoteOnce(tt.input)
			c.Assert(Rank(result), qt.Equals, tt.expectedRank)
		})
	}
}

func TestPromote(t *testing.T) {
	c := qt.New(t)

	// Test promoting Integer to each level
	i := NewInteger(5)

	// Same rank - no change
	result := Promote(i, RankInteger)
	c.Assert(Rank(result), qt.Equals, RankInteger)

	// One level up
	result = Promote(i, RankBigInteger)
	c.Assert(Rank(result), qt.Equals, RankBigInteger)

	// Multiple levels up
	result = Promote(i, RankFloat)
	c.Assert(Rank(result), qt.Equals, RankFloat)

	// All the way to top
	result = Promote(i, RankBigComplex)
	c.Assert(Rank(result), qt.Equals, RankBigComplex)

	// Higher rank number not demoted
	bf := NewBigFloatFromFloat64(5.0)
	result = Promote(bf, RankInteger)
	c.Assert(Rank(result), qt.Equals, RankBigFloat) // Stays at BigFloat
}

func TestPromote_PreservesValue(t *testing.T) {
	c := qt.New(t)

	// Integer 5 promoted through the chain should still represent 5
	i := NewInteger(5)

	bi := Promote(i, RankBigInteger).(*BigInteger)
	c.Assert(bi.Int64(), qt.Equals, int64(5))

	r := Promote(i, RankRational).(*Rational)
	c.Assert(r.Float64(), qt.Equals, float64(5))

	f := Promote(i, RankFloat).(*Float)
	c.Assert(f.Value, qt.Equals, float64(5))

	bf := Promote(i, RankBigFloat).(*BigFloat)
	c.Assert(bf.Float64(), qt.Equals, float64(5))

	bc := Promote(i, RankBigComplex).(*BigComplex)
	c.Assert(bc.RealAsBigFloat().Float64(), qt.Equals, float64(5))
	c.Assert(bc.ImagAsBigFloat().Float64(), qt.Equals, float64(0))
}

func TestCommonRank(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name     string
		a, b     Number
		expected NumericRank
	}{
		{"Integer+Integer", NewInteger(1), NewInteger(2), RankInteger},
		{"Integer+Float", NewInteger(1), NewFloat(2.0), RankFloat},
		{"Float+Integer", NewFloat(1.0), NewInteger(2), RankFloat},
		{"BigInteger+Rational", NewBigIntegerFromInt64(1), NewRational(1, 2), RankRational},
		{"Complex+Integer", NewComplex(complex(1, 0)), NewInteger(2), RankComplex},
		{"BigComplex+anything", NewBigComplexFromBigFloats(NewBigFloatFromFloat64(1), NewBigFloatFromFloat64(0)), NewInteger(2), RankBigComplex},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			c.Assert(CommonRank(tt.a, tt.b), qt.Equals, tt.expected)
		})
	}
}

func TestPromoteBoth(t *testing.T) {
	c := qt.New(t)

	// Integer + Float should both become Float
	a, b := PromoteBoth(NewInteger(5), NewFloat(3.0))
	c.Assert(Rank(a), qt.Equals, RankFloat)
	c.Assert(Rank(b), qt.Equals, RankFloat)

	// BigInteger + Complex should both become Complex (Complex has higher rank)
	a, b = PromoteBoth(NewBigIntegerFromInt64(5), NewComplex(complex(3, 4)))
	c.Assert(Rank(a), qt.Equals, RankComplex)
	c.Assert(Rank(b), qt.Equals, RankComplex)
}

func TestSimplify(t *testing.T) {
	c := qt.New(t)

	// BigComplex with zero imag → simplifies
	bc := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(5), NewBigFloatFromFloat64(0))
	result := Simplify(bc)
	c.Assert(Rank(result), qt.Equals, RankInteger) // Should simplify all the way to Integer

	// Complex with zero imag → Float → possibly further
	cplx := NewComplex(complex(5, 0))
	result = Simplify(cplx)
	c.Assert(Rank(result), qt.Equals, RankInteger)

	// BigFloat that is integer → BigInteger or Integer
	bf := NewBigFloatFromFloat64(5.0)
	result = Simplify(bf)
	c.Assert(Rank(result), qt.Equals, RankInteger)

	// Rational that is integer → BigInteger or Integer
	r := NewRational(10, 2) // = 5
	result = Simplify(r)
	c.Assert(Rank(result), qt.Equals, RankInteger)

	// BigInteger that fits int64 → Integer
	bi := NewBigIntegerFromInt64(42)
	result = Simplify(bi)
	c.Assert(Rank(result), qt.Equals, RankInteger)

	// Non-simplifiable values stay the same
	r2 := NewRational(1, 3) // Not an integer
	result = Simplify(r2)
	c.Assert(Rank(result), qt.Equals, RankRational)

	cplx2 := NewComplex(complex(1, 2)) // Has imaginary part
	result = Simplify(cplx2)
	c.Assert(Rank(result), qt.Equals, RankComplex)
}

func TestExactnessOf(t *testing.T) {
	c := qt.New(t)

	// Exact types
	c.Assert(ExactnessOf(NewInteger(5)), qt.Equals, Exact)
	c.Assert(ExactnessOf(NewBigIntegerFromInt64(5)), qt.Equals, Exact)
	c.Assert(ExactnessOf(NewRational(1, 2)), qt.Equals, Exact)

	// Inexact types
	c.Assert(ExactnessOf(NewFloat(5.0)), qt.Equals, Inexact)
	c.Assert(ExactnessOf(NewBigFloatFromFloat64(5.0)), qt.Equals, Inexact)
	c.Assert(ExactnessOf(NewComplex(complex(5, 0))), qt.Equals, Inexact)

	// BigComplex depends on components
	exactBC := NewBigComplexFromBigIntegers(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(4))
	c.Assert(ExactnessOf(exactBC), qt.Equals, Exact)

	inexactBC := NewBigComplexFromBigFloats(NewBigFloatFromFloat64(3), NewBigFloatFromFloat64(4))
	c.Assert(ExactnessOf(inexactBC), qt.Equals, Inexact)
}

func TestResultExactness(t *testing.T) {
	c := qt.New(t)

	exact1 := NewInteger(5)
	exact2 := NewRational(1, 2)
	inexact1 := NewFloat(5.0)
	inexact2 := NewComplex(complex(1, 0))

	// exact op exact = exact
	c.Assert(ResultExactness(exact1, exact2), qt.Equals, Exact)

	// exact op inexact = inexact
	c.Assert(ResultExactness(exact1, inexact1), qt.Equals, Inexact)

	// inexact op exact = inexact
	c.Assert(ResultExactness(inexact1, exact1), qt.Equals, Inexact)

	// inexact op inexact = inexact
	c.Assert(ResultExactness(inexact1, inexact2), qt.Equals, Inexact)
}

func TestTowerAdd(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name     string
		a, b     Number
		expected string
	}{
		// Same-type operations
		{"Integer+Integer", NewInteger(3), NewInteger(4), "7"},
		{"Float+Float", NewFloat(3.5), NewFloat(4.5), "8"},
		{"Rational+Rational", NewRational(1, 2), NewRational(1, 3), "5/6"},
		{"Complex+Complex", NewComplex(complex(1, 2)), NewComplex(complex(3, 4)), "4+6i"},

		// Cross-type operations (should promote to common type)
		{"Integer+Float", NewInteger(3), NewFloat(4.5), "7.5"},
		{"Float+Integer", NewFloat(4.5), NewInteger(3), "7.5"},
		{"Integer+Rational", NewInteger(1), NewRational(1, 2), "3/2"},
		{"Rational+Float", NewRational(1, 2), NewFloat(0.5), "1"},
		{"Integer+Complex", NewInteger(3), NewComplex(complex(1, 2)), "4+2i"},

		// Result simplification
		{"Integer+Integer=0", NewInteger(3), NewInteger(-3), "0"},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			result := TowerAdd(tt.a, tt.b)
			c.Assert(result.SchemeString(), qt.Equals, tt.expected)
		})
	}
}

func TestTowerSubtract(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name     string
		a, b     Number
		expected string
	}{
		{"Integer-Integer", NewInteger(7), NewInteger(4), "3"},
		{"Float-Float", NewFloat(7.5), NewFloat(4.5), "3"},
		{"Integer-Float", NewInteger(7), NewFloat(4.5), "2.5"},
		{"Complex-Complex", NewComplex(complex(5, 6)), NewComplex(complex(1, 2)), "4+4i"},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			result := TowerSubtract(tt.a, tt.b)
			c.Assert(result.SchemeString(), qt.Equals, tt.expected)
		})
	}
}

func TestTowerMultiply(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name     string
		a, b     Number
		expected string
	}{
		{"Integer*Integer", NewInteger(3), NewInteger(4), "12"},
		{"Float*Float", NewFloat(2.0), NewFloat(3.5), "7"},
		{"Integer*Float", NewInteger(3), NewFloat(2.5), "7.5"},
		{"Rational*Rational", NewRational(2, 3), NewRational(3, 4), "1/2"},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			result := TowerMultiply(tt.a, tt.b)
			c.Assert(result.SchemeString(), qt.Equals, tt.expected)
		})
	}
}

func TestTowerDivide(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name     string
		a, b     Number
		expected string
	}{
		{"Integer/Integer exact", NewInteger(12), NewInteger(4), "3"},
		{"Integer/Integer rational", NewInteger(5), NewInteger(2), "5/2"},
		{"Float/Float", NewFloat(7.5), NewFloat(2.5), "3"},
		{"Integer/Float", NewInteger(7), NewFloat(2.0), "3.5"},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			result := TowerDivide(tt.a, tt.b)
			c.Assert(result.SchemeString(), qt.Equals, tt.expected)
		})
	}
}

func TestTowerDivide_ByZeroPanics(t *testing.T) {
	c := qt.New(t)

	c.Assert(func() { TowerDivide(NewInteger(5), NewInteger(0)) }, qt.PanicMatches, ".*division by zero.*")
	c.Assert(func() { TowerDivide(NewFloat(5.0), NewFloat(0)) }, qt.PanicMatches, ".*division by zero.*")
}

func TestTowerCompare(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name     string
		a, b     Number
		expected int
	}{
		{"Integer<Integer", NewInteger(3), NewInteger(5), -1},
		{"Integer=Integer", NewInteger(5), NewInteger(5), 0},
		{"Integer>Integer", NewInteger(7), NewInteger(5), 1},
		{"Integer<Float", NewInteger(3), NewFloat(5.0), -1},
		{"Float<Integer", NewFloat(3.0), NewInteger(5), -1},
		{"Rational=Integer", NewRational(10, 2), NewInteger(5), 0},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			result := TowerCompare(tt.a, tt.b)
			c.Assert(result, qt.Equals, tt.expected)
		})
	}
}

func TestBinaryOp_AllTypesCrossProduct(t *testing.T) {
	c := qt.New(t)

	// Create one representative of each type
	types := []Number{
		NewInteger(2),
		NewBigIntegerFromInt64(3),
		NewRational(4, 1),
		NewFloat(5.0),
		NewBigFloatFromFloat64(6.0),
		NewComplex(complex(7, 0)),
		NewBigComplexFromBigFloats(NewBigFloatFromFloat64(8), NewBigFloatFromFloat64(0)),
	}

	// Test that all 49 (7x7) combinations work without panic
	for _, a := range types {
		for _, b := range types {
			// Addition should work for all combinations
			result := TowerAdd(a, b)
			c.Assert(result, qt.IsNotNil)

			// Subtraction should work for all combinations
			result = TowerSubtract(a, b)
			c.Assert(result, qt.IsNotNil)

			// Multiplication should work for all combinations
			result = TowerMultiply(a, b)
			c.Assert(result, qt.IsNotNil)

			// Division should work for all combinations (none are zero)
			result = TowerDivide(a, b)
			c.Assert(result, qt.IsNotNil)

			// Comparison should work for all combinations
			cmp := TowerCompare(a, b)
			c.Assert(cmp >= -1 && cmp <= 1, qt.IsTrue)
		}
	}
}
