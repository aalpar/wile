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

package core_test

import (
	"testing"

	"github.com/aalpar/wile/go/values"

	qt "github.com/frankban/quicktest"
)

// ----------------------------------------------------------------------------
// Phase 2: Numeric Predicates & Comparisons
// R7RS §6.2.6 - Numerical operations
// ----------------------------------------------------------------------------

// TestZeroQ tests the zero? predicate.
// R7RS §6.2.6: (zero? z) returns #t if z is zero.
func TestZeroQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		// Integer
		{"zero? on 0", `(zero? 0)`, values.TrueValue},
		{"zero? on positive", `(zero? 5)`, values.FalseValue},
		{"zero? on negative", `(zero? -5)`, values.FalseValue},

		// Float
		{"zero? on 0.0", `(zero? 0.0)`, values.TrueValue},
		{"zero? on positive float", `(zero? 3.14)`, values.FalseValue},
		{"zero? on negative float", `(zero? -3.14)`, values.FalseValue},

		// Rational
		{"zero? on 0/1", `(zero? 0/1)`, values.TrueValue},
		{"zero? on positive rational", `(zero? 1/2)`, values.FalseValue},
		{"zero? on negative rational", `(zero? -1/2)`, values.FalseValue},

		// Complex
		{"zero? on 0+0i", `(zero? 0+0i)`, values.TrueValue},
		{"zero? on 0+1i", `(zero? 0+1i)`, values.FalseValue},
		{"zero? on 1+0i", `(zero? 1+0i)`, values.FalseValue},

		// BigInteger
		{"zero? on bigint 0", `(zero? #z0)`, values.TrueValue},
		{"zero? on bigint positive", `(zero? #z12345678901234567890)`, values.FalseValue},
		{"zero? on bigint negative", `(zero? #z-12345678901234567890)`, values.FalseValue},

		// BigFloat
		{"zero? on bigfloat 0", `(zero? #m0.0)`, values.TrueValue},
		{"zero? on bigfloat positive", `(zero? #m3.14159265358979323846)`, values.FalseValue},
		{"zero? on bigfloat negative", `(zero? #m-2.71828182845904523536)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.out)
		})
	}
}

// TestOddQ tests the odd? predicate.
// R7RS §6.2.6: (odd? n) returns #t if n is odd.
func TestOddQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		// Integer
		{"odd? on 1", `(odd? 1)`, values.TrueValue},
		{"odd? on 2", `(odd? 2)`, values.FalseValue},
		{"odd? on 0", `(odd? 0)`, values.FalseValue},
		{"odd? on -1", `(odd? -1)`, values.TrueValue},
		{"odd? on -2", `(odd? -2)`, values.FalseValue},
		{"odd? on large odd", `(odd? 999999999)`, values.TrueValue},
		{"odd? on large even", `(odd? 1000000000)`, values.FalseValue},

		// BigInteger
		{"odd? on bigint odd", `(odd? #z12345678901234567891)`, values.TrueValue},
		{"odd? on bigint even", `(odd? #z12345678901234567890)`, values.FalseValue},

		// Inexact integer (Float that is mathematically an integer)
		{"odd? on 3.0", `(odd? 3.0)`, values.TrueValue},
		{"odd? on 4.0", `(odd? 4.0)`, values.FalseValue},

		// BigFloat (that is mathematically an integer)
		{"odd? on bigfloat 3.0", `(odd? #m3.0)`, values.TrueValue},
		{"odd? on bigfloat 4.0", `(odd? #m4.0)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.out)
		})
	}
}

// TestOddQ_Errors tests error cases for odd?.
func TestOddQ_Errors(t *testing.T) {
	t.Run("odd? on non-integer float", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(odd? 3.5)`)
	})
	t.Run("odd? on non-integer bigfloat", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(odd? #m3.5)`)
	})
	t.Run("odd? on rational", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(odd? 1/2)`)
	})
	t.Run("odd? on complex", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(odd? 1+2i)`)
	})
}

// TestEvenQ tests the even? predicate.
// R7RS §6.2.6: (even? n) returns #t if n is even.
func TestEvenQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		// Integer
		{"even? on 0", `(even? 0)`, values.TrueValue},
		{"even? on 2", `(even? 2)`, values.TrueValue},
		{"even? on 1", `(even? 1)`, values.FalseValue},
		{"even? on -2", `(even? -2)`, values.TrueValue},
		{"even? on -1", `(even? -1)`, values.FalseValue},
		{"even? on large even", `(even? 1000000000)`, values.TrueValue},
		{"even? on large odd", `(even? 999999999)`, values.FalseValue},

		// BigInteger
		{"even? on bigint even", `(even? #z12345678901234567890)`, values.TrueValue},
		{"even? on bigint odd", `(even? #z12345678901234567891)`, values.FalseValue},

		// Inexact integer
		{"even? on 4.0", `(even? 4.0)`, values.TrueValue},
		{"even? on 3.0", `(even? 3.0)`, values.FalseValue},

		// BigFloat (that is mathematically an integer)
		{"even? on bigfloat 4.0", `(even? #m4.0)`, values.TrueValue},
		{"even? on bigfloat 3.0", `(even? #m3.0)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.out)
		})
	}
}

// TestEvenQ_Errors tests error cases for even?.
func TestEvenQ_Errors(t *testing.T) {
	t.Run("even? on non-integer float", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(even? 3.5)`)
	})
	t.Run("even? on non-integer bigfloat", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(even? #m3.5)`)
	})
	t.Run("even? on rational", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(even? 1/2)`)
	})
	t.Run("even? on complex", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(even? 1+2i)`)
	})
}

// ----------------------------------------------------------------------------
// Numeric Comparison Tests
// R7RS §6.2.6: =, <, >, <=, >=
// ----------------------------------------------------------------------------

// TestNumericEquals tests the = comparison.
// R7RS §6.2.6: (= z1 z2 z3 ...) returns #t if all arguments are numerically equal.
func TestNumericEquals(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		// Basic equality
		{"= two equal integers", `(= 5 5)`, values.TrueValue},
		{"= two unequal integers", `(= 5 3)`, values.FalseValue},
		{"= single argument", `(= 5)`, values.TrueValue},

		// Chain
		{"= chain equal", `(= 5 5 5 5)`, values.TrueValue},
		{"= chain unequal", `(= 5 5 5 6)`, values.FalseValue},

		// Mixed types
		{"= integer and float", `(= 5 5.0)`, values.TrueValue},
		{"= integer and rational", `(= 4 4/1)`, values.TrueValue},
		{"= float and rational", `(= 0.5 1/2)`, values.TrueValue},
		{"= integer and complex", `(= 5 5+0i)`, values.TrueValue},

		// BigInteger
		{"= two equal bigints", `(= #z12345678901234567890 #z12345678901234567890)`, values.TrueValue},
		{"= two unequal bigints", `(= #z12345678901234567890 #z12345678901234567891)`, values.FalseValue},

		// Special values
		{"= +inf.0 +inf.0", `(= +inf.0 +inf.0)`, values.TrueValue},
		{"= -inf.0 -inf.0", `(= -inf.0 -inf.0)`, values.TrueValue},
		{"= +inf.0 -inf.0", `(= +inf.0 -inf.0)`, values.FalseValue},
		// NaN is never equal to anything, including itself
		{"= +nan.0 +nan.0", `(= +nan.0 +nan.0)`, values.FalseValue},

		// Precision boundary tests at 2^53
		// 2^53 = 9007199254740992 is the largest integer that can be exactly
		// represented as a float64. Values above this lose precision.
		{"= at 2^53 boundary exact", `(= 9007199254740992 9007199254740992.0)`, values.TrueValue},
		{"= 2^53+1 vs 2^53 float", `(= 9007199254740993 9007199254740992.0)`, values.FalseValue},
		{"= 2^53 float vs 2^53+1", `(= 9007199254740992.0 9007199254740993)`, values.FalseValue},
		{"= negative boundary", `(= -9007199254740993 -9007199254740992.0)`, values.FalseValue},
		// Integer to float where float is non-integer
		{"= integer and non-integer float", `(= 5 5.5)`, values.FalseValue},
		// Integer to infinity
		{"= integer and +inf.0", `(= 1000000 +inf.0)`, values.FalseValue},
		{"= integer and -inf.0", `(= -1000000 -inf.0)`, values.FalseValue},
		// Integer to NaN
		{"= integer and +nan.0", `(= 42 +nan.0)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.out)
		})
	}
}

// TestNumericLessThan tests the < comparison.
// R7RS §6.2.6: (< x1 x2 x3 ...) returns #t if arguments are monotonically increasing.
func TestNumericLessThan(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		// Basic
		{"< 1 2", `(< 1 2)`, values.TrueValue},
		{"< 2 1", `(< 2 1)`, values.FalseValue},
		{"< equal", `(< 2 2)`, values.FalseValue},
		{"< single", `(< 5)`, values.TrueValue},

		// Chain
		{"< 1 2 3", `(< 1 2 3)`, values.TrueValue},
		{"< 1 3 2", `(< 1 3 2)`, values.FalseValue},
		{"< 1 2 2", `(< 1 2 2)`, values.FalseValue},

		// Mixed types
		{"< integer float", `(< 1 1.5)`, values.TrueValue},
		{"< float integer", `(< 1.5 2)`, values.TrueValue},
		{"< integer rational", `(< 0 1/2)`, values.TrueValue},
		{"< rational integer", `(< 1/2 1)`, values.TrueValue},

		// BigInteger
		{"< bigint", `(< #z12345678901234567890 #z12345678901234567891)`, values.TrueValue},
		{"< bigint reverse", `(< #z12345678901234567891 #z12345678901234567890)`, values.FalseValue},

		// Special values
		{"< -inf.0 0", `(< -inf.0 0)`, values.TrueValue},
		{"< 0 +inf.0", `(< 0 +inf.0)`, values.TrueValue},
		{"< -inf.0 +inf.0", `(< -inf.0 +inf.0)`, values.TrueValue},
		// NaN comparisons are always false
		{"< 1 +nan.0", `(< 1 +nan.0)`, values.FalseValue},
		{"< +nan.0 1", `(< +nan.0 1)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.out)
		})
	}
}

// TestNumericGreaterThan tests the > comparison.
// R7RS §6.2.6: (> x1 x2 x3 ...) returns #t if arguments are monotonically decreasing.
func TestNumericGreaterThan(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		// Basic
		{"> 2 1", `(> 2 1)`, values.TrueValue},
		{"> 1 2", `(> 1 2)`, values.FalseValue},
		{"> equal", `(> 2 2)`, values.FalseValue},
		{"> single", `(> 5)`, values.TrueValue},

		// Chain
		{"> 3 2 1", `(> 3 2 1)`, values.TrueValue},
		{"> 3 1 2", `(> 3 1 2)`, values.FalseValue},
		{"> 3 2 2", `(> 3 2 2)`, values.FalseValue},

		// Mixed types
		{"> float integer", `(> 1.5 1)`, values.TrueValue},
		{"> integer float", `(> 2 1.5)`, values.TrueValue},
		{"> rational integer", `(> 1/2 0)`, values.TrueValue},

		// BigInteger
		{"> bigint", `(> #z12345678901234567891 #z12345678901234567890)`, values.TrueValue},

		// Special values
		{"> +inf.0 0", `(> +inf.0 0)`, values.TrueValue},
		{"> 0 -inf.0", `(> 0 -inf.0)`, values.TrueValue},
		// NaN comparisons are always false
		{"> 1 +nan.0", `(> 1 +nan.0)`, values.FalseValue},
		{"> +nan.0 1", `(> +nan.0 1)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.out)
		})
	}
}

// TestNumericLessThanOrEqual tests the <= comparison.
// R7RS §6.2.6: (<= x1 x2 x3 ...) returns #t if arguments are monotonically non-decreasing.
func TestNumericLessThanOrEqual(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		// Basic
		{"<= 1 2", `(<= 1 2)`, values.TrueValue},
		{"<= 2 2", `(<= 2 2)`, values.TrueValue},
		{"<= 2 1", `(<= 2 1)`, values.FalseValue},
		{"<= single", `(<= 5)`, values.TrueValue},

		// Chain
		{"<= 1 2 3", `(<= 1 2 3)`, values.TrueValue},
		{"<= 1 2 2", `(<= 1 2 2)`, values.TrueValue},
		{"<= 1 3 2", `(<= 1 3 2)`, values.FalseValue},

		// Mixed types
		{"<= integer float", `(<= 1 1.0)`, values.TrueValue},
		{"<= rational integer", `(<= 1/2 1)`, values.TrueValue},

		// BigInteger
		{"<= bigint equal", `(<= #z12345678901234567890 #z12345678901234567890)`, values.TrueValue},
		{"<= bigint less", `(<= #z12345678901234567890 #z12345678901234567891)`, values.TrueValue},

		// Special values
		{"<= -inf.0 +inf.0", `(<= -inf.0 +inf.0)`, values.TrueValue},
		{"<= +inf.0 +inf.0", `(<= +inf.0 +inf.0)`, values.TrueValue},
		// NaN comparisons are always false
		{"<= 1 +nan.0", `(<= 1 +nan.0)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.out)
		})
	}
}

// TestNumericGreaterThanOrEqual tests the >= comparison.
// R7RS §6.2.6: (>= x1 x2 x3 ...) returns #t if arguments are monotonically non-increasing.
func TestNumericGreaterThanOrEqual(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		// Basic
		{">= 2 1", `(>= 2 1)`, values.TrueValue},
		{">= 2 2", `(>= 2 2)`, values.TrueValue},
		{">= 1 2", `(>= 1 2)`, values.FalseValue},
		{">= single", `(>= 5)`, values.TrueValue},

		// Chain
		{">= 3 2 1", `(>= 3 2 1)`, values.TrueValue},
		{">= 3 2 2", `(>= 3 2 2)`, values.TrueValue},
		{">= 3 1 2", `(>= 3 1 2)`, values.FalseValue},

		// Mixed types
		{">= float integer", `(>= 1.0 1)`, values.TrueValue},
		{">= integer rational", `(>= 1 1/2)`, values.TrueValue},

		// BigInteger
		{">= bigint equal", `(>= #z12345678901234567890 #z12345678901234567890)`, values.TrueValue},
		{">= bigint greater", `(>= #z12345678901234567891 #z12345678901234567890)`, values.TrueValue},

		// Special values
		{">= +inf.0 -inf.0", `(>= +inf.0 -inf.0)`, values.TrueValue},
		{">= -inf.0 -inf.0", `(>= -inf.0 -inf.0)`, values.TrueValue},
		// NaN comparisons are always false
		{">= 1 +nan.0", `(>= 1 +nan.0)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.out)
		})
	}
}

// TestComparison_NonRealComplex tests that ordering comparisons reject non-real complex numbers.
// R7RS §6.2.6: <, >, <=, >= require real arguments.
func TestComparison_NonRealComplex(t *testing.T) {
	// Non-real complex should error for ordering comparisons
	t.Run("< with non-real complex", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(< 1+1i 2)`)
	})
	t.Run("> with non-real complex", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(> 1+1i 2)`)
	})
	t.Run("<= with non-real complex", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(<= 1+1i 2)`)
	})
	t.Run(">= with non-real complex", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(>= 1+1i 2)`)
	})

	// = allows complex (R7RS §6.2.6: = works on all numbers)
	t.Run("= with complex", func(t *testing.T) {
		result, err := runSchemeCode(t, `(= 1+1i 1+1i)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, qt.Equals, values.TrueValue)
	})

	// Real complex (zero imaginary) should work with ordering comparisons
	t.Run("< with real complex", func(t *testing.T) {
		result, err := runSchemeCode(t, `(< 1+0i 2+0i)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, qt.Equals, values.TrueValue)
	})
}

// TestComparison_TypeErrors tests that numeric comparisons reject non-numeric arguments.
// R7RS §6.2.6: =, <, >, <=, >= require numeric arguments.
func TestComparison_TypeErrors(t *testing.T) {
	t.Run("= string first arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(= "hello" 1)`)
	})
	t.Run("= boolean first arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(= #t 1)`)
	})
	t.Run("< string first arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(< "hello" 1)`)
	})
	t.Run("< boolean second arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(< 1 #t)`)
	})
	t.Run("> string first arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(> "hello" 1)`)
	})
	t.Run("> boolean second arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(> 1 #t)`)
	})
	t.Run("<= string first arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(<= "hello" 1)`)
	})
	t.Run("<= boolean second arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(<= 1 #t)`)
	})
	t.Run(">= string first arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(>= "hello" 1)`)
	})
	t.Run(">= boolean second arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(>= 1 #t)`)
	})
}

// ----------------------------------------------------------------------------
// Enhanced Type Predicate Tests with BigInteger
// ----------------------------------------------------------------------------

// TestExactnessPredicatesExtended adds BigInteger and BigFloat coverage.
func TestExactnessPredicatesExtended(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		// exact? on BigInteger
		{"exact? on bigint", `(exact? #z12345678901234567890)`, values.TrueValue},

		// inexact? on BigInteger
		{"inexact? on bigint", `(inexact? #z12345678901234567890)`, values.FalseValue},

		// exact-integer? on BigInteger
		{"exact-integer? on bigint", `(exact-integer? #z12345678901234567890)`, values.TrueValue},

		// exact? on BigFloat (inexact)
		{"exact? on bigfloat", `(exact? #m3.14159265358979323846)`, values.FalseValue},

		// inexact? on BigFloat
		{"inexact? on bigfloat", `(inexact? #m3.14159265358979323846)`, values.TrueValue},

		// exact-integer? on BigFloat (always false - inexact)
		{"exact-integer? on bigfloat", `(exact-integer? #m4.0)`, values.FalseValue},

		// exact? on exact complex (integer parts are parsed as exact BigComplex)
		{"exact? on complex", `(exact? 1+2i)`, values.TrueValue},

		// inexact? on exact complex
		{"inexact? on complex", `(inexact? 1+2i)`, values.FalseValue},

		// exact? on inexact complex (float parts)
		{"exact? on inexact complex", `(exact? 1.0+2.0i)`, values.FalseValue},

		// inexact? on inexact complex (float parts)
		{"inexact? on inexact complex", `(inexact? 1.0+2.0i)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.out)
		})
	}
}

// TestSignPredicatesExtended adds BigInteger, BigFloat, and special value coverage.
func TestSignPredicatesExtended(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		// BigInteger
		{"positive? on positive bigint", `(positive? #z12345678901234567890)`, values.TrueValue},
		{"positive? on negative bigint", `(positive? #z-12345678901234567890)`, values.FalseValue},
		{"negative? on positive bigint", `(negative? #z12345678901234567890)`, values.FalseValue},
		{"negative? on negative bigint", `(negative? #z-12345678901234567890)`, values.TrueValue},

		// BigFloat
		{"positive? on positive bigfloat", `(positive? #m3.14)`, values.TrueValue},
		{"positive? on negative bigfloat", `(positive? #m-3.14)`, values.FalseValue},
		{"negative? on positive bigfloat", `(negative? #m3.14)`, values.FalseValue},
		{"negative? on negative bigfloat", `(negative? #m-3.14)`, values.TrueValue},

		// Special values
		{"positive? on +inf.0", `(positive? +inf.0)`, values.TrueValue},
		{"positive? on -inf.0", `(positive? -inf.0)`, values.FalseValue},
		{"negative? on +inf.0", `(negative? +inf.0)`, values.FalseValue},
		{"negative? on -inf.0", `(negative? -inf.0)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.out)
		})
	}
}

// TestSpecialValuePredicatesExtended adds BigInteger and BigFloat coverage.
func TestSpecialValuePredicatesExtended(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		// finite? on BigInteger
		{"finite? on bigint", `(finite? #z12345678901234567890)`, values.TrueValue},

		// infinite? on BigInteger
		{"infinite? on bigint", `(infinite? #z12345678901234567890)`, values.FalseValue},

		// nan? on BigInteger
		{"nan? on bigint", `(nan? #z12345678901234567890)`, values.FalseValue},

		// finite? on BigFloat (always true - big.Float has no Inf/NaN)
		{"finite? on bigfloat", `(finite? #m3.14159265358979323846)`, values.TrueValue},

		// infinite? on BigFloat (always false)
		{"infinite? on bigfloat", `(infinite? #m3.14159265358979323846)`, values.FalseValue},

		// nan? on BigFloat (always false)
		{"nan? on bigfloat", `(nan? #m3.14159265358979323846)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.out)
		})
	}
}

// TestTypeTowerPredicates tests the numeric tower predicates.
// R7RS §6.2.6: number?, complex?, real?, rational?, integer?
func TestTypeTowerPredicates(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		// All numeric types satisfy number?
		{"number? on integer", `(number? 42)`, values.TrueValue},
		{"number? on float", `(number? 3.14)`, values.TrueValue},
		{"number? on rational", `(number? 1/2)`, values.TrueValue},
		{"number? on complex", `(number? 1+2i)`, values.TrueValue},
		{"number? on bigint", `(number? #z12345678901234567890)`, values.TrueValue},
		{"number? on bigfloat", `(number? #m3.14159265358979323846)`, values.TrueValue},
		{"number? on string", `(number? "hello")`, values.FalseValue},

		// complex? - all numbers are complex
		{"complex? on integer", `(complex? 42)`, values.TrueValue},
		{"complex? on float", `(complex? 3.14)`, values.TrueValue},
		{"complex? on complex", `(complex? 1+2i)`, values.TrueValue},
		{"complex? on bigint", `(complex? #z12345678901234567890)`, values.TrueValue},
		{"complex? on bigfloat", `(complex? #m3.14)`, values.TrueValue},

		// real? - excludes complex with non-zero imaginary
		{"real? on integer", `(real? 42)`, values.TrueValue},
		{"real? on float", `(real? 3.14)`, values.TrueValue},
		{"real? on rational", `(real? 1/2)`, values.TrueValue},
		{"real? on bigint", `(real? #z12345678901234567890)`, values.TrueValue},
		{"real? on bigfloat", `(real? #m3.14)`, values.TrueValue},
		{"real? on complex with imag", `(real? 1+2i)`, values.FalseValue},
		{"real? on complex without imag", `(real? 1+0i)`, values.TrueValue},

		// rational? - excludes inf, nan; BigFloat is always finite so always rational
		{"rational? on integer", `(rational? 42)`, values.TrueValue},
		{"rational? on rational", `(rational? 1/2)`, values.TrueValue},
		{"rational? on bigint", `(rational? #z12345678901234567890)`, values.TrueValue},
		{"rational? on bigfloat", `(rational? #m3.14)`, values.TrueValue},
		{"rational? on +inf.0", `(rational? +inf.0)`, values.FalseValue},
		{"rational? on +nan.0", `(rational? +nan.0)`, values.FalseValue},

		// integer? - includes inexact integers
		{"integer? on integer", `(integer? 42)`, values.TrueValue},
		{"integer? on bigint", `(integer? #z12345678901234567890)`, values.TrueValue},
		{"integer? on 3.0", `(integer? 3.0)`, values.TrueValue},
		{"integer? on 3.5", `(integer? 3.5)`, values.FalseValue},
		{"integer? on bigfloat 4.0", `(integer? #m4.0)`, values.TrueValue},
		{"integer? on bigfloat 3.5", `(integer? #m3.5)`, values.FalseValue},
		{"integer? on rational 2/1", `(integer? 2/1)`, values.TrueValue},
		{"integer? on rational 1/2", `(integer? 1/2)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.out)
		})
	}
}
