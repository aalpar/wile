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

package core_test

import (
	"errors"
	"math"
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

func TestAddition(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic integer operations
		{Name: "add two integers", Code: `(+ 1 2)`, Expected: values.NewInteger(3)},
		{Name: "add three integers", Code: `(+ 1 2 3)`, Expected: values.NewInteger(6)},
		{Name: "add single integer", Code: `(+ 5)`, Expected: values.NewInteger(5)},
		{Name: "add no arguments returns 0", Code: `(+)`, Expected: values.NewInteger(0)},
		{Name: "add negative numbers", Code: `(+ -5 3)`, Expected: values.NewInteger(-2)},

		// Float operations
		{Name: "add two floats", Code: `(+ 1.5 2.5)`, Expected: values.NewFloat(4.0)},
		{Name: "add float and integer", Code: `(+ 1 2.5)`, Expected: values.NewFloat(3.5)},
		{Name: "add integer and float", Code: `(+ 2.5 1)`, Expected: values.NewFloat(3.5)},

		// Rational operations
		{Name: "add two rationals", Code: `(+ 1/2 1/4)`, Expected: values.NewRational(3, 4)},
		{Name: "add rational and integer", Code: `(+ 1/2 1)`, Expected: values.NewRational(3, 2)},
		{Name: "add integer and rational", Code: `(+ 1 1/2)`, Expected: values.NewRational(3, 2)},
		{Name: "add rational and float", Code: `(+ 1/2 0.5)`, Expected: values.NewFloat(1.0)},

		// Complex operations
		// The literal 1+2i reads as an EXACT complex (R7RS §6.2.5), so exact complex
		// arithmetic yields an exact result — Chez agrees: (+ 1+2i 3+4i) => 4+6i, and
		// (exact? ...) => #t. These expectations were inexact float-backed complexes
		// and passed anyway, because the numeric EqualTo methods compared across
		// representations and ignored exactness entirely. Wile's carrier is BigComplex
		// where Chez uses a flonum complex; the VALUE and the EXACTNESS match.
		{Name: "add two complex", Code: `(+ 1+2i 3+4i)`, Expected: values.NewBigComplex(values.NewBigIntegerFromInt64(4), values.NewBigIntegerFromInt64(6))},
		{Name: "add complex and integer", Code: `(+ 1+2i 3)`, Expected: values.NewBigComplex(values.NewBigIntegerFromInt64(4), values.NewBigIntegerFromInt64(2))},
		{Name: "add complex and float", Code: `(+ 1+2i 1.5)`, Expected: values.NewBigComplexFromBigFloats(values.NewBigFloatFromFloat64(2.5), values.NewBigFloatFromFloat64(2.0))},

		// BigInteger operations
		{Name: "add two bigintegers", Code: `(+ #z10000000000000000000 #z1)`, Expected: values.NewBigIntegerFromString("10000000000000000001", 10)},
		{Name: "add biginteger and integer", Code: `(+ #z10000000000000000000 5)`, Expected: values.NewBigIntegerFromString("10000000000000000005", 10)},

		// Variadic
		{Name: "add many integers", Code: `(+ 1 2 3 4 5)`, Expected: values.NewInteger(15)},
		{Name: "add many mixed types", Code: `(+ 1 2.0 3/2)`, Expected: values.NewFloat(4.5)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestAddition_SpecialValues(t *testing.T) {
	// Test infinity and NaN behavior
	t.Run("add positive infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(+ 1 +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, 1), qt.IsTrue)
	})

	t.Run("add negative infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(+ 1 -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, -1), qt.IsTrue)
	})

	t.Run("add nan propagation", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(+ 1 +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})

	t.Run("infinity minus infinity is nan", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(+ +inf.0 -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})
}

func TestSubtraction(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic integer operations
		{Name: "subtract two integers", Code: `(- 5 2)`, Expected: values.NewInteger(3)},
		{Name: "negate single integer", Code: `(- 5)`, Expected: values.NewInteger(-5)},
		{Name: "subtract multiple integers", Code: `(- 10 3 2)`, Expected: values.NewInteger(5)},
		{Name: "subtract negative result", Code: `(- 1 5)`, Expected: values.NewInteger(-4)},
		{Name: "negate negative", Code: `(- -5)`, Expected: values.NewInteger(5)},

		// Float operations
		{Name: "subtract two floats", Code: `(- 5.5 2.5)`, Expected: values.NewFloat(3.0)},
		{Name: "negate float", Code: `(- 3.14)`, Expected: values.NewFloat(-3.14)},
		{Name: "subtract float and integer", Code: `(- 5.5 2)`, Expected: values.NewFloat(3.5)},
		{Name: "subtract integer and float", Code: `(- 5 2.5)`, Expected: values.NewFloat(2.5)},

		// Rational operations
		{Name: "subtract two rationals", Code: `(- 3/4 1/4)`, Expected: values.NewRational(1, 2)},
		{Name: "negate rational", Code: `(- 1/2)`, Expected: values.NewRational(-1, 2)},
		{Name: "subtract rational and integer", Code: `(- 3/2 1)`, Expected: values.NewRational(1, 2)},
		{Name: "subtract integer and rational", Code: `(- 2 1/2)`, Expected: values.NewRational(3, 2)},

		// Complex operations
		{Name: "subtract two complex", Code: `(- 5+6i 2+3i)`, Expected: values.NewBigComplex(values.NewBigIntegerFromInt64(3), values.NewBigIntegerFromInt64(3))},
		{Name: "negate complex", Code: `(- 1+2i)`, Expected: values.NewBigComplex(values.NewBigIntegerFromInt64(-1), values.NewBigIntegerFromInt64(-2))},
		{Name: "subtract complex and integer", Code: `(- 5+3i 2)`, Expected: values.NewBigComplex(values.NewBigIntegerFromInt64(3), values.NewBigIntegerFromInt64(3))},

		// BigInteger operations
		{Name: "subtract two bigintegers", Code: `(- #z10000000000000000005 #z5)`, Expected: values.NewBigIntegerFromString("10000000000000000000", 10)},
		{Name: "negate biginteger", Code: `(- #z10000000000000000000)`, Expected: values.NewBigIntegerFromString("-10000000000000000000", 10)},

		// Variadic
		{Name: "subtract many integers", Code: `(- 100 20 30 10)`, Expected: values.NewInteger(40)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestSubtraction_SpecialValues(t *testing.T) {
	t.Run("subtract from infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(- +inf.0 1)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, 1), qt.IsTrue)
	})

	t.Run("negate infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(- +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, -1), qt.IsTrue)
	})

	t.Run("subtract nan propagation", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(- +nan.0 1)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})

	// R7RS §6.2.6: unary (- x) is negation, not 0 - x. They diverge only at
	// inexact zero: 0 - 0.0 rounds to +0.0 (IEEE like-signed-zero subtraction),
	// but negate(0.0) flips the sign bit to -0.0. (eqv? -0.0 0.0) is *unspecified*
	// per R7RS §6.1, so observe the sign through division (1/-0.0 = -inf.0).
	t.Run("negate positive zero yields negative zero", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(/ 1.0 (- 0.0))`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, -1), qt.IsTrue)
	})

	t.Run("negate negative zero yields positive zero", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(/ 1.0 (- -0.0))`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, 1), qt.IsTrue)
	})
}

func TestSubtraction_Errors(t *testing.T) {
	t.Run("subtract no arguments", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(-)`)
	})
	t.Run("subtract string first arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(- "hello" 1)`)
	})
	t.Run("subtract boolean second arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(- 1 #t)`)
	})
}

func TestMultiplication(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic integer operations
		{Name: "multiply two integers", Code: `(* 3 4)`, Expected: values.NewInteger(12)},
		{Name: "multiply three integers", Code: `(* 2 3 4)`, Expected: values.NewInteger(24)},
		{Name: "multiply single integer", Code: `(* 7)`, Expected: values.NewInteger(7)},
		{Name: "multiply no arguments returns 1", Code: `(*)`, Expected: values.NewInteger(1)},
		{Name: "multiply by zero", Code: `(* 5 0)`, Expected: values.NewInteger(0)},
		{Name: "multiply negative numbers", Code: `(* -3 4)`, Expected: values.NewInteger(-12)},
		{Name: "multiply two negatives", Code: `(* -3 -4)`, Expected: values.NewInteger(12)},

		// Float operations
		{Name: "multiply two floats", Code: `(* 2.5 4.0)`, Expected: values.NewFloat(10.0)},
		{Name: "multiply float and integer", Code: `(* 2.5 4)`, Expected: values.NewFloat(10.0)},
		{Name: "multiply integer and float", Code: `(* 4 2.5)`, Expected: values.NewFloat(10.0)},
		{Name: "multiply float by zero", Code: `(* 3.14 0)`, Expected: values.NewInteger(0)}, // zero short-circuits to Integer

		// Rational operations
		{Name: "multiply two rationals", Code: `(* 1/2 2/3)`, Expected: values.NewRational(1, 3)},
		{Name: "multiply rational and integer", Code: `(* 1/2 4)`, Expected: values.NewRational(2, 1)}, // stays Rational
		{Name: "multiply integer and rational", Code: `(* 4 1/2)`, Expected: values.NewRational(2, 1)}, // stays Rational
		{Name: "multiply rational and float", Code: `(* 1/2 3.0)`, Expected: values.NewFloat(1.5)},

		// Complex operations
		{Name: "multiply two complex", Code: `(* 1+2i 3+4i)`, Expected: values.NewBigComplex(values.NewBigIntegerFromInt64(-5), values.NewBigIntegerFromInt64(10))},
		{Name: "multiply complex and integer", Code: `(* 2+3i 2)`, Expected: values.NewBigComplex(values.NewBigIntegerFromInt64(4), values.NewBigIntegerFromInt64(6))},
		{Name: "multiply complex and float", Code: `(* 1+1i 2.0)`, Expected: values.NewBigComplexFromBigFloats(values.NewBigFloatFromFloat64(2.0), values.NewBigFloatFromFloat64(2.0))},

		// BigInteger operations
		{Name: "multiply two bigintegers", Code: `(* #z10000000000000000000 #z2)`, Expected: values.NewBigIntegerFromString("20000000000000000000", 10)},
		{Name: "multiply biginteger and integer", Code: `(* #z10000000000000000000 3)`, Expected: values.NewBigIntegerFromString("30000000000000000000", 10)},

		// Variadic
		{Name: "multiply many integers", Code: `(* 2 3 4 5)`, Expected: values.NewInteger(120)},
		{Name: "multiply many mixed types", Code: `(* 2 3.0 1/2)`, Expected: values.NewFloat(3.0)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestMultiplication_SpecialValues(t *testing.T) {
	t.Run("multiply by infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(* 2 +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, 1), qt.IsTrue)
	})

	t.Run("multiply negative by infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(* -2 +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, -1), qt.IsTrue)
	})

	// An EXACT zero annihilates the product, beating infinity: (* 0 +inf.0) is
	// exact 0 in Chez and Racket. An exact 0 is a mathematical zero, not an
	// IEEE +0.0, so IEEE's 0*inf = NaN rule does not reach it.
	t.Run("exact zero times infinity is exact zero", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(* 0 +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		i, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue, qt.Commentf("want exact Integer, got %T", result))
		qt.Assert(t, i.Value, qt.Equals, int64(0))
	})

	// An INEXACT zero is an IEEE value, and IEEE 754 does govern it.
	t.Run("inexact zero times infinity is nan", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(* 0.0 +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})

	t.Run("multiply nan propagation", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(* 2 +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})
}

func TestDivision(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic integer operations
		{Name: "divide two integers", Code: `(/ 10 2)`, Expected: values.NewInteger(5)},
		{Name: "divide multiple integers", Code: `(/ 100 5 4)`, Expected: values.NewInteger(5)},
		{Name: "divide single integer returns reciprocal", Code: `(/ 5)`, Expected: values.NewRational(1, 5)},
		{Name: "divide single integer 1 returns integer", Code: `(/ 1)`, Expected: values.NewInteger(1)},
		{Name: "divide integers non-evenly returns rational", Code: `(/ 1 2)`, Expected: values.NewRational(1, 2)},
		{Name: "divide integers auto-simplifies rational", Code: `(/ 10 4)`, Expected: values.NewRational(5, 2)},
		{Name: "divide integers evenly returns integer", Code: `(/ 6 3)`, Expected: values.NewInteger(2)},

		// Float operations
		{Name: "divide two floats", Code: `(/ 10.0 4.0)`, Expected: values.NewFloat(2.5)},
		{Name: "divide float and integer", Code: `(/ 10.0 4)`, Expected: values.NewFloat(2.5)},
		{Name: "divide integer and float", Code: `(/ 10 4.0)`, Expected: values.NewFloat(2.5)},
		{Name: "reciprocal of float", Code: `(/ 4.0)`, Expected: values.NewFloat(0.25)},

		// Rational operations
		{Name: "divide two rationals", Code: `(/ 1/2 1/4)`, Expected: values.NewRational(2, 1)},
		{Name: "divide rational and integer", Code: `(/ 3/4 3)`, Expected: values.NewRational(1, 4)},
		{Name: "divide integer and rational", Code: `(/ 3 3/4)`, Expected: values.NewRational(4, 1)},
		{Name: "reciprocal of rational", Code: `(/ 3/4)`, Expected: values.NewRational(4, 3)},

		// Complex operations
		{Name: "divide two complex", Code: `(/ 4+2i 1+1i)`, Expected: values.NewBigComplex(values.NewBigIntegerFromInt64(3), values.NewBigIntegerFromInt64(-1))},
		{Name: "divide complex and integer", Code: `(/ 4+2i 2)`, Expected: values.NewBigComplex(values.NewBigIntegerFromInt64(2), values.NewBigIntegerFromInt64(1))},
		// (/ 1+1i) => 1/2-1/2i, an exact RATIONAL complex. Chez agrees.
		{Name: "reciprocal of complex", Code: `(/ 1+1i)`, Expected: values.NewBigComplex(values.NewRational(1, 2), values.NewRational(-1, 2))},

		// BigInteger operations
		{Name: "divide two bigintegers evenly", Code: `(/ #z20000000000000000000 #z2)`, Expected: values.NewBigIntegerFromString("10000000000000000000", 10)},

		// Variadic
		{Name: "divide many integers", Code: `(/ 120 2 3 4)`, Expected: values.NewInteger(5)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestDivision_SpecialValues(t *testing.T) {
	t.Run("divide by infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(/ 1 +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, f.Value == 0.0, qt.IsTrue)
	})

	t.Run("infinity divided by number", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(/ +inf.0 2)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, 1), qt.IsTrue)
	})

	t.Run("divide nan propagation", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(/ +nan.0 2)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})

	t.Run("infinity divided by infinity is nan", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(/ +inf.0 +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})

	// Note: float division by zero panics in this implementation (guards against division by zero)
	// rather than returning infinity as IEEE754 would suggest
}

func TestDivision_Errors(t *testing.T) {
	t.Run("divide no arguments", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(/)`)
	})

	t.Run("integer division by zero", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(/ 1 0)`)
	})

	t.Run("divide string first arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(/ "hello" 2)`)
	})
	t.Run("divide boolean second arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(/ 1 #t)`)
	})
}

func TestAbs(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Integer operations
		{Name: "abs of positive", Code: `(abs 5)`, Expected: values.NewInteger(5)},
		{Name: "abs of negative", Code: `(abs -5)`, Expected: values.NewInteger(5)},
		{Name: "abs of zero", Code: `(abs 0)`, Expected: values.NewInteger(0)},

		// Float operations
		{Name: "abs of positive float", Code: `(abs 3.14)`, Expected: values.NewFloat(3.14)},
		{Name: "abs of negative float", Code: `(abs -3.14)`, Expected: values.NewFloat(3.14)},
		{Name: "abs of zero float", Code: `(abs 0.0)`, Expected: values.NewFloat(0.0)},

		// Rational operations
		{Name: "abs of positive rational", Code: `(abs 3/4)`, Expected: values.NewRational(3, 4)},
		{Name: "abs of negative rational", Code: `(abs -3/4)`, Expected: values.NewRational(3, 4)},

		// BigInteger operations
		{Name: "abs of positive biginteger", Code: `(abs #z10000000000000000000)`, Expected: values.NewBigIntegerFromString("10000000000000000000", 10)},
		{Name: "abs of negative biginteger", Code: `(abs #z-10000000000000000000)`, Expected: values.NewBigIntegerFromString("10000000000000000000", 10)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestAbs_SpecialValues(t *testing.T) {
	t.Run("abs of positive infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(abs +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, 1), qt.IsTrue)
	})

	t.Run("abs of negative infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(abs -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, 1), qt.IsTrue)
	})

	t.Run("abs of nan", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(abs +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})
}

func TestFloor(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "floor of positive float",
			prog: values.List(values.NewSymbol("floor"), values.NewFloat(3.7)),
			out:  values.NewFloat(3.0),
		},
		{
			name: "floor of negative float",
			prog: values.List(values.NewSymbol("floor"), values.NewFloat(-3.2)),
			out:  values.NewFloat(-4.0),
		},
		{
			name: "floor of integer",
			prog: values.List(values.NewSymbol("floor"), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestCeiling(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "ceiling of positive float",
			prog: values.List(values.NewSymbol("ceiling"), values.NewFloat(3.2)),
			out:  values.NewFloat(4.0),
		},
		{
			name: "ceiling of negative float",
			prog: values.List(values.NewSymbol("ceiling"), values.NewFloat(-3.7)),
			out:  values.NewFloat(-3.0),
		},
		{
			name: "ceiling of integer",
			prog: values.List(values.NewSymbol("ceiling"), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestRound(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "round down",
			prog: values.List(values.NewSymbol("round"), values.NewFloat(3.2)),
			out:  values.NewFloat(3.0),
		},
		{
			name: "round up",
			prog: values.List(values.NewSymbol("round"), values.NewFloat(3.7)),
			out:  values.NewFloat(4.0),
		},
		{
			name: "round of integer",
			prog: values.List(values.NewSymbol("round"), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestTruncate(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "truncate positive",
			prog: values.List(values.NewSymbol("truncate"), values.NewFloat(3.7)),
			out:  values.NewFloat(3.0),
		},
		{
			name: "truncate negative",
			prog: values.List(values.NewSymbol("truncate"), values.NewFloat(-3.7)),
			out:  values.NewFloat(-3.0),
		},
		{
			name: "truncate of integer",
			prog: values.List(values.NewSymbol("truncate"), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestSqrt(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Perfect square integers return exact integer per R7RS §6.2.6
		{Name: "sqrt of perfect square 4", Code: `(sqrt 4)`, Expected: values.NewInteger(2)},
		{Name: "sqrt of perfect square 9", Code: `(sqrt 9)`, Expected: values.NewInteger(3)},
		{Name: "sqrt of perfect square 16", Code: `(sqrt 16)`, Expected: values.NewInteger(4)},
		{Name: "sqrt of 2", Code: `(sqrt 2)`, Expected: values.NewFloat(1.4142135623730951)},
		{Name: "sqrt of 0", Code: `(sqrt 0)`, Expected: values.NewInteger(0)},
		{Name: "sqrt of 1", Code: `(sqrt 1)`, Expected: values.NewInteger(1)},

		// Float operations (always inexact)
		{Name: "sqrt of float", Code: `(sqrt 2.25)`, Expected: values.NewFloat(1.5)},
		{Name: "sqrt of small float", Code: `(sqrt 0.25)`, Expected: values.NewFloat(0.5)},

		// Perfect square rational returns exact rational per R7RS §6.2.6
		{Name: "sqrt of rational perfect square", Code: `(sqrt 1/4)`, Expected: values.NewRational(1, 2)},
		{Name: "sqrt of rational", Code: `(sqrt 2/9)`, Expected: values.NewFloat(0.4714045207910317)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestSqrt_NegativeToComplex(t *testing.T) {
	// Negative perfect-square integers return exact BigComplex per R7RS §6.2.6
	t.Run("sqrt of negative integer", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(sqrt -1)`)
		qt.Assert(t, err, qt.IsNil)
		bc, ok := result.(*values.BigComplex)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, bc.Real().IsZero(), qt.IsTrue)
		qt.Assert(t, bc.IsExact(), qt.IsTrue)
		qt.Assert(t, result, valuestest.SchemeEquals, values.NewBigComplex(
			values.NewBigIntegerFromInt64(0),
			values.NewBigIntegerFromInt64(1),
		))
	})

	t.Run("sqrt of negative 4", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(sqrt -4)`)
		qt.Assert(t, err, qt.IsNil)
		bc, ok := result.(*values.BigComplex)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, bc.Real().IsZero(), qt.IsTrue)
		qt.Assert(t, bc.IsExact(), qt.IsTrue)
		qt.Assert(t, result, valuestest.SchemeEquals, values.NewBigComplex(
			values.NewBigIntegerFromInt64(0),
			values.NewBigIntegerFromInt64(2),
		))
	})
}

func TestSqrt_SpecialValues(t *testing.T) {
	t.Run("sqrt of positive infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(sqrt +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, 1), qt.IsTrue)
	})

	t.Run("sqrt of nan", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(sqrt +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})
}

func TestExpt(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Integer exponents
		{Name: "2^3", Code: `(expt 2 3)`, Expected: values.NewInteger(8)},
		{Name: "2^0", Code: `(expt 2 0)`, Expected: values.NewInteger(1)},
		{Name: "10^2", Code: `(expt 10 2)`, Expected: values.NewInteger(100)},
		{Name: "0^0", Code: `(expt 0 0)`, Expected: values.NewInteger(1)},
		{Name: "0^1", Code: `(expt 0 1)`, Expected: values.NewInteger(0)},
		{Name: "1^100", Code: `(expt 1 100)`, Expected: values.NewInteger(1)},
		{Name: "-2^3", Code: `(expt -2 3)`, Expected: values.NewInteger(-8)},
		{Name: "-2^4", Code: `(expt -2 4)`, Expected: values.NewInteger(16)},

		// Negative integer exponents
		{Name: "2^-1", Code: `(expt 2 -1)`, Expected: values.NewRational(1, 2)},
		{Name: "2^-2", Code: `(expt 2 -2)`, Expected: values.NewRational(1, 4)},
		{Name: "10^-1", Code: `(expt 10 -1)`, Expected: values.NewRational(1, 10)},

		// Float base
		{Name: "2.0^3", Code: `(expt 2.0 3)`, Expected: values.NewFloat(8.0)},
		{Name: "2.5^2", Code: `(expt 2.5 2)`, Expected: values.NewFloat(6.25)},

		// Float exponent (fractional power)
		{Name: "4^0.5", Code: `(expt 4 0.5)`, Expected: values.NewFloat(2.0)},
		// Note: 8^(1/3) and 27^(1/3) tested separately due to floating-point precision

		// Rational base
		{Name: "(1/2)^2", Code: `(expt 1/2 2)`, Expected: values.NewRational(1, 4)},
		{Name: "(1/2)^-1", Code: `(expt 1/2 -1)`, Expected: values.NewInteger(2)},
		{Name: "(2/3)^2", Code: `(expt 2/3 2)`, Expected: values.NewRational(4, 9)},

		// Note: i^2 tested separately due to floating-point precision

		// BigInteger
		{Name: "bigint^2", Code: `(expt #z10000000000 2)`, Expected: values.NewBigIntegerFromString("100000000000000000000", 10)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestExpt_SpecialValues(t *testing.T) {
	t.Run("infinity^2", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(expt +inf.0 2)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, 1), qt.IsTrue)
	})

	t.Run("2^infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(expt 2 +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, 1), qt.IsTrue)
	})

	t.Run("nan exponent", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(expt 2 +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})
}

// TestExpt_FloatingPointPrecision tests cases where floating-point precision
// prevents exact equality. These test mathematical correctness within epsilon.
func TestExpt_FloatingPointPrecision(t *testing.T) {
	const epsilon = 1e-10

	t.Run("8^(1/3) ≈ 2", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(expt 8 1/3)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.Abs(f.Value-2.0) < epsilon, qt.IsTrue)
	})

	t.Run("27^(1/3) ≈ 3", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(expt 27 1/3)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.Abs(f.Value-3.0) < epsilon, qt.IsTrue)
	})

	t.Run("i^2 ≈ -1", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(expt 0+1i 2)`)
		qt.Assert(t, err, qt.IsNil)
		c, ok := result.(*values.Complex)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.Abs(real(c.Value)+1.0) < epsilon, qt.IsTrue)
		qt.Assert(t, math.Abs(imag(c.Value)) < epsilon, qt.IsTrue)
	})
}

func TestSquare(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Integer operations
		{Name: "square of 5", Code: `(square 5)`, Expected: values.NewInteger(25)},
		{Name: "square of -3", Code: `(square -3)`, Expected: values.NewInteger(9)},
		{Name: "square of 0", Code: `(square 0)`, Expected: values.NewInteger(0)},
		{Name: "square of 1", Code: `(square 1)`, Expected: values.NewInteger(1)},

		// Float operations
		{Name: "square of float", Code: `(square 2.5)`, Expected: values.NewFloat(6.25)},
		{Name: "square of negative float", Code: `(square -2.5)`, Expected: values.NewFloat(6.25)},

		// Rational operations
		{Name: "square of rational", Code: `(square 1/2)`, Expected: values.NewRational(1, 4)},
		{Name: "square of negative rational", Code: `(square -2/3)`, Expected: values.NewRational(4, 9)},

		// Exact complex operations (integer parts are parsed as exact BigComplex)
		{Name: "square of exact complex", Code: `(square 1+1i)`, Expected: values.NewBigComplex(values.NewBigIntegerFromInt64(0), values.NewBigIntegerFromInt64(2))},
		{Name: "square of exact imaginary", Code: `(square 0+2i)`, Expected: values.NewBigIntegerFromInt64(-4)},

		// Inexact complex operations
		{Name: "square of inexact complex", Code: `(square 1.0+1.0i)`, Expected: values.NewComplexFromParts(0.0, 2.0)},
		{Name: "square of inexact imaginary", Code: `(square 0.0+2.0i)`, Expected: values.NewComplexFromParts(-4.0, 0.0)},

		// BigInteger operations
		{Name: "square of biginteger", Code: `(square #z10000000000)`, Expected: values.NewBigIntegerFromString("100000000000000000000", 10)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestSquare_SpecialValues(t *testing.T) {
	t.Run("square of infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(square +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, 1), qt.IsTrue)
	})

	t.Run("square of negative infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(square -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, 1), qt.IsTrue)
	})

	t.Run("square of nan", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(square +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})
}

func TestGcd(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic integer operations
		{Name: "gcd of 12 and 8", Code: `(gcd 12 8)`, Expected: values.NewInteger(4)},
		{Name: "gcd of no args", Code: `(gcd)`, Expected: values.NewInteger(0)},
		{Name: "gcd of one arg", Code: `(gcd 5)`, Expected: values.NewInteger(5)},
		{Name: "gcd of coprime numbers", Code: `(gcd 7 11)`, Expected: values.NewInteger(1)},
		{Name: "gcd of same numbers", Code: `(gcd 5 5)`, Expected: values.NewInteger(5)},
		{Name: "gcd with zero", Code: `(gcd 5 0)`, Expected: values.NewInteger(5)},
		{Name: "gcd of two zeros", Code: `(gcd 0 0)`, Expected: values.NewInteger(0)},

		// Negative numbers (gcd is always non-negative)
		{Name: "gcd of negative numbers", Code: `(gcd -12 8)`, Expected: values.NewInteger(4)},
		{Name: "gcd of two negatives", Code: `(gcd -12 -8)`, Expected: values.NewInteger(4)},
		{Name: "gcd of negative single arg", Code: `(gcd -5)`, Expected: values.NewInteger(5)},

		// Variadic (3+ args)
		{Name: "gcd of three numbers", Code: `(gcd 12 18 24)`, Expected: values.NewInteger(6)},
		{Name: "gcd of four numbers", Code: `(gcd 100 50 25 75)`, Expected: values.NewInteger(25)},

		// BigInteger operations
		{Name: "gcd of bigintegers", Code: `(gcd #z100000000000000000000 #z50000000000000000000)`, Expected: values.NewBigIntegerFromString("50000000000000000000", 10)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestLcm(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic integer operations
		{Name: "lcm of 4 and 6", Code: `(lcm 4 6)`, Expected: values.NewInteger(12)},
		{Name: "lcm of no args", Code: `(lcm)`, Expected: values.NewInteger(1)},
		{Name: "lcm of one arg", Code: `(lcm 5)`, Expected: values.NewInteger(5)},
		{Name: "lcm of coprime numbers", Code: `(lcm 7 11)`, Expected: values.NewInteger(77)},
		{Name: "lcm of same numbers", Code: `(lcm 5 5)`, Expected: values.NewInteger(5)},
		{Name: "lcm with zero returns zero", Code: `(lcm 5 0)`, Expected: values.NewInteger(0)},
		{Name: "lcm of two zeros", Code: `(lcm 0 0)`, Expected: values.NewInteger(0)},
		{Name: "lcm of 1 and any number", Code: `(lcm 1 42)`, Expected: values.NewInteger(42)},

		// Negative numbers (lcm is always non-negative)
		{Name: "lcm of negative numbers", Code: `(lcm -4 6)`, Expected: values.NewInteger(12)},
		{Name: "lcm of two negatives", Code: `(lcm -4 -6)`, Expected: values.NewInteger(12)},

		// Variadic (3+ args)
		{Name: "lcm of three numbers", Code: `(lcm 2 3 4)`, Expected: values.NewInteger(12)},
		{Name: "lcm of four numbers", Code: `(lcm 2 3 4 5)`, Expected: values.NewInteger(60)},

		// BigInteger operations
		{Name: "lcm of bigintegers", Code: `(lcm #z10000000000 #z30000000000)`, Expected: values.NewBigIntegerFromString("30000000000", 10)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestQuotient(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic integer operations (truncates toward zero)
		{Name: "quotient 7/3", Code: `(quotient 7 3)`, Expected: values.NewInteger(2)},
		{Name: "quotient -7/3", Code: `(quotient -7 3)`, Expected: values.NewInteger(-2)},
		{Name: "quotient 7/-3", Code: `(quotient 7 -3)`, Expected: values.NewInteger(-2)},
		{Name: "quotient -7/-3", Code: `(quotient -7 -3)`, Expected: values.NewInteger(2)},
		{Name: "quotient exact division", Code: `(quotient 10 2)`, Expected: values.NewInteger(5)},
		{Name: "quotient zero dividend", Code: `(quotient 0 5)`, Expected: values.NewInteger(0)},
		{Name: "quotient 1/larger", Code: `(quotient 1 5)`, Expected: values.NewInteger(0)},

		// Float operations
		{Name: "quotient with floats", Code: `(quotient 7.0 3.0)`, Expected: values.NewFloat(2.0)},
		{Name: "quotient integer and float", Code: `(quotient 7 3.0)`, Expected: values.NewFloat(2.0)},

		// BigInteger operations
		{Name: "quotient bigintegers", Code: `(quotient #z100000000000000000000 #z30000000000000000000)`, Expected: values.NewBigIntegerFromString("3", 10)},

		// MinInt64 / -1 overflows int64: true quotient is +2^63, which wraps to
		// MinInt64 under raw a/b. Must promote to BigInteger equal to +2^63.
		{Name: "quotient MinInt64/-1 overflow", Code: `(quotient -9223372036854775808 -1)`, Expected: values.NewBigIntegerFromString("9223372036854775808", 10)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestQuotient_Errors(t *testing.T) {
	t.Run("quotient by zero", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(quotient 7 0)`)
	})
	t.Run("quotient string first arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(quotient "hello" 3)`)
	})
	t.Run("quotient boolean second arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(quotient 7 #t)`)
	})
}

func TestRemainder(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic integer operations (sign follows dividend)
		{Name: "remainder 7/3", Code: `(remainder 7 3)`, Expected: values.NewInteger(1)},
		{Name: "remainder -7/3", Code: `(remainder -7 3)`, Expected: values.NewInteger(-1)},
		{Name: "remainder 7/-3", Code: `(remainder 7 -3)`, Expected: values.NewInteger(1)},
		{Name: "remainder -7/-3", Code: `(remainder -7 -3)`, Expected: values.NewInteger(-1)},
		{Name: "remainder exact division", Code: `(remainder 10 2)`, Expected: values.NewInteger(0)},
		{Name: "remainder zero dividend", Code: `(remainder 0 5)`, Expected: values.NewInteger(0)},
		{Name: "remainder 1/larger", Code: `(remainder 1 5)`, Expected: values.NewInteger(1)},

		// Float operations
		{Name: "remainder with floats", Code: `(remainder 7.0 3.0)`, Expected: values.NewFloat(1.0)},
		{Name: "remainder negative float", Code: `(remainder -7.0 3.0)`, Expected: values.NewFloat(-1.0)},

		// BigInteger operations
		{Name: "remainder bigintegers", Code: `(remainder #z100000000000000000000 #z30000000000000000000)`, Expected: values.NewBigIntegerFromString("10000000000000000000", 10)},

		// MinInt64 / -1: quotient overflows but remainder is mathematically 0.
		{Name: "remainder MinInt64/-1", Code: `(remainder -9223372036854775808 -1)`, Expected: values.NewInteger(0)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestRemainder_Errors(t *testing.T) {
	t.Run("remainder by zero", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(remainder 7 0)`)
	})
	t.Run("remainder string first arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(remainder "hello" 3)`)
	})
	t.Run("remainder boolean second arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(remainder 7 #t)`)
	})
}

func TestModulo(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic integer operations (sign follows divisor)
		{Name: "modulo 7/3", Code: `(modulo 7 3)`, Expected: values.NewInteger(1)},
		{Name: "modulo -7/3", Code: `(modulo -7 3)`, Expected: values.NewInteger(2)},
		{Name: "modulo 7/-3", Code: `(modulo 7 -3)`, Expected: values.NewInteger(-2)},
		{Name: "modulo -7/-3", Code: `(modulo -7 -3)`, Expected: values.NewInteger(-1)},
		{Name: "modulo exact division", Code: `(modulo 10 2)`, Expected: values.NewInteger(0)},
		{Name: "modulo zero dividend", Code: `(modulo 0 5)`, Expected: values.NewInteger(0)},
		{Name: "modulo 1/larger", Code: `(modulo 1 5)`, Expected: values.NewInteger(1)},

		// Additional sign cases
		{Name: "modulo -1/3", Code: `(modulo -1 3)`, Expected: values.NewInteger(2)},
		{Name: "modulo 1/-3", Code: `(modulo 1 -3)`, Expected: values.NewInteger(-2)},

		// Float operations
		{Name: "modulo with floats", Code: `(modulo 7.0 3.0)`, Expected: values.NewFloat(1.0)},
		{Name: "modulo negative dividend float", Code: `(modulo -7.0 3.0)`, Expected: values.NewFloat(2.0)},

		// BigInteger operations
		{Name: "modulo bigintegers", Code: `(modulo #z100000000000000000007 #z10)`, Expected: values.NewBigIntegerFromString("7", 10)},

		// MinInt64 / -1: quotient overflows but modulo is mathematically 0.
		{Name: "modulo MinInt64/-1", Code: `(modulo -9223372036854775808 -1)`, Expected: values.NewInteger(0)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestModulo_Errors(t *testing.T) {
	t.Run("modulo by zero", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(modulo 7 0)`)
	})
	t.Run("modulo string first arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(modulo "hello" 3)`)
	})
	t.Run("modulo boolean second arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(modulo 7 #t)`)
	})
}

func TestMax(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic integer operations
		{Name: "max of two", Code: `(max 3 5)`, Expected: values.NewInteger(5)},
		{Name: "max of three", Code: `(max 3 5 1)`, Expected: values.NewInteger(5)},
		{Name: "max of one", Code: `(max 7)`, Expected: values.NewInteger(7)},
		{Name: "max with negatives", Code: `(max -3 -5 -1)`, Expected: values.NewInteger(-1)},
		{Name: "max with zero", Code: `(max 0 -5 5)`, Expected: values.NewInteger(5)},

		// Float operations
		{Name: "max of floats", Code: `(max 3.5 2.5)`, Expected: values.NewFloat(3.5)},
		{Name: "max of negative floats", Code: `(max -1.5 -2.5)`, Expected: values.NewFloat(-1.5)},

		// Mixed types (result is inexact if any arg is inexact)
		{Name: "max integer and float", Code: `(max 3 2.5)`, Expected: values.NewFloat(3.0)},
		{Name: "max float wins", Code: `(max 2 3.5)`, Expected: values.NewFloat(3.5)},

		// Rational operations
		{Name: "max of rationals", Code: `(max 1/2 3/4)`, Expected: values.NewRational(3, 4)},
		{Name: "max rational and integer", Code: `(max 1/2 1)`, Expected: values.NewInteger(1)},

		// BigInteger operations
		{Name: "max of bigintegers", Code: `(max #z10000000000000000000 #z20000000000000000000)`, Expected: values.NewBigIntegerFromString("20000000000000000000", 10)},

		// BigFloat operations (always inexact)
		{Name: "max of bigfloats", Code: `(max #m3.5 #m2.5)`, Expected: values.NewBigFloatFromString("3.5")},
		{Name: "max bigfloat and integer", Code: `(max #m2.5 3)`, Expected: values.NewFloat(3.0)}, // Integer wins but becomes inexact
		{Name: "max integer and bigfloat", Code: `(max 3 #m2.5)`, Expected: values.NewFloat(3.0)}, // Integer wins but becomes inexact
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestMax_SpecialValues(t *testing.T) {
	t.Run("max with positive infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(max 1 2 +inf.0 3)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, 1), qt.IsTrue)
	})

	t.Run("max with negative infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(max -inf.0 -100)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, f.Value == -100.0, qt.IsTrue)
	})

	t.Run("max with nan", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(max 1 +nan.0 3)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})
}

func TestMax_Errors(t *testing.T) {
	t.Run("max no arguments", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(max)`)
	})
	t.Run("max string first arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(max "hello" 1)`)
	})
	t.Run("max boolean second arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(max 1 #t)`)
	})
}

func TestMin(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic integer operations
		{Name: "min of two", Code: `(min 3 5)`, Expected: values.NewInteger(3)},
		{Name: "min of three", Code: `(min 3 5 1)`, Expected: values.NewInteger(1)},
		{Name: "min of one", Code: `(min 7)`, Expected: values.NewInteger(7)},
		{Name: "min with negatives", Code: `(min -3 -5 -1)`, Expected: values.NewInteger(-5)},
		{Name: "min with zero", Code: `(min 0 -5 5)`, Expected: values.NewInteger(-5)},

		// Float operations
		{Name: "min of floats", Code: `(min 3.5 2.5)`, Expected: values.NewFloat(2.5)},
		{Name: "min of negative floats", Code: `(min -1.5 -2.5)`, Expected: values.NewFloat(-2.5)},

		// Mixed types (result is inexact if any arg is inexact)
		{Name: "min integer and float", Code: `(min 3 2.5)`, Expected: values.NewFloat(2.5)},
		{Name: "min integer wins", Code: `(min 2 3.5)`, Expected: values.NewFloat(2.0)},

		// Rational operations
		{Name: "min of rationals", Code: `(min 1/2 3/4)`, Expected: values.NewRational(1, 2)},
		{Name: "min rational and integer", Code: `(min 1/2 1)`, Expected: values.NewRational(1, 2)},

		// BigInteger operations
		{Name: "min of bigintegers", Code: `(min #z10000000000000000000 #z20000000000000000000)`, Expected: values.NewBigIntegerFromString("10000000000000000000", 10)},

		// BigFloat operations (always inexact)
		{Name: "min of bigfloats", Code: `(min #m3.5 #m2.5)`, Expected: values.NewBigFloatFromString("2.5")},
		{Name: "min bigfloat and integer", Code: `(min #m2.5 3)`, Expected: values.NewBigFloatFromString("2.5")}, // BigFloat wins, stays BigFloat
		{Name: "min integer and bigfloat", Code: `(min 2 #m3.5)`, Expected: values.NewFloat(2.0)},                // Integer wins but becomes inexact
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestMin_SpecialValues(t *testing.T) {
	t.Run("min with negative infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(min 1 2 -inf.0 3)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, -1), qt.IsTrue)
	})

	t.Run("min with positive infinity", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(min +inf.0 100)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, f.Value == 100.0, qt.IsTrue)
	})

	t.Run("min with nan", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(min 1 +nan.0 3)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})
}

func TestMin_Errors(t *testing.T) {
	t.Run("min no arguments", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(min)`)
	})
	t.Run("min string first arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(min "hello" 1)`)
	})
	t.Run("min boolean second arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(min 1 #t)`)
	})
}

func TestAddition_Errors(t *testing.T) {
	t.Run("add string arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(+ "hello" 1)`)
	})
	t.Run("add boolean arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(+ #t 1)`)
	})
}

func TestMultiplication_Errors(t *testing.T) {
	t.Run("multiply string arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(* "hello" 2)`)
	})
	t.Run("multiply boolean arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(* #t 2)`)
	})
}

func TestAbs_Errors(t *testing.T) {
	t.Run("abs of string", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(abs "hello")`)
	})
	t.Run("abs of boolean", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(abs #t)`)
	})
}

func TestSquare_Errors(t *testing.T) {
	t.Run("square of string", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(square "hello")`)
	})
	t.Run("square of boolean", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(square #t)`)
	})
}

func TestSqrt_Errors(t *testing.T) {
	t.Run("sqrt of string", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(sqrt "hello")`)
	})
	t.Run("sqrt of boolean", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(sqrt #t)`)
	})
}

func TestExpt_Errors(t *testing.T) {
	t.Run("expt string base", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(expt "hello" 2)`)
	})
	t.Run("expt boolean exponent", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(expt 2 #t)`)
	})
}

func TestGcd_Errors(t *testing.T) {
	t.Run("gcd string first arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(gcd "hello" 4)`)
	})
	t.Run("gcd boolean second arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(gcd 4 #t)`)
	})
}

func TestLcm_Errors(t *testing.T) {
	t.Run("lcm string first arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(lcm "hello" 4)`)
	})
	t.Run("lcm boolean second arg", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(lcm 4 #t)`)
	})
}

// TestDivisionByZero_SchemeException verifies that division by zero across all
// numeric types is caught at the VM boundary and converted to a proper Scheme
// exception that guard can handle. This tests the recover in
// applyForeign, not just the values package panic.
func TestDivisionByZero_SchemeException(t *testing.T) {
	c := qt.New(t)
	tcs := []testhelpers.SchemeCodeTestCase{
		// Integer / 0
		{Name: "integer div zero caught by guard", Code: `(guard (exn (#t "caught")) (/ 1 0))`, Expected: values.NewString("caught")},
		// BigInteger / 0 (Issue #22)
		{Name: "biginteger div zero caught by guard", Code: `(guard (exn (#t "caught")) (/ (expt 2 100) 0))`, Expected: values.NewString("caught")},
		// Float / 0
		{Name: "float div zero caught by guard", Code: `(guard (exn (#t "caught")) (/ 1.5 0))`, Expected: values.NewString("caught")},
		// Rational / 0
		{Name: "rational div zero caught by guard", Code: `(guard (exn (#t "caught")) (/ 1/3 0))`, Expected: values.NewString("caught")},
		// Complex / 0
		{Name: "complex div zero caught by guard", Code: `(guard (exn (#t "caught")) (/ 1+2i 0))`, Expected: values.NewString("caught")},
	}
	for _, tc := range tcs {
		c.Run(tc.Name, func(c *qt.C) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestDivisionByZero_ReturnsError verifies that division by zero returns an
// error through the VM rather than panicking.
func TestDivisionByZero_ReturnsError(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "integer / 0", Code: `(/ 1 0)`},
		{Name: "biginteger / 0", Code: `(/ (expt 2 100) 0)`},
		{Name: "float / 0", Code: `(/ 1.5 0)`},
		{Name: "rational / 0", Code: `(/ 1/3 0)`},
		{Name: "complex / 0", Code: `(/ 1+2i 0)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, werr.ErrDivisionByZero), qt.IsTrue)
		})
	}
}

// TestQuotientDivisionByZeroSentinel verifies that quotient/remainder/modulo
// division-by-zero errors use the ErrDivisionByZero sentinel and are
// matchable with errors.Is().
func TestQuotientDivisionByZeroSentinel(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"quotient by zero", `(quotient 10 0)`},
		{"quotient biginteger by zero", `(quotient (expt 2 100) 0)`},
		{"remainder by zero", `(remainder 10 0)`},
		{"remainder biginteger by zero", `(remainder (expt 2 100) 0)`},
		{"modulo by zero", `(modulo 10 0)`},
		{"modulo biginteger by zero", `(modulo (expt 2 100) 0)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, werr.ErrDivisionByZero), qt.IsTrue)
			qt.Assert(t, err, qt.ErrorMatches, `(?s).*division by zero.*`)
		})
	}
}

// TestDivisionByInexactZero verifies that dividing by an inexact zero returns
// ±Inf or NaN per R7RS §6.2.6 and IEEE 754, rather than raising an error.
func TestDivisionByInexactZero(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "(/ 1 0.0) is +inf.0", Code: `(/ 1 0.0)`, Expected: values.NewFloat(math.Inf(1))},
		{Name: "(/ -1 0.0) is -inf.0", Code: `(/ -1 0.0)`, Expected: values.NewFloat(math.Inf(-1))},
		{Name: "(/ 0.0 0.0) is +nan.0", Code: `(nan? (/ 0.0 0.0))`, Expected: values.TrueValue},
		{Name: "(/ 1.0 0.0) is +inf.0", Code: `(/ 1.0 0.0)`, Expected: values.NewFloat(math.Inf(1))},
		{Name: "(/ -1.0 0.0) is -inf.0", Code: `(/ -1.0 0.0)`, Expected: values.NewFloat(math.Inf(-1))},
		{Name: "(/ 1/3 0.0) is +inf.0", Code: `(infinite? (/ 1/3 0.0))`, Expected: values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestOverflowPromotion(t *testing.T) {
	// R7RS §6.2.3: Integer overflow should promote to BigInteger
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "add overflow", Code: `(+ 9223372036854775807 1)`, Expected: values.NewBigIntegerFromString("9223372036854775808", 10)},
		{Name: "subtract underflow", Code: `(- -9223372036854775808 1)`, Expected: values.NewBigIntegerFromString("-9223372036854775809", 10)},
		{Name: "multiply overflow", Code: `(* 9223372036854775807 2)`, Expected: values.NewBigIntegerFromString("18446744073709551614", 10)},
		{Name: "abs MinInt64", Code: `(abs -9223372036854775808)`, Expected: values.NewBigIntegerFromString("9223372036854775808", 10)},
		{Name: "lcm overflow", Code: `(lcm 9223372036854775807 2)`, Expected: values.NewBigIntegerFromString("18446744073709551614", 10)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}

	// Exactness and type preservation
	testhelpers.RunSchemeCodeExpectTrue(t, `(exact? (+ 9223372036854775807 1))`)
	testhelpers.RunSchemeCodeExpectTrue(t, `(integer? (+ 9223372036854775807 1))`)
	testhelpers.RunSchemeCodeExpectTrue(t, `(= (+ 9223372036854775807 1) 9223372036854775808)`)
	testhelpers.RunSchemeCodeExpectTrue(t, `(= (- -9223372036854775808 1) -9223372036854775809)`)
	testhelpers.RunSchemeCodeExpectTrue(t, `(= (* 9223372036854775807 2) 18446744073709551614)`)
	testhelpers.RunSchemeCodeExpectTrue(t, `(= (abs -9223372036854775808) 9223372036854775808)`)
}

// TestSpecialValueArithmetic tests arithmetic with +inf.0, -inf.0, and +nan.0.
// R7RS §6.2.6: These follow IEEE 754 rules.
func TestSpecialValueArithmetic(t *testing.T) {
	// inf + -inf = nan
	t.Run("+inf.0 + -inf.0 is nan", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(+ +inf.0 -inf.0)")
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})

	// 0.0 * inf = nan (IEEE 754). The IEEE rule governs the INEXACT zero only.
	t.Run("0.0 * +inf.0 is nan", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(* 0.0 +inf.0)")
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})

	// (* 0 +inf.0) with an EXACT zero is exact 0, not NaN — R7RS §6.2.2, and
	// both Chez and Racket agree. The exact-zero rule outranks IEEE.
	t.Run("exact 0 * +inf.0 is exact 0", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(* 0 +inf.0)")
		qt.Assert(t, err, qt.IsNil)
		i, ok := result.(*values.Integer)
		qt.Assert(t, ok, qt.IsTrue, qt.Commentf("want exact Integer, got %T", result))
		qt.Assert(t, i.Value, qt.Equals, int64(0))
	})

	// Division by exact zero is an error.
	t.Run("(/ 0 0) is error", func(t *testing.T) {
		_, err := testhelpers.RunSchemeCode(t, "(/ 0 0)")
		qt.Assert(t, err, qt.IsNotNil)
	})

	// R7RS §6.2.6 + IEEE 754: division by inexact zero returns ±inf or nan
	t.Run("(/ 1.0 0.0) is +inf.0", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(/ 1.0 0.0)")
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, 1), qt.IsTrue)
	})

	// inf arithmetic
	t.Run("+inf.0 + 1 is +inf.0", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(+ +inf.0 1)")
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, 1), qt.IsTrue)
	})

	t.Run("-inf.0 - 1 is -inf.0", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(- -inf.0 1)")
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, -1), qt.IsTrue)
	})

	// nan propagation
	t.Run("nan + 1 is nan", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, "(+ +nan.0 1)")
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})
}
