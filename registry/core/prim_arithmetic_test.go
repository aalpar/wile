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

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestAddition(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic integer operations
		{"add two integers", `(+ 1 2)`, values.NewInteger(3)},
		{"add three integers", `(+ 1 2 3)`, values.NewInteger(6)},
		{"add single integer", `(+ 5)`, values.NewInteger(5)},
		{"add no arguments returns 0", `(+)`, values.NewInteger(0)},
		{"add negative numbers", `(+ -5 3)`, values.NewInteger(-2)},

		// Float operations
		{"add two floats", `(+ 1.5 2.5)`, values.NewFloat(4.0)},
		{"add float and integer", `(+ 1 2.5)`, values.NewFloat(3.5)},
		{"add integer and float", `(+ 2.5 1)`, values.NewFloat(3.5)},

		// Rational operations
		{"add two rationals", `(+ 1/2 1/4)`, values.NewRational(3, 4)},
		{"add rational and integer", `(+ 1/2 1)`, values.NewRational(3, 2)},
		{"add integer and rational", `(+ 1 1/2)`, values.NewRational(3, 2)},
		{"add rational and float", `(+ 1/2 0.5)`, values.NewFloat(1.0)},

		// Complex operations
		{"add two complex", `(+ 1+2i 3+4i)`, values.NewComplexFromParts(4.0, 6.0)},
		{"add complex and integer", `(+ 1+2i 3)`, values.NewComplexFromParts(4.0, 2.0)},
		{"add complex and float", `(+ 1+2i 1.5)`, values.NewComplexFromParts(2.5, 2.0)},

		// BigInteger operations
		{"add two bigintegers", `(+ #z10000000000000000000 #z1)`, values.NewBigIntegerFromString("10000000000000000001", 10)},
		{"add biginteger and integer", `(+ #z10000000000000000000 5)`, values.NewBigIntegerFromString("10000000000000000005", 10)},

		// Variadic
		{"add many integers", `(+ 1 2 3 4 5)`, values.NewInteger(15)},
		{"add many mixed types", `(+ 1 2.0 3/2)`, values.NewFloat(4.5)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestAddition_SpecialValues(t *testing.T) {
	// Test infinity and NaN behavior
	t.Run("add positive infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(+ 1 +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})

	t.Run("add negative infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(+ 1 -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), -1), qt.IsTrue)
	})

	t.Run("add nan propagation", func(t *testing.T) {
		result, err := runSchemeCode(t, `(+ 1 +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})

	t.Run("infinity minus infinity is nan", func(t *testing.T) {
		result, err := runSchemeCode(t, `(+ +inf.0 -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})
}

func TestSubtraction(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic integer operations
		{"subtract two integers", `(- 5 2)`, values.NewInteger(3)},
		{"negate single integer", `(- 5)`, values.NewInteger(-5)},
		{"subtract multiple integers", `(- 10 3 2)`, values.NewInteger(5)},
		{"subtract negative result", `(- 1 5)`, values.NewInteger(-4)},
		{"negate negative", `(- -5)`, values.NewInteger(5)},

		// Float operations
		{"subtract two floats", `(- 5.5 2.5)`, values.NewFloat(3.0)},
		{"negate float", `(- 3.14)`, values.NewFloat(-3.14)},
		{"subtract float and integer", `(- 5.5 2)`, values.NewFloat(3.5)},
		{"subtract integer and float", `(- 5 2.5)`, values.NewFloat(2.5)},

		// Rational operations
		{"subtract two rationals", `(- 3/4 1/4)`, values.NewRational(1, 2)},
		{"negate rational", `(- 1/2)`, values.NewRational(-1, 2)},
		{"subtract rational and integer", `(- 3/2 1)`, values.NewRational(1, 2)},
		{"subtract integer and rational", `(- 2 1/2)`, values.NewRational(3, 2)},

		// Complex operations
		{"subtract two complex", `(- 5+6i 2+3i)`, values.NewComplexFromParts(3.0, 3.0)},
		{"negate complex", `(- 1+2i)`, values.NewComplexFromParts(-1.0, -2.0)},
		{"subtract complex and integer", `(- 5+3i 2)`, values.NewComplexFromParts(3.0, 3.0)},

		// BigInteger operations
		{"subtract two bigintegers", `(- #z10000000000000000005 #z5)`, values.NewBigIntegerFromString("10000000000000000000", 10)},
		{"negate biginteger", `(- #z10000000000000000000)`, values.NewBigIntegerFromString("-10000000000000000000", 10)},

		// Variadic
		{"subtract many integers", `(- 100 20 30 10)`, values.NewInteger(40)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestSubtraction_SpecialValues(t *testing.T) {
	t.Run("subtract from infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(- +inf.0 1)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})

	t.Run("negate infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(- +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), -1), qt.IsTrue)
	})

	t.Run("subtract nan propagation", func(t *testing.T) {
		result, err := runSchemeCode(t, `(- +nan.0 1)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})
}

func TestSubtraction_Errors(t *testing.T) {
	t.Run("subtract no arguments", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(-)`)
	})
	t.Run("subtract string first arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(- "hello" 1)`)
	})
	t.Run("subtract boolean second arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(- 1 #t)`)
	})
}

func TestMultiplication(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic integer operations
		{"multiply two integers", `(* 3 4)`, values.NewInteger(12)},
		{"multiply three integers", `(* 2 3 4)`, values.NewInteger(24)},
		{"multiply single integer", `(* 7)`, values.NewInteger(7)},
		{"multiply no arguments returns 1", `(*)`, values.NewInteger(1)},
		{"multiply by zero", `(* 5 0)`, values.NewInteger(0)},
		{"multiply negative numbers", `(* -3 4)`, values.NewInteger(-12)},
		{"multiply two negatives", `(* -3 -4)`, values.NewInteger(12)},

		// Float operations
		{"multiply two floats", `(* 2.5 4.0)`, values.NewFloat(10.0)},
		{"multiply float and integer", `(* 2.5 4)`, values.NewFloat(10.0)},
		{"multiply integer and float", `(* 4 2.5)`, values.NewFloat(10.0)},
		{"multiply float by zero", `(* 3.14 0)`, values.NewInteger(0)}, // zero short-circuits to Integer

		// Rational operations
		{"multiply two rationals", `(* 1/2 2/3)`, values.NewRational(1, 3)},
		{"multiply rational and integer", `(* 1/2 4)`, values.NewRational(2, 1)}, // stays Rational
		{"multiply integer and rational", `(* 4 1/2)`, values.NewRational(2, 1)}, // stays Rational
		{"multiply rational and float", `(* 1/2 3.0)`, values.NewFloat(1.5)},

		// Complex operations
		{"multiply two complex", `(* 1+2i 3+4i)`, values.NewComplexFromParts(-5.0, 10.0)},
		{"multiply complex and integer", `(* 2+3i 2)`, values.NewComplexFromParts(4.0, 6.0)},
		{"multiply complex and float", `(* 1+1i 2.0)`, values.NewComplexFromParts(2.0, 2.0)},

		// BigInteger operations
		{"multiply two bigintegers", `(* #z10000000000000000000 #z2)`, values.NewBigIntegerFromString("20000000000000000000", 10)},
		{"multiply biginteger and integer", `(* #z10000000000000000000 3)`, values.NewBigIntegerFromString("30000000000000000000", 10)},

		// Variadic
		{"multiply many integers", `(* 2 3 4 5)`, values.NewInteger(120)},
		{"multiply many mixed types", `(* 2 3.0 1/2)`, values.NewFloat(3.0)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestMultiplication_SpecialValues(t *testing.T) {
	t.Run("multiply by infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(* 2 +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})

	t.Run("multiply negative by infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(* -2 +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), -1), qt.IsTrue)
	})

	t.Run("zero times infinity is nan", func(t *testing.T) {
		result, err := runSchemeCode(t, `(* 0 +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})

	t.Run("multiply nan propagation", func(t *testing.T) {
		result, err := runSchemeCode(t, `(* 2 +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})
}

func TestDivision(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic integer operations
		{"divide two integers", `(/ 10 2)`, values.NewInteger(5)},
		{"divide multiple integers", `(/ 100 5 4)`, values.NewInteger(5)},
		{"divide single integer returns reciprocal", `(/ 5)`, values.NewRational(1, 5)},
		{"divide single integer 1 returns integer", `(/ 1)`, values.NewInteger(1)},
		{"divide integers non-evenly returns rational", `(/ 1 2)`, values.NewRational(1, 2)},
		{"divide integers auto-simplifies rational", `(/ 10 4)`, values.NewRational(5, 2)},
		{"divide integers evenly returns integer", `(/ 6 3)`, values.NewInteger(2)},

		// Float operations
		{"divide two floats", `(/ 10.0 4.0)`, values.NewFloat(2.5)},
		{"divide float and integer", `(/ 10.0 4)`, values.NewFloat(2.5)},
		{"divide integer and float", `(/ 10 4.0)`, values.NewFloat(2.5)},
		{"reciprocal of float", `(/ 4.0)`, values.NewFloat(0.25)},

		// Rational operations
		{"divide two rationals", `(/ 1/2 1/4)`, values.NewRational(2, 1)},
		{"divide rational and integer", `(/ 3/4 3)`, values.NewRational(1, 4)},
		{"divide integer and rational", `(/ 3 3/4)`, values.NewRational(4, 1)},
		{"reciprocal of rational", `(/ 3/4)`, values.NewRational(4, 3)},

		// Complex operations
		{"divide two complex", `(/ 4+2i 1+1i)`, values.NewComplexFromParts(3.0, -1.0)},
		{"divide complex and integer", `(/ 4+2i 2)`, values.NewComplexFromParts(2.0, 1.0)},
		{"reciprocal of complex", `(/ 1+1i)`, values.NewComplexFromParts(0.5, -0.5)},

		// BigInteger operations
		{"divide two bigintegers evenly", `(/ #z20000000000000000000 #z2)`, values.NewBigIntegerFromString("10000000000000000000", 10)},

		// Variadic
		{"divide many integers", `(/ 120 2 3 4)`, values.NewInteger(5)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestDivision_SpecialValues(t *testing.T) {
	t.Run("divide by infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(/ 1 +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, f.Datum() == 0.0, qt.IsTrue)
	})

	t.Run("infinity divided by number", func(t *testing.T) {
		result, err := runSchemeCode(t, `(/ +inf.0 2)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})

	t.Run("divide nan propagation", func(t *testing.T) {
		result, err := runSchemeCode(t, `(/ +nan.0 2)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})

	t.Run("infinity divided by infinity is nan", func(t *testing.T) {
		result, err := runSchemeCode(t, `(/ +inf.0 +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})

	// Note: float division by zero panics in this implementation (guards against division by zero)
	// rather than returning infinity as IEEE754 would suggest
}

func TestDivision_Errors(t *testing.T) {
	t.Run("divide no arguments", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(/)`)
	})

	t.Run("integer division by zero", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(/ 1 0)`)
	})

	t.Run("divide string first arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(/ "hello" 2)`)
	})
	t.Run("divide boolean second arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(/ 1 #t)`)
	})
}

func TestAbs(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Integer operations
		{"abs of positive", `(abs 5)`, values.NewInteger(5)},
		{"abs of negative", `(abs -5)`, values.NewInteger(5)},
		{"abs of zero", `(abs 0)`, values.NewInteger(0)},

		// Float operations
		{"abs of positive float", `(abs 3.14)`, values.NewFloat(3.14)},
		{"abs of negative float", `(abs -3.14)`, values.NewFloat(3.14)},
		{"abs of zero float", `(abs 0.0)`, values.NewFloat(0.0)},

		// Rational operations
		{"abs of positive rational", `(abs 3/4)`, values.NewRational(3, 4)},
		{"abs of negative rational", `(abs -3/4)`, values.NewRational(3, 4)},

		// BigInteger operations
		{"abs of positive biginteger", `(abs #z10000000000000000000)`, values.NewBigIntegerFromString("10000000000000000000", 10)},
		{"abs of negative biginteger", `(abs #z-10000000000000000000)`, values.NewBigIntegerFromString("10000000000000000000", 10)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestAbs_SpecialValues(t *testing.T) {
	t.Run("abs of positive infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(abs +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})

	t.Run("abs of negative infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(abs -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})

	t.Run("abs of nan", func(t *testing.T) {
		result, err := runSchemeCode(t, `(abs +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestSqrt(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Integer operations
		{"sqrt of perfect square 4", `(sqrt 4)`, values.NewFloat(2.0)},
		{"sqrt of perfect square 9", `(sqrt 9)`, values.NewFloat(3.0)},
		{"sqrt of perfect square 16", `(sqrt 16)`, values.NewFloat(4.0)},
		{"sqrt of 2", `(sqrt 2)`, values.NewFloat(1.4142135623730951)},
		{"sqrt of 0", `(sqrt 0)`, values.NewFloat(0.0)},
		{"sqrt of 1", `(sqrt 1)`, values.NewFloat(1.0)},

		// Float operations
		{"sqrt of float", `(sqrt 2.25)`, values.NewFloat(1.5)},
		{"sqrt of small float", `(sqrt 0.25)`, values.NewFloat(0.5)},

		// Rational operations
		{"sqrt of rational perfect square", `(sqrt 1/4)`, values.NewFloat(0.5)},
		{"sqrt of rational", `(sqrt 2/9)`, values.NewFloat(0.4714045207910317)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestSqrt_NegativeToComplex(t *testing.T) {
	t.Run("sqrt of negative integer", func(t *testing.T) {
		result, err := runSchemeCode(t, `(sqrt -1)`)
		qt.Assert(t, err, qt.IsNil)
		c, ok := result.(*values.Complex)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, c.Real() == 0.0, qt.IsTrue)
		qt.Assert(t, c.Imag() == 1.0, qt.IsTrue)
	})

	t.Run("sqrt of negative 4", func(t *testing.T) {
		result, err := runSchemeCode(t, `(sqrt -4)`)
		qt.Assert(t, err, qt.IsNil)
		c, ok := result.(*values.Complex)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, c.Real() == 0.0, qt.IsTrue)
		qt.Assert(t, c.Imag() == 2.0, qt.IsTrue)
	})
}

func TestSqrt_SpecialValues(t *testing.T) {
	t.Run("sqrt of positive infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(sqrt +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})

	t.Run("sqrt of nan", func(t *testing.T) {
		result, err := runSchemeCode(t, `(sqrt +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})
}

func TestExpt(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Integer exponents
		{"2^3", `(expt 2 3)`, values.NewInteger(8)},
		{"2^0", `(expt 2 0)`, values.NewInteger(1)},
		{"10^2", `(expt 10 2)`, values.NewInteger(100)},
		{"0^0", `(expt 0 0)`, values.NewInteger(1)},
		{"0^1", `(expt 0 1)`, values.NewInteger(0)},
		{"1^100", `(expt 1 100)`, values.NewInteger(1)},
		{"-2^3", `(expt -2 3)`, values.NewInteger(-8)},
		{"-2^4", `(expt -2 4)`, values.NewInteger(16)},

		// Negative integer exponents
		{"2^-1", `(expt 2 -1)`, values.NewRational(1, 2)},
		{"2^-2", `(expt 2 -2)`, values.NewRational(1, 4)},
		{"10^-1", `(expt 10 -1)`, values.NewRational(1, 10)},

		// Float base
		{"2.0^3", `(expt 2.0 3)`, values.NewFloat(8.0)},
		{"2.5^2", `(expt 2.5 2)`, values.NewFloat(6.25)},

		// Float exponent (fractional power)
		{"4^0.5", `(expt 4 0.5)`, values.NewFloat(2.0)},
		// Note: 8^(1/3) and 27^(1/3) tested separately due to floating-point precision

		// Rational base
		{"(1/2)^2", `(expt 1/2 2)`, values.NewRational(1, 4)},
		{"(1/2)^-1", `(expt 1/2 -1)`, values.NewInteger(2)},
		{"(2/3)^2", `(expt 2/3 2)`, values.NewRational(4, 9)},

		// Note: i^2 tested separately due to floating-point precision

		// BigInteger
		{"bigint^2", `(expt #z10000000000 2)`, values.NewBigIntegerFromString("100000000000000000000", 10)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestExpt_SpecialValues(t *testing.T) {
	t.Run("infinity^2", func(t *testing.T) {
		result, err := runSchemeCode(t, `(expt +inf.0 2)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})

	t.Run("2^infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(expt 2 +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})

	t.Run("nan exponent", func(t *testing.T) {
		result, err := runSchemeCode(t, `(expt 2 +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})
}

// TestExpt_FloatingPointPrecision tests cases where floating-point precision
// prevents exact equality. These test mathematical correctness within epsilon.
func TestExpt_FloatingPointPrecision(t *testing.T) {
	const epsilon = 1e-10

	t.Run("8^(1/3) ≈ 2", func(t *testing.T) {
		result, err := runSchemeCode(t, `(expt 8 1/3)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.Abs(f.Datum()-2.0) < epsilon, qt.IsTrue)
	})

	t.Run("27^(1/3) ≈ 3", func(t *testing.T) {
		result, err := runSchemeCode(t, `(expt 27 1/3)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.Abs(f.Datum()-3.0) < epsilon, qt.IsTrue)
	})

	t.Run("i^2 ≈ -1", func(t *testing.T) {
		result, err := runSchemeCode(t, `(expt 0+1i 2)`)
		qt.Assert(t, err, qt.IsNil)
		c, ok := result.(*values.Complex)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.Abs(real(c.Datum())+1.0) < epsilon, qt.IsTrue)
		qt.Assert(t, math.Abs(imag(c.Datum())) < epsilon, qt.IsTrue)
	})
}

func TestSquare(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Integer operations
		{"square of 5", `(square 5)`, values.NewInteger(25)},
		{"square of -3", `(square -3)`, values.NewInteger(9)},
		{"square of 0", `(square 0)`, values.NewInteger(0)},
		{"square of 1", `(square 1)`, values.NewInteger(1)},

		// Float operations
		{"square of float", `(square 2.5)`, values.NewFloat(6.25)},
		{"square of negative float", `(square -2.5)`, values.NewFloat(6.25)},

		// Rational operations
		{"square of rational", `(square 1/2)`, values.NewRational(1, 4)},
		{"square of negative rational", `(square -2/3)`, values.NewRational(4, 9)},

		// Exact complex operations (integer parts are parsed as exact BigComplex)
		{"square of exact complex", `(square 1+1i)`, values.NewBigComplex(values.NewBigIntegerFromInt64(0), values.NewBigIntegerFromInt64(2))},
		{"square of exact imaginary", `(square 0+2i)`, values.NewBigIntegerFromInt64(-4)},

		// Inexact complex operations
		{"square of inexact complex", `(square 1.0+1.0i)`, values.NewComplexFromParts(0.0, 2.0)},
		{"square of inexact imaginary", `(square 0.0+2.0i)`, values.NewComplexFromParts(-4.0, 0.0)},

		// BigInteger operations
		{"square of biginteger", `(square #z10000000000)`, values.NewBigIntegerFromString("100000000000000000000", 10)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestSquare_SpecialValues(t *testing.T) {
	t.Run("square of infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(square +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})

	t.Run("square of negative infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(square -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})

	t.Run("square of nan", func(t *testing.T) {
		result, err := runSchemeCode(t, `(square +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})
}

func TestGcd(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic integer operations
		{"gcd of 12 and 8", `(gcd 12 8)`, values.NewInteger(4)},
		{"gcd of no args", `(gcd)`, values.NewInteger(0)},
		{"gcd of one arg", `(gcd 5)`, values.NewInteger(5)},
		{"gcd of coprime numbers", `(gcd 7 11)`, values.NewInteger(1)},
		{"gcd of same numbers", `(gcd 5 5)`, values.NewInteger(5)},
		{"gcd with zero", `(gcd 5 0)`, values.NewInteger(5)},
		{"gcd of two zeros", `(gcd 0 0)`, values.NewInteger(0)},

		// Negative numbers (gcd is always non-negative)
		{"gcd of negative numbers", `(gcd -12 8)`, values.NewInteger(4)},
		{"gcd of two negatives", `(gcd -12 -8)`, values.NewInteger(4)},
		{"gcd of negative single arg", `(gcd -5)`, values.NewInteger(5)},

		// Variadic (3+ args)
		{"gcd of three numbers", `(gcd 12 18 24)`, values.NewInteger(6)},
		{"gcd of four numbers", `(gcd 100 50 25 75)`, values.NewInteger(25)},

		// BigInteger operations
		{"gcd of bigintegers", `(gcd #z100000000000000000000 #z50000000000000000000)`, values.NewBigIntegerFromString("50000000000000000000", 10)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestLcm(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic integer operations
		{"lcm of 4 and 6", `(lcm 4 6)`, values.NewInteger(12)},
		{"lcm of no args", `(lcm)`, values.NewInteger(1)},
		{"lcm of one arg", `(lcm 5)`, values.NewInteger(5)},
		{"lcm of coprime numbers", `(lcm 7 11)`, values.NewInteger(77)},
		{"lcm of same numbers", `(lcm 5 5)`, values.NewInteger(5)},
		{"lcm with zero returns zero", `(lcm 5 0)`, values.NewInteger(0)},
		{"lcm of two zeros", `(lcm 0 0)`, values.NewInteger(0)},
		{"lcm of 1 and any number", `(lcm 1 42)`, values.NewInteger(42)},

		// Negative numbers (lcm is always non-negative)
		{"lcm of negative numbers", `(lcm -4 6)`, values.NewInteger(12)},
		{"lcm of two negatives", `(lcm -4 -6)`, values.NewInteger(12)},

		// Variadic (3+ args)
		{"lcm of three numbers", `(lcm 2 3 4)`, values.NewInteger(12)},
		{"lcm of four numbers", `(lcm 2 3 4 5)`, values.NewInteger(60)},

		// BigInteger operations
		{"lcm of bigintegers", `(lcm #z10000000000 #z30000000000)`, values.NewBigIntegerFromString("30000000000", 10)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestQuotient(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic integer operations (truncates toward zero)
		{"quotient 7/3", `(quotient 7 3)`, values.NewInteger(2)},
		{"quotient -7/3", `(quotient -7 3)`, values.NewInteger(-2)},
		{"quotient 7/-3", `(quotient 7 -3)`, values.NewInteger(-2)},
		{"quotient -7/-3", `(quotient -7 -3)`, values.NewInteger(2)},
		{"quotient exact division", `(quotient 10 2)`, values.NewInteger(5)},
		{"quotient zero dividend", `(quotient 0 5)`, values.NewInteger(0)},
		{"quotient 1/larger", `(quotient 1 5)`, values.NewInteger(0)},

		// Float operations
		{"quotient with floats", `(quotient 7.0 3.0)`, values.NewFloat(2.0)},
		{"quotient integer and float", `(quotient 7 3.0)`, values.NewFloat(2.0)},

		// BigInteger operations
		{"quotient bigintegers", `(quotient #z100000000000000000000 #z30000000000000000000)`, values.NewBigIntegerFromString("3", 10)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestQuotient_Errors(t *testing.T) {
	t.Run("quotient by zero", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(quotient 7 0)`)
	})
	t.Run("quotient string first arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(quotient "hello" 3)`)
	})
	t.Run("quotient boolean second arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(quotient 7 #t)`)
	})
}

func TestRemainder(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic integer operations (sign follows dividend)
		{"remainder 7/3", `(remainder 7 3)`, values.NewInteger(1)},
		{"remainder -7/3", `(remainder -7 3)`, values.NewInteger(-1)},
		{"remainder 7/-3", `(remainder 7 -3)`, values.NewInteger(1)},
		{"remainder -7/-3", `(remainder -7 -3)`, values.NewInteger(-1)},
		{"remainder exact division", `(remainder 10 2)`, values.NewInteger(0)},
		{"remainder zero dividend", `(remainder 0 5)`, values.NewInteger(0)},
		{"remainder 1/larger", `(remainder 1 5)`, values.NewInteger(1)},

		// Float operations
		{"remainder with floats", `(remainder 7.0 3.0)`, values.NewFloat(1.0)},
		{"remainder negative float", `(remainder -7.0 3.0)`, values.NewFloat(-1.0)},

		// BigInteger operations
		{"remainder bigintegers", `(remainder #z100000000000000000000 #z30000000000000000000)`, values.NewBigIntegerFromString("10000000000000000000", 10)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestRemainder_Errors(t *testing.T) {
	t.Run("remainder by zero", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(remainder 7 0)`)
	})
	t.Run("remainder string first arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(remainder "hello" 3)`)
	})
	t.Run("remainder boolean second arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(remainder 7 #t)`)
	})
}

func TestModulo(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic integer operations (sign follows divisor)
		{"modulo 7/3", `(modulo 7 3)`, values.NewInteger(1)},
		{"modulo -7/3", `(modulo -7 3)`, values.NewInteger(2)},
		{"modulo 7/-3", `(modulo 7 -3)`, values.NewInteger(-2)},
		{"modulo -7/-3", `(modulo -7 -3)`, values.NewInteger(-1)},
		{"modulo exact division", `(modulo 10 2)`, values.NewInteger(0)},
		{"modulo zero dividend", `(modulo 0 5)`, values.NewInteger(0)},
		{"modulo 1/larger", `(modulo 1 5)`, values.NewInteger(1)},

		// Additional sign cases
		{"modulo -1/3", `(modulo -1 3)`, values.NewInteger(2)},
		{"modulo 1/-3", `(modulo 1 -3)`, values.NewInteger(-2)},

		// Float operations
		{"modulo with floats", `(modulo 7.0 3.0)`, values.NewFloat(1.0)},
		{"modulo negative dividend float", `(modulo -7.0 3.0)`, values.NewFloat(2.0)},

		// BigInteger operations
		{"modulo bigintegers", `(modulo #z100000000000000000007 #z10)`, values.NewBigIntegerFromString("7", 10)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestModulo_Errors(t *testing.T) {
	t.Run("modulo by zero", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(modulo 7 0)`)
	})
	t.Run("modulo string first arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(modulo "hello" 3)`)
	})
	t.Run("modulo boolean second arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(modulo 7 #t)`)
	})
}

func TestMax(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic integer operations
		{"max of two", `(max 3 5)`, values.NewInteger(5)},
		{"max of three", `(max 3 5 1)`, values.NewInteger(5)},
		{"max of one", `(max 7)`, values.NewInteger(7)},
		{"max with negatives", `(max -3 -5 -1)`, values.NewInteger(-1)},
		{"max with zero", `(max 0 -5 5)`, values.NewInteger(5)},

		// Float operations
		{"max of floats", `(max 3.5 2.5)`, values.NewFloat(3.5)},
		{"max of negative floats", `(max -1.5 -2.5)`, values.NewFloat(-1.5)},

		// Mixed types (result is inexact if any arg is inexact)
		{"max integer and float", `(max 3 2.5)`, values.NewFloat(3.0)},
		{"max float wins", `(max 2 3.5)`, values.NewFloat(3.5)},

		// Rational operations
		{"max of rationals", `(max 1/2 3/4)`, values.NewRational(3, 4)},
		{"max rational and integer", `(max 1/2 1)`, values.NewInteger(1)},

		// BigInteger operations
		{"max of bigintegers", `(max #z10000000000000000000 #z20000000000000000000)`, values.NewBigIntegerFromString("20000000000000000000", 10)},

		// BigFloat operations (always inexact)
		{"max of bigfloats", `(max #m3.5 #m2.5)`, values.NewBigFloatFromString("3.5")},
		{"max bigfloat and integer", `(max #m2.5 3)`, values.NewFloat(3.0)}, // Integer wins but becomes inexact
		{"max integer and bigfloat", `(max 3 #m2.5)`, values.NewFloat(3.0)}, // Integer wins but becomes inexact
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestMax_SpecialValues(t *testing.T) {
	t.Run("max with positive infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(max 1 2 +inf.0 3)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})

	t.Run("max with negative infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(max -inf.0 -100)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, f.Datum() == -100.0, qt.IsTrue)
	})

	t.Run("max with nan", func(t *testing.T) {
		result, err := runSchemeCode(t, `(max 1 +nan.0 3)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})
}

func TestMax_Errors(t *testing.T) {
	t.Run("max no arguments", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(max)`)
	})
	t.Run("max string first arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(max "hello" 1)`)
	})
	t.Run("max boolean second arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(max 1 #t)`)
	})
}

func TestMin(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic integer operations
		{"min of two", `(min 3 5)`, values.NewInteger(3)},
		{"min of three", `(min 3 5 1)`, values.NewInteger(1)},
		{"min of one", `(min 7)`, values.NewInteger(7)},
		{"min with negatives", `(min -3 -5 -1)`, values.NewInteger(-5)},
		{"min with zero", `(min 0 -5 5)`, values.NewInteger(-5)},

		// Float operations
		{"min of floats", `(min 3.5 2.5)`, values.NewFloat(2.5)},
		{"min of negative floats", `(min -1.5 -2.5)`, values.NewFloat(-2.5)},

		// Mixed types (result is inexact if any arg is inexact)
		{"min integer and float", `(min 3 2.5)`, values.NewFloat(2.5)},
		{"min integer wins", `(min 2 3.5)`, values.NewFloat(2.0)},

		// Rational operations
		{"min of rationals", `(min 1/2 3/4)`, values.NewRational(1, 2)},
		{"min rational and integer", `(min 1/2 1)`, values.NewRational(1, 2)},

		// BigInteger operations
		{"min of bigintegers", `(min #z10000000000000000000 #z20000000000000000000)`, values.NewBigIntegerFromString("10000000000000000000", 10)},

		// BigFloat operations (always inexact)
		{"min of bigfloats", `(min #m3.5 #m2.5)`, values.NewBigFloatFromString("2.5")},
		{"min bigfloat and integer", `(min #m2.5 3)`, values.NewBigFloatFromString("2.5")}, // BigFloat wins, stays BigFloat
		{"min integer and bigfloat", `(min 2 #m3.5)`, values.NewFloat(2.0)},                // Integer wins but becomes inexact
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestMin_SpecialValues(t *testing.T) {
	t.Run("min with negative infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(min 1 2 -inf.0 3)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), -1), qt.IsTrue)
	})

	t.Run("min with positive infinity", func(t *testing.T) {
		result, err := runSchemeCode(t, `(min +inf.0 100)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, f.Datum() == 100.0, qt.IsTrue)
	})

	t.Run("min with nan", func(t *testing.T) {
		result, err := runSchemeCode(t, `(min 1 +nan.0 3)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})
}

func TestMin_Errors(t *testing.T) {
	t.Run("min no arguments", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(min)`)
	})
	t.Run("min string first arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(min "hello" 1)`)
	})
	t.Run("min boolean second arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(min 1 #t)`)
	})
}

func TestAddition_Errors(t *testing.T) {
	t.Run("add string arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(+ "hello" 1)`)
	})
	t.Run("add boolean arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(+ #t 1)`)
	})
}

func TestMultiplication_Errors(t *testing.T) {
	t.Run("multiply string arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(* "hello" 2)`)
	})
	t.Run("multiply boolean arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(* #t 2)`)
	})
}

func TestAbs_Errors(t *testing.T) {
	t.Run("abs of string", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(abs "hello")`)
	})
	t.Run("abs of boolean", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(abs #t)`)
	})
}

func TestSquare_Errors(t *testing.T) {
	t.Run("square of string", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(square "hello")`)
	})
	t.Run("square of boolean", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(square #t)`)
	})
}

func TestSqrt_Errors(t *testing.T) {
	t.Run("sqrt of string", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(sqrt "hello")`)
	})
	t.Run("sqrt of boolean", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(sqrt #t)`)
	})
}

func TestExpt_Errors(t *testing.T) {
	t.Run("expt string base", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(expt "hello" 2)`)
	})
	t.Run("expt boolean exponent", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(expt 2 #t)`)
	})
}

func TestGcd_Errors(t *testing.T) {
	t.Run("gcd string first arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(gcd "hello" 4)`)
	})
	t.Run("gcd boolean second arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(gcd 4 #t)`)
	})
}

func TestLcm_Errors(t *testing.T) {
	t.Run("lcm string first arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(lcm "hello" 4)`)
	})
	t.Run("lcm boolean second arg", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(lcm 4 #t)`)
	})
}

// TestDivisionByZero_SchemeException verifies that division by zero across all
// numeric types is caught at the VM boundary and converted to a proper Scheme
// exception that guard can handle. This tests the recover in
// OperationForeignFunctionCall.Apply, not just the values package panic.
func TestDivisionByZero_SchemeException(t *testing.T) {
	c := qt.New(t)
	tcs := []schemeCodeTestCase{
		// Integer / 0
		{"integer div zero caught by guard",
			`(guard (exn (#t "caught")) (/ 1 0))`,
			values.NewString("caught")},
		// BigInteger / 0 (Issue #22)
		{"biginteger div zero caught by guard",
			`(guard (exn (#t "caught")) (/ (expt 2 100) 0))`,
			values.NewString("caught")},
		// Float / 0
		{"float div zero caught by guard",
			`(guard (exn (#t "caught")) (/ 1.5 0))`,
			values.NewString("caught")},
		// Rational / 0
		{"rational div zero caught by guard",
			`(guard (exn (#t "caught")) (/ 1/3 0))`,
			values.NewString("caught")},
		// Complex / 0
		{"complex div zero caught by guard",
			`(guard (exn (#t "caught")) (/ 1+2i 0))`,
			values.NewString("caught")},
	}
	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			result, err := runSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, values.SchemeEquals, tc.expected)
		})
	}
}

// TestDivisionByZero_ReturnsError verifies that division by zero returns an
// error through the VM rather than panicking.
func TestDivisionByZero_ReturnsError(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{"integer / 0", `(/ 1 0)`},
		{"biginteger / 0", `(/ (expt 2 100) 0)`},
		{"float / 0", `(/ 1.5 0)`},
		{"rational / 0", `(/ 1/3 0)`},
		{"complex / 0", `(/ 1+2i 0)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
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
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, values.ErrDivisionByZero), qt.IsTrue)
			qt.Assert(t, err, qt.ErrorMatches, `(?s).*division by zero.*`)
		})
	}
}

func TestOverflowPromotion(t *testing.T) {
	// R7RS §6.2.3: Integer overflow should promote to BigInteger
	tcs := []schemeCodeTestCase{
		{"add overflow", `(+ 9223372036854775807 1)`, values.NewBigIntegerFromString("9223372036854775808", 10)},
		{"subtract underflow", `(- -9223372036854775808 1)`, values.NewBigIntegerFromString("-9223372036854775809", 10)},
		{"multiply overflow", `(* 9223372036854775807 2)`, values.NewBigIntegerFromString("18446744073709551614", 10)},
		{"abs MinInt64", `(abs -9223372036854775808)`, values.NewBigIntegerFromString("9223372036854775808", 10)},
		{"lcm overflow", `(lcm 9223372036854775807 2)`, values.NewBigIntegerFromString("18446744073709551614", 10)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}

	// Exactness and type preservation
	runSchemeCodeExpectTrue(t, `(exact? (+ 9223372036854775807 1))`)
	runSchemeCodeExpectTrue(t, `(integer? (+ 9223372036854775807 1))`)
	runSchemeCodeExpectTrue(t, `(= (+ 9223372036854775807 1) 9223372036854775808)`)
	runSchemeCodeExpectTrue(t, `(= (- -9223372036854775808 1) -9223372036854775809)`)
	runSchemeCodeExpectTrue(t, `(= (* 9223372036854775807 2) 18446744073709551614)`)
	runSchemeCodeExpectTrue(t, `(= (abs -9223372036854775808) 9223372036854775808)`)
}

// TestSpecialValueArithmetic tests arithmetic with +inf.0, -inf.0, and +nan.0.
// R7RS §6.2.6: These follow IEEE 754 rules.
func TestSpecialValueArithmetic(t *testing.T) {
	// inf + -inf = nan
	t.Run("+inf.0 + -inf.0 is nan", func(t *testing.T) {
		result, err := runSchemeCode(t, "(+ +inf.0 -inf.0)")
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})

	// 0 * inf = nan (IEEE 754)
	t.Run("0 * +inf.0 is nan", func(t *testing.T) {
		result, err := runSchemeCode(t, "(* 0 +inf.0)")
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})

	// Division by exact zero is an error.
	t.Run("(/ 0 0) is error", func(t *testing.T) {
		_, err := runSchemeCode(t, "(/ 0 0)")
		qt.Assert(t, err, qt.IsNotNil)
	})

	t.Run("(/ 1.0 0.0) is error", func(t *testing.T) {
		_, err := runSchemeCode(t, "(/ 1.0 0.0)")
		qt.Assert(t, err, qt.IsNotNil)
	})

	// inf arithmetic
	t.Run("+inf.0 + 1 is +inf.0", func(t *testing.T) {
		result, err := runSchemeCode(t, "(+ +inf.0 1)")
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, 1), qt.IsTrue)
	})

	t.Run("-inf.0 - 1 is -inf.0", func(t *testing.T) {
		result, err := runSchemeCode(t, "(- -inf.0 1)")
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Value, -1), qt.IsTrue)
	})

	// nan propagation
	t.Run("nan + 1 is nan", func(t *testing.T) {
		result, err := runSchemeCode(t, "(+ +nan.0 1)")
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Value), qt.IsTrue)
	})
}
