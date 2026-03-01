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
	"math"
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestRounding tests floor, ceiling, round, and truncate with floats, integers, and rationals
func TestRounding(t *testing.T) {
	tests := []testhelpers.SchemeCodeTestCase{
		// Floor tests - floats and integers
		{
			Name:     "floor positive float",
			Code:     "(floor 3.7)",
			Expected: values.NewFloat(3.0),
		},
		{
			Name:     "floor negative float",
			Code:     "(floor -3.7)",
			Expected: values.NewFloat(-4.0),
		},
		{
			Name:     "floor integer passthrough",
			Code:     "(floor 3)",
			Expected: values.NewInteger(3),
		},
		{
			Name:     "floor exact zero",
			Code:     "(floor 0)",
			Expected: values.NewInteger(0),
		},
		{
			Name:     "floor inexact zero",
			Code:     "(floor 0.0)",
			Expected: values.NewFloat(0.0),
		},
		{
			Name:     "floor large positive float",
			Code:     "(floor 100.9)",
			Expected: values.NewFloat(100.0),
		},
		{
			Name:     "floor large negative float",
			Code:     "(floor -100.1)",
			Expected: values.NewFloat(-101.0),
		},
		// Floor tests - rationals (exact inputs → exact outputs per R7RS)
		{
			Name:     "floor 5/2 positive",
			Code:     "(floor 5/2)",
			Expected: values.NewInteger(2),
		},
		{
			Name:     "floor -5/2 negative",
			Code:     "(floor -5/2)",
			Expected: values.NewInteger(-3),
		},
		{
			Name:     "floor 7/3",
			Code:     "(floor 7/3)",
			Expected: values.NewInteger(2),
		},
		{
			Name:     "floor -7/3 negative",
			Code:     "(floor -7/3)",
			Expected: values.NewInteger(-3),
		},
		{
			Name:     "floor 1/2",
			Code:     "(floor 1/2)",
			Expected: values.NewInteger(0),
		},
		{
			Name:     "floor -1/2 negative",
			Code:     "(floor -1/2)",
			Expected: values.NewInteger(-1),
		},
		// Ceiling tests - floats and integers
		{
			Name:     "ceiling positive float",
			Code:     "(ceiling 3.2)",
			Expected: values.NewFloat(4.0),
		},
		{
			Name:     "ceiling negative float",
			Code:     "(ceiling -3.2)",
			Expected: values.NewFloat(-3.0),
		},
		{
			Name:     "ceiling integer passthrough",
			Code:     "(ceiling 3)",
			Expected: values.NewInteger(3),
		},
		{
			Name:     "ceiling exact zero",
			Code:     "(ceiling 0)",
			Expected: values.NewInteger(0),
		},
		{
			Name:     "ceiling inexact zero",
			Code:     "(ceiling 0.0)",
			Expected: values.NewFloat(0.0),
		},
		{
			Name:     "ceiling large positive float",
			Code:     "(ceiling 100.1)",
			Expected: values.NewFloat(101.0),
		},
		{
			Name:     "ceiling large negative float",
			Code:     "(ceiling -100.9)",
			Expected: values.NewFloat(-100.0),
		},
		// Ceiling tests - rationals (exact inputs → exact outputs per R7RS)
		{
			Name:     "ceiling 5/2 positive",
			Code:     "(ceiling 5/2)",
			Expected: values.NewInteger(3),
		},
		{
			Name:     "ceiling -5/2 negative",
			Code:     "(ceiling -5/2)",
			Expected: values.NewInteger(-2),
		},
		{
			Name:     "ceiling 7/3",
			Code:     "(ceiling 7/3)",
			Expected: values.NewInteger(3),
		},
		{
			Name:     "ceiling -7/3 negative",
			Code:     "(ceiling -7/3)",
			Expected: values.NewInteger(-2),
		},
		{
			Name:     "ceiling 1/2",
			Code:     "(ceiling 1/2)",
			Expected: values.NewInteger(1),
		},
		{
			Name:     "ceiling -1/2 negative",
			Code:     "(ceiling -1/2)",
			Expected: values.NewInteger(0),
		},
		// Round tests - floats and integers (R7RS §6.2.6: round to even)
		{
			Name:     "round 3.5 half to even",
			Code:     "(round 3.5)",
			Expected: values.NewFloat(4.0), // 4 is even
		},
		{
			Name:     "round 2.5 half to even",
			Code:     "(round 2.5)",
			Expected: values.NewFloat(2.0), // 2 is even
		},
		{
			Name:     "round -3.5 half to even",
			Code:     "(round -3.5)",
			Expected: values.NewFloat(-4.0), // -4 is even
		},
		{
			Name:     "round integer passthrough",
			Code:     "(round 3)",
			Expected: values.NewInteger(3),
		},
		{
			Name:     "round 4.5 half to even",
			Code:     "(round 4.5)",
			Expected: values.NewFloat(4.0), // 4 is even
		},
		{
			Name:     "round 5.5 half to even",
			Code:     "(round 5.5)",
			Expected: values.NewFloat(6.0), // 6 is even
		},
		{
			Name:     "round exact zero",
			Code:     "(round 0)",
			Expected: values.NewInteger(0),
		},
		{
			Name:     "round inexact zero",
			Code:     "(round 0.0)",
			Expected: values.NewFloat(0.0),
		},
		{
			Name:     "round 3.2 down",
			Code:     "(round 3.2)",
			Expected: values.NewFloat(3.0),
		},
		{
			Name:     "round 3.8 up",
			Code:     "(round 3.8)",
			Expected: values.NewFloat(4.0),
		},
		// Additional round-to-even half cases
		{
			Name:     "round 0.5 half to even",
			Code:     "(round 0.5)",
			Expected: values.NewFloat(0.0), // 0 is even
		},
		{
			Name:     "round 1.5 half to even",
			Code:     "(round 1.5)",
			Expected: values.NewFloat(2.0), // 2 is even
		},
		{
			Name:     "round -0.5 half to even",
			Code:     "(round -0.5)",
			Expected: values.NewFloat(0.0), // 0 is even
		},
		{
			Name:     "round -1.5 half to even",
			Code:     "(round -1.5)",
			Expected: values.NewFloat(-2.0), // -2 is even
		},
		{
			Name:     "round 6.5 half to even",
			Code:     "(round 6.5)",
			Expected: values.NewFloat(6.0), // 6 is even
		},
		{
			Name:     "round 7.5 half to even",
			Code:     "(round 7.5)",
			Expected: values.NewFloat(8.0), // 8 is even
		},
		// Round tests - rationals (exact inputs → exact outputs per R7RS, round to even)
		{
			Name:     "round 5/2 half to even",
			Code:     "(round 5/2)",
			Expected: values.NewInteger(2), // 2.5 rounds to 2 (even)
		},
		{
			Name:     "round 7/2 half to even",
			Code:     "(round 7/2)",
			Expected: values.NewInteger(4), // 3.5 rounds to 4 (even)
		},
		{
			Name:     "round 7/3",
			Code:     "(round 7/3)",
			Expected: values.NewInteger(2), // 2.333... rounds to 2
		},
		{
			Name:     "round -5/2 half to even",
			Code:     "(round -5/2)",
			Expected: values.NewInteger(-2), // -2.5 rounds to -2 (even)
		},
		{
			Name:     "round 8/3",
			Code:     "(round 8/3)",
			Expected: values.NewInteger(3), // 2.666... rounds to 3
		},
		{
			Name:     "round 1/3",
			Code:     "(round 1/3)",
			Expected: values.NewInteger(0), // 0.333... rounds to 0
		},
		{
			Name:     "round 1/2 half to even",
			Code:     "(round 1/2)",
			Expected: values.NewInteger(0), // 0.5 rounds to 0 (even)
		},
		{
			Name:     "round 3/2 half to even",
			Code:     "(round 3/2)",
			Expected: values.NewInteger(2), // 1.5 rounds to 2 (even)
		},
		// Truncate tests - floats and integers
		{
			Name:     "truncate positive float",
			Code:     "(truncate 3.7)",
			Expected: values.NewFloat(3.0),
		},
		{
			Name:     "truncate negative float",
			Code:     "(truncate -3.7)",
			Expected: values.NewFloat(-3.0),
		},
		{
			Name:     "truncate integer passthrough",
			Code:     "(truncate 3)",
			Expected: values.NewInteger(3),
		},
		{
			Name:     "truncate exact zero",
			Code:     "(truncate 0)",
			Expected: values.NewInteger(0),
		},
		{
			Name:     "truncate inexact zero",
			Code:     "(truncate 0.0)",
			Expected: values.NewFloat(0.0),
		},
		{
			Name:     "truncate large positive float",
			Code:     "(truncate 999.999)",
			Expected: values.NewFloat(999.0),
		},
		{
			Name:     "truncate large negative float",
			Code:     "(truncate -999.999)",
			Expected: values.NewFloat(-999.0),
		},
		// Truncate tests - rationals (exact inputs → exact outputs per R7RS)
		{
			Name:     "truncate 5/2 positive",
			Code:     "(truncate 5/2)",
			Expected: values.NewInteger(2),
		},
		{
			Name:     "truncate -5/2 negative",
			Code:     "(truncate -5/2)",
			Expected: values.NewInteger(-2),
		},
		{
			Name:     "truncate 7/3",
			Code:     "(truncate 7/3)",
			Expected: values.NewInteger(2),
		},
		{
			Name:     "truncate -7/3 negative",
			Code:     "(truncate -7/3)",
			Expected: values.NewInteger(-2),
		},
		{
			Name:     "truncate 1/2",
			Code:     "(truncate 1/2)",
			Expected: values.NewInteger(0),
		},
		{
			Name:     "truncate -1/2 negative",
			Code:     "(truncate -1/2)",
			Expected: values.NewInteger(0),
		},
	}

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tt.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tt.Expected)
		})
	}
}

// TestRounding_TypeErrors tests that rounding functions reject non-numeric arguments.
// R7RS §6.2.6: floor, ceiling, round, truncate require numeric arguments.
func TestRounding_TypeErrors(t *testing.T) {
	t.Run("floor of string", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(floor "hello")`)
	})
	t.Run("floor of boolean", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(floor #t)`)
	})
	t.Run("ceiling of string", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(ceiling "hello")`)
	})
	t.Run("ceiling of boolean", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(ceiling #t)`)
	})
	t.Run("round of string", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(round "hello")`)
	})
	t.Run("round of boolean", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(round #t)`)
	})
	t.Run("truncate of string", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(truncate "hello")`)
	})
	t.Run("truncate of boolean", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectError(t, `(truncate #t)`)
	})
}

// TestRounding_SpecialValues tests rounding functions with +inf.0, -inf.0, and +nan.0.
// R7RS §6.2.6: These functions return their argument for infinite and NaN inputs.
func TestRounding_SpecialValues(t *testing.T) {
	t.Run("floor of +inf.0", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(floor +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})
	t.Run("floor of -inf.0", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(floor -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), -1), qt.IsTrue)
	})
	t.Run("floor of +nan.0", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(floor +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})
	t.Run("ceiling of +inf.0", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(ceiling +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})
	t.Run("ceiling of -inf.0", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(ceiling -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), -1), qt.IsTrue)
	})
	t.Run("ceiling of +nan.0", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(ceiling +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})
	t.Run("round of +inf.0", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(round +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})
	t.Run("round of -inf.0", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(round -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), -1), qt.IsTrue)
	})
	t.Run("round of +nan.0", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(round +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})
	t.Run("truncate of +inf.0", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(truncate +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})
	t.Run("truncate of -inf.0", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(truncate -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), -1), qt.IsTrue)
	})
	t.Run("truncate of +nan.0", func(t *testing.T) {
		result, err := testhelpers.RunSchemeCode(t, `(truncate +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})
}

// TestFloorQuotient_AllSigns tests floor-quotient with all sign combinations.
// R7RS §6.2.6: floor-quotient returns floor(n1/n2).
func TestFloorQuotient_AllSigns(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "(10, 3) -> 3", Code: `(floor-quotient 10 3)`, Expected: values.NewInteger(3)},
		{Name: "(-10, 3) -> -4", Code: `(floor-quotient -10 3)`, Expected: values.NewInteger(-4)},
		{Name: "(10, -3) -> -4", Code: `(floor-quotient 10 -3)`, Expected: values.NewInteger(-4)},
		{Name: "(-10, -3) -> 3", Code: `(floor-quotient -10 -3)`, Expected: values.NewInteger(3)},
		{Name: "float input", Code: `(floor-quotient 10.0 3)`, Expected: values.NewFloat(3.0)},
		{Name: "rational input", Code: `(floor-quotient 7/2 3/2)`, Expected: values.NewInteger(2)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestFloorRemainder_AllSigns tests floor-remainder with all sign combinations.
// R7RS §6.2.6: floor-remainder returns n1 - n2*floor(n1/n2).
func TestFloorRemainder_AllSigns(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "(10, 3) -> 1", Code: `(floor-remainder 10 3)`, Expected: values.NewInteger(1)},
		{Name: "(-10, 3) -> 2", Code: `(floor-remainder -10 3)`, Expected: values.NewInteger(2)},
		{Name: "(10, -3) -> -2", Code: `(floor-remainder 10 -3)`, Expected: values.NewInteger(-2)},
		{Name: "(-10, -3) -> -1", Code: `(floor-remainder -10 -3)`, Expected: values.NewInteger(-1)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestTruncateQuotient_AllSigns tests truncate-quotient with all sign combinations.
// R7RS §6.2.6: truncate-quotient returns truncate(n1/n2).
func TestTruncateQuotient_AllSigns(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "(10, 3) -> 3", Code: `(truncate-quotient 10 3)`, Expected: values.NewInteger(3)},
		{Name: "(-10, 3) -> -3", Code: `(truncate-quotient -10 3)`, Expected: values.NewInteger(-3)},
		{Name: "(10, -3) -> -3", Code: `(truncate-quotient 10 -3)`, Expected: values.NewInteger(-3)},
		{Name: "(-10, -3) -> 3", Code: `(truncate-quotient -10 -3)`, Expected: values.NewInteger(3)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestTruncateRemainder_AllSigns tests truncate-remainder with all sign combinations.
// R7RS §6.2.6: truncate-remainder returns n1 - n2*truncate(n1/n2).
func TestTruncateRemainder_AllSigns(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "(10, 3) -> 1", Code: `(truncate-remainder 10 3)`, Expected: values.NewInteger(1)},
		{Name: "(-10, 3) -> -1", Code: `(truncate-remainder -10 3)`, Expected: values.NewInteger(-1)},
		{Name: "(10, -3) -> 1", Code: `(truncate-remainder 10 -3)`, Expected: values.NewInteger(1)},
		{Name: "(-10, -3) -> -1", Code: `(truncate-remainder -10 -3)`, Expected: values.NewInteger(-1)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestFloorTruncateMultiValue tests floor/ and truncate/ multi-value returns.
// R7RS §6.2.6: These return two values (quotient and remainder).
func TestFloorTruncateMultiValue(t *testing.T) {
	// floor/ returns (quotient remainder)
	t.Run("floor/ 10 3", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectTrue(t, "(equal? (call-with-values (lambda () (floor/ 10 3)) list) '(3 1))")
	})
	t.Run("floor/ -10 3", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectTrue(t, "(equal? (call-with-values (lambda () (floor/ -10 3)) list) '(-4 2))")
	})
	// truncate/ returns (quotient remainder)
	t.Run("truncate/ 10 3", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectTrue(t, "(equal? (call-with-values (lambda () (truncate/ 10 3)) list) '(3 1))")
	})
	t.Run("truncate/ -10 3", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectTrue(t, "(equal? (call-with-values (lambda () (truncate/ -10 3)) list) '(-3 -1))")
	})
}

// TestFloorTruncateInvariant tests the R7RS invariant: x = q*d + r.
func TestFloorTruncateInvariant(t *testing.T) {
	// (= x (+ (* (floor-quotient x d) d) (floor-remainder x d)))
	t.Run("floor invariant positive", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectTrue(t, "(= 10 (+ (* (floor-quotient 10 3) 3) (floor-remainder 10 3)))")
	})
	t.Run("floor invariant negative dividend", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectTrue(t, "(= -10 (+ (* (floor-quotient -10 3) 3) (floor-remainder -10 3)))")
	})
	// (= x (+ (* (truncate-quotient x d) d) (truncate-remainder x d)))
	t.Run("truncate invariant positive", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectTrue(t, "(= 10 (+ (* (truncate-quotient 10 3) 3) (truncate-remainder 10 3)))")
	})
	t.Run("truncate invariant negative dividend", func(t *testing.T) {
		testhelpers.RunSchemeCodeExpectTrue(t, "(= -10 (+ (* (truncate-quotient -10 3) 3) (truncate-remainder -10 3)))")
	})
}

// TestFloorTruncateDivisionByZero tests division by zero produces an error.
func TestFloorTruncateDivisionByZero(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "floor-quotient by zero", Code: `(floor-quotient 10 0)`},
		{Name: "floor-remainder by zero", Code: `(floor-remainder 10 0)`},
		{Name: "truncate-quotient by zero", Code: `(truncate-quotient 10 0)`},
		{Name: "truncate-remainder by zero", Code: `(truncate-remainder 10 0)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
