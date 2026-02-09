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

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// TestRounding tests floor, ceiling, round, and truncate with floats, integers, and rationals
func TestRounding(t *testing.T) {
	tests := []schemeCodeTestCase{
		// Floor tests - floats and integers
		{
			name:     "floor positive float",
			code:     "(floor 3.7)",
			expected: values.NewFloat(3.0),
		},
		{
			name:     "floor negative float",
			code:     "(floor -3.7)",
			expected: values.NewFloat(-4.0),
		},
		{
			name:     "floor integer passthrough",
			code:     "(floor 3)",
			expected: values.NewInteger(3),
		},
		{
			name:     "floor exact zero",
			code:     "(floor 0)",
			expected: values.NewInteger(0),
		},
		{
			name:     "floor inexact zero",
			code:     "(floor 0.0)",
			expected: values.NewFloat(0.0),
		},
		{
			name:     "floor large positive float",
			code:     "(floor 100.9)",
			expected: values.NewFloat(100.0),
		},
		{
			name:     "floor large negative float",
			code:     "(floor -100.1)",
			expected: values.NewFloat(-101.0),
		},
		// Floor tests - rationals (exact inputs → exact outputs per R7RS)
		{
			name:     "floor 5/2 positive",
			code:     "(floor 5/2)",
			expected: values.NewInteger(2),
		},
		{
			name:     "floor -5/2 negative",
			code:     "(floor -5/2)",
			expected: values.NewInteger(-3),
		},
		{
			name:     "floor 7/3",
			code:     "(floor 7/3)",
			expected: values.NewInteger(2),
		},
		{
			name:     "floor -7/3 negative",
			code:     "(floor -7/3)",
			expected: values.NewInteger(-3),
		},
		{
			name:     "floor 1/2",
			code:     "(floor 1/2)",
			expected: values.NewInteger(0),
		},
		{
			name:     "floor -1/2 negative",
			code:     "(floor -1/2)",
			expected: values.NewInteger(-1),
		},
		// Ceiling tests - floats and integers
		{
			name:     "ceiling positive float",
			code:     "(ceiling 3.2)",
			expected: values.NewFloat(4.0),
		},
		{
			name:     "ceiling negative float",
			code:     "(ceiling -3.2)",
			expected: values.NewFloat(-3.0),
		},
		{
			name:     "ceiling integer passthrough",
			code:     "(ceiling 3)",
			expected: values.NewInteger(3),
		},
		{
			name:     "ceiling exact zero",
			code:     "(ceiling 0)",
			expected: values.NewInteger(0),
		},
		{
			name:     "ceiling inexact zero",
			code:     "(ceiling 0.0)",
			expected: values.NewFloat(0.0),
		},
		{
			name:     "ceiling large positive float",
			code:     "(ceiling 100.1)",
			expected: values.NewFloat(101.0),
		},
		{
			name:     "ceiling large negative float",
			code:     "(ceiling -100.9)",
			expected: values.NewFloat(-100.0),
		},
		// Ceiling tests - rationals (exact inputs → exact outputs per R7RS)
		{
			name:     "ceiling 5/2 positive",
			code:     "(ceiling 5/2)",
			expected: values.NewInteger(3),
		},
		{
			name:     "ceiling -5/2 negative",
			code:     "(ceiling -5/2)",
			expected: values.NewInteger(-2),
		},
		{
			name:     "ceiling 7/3",
			code:     "(ceiling 7/3)",
			expected: values.NewInteger(3),
		},
		{
			name:     "ceiling -7/3 negative",
			code:     "(ceiling -7/3)",
			expected: values.NewInteger(-2),
		},
		{
			name:     "ceiling 1/2",
			code:     "(ceiling 1/2)",
			expected: values.NewInteger(1),
		},
		{
			name:     "ceiling -1/2 negative",
			code:     "(ceiling -1/2)",
			expected: values.NewInteger(0),
		},
		// Round tests - floats and integers (R7RS §6.2.6: round to even)
		{
			name:     "round 3.5 half to even",
			code:     "(round 3.5)",
			expected: values.NewFloat(4.0), // 4 is even
		},
		{
			name:     "round 2.5 half to even",
			code:     "(round 2.5)",
			expected: values.NewFloat(2.0), // 2 is even
		},
		{
			name:     "round -3.5 half to even",
			code:     "(round -3.5)",
			expected: values.NewFloat(-4.0), // -4 is even
		},
		{
			name:     "round integer passthrough",
			code:     "(round 3)",
			expected: values.NewInteger(3),
		},
		{
			name:     "round 4.5 half to even",
			code:     "(round 4.5)",
			expected: values.NewFloat(4.0), // 4 is even
		},
		{
			name:     "round 5.5 half to even",
			code:     "(round 5.5)",
			expected: values.NewFloat(6.0), // 6 is even
		},
		{
			name:     "round exact zero",
			code:     "(round 0)",
			expected: values.NewInteger(0),
		},
		{
			name:     "round inexact zero",
			code:     "(round 0.0)",
			expected: values.NewFloat(0.0),
		},
		{
			name:     "round 3.2 down",
			code:     "(round 3.2)",
			expected: values.NewFloat(3.0),
		},
		{
			name:     "round 3.8 up",
			code:     "(round 3.8)",
			expected: values.NewFloat(4.0),
		},
		// Additional round-to-even half cases
		{
			name:     "round 0.5 half to even",
			code:     "(round 0.5)",
			expected: values.NewFloat(0.0), // 0 is even
		},
		{
			name:     "round 1.5 half to even",
			code:     "(round 1.5)",
			expected: values.NewFloat(2.0), // 2 is even
		},
		{
			name:     "round -0.5 half to even",
			code:     "(round -0.5)",
			expected: values.NewFloat(0.0), // 0 is even
		},
		{
			name:     "round -1.5 half to even",
			code:     "(round -1.5)",
			expected: values.NewFloat(-2.0), // -2 is even
		},
		{
			name:     "round 6.5 half to even",
			code:     "(round 6.5)",
			expected: values.NewFloat(6.0), // 6 is even
		},
		{
			name:     "round 7.5 half to even",
			code:     "(round 7.5)",
			expected: values.NewFloat(8.0), // 8 is even
		},
		// Round tests - rationals (exact inputs → exact outputs per R7RS, round to even)
		{
			name:     "round 5/2 half to even",
			code:     "(round 5/2)",
			expected: values.NewInteger(2), // 2.5 rounds to 2 (even)
		},
		{
			name:     "round 7/2 half to even",
			code:     "(round 7/2)",
			expected: values.NewInteger(4), // 3.5 rounds to 4 (even)
		},
		{
			name:     "round 7/3",
			code:     "(round 7/3)",
			expected: values.NewInteger(2), // 2.333... rounds to 2
		},
		{
			name:     "round -5/2 half to even",
			code:     "(round -5/2)",
			expected: values.NewInteger(-2), // -2.5 rounds to -2 (even)
		},
		{
			name:     "round 8/3",
			code:     "(round 8/3)",
			expected: values.NewInteger(3), // 2.666... rounds to 3
		},
		{
			name:     "round 1/3",
			code:     "(round 1/3)",
			expected: values.NewInteger(0), // 0.333... rounds to 0
		},
		{
			name:     "round 1/2 half to even",
			code:     "(round 1/2)",
			expected: values.NewInteger(0), // 0.5 rounds to 0 (even)
		},
		{
			name:     "round 3/2 half to even",
			code:     "(round 3/2)",
			expected: values.NewInteger(2), // 1.5 rounds to 2 (even)
		},
		// Truncate tests - floats and integers
		{
			name:     "truncate positive float",
			code:     "(truncate 3.7)",
			expected: values.NewFloat(3.0),
		},
		{
			name:     "truncate negative float",
			code:     "(truncate -3.7)",
			expected: values.NewFloat(-3.0),
		},
		{
			name:     "truncate integer passthrough",
			code:     "(truncate 3)",
			expected: values.NewInteger(3),
		},
		{
			name:     "truncate exact zero",
			code:     "(truncate 0)",
			expected: values.NewInteger(0),
		},
		{
			name:     "truncate inexact zero",
			code:     "(truncate 0.0)",
			expected: values.NewFloat(0.0),
		},
		{
			name:     "truncate large positive float",
			code:     "(truncate 999.999)",
			expected: values.NewFloat(999.0),
		},
		{
			name:     "truncate large negative float",
			code:     "(truncate -999.999)",
			expected: values.NewFloat(-999.0),
		},
		// Truncate tests - rationals (exact inputs → exact outputs per R7RS)
		{
			name:     "truncate 5/2 positive",
			code:     "(truncate 5/2)",
			expected: values.NewInteger(2),
		},
		{
			name:     "truncate -5/2 negative",
			code:     "(truncate -5/2)",
			expected: values.NewInteger(-2),
		},
		{
			name:     "truncate 7/3",
			code:     "(truncate 7/3)",
			expected: values.NewInteger(2),
		},
		{
			name:     "truncate -7/3 negative",
			code:     "(truncate -7/3)",
			expected: values.NewInteger(-2),
		},
		{
			name:     "truncate 1/2",
			code:     "(truncate 1/2)",
			expected: values.NewInteger(0),
		},
		{
			name:     "truncate -1/2 negative",
			code:     "(truncate -1/2)",
			expected: values.NewInteger(0),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tt.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tt.expected)
		})
	}
}

// TestRounding_TypeErrors tests that rounding functions reject non-numeric arguments.
// R7RS §6.2.6: floor, ceiling, round, truncate require numeric arguments.
func TestRounding_TypeErrors(t *testing.T) {
	t.Run("floor of string", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(floor "hello")`)
	})
	t.Run("floor of boolean", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(floor #t)`)
	})
	t.Run("ceiling of string", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(ceiling "hello")`)
	})
	t.Run("ceiling of boolean", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(ceiling #t)`)
	})
	t.Run("round of string", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(round "hello")`)
	})
	t.Run("round of boolean", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(round #t)`)
	})
	t.Run("truncate of string", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(truncate "hello")`)
	})
	t.Run("truncate of boolean", func(t *testing.T) {
		runSchemeCodeExpectError(t, `(truncate #t)`)
	})
}

// TestRounding_SpecialValues tests rounding functions with +inf.0, -inf.0, and +nan.0.
// R7RS §6.2.6: These functions return their argument for infinite and NaN inputs.
func TestRounding_SpecialValues(t *testing.T) {
	t.Run("floor of +inf.0", func(t *testing.T) {
		result, err := runSchemeCode(t, `(floor +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})
	t.Run("floor of -inf.0", func(t *testing.T) {
		result, err := runSchemeCode(t, `(floor -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), -1), qt.IsTrue)
	})
	t.Run("floor of +nan.0", func(t *testing.T) {
		result, err := runSchemeCode(t, `(floor +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})
	t.Run("ceiling of +inf.0", func(t *testing.T) {
		result, err := runSchemeCode(t, `(ceiling +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})
	t.Run("ceiling of -inf.0", func(t *testing.T) {
		result, err := runSchemeCode(t, `(ceiling -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), -1), qt.IsTrue)
	})
	t.Run("ceiling of +nan.0", func(t *testing.T) {
		result, err := runSchemeCode(t, `(ceiling +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})
	t.Run("round of +inf.0", func(t *testing.T) {
		result, err := runSchemeCode(t, `(round +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})
	t.Run("round of -inf.0", func(t *testing.T) {
		result, err := runSchemeCode(t, `(round -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), -1), qt.IsTrue)
	})
	t.Run("round of +nan.0", func(t *testing.T) {
		result, err := runSchemeCode(t, `(round +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})
	t.Run("truncate of +inf.0", func(t *testing.T) {
		result, err := runSchemeCode(t, `(truncate +inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), 1), qt.IsTrue)
	})
	t.Run("truncate of -inf.0", func(t *testing.T) {
		result, err := runSchemeCode(t, `(truncate -inf.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsInf(f.Datum(), -1), qt.IsTrue)
	})
	t.Run("truncate of +nan.0", func(t *testing.T) {
		result, err := runSchemeCode(t, `(truncate +nan.0)`)
		qt.Assert(t, err, qt.IsNil)
		f, ok := result.(*values.Float)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, math.IsNaN(f.Datum()), qt.IsTrue)
	})
}

// TestFloorQuotient_AllSigns tests floor-quotient with all sign combinations.
// R7RS §6.2.6: floor-quotient returns floor(n1/n2).
func TestFloorQuotient_AllSigns(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{name: "(10, 3) -> 3", code: `(floor-quotient 10 3)`, expected: values.NewInteger(3)},
		{name: "(-10, 3) -> -4", code: `(floor-quotient -10 3)`, expected: values.NewInteger(-4)},
		{name: "(10, -3) -> -4", code: `(floor-quotient 10 -3)`, expected: values.NewInteger(-4)},
		{name: "(-10, -3) -> 3", code: `(floor-quotient -10 -3)`, expected: values.NewInteger(3)},
		{name: "float input", code: `(floor-quotient 10.0 3)`, expected: values.NewFloat(3.0)},
		{name: "rational input", code: `(floor-quotient 7/2 3/2)`, expected: values.NewInteger(2)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// TestFloorRemainder_AllSigns tests floor-remainder with all sign combinations.
// R7RS §6.2.6: floor-remainder returns n1 - n2*floor(n1/n2).
func TestFloorRemainder_AllSigns(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{name: "(10, 3) -> 1", code: `(floor-remainder 10 3)`, expected: values.NewInteger(1)},
		{name: "(-10, 3) -> 2", code: `(floor-remainder -10 3)`, expected: values.NewInteger(2)},
		{name: "(10, -3) -> -2", code: `(floor-remainder 10 -3)`, expected: values.NewInteger(-2)},
		{name: "(-10, -3) -> -1", code: `(floor-remainder -10 -3)`, expected: values.NewInteger(-1)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// TestTruncateQuotient_AllSigns tests truncate-quotient with all sign combinations.
// R7RS §6.2.6: truncate-quotient returns truncate(n1/n2).
func TestTruncateQuotient_AllSigns(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{name: "(10, 3) -> 3", code: `(truncate-quotient 10 3)`, expected: values.NewInteger(3)},
		{name: "(-10, 3) -> -3", code: `(truncate-quotient -10 3)`, expected: values.NewInteger(-3)},
		{name: "(10, -3) -> -3", code: `(truncate-quotient 10 -3)`, expected: values.NewInteger(-3)},
		{name: "(-10, -3) -> 3", code: `(truncate-quotient -10 -3)`, expected: values.NewInteger(3)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// TestTruncateRemainder_AllSigns tests truncate-remainder with all sign combinations.
// R7RS §6.2.6: truncate-remainder returns n1 - n2*truncate(n1/n2).
func TestTruncateRemainder_AllSigns(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{name: "(10, 3) -> 1", code: `(truncate-remainder 10 3)`, expected: values.NewInteger(1)},
		{name: "(-10, 3) -> -1", code: `(truncate-remainder -10 3)`, expected: values.NewInteger(-1)},
		{name: "(10, -3) -> 1", code: `(truncate-remainder 10 -3)`, expected: values.NewInteger(1)},
		{name: "(-10, -3) -> -1", code: `(truncate-remainder -10 -3)`, expected: values.NewInteger(-1)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// TestFloorTruncateMultiValue tests floor/ and truncate/ multi-value returns.
// R7RS §6.2.6: These return two values (quotient and remainder).
func TestFloorTruncateMultiValue(t *testing.T) {
	// floor/ returns (quotient remainder)
	t.Run("floor/ 10 3", func(t *testing.T) {
		runSchemeCodeExpectTrue(t, "(equal? (call-with-values (lambda () (floor/ 10 3)) list) '(3 1))")
	})
	t.Run("floor/ -10 3", func(t *testing.T) {
		runSchemeCodeExpectTrue(t, "(equal? (call-with-values (lambda () (floor/ -10 3)) list) '(-4 2))")
	})
	// truncate/ returns (quotient remainder)
	t.Run("truncate/ 10 3", func(t *testing.T) {
		runSchemeCodeExpectTrue(t, "(equal? (call-with-values (lambda () (truncate/ 10 3)) list) '(3 1))")
	})
	t.Run("truncate/ -10 3", func(t *testing.T) {
		runSchemeCodeExpectTrue(t, "(equal? (call-with-values (lambda () (truncate/ -10 3)) list) '(-3 -1))")
	})
}

// TestFloorTruncateInvariant tests the R7RS invariant: x = q*d + r.
func TestFloorTruncateInvariant(t *testing.T) {
	// (= x (+ (* (floor-quotient x d) d) (floor-remainder x d)))
	t.Run("floor invariant positive", func(t *testing.T) {
		runSchemeCodeExpectTrue(t, "(= 10 (+ (* (floor-quotient 10 3) 3) (floor-remainder 10 3)))")
	})
	t.Run("floor invariant negative dividend", func(t *testing.T) {
		runSchemeCodeExpectTrue(t, "(= -10 (+ (* (floor-quotient -10 3) 3) (floor-remainder -10 3)))")
	})
	// (= x (+ (* (truncate-quotient x d) d) (truncate-remainder x d)))
	t.Run("truncate invariant positive", func(t *testing.T) {
		runSchemeCodeExpectTrue(t, "(= 10 (+ (* (truncate-quotient 10 3) 3) (truncate-remainder 10 3)))")
	})
	t.Run("truncate invariant negative dividend", func(t *testing.T) {
		runSchemeCodeExpectTrue(t, "(= -10 (+ (* (truncate-quotient -10 3) 3) (truncate-remainder -10 3)))")
	})
}

// TestFloorTruncateDivisionByZero tests division by zero produces an error.
func TestFloorTruncateDivisionByZero(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "floor-quotient by zero", code: `(floor-quotient 10 0)`},
		{name: "floor-remainder by zero", code: `(floor-remainder 10 0)`},
		{name: "truncate-quotient by zero", code: `(truncate-quotient 10 0)`},
		{name: "truncate-remainder by zero", code: `(truncate-remainder 10 0)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
