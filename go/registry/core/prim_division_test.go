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
	"testing"

	qt "github.com/frankban/quicktest"

	"wile/values"
)

// ============================================================================
// Phase 7: Division Operations Tests
// ============================================================================
//
// This file contains comprehensive tests for R7RS division primitives:
// - floor/, floor-quotient, floor-remainder
// - truncate/, truncate-quotient, truncate-remainder
//
// R7RS §6.2.6 specifies:
// - truncate-quotient: rounds quotient toward zero
// - truncate-remainder: has the sign of the dividend (first argument)
// - floor-quotient: rounds quotient toward negative infinity
// - floor-remainder: has the sign of the divisor (second argument)
//
// The key invariant for both:
//   n = d * q + r
// where n is dividend, d is divisor, q is quotient, r is remainder.
//
// ============================================================================

// ----------------------------------------------------------------------------
// floor/ Tests
// ----------------------------------------------------------------------------

func TestFloorDivComprehensive(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected values.Value
	}{
		// Basic positive cases
		{
			name:     "positive / positive exact",
			code:     `(call-with-values (lambda () (floor/ 12 4)) list)`,
			expected: values.List(values.NewInteger(3), values.NewInteger(0)),
		},
		{
			name:     "positive / positive with remainder",
			code:     `(call-with-values (lambda () (floor/ 10 3)) list)`,
			expected: values.List(values.NewInteger(3), values.NewInteger(1)),
		},
		{
			name:     "positive / positive small dividend",
			code:     `(call-with-values (lambda () (floor/ 1 5)) list)`,
			expected: values.List(values.NewInteger(0), values.NewInteger(1)),
		},

		// Negative dividend cases
		{
			name:     "negative / positive",
			code:     `(call-with-values (lambda () (floor/ -10 3)) list)`,
			expected: values.List(values.NewInteger(-4), values.NewInteger(2)),
		},
		{
			name:     "negative / positive exact",
			code:     `(call-with-values (lambda () (floor/ -12 4)) list)`,
			expected: values.List(values.NewInteger(-3), values.NewInteger(0)),
		},
		{
			name:     "negative / positive small",
			code:     `(call-with-values (lambda () (floor/ -1 5)) list)`,
			expected: values.List(values.NewInteger(-1), values.NewInteger(4)),
		},

		// Negative divisor cases
		{
			name:     "positive / negative",
			code:     `(call-with-values (lambda () (floor/ 10 -3)) list)`,
			expected: values.List(values.NewInteger(-4), values.NewInteger(-2)),
		},
		{
			name:     "positive / negative exact",
			code:     `(call-with-values (lambda () (floor/ 12 -4)) list)`,
			expected: values.List(values.NewInteger(-3), values.NewInteger(0)),
		},

		// Both negative cases
		{
			name:     "negative / negative",
			code:     `(call-with-values (lambda () (floor/ -10 -3)) list)`,
			expected: values.List(values.NewInteger(3), values.NewInteger(-1)),
		},
		{
			name:     "negative / negative exact",
			code:     `(call-with-values (lambda () (floor/ -12 -4)) list)`,
			expected: values.List(values.NewInteger(3), values.NewInteger(0)),
		},

		// Zero dividend
		{
			name:     "zero / positive",
			code:     `(call-with-values (lambda () (floor/ 0 5)) list)`,
			expected: values.List(values.NewInteger(0), values.NewInteger(0)),
		},
		{
			name:     "zero / negative",
			code:     `(call-with-values (lambda () (floor/ 0 -5)) list)`,
			expected: values.List(values.NewInteger(0), values.NewInteger(0)),
		},

		// Large numbers
		{
			name:     "large positive values",
			code:     `(call-with-values (lambda () (floor/ 1000000 7)) list)`,
			expected: values.List(values.NewInteger(142857), values.NewInteger(1)),
		},
		{
			name:     "large negative dividend",
			code:     `(call-with-values (lambda () (floor/ -1000000 7)) list)`,
			expected: values.List(values.NewInteger(-142858), values.NewInteger(6)),
		},

		// Edge case: divisor is 1 or -1
		{
			name:     "divide by 1",
			code:     `(call-with-values (lambda () (floor/ 42 1)) list)`,
			expected: values.List(values.NewInteger(42), values.NewInteger(0)),
		},
		{
			name:     "divide by -1",
			code:     `(call-with-values (lambda () (floor/ 42 -1)) list)`,
			expected: values.List(values.NewInteger(-42), values.NewInteger(0)),
		},
		{
			name:     "negative divide by -1",
			code:     `(call-with-values (lambda () (floor/ -42 -1)) list)`,
			expected: values.List(values.NewInteger(42), values.NewInteger(0)),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// TestFloorDivIdentity verifies the mathematical identity: n = d * q + r
func TestFloorDivIdentity(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"10/3", `(let-values (((q r) (floor/ 10 3))) (= 10 (+ (* 3 q) r)))`},
		{"-10/3", `(let-values (((q r) (floor/ -10 3))) (= -10 (+ (* 3 q) r)))`},
		{"10/-3", `(let-values (((q r) (floor/ 10 -3))) (= 10 (+ (* -3 q) r)))`},
		{"-10/-3", `(let-values (((q r) (floor/ -10 -3))) (= -10 (+ (* -3 q) r)))`},
		{"0/5", `(let-values (((q r) (floor/ 0 5))) (= 0 (+ (* 5 q) r)))`},
		{"100/7", `(let-values (((q r) (floor/ 100 7))) (= 100 (+ (* 7 q) r)))`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, values.TrueValue)
		})
	}
}

// TestFloorDivRemainderSign verifies that remainder has the sign of the divisor
func TestFloorDivRemainderSign(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		// Positive divisor -> remainder >= 0
		{"pos divisor, pos rem", `(>= (floor-remainder 10 3) 0)`},
		{"pos divisor, neg dividend", `(>= (floor-remainder -10 3) 0)`},
		// Negative divisor -> remainder <= 0
		{"neg divisor, pos dividend", `(<= (floor-remainder 10 -3) 0)`},
		{"neg divisor, neg dividend", `(<= (floor-remainder -10 -3) 0)`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, values.TrueValue)
		})
	}
}

// ----------------------------------------------------------------------------
// floor-quotient Tests
// ----------------------------------------------------------------------------

func TestFloorQuotientComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic cases
		{name: "positive / positive exact", code: `(floor-quotient 12 4)`, expected: values.NewInteger(3)},
		{name: "positive / positive with remainder", code: `(floor-quotient 10 3)`, expected: values.NewInteger(3)},
		{name: "positive / positive small", code: `(floor-quotient 1 5)`, expected: values.NewInteger(0)},

		// Negative dividend
		{name: "negative / positive", code: `(floor-quotient -10 3)`, expected: values.NewInteger(-4)},
		{name: "negative / positive exact", code: `(floor-quotient -12 4)`, expected: values.NewInteger(-3)},

		// Negative divisor
		{name: "positive / negative", code: `(floor-quotient 10 -3)`, expected: values.NewInteger(-4)},
		{name: "positive / negative exact", code: `(floor-quotient 12 -4)`, expected: values.NewInteger(-3)},

		// Both negative
		{name: "negative / negative", code: `(floor-quotient -10 -3)`, expected: values.NewInteger(3)},
		{name: "negative / negative exact", code: `(floor-quotient -12 -4)`, expected: values.NewInteger(3)},

		// Zero dividend
		{name: "zero / positive", code: `(floor-quotient 0 5)`, expected: values.NewInteger(0)},
		{name: "zero / negative", code: `(floor-quotient 0 -5)`, expected: values.NewInteger(0)},

		// Large numbers
		{name: "large values", code: `(floor-quotient 1000000 7)`, expected: values.NewInteger(142857)},
		{name: "large negative dividend", code: `(floor-quotient -1000000 7)`, expected: values.NewInteger(-142858)},

		// Divide by 1 or -1
		{name: "divide by 1", code: `(floor-quotient 42 1)`, expected: values.NewInteger(42)},
		{name: "divide by -1", code: `(floor-quotient 42 -1)`, expected: values.NewInteger(-42)},
		{name: "negative divide by -1", code: `(floor-quotient -42 -1)`, expected: values.NewInteger(42)},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// ----------------------------------------------------------------------------
// floor-remainder Tests
// ----------------------------------------------------------------------------

func TestFloorRemainderComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic cases
		{name: "positive / positive exact", code: `(floor-remainder 12 4)`, expected: values.NewInteger(0)},
		{name: "positive / positive with remainder", code: `(floor-remainder 10 3)`, expected: values.NewInteger(1)},
		{name: "positive / positive small", code: `(floor-remainder 1 5)`, expected: values.NewInteger(1)},

		// Negative dividend
		{name: "negative / positive", code: `(floor-remainder -10 3)`, expected: values.NewInteger(2)},
		{name: "negative / positive exact", code: `(floor-remainder -12 4)`, expected: values.NewInteger(0)},

		// Negative divisor
		{name: "positive / negative", code: `(floor-remainder 10 -3)`, expected: values.NewInteger(-2)},
		{name: "positive / negative exact", code: `(floor-remainder 12 -4)`, expected: values.NewInteger(0)},

		// Both negative
		{name: "negative / negative", code: `(floor-remainder -10 -3)`, expected: values.NewInteger(-1)},
		{name: "negative / negative exact", code: `(floor-remainder -12 -4)`, expected: values.NewInteger(0)},

		// Zero dividend
		{name: "zero / positive", code: `(floor-remainder 0 5)`, expected: values.NewInteger(0)},
		{name: "zero / negative", code: `(floor-remainder 0 -5)`, expected: values.NewInteger(0)},

		// Large numbers
		{name: "large values", code: `(floor-remainder 1000000 7)`, expected: values.NewInteger(1)},
		{name: "large negative dividend", code: `(floor-remainder -1000000 7)`, expected: values.NewInteger(6)},

		// Divide by 1 or -1
		{name: "divide by 1", code: `(floor-remainder 42 1)`, expected: values.NewInteger(0)},
		{name: "divide by -1", code: `(floor-remainder 42 -1)`, expected: values.NewInteger(0)},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// ----------------------------------------------------------------------------
// truncate/ Tests
// ----------------------------------------------------------------------------

func TestTruncateDivComprehensive(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected values.Value
	}{
		// Basic positive cases
		{
			name:     "positive / positive exact",
			code:     `(call-with-values (lambda () (truncate/ 12 4)) list)`,
			expected: values.List(values.NewInteger(3), values.NewInteger(0)),
		},
		{
			name:     "positive / positive with remainder",
			code:     `(call-with-values (lambda () (truncate/ 10 3)) list)`,
			expected: values.List(values.NewInteger(3), values.NewInteger(1)),
		},
		{
			name:     "positive / positive small dividend",
			code:     `(call-with-values (lambda () (truncate/ 1 5)) list)`,
			expected: values.List(values.NewInteger(0), values.NewInteger(1)),
		},

		// Negative dividend cases
		{
			name:     "negative / positive",
			code:     `(call-with-values (lambda () (truncate/ -10 3)) list)`,
			expected: values.List(values.NewInteger(-3), values.NewInteger(-1)),
		},
		{
			name:     "negative / positive exact",
			code:     `(call-with-values (lambda () (truncate/ -12 4)) list)`,
			expected: values.List(values.NewInteger(-3), values.NewInteger(0)),
		},
		{
			name:     "negative / positive small",
			code:     `(call-with-values (lambda () (truncate/ -1 5)) list)`,
			expected: values.List(values.NewInteger(0), values.NewInteger(-1)),
		},

		// Negative divisor cases
		{
			name:     "positive / negative",
			code:     `(call-with-values (lambda () (truncate/ 10 -3)) list)`,
			expected: values.List(values.NewInteger(-3), values.NewInteger(1)),
		},
		{
			name:     "positive / negative exact",
			code:     `(call-with-values (lambda () (truncate/ 12 -4)) list)`,
			expected: values.List(values.NewInteger(-3), values.NewInteger(0)),
		},

		// Both negative cases
		{
			name:     "negative / negative",
			code:     `(call-with-values (lambda () (truncate/ -10 -3)) list)`,
			expected: values.List(values.NewInteger(3), values.NewInteger(-1)),
		},
		{
			name:     "negative / negative exact",
			code:     `(call-with-values (lambda () (truncate/ -12 -4)) list)`,
			expected: values.List(values.NewInteger(3), values.NewInteger(0)),
		},

		// Zero dividend
		{
			name:     "zero / positive",
			code:     `(call-with-values (lambda () (truncate/ 0 5)) list)`,
			expected: values.List(values.NewInteger(0), values.NewInteger(0)),
		},
		{
			name:     "zero / negative",
			code:     `(call-with-values (lambda () (truncate/ 0 -5)) list)`,
			expected: values.List(values.NewInteger(0), values.NewInteger(0)),
		},

		// Large numbers
		{
			name:     "large positive values",
			code:     `(call-with-values (lambda () (truncate/ 1000000 7)) list)`,
			expected: values.List(values.NewInteger(142857), values.NewInteger(1)),
		},
		{
			name:     "large negative dividend",
			code:     `(call-with-values (lambda () (truncate/ -1000000 7)) list)`,
			expected: values.List(values.NewInteger(-142857), values.NewInteger(-1)),
		},

		// Edge case: divisor is 1 or -1
		{
			name:     "divide by 1",
			code:     `(call-with-values (lambda () (truncate/ 42 1)) list)`,
			expected: values.List(values.NewInteger(42), values.NewInteger(0)),
		},
		{
			name:     "divide by -1",
			code:     `(call-with-values (lambda () (truncate/ 42 -1)) list)`,
			expected: values.List(values.NewInteger(-42), values.NewInteger(0)),
		},
		{
			name:     "negative divide by -1",
			code:     `(call-with-values (lambda () (truncate/ -42 -1)) list)`,
			expected: values.List(values.NewInteger(42), values.NewInteger(0)),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// TestTruncateDivIdentity verifies the mathematical identity: n = d * q + r
func TestTruncateDivIdentity(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"10/3", `(let-values (((q r) (truncate/ 10 3))) (= 10 (+ (* 3 q) r)))`},
		{"-10/3", `(let-values (((q r) (truncate/ -10 3))) (= -10 (+ (* 3 q) r)))`},
		{"10/-3", `(let-values (((q r) (truncate/ 10 -3))) (= 10 (+ (* -3 q) r)))`},
		{"-10/-3", `(let-values (((q r) (truncate/ -10 -3))) (= -10 (+ (* -3 q) r)))`},
		{"0/5", `(let-values (((q r) (truncate/ 0 5))) (= 0 (+ (* 5 q) r)))`},
		{"100/7", `(let-values (((q r) (truncate/ 100 7))) (= 100 (+ (* 7 q) r)))`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, values.TrueValue)
		})
	}
}

// TestTruncateDivRemainderSign verifies that remainder has the sign of the dividend
func TestTruncateDivRemainderSign(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		// Positive dividend -> remainder >= 0
		{"pos dividend, pos divisor", `(>= (truncate-remainder 10 3) 0)`},
		{"pos dividend, neg divisor", `(>= (truncate-remainder 10 -3) 0)`},
		// Negative dividend -> remainder <= 0
		{"neg dividend, pos divisor", `(<= (truncate-remainder -10 3) 0)`},
		{"neg dividend, neg divisor", `(<= (truncate-remainder -10 -3) 0)`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, values.TrueValue)
		})
	}
}

// ----------------------------------------------------------------------------
// truncate-quotient Tests
// ----------------------------------------------------------------------------

func TestTruncateQuotientComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic cases
		{name: "positive / positive exact", code: `(truncate-quotient 12 4)`, expected: values.NewInteger(3)},
		{name: "positive / positive with remainder", code: `(truncate-quotient 10 3)`, expected: values.NewInteger(3)},
		{name: "positive / positive small", code: `(truncate-quotient 1 5)`, expected: values.NewInteger(0)},

		// Negative dividend
		{name: "negative / positive", code: `(truncate-quotient -10 3)`, expected: values.NewInteger(-3)},
		{name: "negative / positive exact", code: `(truncate-quotient -12 4)`, expected: values.NewInteger(-3)},

		// Negative divisor
		{name: "positive / negative", code: `(truncate-quotient 10 -3)`, expected: values.NewInteger(-3)},
		{name: "positive / negative exact", code: `(truncate-quotient 12 -4)`, expected: values.NewInteger(-3)},

		// Both negative
		{name: "negative / negative", code: `(truncate-quotient -10 -3)`, expected: values.NewInteger(3)},
		{name: "negative / negative exact", code: `(truncate-quotient -12 -4)`, expected: values.NewInteger(3)},

		// Zero dividend
		{name: "zero / positive", code: `(truncate-quotient 0 5)`, expected: values.NewInteger(0)},
		{name: "zero / negative", code: `(truncate-quotient 0 -5)`, expected: values.NewInteger(0)},

		// Large numbers
		{name: "large values", code: `(truncate-quotient 1000000 7)`, expected: values.NewInteger(142857)},
		{name: "large negative dividend", code: `(truncate-quotient -1000000 7)`, expected: values.NewInteger(-142857)},

		// Divide by 1 or -1
		{name: "divide by 1", code: `(truncate-quotient 42 1)`, expected: values.NewInteger(42)},
		{name: "divide by -1", code: `(truncate-quotient 42 -1)`, expected: values.NewInteger(-42)},
		{name: "negative divide by -1", code: `(truncate-quotient -42 -1)`, expected: values.NewInteger(42)},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// ----------------------------------------------------------------------------
// truncate-remainder Tests
// ----------------------------------------------------------------------------

func TestTruncateRemainderComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic cases
		{name: "positive / positive exact", code: `(truncate-remainder 12 4)`, expected: values.NewInteger(0)},
		{name: "positive / positive with remainder", code: `(truncate-remainder 10 3)`, expected: values.NewInteger(1)},
		{name: "positive / positive small", code: `(truncate-remainder 1 5)`, expected: values.NewInteger(1)},

		// Negative dividend
		{name: "negative / positive", code: `(truncate-remainder -10 3)`, expected: values.NewInteger(-1)},
		{name: "negative / positive exact", code: `(truncate-remainder -12 4)`, expected: values.NewInteger(0)},

		// Negative divisor
		{name: "positive / negative", code: `(truncate-remainder 10 -3)`, expected: values.NewInteger(1)},
		{name: "positive / negative exact", code: `(truncate-remainder 12 -4)`, expected: values.NewInteger(0)},

		// Both negative
		{name: "negative / negative", code: `(truncate-remainder -10 -3)`, expected: values.NewInteger(-1)},
		{name: "negative / negative exact", code: `(truncate-remainder -12 -4)`, expected: values.NewInteger(0)},

		// Zero dividend
		{name: "zero / positive", code: `(truncate-remainder 0 5)`, expected: values.NewInteger(0)},
		{name: "zero / negative", code: `(truncate-remainder 0 -5)`, expected: values.NewInteger(0)},

		// Large numbers
		{name: "large values", code: `(truncate-remainder 1000000 7)`, expected: values.NewInteger(1)},
		{name: "large negative dividend", code: `(truncate-remainder -1000000 7)`, expected: values.NewInteger(-1)},

		// Divide by 1 or -1
		{name: "divide by 1", code: `(truncate-remainder 42 1)`, expected: values.NewInteger(0)},
		{name: "divide by -1", code: `(truncate-remainder 42 -1)`, expected: values.NewInteger(0)},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// ----------------------------------------------------------------------------
// Comparison: floor vs truncate
// ----------------------------------------------------------------------------

// TestFloorVsTruncateDifference shows cases where floor and truncate differ
func TestFloorVsTruncateDifference(t *testing.T) {
	tcs := []struct {
		name         string
		floorCode    string
		truncateCode string
		floorResult  values.Value
		truncResult  values.Value
		shouldDiffer bool
	}{
		{
			name:         "positive/positive - same",
			floorCode:    `(floor-quotient 10 3)`,
			truncateCode: `(truncate-quotient 10 3)`,
			floorResult:  values.NewInteger(3),
			truncResult:  values.NewInteger(3),
			shouldDiffer: false,
		},
		{
			name:         "negative/positive - different",
			floorCode:    `(floor-quotient -10 3)`,
			truncateCode: `(truncate-quotient -10 3)`,
			floorResult:  values.NewInteger(-4),
			truncResult:  values.NewInteger(-3),
			shouldDiffer: true,
		},
		{
			name:         "positive/negative - different",
			floorCode:    `(floor-quotient 10 -3)`,
			truncateCode: `(truncate-quotient 10 -3)`,
			floorResult:  values.NewInteger(-4),
			truncResult:  values.NewInteger(-3),
			shouldDiffer: true,
		},
		{
			name:         "negative/negative - same",
			floorCode:    `(floor-quotient -10 -3)`,
			truncateCode: `(truncate-quotient -10 -3)`,
			floorResult:  values.NewInteger(3),
			truncResult:  values.NewInteger(3),
			shouldDiffer: false,
		},
		{
			name:         "exact division - same",
			floorCode:    `(floor-quotient -12 4)`,
			truncateCode: `(truncate-quotient -12 4)`,
			floorResult:  values.NewInteger(-3),
			truncResult:  values.NewInteger(-3),
			shouldDiffer: false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			floorRes, err := runSchemeCode(t, tc.floorCode)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, floorRes, values.SchemeEquals, tc.floorResult)

			truncRes, err := runSchemeCode(t, tc.truncateCode)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, truncRes, values.SchemeEquals, tc.truncResult)

			if tc.shouldDiffer {
				qt.Assert(t, floorRes, qt.Not(values.SchemeEquals), truncRes)
			} else {
				qt.Assert(t, floorRes, values.SchemeEquals, truncRes)
			}
		})
	}
}

// ----------------------------------------------------------------------------
// Error Tests
// ----------------------------------------------------------------------------

func TestDivisionErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		// Division by zero
		{name: "floor/ divide by zero", code: `(floor/ 10 0)`},
		{name: "floor-quotient divide by zero", code: `(floor-quotient 10 0)`},
		{name: "floor-remainder divide by zero", code: `(floor-remainder 10 0)`},
		{name: "truncate/ divide by zero", code: `(truncate/ 10 0)`},
		{name: "truncate-quotient divide by zero", code: `(truncate-quotient 10 0)`},
		{name: "truncate-remainder divide by zero", code: `(truncate-remainder 10 0)`},

		// Wrong type - first argument (strings, symbols, lists are invalid; floats/rationals are valid per R7RS)
		{name: "floor/ string dividend", code: `(floor/ "10" 3)`},
		{name: "floor/ symbol dividend", code: `(floor/ 'ten 3)`},
		{name: "floor/ list dividend", code: `(floor/ '(10) 3)`},

		{name: "truncate/ string dividend", code: `(truncate/ "10" 3)`},
		{name: "truncate/ symbol dividend", code: `(truncate/ 'ten 3)`},
		{name: "truncate/ list dividend", code: `(truncate/ '(10) 3)`},

		// Wrong type - second argument
		{name: "floor/ string divisor", code: `(floor/ 10 "3")`},
		{name: "floor/ symbol divisor", code: `(floor/ 10 'three)`},
		{name: "floor/ list divisor", code: `(floor/ 10 '(3))`},

		{name: "truncate/ string divisor", code: `(truncate/ 10 "3")`},
		{name: "truncate/ symbol divisor", code: `(truncate/ 10 'three)`},
		{name: "truncate/ list divisor", code: `(truncate/ 10 '(3))`},

		// Both arguments wrong type
		{name: "floor/ both strings", code: `(floor/ "10" "3")`},
		{name: "truncate/ both strings", code: `(truncate/ "10" "3")`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// ----------------------------------------------------------------------------
// modulo and remainder compatibility tests
// ----------------------------------------------------------------------------

// TestModuloEqualsFloorRemainder verifies that modulo gives same results as floor-remainder
func TestModuloEqualsFloorRemainder(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"10/3", `(= (modulo 10 3) (floor-remainder 10 3))`},
		{"-10/3", `(= (modulo -10 3) (floor-remainder -10 3))`},
		{"10/-3", `(= (modulo 10 -3) (floor-remainder 10 -3))`},
		{"-10/-3", `(= (modulo -10 -3) (floor-remainder -10 -3))`},
		{"0/5", `(= (modulo 0 5) (floor-remainder 0 5))`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, values.TrueValue)
		})
	}
}

// TestRemainderEqualsTruncateRemainder verifies that remainder gives same results as truncate-remainder
func TestRemainderEqualsTruncateRemainder(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"10/3", `(= (remainder 10 3) (truncate-remainder 10 3))`},
		{"-10/3", `(= (remainder -10 3) (truncate-remainder -10 3))`},
		{"10/-3", `(= (remainder 10 -3) (truncate-remainder 10 -3))`},
		{"-10/-3", `(= (remainder -10 -3) (truncate-remainder -10 -3))`},
		{"0/5", `(= (remainder 0 5) (truncate-remainder 0 5))`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, values.TrueValue)
		})
	}
}

// TestQuotientEqualsTruncateQuotient verifies that quotient gives same results as truncate-quotient
func TestQuotientEqualsTruncateQuotient(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"10/3", `(= (quotient 10 3) (truncate-quotient 10 3))`},
		{"-10/3", `(= (quotient -10 3) (truncate-quotient -10 3))`},
		{"10/-3", `(= (quotient 10 -3) (truncate-quotient 10 -3))`},
		{"-10/-3", `(= (quotient -10 -3) (truncate-quotient -10 -3))`},
		{"0/5", `(= (quotient 0 5) (truncate-quotient 0 5))`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, values.TrueValue)
		})
	}
}
