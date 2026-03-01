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

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, values.TrueValue)
		})
	}
}

// ----------------------------------------------------------------------------
// floor-quotient Tests
// ----------------------------------------------------------------------------

func TestFloorQuotientComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic cases
		{Name: "positive / positive exact", Code: `(floor-quotient 12 4)`, Expected: values.NewInteger(3)},
		{Name: "positive / positive with remainder", Code: `(floor-quotient 10 3)`, Expected: values.NewInteger(3)},
		{Name: "positive / positive small", Code: `(floor-quotient 1 5)`, Expected: values.NewInteger(0)},

		// Negative dividend
		{Name: "negative / positive", Code: `(floor-quotient -10 3)`, Expected: values.NewInteger(-4)},
		{Name: "negative / positive exact", Code: `(floor-quotient -12 4)`, Expected: values.NewInteger(-3)},

		// Negative divisor
		{Name: "positive / negative", Code: `(floor-quotient 10 -3)`, Expected: values.NewInteger(-4)},
		{Name: "positive / negative exact", Code: `(floor-quotient 12 -4)`, Expected: values.NewInteger(-3)},

		// Both negative
		{Name: "negative / negative", Code: `(floor-quotient -10 -3)`, Expected: values.NewInteger(3)},
		{Name: "negative / negative exact", Code: `(floor-quotient -12 -4)`, Expected: values.NewInteger(3)},

		// Zero dividend
		{Name: "zero / positive", Code: `(floor-quotient 0 5)`, Expected: values.NewInteger(0)},
		{Name: "zero / negative", Code: `(floor-quotient 0 -5)`, Expected: values.NewInteger(0)},

		// Large numbers
		{Name: "large values", Code: `(floor-quotient 1000000 7)`, Expected: values.NewInteger(142857)},
		{Name: "large negative dividend", Code: `(floor-quotient -1000000 7)`, Expected: values.NewInteger(-142858)},

		// Divide by 1 or -1
		{Name: "divide by 1", Code: `(floor-quotient 42 1)`, Expected: values.NewInteger(42)},
		{Name: "divide by -1", Code: `(floor-quotient 42 -1)`, Expected: values.NewInteger(-42)},
		{Name: "negative divide by -1", Code: `(floor-quotient -42 -1)`, Expected: values.NewInteger(42)},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// ----------------------------------------------------------------------------
// floor-remainder Tests
// ----------------------------------------------------------------------------

func TestFloorRemainderComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic cases
		{Name: "positive / positive exact", Code: `(floor-remainder 12 4)`, Expected: values.NewInteger(0)},
		{Name: "positive / positive with remainder", Code: `(floor-remainder 10 3)`, Expected: values.NewInteger(1)},
		{Name: "positive / positive small", Code: `(floor-remainder 1 5)`, Expected: values.NewInteger(1)},

		// Negative dividend
		{Name: "negative / positive", Code: `(floor-remainder -10 3)`, Expected: values.NewInteger(2)},
		{Name: "negative / positive exact", Code: `(floor-remainder -12 4)`, Expected: values.NewInteger(0)},

		// Negative divisor
		{Name: "positive / negative", Code: `(floor-remainder 10 -3)`, Expected: values.NewInteger(-2)},
		{Name: "positive / negative exact", Code: `(floor-remainder 12 -4)`, Expected: values.NewInteger(0)},

		// Both negative
		{Name: "negative / negative", Code: `(floor-remainder -10 -3)`, Expected: values.NewInteger(-1)},
		{Name: "negative / negative exact", Code: `(floor-remainder -12 -4)`, Expected: values.NewInteger(0)},

		// Zero dividend
		{Name: "zero / positive", Code: `(floor-remainder 0 5)`, Expected: values.NewInteger(0)},
		{Name: "zero / negative", Code: `(floor-remainder 0 -5)`, Expected: values.NewInteger(0)},

		// Large numbers
		{Name: "large values", Code: `(floor-remainder 1000000 7)`, Expected: values.NewInteger(1)},
		{Name: "large negative dividend", Code: `(floor-remainder -1000000 7)`, Expected: values.NewInteger(6)},

		// Divide by 1 or -1
		{Name: "divide by 1", Code: `(floor-remainder 42 1)`, Expected: values.NewInteger(0)},
		{Name: "divide by -1", Code: `(floor-remainder 42 -1)`, Expected: values.NewInteger(0)},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, values.TrueValue)
		})
	}
}

// ----------------------------------------------------------------------------
// truncate-quotient Tests
// ----------------------------------------------------------------------------

func TestTruncateQuotientComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic cases
		{Name: "positive / positive exact", Code: `(truncate-quotient 12 4)`, Expected: values.NewInteger(3)},
		{Name: "positive / positive with remainder", Code: `(truncate-quotient 10 3)`, Expected: values.NewInteger(3)},
		{Name: "positive / positive small", Code: `(truncate-quotient 1 5)`, Expected: values.NewInteger(0)},

		// Negative dividend
		{Name: "negative / positive", Code: `(truncate-quotient -10 3)`, Expected: values.NewInteger(-3)},
		{Name: "negative / positive exact", Code: `(truncate-quotient -12 4)`, Expected: values.NewInteger(-3)},

		// Negative divisor
		{Name: "positive / negative", Code: `(truncate-quotient 10 -3)`, Expected: values.NewInteger(-3)},
		{Name: "positive / negative exact", Code: `(truncate-quotient 12 -4)`, Expected: values.NewInteger(-3)},

		// Both negative
		{Name: "negative / negative", Code: `(truncate-quotient -10 -3)`, Expected: values.NewInteger(3)},
		{Name: "negative / negative exact", Code: `(truncate-quotient -12 -4)`, Expected: values.NewInteger(3)},

		// Zero dividend
		{Name: "zero / positive", Code: `(truncate-quotient 0 5)`, Expected: values.NewInteger(0)},
		{Name: "zero / negative", Code: `(truncate-quotient 0 -5)`, Expected: values.NewInteger(0)},

		// Large numbers
		{Name: "large values", Code: `(truncate-quotient 1000000 7)`, Expected: values.NewInteger(142857)},
		{Name: "large negative dividend", Code: `(truncate-quotient -1000000 7)`, Expected: values.NewInteger(-142857)},

		// Divide by 1 or -1
		{Name: "divide by 1", Code: `(truncate-quotient 42 1)`, Expected: values.NewInteger(42)},
		{Name: "divide by -1", Code: `(truncate-quotient 42 -1)`, Expected: values.NewInteger(-42)},
		{Name: "negative divide by -1", Code: `(truncate-quotient -42 -1)`, Expected: values.NewInteger(42)},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// ----------------------------------------------------------------------------
// truncate-remainder Tests
// ----------------------------------------------------------------------------

func TestTruncateRemainderComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic cases
		{Name: "positive / positive exact", Code: `(truncate-remainder 12 4)`, Expected: values.NewInteger(0)},
		{Name: "positive / positive with remainder", Code: `(truncate-remainder 10 3)`, Expected: values.NewInteger(1)},
		{Name: "positive / positive small", Code: `(truncate-remainder 1 5)`, Expected: values.NewInteger(1)},

		// Negative dividend
		{Name: "negative / positive", Code: `(truncate-remainder -10 3)`, Expected: values.NewInteger(-1)},
		{Name: "negative / positive exact", Code: `(truncate-remainder -12 4)`, Expected: values.NewInteger(0)},

		// Negative divisor
		{Name: "positive / negative", Code: `(truncate-remainder 10 -3)`, Expected: values.NewInteger(1)},
		{Name: "positive / negative exact", Code: `(truncate-remainder 12 -4)`, Expected: values.NewInteger(0)},

		// Both negative
		{Name: "negative / negative", Code: `(truncate-remainder -10 -3)`, Expected: values.NewInteger(-1)},
		{Name: "negative / negative exact", Code: `(truncate-remainder -12 -4)`, Expected: values.NewInteger(0)},

		// Zero dividend
		{Name: "zero / positive", Code: `(truncate-remainder 0 5)`, Expected: values.NewInteger(0)},
		{Name: "zero / negative", Code: `(truncate-remainder 0 -5)`, Expected: values.NewInteger(0)},

		// Large numbers
		{Name: "large values", Code: `(truncate-remainder 1000000 7)`, Expected: values.NewInteger(1)},
		{Name: "large negative dividend", Code: `(truncate-remainder -1000000 7)`, Expected: values.NewInteger(-1)},

		// Divide by 1 or -1
		{Name: "divide by 1", Code: `(truncate-remainder 42 1)`, Expected: values.NewInteger(0)},
		{Name: "divide by -1", Code: `(truncate-remainder 42 -1)`, Expected: values.NewInteger(0)},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
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
			floorRes, err := testhelpers.RunSchemeCode(t, tc.floorCode)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, floorRes, valuestest.SchemeEquals, tc.floorResult)

			truncRes, err := testhelpers.RunSchemeCode(t, tc.truncateCode)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, truncRes, valuestest.SchemeEquals, tc.truncResult)

			if tc.shouldDiffer {
				qt.Assert(t, floorRes, qt.Not(valuestest.SchemeEquals), truncRes)
			} else {
				qt.Assert(t, floorRes, valuestest.SchemeEquals, truncRes)
			}
		})
	}
}

// ----------------------------------------------------------------------------
// Error Tests
// ----------------------------------------------------------------------------

func TestDivisionErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		// Division by zero
		{Name: "floor/ divide by zero", Code: `(floor/ 10 0)`},
		{Name: "floor-quotient divide by zero", Code: `(floor-quotient 10 0)`},
		{Name: "floor-remainder divide by zero", Code: `(floor-remainder 10 0)`},
		{Name: "truncate/ divide by zero", Code: `(truncate/ 10 0)`},
		{Name: "truncate-quotient divide by zero", Code: `(truncate-quotient 10 0)`},
		{Name: "truncate-remainder divide by zero", Code: `(truncate-remainder 10 0)`},

		// Wrong type - first argument (strings, symbols, lists are invalid; floats/rationals are valid per R7RS)
		{Name: "floor/ string dividend", Code: `(floor/ "10" 3)`},
		{Name: "floor/ symbol dividend", Code: `(floor/ 'ten 3)`},
		{Name: "floor/ list dividend", Code: `(floor/ '(10) 3)`},

		{Name: "truncate/ string dividend", Code: `(truncate/ "10" 3)`},
		{Name: "truncate/ symbol dividend", Code: `(truncate/ 'ten 3)`},
		{Name: "truncate/ list dividend", Code: `(truncate/ '(10) 3)`},

		// Wrong type - second argument
		{Name: "floor/ string divisor", Code: `(floor/ 10 "3")`},
		{Name: "floor/ symbol divisor", Code: `(floor/ 10 'three)`},
		{Name: "floor/ list divisor", Code: `(floor/ 10 '(3))`},

		{Name: "truncate/ string divisor", Code: `(truncate/ 10 "3")`},
		{Name: "truncate/ symbol divisor", Code: `(truncate/ 10 'three)`},
		{Name: "truncate/ list divisor", Code: `(truncate/ 10 '(3))`},

		// Both arguments wrong type
		{Name: "floor/ both strings", Code: `(floor/ "10" "3")`},
		{Name: "truncate/ both strings", Code: `(truncate/ "10" "3")`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, values.TrueValue)
		})
	}
}
