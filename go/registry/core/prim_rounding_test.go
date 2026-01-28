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

	"wile/values"

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
		// Round tests - floats and integers
		{
			name:     "round 3.5 up",
			code:     "(round 3.5)",
			expected: values.NewFloat(4.0),
		},
		{
			name:     "round 2.5 up",
			code:     "(round 2.5)",
			expected: values.NewFloat(3.0),
		},
		{
			name:     "round negative half",
			code:     "(round -3.5)",
			expected: values.NewFloat(-4.0),
		},
		{
			name:     "round integer passthrough",
			code:     "(round 3)",
			expected: values.NewInteger(3),
		},
		{
			name:     "round 4.5 up",
			code:     "(round 4.5)",
			expected: values.NewFloat(5.0),
		},
		{
			name:     "round 5.5 up",
			code:     "(round 5.5)",
			expected: values.NewFloat(6.0),
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
		// Round tests - rationals (exact inputs → exact outputs per R7RS)
		{
			name:     "round 5/2 half away from zero",
			code:     "(round 5/2)",
			expected: values.NewInteger(3), // 2.5 rounds to 3 (away from zero)
		},
		{
			name:     "round 7/2 half away from zero",
			code:     "(round 7/2)",
			expected: values.NewInteger(4), // 3.5 rounds to 4 (away from zero)
		},
		{
			name:     "round 7/3",
			code:     "(round 7/3)",
			expected: values.NewInteger(2), // 2.333... rounds to 2
		},
		{
			name:     "round -5/2 negative half",
			code:     "(round -5/2)",
			expected: values.NewInteger(-3), // -2.5 rounds to -3 (away from zero)
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

func TestFloorDivQuotientRemainder(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "floor/ basic",
			code: `(floor/ 10 3)`,
		},
		{
			name: "floor-quotient",
			code: `(floor-quotient 10 3)`,
		},
		{
			name: "floor-remainder",
			code: `(floor-remainder 10 3)`,
		},
		{
			name: "truncate/",
			code: `(truncate/ 10 3)`,
		},
		{
			name: "truncate-quotient",
			code: `(truncate-quotient 10 3)`,
		},
		{
			name: "truncate-remainder",
			code: `(truncate-remainder 10 3)`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}
