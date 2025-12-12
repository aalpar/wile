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

package primitives_test

import (
	"testing"

	"wile/values"

	qt "github.com/frankban/quicktest"
)

// ----------------------------------------------------------------------------
// Floor Tests - Extended Coverage
// ----------------------------------------------------------------------------

func TestFloorExtended(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
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
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tt.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tt.expected)
		})
	}
}

// ----------------------------------------------------------------------------
// Ceiling Tests - Extended Coverage
// ----------------------------------------------------------------------------

func TestCeilingExtended(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
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
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tt.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tt.expected)
		})
	}
}

// ----------------------------------------------------------------------------
// Round Tests - Extended Coverage
// ----------------------------------------------------------------------------

func TestRoundExtended(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
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
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tt.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tt.expected)
		})
	}
}

// ----------------------------------------------------------------------------
// Truncate Tests - Extended Coverage
// ----------------------------------------------------------------------------

func TestTruncateExtended(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
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
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tt.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tt.expected)
		})
	}
}
