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
// Floor Tests - Rational Arguments
// ----------------------------------------------------------------------------

func TestFloorRational(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "floor 5/2 positive",
			code:     "(floor 5/2)",
			expected: values.NewFloat(2.0),
		},
		{
			name:     "floor -5/2 negative",
			code:     "(floor -5/2)",
			expected: values.NewFloat(-3.0),
		},
		{
			name:     "floor 7/3",
			code:     "(floor 7/3)",
			expected: values.NewFloat(2.0),
		},
		{
			name:     "floor -7/3 negative",
			code:     "(floor -7/3)",
			expected: values.NewFloat(-3.0),
		},
		{
			name:     "floor 1/2",
			code:     "(floor 1/2)",
			expected: values.NewFloat(0.0),
		},
		{
			name:     "floor -1/2 negative",
			code:     "(floor -1/2)",
			expected: values.NewFloat(-1.0),
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
// Ceiling Tests - Rational Arguments
// ----------------------------------------------------------------------------

func TestCeilingRational(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "ceiling 5/2 positive",
			code:     "(ceiling 5/2)",
			expected: values.NewFloat(3.0),
		},
		{
			name:     "ceiling -5/2 negative",
			code:     "(ceiling -5/2)",
			expected: values.NewFloat(-2.0),
		},
		{
			name:     "ceiling 7/3",
			code:     "(ceiling 7/3)",
			expected: values.NewFloat(3.0),
		},
		{
			name:     "ceiling -7/3 negative",
			code:     "(ceiling -7/3)",
			expected: values.NewFloat(-2.0),
		},
		{
			name:     "ceiling 1/2",
			code:     "(ceiling 1/2)",
			expected: values.NewFloat(1.0),
		},
		{
			name:     "ceiling -1/2 negative",
			code:     "(ceiling -1/2)",
			expected: values.NewFloat(0.0),
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
// Round Tests - Rational Arguments
// ----------------------------------------------------------------------------

func TestRoundRational(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "round 5/2 half away from zero",
			code:     "(round 5/2)",
			expected: values.NewFloat(3.0), // 2.5 rounds to 3 (away from zero)
		},
		{
			name:     "round 7/2 half away from zero",
			code:     "(round 7/2)",
			expected: values.NewFloat(4.0), // 3.5 rounds to 4 (away from zero)
		},
		{
			name:     "round 7/3",
			code:     "(round 7/3)",
			expected: values.NewFloat(2.0), // 2.333... rounds to 2
		},
		{
			name:     "round -5/2 negative half",
			code:     "(round -5/2)",
			expected: values.NewFloat(-3.0), // -2.5 rounds to -3 (away from zero)
		},
		{
			name:     "round 8/3",
			code:     "(round 8/3)",
			expected: values.NewFloat(3.0), // 2.666... rounds to 3
		},
		{
			name:     "round 1/3",
			code:     "(round 1/3)",
			expected: values.NewFloat(0.0), // 0.333... rounds to 0
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
// Truncate Tests - Rational Arguments
// ----------------------------------------------------------------------------

func TestTruncateRational(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "truncate 5/2 positive",
			code:     "(truncate 5/2)",
			expected: values.NewFloat(2.0),
		},
		{
			name:     "truncate -5/2 negative",
			code:     "(truncate -5/2)",
			expected: values.NewFloat(-2.0),
		},
		{
			name:     "truncate 7/3",
			code:     "(truncate 7/3)",
			expected: values.NewFloat(2.0),
		},
		{
			name:     "truncate -7/3 negative",
			code:     "(truncate -7/3)",
			expected: values.NewFloat(-2.0),
		},
		{
			name:     "truncate 1/2",
			code:     "(truncate 1/2)",
			expected: values.NewFloat(0.0),
		},
		{
			name:     "truncate -1/2 negative",
			code:     "(truncate -1/2)",
			expected: values.NewFloat(0.0),
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
