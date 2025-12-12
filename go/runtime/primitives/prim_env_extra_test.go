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

	qt "github.com/frankban/quicktest"
	"wile/values"
)

func TestNullEnvironment(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "null-environment version 5",
			code: `(null-environment 5)`,
		},
		{
			name: "null-environment version 7",
			code: `(null-environment 7)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
			// Result should be a SchemeEnvironment
			_, ok := result.(*values.SchemeEnvironment)
			qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected SchemeEnvironment, got %T", result))
		})
	}
}

func TestSchemeReportEnvironment(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "scheme-report-environment version 5",
			code: `(scheme-report-environment 5)`,
		},
		{
			name: "scheme-report-environment version 7",
			code: `(scheme-report-environment 7)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
			// Result should be a SchemeEnvironment
			_, ok := result.(*values.SchemeEnvironment)
			qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected SchemeEnvironment, got %T", result))
		})
	}
}

func TestEnvironmentPrimitiveError(t *testing.T) {
	// Test that environment returns an error without library registry
	// This covers the error path in the function
	result, err := runSchemeCode(t, `(environment)`)
	// We expect an error since our test env doesn't have a library registry
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, result, qt.IsNil)
}

func TestEvalWithEnvironments(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "eval with scheme-report-environment",
			code:     `(eval '(+ 1 2) (scheme-report-environment 5))`,
			expected: values.NewInteger(3),
		},
		{
			name:     "eval with null-environment",
			code:     `(eval '(if #t 1 2) (null-environment 5))`,
			expected: values.NewInteger(1),
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}
