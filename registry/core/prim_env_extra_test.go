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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestNullEnvironment(t *testing.T) {
	tests := []schemeCodeErrorTestCase{
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
			// Result should be a TopLevelEnvironment
			_, ok := result.(*environment.TopLevelEnvironment)
			qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected TopLevelEnvironment, got %T", result))
		})
	}
}

func TestSchemeReportEnvironment(t *testing.T) {
	tests := []schemeCodeErrorTestCase{
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
			// Result should be a TopLevelEnvironment
			_, ok := result.(*environment.TopLevelEnvironment)
			qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected TopLevelEnvironment, got %T", result))
		})
	}
}

func TestEnvironmentPrimitiveError(t *testing.T) {
	// Test that (environment) with a library import fails when no library
	// registry is configured (test env doesn't have one).
	_, err := runSchemeCode(t, `(environment '(scheme base))`)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestEvalWithEnvironments(t *testing.T) {
	tests := []schemeCodeTestCase{
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
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

func TestEnvironmentQ(t *testing.T) {
	trueTests := []schemeCodeErrorTestCase{
		{name: "interaction-environment is env", code: `(environment? (interaction-environment))`},
	}
	for _, tc := range trueTests {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectTrue(t, tc.code)
		})
	}

	falseTests := []schemeCodeErrorTestCase{
		{name: "integer is not env", code: `(environment? 42)`},
		{name: "string is not env", code: `(environment? "hello")`},
	}
	for _, tc := range falseTests {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectFalse(t, tc.code)
		})
	}
}

func TestEnvironmentBoundNames(t *testing.T) {
	trueTests := []schemeCodeErrorTestCase{
		{name: "result is a pair", code: `(pair? (environment-bound-names (interaction-environment)))`},
		{name: "elements are symbols", code: `(symbol? (car (environment-bound-names (interaction-environment))))`},
	}
	for _, tc := range trueTests {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectTrue(t, tc.code)
		})
	}

	errorTests := []schemeCodeErrorTestCase{
		{name: "wrong type arg", code: `(environment-bound-names 42)`},
	}
	for _, tc := range errorTests {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestEnvironmentRef(t *testing.T) {
	trueTests := []schemeCodeErrorTestCase{
		{name: "lookup + is procedure", code: `(procedure? (environment-ref (interaction-environment) '+))`},
	}
	for _, tc := range trueTests {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectTrue(t, tc.code)
		})
	}

	errorTests := []schemeCodeErrorTestCase{
		{name: "unbound symbol", code: `(environment-ref (interaction-environment) 'nonexistent-xyz)`},
		{name: "wrong env type", code: `(environment-ref 42 '+)`},
		{name: "wrong symbol type", code: `(environment-ref (interaction-environment) 42)`},
	}
	for _, tc := range errorTests {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}

func TestEnvironmentBoundQ(t *testing.T) {
	trueTests := []schemeCodeErrorTestCase{
		{name: "bound symbol", code: `(environment-bound? (interaction-environment) '+)`},
	}
	for _, tc := range trueTests {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectTrue(t, tc.code)
		})
	}

	falseTests := []schemeCodeErrorTestCase{
		{name: "unbound symbol", code: `(environment-bound? (interaction-environment) 'nonexistent-xyz)`},
	}
	for _, tc := range falseTests {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectFalse(t, tc.code)
		})
	}

	errorTests := []schemeCodeErrorTestCase{
		{name: "wrong env type", code: `(environment-bound? 42 '+)`},
		{name: "wrong symbol type", code: `(environment-bound? (interaction-environment) 42)`},
	}
	for _, tc := range errorTests {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}
