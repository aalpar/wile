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
	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestNullEnvironment(t *testing.T) {
	tests := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "null-environment version 5",
			Code: `(null-environment 5)`,
		},
		{
			Name: "null-environment version 7",
			Code: `(null-environment 7)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
			// Result should be a Namespace
			_, ok := result.(*environment.Namespace)
			qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected Namespace, got %T", result))
		})
	}
}

func TestSchemeReportEnvironment(t *testing.T) {
	tests := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "scheme-report-environment version 5",
			Code: `(scheme-report-environment 5)`,
		},
		{
			Name: "scheme-report-environment version 7",
			Code: `(scheme-report-environment 7)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
			// Result should be a Namespace
			_, ok := result.(*environment.Namespace)
			qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected Namespace, got %T", result))
		})
	}
}

func TestEnvironmentPrimitiveError(t *testing.T) {
	// Test that (environment) with a library import fails when no library
	// registry is configured (test env doesn't have one).
	_, err := testhelpers.RunSchemeCode(t, `(environment '(scheme base))`)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestEvalWithEnvironments(t *testing.T) {
	tests := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "eval with scheme-report-environment",
			Code:     `(eval '(+ 1 2) (scheme-report-environment 5))`,
			Expected: values.NewInteger(3),
		},
		{
			Name:     "eval with null-environment",
			Code:     `(eval '(if #t 1 2) (null-environment 5))`,
			Expected: values.NewInteger(1),
		},
	}

	for _, tc := range tests {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestEnvironmentQ(t *testing.T) {
	trueTests := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "interaction-environment is env", Code: `(environment? (interaction-environment))`},
	}
	for _, tc := range trueTests {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.Code)
		})
	}

	falseTests := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "integer is not env", Code: `(environment? 42)`},
		{Name: "string is not env", Code: `(environment? "hello")`},
	}
	for _, tc := range falseTests {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectFalse(t, tc.Code)
		})
	}
}

func TestEnvironmentBoundNames(t *testing.T) {
	trueTests := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "result is a pair", Code: `(pair? (environment-bound-names (interaction-environment)))`},
		{Name: "elements are symbols", Code: `(symbol? (car (environment-bound-names (interaction-environment))))`},
	}
	for _, tc := range trueTests {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.Code)
		})
	}

	errorTests := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "wrong type arg", Code: `(environment-bound-names 42)`},
	}
	for _, tc := range errorTests {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

func TestEnvironmentRef(t *testing.T) {
	trueTests := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "lookup + is procedure", Code: `(procedure? (environment-ref (interaction-environment) '+))`},
	}
	for _, tc := range trueTests {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.Code)
		})
	}

	errorTests := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "unbound symbol", Code: `(environment-ref (interaction-environment) 'nonexistent-xyz)`},
		{Name: "wrong env type", Code: `(environment-ref 42 '+)`},
		{Name: "wrong symbol type", Code: `(environment-ref (interaction-environment) 42)`},
	}
	for _, tc := range errorTests {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

func TestEnvironmentBoundQ(t *testing.T) {
	trueTests := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "bound symbol", Code: `(environment-bound? (interaction-environment) '+)`},
	}
	for _, tc := range trueTests {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectTrue(t, tc.Code)
		})
	}

	falseTests := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "unbound symbol", Code: `(environment-bound? (interaction-environment) 'nonexistent-xyz)`},
	}
	for _, tc := range falseTests {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectFalse(t, tc.Code)
		})
	}

	errorTests := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "wrong env type", Code: `(environment-bound? 42 '+)`},
		{Name: "wrong symbol type", Code: `(environment-bound? (interaction-environment) 42)`},
	}
	for _, tc := range errorTests {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}
