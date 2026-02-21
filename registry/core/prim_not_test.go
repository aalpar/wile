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

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// not Tests (R7RS §6.3 - Boolean negation)

func TestNotComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Only #f is false
		{name: "not false is true", code: `(not #f)`, expected: values.TrueValue},

		// Everything else is true (returns #f)
		{name: "not true", code: `(not #t)`, expected: values.FalseValue},
		{name: "not zero", code: `(not 0)`, expected: values.FalseValue},
		{name: "not one", code: `(not 1)`, expected: values.FalseValue},
		{name: "not negative", code: `(not -1)`, expected: values.FalseValue},
		{name: "not empty list", code: `(not '())`, expected: values.FalseValue},
		{name: "not non-empty list", code: `(not '(1 2 3))`, expected: values.FalseValue},
		{name: "not empty string", code: `(not "")`, expected: values.FalseValue},
		{name: "not non-empty string", code: `(not "hello")`, expected: values.FalseValue},
		{name: "not symbol", code: `(not 'foo)`, expected: values.FalseValue},
		{name: "not vector", code: `(not #(1 2 3))`, expected: values.FalseValue},
		{name: "not empty vector", code: `(not #())`, expected: values.FalseValue},
		{name: "not procedure", code: `(not +)`, expected: values.FalseValue},
		{name: "not lambda", code: `(not (lambda (x) x))`, expected: values.FalseValue},
		{name: "not character", code: `(not #\a)`, expected: values.FalseValue},
		{name: "not float", code: `(not 3.14)`, expected: values.FalseValue},
		{name: "not rational", code: `(not 1/2)`, expected: values.FalseValue},
		{name: "not complex", code: `(not 1+2i)`, expected: values.FalseValue},

		// Double negation
		{name: "not not false", code: `(not (not #f))`, expected: values.FalseValue},
		{name: "not not true", code: `(not (not #t))`, expected: values.TrueValue},
		{name: "not not number", code: `(not (not 42))`, expected: values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}
