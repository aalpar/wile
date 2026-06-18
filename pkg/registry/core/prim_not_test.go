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

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// not Tests (R7RS §6.3 - Boolean negation)

func TestNotComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Only #f is false
		{Name: "not false is true", Code: `(not #f)`, Expected: values.TrueValue},

		// Everything else is true (returns #f)
		{Name: "not true", Code: `(not #t)`, Expected: values.FalseValue},
		{Name: "not zero", Code: `(not 0)`, Expected: values.FalseValue},
		{Name: "not one", Code: `(not 1)`, Expected: values.FalseValue},
		{Name: "not negative", Code: `(not -1)`, Expected: values.FalseValue},
		{Name: "not empty list", Code: `(not '())`, Expected: values.FalseValue},
		{Name: "not non-empty list", Code: `(not '(1 2 3))`, Expected: values.FalseValue},
		{Name: "not empty string", Code: `(not "")`, Expected: values.FalseValue},
		{Name: "not non-empty string", Code: `(not "hello")`, Expected: values.FalseValue},
		{Name: "not symbol", Code: `(not 'foo)`, Expected: values.FalseValue},
		{Name: "not vector", Code: `(not #(1 2 3))`, Expected: values.FalseValue},
		{Name: "not empty vector", Code: `(not #())`, Expected: values.FalseValue},
		{Name: "not procedure", Code: `(not +)`, Expected: values.FalseValue},
		{Name: "not lambda", Code: `(not (lambda (x) x))`, Expected: values.FalseValue},
		{Name: "not character", Code: `(not #\a)`, Expected: values.FalseValue},
		{Name: "not float", Code: `(not 3.14)`, Expected: values.FalseValue},
		{Name: "not rational", Code: `(not 1/2)`, Expected: values.FalseValue},
		{Name: "not complex", Code: `(not 1+2i)`, Expected: values.FalseValue},

		// Double negation
		{Name: "not not false", Code: `(not (not #f))`, Expected: values.FalseValue},
		{Name: "not not true", Code: `(not (not #t))`, Expected: values.TrueValue},
		{Name: "not not number", Code: `(not (not 42))`, Expected: values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
