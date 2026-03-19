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

package machine_test

import (
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestCompileHelpers tests compile-time helper infrastructure: self-evaluating
// literals, symbol resolution, and expandCompileExecute.
//
// Source: compile_helpers.go (expandCompileExecute, executeFormsAtCompileTime,
// ensureState).
func TestCompileHelpers(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Self-evaluating literals
		{Name: "integer literal", Code: `42`, Expected: values.NewInteger(42)},
		{Name: "negative integer", Code: `-7`, Expected: values.NewInteger(-7)},
		{Name: "string literal", Code: `"hello"`, Expected: values.NewString("hello")},
		{Name: "boolean true", Code: `#t`, Expected: values.TrueValue},
		{Name: "boolean false", Code: `#f`, Expected: values.FalseValue},
		{Name: "character literal", Code: `#\a`, Expected: values.NewCharacter('a')},
		{Name: "empty list via quote", Code: `'()`, Expected: values.EmptyList},

		// Symbol resolution via let
		{Name: "let binding resolves symbol", Code: `(let ((x 5)) x)`, Expected: values.NewInteger(5)},
		{
			Name:     "nested let resolves inner binding",
			Code:     `(let ((x 1)) (let ((y (+ x 1))) y))`,
			Expected: values.NewInteger(2),
		},
		{
			Name:     "let shadows outer binding",
			Code:     `(let ((x 10)) (let ((x 20)) x))`,
			Expected: values.NewInteger(20),
		},

		// Quote produces literals
		{Name: "quoted symbol", Code: `'foo`, Expected: values.NewSymbol("foo")},
		{
			Name:     "quoted list",
			Code:     `'(1 2 3)`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},

		// begin-for-syntax uses expandCompileExecute
		// (tested indirectly; the infrastructure is exercised by any compile-time evaluation)
		{
			Name:     "begin sequences expressions",
			Code:     `(begin 1 2 3)`,
			Expected: values.NewInteger(3),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
