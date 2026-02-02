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

	"github.com/aalpar/wile/go/registry/testhelpers"
	"github.com/aalpar/wile/go/values"
)

// TestCoreArithmetic tests basic arithmetic primitives.
func TestCoreArithmetic(t *testing.T) {
	c := qt.New(t)

	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "add two", Code: "(+ 1 2)", Expected: values.NewInteger(3)},
		{Name: "add three", Code: "(+ 1 2 3)", Expected: values.NewInteger(6)},
		{Name: "add zero args", Code: "(+)", Expected: values.NewInteger(0)},
		{Name: "subtract", Code: "(- 10 3)", Expected: values.NewInteger(7)},
		{Name: "multiply", Code: "(* 4 5)", Expected: values.NewInteger(20)},
		{Name: "multiply zero args", Code: "(*)", Expected: values.NewInteger(1)},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, values.SchemeEquals, tc.Expected)
		})
	}
}

// TestCorePredicates tests basic type predicates.
func TestCorePredicates(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected bool
	}{
		{"null? empty list", "(null? '())", true},
		{"null? pair", "(null? '(1))", false},
		{"pair? pair", "(pair? '(1 . 2))", true},
		{"pair? list", "(pair? '(1 2 3))", true},
		{"pair? empty", "(pair? '())", false},
		{"number? integer", "(number? 42)", true},
		{"number? string", `(number? "hello")`, false},
		{"string? string", `(string? "hello")`, true},
		{"string? number", "(string? 42)", false},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			if tc.expected {
				testhelpers.RunSchemeCodeExpectTrue(t, tc.code)
			} else {
				testhelpers.RunSchemeCodeExpectFalse(t, tc.code)
			}
		})
	}
}

// TestCoreListOperations tests basic list operations.
func TestCoreListOperations(t *testing.T) {
	c := qt.New(t)

	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "cons", Code: "(cons 1 2)", Expected: values.NewCons(values.NewInteger(1), values.NewInteger(2))},
		{Name: "car", Code: "(car '(1 2 3))", Expected: values.NewInteger(1)},
		{Name: "cdr", Code: "(cdr '(1 2 3))", Expected: values.List(values.NewInteger(2), values.NewInteger(3))},
		{Name: "list", Code: "(list 1 2 3)", Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{Name: "length", Code: "(length '(1 2 3))", Expected: values.NewInteger(3)},
		{Name: "append", Code: "(append '(1 2) '(3 4))", Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4))},
		{Name: "reverse", Code: "(reverse '(1 2 3))", Expected: values.List(values.NewInteger(3), values.NewInteger(2), values.NewInteger(1))},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, values.SchemeEquals, tc.Expected)
		})
	}
}
