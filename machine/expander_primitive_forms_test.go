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

func TestExpanderPrimitiveForms(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// if
		{Name: "if true branch", Code: `(if #t 'yes 'no)`, Expected: values.NewSymbol("yes")},
		{Name: "if false branch", Code: `(if #f 'yes 'no)`, Expected: values.NewSymbol("no")},
		{Name: "if true no alternative", Code: `(if #t 'yes)`, Expected: values.NewSymbol("yes")},
		{Name: "if truthy value", Code: `(if 42 'yes 'no)`, Expected: values.NewSymbol("yes")},
		{Name: "if with expression test", Code: `(if (> 3 2) 'greater 'not-greater)`, Expected: values.NewSymbol("greater")},

		// begin
		{Name: "begin single", Code: `(begin 42)`, Expected: values.NewInteger(42)},
		{Name: "begin multiple", Code: `(begin 1 2 3)`, Expected: values.NewInteger(3)},
		{Name: "begin with side effects", Code: `(begin (define x 10) (+ x 5))`, Expected: values.NewInteger(15)},

		// set!
		{Name: "set! mutates variable", Code: `(let ((x 1)) (set! x 2) x)`, Expected: values.NewInteger(2)},
		{Name: "set! with expression", Code: `(let ((x 1)) (set! x (+ x 10)) x)`, Expected: values.NewInteger(11)},

		// define
		{Name: "define simple variable", Code: `(begin (define x 42) x)`, Expected: values.NewInteger(42)},
		{Name: "define function shorthand", Code: `(begin (define (f x) (* x 2)) (f 5))`, Expected: values.NewInteger(10)},
		{Name: "define function with body", Code: `(begin (define (add a b) (+ a b)) (add 3 7))`, Expected: values.NewInteger(10)},
		{Name: "define with expression", Code: `(begin (define y (+ 20 22)) y)`, Expected: values.NewInteger(42)},

		// quote
		{Name: "quote list", Code: `(quote (1 2 3))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},
		{Name: "quote symbol", Code: `'hello`, Expected: values.NewSymbol("hello")},
		{Name: "quote number", Code: `'42`, Expected: values.NewInteger(42)},
		{Name: "quote nested", Code: `'(a (b c))`, Expected: values.List(
			values.NewSymbol("a"),
			values.List(values.NewSymbol("b"), values.NewSymbol("c")),
		)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestExpanderPrimitiveFormsErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "set! non-variable", Code: `(set! 42 1)`},
		{Name: "set! missing value", Code: `(set!)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}
