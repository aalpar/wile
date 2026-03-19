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

func TestCallPromoted(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// null?
		{Name: "null? empty list", Code: `(null? '())`, Expected: values.TrueValue},
		{Name: "null? integer", Code: `(null? 1)`, Expected: values.FalseValue},
		{Name: "null? pair", Code: `(null? '(1 2))`, Expected: values.FalseValue},
		{Name: "null? string", Code: `(null? "hello")`, Expected: values.FalseValue},

		// pair?
		{Name: "pair? pair", Code: `(pair? '(1))`, Expected: values.TrueValue},
		{Name: "pair? cons", Code: `(pair? (cons 1 2))`, Expected: values.TrueValue},
		{Name: "pair? integer", Code: `(pair? 1)`, Expected: values.FalseValue},
		{Name: "pair? empty list", Code: `(pair? '())`, Expected: values.FalseValue},

		// eq?
		{Name: "eq? same symbol", Code: `(eq? 'a 'a)`, Expected: values.TrueValue},
		{Name: "eq? different symbols", Code: `(eq? 'a 'b)`, Expected: values.FalseValue},
		{Name: "eq? booleans true", Code: `(eq? #t #t)`, Expected: values.TrueValue},
		{Name: "eq? booleans false", Code: `(eq? #t #f)`, Expected: values.FalseValue},

		// car
		{Name: "car of pair", Code: `(car '(1 2))`, Expected: values.NewInteger(1)},
		{Name: "car of nested", Code: `(car '((a) b))`, Expected: values.List(values.NewSymbol("a"))},

		// cdr
		{Name: "cdr of pair", Code: `(cdr '(1 2))`, Expected: values.List(values.NewInteger(2))},
		{Name: "cdr of single", Code: `(cdr '(1))`, Expected: values.EmptyList},

		// cons
		{Name: "cons improper pair", Code: `(cons 1 2)`, Expected: values.NewCons(values.NewInteger(1), values.NewInteger(2))},
		{Name: "cons proper list", Code: `(cons 1 '())`, Expected: values.List(values.NewInteger(1))},
		{Name: "cons onto list", Code: `(cons 1 '(2 3))`, Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))},

		// vector?
		{Name: "vector? vector", Code: `(vector? #(1 2 3))`, Expected: values.TrueValue},
		{Name: "vector? integer", Code: `(vector? 1)`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCallPromotedErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "car of non-pair", Code: `(car 5)`},
		{Name: "cdr of non-pair", Code: `(cdr 5)`},
		{Name: "car of empty list", Code: `(car '())`},
		{Name: "cdr of empty list", Code: `(cdr '())`},
		{Name: "vector-ref non-vector", Code: `(vector-ref 5 0)`},
		{Name: "vector-ref out of bounds", Code: `(vector-ref #(1 2 3) 5)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
