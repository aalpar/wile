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

// define-values Tests (R7RS §5.3.3)

func TestDefineValuesComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Zero values
		{
			Name:     "zero values",
			Code:     `(begin (define-values () (values)) 'ok)`,
			Expected: values.NewSymbol("ok"),
		},

		// Single value
		{
			Name:     "single value",
			Code:     `(begin (define-values (x) (values 42)) x)`,
			Expected: values.NewInteger(42),
		},

		// Two values
		{
			Name:     "two values",
			Code:     `(begin (define-values (a b) (values 1 2)) (list a b))`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2)),
		},

		// Three values
		{
			Name:     "three values",
			Code:     `(begin (define-values (a b c) (values 1 2 3)) (list a b c))`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},

		// Four values
		{
			Name:     "four values",
			Code:     `(begin (define-values (w x y z) (values 'a 'b 'c 'd)) (list w x y z))`,
			Expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"), values.NewSymbol("d")),
		},

		// With floor/
		{
			Name:     "floor/ returns two values",
			Code:     `(begin (define-values (quot rem) (floor/ 17 5)) (list quot rem))`,
			Expected: values.List(values.NewInteger(3), values.NewInteger(2)),
		},

		// Mixed types
		{
			Name:     "mixed types",
			Code:     `(begin (define-values (num str sym) (values 42 "hello" 'world)) (list num str sym))`,
			Expected: values.List(values.NewInteger(42), values.NewString("hello"), values.NewSymbol("world")),
		},

		// Rest pattern: bare identifier collects all values as a list (R7RS §5.3.3)
		{
			Name:     "rest pattern collects all values",
			Code:     `(begin (define-values x (values 1 2)) x)`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			Name:     "rest pattern single value",
			Code:     `(begin (define-values x (values 42)) x)`,
			Expected: values.List(values.NewInteger(42)),
		},
		{
			Name:     "rest pattern no values",
			Code:     `(begin (define-values x (values)) x)`,
			Expected: values.EmptyList,
		},
		{
			Name:     "rest pattern many values",
			Code:     `(begin (define-values x (values 'a 'b 'c 'd 'e)) x)`,
			Expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"), values.NewSymbol("d"), values.NewSymbol("e")),
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
