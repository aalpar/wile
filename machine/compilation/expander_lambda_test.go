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

package compilation_test

import (
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestExpanderLambda(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Simple lambda
		{
			Name:     "identity lambda",
			Code:     `((lambda (x) x) 42)`,
			Expected: values.NewInteger(42),
		},
		// Rest params
		{
			Name:     "rest params collects all args",
			Code:     `((lambda x x) 1 2 3)`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		// Dotted formals
		{
			Name:     "dotted formals rest",
			Code:     `((lambda (x . rest) rest) 1 2 3)`,
			Expected: values.List(values.NewInteger(2), values.NewInteger(3)),
		},
		{
			Name:     "dotted formals first",
			Code:     `((lambda (x . rest) x) 1 2 3)`,
			Expected: values.NewInteger(1),
		},
		// case-lambda
		{
			Name:     "case-lambda nullary",
			Code:     `((case-lambda (() 0) ((x) x) ((x y) (+ x y))))`,
			Expected: values.NewInteger(0),
		},
		{
			Name:     "case-lambda unary",
			Code:     `((case-lambda (() 0) ((x) x) ((x y) (+ x y))) 99)`,
			Expected: values.NewInteger(99),
		},
		{
			Name:     "case-lambda binary",
			Code:     `((case-lambda (() 0) ((x) x) ((x y) (+ x y))) 3 4)`,
			Expected: values.NewInteger(7),
		},
		// Nested lambda (closure)
		{
			Name:     "nested lambda closure",
			Code:     `((lambda (x) ((lambda (y) (+ x y)) 10)) 20)`,
			Expected: values.NewInteger(30),
		},
		// Lambda with internal defines
		{
			Name:     "lambda body with internal defines",
			Code:     `((lambda () (define a 1) (define b 2) (+ a b)))`,
			Expected: values.NewInteger(3),
		},
		// Multi-arg lambda
		{
			Name:     "multi-arg lambda",
			Code:     `((lambda (a b c) (+ a (* b c))) 1 2 3)`,
			Expected: values.NewInteger(7),
		},
		// Lambda returning lambda
		{
			Name:     "lambda returning lambda (currying)",
			Code:     `(((lambda (x) (lambda (y) (+ x y))) 10) 20)`,
			Expected: values.NewInteger(30),
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
