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

func TestMachineContextApply(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Lambda application
		{Name: "identity lambda", Code: `((lambda (x) x) 42)`, Expected: values.NewInteger(42)},
		{Name: "lambda with body", Code: `((lambda (x y) (+ x y)) 3 4)`, Expected: values.NewInteger(7)},
		{Name: "nested lambda", Code: `((lambda (x) ((lambda (y) (+ x y)) 10)) 5)`, Expected: values.NewInteger(15)},

		// Case-lambda application
		{Name: "case-lambda one arg", Code: `((case-lambda ((x) x) ((x y) (+ x y))) 42)`, Expected: values.NewInteger(42)},
		{Name: "case-lambda two args", Code: `((case-lambda ((x) x) ((x y) (+ x y))) 1 2)`, Expected: values.NewInteger(3)},

		// Parameter application
		{Name: "parameter get", Code: `(let ((p (make-parameter 10))) (p))`, Expected: values.NewInteger(10)},
		{Name: "parameter set", Code: `(let ((p (make-parameter 10))) (p 20) (p))`, Expected: values.NewInteger(20)},

		// Variadic lambda
		{Name: "variadic lambda", Code: `((lambda (x . rest) x) 1 2 3)`, Expected: values.NewInteger(1)},
		{Name: "variadic rest", Code: `((lambda (x . rest) rest) 1 2 3)`, Expected: values.List(values.NewInteger(2), values.NewInteger(3))},

		// Higher-order function
		{Name: "higher-order", Code: `((lambda (f x) (f x)) (lambda (n) (* n 2)) 5)`, Expected: values.NewInteger(10)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestMachineContextApplyErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-callable integer", Code: `(1 2 3)`},
		{Name: "non-callable string", Code: `("hello" 1)`},
		{Name: "arity mismatch too many", Code: `((lambda (x) x) 1 2)`},
		{Name: "arity mismatch too few", Code: `((lambda (x y) x))`},
		{Name: "case-lambda no match", Code: `((case-lambda ((x) x) ((x y) x)) 1 2 3)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
