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

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestExpanderLetSyntax(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic let-syntax
		{
			Name:     "let-syntax double",
			Code:     `(let-syntax ((double (syntax-rules () ((_ x) (+ x x))))) (double 5))`,
			Expected: values.NewInteger(10),
		},
		// Hygiene: macro-introduced variable does not capture user variable
		{
			Name: "let-syntax hygiene intro var does not capture",
			Code: `(let-syntax ((swap (syntax-rules ()
			                ((_ a b)
			                 (let ((tmp a))
			                   (list b tmp))))))
			         (let ((tmp 99))
			           (swap tmp 1)))`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(99)),
		},
		// letrec-syntax with recursive macro
		{
			Name: "letrec-syntax recursive my-or",
			Code: `(letrec-syntax ((my-or (syntax-rules ()
			                          ((_) #f)
			                          ((_ e) e)
			                          ((_ e1 e2 ...)
			                           (let ((t e1))
			                             (if t t (my-or e2 ...)))))))
			         (my-or #f #f 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "letrec-syntax my-or all false",
			Code: `(letrec-syntax ((my-or (syntax-rules ()
			                          ((_) #f)
			                          ((_ e) e)
			                          ((_ e1 e2 ...)
			                           (let ((t e1))
			                             (if t t (my-or e2 ...)))))))
			         (my-or #f #f #f))`,
			Expected: values.FalseValue,
		},
		// let-syntax body with define
		{
			Name: "let-syntax body with define",
			Code: `(let-syntax ((id (syntax-rules () ((_ x) x))))
			         (define v (id 99))
			         v)`,
			Expected: values.NewInteger(99),
		},
		// let-syntax with multiple bindings
		{
			Name: "let-syntax multiple bindings",
			Code: `(let-syntax ((first (syntax-rules () ((_ x y) x)))
			              (second (syntax-rules () ((_ x y) y))))
			         (+ (first 10 20) (second 10 20)))`,
			Expected: values.NewInteger(30),
		},
		// Nested let-syntax
		{
			Name: "nested let-syntax",
			Code: `(let-syntax ((add1 (syntax-rules () ((_ x) (+ x 1)))))
			         (let-syntax ((add2 (syntax-rules () ((_ x) (add1 (add1 x))))))
			           (add2 10)))`,
			Expected: values.NewInteger(12),
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
