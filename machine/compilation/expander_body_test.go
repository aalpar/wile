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

func TestExpanderBody(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Internal definitions in body
		{
			Name:     "internal defines in let",
			Code:     `(let () (define x 1) (define y 2) (+ x y))`,
			Expected: values.NewInteger(3),
		},
		{
			Name:     "internal define-syntax in body",
			Code:     `(let () (define-syntax dbl (syntax-rules () ((_ v) (+ v v)))) (dbl 5))`,
			Expected: values.NewInteger(10),
		},
		{
			Name:     "mixed define-syntax and define in body",
			Code:     `(let () (define-syntax id (syntax-rules () ((_ v) v))) (define x (id 42)) x)`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "forward reference with letrec",
			Code: `(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
			                 (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
			          (even? 4))`,
			Expected: values.TrueValue,
		},
		{
			Name: "forward reference odd",
			Code: `(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
			                 (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
			          (odd? 3))`,
			Expected: values.TrueValue,
		},
		{
			Name:     "internal define with sequential use",
			Code:     `(let () (define a 10) (define b (+ a 5)) b)`,
			Expected: values.NewInteger(15),
		},
		{
			Name: "body with define-syntax used by later define",
			Code: `(let ()
			         (define-syntax double (syntax-rules () ((_ x) (+ x x))))
			         (define val (double 21))
			         val)`,
			Expected: values.NewInteger(42),
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
