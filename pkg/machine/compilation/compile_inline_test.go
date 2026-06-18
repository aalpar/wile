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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

func TestInlineLetBoundLambda(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "single arg", Code: `(let ((f (lambda (x) (+ x 1)))) (f 42))`, Expected: values.NewInteger(43)},
		{Name: "two args", Code: `(let ((f (lambda (x y) (+ x y)))) (f 3 4))`, Expected: values.NewInteger(7)},
		{Name: "thunk", Code: `(let ((f (lambda () 42))) (f))`, Expected: values.NewInteger(42)},
		{Name: "multiple calls", Code: `(let ((f (lambda (x) (+ x 1)))) (+ (f 1) (f 2)))`, Expected: values.NewInteger(5)},
		{Name: "two inlinable bindings", Code: `(let ((add (lambda (a b) (+ a b))) (mul (lambda (a b) (* a b)))) (add (mul 2 3) (mul 4 5)))`, Expected: values.NewInteger(26)},
		{Name: "free variable", Code: `(let ((x 10)) (let ((f (lambda (y) (+ x y)))) (f 32)))`, Expected: values.NewInteger(42)},
		{Name: "tail position", Code: `(let ((f (lambda (x) x))) (f 99))`, Expected: values.NewInteger(99)},
		{Name: "multi-expr body", Code: `(let ((f (lambda (x) (define y (+ x 1)) (+ y 2)))) (f 10))`, Expected: values.NewInteger(13)},
		{Name: "nested inlinable", Code: `(let ((f (lambda (x) (let ((g (lambda (y) (+ y 1)))) (g x))))) (f 5))`, Expected: values.NewInteger(6)},
		{Name: "if in body", Code: `(let ((f (lambda (x) (if (> x 0) x (- x))))) (f -5))`, Expected: values.NewInteger(5)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestInlineSkipsNonEligible(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "mutable", Code: `(let ((f (lambda (x) x))) (set! f (lambda (x) (+ x 1))) (f 42))`, Expected: values.NewInteger(43)},
		{Name: "escaped returned", Code: `(let ((f (lambda (x) (+ x 1)))) (let ((g f)) (g 42)))`, Expected: values.NewInteger(43)},
		{Name: "variadic", Code: `(let ((f (lambda (x . rest) (cons x rest)))) (f 1 2 3))`,
			Expected: values.NewCons(
				values.NewInteger(1),
				values.NewCons(
					values.NewInteger(2),
					values.NewCons(
						values.NewInteger(3),
						values.EmptyList,
					),
				),
			),
		},
		{Name: "letrec recursive", Code: `(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))`, Expected: values.NewInteger(120)},
		{Name: "named let", Code: `(let loop ((n 5) (acc 1)) (if (= n 0) acc (loop (- n 1) (* acc n))))`, Expected: values.NewInteger(120)},
		{Name: "let* shadowed", Code: `(let* ((x 1) (f (lambda () x)) (x 2)) (f))`, Expected: values.NewInteger(1)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
