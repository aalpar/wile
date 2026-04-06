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

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

func TestCompileLetBasic(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "single binding", Code: `(let ((x 1)) x)`, Expected: values.NewInteger(1)},
		{Name: "multiple bindings", Code: `(let ((x 1) (y 2)) (+ x y))`, Expected: values.NewInteger(3)},
		{Name: "nested let", Code: `(let ((x 1)) (let ((y 2)) (+ x y)))`, Expected: values.NewInteger(3)},
		{Name: "let in tail position", Code: `((lambda () (let ((x 42)) x)))`, Expected: values.NewInteger(42)},
		{Name: "let with set!", Code: `(let ((x 1)) (set! x 2) x)`, Expected: values.NewInteger(2)},
		{Name: "let closure capture", Code: `(let ((x 1)) (let ((f (lambda () x))) (f)))`, Expected: values.NewInteger(1)},
		{Name: "empty bindings", Code: `(let () 42)`, Expected: values.NewInteger(42)},
		{Name: "multiple body", Code: `(let ((x 1)) (+ x 1) (+ x 2))`, Expected: values.NewInteger(3)},
		{Name: "let returns lambda", Code: `((let ((x 1)) (lambda () x)))`, Expected: values.NewInteger(1)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCompileLetStarBasic(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "sequential visibility", Code: `(let* ((x 1) (y (+ x 1))) y)`, Expected: values.NewInteger(2)},
		{Name: "chain of three", Code: `(let* ((a 1) (b (+ a 1)) (c (+ b 1))) c)`, Expected: values.NewInteger(3)},
		{Name: "empty bindings", Code: `(let* () 42)`, Expected: values.NewInteger(42)},
		{Name: "let* with set!", Code: `(let* ((x 1)) (set! x 2) x)`, Expected: values.NewInteger(2)},
		{Name: "let* closure captures preceding", Code: `(let* ((x 10) (f (lambda () x))) (f))`, Expected: values.NewInteger(10)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCompileLetrecBasic(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "recursive factorial", Code: `(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))`, Expected: values.NewInteger(120)},
		{Name: "mutual recursion", Code: `(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 10))`, Expected: values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCompileLetrecStarBasic(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "sequential with values", Code: `(letrec* ((x 1) (y (+ x 1))) y)`, Expected: values.NewInteger(2)},
		{Name: "forward ref via closure", Code: `(letrec* ((f (lambda () g)) (g 42)) (f))`, Expected: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCompileNamedLet(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "factorial", Code: `(let loop ((n 5) (acc 1)) (if (= n 0) acc (loop (- n 1) (* acc n))))`, Expected: values.NewInteger(120)},
		{Name: "empty bindings", Code: `(let loop () 42)`, Expected: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestLetIntegration(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Shadowing special forms
		{Name: "shadow if", Code: `(let ((if 42)) if)`, Expected: values.NewInteger(42)},
		// Nested binding forms
		{Name: "let inside let*", Code: `(let* ((x 1)) (let ((y (+ x 1))) y))`, Expected: values.NewInteger(2)},
		{Name: "let* inside let", Code: `(let ((x 10)) (let* ((y x) (z (+ y 1))) z))`, Expected: values.NewInteger(11)},
		{Name: "letrec inside let", Code: `(let ((x 10)) (letrec ((f (lambda () x))) (f)))`, Expected: values.NewInteger(10)},
		// All four forms work
		{Name: "letrec factorial", Code: `(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))`, Expected: values.NewInteger(120)},
		{Name: "letrec* sequential", Code: `(letrec* ((x 1) (y (+ x 1))) y)`, Expected: values.NewInteger(2)},
		// let bindings don't see each other
		{Name: "let bindings isolated", Code: `(let ((x 10)) (let ((x 1) (y x)) y))`, Expected: values.NewInteger(10)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestLetMacroGenerated(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "cond true", Code: `(cond (#t 42))`, Expected: values.NewInteger(42)},
		{Name: "cond multi", Code: `(cond (#f 1) (#t 2))`, Expected: values.NewInteger(2)},
		{Name: "cond =>", Code: `(cond ((assv 2 '((1 . one) (2 . two) (3 . three))) => cdr))`, Expected: values.NewSymbol("two")},
		{Name: "case", Code: `(case (+ 1 1) ((1) 'one) ((2) 'two) ((3) 'three))`, Expected: values.NewSymbol("two")},
		{Name: "do loop", Code: `(do ((i 0 (+ i 1))) ((= i 5) i))`, Expected: values.NewInteger(5)},
		{Name: "and", Code: `(and 1 2 3)`, Expected: values.NewInteger(3)},
		{Name: "or", Code: `(or #f #f 42)`, Expected: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestLetInternalDefine(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "define in let body", Code: `(let ((x 1)) (define y 2) (+ x y))`, Expected: values.NewInteger(3)},
		{Name: "define in let* body", Code: `(let* ((x 1)) (define y (+ x 1)) y)`, Expected: values.NewInteger(2)},
		{Name: "define in letrec body", Code: `(letrec ((f (lambda () 1))) (define x (f)) x)`, Expected: values.NewInteger(1)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestLetShadowingLet(t *testing.T) {
	result, err := testhelpers.RunSchemeCode(t, `(let ((let 42)) let)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(42))
}

func TestLetCallCC(t *testing.T) {
	result, err := testhelpers.RunSchemeCode(t, `(call-with-current-continuation (lambda (k) (let ((x 42)) (k x))))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(42))
}

func TestLetStarSelfShadowCallCC(t *testing.T) {
	// Regression: let* self-rebinding must not shadow the outer binding
	// during init compilation. The init expression should see the OUTER k,
	// not the new (void) slot. R7RS 4.2.2: "Semantically, a let* expression
	// is equivalent to nested let expressions."
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "let* self-shadow with call/cc escape",
			Code: `(call-with-current-continuation
				(lambda (k)
					(let* ((k (begin (k 'escaped) 'dead)))
						k)))`,
			Expected: values.NewSymbol("escaped"),
		},
		{
			Name: "let* self-shadow reads outer value",
			Code: `(let ((x 10))
				(let* ((x (+ x 1)))
					x))`,
			Expected: values.NewInteger(11),
		},
		{
			Name: "let* second binding sees first not self",
			Code: `(let ((y 100))
				(let* ((y 1) (y (+ y 1)))
					y))`,
			Expected: values.NewInteger(2),
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

func TestLetrecCallCC(t *testing.T) {
	code := `(call-with-current-continuation
		(lambda (k)
			(letrec ((f (lambda (n) (if (= n 0) (k 42) (f (- n 1))))))
				(f 3))))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(42))
}
