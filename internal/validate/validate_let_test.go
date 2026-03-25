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

package validate_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/internal/validate"
	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

// ============================================================================
// let
// ============================================================================

func TestLetValidation(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "single binding", Code: `(let ((x 1)) x)`, Expected: values.NewInteger(1)},
		{Name: "multiple bindings", Code: `(let ((x 1) (y 2)) (+ x y))`, Expected: values.NewInteger(3)},
		{Name: "empty bindings", Code: `(let () 42)`, Expected: values.NewInteger(42)},
		{Name: "nested let", Code: `(let ((x 1)) (let ((y 2)) (+ x y)))`, Expected: values.NewInteger(3)},
		{Name: "bindings isolated", Code: `(let ((x 10)) (let ((x 1) (y x)) y))`, Expected: values.NewInteger(10)},
		{Name: "multiple body", Code: `(let ((x 1)) (+ x 1) (+ x 2))`, Expected: values.NewInteger(3)},
		{Name: "set! in body", Code: `(let ((x 1)) (set! x 2) x)`, Expected: values.NewInteger(2)},
		{Name: "closure capture", Code: `(let ((x 1)) (let ((f (lambda () x))) (f)))`, Expected: values.NewInteger(1)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestLetValidation_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "no args", Code: `(let)`},
		{Name: "no body", Code: `(let ((x 1)))`},
		{Name: "non-list bindings", Code: `(let "bad" 1)`},
		{Name: "flat bindings", Code: `(let (x 1) 2)`},
		{Name: "non-symbol name", Code: `(let ((1 2)) 3)`},
		{Name: "binding wrong arity", Code: `(let ((x)) 1)`},
		{Name: "binding too many", Code: `(let ((x 1 2)) 1)`},
		{Name: "improper binding pair", Code: `(let ((x . 1)) x)`},
		{Name: "improper bindings list", Code: `(let ((x 1) . y) x)`},
		{Name: "duplicate binding", Code: `(let ((x 1) (x 2)) x)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ============================================================================
// let*
// ============================================================================

func TestLetStarValidation(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "sequential visibility", Code: `(let* ((x 1) (y (+ x 1))) y)`, Expected: values.NewInteger(2)},
		{Name: "chain of three", Code: `(let* ((a 1) (b (+ a 1)) (c (+ b 1))) c)`, Expected: values.NewInteger(3)},
		{Name: "empty bindings", Code: `(let* () 42)`, Expected: values.NewInteger(42)},
		{Name: "set! in body", Code: `(let* ((x 1)) (set! x 2) x)`, Expected: values.NewInteger(2)},
		// Duplicate names: R7RS allows shadowing in let* (sequential binding)
		{Name: "duplicate shadow", Code: `(let* ((x 1) (x 2)) x)`, Expected: values.NewInteger(2)},
		{Name: "duplicate closure capture", Code: `(let* ((x 1) (f (lambda () x)) (x 2)) (f))`, Expected: values.NewInteger(1)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestLetStarValidation_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "no body", Code: `(let* ((x 1)))`},
		{Name: "non-symbol name", Code: `(let* ((1 2)) 3)`},
		{Name: "binding wrong arity", Code: `(let* ((x)) 1)`},
		{Name: "binding too many", Code: `(let* ((x 1 2)) 1)`},
		{Name: "improper binding pair", Code: `(let* ((x . 1)) x)`},
		{Name: "improper bindings list", Code: `(let* ((x 1) . y) x)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ============================================================================
// letrec
// ============================================================================

func TestLetrecValidation(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "recursive", Code: `(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))`, Expected: values.NewInteger(120)},
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

func TestLetrecValidation_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "no body", Code: `(letrec ((x 1)))`},
		{Name: "non-symbol name", Code: `(letrec ((1 2)) 3)`},
		{Name: "binding too many", Code: `(letrec ((x 1 2)) 1)`},
		{Name: "improper bindings list", Code: `(letrec ((x 1) . y) 1)`},
		{Name: "duplicate binding", Code: `(letrec ((x 1) (x 2)) x)`},
		{Name: "duplicate letrec*", Code: `(letrec* ((x 1) (x 2)) x)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ============================================================================
// letrec*
// ============================================================================

func TestLetrecStarValidation(t *testing.T) {
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

// ============================================================================
// named let
// ============================================================================

func TestNamedLetValidation(t *testing.T) {
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

func TestNamedLetValidation_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "no args", Code: `(let loop)`},
		{Name: "no body", Code: `(let loop ((x 1)))`},
		{Name: "non-list bindings", Code: `(let loop "bad" 1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ============================================================================
// LetKind.String
// ============================================================================

func TestLetKindString(t *testing.T) {
	c := qt.New(t)
	c.Assert(validate.LetKindLet.String(), qt.Equals, "let")
	c.Assert(validate.LetKindLetStar.String(), qt.Equals, "let*")
	c.Assert(validate.LetKindLetrec.String(), qt.Equals, "letrec")
	c.Assert(validate.LetKindLetrecStar.String(), qt.Equals, "letrec*")
	c.Assert(validate.LetKind(99).String(), qt.Equals, "let?")
}
