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

// TestWithSyntax exercises CompileWithSyntax, compileWithSyntaxBody, and
// buildWithSyntaxBegin through Scheme-level macros that use with-syntax
// inside define-syntax + lambda + syntax-case transformers.
func TestWithSyntax(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "single binding",
			Code: `(begin
				(define-syntax my-const
					(lambda (stx)
						(syntax-case stx ()
							((_ val)
							 (with-syntax ((result (syntax val)))
								(syntax result))))))
				(my-const 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "empty bindings passes through to body",
			Code: `(begin
				(define-syntax just-body
					(lambda (stx)
						(syntax-case stx ()
							((_ x)
							 (with-syntax ()
								(syntax x))))))
				(just-body 99))`,
			Expected: values.NewInteger(99),
		},
		{
			Name: "empty bindings with arithmetic body",
			Code: `(begin
				(define-syntax add-direct
					(lambda (stx)
						(syntax-case stx ()
							((_ x y)
							 (with-syntax ()
								(syntax (+ x y)))))))
				(add-direct 3 4))`,
			Expected: values.NewInteger(7),
		},
		{
			Name: "two bindings returning last",
			Code: `(begin
				(define-syntax ws-last
					(lambda (stx)
						(syntax-case stx ()
							((_ x)
							 (with-syntax ((a (syntax (+ x 1)))
							               (b (syntax (+ x 2))))
								(syntax b))))))
				(ws-last 10))`,
			Expected: values.NewInteger(12),
		},
		{
			Name: "two bindings returning second",
			Code: `(begin
				(define-syntax ws-second
					(lambda (stx)
						(syntax-case stx ()
							((_ x)
							 (with-syntax ((a (syntax (+ x 10)))
							               (b (syntax (+ x 20))))
								(syntax b))))))
				(ws-second 5))`,
			Expected: values.NewInteger(25),
		},
		{
			Name: "three bindings returning last",
			Code: `(begin
				(define-syntax ws-three
					(lambda (stx)
						(syntax-case stx ()
							((_ x)
							 (with-syntax ((a (syntax 1))
							               (b (syntax 2))
							               (c (syntax (+ x 100))))
								(syntax c))))))
				(ws-three 5))`,
			Expected: values.NewInteger(105),
		},
		{
			Name: "three bindings returning middle",
			Code: `(begin
				(define-syntax ws-mid
					(lambda (stx)
						(syntax-case stx ()
							((_ x)
							 (with-syntax ((a (syntax 1))
							               (b (syntax (* x 3)))
							               (c (syntax 99)))
								(syntax b))))))
				(ws-mid 7))`,
			Expected: values.NewInteger(21),
		},
		{
			Name: "multiple body expressions returns last",
			Code: `(begin
				(define-syntax ws-multi-body
					(lambda (stx)
						(syntax-case stx ()
							((_ x)
							 (with-syntax ((a (syntax (+ x 1)))
							               (b (syntax (+ x 2))))
								(syntax a)
								(syntax b))))))
				(ws-multi-body 10))`,
			Expected: values.NewInteger(12),
		},
		{
			Name: "empty bindings with multiple body expressions",
			Code: `(begin
				(define-syntax ws-empty-multi
					(lambda (stx)
						(syntax-case stx ()
							((_ x y)
							 (with-syntax ()
								(syntax x)
								(syntax (+ x y)))))))
				(ws-empty-multi 3 4))`,
			Expected: values.NewInteger(7),
		},
		{
			Name: "produces list expression",
			Code: `(begin
				(define-syntax ws-swap
					(lambda (stx)
						(syntax-case stx ()
							((_ a b)
							 (with-syntax ((dummy (syntax 0))
							               (swapped (syntax (list b a))))
								(syntax swapped))))))
				(ws-swap 1 2))`,
			Expected: values.List(
				values.NewInteger(2),
				values.NewInteger(1),
			),
		},
		{
			Name: "nested with-syntax",
			Code: `(begin
				(define-syntax ws-nested
					(lambda (stx)
						(syntax-case stx ()
							((_ x)
							 (with-syntax ((d1 (syntax 0))
							               (inner (syntax x)))
								(with-syntax ((d2 (syntax 0))
								              (result (syntax (+ inner 100))))
									(syntax result)))))))
				(ws-nested 5))`,
			Expected: values.NewInteger(105),
		},
		{
			Name: "with-syntax produces define form",
			Code: `(begin
				(define-syntax def-doubled
					(lambda (stx)
						(syntax-case stx ()
							((_ name val)
							 (with-syntax ((d (syntax 0))
							               (expr (syntax (* val 2))))
								(syntax (define name expr)))))))
				(def-doubled my-var 21)
				my-var)`,
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

// TestWithSyntaxErrors exercises error paths in CompileWithSyntax.
func TestWithSyntaxErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "no body expressions",
			Code: `(begin
				(define-syntax bad-no-body
					(lambda (stx)
						(syntax-case stx ()
							((_)
							 (with-syntax ((x (syntax 1))
							               (y (syntax 2))))))))
				(bad-no-body))`,
		},
		{
			Name: "malformed binding not a list",
			Code: `(begin
				(define-syntax bad-binding
					(lambda (stx)
						(syntax-case stx ()
							((_)
							 (with-syntax (x)
								(syntax 1))))))
				(bad-binding))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			err := testhelpers.RunSchemeCodeExpectError(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
