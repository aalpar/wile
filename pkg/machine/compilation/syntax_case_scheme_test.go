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

func TestSyntaxCaseHappyPaths(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "single clause with pattern variable",
			Code: `(begin
				(define-syntax add1
					(lambda (stx)
						(syntax-case stx ()
							((_ x) (syntax (+ x 1))))))
				(add1 10))`,
			Expected: values.NewInteger(11),
		},
		{
			Name: "multiple clauses first matches",
			Code: `(begin
				(define-syntax my-op
					(lambda (stx)
						(syntax-case stx ()
							((_ x) (syntax (+ x 100)))
							((_ x y) (syntax (+ x y))))))
				(my-op 5))`,
			Expected: values.NewInteger(105),
		},
		{
			Name: "multiple clauses second matches",
			Code: `(begin
				(define-syntax my-op
					(lambda (stx)
						(syntax-case stx ()
							((_ x) (syntax (+ x 100)))
							((_ x y) (syntax (+ x y))))))
				(my-op 3 4))`,
			Expected: values.NewInteger(7),
		},
		{
			Name: "fender true selects first clause",
			Code: `(begin
				(define-syntax check-positive
					(lambda (stx)
						(syntax-case stx ()
							((_ x) (positive? (syntax->datum (syntax x)))
								(syntax 'positive))
							((_ x)
								(syntax 'non-positive)))))
				(check-positive 5))`,
			Expected: values.NewSymbol("positive"),
		},
		{
			Name: "fender false falls through to next clause",
			Code: `(begin
				(define-syntax check-positive
					(lambda (stx)
						(syntax-case stx ()
							((_ x) (positive? (syntax->datum (syntax x)))
								(syntax 'positive))
							((_ x)
								(syntax 'non-positive)))))
				(check-positive -3))`,
			Expected: values.NewSymbol("non-positive"),
		},
		{
			Name: "literals list matching",
			Code: `(begin
				(define-syntax my-cond
					(lambda (stx)
						(syntax-case stx (=>)
							((_ test => proc) (syntax (proc test)))
							((_ test body) (syntax (if test body #f))))))
				(my-cond 42 => (lambda (x) (+ x 1))))`,
			Expected: values.NewInteger(43),
		},
		{
			Name: "hash-syntax shorthand",
			Code: `(begin
				(define-syntax double
					(lambda (stx)
						(syntax-case stx ()
							((_ x) #'(+ x x)))))
				(double 21))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "multiple pattern variables",
			Code: `(begin
				(define-syntax swap-pair
					(lambda (stx)
						(syntax-case stx ()
							((_ a b) (syntax (list b a))))))
				(swap-pair 1 2))`,
			Expected: values.List(values.NewInteger(2), values.NewInteger(1)),
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

func TestSyntaxCaseErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "no matching clause",
			Code: `(begin
				(define-syntax strict-match
					(lambda (stx)
						(syntax-case stx ()
							((_ x y) (syntax (+ x y))))))
				(strict-match 1 2 3))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestSyntaxCaseEllipsisHygiene checks that templates containing ellipsis are
// expanded hygienically (R7RS §4.3), matching the already-correct non-ellipsis
// path. Two failure modes:
//   - capture: a template-introduced binder must not swallow user identifiers
//     that arrive through an ellipsis pattern variable.
//   - referential transparency: a free template identifier must resolve at the
//     macro definition site, not the use site.
//
// Each ellipsis case is paired with its non-ellipsis control, which exercises
// the compile-time template path (compileSyntaxTemplateToOps) and was already
// hygienic — the fix makes the runtime ellipsis path (OperationSyntaxTemplateExpand)
// behave identically.
func TestSyntaxCaseEllipsisHygiene(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			// Introduced `tmp`=0 must not capture the user's `tmp`=99 spliced via `x ...`.
			Name: "capture: ellipsis-spliced ids not captured by introduced binder",
			Code: `(begin
				(define-syntax m
					(lambda (stx)
						(syntax-case stx ()
							((_ (x ...)) (syntax (let ((tmp 0)) (list tmp x ...)))))))
				(let ((tmp 99)) (m (tmp tmp))))`,
			Expected: values.List(values.NewInteger(0), values.NewInteger(99), values.NewInteger(99)),
		},
		{
			Name: "capture control: non-ellipsis sibling is hygienic",
			Code: `(begin
				(define-syntax mc
					(lambda (stx)
						(syntax-case stx ()
							((_ a b) (syntax (let ((tmp 0)) (list tmp a b)))))))
				(let ((tmp 99)) (mc tmp tmp)))`,
			Expected: values.List(values.NewInteger(0), values.NewInteger(99), values.NewInteger(99)),
		},
		{
			// Free template id `list` must resolve to the definition-site global,
			// not the use-site shadowing binding.
			Name: "referential transparency: free id resolves at definition site (ellipsis)",
			Code: `(begin
				(define-syntax m2
					(lambda (stx)
						(syntax-case stx ()
							((_ (x ...)) (syntax (list x ...))))))
				(let ((list (lambda args 'shadowed))) (m2 (1 2 3))))`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			Name: "referential transparency control: non-ellipsis sibling",
			Code: `(begin
				(define-syntax m2c
					(lambda (stx)
						(syntax-case stx ()
							((_ a b) (syntax (list a b))))))
				(let ((list (lambda args 'shadowed))) (m2c 1 2)))`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			// Nested ellipsis must remain hygienic at every depth.
			Name: "capture: nested ellipsis not captured",
			Code: `(begin
				(define-syntax mn
					(lambda (stx)
						(syntax-case stx ()
							((_ ((x ...) ...)) (syntax (let ((tmp 0)) (list tmp (list x ...) ...)))))))
				(let ((tmp 99)) (mn ((tmp tmp) (tmp)))))`,
			Expected: values.List(
				values.NewInteger(0),
				values.List(values.NewInteger(99), values.NewInteger(99)),
				values.List(values.NewInteger(99)),
			),
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
