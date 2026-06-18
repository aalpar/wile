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

// TestInlinePredicate exercises the inline candidate predicate in
// registerInlineCandidates (compile_let.go) by verifying that compiled
// programs produce correct results regardless of whether the predicate
// accepts or rejects a binding for inlining.
//
// The predicate accepts when all of:
//   - !Mutable (no set! on the binding)
//   - !Escapes (binding never appears in non-call position)
//   - Init is *ValidatedLambda (not case-lambda, not a non-lambda value)
//   - params.Rest == nil (non-variadic)
//   - len(body) <= inlineThreshold
func TestInlinePredicate(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Eligible: !Mutable && !Escapes && lambda && non-variadic && small body
		{
			Name:     "eligible simple identity",
			Code:     `(let ((f (lambda (x) x))) (f 42))`,
			Expected: values.NewInteger(42),
		},

		// NOT eligible: Mutable (set! on the binding)
		{
			Name:     "mutable blocks inline",
			Code:     `(let ((f (lambda (x) x))) (set! f (lambda (x) (+ x 1))) (f 42))`,
			Expected: values.NewInteger(43),
		},

		// NOT eligible: Escapes (used as init of a define, not in call position)
		{
			Name:     "escape blocks inline via define alias",
			Code:     `(let ((f (lambda (x) (+ x 1)))) (define g f) (g 5))`,
			Expected: values.NewInteger(6),
		},

		// NOT eligible: Escapes (passed as argument to map)
		{
			Name: "escape blocks inline passed as arg",
			Code: `(let ((f (lambda (x) (+ x 1)))) (map f '(1 2 3)))`,
			Expected: values.List(
				values.NewInteger(2),
				values.NewInteger(3),
				values.NewInteger(4),
			),
		},

		// NOT eligible: case-lambda produces ValidatedCaseLambda, not ValidatedLambda
		{
			Name:     "case-lambda not inlined",
			Code:     `(let ((f (case-lambda ((x) x) ((x y) (+ x y))))) (f 42))`,
			Expected: values.NewInteger(42),
		},

		// NOT eligible: variadic (params.Rest != nil)
		{
			Name:     "variadic not inlined",
			Code:     `(let ((f (lambda (x . rest) x))) (f 42 43))`,
			Expected: values.NewInteger(42),
		},

		// NOT eligible: non-lambda init (plain value, not a closure)
		{
			Name:     "non-lambda init not inlined",
			Code:     `(let ((x 42)) x)`,
			Expected: values.NewInteger(42),
		},

		// Mixed: one eligible binding (f) and one ineligible (g, variadic)
		{
			Name:     "mixed eligible and ineligible",
			Code:     `(let ((f (lambda (x) (+ x 1))) (g (lambda (x . rest) x))) (+ (f 1) (g 2 3)))`,
			Expected: values.NewInteger(4),
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
