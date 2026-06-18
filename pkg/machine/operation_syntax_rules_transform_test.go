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

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestSyntaxRulesTransform tests the runtime behavior of syntax-rules
// pattern matching and template expansion via Scheme-level tests.
func TestSyntaxRulesTransform(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "simple identity macro",
			Code: `(begin
			  (define-syntax my-id
			    (syntax-rules ()
			      ((_ x) x)))
			  (my-id 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "simple rewrite to if",
			Code: `(begin
			  (define-syntax my-if
			    (syntax-rules ()
			      ((_ test then else)
			       (if test then else))))
			  (my-if #t 'yes 'no))`,
			Expected: values.NewSymbol("yes"),
		},
		{
			Name: "my-if false branch",
			Code: `(begin
			  (define-syntax my-if
			    (syntax-rules ()
			      ((_ test then else)
			       (if test then else))))
			  (my-if #f 'yes 'no))`,
			Expected: values.NewSymbol("no"),
		},
		{
			Name: "ellipsis collects elements into list",
			Code: `(begin
			  (define-syntax my-list
			    (syntax-rules ()
			      ((_ x ...) (list x ...))))
			  (my-list 1 2 3))`,
			Expected: values.List(
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
			),
		},
		{
			Name: "ellipsis with zero elements",
			Code: `(begin
			  (define-syntax my-list
			    (syntax-rules ()
			      ((_ x ...) (list x ...))))
			  (my-list))`,
			Expected: values.EmptyList,
		},
		{
			Name: "multiple clauses first match wins",
			Code: `(begin
			  (define-syntax my-match
			    (syntax-rules ()
			      ((_ a) 'one)
			      ((_ a b) 'two)
			      ((_ a b c) 'three)))
			  (my-match x y))`,
			Expected: values.NewSymbol("two"),
		},
		{
			Name: "multiple clauses single arg",
			Code: `(begin
			  (define-syntax my-match
			    (syntax-rules ()
			      ((_ a) 'one)
			      ((_ a b) 'two)
			      ((_ a b c) 'three)))
			  (my-match x))`,
			Expected: values.NewSymbol("one"),
		},
		{
			Name: "multiple clauses three args",
			Code: `(begin
			  (define-syntax my-match
			    (syntax-rules ()
			      ((_ a) 'one)
			      ((_ a b) 'two)
			      ((_ a b c) 'three)))
			  (my-match x y z))`,
			Expected: values.NewSymbol("three"),
		},
		{
			Name: "pattern variable in nested template",
			Code: `(begin
			  (define-syntax my-pair
			    (syntax-rules ()
			      ((_ a b) (cons a b))))
			  (my-pair 1 2))`,
			Expected: values.NewCons(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			Name: "underscore as wildcard",
			Code: `(begin
			  (define-syntax my-second
			    (syntax-rules ()
			      ((_ _ x) x)))
			  (my-second ignored 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "nested pattern matching",
			Code: `(begin
			  (define-syntax my-swap
			    (syntax-rules ()
			      ((_ (a b)) (list b a))))
			  (my-swap (1 2)))`,
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

// TestSyntaxRulesTransformLiterals tests literal matching in syntax-rules.
func TestSyntaxRulesTransformLiterals(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "literal matching with else",
			Code: `(begin
			  (define-syntax my-cond
			    (syntax-rules (else)
			      ((_ (else e)) e)
			      ((_ (t e) rest ...)
			       (if t e (my-cond rest ...)))))
			  (my-cond (else 42)))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "literal matching falls through to else clause",
			Code: `(begin
			  (define-syntax my-cond
			    (syntax-rules (else)
			      ((_ (else e)) e)
			      ((_ (t e) rest ...)
			       (if t e (my-cond rest ...)))))
			  (my-cond (#f 1) (else 2)))`,
			Expected: values.NewInteger(2),
		},
		{
			Name: "literal matching true clause matched",
			Code: `(begin
			  (define-syntax my-cond
			    (syntax-rules (else)
			      ((_ (else e)) e)
			      ((_ (t e) rest ...)
			       (if t e (my-cond rest ...)))))
			  (my-cond (#t 99) (else 0)))`,
			Expected: values.NewInteger(99),
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

// TestSyntaxRulesTransformHygiene tests that syntax-rules macros maintain hygiene.
func TestSyntaxRulesTransformHygiene(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "swap macro does not capture user tmp",
			Code: `(begin
			  (define-syntax my-swap!
			    (syntax-rules ()
			      ((_ a b)
			       (let ((tmp a))
			         (set! a b)
			         (set! b tmp)))))
			  (define tmp 100)
			  (define x 1)
			  (define y 2)
			  (my-swap! x y)
			  tmp)`,
			Expected: values.NewInteger(100),
		},
		{
			Name: "swap macro swaps correctly",
			Code: `(begin
			  (define-syntax my-swap!
			    (syntax-rules ()
			      ((_ a b)
			       (let ((tmp a))
			         (set! a b)
			         (set! b tmp)))))
			  (define x 1)
			  (define y 2)
			  (my-swap! x y)
			  (+ x y))`,
			// x was 1, y was 2; after swap x=2, y=1; sum is 3
			Expected: values.NewInteger(3),
		},
		{
			Name: "or macro returns first truthy value",
			Code: `(begin
			  (define-syntax my-or
			    (syntax-rules ()
			      ((_) #f)
			      ((_ e) e)
			      ((_ e1 e2 ...)
			       (let ((t e1))
			         (if t t (my-or e2 ...))))))
			  (my-or #f #f 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "or macro returns first truthy among multiple",
			Code: `(begin
			  (define-syntax my-or
			    (syntax-rules ()
			      ((_) #f)
			      ((_ e) e)
			      ((_ e1 e2 ...)
			       (let ((t e1))
			         (if t t (my-or e2 ...))))))
			  (my-or #f 7 42))`,
			Expected: values.NewInteger(7),
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

// TestSyntaxRulesTransformErrors tests error cases in syntax-rules expansion.
func TestSyntaxRulesTransformErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "no matching clause",
			Code: `(begin
			  (define-syntax my-fixed
			    (syntax-rules ()
			      ((_ a b) (+ a b))))
			  (my-fixed 1 2 3))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
