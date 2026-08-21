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

// TestSyntaxCaseEllipsisTemplateUnderBodyBinder pins that an ellipsis template
// still substitutes its pattern variables when the (syntax ...) form sits under
// a binding form in the clause body.
//
// A clause body's `let`/`lambda`/`do` adds its own scope to every identifier
// beneath it, the template's included, while the pattern symbols
// compileSyntaxCaseClause recorded carry only the transformer lambda's. The
// template occurrence is therefore a strict SUPERSET of its pattern variable.
// The gate used to demand set equality, which every one of these fails: the
// pattern variable was refused, fell through to free-identifier hygiene, and
// escaped into the expansion as an unbound name (`no such binding "a"`).
//
// What makes the superset admissible is not the superset relation itself — an
// outer macro's introduced identifier is a superset of a pattern variable too,
// and must NOT substitute (TestNestedMacroDoesNotCaptureOuterIntroduction). It
// is that the clause's expander logged those exact scopes as ones a binder
// inside the body minted. See match.TemplateDenotesPatternVariable.
//
// Only the ellipsis path routes through the runtime gate; a template without
// ellipsis is emitted by compileSyntaxTemplateToOps, which applies the same
// predicate at compile time. The last two cases are kept as controls. Chez
// agrees with every row.
func TestSyntaxCaseEllipsisTemplateUnderBodyBinder(t *testing.T) {
	oneTwoThree := values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			// The reported shape: `a` is not even the ellipsis variable, but one
			// ellipsis anywhere in the template routes the WHOLE template through
			// the runtime expander, so every pattern variable in it is refused.
			Name: "let: depth-0 and ellipsis variables both substitute",
			Code: `(begin
				(define-syntax m
					(lambda (stx)
						(syntax-case stx ()
							((_ a c ...) (let ((p 1)) (syntax (list a c ...)))))))
				(m 1 2 3))`,
			Expected: oneTwoThree,
		},
		{
			Name: "let: ellipsis variable alone",
			Code: `(begin
				(define-syntax m
					(lambda (stx)
						(syntax-case stx ()
							((_ a c ...) (let ((p 1)) (syntax (list c ...)))))))
				(m 1 2 3))`,
			Expected: values.List(values.NewInteger(2), values.NewInteger(3)),
		},
		{
			Name: "let*",
			Code: `(begin
				(define-syntax m
					(lambda (stx)
						(syntax-case stx ()
							((_ a c ...) (let* ((p 1)) (syntax (list a c ...)))))))
				(m 1 2 3))`,
			Expected: oneTwoThree,
		},
		{
			Name: "letrec",
			Code: `(begin
				(define-syntax m
					(lambda (stx)
						(syntax-case stx ()
							((_ a c ...) (letrec ((p 1)) (syntax (list a c ...)))))))
				(m 1 2 3))`,
			Expected: oneTwoThree,
		},
		{
			Name: "named let",
			Code: `(begin
				(define-syntax m
					(lambda (stx)
						(syntax-case stx ()
							((_ a c ...) (let loop ((p 1)) (syntax (list a c ...)))))))
				(m 1 2 3))`,
			Expected: oneTwoThree,
		},
		{
			Name: "lambda",
			Code: `(begin
				(define-syntax m
					(lambda (stx)
						(syntax-case stx ()
							((_ a c ...) ((lambda (p) (syntax (list a c ...))) 1)))))
				(m 1 2 3))`,
			Expected: oneTwoThree,
		},
		{
			Name: "do",
			Code: `(begin
				(define-syntax m
					(lambda (stx)
						(syntax-case stx ()
							((_ a c ...) (do ((p 1 2)) (#t (syntax (list a c ...))))))))
				(m 1 2 3))`,
			Expected: oneTwoThree,
		},
		{
			// Two nested binders stack two scopes, so the delta is not a single
			// scope and cannot be excused as an off-by-one.
			Name: "nested let",
			Code: `(begin
				(define-syntax m
					(lambda (stx)
						(syntax-case stx ()
							((_ a c ...) (let ((p 1)) (let ((q 2)) (syntax (list a c ...))))))))
				(m 1 2 3))`,
			Expected: oneTwoThree,
		},
		{
			// begin binds nothing, so it adds no scope: the control that isolates
			// the binder as the discriminator rather than the extra nesting.
			Name: "control: begin adds no scope",
			Code: `(begin
				(define-syntax m
					(lambda (stx)
						(syntax-case stx ()
							((_ a c ...) (begin (syntax (list a c ...)))))))
				(m 1 2 3))`,
			Expected: oneTwoThree,
		},
		{
			// The non-ellipsis control: same let, same scopes, resolved through
			// the environment instead of the gate.
			Name: "control: non-ellipsis template under let",
			Code: `(begin
				(define-syntax m
					(lambda (stx)
						(syntax-case stx ()
							((_ a c) (let ((p 1)) (syntax (list a c)))))))
				(m 1 2))`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2)),
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

// TestNestedMacroDoesNotCaptureOuterIntroduction pins that an identifier an
// OUTER macro introduces into the template of a macro it generates is not
// captured by that inner macro's same-named pattern variable.
//
// A macro-generating macro splices the inner macro's pattern in from its own use
// site while writing the inner macro's template itself, so the two `x` here have
// different provenance: the pattern's is the user's, the template's is outer's.
// R7RS §4.3 fixes the template one to outer's definition site — the global 99 —
// and nothing the inner pattern spells can take it back. Chez and Racket both
// answer (99) on every row; Wile answered (5) on the syntax-case rows.
//
// The scope sets alone cannot decide this. The template `x` is a strict superset
// of the pattern `x` in exactly the way a legitimate reference under a clause
// body's `let` is (TestSyntaxCaseEllipsisTemplateUnderBodyBinder), so a subset
// test admits the capture and set equality refuses the `let`. What separates
// them is whether a binder inside THIS clause body minted the extra scope; see
// match.TemplateDenotesPatternVariable.
//
// The four rows cross the two transformer kinds because they reach the gate by
// different routes: syntax-rules through the runtime expander with no binder
// allowance, syntax-case through compileSyntaxTemplateToOps, which before this
// resolved the template symbol by NAME under a wildcard scope query and so could
// not see the difference at all.
func TestNestedMacroDoesNotCaptureOuterIntroduction(t *testing.T) {
	ninetyNine := values.List(values.NewInteger(99))
	srInner := `(syntax-rules () (pat (list x)))`
	scInner := `(lambda (s2) (syntax-case s2 () (pat (syntax (list x)))))`
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "syntax-rules generates syntax-rules",
			Code: `(begin
				(define x 99)
				(define-syntax outer
					(syntax-rules ()
						((_ name pat) (define-syntax name ` + srInner + `))))
				(outer inner (_ x))
				(inner 5))`,
			Expected: ninetyNine,
		},
		{
			Name: "syntax-rules generates syntax-case",
			Code: `(begin
				(define x 99)
				(define-syntax outer
					(syntax-rules ()
						((_ name pat) (define-syntax name ` + scInner + `))))
				(outer inner (_ x))
				(inner 5))`,
			Expected: ninetyNine,
		},
		{
			Name: "syntax-case generates syntax-rules",
			Code: `(begin
				(define x 99)
				(define-syntax outer
					(lambda (stx)
						(syntax-case stx ()
							((_ name pat) (syntax (define-syntax name ` + srInner + `))))))
				(outer inner (_ x))
				(inner 5))`,
			Expected: ninetyNine,
		},
		{
			Name: "syntax-case generates syntax-case",
			Code: `(begin
				(define x 99)
				(define-syntax outer
					(lambda (stx)
						(syntax-case stx ()
							((_ name pat) (syntax (define-syntax name ` + scInner + `))))))
				(outer inner (_ x))
				(inner 5))`,
			Expected: ninetyNine,
		},
		{
			// The control that keeps the fix from being "refuse every superset":
			// written by hand, the pattern and the template share provenance, so
			// the substitution is the ordinary one and must still happen.
			Name: "control: hand-written inner macro still substitutes",
			Code: `(begin
				(define x 99)
				(define-syntax inner (syntax-rules () ((_ x) (list x))))
				(inner 5))`,
			Expected: values.List(values.NewInteger(5)),
		},
		{
			// The other control, and the one that shows provenance rather than
			// generation is the discriminator: here the inner PATTERN is written by
			// outer too, so both `v` occurrences carry outer's scope and agree.
			Name: "control: generated pattern and template agree",
			Code: `(begin
				(define-syntax outer
					(syntax-rules ()
						((_ name)
						 (define-syntax name (syntax-rules () ((_ v) (list v v)))))))
				(outer inner)
				(inner 4))`,
			Expected: values.List(values.NewInteger(4), values.NewInteger(4)),
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
