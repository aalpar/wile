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

// TestPatternLiteralUnderUseSiteScope pins R7RS §4.3.2 literal matching against
// the use-site scope every macro invocation now stamps on its input
// (compilation.newUseSiteScope).
//
// Why this file exists. `literalScopesMatchWithDef`
// (internal/match/syntax_adapter.go) reads `input.Scopes()` twice, and use-site
// scopes made that set strictly larger on every identifier that arrives through
// a macro use:
//
//   - `filterRebindingScopes(input.Scopes())` is provably unaffected — a
//     use-site scope is minted with NewScopeWithLabel, not
//     NewRebindingScopeWithLabel, so it is filtered out before the comparison.
//   - `checker.GetLiteralBinding(input.Key(), input.Scopes())` is a RESOLUTION,
//     and resolution is subset: a larger reference set admits strictly more
//     candidates, and the maximal-cardinality argmax can therefore land on a
//     different binding. That is the half a test has to cover.
//
// The rows that matter are the ones where resolution genuinely reaches a
// binding carrying a use-site scope — a `let` binder that arrived FROM the use
// site, which the expander's pruner deliberately leaves unpruned (only
// definitions are pruned, because only a definition escapes the macro use).
// Rows whose binder comes from the macro's own template do not test this: they
// are hygienically distinct from the use-site identifier and shadow nothing,
// which is what "outer template binder does not shadow" records below.
//
// Measured 2026-08-20 three ways — pre-use-site-scopes (52809866), post
// (a7c148da), and Chez 10 — unanimous on every row. This test is the pin, not
// a bug report.
func TestPatternLiteralUnderUseSiteScope(t *testing.T) {
	const inner = `(define-syntax inner
		(syntax-rules (lit) ((_ lit) 'matched) ((_ x) 'other)))`

	matched := values.NewSymbol("matched")
	other := values.NewSymbol("other")

	tcs := []testhelpers.SchemeCodeTestCase{
		{
			// The discriminating row. Both the `let` binder and the literal
			// occurrence arrive from the use site, so both wear the SAME use-site
			// scope and the binder shadows: `lit` resolves to the local, the
			// pattern literal resolves at inner's definition site, the two
			// bindings differ, no match.
			Name: "use-site binder shadows an inner macro's literal",
			Code: `(begin
				` + inner + `
				(define-syntax outer
					(syntax-rules () ((_ b y) (let ((b 9)) (inner y)))))
				(outer lit lit))`,
			Expected: other,
		},
		{
			// Same shadow reached through one identifier instead of two.
			Name: "use-site binder shadows the occurrence it also binds",
			Code: `(begin
				` + inner + `
				(define-syntax outer
					(syntax-rules () ((_ b) (let ((b 9)) (inner b)))))
				(outer lit))`,
			Expected: other,
		},
		{
			// The control that keeps the row above from passing for the wrong
			// reason. Here `lit` in the binder position comes from OUTER's
			// template, so it is macro-introduced and hygienically distinct from
			// the use-site `lit` in the body — it shadows nothing, and the
			// literal still matches.
			Name: "outer template binder does not shadow",
			Code: `(begin
				` + inner + `
				(define-syntax outer
					(syntax-rules () ((_ y) (let ((lit 9)) (inner y)))))
				(outer lit))`,
			Expected: matched,
		},
		{
			// The literal arrives from the use site and nothing shadows it.
			// Unbound on both sides, so R7RS §4.3.2's second clause applies.
			Name: "literal from the use site still matches",
			Code: `(begin
				` + inner + `
				(define-syntax outer (syntax-rules () ((_ y) (inner y))))
				(outer lit))`,
			Expected: matched,
		},
		{
			// Same, with the name bound globally: both sides resolve to the same
			// binding, so §4.3.2's first clause applies.
			Name: "globally bound literal from the use site still matches",
			Code: `(begin
				(define lit 1)
				` + inner + `
				(define-syntax outer (syntax-rules () ((_ y) (inner y))))
				(outer lit))`,
			Expected: matched,
		},
		{
			// The stdlib-critical shape, on the PINNED arm: cond's `=>` carries a
			// definition-site pin (LiteralPin), so this exercises
			// GetLiteralBinding rather than the unpinned both-sides comparison.
			// The use-site `=>` binder shadows, so `=>` is not the literal and
			// the clause body runs as an ordinary sequence yielding 'ok.
			Name: "use-site binder shadows cond's => (pinned arm)",
			Code: `(begin
				(define-syntax outer
					(syntax-rules () ((_ b body) (let ((b #f)) body))))
				(outer => (cond (#t => 'ok))))`,
			Expected: values.NewSymbol("ok"),
		},
		{
			// An auxiliary keyword reaching a macro's expansion unshadowed. The
			// `else` here is introduced by the macro's own template, so it wears
			// the introduction scope and no use-site scope at all.
			Name: "template-introduced else still matches",
			Code: `(begin
				(define-syntax m
					(syntax-rules () ((_ x) (cond (x 'yes) (else 'no)))))
				(list (m #t) (m #f)))`,
			Expected: values.List(values.NewSymbol("yes"), values.NewSymbol("no")),
		},
		{
			// The mirror: `else` arrives FROM the use site, wearing the use-site
			// scope, and must still be recognised as cond's literal.
			Name: "else from the use site still matches",
			Code: `(begin
				(define-syntax m
					(syntax-rules () ((_ e) (cond (#f 'a) (e 'b)))))
				(m else))`,
			Expected: values.NewSymbol("b"),
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
