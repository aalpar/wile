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

	"github.com/aalpar/wile/pkg/internal/match"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/registry/testhelpers"
)

// patternVarRatchetCase is one program and the two counts its compile and run
// must produce.
type patternVarRatchetCase struct {
	Name string
	Code string
	// Emits is compilation.TemplateLocalEmits: template occurrences resolved to
	// a local at compile time, the non-ellipsis path.
	//
	// It is TWICE the occurrence count, because a transformer body is compiled
	// twice: once by the expander when it meets the define-syntax, so later body
	// forms can use the macro, and again by the compiler. The "defined but never
	// used" row is what isolates that — one occurrence, two emits, no use.
	Emits uint64
	// Substitutions is match.PatternVarSubstitutions: template occurrences
	// replaced by a capture at expansion time, the ellipsis and syntax-rules path.
	//
	// Every row carries the same fixed harness cost, which the "baseline" row
	// reports on its own. Subtract it to read a row's own contribution. That
	// arrangement is deliberate: a change in the harness or the bootstrap Scheme
	// shifts every row by the same amount and is recognisable as such, while a
	// change in substitution behaviour moves particular rows.
	Substitutions uint64
}

// patternVarRatchetCorpus routes every shape of template identifier through both
// resolution paths. Each row's own contribution was hand-checked against the
// program: m3 substitutes a, c, c; m6 substitutes t twice (once per outer
// repetition) plus x three times; `outer` substitutes name and pat and must NOT
// substitute the inner x.
var patternVarRatchetCorpus = []patternVarRatchetCase{
	{
		// The harness floor. Contains no macro of its own, so its whole count is
		// what one RunSchemeCode costs before the row under test does anything.
		Name:          "baseline: no macro",
		Code:          `(+ 1 2)`,
		Emits:         0,
		Substitutions: 79,
	},
	{
		// One template occurrence, never used. Isolates the doubling to macro
		// DEFINITION rather than to expansion.
		Name: "compile-time: defined but never used",
		Code: `(begin
			(define-syntax m0 (lambda (stx) (syntax-case stx () ((_ a) (syntax (list a))))))
			42)`,
		Emits:         2,
		Substitutions: 79,
	},
	{
		// No ellipsis, so compileSyntaxTemplateToOps emits the template.
		Name: "compile-time: two variables",
		Code: `(begin
			(define-syntax m1 (lambda (stx) (syntax-case stx () ((_ a b) (syntax (list a b))))))
			(m1 1 2))`,
		Emits:         4,
		Substitutions: 79,
	},
	{
		// One variable used twice, so the count is per OCCURRENCE, not per variable.
		Name: "compile-time: one variable used twice",
		Code: `(begin
			(define-syntax m2 (lambda (stx) (syntax-case stx () ((_ a) (syntax (list a a))))))
			(m2 3))`,
		Emits:         4,
		Substitutions: 79,
	},
	{
		// One ellipsis anywhere routes the WHOLE template through the expander, so
		// the depth-0 `a` is substituted there too rather than emitted.
		Name: "runtime: ellipsis template",
		Code: `(begin
			(define-syntax m3 (lambda (stx) (syntax-case stx () ((_ a c ...) (syntax (list a c ...))))))
			(m3 1 2 3))`,
		Emits:         0,
		Substitutions: 82,
	},
	{
		// The shape set equality refused until 2026-08-20, and the shape a use-site
		// scope must not start refusing. Same counts as the row above: the clause
		// body's `let` changes the scopes, not the substitutions.
		Name: "runtime: ellipsis template under a clause-body let",
		Code: `(begin
			(define-syntax m4
				(lambda (stx) (syntax-case stx () ((_ a c ...) (let ((p 1)) (syntax (list a c ...)))))))
			(m4 1 2 3))`,
		Emits:         0,
		Substitutions: 82,
	},
	{
		// syntax-rules reaches the runtime expander with no binder allowance, so
		// its gate is set equality even after 2026-08-20.
		Name: "runtime: syntax-rules",
		Code: `(begin
			(define-syntax m5 (syntax-rules () ((_ a b) (list a b))))
			(m5 1 2))`,
		Emits:         0,
		Substitutions: 81,
	},
	{
		// Nested ellipsis, so a broadcast variable is counted once per repetition
		// it is replicated into (R7RS §4.3.2), not once per template occurrence.
		Name: "runtime: nested ellipsis with a broadcast variable",
		Code: `(begin
			(define-syntax m6 (syntax-rules () ((_ t ((x ...) ...)) (list (list t x ...) ...))))
			(m6 0 ((1 2) (3))))`,
		Emits:         0,
		Substitutions: 84,
	},
	{
		// A macro-generating macro. The inner PATTERN comes from the use site and
		// the inner TEMPLATE from outer's template, so the inner `x` must NOT
		// substitute: the 2 above baseline are outer's own `name` and `pat`. If a
		// change starts capturing here this row goes to 82, which is the reading a
		// value assertion gives only when the escaping name collides with a global.
		Name: "capture: outer's introduced x is not the inner pattern variable",
		Code: `(begin
			(define x 99)
			(define-syntax outer
				(syntax-rules () ((_ name pat) (define-syntax name (syntax-rules () (pat (list x)))))))
			(outer inner (_ x))
			(inner 5))`,
		Emits:         0,
		Substitutions: 81,
	},
}

// TestPatternVarResolutionSiteCount is the deopt ratchet for the use-site-scope
// arc (plans/2026-08-20-use-site-scopes-impl.local.md, phase 0). Every row must
// be UNCHANGED by every phase of that arc.
//
// Why a count rather than the value assertions that already cover these
// programs: both failure directions of the substitution gate are silent when the
// escaping name collides with a binding of the same spelling. A pattern variable
// that stops substituting falls through to free-identifier hygiene, and a
// template saying `(list x)` still evaluates, to the global `x`. That is the
// 2026-08-20 capture defect read backwards, and it is how `(inner 5)` returned
// `(5)` instead of `(99)` while every value assertion in the tree stayed green.
//
// The two counts are separate because the two paths have failed independently. A
// template with an ellipsis is expanded at run time and one without is emitted at
// compile time; the 2026-08-20 defect was fixed on the first while remaining live
// on the second for as long as it took to notice that `AllScopes` made the second
// blind to scopes entirely.
//
// A movement is not automatically a regression, but it is always a decision.
// Re-pin in the same commit that causes it and name the rows that moved.
func TestPatternVarResolutionSiteCount(t *testing.T) {
	for _, tc := range patternVarRatchetCorpus {
		t.Run(tc.Name, func(t *testing.T) {
			// Snapshot and diff rather than read absolutely: the counters are
			// process-global, so their value at entry depends on whatever ran
			// before in this binary. Neither this package nor pkg/internal/match
			// calls t.Parallel(), so nothing runs concurrently to inflate a diff.
			emitsBefore := compilation.TemplateLocalEmits()
			substitutionsBefore := match.PatternVarSubstitutions()

			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)

			qt.Assert(t, compilation.TemplateLocalEmits()-emitsBefore, qt.Equals, tc.Emits,
				qt.Commentf("compile-time template occurrences resolved to a local"))
			qt.Assert(t, match.PatternVarSubstitutions()-substitutionsBefore, qt.Equals, tc.Substitutions,
				qt.Commentf("runtime template occurrences replaced by a capture"))
		})
	}
}
