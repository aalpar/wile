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

package wile_test

import (
	"context"
	"sort"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"
)

// phase1BasePrologue is the whole variable under test. Every row below runs its
// body twice — without this line and with it — and asserts the two answers are
// EQUAL. The import supplies names; it must not move anything else.
const phase1BasePrologue = "(import (for-syntax (scheme base)))\n"

// TestPhase1BaseImportIsBehaviourNeutral pins the defect filed 2026-09-09: a
// phase-1 (scheme base) import changed four observable things that have nothing
// to do with the names it supplies.
//
// One cause, two readers. (scheme base) exports 21 of the phase-1 machinery's
// names as value-less compile-time keywords, and a phase-shifted import installs
// them at (phase 1, MUTABLE) — installImportedBinding's shadowable arm is
// guarded on PhaseRuntime — where tierExactMutable out-ranks the (phase 1,
// SEALED) rows the primitive expanders and bootstrap macros live at. Both
// candidates carry the empty scope set, so the import wins every top-level
// query. LookupPhaseBinding and lookupMacroBinding then FAILED CLOSED on the
// wrong-typed winner instead of probing the tier the row was written at, and
// `let`, `quote` and friends stopped being core forms.
//
// (scheme cxr) at phase 1 moves NOTHING — it shares no name with the machinery —
// which is what says the trigger is the collision set rather than for-syntax
// imports as such. The auxiliary-keyword reading (else, =>, _, ...) is refuted:
// (only (scheme base) let) reproduces rows 1-5 alone and (only (scheme base) else)
// reproduces none.
//
// Every row is RED before the two-site fix and asserts EQUALITY, not a constant,
// so it cannot be satisfied by making both arms equally wrong without also
// contradicting `want`.
func TestPhase1BaseImportIsBehaviourNeutral(t *testing.T) {
	tests := []struct {
		name string
		body string
		want string
	}{
		{
			// The masked form is `let`: with it demoted to an ordinary call, no
			// let scope is minted, the use-site binder's scope set is empty, and
			// ScopesCompatible([], {intro}) passes where {let} ⊄ {intro} refused.
			name: "free-identifier=? sees the use-site shadow",
			body: `(define-syntax m
  (lambda (stx)
    (let ((f (syntax->list stx)))
      (if (free-identifier=? (car (cdr f)) (quote-syntax else)) #''else #''not))))
(list (m else) (let ((else 1)) (m else)))`,
			want: "(else not)",
		},
		{
			// The same arithmetic, reached through the syntax-rules literal
			// matcher instead of the primitive: this is not a free-identifier=?
			// bug, and the row exists to say so.
			name: "a syntax-rules literal sees the use-site shadow",
			body: `(define-syntax m3 (syntax-rules (else) ((_ else) 'kw) ((_ x) 'notkw)))
(list (m3 else) (let ((else 1)) (m3 else)))`,
			want: "(kw notkw)",
		},
		{
			// The honest subject of the expand-once column. With `quote` masked,
			// the expander walks INTO quoted data and macro-expands it.
			name: "quote does not expand its datum",
			body: `(quote (when #t 1))`,
			want: "(when #t 1)",
		},
		{
			name: "a quoted keyword survives as a datum",
			body: `(car (quote (when)))`,
			want: "when",
		},
		{
			// Downstream of the row above, and the shape the defect was filed
			// under. expand-once's second value reports whether expansion
			// occurred; it flips to #f because its argument was already expanded
			// at compile time, so the value is honest and `quote` is the defect.
			name: "expand-once still has something to expand",
			body: `(call-with-values
  (lambda () (expand-once (datum->syntax #f '(when #t 1))))
  (lambda (form expanded) expanded))`,
			want: "#t",
		},
		{
			// TestP2_ERRenameResolvesAtDefinitionSite's contract, at top level.
			// resolveRenamedSymbol's own output is byte-identical in both arms —
			// the capture is the empty-scoped use-site binder above, not a moved pin.
			name: "an ER rename resolves at the definition site",
			body: `(define (helper) 'def)
(define-syntax er-h (er-macro-transformer (lambda (f r c) (list (r 'helper)))))
(let ((helper (lambda () 'use))) (er-h))`,
			want: "def",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			bare := evalSyntaxForms(t, tc.body, withStdlibResolver()...)
			c.Assert(bare, qt.Equals, tc.want)
			imported := evalSyntaxForms(t, phase1BasePrologue+tc.body, withStdlibResolver()...)
			c.Assert(imported, qt.Equals, bare)
		})
	}
}

// TestPhase1BaseImportMasksNoPhaseRow is the site-count ratchet, and it is
// white-box on purpose. The obvious black-box form — run (let ((x 1)) x) and
// friends after the import and assert the values — PASSES ON MASTER: a masked
// head still reaches the COMPILER's own syntax-form dispatch, so the answer is
// right and only the EXPANDER stopped seeing the form. It would pin an answer,
// not the change.
//
// So ask the reader directly, and derive the name set from the store rather than
// listing it: every name whose sealed phase-1 row LookupPrimitiveExpander
// answers for before the import must still answer after it. That makes the
// ratchet count SITES — a fix narrowed to the one name someone measured leaves
// the other twenty red, and the failure names them.
func TestPhase1BaseImportMasksNoPhaseRow(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, append([]wile.EngineOption{wile.WithProfile(wile.KitchenSink)},
		withStdlibResolver()...)...)
	c.Assert(err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()

	// The universe is the store's own sealed slots, filtered by the reader under
	// test. Names, not slots: a name can own several, and the reader takes one
	// ranked answer for the name.
	seen := map[values.Symbol]bool{}
	before := []values.Symbol{}
	for _, ns := range eng.Environment().Namespace().Store().SealedSlots() {
		if seen[ns.Name] {
			continue
		}
		seen[ns.Name] = true
		name := ns.Name
		exp := compilation.LookupPrimitiveExpander(eng.Environment(), &name, nil)
		if exp == nil {
			continue
		}
		before = append(before, name)
	}
	c.Assert(len(before) > 20, qt.IsTrue,
		qt.Commentf("expected the phase-1 expander table to be populated; got %d names, "+
			"so this ratchet is blind", len(before)))

	_, err = eng.EvalMultiple(ctx, phase1BasePrologue)
	c.Assert(err, qt.IsNil,
		qt.Commentf("the import itself must succeed — the defect is that it succeeds too well"))

	masked := []string{}
	for _, name := range before {
		sym := name
		exp := compilation.LookupPrimitiveExpander(eng.Environment(), &sym, nil)
		if exp == nil {
			masked = append(masked, sym.Key)
		}
	}
	sort.Strings(masked)
	c.Assert(masked, qt.HasLen, 0,
		qt.Commentf("%d of %d phase-1 expander rows were masked by the import: %v",
			len(masked), len(before), masked))
}

// TestPhase1BaseImportDoesNotReviveTheGoSyntaxRules is the fourth column, and it
// is a COUNTER assertion because the value assertion passes either way: under
// WithSchemeSyntaxForms the imported `syntax-rules` is a phase-0 *SyntaxCompiler
// lifted to (phase 1, mutable), where it out-ranks the Scheme macro at
// (1, sealed). lookupMacroBinding's ARM 2 declined it on type, left the head
// unexpanded, and the COMPILER dispatched the Go form — same answer, wrong layer.
// Only compilation.GoSyntaxFormCompiles() can see that, which is why the delta,
// not the result, is the subject.
func TestPhase1BaseImportDoesNotReviveTheGoSyntaxRules(t *testing.T) {
	c := qt.New(t)
	src := phase1BasePrologue + `(define-syntax sr (syntax-rules () ((_ x) (list x x))))
(sr 1)`
	before := compilation.GoSyntaxFormCompiles()
	c.Assert(evalSyntaxForms(t, src, withStdlibResolver(wile.WithSchemeSyntaxForms())...), qt.Equals, "(1 1)")
	c.Assert(compilation.GoSyntaxFormCompiles(), qt.Equals, before)
}
