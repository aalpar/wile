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

package compilation

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// The definition-site literal pin descends from its own phase and takes the
// first hit. Auxiliary syntax (else, =>) is written through the phase-0
// sealed-write view, which since the ambient tier's deletion lands at
// (ExactPhase(0), sealed) — an ordinary rung of the descent, not a tier standing
// outside every phase. The ordering the pin needs is unchanged and now falls out
// of the ordinary tier rule: a user (define else 5) at phase 0 is a MUTABLE slot
// at that same coordinate, so T1 beats T2 on the rung the descent stops at. This
// is the unit pin for the ordering TestPatternLiteralRespectsAUseSiteShadow
// ("syntax-case, global shadow", pkg/wile) needs end-to-end.
func TestLookupLiteralBindingDescendsPhasesFromItsOwn(t *testing.T) {
	const sym = "else"

	// A store whose only `else` is the auxiliary keyword, written through the
	// phase-0 sealed-write view exactly as registry apply writes it.
	newStore := func() (*environment.Namespace, *environment.Binding) {
		ns := environment.NewNamespace()
		idx, _ := ns.Runtime().SealedWriteViewAt(environment.PhaseRuntime).
			MaybeCreateOwnGlobalBinding(values.NewSymbol(sym), environment.BindingTypePrimitive, nil)
		return ns, ns.Store().GetOwnGlobalBinding(idx)
	}
	// A user (define else …) at the given phase: an exact-phase mutable slot.
	shadow := func(ns *environment.Namespace, phase environment.Phase) *environment.Binding {
		view := ns.Runtime().AtPhase(phase)
		idx, _ := view.MaybeCreateOwnGlobalBinding(values.NewSymbol(sym), environment.BindingTypeVariable, nil)
		return ns.Store().GetOwnGlobalBinding(idx)
	}

	t.Run("definition site at phase 1: a phase-0 user shadow wins over the keyword", func(t *testing.T) {
		ns, _ := newStore()
		user := shadow(ns, environment.PhaseRuntime)
		env := ns.Runtime().AtPhase(environment.PhaseExpand)
		got, ok := lookupLiteralBinding(env, sym, nil, definitionFallbackPhases(env))
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, got, qt.Equals, user,
			qt.Commentf("both are at phase 0 now, so the mutable T1 slot outranks the sealed T2 keyword"))
	})
	t.Run("definition site at phase 1, no shadow anywhere: the keyword", func(t *testing.T) {
		ns, keyword := newStore()
		env := ns.Runtime().AtPhase(environment.PhaseExpand)
		got, ok := lookupLiteralBinding(env, sym, nil, definitionFallbackPhases(env))
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, got, qt.Equals, keyword,
			qt.Commentf("the descent, not an ambient tier, is what carries phase 1 down to the phase-0 keyword"))
	})
	t.Run("an own-phase shadow outranks a lower-phase one", func(t *testing.T) {
		ns, _ := newStore()
		_ = shadow(ns, environment.PhaseRuntime)
		own := shadow(ns, environment.PhaseExpand)
		env := ns.Runtime().AtPhase(environment.PhaseExpand)
		got, _ := lookupLiteralBinding(env, sym, nil, definitionFallbackPhases(env))
		qt.Assert(t, got, qt.Equals, own)
	})
	t.Run("use site: own phase, then the language's rows, and in that order", func(t *testing.T) {
		// The use site passes no fallbacks, so its exact-tier probe sees its own
		// phase and nothing else. Deleting the ambient tier left it with nothing
		// at phase 1: the phase-0 keyword and the phase-0 user shadow are both
		// invisible from here.
		ns, keyword := newStore()
		user := shadow(ns, environment.PhaseRuntime)
		env := ns.Runtime().AtPhase(environment.PhaseExpand)

		got, ok := lookupLiteralBinding(env, sym, nil, nil)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, got, qt.IsNil,
			qt.Commentf("a lower phase's slot is another program's, whether it is the keyword or the shadow"))

		// The descent's LAST step is what the language supplies, and after Stage
		// A that is a bulk row rather than the ambient tier. The step moved with
		// the tier rather than being deleted with it: the ORDER is what this
		// function exists to impose, not which store structure answers.
		//
		// The earlier revision of this subtest pinned the asymmetry — an ordinary
		// read at phase 1 resolving through the row while the R7RS 4.3.2 literal
		// pin answered nil — and said the assertion was what would fail when Task
		// 7 Step 3's decision landed. It landed: lookupLiteralBinding's last step
		// is now BulkBindingAt, a ROW-ONLY read, so the two agree.
		//
		// Row-only, not GetBinding: the steps above use ExactBinding, so a full
		// read here would re-probe those same per-symbol slots at this phase and
		// collapse the ordering.
		store := ns.Store()
		src := environment.NewSealedStoreBulkSource(store, environment.PhaseRuntime, environment.BaseSourceName())
		store.InstallBulkRow(src, nil, environment.ExactPhase(environment.PhaseExpand), true)

		qt.Assert(t, env.GetBinding(values.NewSymbol(sym), values.EmptyScopes()), qt.Equals, keyword,
			qt.Commentf("the row is sealed-tier restricted, so it supplies the keyword and not the user shadow"))
		qt.Assert(t, env.GetBinding(values.NewSymbol(sym), values.EmptyScopes()), qt.Not(qt.Equals), user)

		got, ok = lookupLiteralBinding(env, sym, nil, nil)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, got, qt.Equals, keyword,
			qt.Commentf("the literal pin and an ordinary read must agree on what the language supplies"))
		qt.Assert(t, got, qt.Not(qt.Equals), user,
			qt.Commentf("the row is sealed-only, so a phase-0 user shadow is still not reachable from phase 1 through it"))
	})
	t.Run("a name bound nowhere: nil, and not a refusal", func(t *testing.T) {
		ns := environment.NewNamespace()
		env := ns.Runtime().AtPhase(environment.PhaseExpand)
		got, ok := lookupLiteralBinding(env, "lit", nil, definitionFallbackPhases(env))
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, got, qt.IsNil)
	})
}

// A tie in a LOSING tier is dead: probeTiersLocked flags ambiguity only against
// the current best, which is always in the winning tier. With the ambient tier
// deleted this plays out inside ONE phase rather than across the phase axis —
// the sealed slots that used to sit at (ANY, sealed) are now at (0, sealed), so
// a mutable phase-0 slot beats them at T1 and their tie is never scored.
//
// Latent as production stands: every sealed registration passes nil scopes, so a
// name has at most one sealed slot per phase and the tier cannot tie. The scoped
// slots below are built directly.
func TestLookupLiteralBindingSealedTieIsDeadUnderAMutableHit(t *testing.T) {
	const sym = "else"
	// {A} and {B} are incomparable, equal-cardinality, and both subsets of the
	// query {A,B}: neither is THE maximal match, which is Flatt's ambiguity.
	scopeA := syntax.NewScope()
	scopeB := syntax.NewScope()
	query := []*syntax.Scope{scopeA, scopeB}

	// Two sealed slots of one name at phase 0, written the way auxiliary syntax
	// is (the phase-0 sealed-write view). CreateGlobalBindingAt reuses a slot
	// only on EXACT scope-set equality, so distinct scope sets at one coordinate
	// are two slots rather than one.
	newTiedStore := func(t *testing.T) *environment.Namespace {
		ns := environment.NewNamespace()
		sealedRoot := ns.Runtime().SealedWriteViewAt(environment.PhaseRuntime)
		for _, scopes := range [][]*syntax.Scope{{scopeA}, {scopeB}} {
			_, created := sealedRoot.MaybeCreateOwnGlobalBinding(
				values.NewSymbol(sym), environment.BindingTypePrimitive, scopes)
			qt.Assert(t, created, qt.IsTrue)
		}
		return ns
	}

	t.Run("a phase-0 mutable slot answers the phase-1 definition-site probe", func(t *testing.T) {
		ns := newTiedStore(t)
		idx, _ := ns.Runtime().MaybeCreateOwnGlobalBinding(
			values.NewSymbol(sym), environment.BindingTypeVariable, nil)
		user := ns.Store().GetOwnGlobalBinding(idx)
		qt.Assert(t, user, qt.IsNotNil)

		env := ns.Runtime().AtPhase(environment.PhaseExpand)
		got, ok := lookupLiteralBinding(env, sym, query, definitionFallbackPhases(env))
		qt.Assert(t, ok, qt.IsTrue,
			qt.Commentf("the sealed tie lost to the mutable slot on the same rung; it must not be answered"))
		qt.Assert(t, got, qt.Equals, user)
	})
	t.Run("with no mutable slot the sealed tie is the answer", func(t *testing.T) {
		ns := newTiedStore(t)
		env := ns.Runtime().AtPhase(environment.PhaseExpand)
		got, ok := lookupLiteralBinding(env, sym, query, definitionFallbackPhases(env))
		qt.Assert(t, ok, qt.IsFalse)
		qt.Assert(t, got, qt.IsNil)
	})
}

// A tie refuses at the phase it is met. ExactBinding and ExactBindingAt report
// the tie directly rather than falling through, so the descent stops there
// instead of continuing to a lower phase that might answer cleanly. This was
// originally the ratchet on probeIgnoringAmbientTie's `&& ambientTie` conjunct
// (deleted with that helper, and the ambient tier deleted after it); it stays as
// the pin of the property the conjunct existed to protect.
//
// The store here holds mutable slots ONLY, so nothing sealed sits under the tie.
// Its sibling below adds the sealed half.
func TestLookupLiteralBindingMutableTieIsRefusedWithNothingSealed(t *testing.T) {
	const sym = "else"
	scopeA := syntax.NewScope()
	scopeB := syntax.NewScope()
	query := []*syntax.Scope{scopeA, scopeB}

	// Two phase-0 mutable slots of one name under {A} and {B}, and no sealed
	// slot at all.
	newTiedRuntime := func(t *testing.T) *environment.Namespace {
		ns := environment.NewNamespace()
		for _, scopes := range [][]*syntax.Scope{{scopeA}, {scopeB}} {
			_, created := ns.Runtime().MaybeCreateOwnGlobalBinding(
				values.NewSymbol(sym), environment.BindingTypeVariable, scopes)
			qt.Assert(t, created, qt.IsTrue)
		}
		return ns
	}

	t.Run("own phase", func(t *testing.T) {
		ns := newTiedRuntime(t)
		got, ok := lookupLiteralBinding(ns.Runtime(), sym, query, nil)
		qt.Assert(t, ok, qt.IsFalse)
		qt.Assert(t, got, qt.IsNil)
	})
	t.Run("reached by the definition-site descent", func(t *testing.T) {
		ns := newTiedRuntime(t)
		env := ns.Runtime().AtPhase(environment.PhaseExpand)
		got, ok := lookupLiteralBinding(env, sym, query, definitionFallbackPhases(env))
		qt.Assert(t, ok, qt.IsFalse,
			qt.Commentf("a tie met on the descent must be refused, not swallowed"))
		qt.Assert(t, got, qt.IsNil)
	})
}

// A MUTABLE tie is refused even though the sealed tier beneath it is also tied.
// Before ambiguity became a returned value the pin swallowed this case
// (documented as a residual on probeIgnoringAmbientTie): the second tie was in
// the then-ambient tier, every ambiguity panic on the descent was recovered, and
// the live tie was indistinguishable from the dead one. It answered (nil, false)
// by the accident of a flag, so this is a PIN of the answer and a GATE on the
// reason: TestLookupLiteralBindingMutableTieIsRefusedWithNothingSealed beside it
// holds the other half.
//
// Post-deletion the two ties are at ONE coordinate pair, (0, mutable) and
// (0, sealed), rather than at a phase and outside every phase. The winning tier
// is T1, so it is the mutable tie that is scored and the sealed one that is dead.
func TestLookupLiteralBindingMutableTieIsRefusedBesideASealedTie(t *testing.T) {
	const sym = "else"
	scopeA := syntax.NewScope()
	scopeB := syntax.NewScope()
	query := []*syntax.Scope{scopeA, scopeB}

	ns := environment.NewNamespace()
	sealedRoot := ns.Runtime().SealedWriteViewAt(environment.PhaseRuntime)
	for _, scopes := range [][]*syntax.Scope{{scopeA}, {scopeB}} {
		_, created := sealedRoot.MaybeCreateOwnGlobalBinding(
			values.NewSymbol(sym), environment.BindingTypePrimitive, scopes)
		qt.Assert(t, created, qt.IsTrue)
		_, created = ns.Runtime().MaybeCreateOwnGlobalBinding(
			values.NewSymbol(sym), environment.BindingTypeVariable, scopes)
		qt.Assert(t, created, qt.IsTrue)
	}

	env := ns.Runtime().AtPhase(environment.PhaseExpand)
	got, ok := lookupLiteralBinding(env, sym, query, definitionFallbackPhases(env))
	qt.Assert(t, ok, qt.IsFalse)
	qt.Assert(t, got, qt.IsNil)
}

// TestLookupLiteralBindingExactTieIsRefusedDespiteACleanLowerPhase is the case
// TestLookupLiteralBindingMutableTieIsRefusedBesideASealedTie does not reach:
// there, phase 0 is ALSO tied, so the old swallow-and-descend and the current
// immediate refusal land on the same (nil, false) by accident. Here phase 0
// carries one CLEAN slot instead, unscoped and so a match under any query.
//
// Before c8080848, probeIgnoringAmbientTie(env, s, sq, ambientTie=true)
// swallowed the phase-1 exact tie precisely because a second tier was ALSO tied,
// the descent fell through to phase 0, and the clean slot answered
// (binding, true): a live ambiguity silently resolved to an unrelated binding.
// Now env.ExactBinding at phase 1 reports the tie directly and
// lookupLiteralBinding refuses before the descent ever reaches phase 0.
//
// The second tie is written through the phase-0 sealed-write view, which used to
// mean the ambient coordinate and now means (0, sealed). It is a losing tier
// either way, which is why deleting the ambient tier did not move this answer.
func TestLookupLiteralBindingExactTieIsRefusedDespiteACleanLowerPhase(t *testing.T) {
	const sym = "else"
	scopeA := syntax.NewScope()
	scopeB := syntax.NewScope()
	query := []*syntax.Scope{scopeA, scopeB}

	ns := environment.NewNamespace()

	// Phase 1: an exact-tier tie ({A} and {B} are incomparable, equal-cardinality
	// subsets of the query).
	expand := ns.Runtime().AtPhase(environment.PhaseExpand)
	for _, scopes := range [][]*syntax.Scope{{scopeA}, {scopeB}} {
		_, created := expand.MaybeCreateOwnGlobalBinding(
			values.NewSymbol(sym), environment.BindingTypeVariable, scopes)
		qt.Assert(t, created, qt.IsTrue)
	}

	// Phase 0, sealed: the same two scope sets, also tied.
	sealedRoot := ns.Runtime().SealedWriteViewAt(environment.PhaseRuntime)
	for _, scopes := range [][]*syntax.Scope{{scopeA}, {scopeB}} {
		_, created := sealedRoot.MaybeCreateOwnGlobalBinding(
			values.NewSymbol(sym), environment.BindingTypePrimitive, scopes)
		qt.Assert(t, created, qt.IsTrue)
	}

	// Phase 0: one clean slot, unscoped, resolving under any query.
	idx, created := ns.Runtime().MaybeCreateOwnGlobalBinding(
		values.NewSymbol(sym), environment.BindingTypeVariable, nil)
	qt.Assert(t, created, qt.IsTrue)
	clean := ns.Store().GetOwnGlobalBinding(idx)
	qt.Assert(t, clean, qt.IsNotNil)

	got, ok := lookupLiteralBinding(expand, sym, query, definitionFallbackPhases(expand))
	qt.Assert(t, ok, qt.IsFalse,
		qt.Commentf("a phase-1 exact tie must be refused even though a clean phase-0 slot could otherwise answer it"))
	qt.Assert(t, got, qt.IsNil)
}
