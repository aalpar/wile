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

// The definition-site literal pin ranks an ambient keyword BELOW an exact-phase
// binding at a lower phase. Auxiliary syntax lives at the ambient coordinate,
// which a probe at ANY phase reaches as T3, so a phase-1 probe for `else` would
// otherwise answer the keyword before the descent has looked at phase 0, and a
// syntax-case macro's literal is written at phase 1 and used at phase 0. This is
// the unit pin for the ordering TestPatternLiteralRespectsAUseSiteShadow
// ("syntax-case, global shadow", pkg/wile) needs end-to-end.
func TestLookupLiteralBindingAmbientLast(t *testing.T) {
	const sym = "else"

	// A store whose only `else` is the ambient keyword, written through the
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

	t.Run("definition site at phase 1: a phase-0 user shadow wins over ambient", func(t *testing.T) {
		ns, _ := newStore()
		user := shadow(ns, environment.PhaseRuntime)
		env := ns.Runtime().AtPhase(environment.PhaseExpand)
		got, ok := lookupLiteralBinding(env, sym, nil, definitionFallbackPhases(env))
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, got, qt.Equals, user)
	})
	t.Run("definition site at phase 1, no shadow anywhere: ambient", func(t *testing.T) {
		ns, ambient := newStore()
		env := ns.Runtime().AtPhase(environment.PhaseExpand)
		got, ok := lookupLiteralBinding(env, sym, nil, definitionFallbackPhases(env))
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, got, qt.Equals, ambient)
	})
	t.Run("an own-phase shadow outranks a lower-phase one", func(t *testing.T) {
		ns, _ := newStore()
		_ = shadow(ns, environment.PhaseRuntime)
		own := shadow(ns, environment.PhaseExpand)
		env := ns.Runtime().AtPhase(environment.PhaseExpand)
		got, _ := lookupLiteralBinding(env, sym, nil, definitionFallbackPhases(env))
		qt.Assert(t, got, qt.Equals, own)
	})
	t.Run("use site: own phase then ambient, never a lower phase", func(t *testing.T) {
		ns, ambient := newStore()
		_ = shadow(ns, environment.PhaseRuntime)
		env := ns.Runtime().AtPhase(environment.PhaseExpand)
		got, _ := lookupLiteralBinding(env, sym, nil, nil)
		qt.Assert(t, got, qt.Equals, ambient)
	})
	t.Run("a name that is not ambient: first exact hit, else nil", func(t *testing.T) {
		ns := environment.NewNamespace()
		env := ns.Runtime().AtPhase(environment.PhaseExpand)
		got, ok := lookupLiteralBinding(env, "lit", nil, definitionFallbackPhases(env))
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, got, qt.IsNil)
	})
}

// An ambient-tier tie is DEAD when an exact-phase binding wins, which is
// probeRankedLocked's own losing-tier rule ("a tie in a losing tier is dead and
// must not panic") applied to the re-ranking lookupLiteralBinding performs: the
// ambient tier loses to EVERY exact phase here, not merely to the query phase.
//
// The ambient tier is now probed dead last: store.AmbientBinding runs only
// after env.ExactBinding at env's own phase and ExactBindingAt at every
// fallback phase have all come back with neither a binding nor a tie. An
// exact-phase hit returns before the ambient probe is ever reached, so this
// test's ambient tie is never even computed when an exact phase wins; it
// surfaces only in the "nothing exact anywhere" case below.
//
// Latent as production stands: every ambient registration passes nil scopes, so
// a name has at most one ambient slot and the tier cannot tie. The scoped
// ambient slots below are built directly.
func TestLookupLiteralBindingAmbientTieIsDeadUnderAnExactHit(t *testing.T) {
	const sym = "else"
	// {A} and {B} are incomparable, equal-cardinality, and both subsets of the
	// query {A,B}: neither is THE maximal match, which is Flatt's ambiguity.
	scopeA := syntax.NewScope()
	scopeB := syntax.NewScope()
	query := []*syntax.Scope{scopeA, scopeB}

	// Two ambient slots of one name, written the only way an ambient slot can be
	// (the phase-0 sealed-write view). CreateGlobalBindingAt reuses a slot only
	// on EXACT scope-set equality, so distinct scope sets at (ANY, sealed) are
	// two slots rather than one.
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

	t.Run("a phase-0 shadow answers the phase-1 definition-site probe", func(t *testing.T) {
		ns := newTiedStore(t)
		idx, _ := ns.Runtime().MaybeCreateOwnGlobalBinding(
			values.NewSymbol(sym), environment.BindingTypeVariable, nil)
		user := ns.Store().GetOwnGlobalBinding(idx)
		qt.Assert(t, user, qt.IsNotNil)

		env := ns.Runtime().AtPhase(environment.PhaseExpand)
		got, ok := lookupLiteralBinding(env, sym, query, definitionFallbackPhases(env))
		qt.Assert(t, ok, qt.IsTrue,
			qt.Commentf("the ambient tie lost to a phase-0 binding; it must not be answered"))
		qt.Assert(t, got, qt.Equals, user)
	})
	t.Run("with nothing exact anywhere the tie is the answer", func(t *testing.T) {
		ns := newTiedStore(t)
		env := ns.Runtime().AtPhase(environment.PhaseExpand)
		got, ok := lookupLiteralBinding(env, sym, query, definitionFallbackPhases(env))
		qt.Assert(t, ok, qt.IsFalse)
		qt.Assert(t, got, qt.IsNil)
	})
}

// An exact-tier tie refuses at the phase it is met, whatever the ambient tier
// holds: ExactBinding and ExactBindingAt report the tie directly, before the
// ambient tier is ever consulted, and the ambient tier answers only after
// every exact tier (env's own phase, then each fallback) has missed. This was
// originally the ratchet on probeIgnoringAmbientTie's `&& ambientTie`
// conjunct (deleted with that helper); it stays as the pin of the property
// the conjunct existed to protect.
func TestLookupLiteralBindingExactTieIsRefusedWithoutAnAmbientTie(t *testing.T) {
	const sym = "else"
	scopeA := syntax.NewScope()
	scopeB := syntax.NewScope()
	query := []*syntax.Scope{scopeA, scopeB}

	// Two phase-0 mutable slots of one name under {A} and {B}, and no ambient
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
			qt.Commentf("an exact-tier tie with no ambient tie must be refused, not swallowed"))
		qt.Assert(t, got, qt.IsNil)
	})
}

// An exact-tier tie is refused at the phase it is found, whatever the ambient
// tier holds. Before ambiguity became a returned value the pin swallowed this
// case (documented as a residual on probeIgnoringAmbientTie): with the ambient
// tier ALSO tied, every ambiguity panic on the descent was recovered and the
// exact tie at phase 0 was indistinguishable from the dead ambient one. It
// answered (nil, false) by the accident of the ambient flag, so this is a PIN of
// the answer and a GATE on the reason: TestLookupLiteralBindingExactTieIsRefused
// WithoutAnAmbientTie beside it holds the other half.
func TestLookupLiteralBindingExactTieIsRefusedEvenWithAnAmbientTie(t *testing.T) {
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
// TestLookupLiteralBindingExactTieIsRefusedEvenWithAnAmbientTie does not reach:
// there, phase 0 is ALSO tied, so the old swallow-and-descend and the current
// immediate refusal land on the same (nil, false) by accident. Here phase 0
// carries one CLEAN slot instead, unscoped and so a match under any query.
//
// Before c8080848, probeIgnoringAmbientTie(env, s, sq, ambientTie=true)
// swallowed the phase-1 exact tie precisely because the ambient tier was ALSO
// tied, the descent fell through to phase 0, and the clean slot answered
// (binding, true): a live ambiguity silently resolved to an unrelated binding.
// Now env.ExactBinding at phase 1 reports the tie directly and
// lookupLiteralBinding refuses before the descent ever reaches phase 0.
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

	// Ambient tier: the same two scope sets, also tied.
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
