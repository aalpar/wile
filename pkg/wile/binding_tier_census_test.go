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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/registry/core"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// These are ANSWER pins: they record where a fully built engine actually puts a
// binding, measured rather than reasoned. They exist because the phase/tier
// question was answered wrongly in a code comment for a long time with nothing
// contradicting it.
//
// The question they answer has CHANGED TWICE under Stage A of the Flatt binding
// model (plans/2026-09-08-flatt-binding-model-a-design).
//
// First, the ambient (ANY-phase, sealed) tier was deleted: writeCoordinates
// sends every sealed phase-0 write to ExactPhase(0) instead of AnyPhase(), and
// probeTiersLocked no longer has a wildcard arm. "Ambient" is therefore
// uniformly false, and the original two-column census would read as a row of
// zeroes.
//
// Second — and this is what the file now records — what replaced the wildcard is
// not one row but TWO, and the difference between them is the whole point. A
// dialect declares INITIAL IMPORTS (pkg/wile/dialect.go), installed at engine
// origin as BULK ROWS (pkg/environment/bulk_source.go): one resolution candidate
// standing for every name a store supplies at a phase, consulted only when the
// per-symbol probe misses.
//
//   - The BASE row is declared at phase 0 only, and carries the whole sealed base.
//   - The MACRO VOCABULARY row is installed at EVERY macro phase as that phase's
//     view is minted, and is a STRICT SUBSET of the base: the macro-writing
//     kernel, plus any %-prefixed bootstrap-private name (defaultMacroVocabulary,
//     macroVocabularyAdmits).
//
// So phase-1 visibility is three-valued, not two, and which of the three a name
// falls in decides whether a procedural transformer body that calls it must
// write (import (for-syntax (scheme base))).

// tierFootprint is where one name lands, in the three coordinates Stage A left
// behind:
//
//   - exactPhase0 — a slot the store itself owns at phase 0, i.e. the sealed base;
//   - exactPhase1 — a SECOND registration giving the name a slot of its own at
//     phase 1, which dual-phase Go primitives have and Scheme defines do not;
//   - reachablePhase1 — what a phase-1 frame actually answers, which is
//     exactPhase1 OR the macro-vocabulary row standing in for a name with no
//     phase-1 slot behind it.
//
// exactPhase1 implies reachablePhase1. The converse gap is the vocabulary row,
// and the names in neither are the ones D4 costs an import.
type tierFootprint struct {
	exactPhase0     bool
	exactPhase1     bool
	reachablePhase1 bool
}

// censusEngine builds the engine every pin here measures. Default options on
// purpose: the census is about what an engine puts in the store before any
// caller asks for a profile, and the footprints below are identical under
// KitchenSink.
func censusEngine(t *testing.T) *wile.Engine {
	t.Helper()
	q, err := wile.NewEngine(context.Background())
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = q.Close()
	})
	return q
}

// footprintOfName measures the three coordinates for name.
//
// reachablePhase1 is read off the FRAME, not the store, and that difference is
// the whole measurement: the frame's GetBinding falls through to the installed
// bulk rows when the per-symbol probe misses, while the store's own
// ExactBindingAt never does. A name the frame answers at phase 1 and
// ExactBindingAt does not is therefore a name a row supplied.
//
// The ambient assertion is a ratchet, not a measurement: the tier is gone, so
// every name must answer nil there. A non-nil answer means the tier came back,
// and every row below would then be measuring the old mechanism under a new name.
func footprintOfName(t *testing.T, eng *wile.Engine, name string) tierFootprint {
	t.Helper()
	sym := values.NewSymbol(name)
	store := eng.Environment().GlobalEnvironment()

	amb, ambTie := store.AmbientBinding(sym, syntax.EmptyScopes())
	qt.Assert(t, ambTie, qt.IsFalse, qt.Commentf("%q ties in the ambient tier", name))
	qt.Assert(t, amb, qt.IsNil,
		qt.Commentf("%q answers in the ambient tier, which Stage A deleted", name))

	ex0, ex0Tie := store.ExactBindingAt(sym, syntax.EmptyScopes(), environment.PhaseRuntime)
	qt.Assert(t, ex0Tie, qt.IsFalse, qt.Commentf("%q ties at exact phase 0", name))
	ex1, ex1Tie := store.ExactBindingAt(sym, syntax.EmptyScopes(), environment.PhaseExpand)
	qt.Assert(t, ex1Tie, qt.IsFalse, qt.Commentf("%q ties at exact phase 1", name))

	reachable := eng.Environment().AtPhase(environment.PhaseExpand).
		GetBinding(sym, syntax.EmptyScopes())

	q := tierFootprint{
		exactPhase0:     ex0 != nil,
		exactPhase1:     ex1 != nil,
		reachablePhase1: reachable != nil,
	}
	return q
}

// TestKeywordSlotIsNotTheExpanderSlot pins that the slot registry/core/specialforms.go
// mints for syntax-rules, quote or import ("needs a binding for library export
// resolution") is a DIFFERENT slot from the primitive-expander registration,
// which is exact-phase-1 (see TestPrimitiveExpandersLandAtExactPhaseOne in
// pkg/machine/compilation). let-syntax has no keyword row and is the
// discriminating case: same registration path, phase-1 slot only.
//
// Stage A moved the keyword row without changing which registration owns it.
// The row was a sealed phase-0 write, so it used to land ambient and now lands
// at ExactPhase(0) — the two-slot claim is unchanged, the coordinate of the
// first slot is not. Every name here owns a phase-1 slot outright, so the
// narrowing of the phase-1 row to the macro vocabulary cannot reach them.
func TestKeywordSlotIsNotTheExpanderSlot(t *testing.T) {
	eng := censusEngine(t)

	tcs := []struct {
		name string
		want tierFootprint
	}{
		// Keyword row in specialforms.go AND a primitive expander: two slots.
		{"syntax-rules", tierFootprint{exactPhase0: true, exactPhase1: true, reachablePhase1: true}},
		{"quote", tierFootprint{exactPhase0: true, exactPhase1: true, reachablePhase1: true}},
		{"import", tierFootprint{exactPhase0: true, exactPhase1: true, reachablePhase1: true}},
		// Primitive expander with no keyword row: expander slot only. Its
		// absence at phase 0 is not an absence at the top level — a macro use
		// at phase 0 is looked up one phase UP (lookupMacroBinding), which is
		// exactly the slot this row says exists.
		{"let-syntax", tierFootprint{exactPhase1: true, reachablePhase1: true}},
		// Bootstrap macro, written at phase 1: no keyword row either.
		{"when", tierFootprint{exactPhase1: true, reachablePhase1: true}},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, footprintOfName(t, eng, tc.name), qt.Equals, tc.want)
		})
	}
}

// TestEllipsisAndUnderscoreReachPhaseOneOnlyThroughABulkRow pins the two names
// that hold NO phase-1 slot at all. Before Stage A they were reachable at phase 1
// because the ambient tier was phase-blind; now they are reachable because the
// MACRO VOCABULARY row lists them by name (defaultMacroVocabulary's declarative
// group), and that listing is the only thing holding them up — the base row is
// declared at phase 0 and cannot. Racket's measured phase-1 surface under
// racket/base is the same set plus syntax-rules.
//
// Nothing in the expander depends on the answer today: pkg/internal/match/
// syntax_compiler.go matches both by NAME, and under WithoutAmbientBindings they
// hold zero slots anywhere and still expand. The pin is here so a future
// scope-aware matcher finds the supply line already stated rather than assumed,
// and so that dropping them from the vocabulary is a visible edit rather than a
// silent one.
func TestEllipsisAndUnderscoreReachPhaseOneOnlyThroughABulkRow(t *testing.T) {
	eng := censusEngine(t)

	for _, name := range []string{"...", "_"} {
		t.Run(name, func(t *testing.T) {
			qt.Assert(t, footprintOfName(t, eng, name), qt.Equals,
				tierFootprint{exactPhase0: true, reachablePhase1: true})
		})
	}
}

// phaseOneGroup names the ROUTE by which a name is, or is not, visible at phase
// 1. Each group has a defining footprint (footprintFor), so naming the group in
// a table row is a claim the measurement can refute rather than a label glued on
// beside it.
type phaseOneGroup string

const (
	// groupDualPhaseGo is a Go primitive registered at BOTH phases
	// (registry/apply.go's phaseTargets loop). It owns a phase-1 slot outright,
	// so no row and no import is involved.
	groupDualPhaseGo phaseOneGroup = "dual-phase Go primitive"
	// groupMacroVocabulary owns no phase-1 slot and is reachable anyway, because
	// the macro-vocabulary row admits the name at every macro phase.
	groupMacroVocabulary phaseOneGroup = "macro-vocabulary row"
	// groupBootstrapRuntime is DEFINED in bootstrap/stdlib Scheme: written at
	// phase 0 sealed, admitted by no row, reachable at phase 1 through nothing.
	// An explicit (import (for-syntax (scheme base))) is the only route.
	groupBootstrapRuntime phaseOneGroup = "bootstrap Scheme runtime define"
)

// footprintFor turns a group plus a phase-0 answer into the footprint that group
// means. It is what makes the group column load bearing: a row that names the
// wrong group fails on the phase-1 coordinates.
func footprintFor(t *testing.T, exactPhase0 bool, g phaseOneGroup) tierFootprint {
	t.Helper()
	q := tierFootprint{exactPhase0: exactPhase0}
	switch g {
	case groupDualPhaseGo:
		q.exactPhase1 = true
		q.reachablePhase1 = true
	case groupMacroVocabulary:
		q.reachablePhase1 = true
	case groupBootstrapRuntime:
		// Both phase-1 coordinates stay false: that is the group's definition.
	default:
		t.Fatalf("unknown phase-one group %q", g)
	}
	return q
}

// TestPhaseOneVisibilityHasThreeGroups pins the asymmetry that sized Stage A's
// phase-distinctness break (D3), in the three-way form the narrowed phase-1 row
// left behind.
//
// RENAMED from TestStdlibDefinesReachPhaseOneOnlyThroughABulkRow, whose name
// asserted something no longer true. That name claimed the stdlib Scheme defines
// DO reach phase 1, through a bulk row, and it held while the phase-1 row carried
// the whole base. The row has since been narrowed to the macro vocabulary, which
// admits none of them, so assoc, caar, cadr, vector-map and string-map now reach
// phase 1 through NOTHING. That is not a regression, it IS D4 — it is what makes
// TestPhase1_ProceduralTransformerUnboundWithoutImport go green — and group (c)
// below is the group D4 is about.
//
// The three groups, every value read off a built engine:
//
//	(a) dual-phase Go primitives — a phase-1 slot of their own, so nothing to
//	    declare;
//	(b) macro-vocabulary members — no slot at phase 1, reachable THROUGH THE ROW,
//	    which is Racket's rule (i): the macro-writing kernel is ambient to a
//	    transformer;
//	(c) bootstrap Scheme runtime definitions — neither, and the only route to them
//	    from a procedural transformer body is (import (for-syntax (scheme base))),
//	    or (scheme cxr) for the deeper c[ad]+r names.
//
// The cost of (c) is thus a DECLARATION, and it falls on the Scheme layer, never
// on the primitive layer. Measure, do not reason: the value of these rows is that
// they were read off a built engine.
func TestPhaseOneVisibilityHasThreeGroups(t *testing.T) {
	eng := censusEngine(t)

	tcs := []struct {
		name        string
		exactPhase0 bool
		group       phaseOneGroup
	}{
		// (a) Registered at both phases (registry/apply.go phaseTargets).
		{"car", true, groupDualPhaseGo},
		{"list-copy", true, groupDualPhaseGo},
		// (b) In defaultMacroVocabulary. apply and not are the surprising ones:
		// they read like ordinary runtime procedures and are runtime-only in the
		// registry (see TestCorePrimitivePhaseCensus), so the row is the whole of
		// why a transformer body may call them unimported.
		{"else", true, groupMacroVocabulary},
		{"=>", true, groupMacroVocabulary},
		{"apply", true, groupMacroVocabulary},
		{"not", true, groupMacroVocabulary},
		// Syntax introspection: a phase-1 registration AND a vocabulary entry, so
		// it is group (a) by footprint. Listed because the vocabulary comment
		// claims no library exports it, which makes the slot its only supply.
		{"datum->syntax", true, groupDualPhaseGo},
		// (c) Defined in bootstrap/stdlib Scheme. cadr is A15's exemplar; the
		// deeper c[ad]+r names live in cxr.sld, not base.sld.
		{"assoc", true, groupBootstrapRuntime},
		{"caar", true, groupBootstrapRuntime},
		{"cadr", true, groupBootstrapRuntime},
		{"vector-map", true, groupBootstrapRuntime},
		{"string-map", true, groupBootstrapRuntime},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			want := footprintFor(t, tc.exactPhase0, tc.group)
			qt.Assert(t, footprintOfName(t, eng, tc.name), qt.Equals, want,
				qt.Commentf("%q is filed as %s", tc.name, tc.group))
		})
	}
}

// TestCorePrimitivePhaseCensus pins the SHAPE of core's phase declarations: the
// pair/list/predicate vocabulary a transformer body would reach for is registered
// at both phases, while the runtime-only names are the continuation, error-object,
// parameter and introspection family. Exemplars rather than a count, so adding a
// primitive does not fail this; the counts are logged for the record.
//
// This is the REGISTRY's answer, which is not the store's: a runtime-only
// primitive can still be reachable at phase 1 if the macro-vocabulary row admits
// it, which is exactly what happens to apply below. The store's answer is
// TestPhaseOneVisibilityHasThreeGroups.
//
// Measured 2026-09-08 over core.AddToRegistry: 155 names at both phases, 63
// runtime-only, 0 expand-only.
func TestCorePrimitivePhaseCensus(t *testing.T) {
	reg := registry.NewRegistry()
	err := core.AddToRegistry(reg)
	qt.Assert(t, err, qt.IsNil)

	phasesOf := func(name string) (runtime, expand bool) {
		r, ok := reg.PrimitiveByName(name)
		qt.Assert(t, ok, qt.IsTrue, qt.Commentf("core does not register %q", name))
		return r.Phases.Has(environment.PhaseRuntime), r.Phases.Has(environment.PhaseExpand)
	}

	for _, name := range []string{"car", "cdr", "cons", "null?", "eq?", "list-copy"} {
		t.Run("both/"+name, func(t *testing.T) {
			runtimePhase, expandPhase := phasesOf(name)
			qt.Assert(t, runtimePhase, qt.IsTrue)
			qt.Assert(t, expandPhase, qt.IsTrue,
				qt.Commentf("%q must stay visible at phase 1", name))
		})
	}
	for _, name := range []string{"call/cc", "raise", "values", "apply", "make-parameter"} {
		t.Run("runtime-only/"+name, func(t *testing.T) {
			runtimePhase, expandPhase := phasesOf(name)
			qt.Assert(t, runtimePhase, qt.IsTrue)
			qt.Assert(t, expandPhase, qt.IsFalse,
				qt.Commentf("%q owns no phase-1 slot; a phase-1 body reaches it only "+
					"through the macro-vocabulary row or an explicit for-syntax import", name))
		})
	}

	var both, runtimeOnly, expandOnly int
	var expandOnlyNames []string
	for _, name := range reg.PrimitiveNames() {
		r, ok := reg.PrimitiveByName(name)
		if !ok {
			continue
		}
		hasRuntime := r.Phases.Has(environment.PhaseRuntime)
		hasExpand := r.Phases.Has(environment.PhaseExpand)
		switch {
		case hasRuntime && hasExpand:
			both++
		case hasRuntime:
			runtimeOnly++
		case hasExpand:
			expandOnly++
			expandOnlyNames = append(expandOnlyNames, name)
		}
	}
	sort.Strings(expandOnlyNames)
	t.Logf("core phase census: both=%d runtime-only=%d expand-only=%d %v",
		both, runtimeOnly, expandOnly, expandOnlyNames)
	// The dual-phase set must stay the majority: it is the thing that keeps Stage A's
	// D3 break off the primitive layer. A wholesale policy change trips this; adding
	// or removing individual primitives does not.
	qt.Assert(t, both > runtimeOnly, qt.IsTrue,
		qt.Commentf("dual-phase primitives (%d) must outnumber runtime-only (%d)", both, runtimeOnly))
}
