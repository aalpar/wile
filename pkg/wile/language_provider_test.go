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

package wile

// LanguageProvider, and the first half of design section 6.3's counter ratchet.
//
// Plan: plans/2026-09-08-flatt-binding-model-a-impl.local.md, Task 3.
//
// Internal (package wile, not wile_test) because installInitialImports and
// initialImportsFor are unexported and the ratchet counts what they did, not
// what a program can observe. Every change of this shape fails toward the old
// behaviour and the old behaviour is green: rows installed but never consulted
// would leave the whole suite passing, which is why a COUNT is a gate here and
// a behavioural assertion is not.

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// TestInitialImportsMatchDialectDeclaration is design section 6.3, half one:
// the number of bulk rows installed at engine origin EQUALS the dialect's
// declaration count.
//
// Exact equality, not a lower bound. A lower bound would pass a change that
// installed a row per exported NAME, which is the eager per-name shape bulk
// rows exist to replace, and it is the shape this stage would silently regress
// to if materialization or resolution were wired wrong.
func TestInitialImportsMatchDialectDeclaration(t *testing.T) {
	eng, err := NewEngine(context.Background(), WithProfile(KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()

	declared := initialImportsFor(DefaultDialect)
	store := eng.Namespace().Runtime().GlobalEnvironment()
	got := store.BulkRowCount()

	// Rows = the PhasedImport declarations, plus one macro-vocabulary row per
	// macro phase the engine has minted a view for. The second term is not slack
	// in the ratchet: MacroPhasesWithRows reports exactly which phases were
	// reached, so the identity below still pins the count on both sides.
	phases := store.MacroPhasesWithRows()
	qt.Assert(t, got, qt.Equals, len(declared)+phases,
		qt.Commentf("origin installed %d rows for %d declarations plus %d macro phases", got, len(declared), phases))
	qt.Assert(t, len(declared) > 0, qt.IsTrue,
		qt.Commentf("a zero declaration count would make this ratchet vacuous"))
	qt.Assert(t, phases > 0, qt.IsTrue,
		qt.Commentf("no macro phase carries the vocabulary; every transformer body would be starved"))
}

// phasedImportDialect is a LanguageProvider that declares exactly what it is
// told to.
type phasedImportDialect struct {
	imports []PhasedImport
}

func (phasedImportDialect) Name() string {
	return "phased-import-probe"
}

func (phasedImportDialect) InstallForms(_ *forms.FormRegistry) error {
	return nil
}

func (p phasedImportDialect) InitialImports() []PhasedImport {
	return p.imports
}

// TestLanguageProviderOverridesTheDefaultDeclaration pins that the capability
// is consulted, and that the ratchet tracks the DIALECT rather than a constant.
//
// Without this, a wiring bug that ignored the capability and always installed
// the default would satisfy the count ratchet above, since the default is what
// it counts.
func TestLanguageProviderOverridesTheDefaultDeclaration(t *testing.T) {
	d := phasedImportDialect{
		imports: []PhasedImport{
			{Library: environment.BaseSourceName(), Phase: environment.PhaseRuntime},
			{Library: environment.BaseSourceName(), Phase: environment.PhaseExpand},
			{Library: values.NewSymbol("#%probe-extra"), Phase: environment.PhaseExpand},
		},
	}
	eng, err := NewEngine(context.Background(), WithProfile(KitchenSink), WithDialect(d))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()

	store := eng.Namespace().Runtime().GlobalEnvironment()
	got := store.BulkRowCount() - store.MacroPhasesWithRows()
	qt.Assert(t, got, qt.Equals, 3,
		qt.Commentf("the dialect's three declarations must install three rows"))
	qt.Assert(t, got, qt.Not(qt.Equals), len(defaultInitialImports()),
		qt.Commentf("a count equal to the default's would not prove the capability was read"))
}

// TestAbsentLanguageProviderMeansTheDefault pins the boundary the capability's
// doc states: an absent capability is "the default declaration", never "no
// imports".
//
// A dialect wanting an empty base has to say so by returning an empty slice.
// Reading absence as emptiness would silently unbind the whole base for every
// existing dialect, none of which implements the capability.
func TestAbsentLanguageProviderMeansTheDefault(t *testing.T) {
	_, isProvider := DefaultDialect.(LanguageProvider)
	qt.Assert(t, isProvider, qt.IsFalse,
		qt.Commentf("DefaultDialect must NOT implement the capability, or this test proves nothing"))

	got := initialImportsFor(DefaultDialect)
	qt.Assert(t, got, qt.DeepEquals, defaultInitialImports())
	qt.Assert(t, len(got) > 0, qt.IsTrue)
}

// TestPhasedImportPhaseIsTheInstallPhase pins D11's field semantics.
//
// PhasedImport.Phase is the INSTALL phase — where the row lands in the importing
// store — not the source's own phase. The two coincide for the default dialect
// and diverge under for-syntax, which is why the field needed the name. The
// source's phase lives in its BulkSource identity instead, which is why
// environment.BulkSource takes no phase argument.
func TestPhasedImportPhaseIsTheInstallPhase(t *testing.T) {
	declared := defaultInitialImports()
	qt.Assert(t, declared, qt.HasLen, 1)
	qt.Assert(t, declared[0].Phase, qt.Equals, environment.PhaseRuntime,
		qt.Commentf("the base is declared at phase 0 only; phase 1 and above get the macro VOCABULARY, which is a strict subset"))

	// The install phase and the source phase are different, and that is the whole
	// of D11. installInitialImports builds every source at PhaseRuntime — where
	// LoadBootstrapCore writes — while installing the row at the declared phase.
	// Conflating them was measured: the phase-1 row then looked for base names at
	// phase 1, found nothing, and supplied nothing while still installing and
	// ranking.
	eng, err := NewEngine(context.Background(), WithProfile(KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()
	root := eng.Namespace().Runtime()
	sym := values.NewSymbol("syntax-rules")
	qt.Assert(t, root.AtPhase(environment.PhaseExpand).GetBinding(sym, syntax.EmptyScopes()), qt.IsNotNil,
		qt.Commentf("a vocabulary name must reach phase 1 through the row whose SOURCE is phase 0"))
}

// TestBulkRowsCarryTheEmptyScopeSet is the premise resolveRankedLocked's
// miss-only bulk consultation rests on, stated over a real engine.
//
// Resolution consults bulk rows only when the per-symbol probe MISSES. That is
// the slot-beats-row tie-break exactly — a row is sealed, so it ranks
// tierExactSealed; a tierExactMutable or tierExactImported slot outranks it; a
// tierExactSealed slot ties it on tier and, with both scope sets empty, the
// tie-break awards the slot; and no slot can lose on cardinality to an empty
// row set — but ONLY while every installed row carries the empty scope set. A
// row with a non-empty set could outrank a slot on cardinality, and the
// miss-only shape would silently never let it.
//
// It runs over a LIBRARY-BEARING engine, not only a bare one, and that is the
// whole reason it moved out of pkg/environment. A compiled library gets its own
// store, and a library store's SLOTS are where non-empty scope sets actually
// live: measured 2026-09-10 over (scheme base), 1025 slots at zero scopes, 5 at
// one (the library body's binders under the library scope) and 1 at two (a
// macro-introduced binder), against an engine control of 781/781 empty. Rows
// are a different object from slots, so that census does not refute the
// premise — but a ratchet that only looked at the engine store would be looking
// exactly where the premise was never in doubt.
//
// GUARD, not a pin: the PROPERTY it asserts already holds on master, and the
// test is here to keep it holding. Not "it passes on master" — it cannot even
// compile there, since EachBulkRow arrived with it. When Stage B moves the phase
// INTO the scope set it will create the first non-empty row, and this test going
// red is the signal that the miss-only fast path has to become the full argmax.
func TestBulkRowsCarryTheEmptyScopeSet(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx,
		WithProfile(KitchenSink),
		WithSourceFS(stdlib.FS),
		WithLibraryPaths())
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()

	// A real library compile, so at least one store below is a library's own.
	_, err = eng.EvalMultiple(ctx, "(import (scheme base))")
	qt.Assert(t, err, qt.IsNil)

	reg, ok := eng.Environment().LibraryRegistry().(*compilation.LibraryRegistry)
	qt.Assert(t, ok, qt.IsTrue,
		qt.Commentf("without the registry this test degrades to the bare-engine case it exists to widen"))

	engineStore := eng.Namespace().Runtime().GlobalEnvironment()
	stores := map[*environment.GlobalEnvironmentFrame]string{engineStore: "engine"}
	ownStores := 0
	for _, lib := range reg.All() {
		libStore := lib.Env.GlobalEnvironment()
		if libStore == engineStore {
			// A synthetic extension library shares the engine's store; only a
			// compiled .sld gets one of its own.
			continue
		}
		_, seen := stores[libStore]
		if seen {
			continue
		}
		stores[libStore] = lib.Name.SchemeString()
		ownStores++
	}
	qt.Assert(t, ownStores > 0, qt.IsTrue,
		qt.Commentf("no library minted its own store; the library half of this ratchet did not run"))

	for store, name := range stores {
		rows := 0
		store.EachBulkRow(func(scopes []*syntax.Scope, phase environment.Phase, sealed bool) bool {
			qt.Assert(t, scopes, qt.HasLen, 0,
				qt.Commentf("%s store: row %d at (phase %d, sealed=%v) carries %d scopes; resolveRankedLocked's miss-only consultation is then unsound",
					name, rows, phase, sealed, len(scopes)))
			rows++
			return true
		})
		qt.Assert(t, rows > 0, qt.IsTrue,
			qt.Commentf("%s store holds no rows at all, so its pass above asserts nothing", name))
	}
}

// TestBulkResolutionCountIsInBand is design section 6.3's second half at ENGINE
// scale: rows must answer resolutions, and answer roughly as many as they do
// today.
//
// The two existing assertions on this counter are both synthetic — `> 0` over a
// hand-built store (compilation.TestSyntaxCompilersRegistry) and `== 1` over two
// slots and one row (TestResolveRankedCrossPhaseNeedsABulkRow). A fresh
// KitchenSink engine reports 77, two orders of magnitude away from either, so
// neither would notice the row path being disabled at engine scale.
//
// Both edges are failures, and they mean different things:
//
//   - Below the band: rows installed but never consulted, or silently replaced
//     by eager per-name copying. That change fails toward the old behaviour and
//     the old behaviour is green everywhere else in the suite.
//   - Above the band: the miss-only consultation has stopped being miss-only, or
//     the base's own store rows have stopped being skipped by the
//     selfStore/same-phase check — which cost a measured +6% engine startup
//     before that check existed.
//
// Measured 2026-09-10: 77 for KitchenSink, Small and Console; 73 for Tiny;
// deterministic across runs. The band is roughly half to double that, wide
// enough that adding a bootstrap definition does not redden it and narrow
// enough that losing the row path does.
//
// GUARD, not a pin: the PROPERTY it asserts already holds on master — a fresh
// KitchenSink engine reports 77 there too — and the test is here to keep it
// holding. Not "it passes on master": it shares a file with
// TestBulkRowsCarryTheEmptyScopeSet, which needs EachBulkRow, so the file does
// not compile there.
func TestBulkResolutionCountIsInBand(t *testing.T) {
	eng, err := NewEngine(context.Background(), WithProfile(KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()

	store := eng.Namespace().Runtime().GlobalEnvironment()
	got := store.BulkResolutionCount()
	qt.Assert(t, got >= 40, qt.IsTrue,
		qt.Commentf("bulk rows answered only %d resolutions at engine origin; measured 77 on 2026-09-10", got))
	qt.Assert(t, got <= 160, qt.IsTrue,
		qt.Commentf("bulk rows answered %d resolutions at engine origin; measured 77 on 2026-09-10", got))

	// Paired with the row counters, so "rows installed but never consulted" and
	// "resolutions answered by no row" are each a failure rather than two
	// independently green facts.
	rows := store.BulkRowCount()
	qt.Assert(t, rows > 0, qt.IsTrue,
		qt.Commentf("no rows installed, so the count above is vacuous"))
	qt.Assert(t, store.MacroPhasesWithRows() > 0, qt.IsTrue,
		qt.Commentf("no macro phase carries the vocabulary row; every transformer body would be starved"))
	qt.Assert(t, got > int64(rows), qt.IsTrue,
		qt.Commentf("%d resolutions over %d rows: a row that answers at most one name is a per-name install wearing a row's name", got, rows))
}
