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
