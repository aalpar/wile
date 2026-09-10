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
	got := eng.Namespace().Runtime().GlobalEnvironment().BulkRowCount()
	qt.Assert(t, got, qt.Equals, len(declared),
		qt.Commentf("origin installed %d rows for %d declarations", got, len(declared)))
	qt.Assert(t, got > 0, qt.IsTrue,
		qt.Commentf("a zero count would make this ratchet vacuous"))
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

	got := eng.Namespace().Runtime().GlobalEnvironment().BulkRowCount()
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
	qt.Assert(t, declared, qt.HasLen, 2)
	qt.Assert(t, declared[0].Phase, qt.Equals, environment.PhaseRuntime)
	qt.Assert(t, declared[1].Phase, qt.Equals, environment.PhaseExpand)
	// Both rows name the SAME source. Two rows over one source at two install
	// phases is the shape a single Phase field would be ambiguous about, and it
	// is the default dialect's actual declaration.
	qt.Assert(t, declared[0].Library, qt.Equals, declared[1].Library)
}
