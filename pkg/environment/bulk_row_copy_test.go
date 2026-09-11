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

package environment

// Store-level pins for what GlobalEnvironmentFrame.Copy must carry.
//
// Plan: plans/2026-09-10-flatt-binding-model-b-impl.local.md, Task 2.
//
// These are ANSWER pins, not count pins, and the distinction is the whole
// reason the file exists. BulkRowCount is EQUAL across the defect — measured,
// 2 rows before and 2 after — because a row Copy failed to re-point is still a
// row. It is simply INERT: materializeBulkLocked requires store == p, so a row
// still pointed at the parent finds the parent's *Binding, fails to turn it
// into a slot of the copy, and resolution falls through to a miss. Only asking
// whether a name RESOLVES through a copied row can see that.
//
// In-package because bulkRef, storeBulkSource's restriction fields and the
// store's row slices are unexported, and because two of the three properties
// (the carried minTier, the carried templates) are structural.
//
// The Scheme-level face of the same three defects is
// pkg/wile/report_env_macro_vocabulary_test.go.

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// vocabularySourceName is the row identity these pins use. The engine's own
// spelling is dialect.go's MacroVocabularyName; pkg/environment cannot import
// pkg/wile, so the shape is rebuilt here rather than borrowed.
func vocabularySourceName() values.Value {
	return values.NewSymbol("#%test-vocabulary")
}

// vocabularyRow builds the SHAPE engine.go's installInitialImports installs at
// every macro phase: a sealed store source — restricted to the sealed tier and
// to the owner's own installs — wrapped in a name filter.
//
// The wrapper is not decoration. It is the half Copy's *storeBulkSource type
// assertion skipped, so a pin built on a bare storeBulkSource would pass
// against the defect.
func vocabularyRow(store *GlobalEnvironmentFrame, admitted ...string) BulkSource {
	set := make(map[string]struct{}, len(admitted))
	for _, name := range admitted {
		set[name] = struct{}{}
	}
	admits := func(name string) bool {
		_, ok := set[name]
		return ok
	}
	return NewFilteredBulkSource(
		NewSealedStoreBulkSource(store, PhaseRuntime, vocabularySourceName()),
		admits,
		set,
		vocabularySourceName(),
	)
}

// resolveThroughRows resolves name at phase through resolveRankedLocked, the one
// probe that consults bulk rows, and returns the binding it lands on.
//
// The ranked probe rather than ExactBindingAt: the latter goes straight to
// probeTiersLocked and never reaches a row, so it would report a miss for both
// a working copy and a broken one.
func resolveThroughRows(t *testing.T, store *GlobalEnvironmentFrame, name string, phase Phase) (*Binding, bool) {
	t.Helper()
	store.mu.RLock()
	defer store.mu.RUnlock()

	ref, ok := store.resolveRankedLocked(*values.NewSymbol(name), syntax.EmptyScopes(), phase)
	if !ok {
		return nil, false
	}
	return store.bindings[ref.slot], true
}

// unwrapStoreSource peels the wrappers off a source and returns the
// storeBulkSource underneath.
func unwrapStoreSource(t *testing.T, src BulkSource) *storeBulkSource {
	t.Helper()
	switch v := src.(type) {
	case *storeBulkSource:
		return v
	case *filteredBulkSource:
		return unwrapStoreSource(t, v.inner)
	case *renamedBulkSource:
		return unwrapStoreSource(t, v.inner)
	default:
		t.Fatalf("source %T wraps no storeBulkSource", src)
		return nil
	}
}

// TestCopiedBulkRowAnswersInTheCopy is the ANSWER pin: a name the parent
// reaches through a bulk row must still be reachable through the COPY's row,
// and must land on the COPY's binding.
//
// RED on master: the row's source is a filteredBulkSource, which Copy's type
// assertion skipped, so the copy's row still reads the parent. The lookup
// succeeds against the parent's store and materializeBulkLocked then refuses
// it, because the binding it found is not a slot of p — so resolution reports a
// MISS while the row count is unchanged.
func TestCopiedBulkRowAnswersInTheCopy(t *testing.T) {
	ns := NewNamespace()
	owner := ns.Runtime()
	store := owner.GlobalEnvironment()

	own := sealAt(t, owner, PhaseRuntime, "vocab-name", values.NewInteger(1))
	store.InstallBulkRow(vocabularyRow(store, "vocab-name"), nil, ExactPhase(PhaseExpand), true)

	// The control: the parent answers at phase 1 through the row, since the name
	// has no phase-1 slot of its own.
	parentBnd, ok := resolveThroughRows(t, store, "vocab-name", PhaseExpand)
	qt.Assert(t, ok, qt.IsTrue,
		qt.Commentf("the row must answer in the parent, or the pin measures nothing"))
	qt.Assert(t, parentBnd == own, qt.IsTrue)

	cp := store.Copy()
	qt.Assert(t, cp.BulkRowCount(), qt.Equals, store.BulkRowCount(),
		qt.Commentf("the count is EQUAL across the defect; it is a guard here, not the pin"))

	copyBnd, ok := resolveThroughRows(t, cp, "vocab-name", PhaseExpand)
	qt.Assert(t, ok, qt.IsTrue,
		qt.Commentf("a copied row must be live in the copy, not merely present"))
	qt.Assert(t, copyBnd, qt.IsNotNil)
	qt.Assert(t, copyBnd != own, qt.IsTrue,
		qt.Commentf("the copy must resolve to its OWN binding; landing on the parent's is the aliasing NewSchemeReportNamespace promises not to do"))
	qt.Assert(t, copyBnd.Value(), qt.Equals, own.Value())
}

// TestCopiedSealedRowKeepsItsRestrictions is the Step 5 ratchet: BOTH fields
// that make a sealed source mean "the base" survive the copy.
//
// The two arms are not equally discriminating today, and saying so is part of
// the pin. minTier is observable: reconstructing the source through
// NewStoreBulkSource floors it at tierExactMutable, and a phase-0 MUTABLE name
// then leaks into phase 1 through the row. ownInstallsOnly is NOT separately
// observable — an imported binding ranks at tierExactImported, which is already
// below the tierExactSealed floor, so the behavioural arm below is refused
// twice over. That redundancy is the field's stated reason for still existing
// (see storeBulkSource.ownInstallsOnly), and it is why the field is pinned
// STRUCTURALLY as well. Task 6 deletes the field; when it does, the structural
// assertion and the imported arm's second reason go with it.
func TestCopiedSealedRowKeepsItsRestrictions(t *testing.T) {
	ns := NewNamespace()
	owner := ns.Runtime()
	store := owner.GlobalEnvironment()

	sealAt(t, owner, PhaseRuntime, "ratchet-own", values.NewInteger(1))

	// A phase-0 MUTABLE binding: below a sealed source's tier floor.
	_, err := owner.DefineOwnGlobal(values.NewSymbol("ratchet-mutable"), BindingTypeVariable, nil, values.NewInteger(2))
	qt.Assert(t, err, qt.IsNil)

	// A phase-0 IMPORTED binding: ranks at tierExactImported, also below the
	// floor, and carries the Imported meta ownInstallsOnly refuses.
	gi, created := store.CreateImportedGlobalBindingAt(values.NewSymbol("ratchet-imported"),
		BindingTypeVariable, nil, ExactPhase(PhaseRuntime), true)
	qt.Assert(t, created, qt.IsTrue)
	err = store.SetOwnGlobalValue(gi, values.NewInteger(3))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, store.GetOwnGlobalBinding(gi).IsImported(), qt.IsTrue)

	store.InstallBulkRow(
		vocabularyRow(store, "ratchet-own", "ratchet-mutable", "ratchet-imported"),
		nil, ExactPhase(PhaseExpand), true)

	cp := store.Copy()
	qt.Assert(t, cp.BulkRowCount(), qt.Equals, 1)

	cp.mu.RLock()
	copied := cp.bulkRows[0].src
	cp.mu.RUnlock()

	inner := unwrapStoreSource(t, copied)
	qt.Assert(t, inner.store == cp, qt.IsTrue,
		qt.Commentf("the re-point must reach THROUGH the filter to the store source"))
	qt.Assert(t, inner.minTier, qt.Equals, tierExactSealed,
		qt.Commentf("minTier must be CARRIED; reconstructing through NewStoreBulkSource floors it at tierExactMutable"))
	qt.Assert(t, inner.ownInstallsOnly, qt.IsTrue,
		qt.Commentf("ownInstallsOnly must be carried too, until Task 6 proves it dead and deletes it"))

	rows := []struct {
		name string
		want bool
		why  string
	}{
		{name: "ratchet-own", want: true,
			why: "a sealed own install is what the row is FOR"},
		{name: "ratchet-mutable", want: false,
			why: "a phase-0 mutable name reaching phase 1 through the row is the collapse of phase hermeticity"},
		{name: "ratchet-imported", want: false,
			why: "an import must not be supplied as if it were the base"},
	}
	for _, row := range rows {
		t.Run(row.name, func(t *testing.T) {
			_, ok := resolveThroughRows(t, cp, row.name, PhaseExpand)
			qt.Assert(t, ok, qt.Equals, row.want, qt.Commentf("%s", row.why))
		})
	}
}

// TestCopyCarriesMacroPhaseTemplatesWithoutDuplicating pins the third fault and
// the trap that fixing it naively introduces.
//
// Three distinct failures, each caught by its own assertion:
//
//   - Templates dropped (master): a phase the copy reaches for the FIRST time
//     gets no row, so the vocabulary is gone from every phase above the ones the
//     parent happened to have materialized.
//   - Templates carried, seen-set zeroed: installMacroRowLocked appends
//     UNCONDITIONALLY, so EnsureMacroPhaseRows re-installs at a phase whose
//     materialized row Copy already carried. The row count grows, silently.
//   - Templates carried but not re-pointed: the newly installed row reads the
//     PARENT, so it is inert for exactly the reason a carried-but-not-re-pointed
//     bulk row is.
func TestCopyCarriesMacroPhaseTemplatesWithoutDuplicating(t *testing.T) {
	ns := NewNamespace()
	owner := ns.Runtime()
	store := owner.GlobalEnvironment()

	own := sealAt(t, owner, PhaseRuntime, "tower-name", values.NewInteger(7))
	store.InstallMacroPhaseRow(vocabularyRow(store, "tower-name"), nil, true)

	// Minting the phase-1 view materializes the template there.
	owner.AtPhase(PhaseExpand)
	qt.Assert(t, store.BulkRowCount(), qt.Equals, 1)

	rep := ns.NewSchemeReportNamespace()
	cp := rep.Runtime().GlobalEnvironment()
	qt.Assert(t, cp != store, qt.IsTrue)

	cp.mu.RLock()
	templates := len(cp.macroPhaseRows)
	cp.mu.RUnlock()
	qt.Assert(t, templates, qt.Equals, 1,
		qt.Commentf("the templates must be carried, or a phase the copy reaches first gets no vocabulary row"))

	// The already-materialized row answers in the copy, on the copy's binding.
	carried, ok := resolveThroughRows(t, cp, "tower-name", PhaseExpand)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, carried != own, qt.IsTrue)

	// Minting phase 1 in the COPY must be a no-op: Copy carried the seen-set
	// alongside the rows, so the template is not installed a second time.
	rep.Runtime().AtPhase(PhaseExpand)
	qt.Assert(t, cp.BulkRowCount(), qt.Equals, 1,
		qt.Commentf("re-installing a template at a phase whose row Copy already carried duplicates the row"))

	// A phase the parent never reached installs from the carried template, and
	// the row it installs must read the COPY.
	rep.Runtime().AtPhase(PhaseExpand + 1)
	qt.Assert(t, cp.BulkRowCount(), qt.Equals, 2)
	fresh, ok := resolveThroughRows(t, cp, "tower-name", PhaseExpand+1)
	qt.Assert(t, ok, qt.IsTrue,
		qt.Commentf("a template installed at a new phase must be live, which needs the template re-pointed at the copy"))
	qt.Assert(t, fresh == carried, qt.IsTrue,
		qt.Commentf("both phases must reach the SAME slot of the copy"))

	// The parent is untouched by any of it.
	qt.Assert(t, store.BulkRowCount(), qt.Equals, 1)
}
