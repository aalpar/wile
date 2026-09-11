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

// Unit pins for BulkSource and the bulk row.
//
// Plan: plans/2026-09-08-flatt-binding-model-a-impl.local.md, Task 4.
// These are internal (package environment) because bulkRef, its fields and the
// store's bulkRows slice are unexported, and because the properties under test
// are structural rather than behavioural.

import (
	"slices"
	"testing"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// sealAt writes one sealed binding into store at phase and returns it.
func sealAt(t *testing.T, owner *EnvironmentFrame, phase Phase, name string, v values.Value) *Binding {
	t.Helper()
	gi, created := owner.SealedWriteViewAt(phase).
		MaybeCreateOwnGlobalBinding(values.NewSymbol(name), BindingTypePrimitive, nil)
	qt.Assert(t, created, qt.IsTrue)
	err := owner.GlobalEnvironment().SetOwnGlobalValue(gi, v)
	qt.Assert(t, err, qt.IsNil)
	q := owner.GlobalEnvironment().GetOwnGlobalBinding(gi)
	qt.Assert(t, q, qt.IsNotNil)
	return q
}

// TestBulkSourceIsPerStoreAndPhase is D11's identity pin: one store, two
// phases, two sources, and they answer DIFFERENTLY for a name bound at both.
//
// The measured real-world case is syntax-rules, which carries a phase-0
// SyntaxCompiler and a phase-1 PrimitiveExpander from two unrelated
// registrations. That is the case a single source with a phase PARAMETER could
// not express without pushing the question to every call site, which is the
// design D11 rejects.
func TestBulkSourceIsPerStoreAndPhase(t *testing.T) {
	ns := NewNamespace()
	owner := ns.Runtime()

	atZero := sealAt(t, owner, PhaseRuntime, "two-phase-name", values.NewInteger(10))
	atOne := sealAt(t, owner, PhaseExpand, "two-phase-name", values.NewInteger(11))
	qt.Assert(t, atZero, qt.Not(qt.Equals), atOne,
		qt.Commentf("the two phases must hold DIFFERENT bindings or the pin is vacuous"))

	store := owner.GlobalEnvironment()
	src0 := NewStoreBulkSource(store, PhaseRuntime, values.NewSymbol("src-at-0"))
	src1 := NewStoreBulkSource(store, PhaseExpand, values.NewSymbol("src-at-1"))

	sym := *values.NewSymbol("two-phase-name")

	got0, ok := src0.LookupExport(sym)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, got0, qt.Equals, atZero)

	got1, ok := src1.LookupExport(sym)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, got1, qt.Equals, atOne)

	// A name held at ONE phase only must be supplied by exactly one source.
	sealAt(t, owner, PhaseExpand, "expand-only", values.NewInteger(1))
	_, ok = src0.LookupExport(*values.NewSymbol("expand-only"))
	qt.Assert(t, ok, qt.IsFalse,
		qt.Commentf("a phase-0 source must not supply a phase-1-only name"))
	_, ok = src1.LookupExport(*values.NewSymbol("expand-only"))
	qt.Assert(t, ok, qt.IsTrue)
}

// TestBulkRowIsLiveNotSnapshot is design section 6.1's sixth row, and it is the
// stage's headline property: a bulk row is a LIVE REFERENCE to the source
// store, not a snapshot.
//
// Everything order-independent about the import edge follows from it. Rows are
// installed at engine origin, BEFORE LoadBootstrapCore writes the base, so a
// row that snapshotted would supply nothing at all. Until this test existed the
// property appeared only as prose in two plan documents; on a tree with no bulk
// rows a snapshot and a live reference are indistinguishable, so it would have
// shipped untested. Stage C's serialization depends on it.
func TestBulkRowIsLiveNotSnapshot(t *testing.T) {
	ns := NewNamespace()
	owner := ns.Runtime()
	store := owner.GlobalEnvironment()

	src := NewStoreBulkSource(store, PhaseRuntime, values.NewSymbol("live-src"))

	// Resolve a miss BEFORE the name exists — the row is already installed.
	sym := *values.NewSymbol("added-after-the-row")
	_, ok := src.LookupExport(sym)
	qt.Assert(t, ok, qt.IsFalse)

	added := sealAt(t, owner, PhaseRuntime, "added-after-the-row", values.NewInteger(42))

	got, ok := src.LookupExport(sym)
	qt.Assert(t, ok, qt.IsTrue,
		qt.Commentf("a name added to the source AFTER the source was minted must resolve through it"))
	qt.Assert(t, got, qt.Equals, added)
}

// TestBulkSourceExportNamesAgreesWithLookup pins that the enumeration and the
// lookup answer the same question.
//
// They are two different code paths — ExportNames walks the store's key set and
// filters, LookupExport probes one name — and a source whose enumeration
// over-reported would make the R7RS section 5.6 conflict detection raise on
// names it cannot actually supply.
func TestBulkSourceExportNamesAgreesWithLookup(t *testing.T) {
	ns := NewNamespace()
	owner := ns.Runtime()
	store := owner.GlobalEnvironment()

	sealAt(t, owner, PhaseRuntime, "at-zero-a", values.NewInteger(1))
	sealAt(t, owner, PhaseRuntime, "at-zero-b", values.NewInteger(2))
	sealAt(t, owner, PhaseExpand, "at-one", values.NewInteger(3))

	src := NewStoreBulkSource(store, PhaseRuntime, values.NewSymbol("s"))

	names := []string{}
	for n := range src.ExportNames() {
		_, ok := src.LookupExport(n)
		qt.Assert(t, ok, qt.IsTrue,
			qt.Commentf("ExportNames yielded %q but LookupExport refuses it", n.Key))
		names = append(names, n.Key)
	}
	slices.Sort(names)
	qt.Assert(t, slices.Contains(names, "at-zero-a"), qt.IsTrue)
	qt.Assert(t, slices.Contains(names, "at-zero-b"), qt.IsTrue)
	qt.Assert(t, slices.Contains(names, "at-one"), qt.IsFalse,
		qt.Commentf("a phase-0 source must not enumerate a phase-1-only name"))
}

// TestBulkSourceNameIsADatum pins that a row's identity is serializable.
//
// SourceName must never be a Go pointer: Stage C writes a row into a
// pre-compiled binary, and the R7RS section 5.6 diagnostics name both libraries
// through it. A datum survives both; a pointer survives neither.
func TestBulkSourceNameIsADatum(t *testing.T) {
	ns := NewNamespace()
	name := values.NewSymbol("(scheme base)")
	src := NewStoreBulkSource(ns.Runtime().GlobalEnvironment(), PhaseRuntime, name)
	got := src.SourceName()
	qt.Assert(t, got, qt.Equals, values.Value(name))
}

// TestCopyCarriesBulkRowsAndRepointsSelfReferential pins A18's decision.
//
// Copy's own comment records this exact defect for exactPhases — "leaving it
// empty was the defect that made a copied namespace's phase-2 bindings
// unreachable" — and a dropped bulk row is that defect with a larger blast
// radius: after Stage A a report env would lose the entire base at once. The
// matrix cannot catch it, because its report-env row is pinned on cond, which
// has a per-symbol phase-1 slot.
//
// The re-pointing half is what keeps NewSchemeReportNamespace's stated contract
// that "q aliases nothing". A row copied verbatim would still read the ORIGINAL
// store, so a resolution in the copy would materialize a slot holding the
// original's *Binding and a set! through the copy would reach the parent.
func TestCopyCarriesBulkRowsAndRepointsSelfReferential(t *testing.T) {
	ns := NewNamespace()
	owner := ns.Runtime()
	store := owner.GlobalEnvironment()

	// A self-referential row: the source reads the very store being copied.
	selfSrc := NewStoreBulkSource(store, PhaseRuntime, values.NewSymbol("self"))
	store.InstallBulkRow(selfSrc, nil, PhaseRuntime, true)

	// A foreign row: the source reads a DIFFERENT owner's store, which is what a
	// genuine library import looks like.
	other := ns.NewChildRuntime()
	foreignSrc := NewStoreBulkSource(other.GlobalEnvironment(), PhaseRuntime, values.NewSymbol("other"))
	store.InstallBulkRow(foreignSrc, nil, PhaseRuntime, true)

	qt.Assert(t, store.BulkRowCount(), qt.Equals, 2)

	cp := store.Copy()
	qt.Assert(t, cp.BulkRowCount(), qt.Equals, 2,
		qt.Commentf("Copy must carry bulk rows; dropping them costs a report env every bulk-supplied name"))

	cpSelf, ok := cp.bulkRows[0].src.(*storeBulkSource)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, cpSelf.store, qt.Equals, cp,
		qt.Commentf("a self-referential row must be re-pointed at the copy, or the copy aliases the original"))
	qt.Assert(t, cpSelf.phase, qt.Equals, PhaseRuntime)

	cpForeign, ok := cp.bulkRows[1].src.(*storeBulkSource)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, cpForeign.store, qt.Equals, other.GlobalEnvironment(),
		qt.Commentf("a foreign row must keep pointing at the exporter, which importers share on master too"))
}

// TestBulkRefCarriesItsOwnScopes pins A12: bulkRef does NOT mirror slotRef.
//
// slotRef is {slot, phase, sealed} with no scopes field — a per-symbol
// candidate's scope set is read off its *Binding, which is where
// probeTiersLocked gets it. A bulk row stands for many bindings with many
// different scope sets, so it must carry one of its own, and scopes is
// therefore a NEW axis. Stage B's "one fold covers both in a single move"
// obligation is correspondingly weaker than design section 7 states: it covers
// phase and sealed; scopes has to be handled separately.
func TestBulkRefCarriesItsOwnScopes(t *testing.T) {
	ns := NewNamespace()
	store := ns.Runtime().GlobalEnvironment()

	scope := syntax.NewScope()
	src := NewStoreBulkSource(store, PhaseRuntime, values.NewSymbol("s"))
	store.InstallBulkRow(src, []*syntax.Scope{scope}, PhaseRuntime, true)

	store.mu.RLock()
	defer store.mu.RUnlock()
	qt.Assert(t, store.bulkRows, qt.HasLen, 1)
	qt.Assert(t, store.bulkRows[0].scopes, qt.HasLen, 1)
	qt.Assert(t, store.bulkRows[0].scopes[0], qt.Equals, scope)
	qt.Assert(t, store.bulkRows[0].phase, qt.Equals, PhaseRuntime)
	qt.Assert(t, store.bulkRows[0].sealed, qt.IsTrue)
}

// TestBaseSourceNameIsReservedAndUnimportable pins Task 4 Step 3's half of
// design section 4.1: the base gets a source identity without getting an
// importable library name.
//
// The spelling is deliberately not a legal library name a user could write, so
// nothing can collide with it and no .sld can claim it. The other half of the
// property — that (import (wile base)) fails — is not assertable here, since
// pkg/environment has no library registry; it is stated in BaseSourceName's own
// comment and exercised by the pkg/wile pins that import (scheme base) instead.
func TestBaseSourceNameIsReservedAndUnimportable(t *testing.T) {
	name := BaseSourceName()
	sym, ok := name.(*values.Symbol)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("the base's identity must be a datum, not a pointer"))
	qt.Assert(t, sym.Key, qt.Equals, "#%wile-base")
}

// TestBulkRowsCarryTheEmptyScopeSet pins the premise the resolution fast path
// rests on.
//
// resolveRankedLocked consults bulk rows only when the per-symbol probe MISSES.
// That is the tie-break rule exactly — a row is sealed, hence T2, so a T1 slot
// outranks it; a T2 slot ties on tier and, with both scope sets empty, the
// tie-break awards the slot; and no slot loses on cardinality to an empty set —
// but ONLY while every installed row carries the empty scope set. A row with a
// non-empty set could outrank a slot on cardinality, and the miss-only shape
// would silently never let it.
//
// So the premise is a gate, not a comment. Stage B's move of the phase INTO the
// scope set is what would create the first non-empty row; when it does, this
// test is the thing that says the fast path has to become the full argmax.
func TestBulkRowsCarryTheEmptyScopeSet(t *testing.T) {
	ns := NewNamespace()
	store := ns.Runtime().GlobalEnvironment()
	src := NewSealedStoreBulkSource(store, PhaseRuntime, BaseSourceName())
	store.InstallBulkRow(src, nil, PhaseExpand, true)

	store.mu.RLock()
	defer store.mu.RUnlock()
	for i, row := range store.bulkRows {
		qt.Assert(t, row.scopes, qt.HasLen, 0,
			qt.Commentf("row %d carries %d scopes; resolveRankedLocked's miss-only fast path is then unsound", i, len(row.scopes)))
	}
}

// TestSealedBaseSourceExcludesImports pins the predicate that separates the
// base from an import when the two share a coordinate.
//
// They do share one: an import installs at (phase 0, sealed), which is
// exactly where the base's own writes land once the ambient branch is gone, and
// there is no third coordinate to move either onto. So the base source draws the
// line on Imported meta instead — the same fact importConflicts keys on.
//
// Without it a library-private name imported at phase 0 resolves inside a
// transformer body, measured.
func TestSealedBaseSourceExcludesImports(t *testing.T) {
	ns := NewNamespace()
	owner := ns.Runtime()
	store := owner.GlobalEnvironment()

	ownInstall := sealAt(t, owner, PhaseRuntime, "engine-own", values.NewInteger(1))
	qt.Assert(t, ownInstall.IsImported(), qt.IsFalse)

	imported := sealAt(t, owner, PhaseRuntime, "looks-imported", values.NewInteger(2))
	imported.UpdateMeta(func(m *BindingMeta) bool {
		m.Imported = true
		return true
	})
	qt.Assert(t, imported.IsImported(), qt.IsTrue)

	base := NewSealedStoreBulkSource(store, PhaseRuntime, BaseSourceName())

	_, ok := base.LookupExport(*values.NewSymbol("engine-own"))
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("the base source must supply what the engine installed"))
	_, ok = base.LookupExport(*values.NewSymbol("looks-imported"))
	qt.Assert(t, ok, qt.IsFalse, qt.Commentf("the base source must NOT supply an imported binding"))

	// The unrestricted form is the foreign-store shape and supplies both, which
	// is why the two constructors exist.
	foreign := NewStoreBulkSource(store, PhaseRuntime, values.NewSymbol("lib"))
	_, ok = foreign.LookupExport(*values.NewSymbol("looks-imported"))
	qt.Assert(t, ok, qt.IsTrue)
}
