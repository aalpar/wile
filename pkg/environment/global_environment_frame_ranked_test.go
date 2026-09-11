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

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// The ranked probe over a hand-built mixed store (design §4.3):
// tierExactMutable outranks the sealed tiers at the SAME phase; a slot at any
// OTHER phase is not a candidate at all; maximal scope cardinality ranks within
// the winning tier only.
//
// The query phase's own slots are now the WHOLE candidate set. Stage A deleted
// the phase-blind ambient tier and Stage B the coordinate itself, so this
// table's third case, "ambient visible from every phase", has no subject; the
// property that replaced it — cross-phase visibility supplied by a bulk row — is
// TestResolveRankedCrossPhaseNeedsABulkRow below.
//
// Every sealed slot below is created UNSTAMPED, so it ranks tierExactSealed, not
// tierExactImported. Only CreateImportedGlobalBindingAt reaches the latter.
func TestResolveRankedTiers(t *testing.T) {
	sym := values.NewSymbol("v")
	mk := func(entries []struct {
		phase  Phase
		sealed bool
	}) *GlobalEnvironmentFrame {
		q := NewGlobalEnvironmentFrame()
		for _, e := range entries {
			q.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, e.phase, e.sealed)
		}
		return q
	}
	type probe struct {
		phase    Phase
		wantSlot int
		wantOK   bool
	}
	tcs := []struct {
		name    string
		entries []struct {
			phase  Phase
			sealed bool
		}
		probes []probe
	}{
		{name: "mutable beats sealed",
			entries: []struct {
				phase  Phase
				sealed bool
			}{
				{0, true},  // slot 0: tierExactSealed at 0
				{0, false}, // slot 1: tierExactMutable at 0
			},
			probes: []probe{
				{phase: 0, wantSlot: 1, wantOK: true},
				// Neither slot is a candidate one phase up: a sealed write is
				// exact-phase like any other, so it no longer leaks upward.
				{phase: 1, wantOK: false},
			}},
		{name: "other exact phase is no candidate",
			entries: []struct {
				phase  Phase
				sealed bool
			}{
				{1, false},
			},
			probes: []probe{
				{phase: 0, wantOK: false},
				{phase: 1, wantSlot: 0, wantOK: true},
				{phase: 2, wantOK: false},
			}},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			g := mk(tc.entries)
			for _, pr := range tc.probes {
				// Anonymous function per iteration so the RUnlock fires at the end
				// of THIS probe, not accumulated via defer to the end of the
				// subtest — resolveRankedLocked's own doc requires defer release
				// (it can panic mid-hold), but stacking that defer across loop
				// iterations would hold the lock across every later probe too.
				ref, ok := func() (slotRef, bool) {
					g.mu.RLock()
					defer g.mu.RUnlock()
					return g.resolveRankedLocked(*sym, syntax.EmptyScopes(), pr.phase)
				}()
				qt.Assert(t, ok, qt.Equals, pr.wantOK, qt.Commentf("phase %d", pr.phase))
				if pr.wantOK {
					qt.Assert(t, ref.slot, qt.Equals, pr.wantSlot, qt.Commentf("phase %d", pr.phase))
				}
			}
		})
	}
}

// Cross-phase visibility is a BULK ROW's property now, not a coordinate's, and
// this is what TestResolveRankedTiers' "ambient visible from every phase" case
// became. Before Stage A a sealed phase-0 write landed at (ANY, sealed) and
// every phase's probe reached it; it now lands at (phase 0, sealed), and
// phase 1 reaches it only because the dialect declared the base at phase 1,
// which installs a row over the same store at (phase 1, sealed).
//
// The probe BEFORE the row is installed is the control: resolveRankedLocked
// consults rows only on a per-symbol miss, so a pass without that leg would not
// distinguish the row doing the work from the coordinate still doing it.
func TestResolveRankedCrossPhaseNeedsABulkRow(t *testing.T) {
	sym := values.NewSymbol("v")
	g := NewGlobalEnvironmentFrame()
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, PhaseRuntime, true)

	// Anonymous function per probe so the RUnlock fires at the end of THIS
	// probe: resolveRankedLocked's own doc requires defer release (it can panic
	// mid-hold), and InstallBulkRow between two probes takes the write lock.
	probeAt := func(phase Phase) (slotRef, bool) {
		g.mu.RLock()
		defer g.mu.RUnlock()
		return g.resolveRankedLocked(*sym, syntax.EmptyScopes(), phase)
	}

	ref, ok := probeAt(PhaseRuntime)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, ref.slot, qt.Equals, 0)
	_, ok = probeAt(PhaseExpand)
	qt.Assert(t, ok, qt.IsFalse,
		qt.Commentf("a sealed phase-0 slot must not be reachable from phase 1 on its own"))

	g.InstallBulkRow(NewSealedStoreBulkSource(g, PhaseRuntime, BaseSourceName()),
		nil, PhaseExpand, true, BulkOriginLanguage)

	ref, ok = probeAt(PhaseExpand)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, ref.slot, qt.Equals, 0,
		qt.Commentf("the row must resolve to the phase-0 slot ITSELF; a copy would fork later writes"))
	qt.Assert(t, g.BulkResolutionCount(), qt.Equals, int64(1))
}

// A tie in a LOSING tier must not panic: rank decides first, ambiguity is only
// asked of the winning tier (P8 scoped to the probe).
func TestResolveRankedAmbiguityScopedToWinningTier(t *testing.T) {
	sym := values.NewSymbol("v")
	sc1 := syntax.NewScope()
	sc2 := syntax.NewScope()
	g := NewGlobalEnvironmentFrame()
	// Two incomparable tierExactSealed candidates...
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, []*syntax.Scope{sc1}, 0, true)
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, []*syntax.Scope{sc2}, 0, true)
	// ...and one tierExactMutable winner.
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, 0, false)

	g.mu.RLock()
	defer g.mu.RUnlock()
	ref, ok := g.resolveRankedLocked(*sym, syntax.ScopesOf([]*syntax.Scope{sc1, sc2}), 0)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, ref.slot, qt.Equals, 2)

	// With the mutable winner gone, the sealed tie is the winning tier and must panic.
	g2 := NewGlobalEnvironmentFrame()
	g2.CreateGlobalBindingAt(sym, BindingTypeVariable, []*syntax.Scope{sc1}, 0, true)
	g2.CreateGlobalBindingAt(sym, BindingTypeVariable, []*syntax.Scope{sc2}, 0, true)
	qt.Assert(t, func() {
		g2.mu.RLock()
		defer g2.mu.RUnlock()
		g2.resolveRankedLocked(*sym, syntax.ScopesOf([]*syntax.Scope{sc1, sc2}), 0)
	}, qt.PanicMatches, ".*ambiguous.*")
}

// Coordinate identity on CREATE: scope-set equality alone must NOT reuse a slot
// at different coordinates — that reuse rule is what makes a phase-0 define a
// SHADOW of the sealed entry rather than a supersede once the stores merge, and
// equally what makes a define-for-syntax over the registry's (1, sealed) expand
// copy a shadow.
func TestCreateMatchesCoordinatesAndScopes(t *testing.T) {
	// The wildcard row this used to open with is gone: Stage A refuses that
	// coordinate at the write API, so the two coordinates that differ meaningfully
	// now are (0, sealed) and (0, mutable) — which is the pair the doc above is
	// actually about, since it is a phase-0 define shadowing the sealed entry.
	sym := values.NewSymbol("v")
	g := NewGlobalEnvironmentFrame()
	_, created := g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, 0, true)
	qt.Assert(t, created, qt.IsTrue)
	// Same scopes (∅), different coordinates: a NEW slot.
	_, created = g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, 0, false)
	qt.Assert(t, created, qt.IsTrue)
	// Same scopes, same coordinates: reuse.
	_, created = g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, 0, false)
	qt.Assert(t, created, qt.IsFalse)
}

// Copy's per-slot coordinate carry-forward is production-live: it is the whole
// of NewSchemeReportNamespace, which builds its store by copying the parent's
// rather than minting fresh. Silently dropping the stamps would collapse every
// sealed entry in a scheme-report namespace onto the zero value
// (phase 0, mutable), where it starts colliding with real phase-0 user
// defines instead of being shadowed by them.
//
// The two slots below are the two coordinates a write path actually mints at
// phase 0 since Stage A — sealed and mutable — rather than the ambient one this
// test used to open with, which nothing writes any more.
func TestCopyPreservesCoordinates(t *testing.T) {
	sym := values.NewSymbol("v")
	g := NewGlobalEnvironmentFrame()
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, PhaseRuntime, true)
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, PhaseRuntime, false)

	c := g.Copy()

	qt.Assert(t, len(c.keys[*sym]), qt.Equals, 2)
	qt.Assert(t, c.keys[*sym][0].phase, qt.Equals, PhaseRuntime)
	qt.Assert(t, c.keys[*sym][0].sealed, qt.IsTrue)
	qt.Assert(t, c.keys[*sym][1].phase, qt.Equals, PhaseRuntime)
	qt.Assert(t, c.keys[*sym][1].sealed, qt.IsFalse)
}

// The q.IsAll() wildcard branch is bespoke relative to the pre-fold wildcard
// path (a plain first-live-slot loop with no tier concept): it tracks
// tier across the whole slot list, applies the same phase filter the scoped
// branch does, and returns the highest tier's first live slot. Every existing
// wildcard caller that the fold moved onto the probe — resolveGlobal, and
// through it EnvironmentFrame.GetGlobalIndex — inherits this ranked behavior, so
// pin it here.
func TestResolveRankedWildcard(t *testing.T) {
	sym := values.NewSymbol("v")

	t.Run("tier order", func(t *testing.T) {
		// The ambient tier is gone, so these two are the whole order here.
		// tierExactMutable must still outrank tierExactSealed, which is what makes
		// a phase-0 define shadow the sealed base rather than assign through it.
		g := NewGlobalEnvironmentFrame()
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, 0, true)  // slot 0: tierExactSealed
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, 0, false) // slot 1: tierExactMutable

		g.mu.RLock()
		defer g.mu.RUnlock()
		ref, ok := g.resolveRankedLocked(*sym, syntax.AllScopes(), 0)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, ref.slot, qt.Equals, 1)
	})

	t.Run("other exact phase is not a candidate", func(t *testing.T) {
		g := NewGlobalEnvironmentFrame()
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, 1, false)

		g.mu.RLock()
		defer g.mu.RUnlock()
		ref, ok := g.resolveRankedLocked(*sym, syntax.AllScopes(), 0)
		qt.Assert(t, ok, qt.IsFalse)
		// The slot value on the walk-exhausted return, not the len(slots)==0
		// short-circuit: this frame HAS a slot for the name, the wildcard loop
		// runs and rejects it, and bestSlot must still be the zero the caller
		// is told to ignore. Decoupling bestSlot from bestTier would show up
		// only here.
		qt.Assert(t, ref.slot, qt.Equals, 0)
	})

	t.Run("nil'd slot is skipped", func(t *testing.T) {
		g := NewGlobalEnvironmentFrame()
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, 0, false)
		// A live key pointing at a nil binding — the pre-fold wildcard path
		// guarded exactly this state (a slot DeleteBinding emptied); the ranked
		// probe's wildcard branch must guard it identically.
		g.bindings[0] = nil

		g.mu.RLock()
		defer g.mu.RUnlock()
		ref, ok := g.resolveRankedLocked(*sym, syntax.AllScopes(), 0)
		qt.Assert(t, ok, qt.IsFalse)
		qt.Assert(t, ref.slot, qt.Equals, 0)
	})

	t.Run("no candidate at all", func(t *testing.T) {
		g := NewGlobalEnvironmentFrame()

		g.mu.RLock()
		defer g.mu.RUnlock()
		ref, ok := g.resolveRankedLocked(*sym, syntax.AllScopes(), 0)
		qt.Assert(t, ok, qt.IsFalse)
		qt.Assert(t, ref.slot, qt.Equals, 0)
	})
}

// A wider (larger-cardinality) scope set beats a narrower one INSIDE one tier.
// TestResolveRankedTiers only ever passes nil scopes, so it pins the tier gate
// but never the ranking scopedBestOf performs within the winning tier — the
// mechanism is shared with resolveAtCoordsLocked, but nothing pinned it here.
func TestResolveRankedCardinalityWithinTier(t *testing.T) {
	sym := values.NewSymbol("v")
	scA := syntax.NewScope()
	scB := syntax.NewScope()
	g := NewGlobalEnvironmentFrame()
	// Both tierExactMutable at phase 0; {scA} subset {scA, scB}.
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, []*syntax.Scope{scA}, 0, false)
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, []*syntax.Scope{scA, scB}, 0, false)

	g.mu.RLock()
	defer g.mu.RUnlock()
	ref, ok := g.resolveRankedLocked(*sym, syntax.ScopesOf([]*syntax.Scope{scA, scB}), 0)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, ref.slot, qt.Equals, 1) // the wider {scA, scB} slot wins
}

// TestTierOrdinalsHaveNotRenumbered is a TRIPWIRE, and it is the only
// enforcement behind the "name a tier by its identifier, never by an ordinal"
// convention stated at the tier enum.
//
// Eighteen comments across the tree still label THESE tiers "T1"/"T2"/"T3", in
// seven files — eight of them live labels, the other ten quotations of the wrong
// text they replaced or the convention statement itself. Each live one is an
// unchecked second copy of this enum's ordering: inserting tierExactImported
// renumbered everything below it, and every one of those labels silently changed
// referent with no test going red.
//
// Eighteen, not the "roughly a hundred" this doc claimed until 2026-09-11. That
// figure was unmeasured and counted two unrelated namespaces that share the
// spelling. Measured, 2026-09-11 —
//
//	grep -rEoh '\bT[123]\b' --include=*.go pkg/ | wc -l    # 81
//
// — of which forty-seven are the CLOSURE tiers of design section 5.3.1
// (boxing.go, compile_closure.go, operations_closure.go,
// flat_closure_ratchet_test.go) and sixteen belong to the architectural review.
// Those are correct where they stand; see the RED instruction at the bottom of
// this doc before acting on the grep.
//
// A site-COUNT ratchet would not have caught that and is the wrong shape here —
// the population does not change when a tier is inserted, so a count stays green
// through exactly the event that breaks the labels. It guards against new labels
// being written, which is not the failure that happened. This pins the ORDINALS
// instead, so it fails on the event that moves a label's referent.
//
// WHAT IT DOES NOT DO, stated so nobody mistakes its green for more than it is:
//
//   - It pins the ENUM, never the labels. It certifies only that no label's
//     referent has moved SINCE THIS BASELINE. It cannot tell you whether the
//     labels were right when the baseline was taken, and it froze a baseline it
//     never validated. That is not hypothetical: resolveRankedLocked's
//     bulk-consultation tie-break premise, which sits above the tier enum in
//     global_environment_frame.go, said "hence T2 … a T2 slot ties it" about a
//     tierExactSealed row, and sat green under this very test until it was read
//     by hand.
//   - A tier that keeps its VALUE but changes its meaning does not redden it.
//     Values are all it compares.
//   - APPENDING a tier after tierExactSealed correctly does not redden it: no
//     existing ordinal moves, so no existing label changes referent. That is the
//     test working, not a gap. It used to be label-safe and REACHABILITY-unsafe
//     as well — every ranked ceiling was the literal tierExactSealed passed as a
//     maxTier argument, so an appended tier failed `t > maxTier` in every reader
//     and resolved nothing, silently. That half is closed: the ceiling is
//     tierHighest, derived from the enum's own tierCount, so appending a tier
//     extends it. What an append still needs a human for is the FLOORS —
//     SealedBindingAt and SealedGlobalIndexAt floor at tierExactSealed, and
//     whether a newly appended tier belongs above that floor is a question the
//     enum cannot answer.
//
// It catches insertion, reorder and mid-enum removal — the ways an ordinal's
// referent actually moves — and a rename or an outright removal breaks the
// build here instead.
//
// It also guards tierCount's POSITION, which is a second job and a newer one.
// Since 2026-09-10 the ranked ceiling is tierHighest = tierCount - 1 rather than
// the literal tierExactSealed passed as a maxTier argument, which is what makes
// appending a tier extend the ceiling instead of leaving the new tier dead. That
// derivation is only as good as where tierCount sits: a tier declared AFTER the
// sentinel leaves tierHighest at tierExactSealed and re-opens the very defect,
// moving no ordinal and reddening nothing the ordinal loop checks.
//
// So the position is asserted against the TABLE, not against tierExactSealed.
// The obvious form, tierCount == tierExactSealed+1, is green in exactly the case
// it is there to catch (verified by appending a tier past the sentinel and
// watching it pass), because moving the sentinel is not what goes wrong —
// declaring a tier past it is. tierCount == len(tcs) catches that, and it
// inherits the ordinal loop's one dependency: the table has to name every tier.
// That dependency is the residual. If you append a tier and update NOTHING here,
// both halves stay green and your tier resolves nothing; the doc block below is
// the only thing that tells you so. The position-independent alternative — delete
// the ceiling comparison entirely, since after the maxTier parameter went away no
// reader wants a ceiling below the top — was considered and not taken, because
// the fold-in asked for a named constant.
//
// HOW TO APPEND A TIER: declare it BEFORE tierCount, add a row to the table
// below, and re-read the sealed FLOORS (SealedBindingAt, SealedGlobalIndexAt)
// named further down.
//
// IF THIS TEST IS RED because you added, removed or reordered a tier: that is
// the test working, not a stale expectation. Every ordinal label FOR THESE TIERS
// now denotes a different constant. Rewrite them to name the identifier
// (preferred — see the enum's doc for why), or renumber them, then update the
// table here.
//
// The labels this means are the EIGHTEEN counted above, not every `\bT[123]\b`
// in the tree: `grep -rEoh '\bT[123]\b' --include=*.go pkg/` also returns the
// closure tiers and the review item IDs, and rewriting those would corrupt
// correct comments. Start from the files that name a tier IDENTIFIER
// (tierExactMutable / tierExactImported / tierExactSealed) or resolveRankedLocked
// in the same paragraph; nothing here is a substitute for reading each site.
func TestTierOrdinalsHaveNotRenumbered(t *testing.T) {
	tcs := []struct {
		name    string
		tier    int
		ordinal int
	}{
		{"tierExactMutable", tierExactMutable, 1},
		{"tierExactImported", tierExactImported, 2},
		{"tierExactSealed", tierExactSealed, 3},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.tier, qt.Equals, tc.ordinal-1,
				qt.Commentf("BINDING-tier comments call tier %d %q; it is now a different constant. "+
					"Rewrite those labels only — the closure tiers (boxing.go and neighbours) and the "+
					"architectural-review item IDs spell themselves the same way and are correct",
					tc.ordinal, tc.name))
		})
	}
	qt.Assert(t, tierNone, qt.Equals, -1,
		qt.Commentf("tierNone must stay outside the ordinal range, or a floor/ceiling test admits it"))

	// tierCount's POSITION, against the table as the enumeration of real tiers.
	// tierHighest is tierCount-1 and is the ceiling every ranked reader applies,
	// so where the sentinel sits IS the ceiling; a tier declared after it is
	// unreachable through every ranked reader, silently, with no ordinal moved
	// for the loop above to catch.
	//
	// Comparing tierCount against tierExactSealed+1 does NOT catch that — checked,
	// not assumed: append a tier after the sentinel and tierCount is still
	// tierExactSealed+1, so the comparison stays green while tierHighest stays at
	// 2. Comparing it against the TABLE does, because the table has to name every
	// real tier and a fourth tier makes len(tcs) 4 while tierCount is still 3.
	qt.Assert(t, tierCount, qt.Equals, len(tcs),
		qt.Commentf("tierCount (%d) must be one past the LAST tier, and the table must name all %d of them; a tier declared after the sentinel is excluded by every ranked ceiling", tierCount, len(tcs)))
	qt.Assert(t, tierHighest, qt.Equals, tcs[len(tcs)-1].tier,
		qt.Commentf("the ranked ceiling must admit the highest tabled tier"))
}

// TestBulkRowTieRanksLikeASlotTie is Task 5's RED pin: a bulk row and a
// per-symbol slot must decide an incomparable equal-cardinality tie the SAME
// way, because it is one rule.
//
// Before the merge it was two. Two SLOTS carrying {s1} and {s2} under a query of
// {s1, s2} raise ErrAmbiguousBinding — that is
// TestResolveRankedAmbiguityScopedToWinningTier, and the third subtest below
// restates it so the two answers sit in one file. Two ROWS in the IDENTICAL
// configuration computed no ambiguity at all: probeBulkLocked had no ambiguity
// arm, and its equal-cardinality comparison was `>=` rather than `>`, so the
// resolution silently went to whichever row was installed LAST.
//
// Nothing made that visible. Flipping the slot side's `>` to `>=` left the
// entire suite green, so the tie-break OPERATOR was untested, not merely the
// ambiguity half — which is why this pin asserts the ambiguous ANSWER rather
// than which of the two rows wins.
func TestBulkRowTieRanksLikeASlotTie(t *testing.T) {
	sym := values.NewSymbol("v")
	sc1 := syntax.NewScope()
	sc2 := syntax.NewScope()
	query := syntax.ScopesOf([]*syntax.Scope{sc1, sc2})

	// Two rows over THIS store's phase-0 sealed slot, installed at phase 1 under
	// incomparable one-element scope sets. Each is compatible with the query
	// ({s1} ⊆ {s1,s2} and {s2} ⊆ {s1,s2}), they tie on tier (both sealed at the
	// query phase) and on cardinality, and neither set is a subset of the other.
	mk := func() *GlobalEnvironmentFrame {
		q := NewGlobalEnvironmentFrame()
		q.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, PhaseRuntime, true)
		q.InstallBulkRow(NewSealedStoreBulkSource(q, PhaseRuntime, values.NewSymbol("srcA")),
			[]*syntax.Scope{sc1}, PhaseExpand, true, BulkOriginLanguage)
		q.InstallBulkRow(NewSealedStoreBulkSource(q, PhaseRuntime, values.NewSymbol("srcB")),
			[]*syntax.Scope{sc2}, PhaseExpand, true, BulkOriginLanguage)
		return q
	}

	t.Run("rows report the tie", func(t *testing.T) {
		g := mk()
		bnd, ambiguous := g.BulkBindingAt(sym, query, PhaseExpand)
		qt.Assert(t, ambiguous, qt.IsTrue,
			qt.Commentf("two incomparable rows of equal cardinality are Flatt-ambiguous; the row walk reported the last-installed one instead"))
		qt.Assert(t, bnd, qt.IsNil,
			qt.Commentf("BulkBindingAt's doc already specifies the tie as an ANSWER; a binding alongside it is the tie being swallowed"))
	})

	t.Run("ranked resolution raises the tie", func(t *testing.T) {
		g := mk()
		qt.Assert(t, func() {
			g.mu.RLock()
			defer g.mu.RUnlock()
			g.resolveRankedLocked(*sym, query, PhaseExpand)
		}, qt.PanicMatches, ".*ambiguous.*",
			qt.Commentf("the per-symbol probe raises this tie; consulting rows on its miss must not answer where it would have refused"))
	})

	t.Run("slots in the identical configuration", func(t *testing.T) {
		// The control, and the reason the two subtests above are a defect rather
		// than a preference: same scope sets, same query, same phase, slots
		// instead of rows.
		g := NewGlobalEnvironmentFrame()
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, []*syntax.Scope{sc1}, PhaseExpand, true)
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, []*syntax.Scope{sc2}, PhaseExpand, true)
		qt.Assert(t, func() {
			g.mu.RLock()
			defer g.mu.RUnlock()
			g.resolveRankedLocked(*sym, query, PhaseExpand)
		}, qt.PanicMatches, ".*ambiguous.*")
	})
}

// TestBulkTierOfClassifiesByOrigin is the classifier half of Task 6 Step 3: a
// row is ranked by the same three-way rule a slot is, with bulkRef.origin
// standing where tierOf reads Binding.IsImported().
//
// RED before the origin arm: every sealed row answered tierExactSealed, so the
// import row and the language row were indistinguishable and the "row" column
// of the tier table had no entry at all.
func TestBulkTierOfClassifiesByOrigin(t *testing.T) {
	tcs := []struct {
		name   string
		sealed bool
		origin BulkOrigin
		want   int
	}{
		{name: "mutable row", sealed: false, origin: BulkOriginLanguage, want: tierExactMutable},
		{name: "sealed language row", sealed: true, origin: BulkOriginLanguage, want: tierExactSealed},
		{name: "sealed import row", sealed: true, origin: BulkOriginImport, want: tierExactImported},
		// A mutable row is already the lowest tier, so the origin cannot lower it
		// further. Stated as a row rather than left implicit: the arm is guarded
		// on sealed, and dropping that guard would rank a mutable import row
		// ABOVE a mutable slot.
		{name: "mutable import row stays mutable", sealed: false, origin: BulkOriginImport, want: tierExactMutable},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			row := bulkRef{phase: PhaseExpand, sealed: tc.sealed, origin: tc.origin}
			qt.Assert(t, bulkTierOf(row, PhaseExpand), qt.Equals, tc.want)
			qt.Assert(t, bulkTierOf(row, PhaseRuntime), qt.Equals, tierNone,
				qt.Commentf("a row is a candidate at its declared phase alone, whatever its origin"))
		})
	}
}

// TestImportedRowOutranksALanguageRow is the behavioural half: when two rows
// supply one name at one coordinate, the IMPORT row wins — not the row that
// happened to be installed first.
//
// RED before the origin arm, and red for a reason worth stating: the two rows
// then tie at tierExactSealed, rankedArgmax breaks an equal-cardinality tie by
// FIRST INSTALLED, and the language row is installed first here deliberately. So
// the pin fails with the language row's value rather than with a miss, which is
// the failure a future revert would produce.
//
// Both rows read THIS store at phase 0 and are installed at phase 1, because
// materializeBulkLocked refuses a foreign store (store != p) and a same-store
// row at the query phase is skipped as having nothing to add. They supply
// DIFFERENT bindings because their tier FLOORS differ: the sealed source reaches
// the sealed slot, the unrestricted one reaches the mutable slot that outranks
// it. That is the only in-tree way to make two rows over one store disagree,
// and disagreeing is what makes the winner observable.
func TestImportedRowOutranksALanguageRow(t *testing.T) {
	ns := NewNamespace()
	owner := ns.Runtime()
	store := owner.GlobalEnvironment()

	sealed := sealAt(t, owner, PhaseRuntime, "contested", values.NewInteger(1))
	_, err := owner.DefineOwnGlobal(values.NewSymbol("contested"), BindingTypeVariable, nil, values.NewInteger(2))
	qt.Assert(t, err, qt.IsNil)

	store.InstallBulkRow(
		NewSealedStoreBulkSource(store, PhaseRuntime, values.NewSymbol("#%lang")),
		nil, PhaseExpand, true, BulkOriginLanguage)
	store.InstallBulkRow(
		NewStoreBulkSource(store, PhaseRuntime, values.NewSymbol("#%imported-lib")),
		nil, PhaseExpand, true, BulkOriginImport)

	got, ok := resolveThroughRows(t, store, "contested", PhaseExpand)
	qt.Assert(t, ok, qt.IsTrue,
		qt.Commentf("neither row answered; the pin measures nothing"))
	qt.Assert(t, got != sealed, qt.IsTrue,
		qt.Commentf("the language row won: a sealed import row must outrank a sealed language row, as tierExactImported outranks tierExactSealed for slots"))
	qt.Assert(t, got.Value().SchemeString(), qt.Equals, "2")
}
