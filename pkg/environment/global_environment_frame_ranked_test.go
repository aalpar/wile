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
		nil, PhaseExpand, true)

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
// Roughly a hundred comments across the tree still label tiers "T1"/"T2"/"T3".
// Each is an unchecked second copy of this enum's ordering: inserting
// tierExactImported renumbered everything below it, and every one of those
// labels silently changed referent with no test going red.
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
//     test working, not a gap — but do NOT read it as "appending is safe". It is
//     label-safe and REACHABILITY-unsafe. Every ranked ceiling in the tree is the
//     literal tierExactSealed — probeRankedLocked's call to probeTiersLocked,
//     resolveRankedLocked's and BulkBindingAt's calls to probeBulkLocked, and
//     ExactBindingAt's to bindingWithinTiers — so a tier appended past it fails
//     probeTiersLocked's `t > maxTier` filter for every reader and resolves
//     nothing, silently and with no test red. Whoever appends the next tier must
//     move those ceilings too, and nothing but this sentence will say so.
//
// It catches insertion, reorder and mid-enum removal — the ways an ordinal's
// referent actually moves — and a rename or an outright removal breaks the
// build here instead.
//
// IF THIS TEST IS RED because you added, removed or reordered a tier: that is
// the test working, not a stale expectation. Every "T<n>" in a comment now
// denotes a different constant. Rewrite them to name the identifier (preferred —
// see the enum's doc for why), or renumber them, then update the table here.
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
				qt.Commentf("comments across the tree call tier %d %q; it is now a different constant",
					tc.ordinal, tc.name))
		})
	}
	qt.Assert(t, tierNone, qt.Equals, -1,
		qt.Commentf("tierNone must stay outside the ordinal range, or a floor/ceiling test admits it"))
}
