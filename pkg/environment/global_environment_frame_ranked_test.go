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
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// The ranked probe over a hand-built mixed store (design §4.3): tier T1
// (exact phase, mutable) > T2 (exact phase, sealed); a slot at any OTHER exact
// phase is not a candidate at all; maximal scope cardinality ranks within the
// winning tier only.
//
// The two exact tiers at the query phase are now the WHOLE candidate set. Stage
// A deleted the ambient (ANY, sealed) tier, so this table's third case,
// "ambient visible from every phase", has no subject; the property that
// replaced it — cross-phase visibility supplied by a bulk row — is
// TestResolveRankedCrossPhaseNeedsABulkRow below.
func TestResolveRankedTiers(t *testing.T) {
	sym := values.NewSymbol("v")
	mk := func(entries []struct {
		phase  PhaseKey
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
			phase  PhaseKey
			sealed bool
		}
		probes []probe
	}{
		{name: "T1 beats T2",
			entries: []struct {
				phase  PhaseKey
				sealed bool
			}{
				{ExactPhase(0), true},  // slot 0: T2 at 0
				{ExactPhase(0), false}, // slot 1: T1 at 0
			},
			probes: []probe{
				{phase: 0, wantSlot: 1, wantOK: true},
				// Neither slot is a candidate one phase up: a sealed write is
				// exact-phase like any other, so it no longer leaks upward.
				{phase: 1, wantOK: false},
			}},
		{name: "other exact phase is no candidate",
			entries: []struct {
				phase  PhaseKey
				sealed bool
			}{
				{ExactPhase(1), false},
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
// every phase's probe reached it; it now lands at (ExactPhase(0), sealed), and
// phase 1 reaches it only because the dialect declared the base at phase 1,
// which installs a row over the same store at (ExactPhase(1), sealed).
//
// The probe BEFORE the row is installed is the control: resolveRankedLocked
// consults rows only on a per-symbol miss, so a pass without that leg would not
// distinguish the row doing the work from the coordinate still doing it.
func TestResolveRankedCrossPhaseNeedsABulkRow(t *testing.T) {
	sym := values.NewSymbol("v")
	g := NewGlobalEnvironmentFrame()
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(PhaseRuntime), true)

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
		nil, ExactPhase(PhaseExpand), true)

	ref, ok = probeAt(PhaseExpand)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, ref.slot, qt.Equals, 0,
		qt.Commentf("the row must resolve to the phase-0 slot ITSELF; a copy would fork later writes"))
	qt.Assert(t, g.BulkResolutionCount(), qt.Equals, int64(1))
}

// (ANY, mutable) is forbidden: no population produces it, so the write API
// refuses it rather than modeling a row nothing means (design §4.1). Pinned via
// errors.Is on the sentinel, not the panic text: a message-only assertion would
// keep passing even if the sentinel choice changed underneath it, which is
// exactly the identity the house error-handling rule protects.
//
// WIDENED by Stage A: the refusal now covers the wildcard coordinate outright,
// sealed and mutable alike. writeCoordinates produces no wildcard key for any
// view and tierOf classifies one as tierNone, so accepting a sealed one would
// leave a slot nothing ranks and nothing can read. Both rows are asserted so a
// narrowing back to the mutable half alone fails here.
func TestCreateGlobalBindingAtRefusesAnyPhase(t *testing.T) {
	sym := values.NewSymbol("v")

	for _, sealed := range []bool{false, true} {
		g := NewGlobalEnvironmentFrame()
		r := capturePanic(func() {
			g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, AnyPhase(), sealed)
		})
		qt.Assert(t, r, qt.IsNotNil, qt.Commentf("sealed=%t", sealed))
		err, ok := r.(error)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue,
			qt.Commentf("sealed=%t", sealed))
	}
}

// A tie in a LOSING tier must not panic: rank decides first, ambiguity is only
// asked of the winning tier (P8 scoped to the probe).
func TestResolveRankedAmbiguityScopedToWinningTier(t *testing.T) {
	sym := values.NewSymbol("v")
	sc1 := syntax.NewScope()
	sc2 := syntax.NewScope()
	g := NewGlobalEnvironmentFrame()
	// Two incomparable T2 candidates...
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, []*syntax.Scope{sc1}, ExactPhase(0), true)
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, []*syntax.Scope{sc2}, ExactPhase(0), true)
	// ...and one T1 winner.
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(0), false)

	g.mu.RLock()
	defer g.mu.RUnlock()
	ref, ok := g.resolveRankedLocked(*sym, syntax.ScopesOf([]*syntax.Scope{sc1, sc2}), 0)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, ref.slot, qt.Equals, 2)

	// With the T1 winner gone, the T2 tie is the winning tier and must panic.
	g2 := NewGlobalEnvironmentFrame()
	g2.CreateGlobalBindingAt(sym, BindingTypeVariable, []*syntax.Scope{sc1}, ExactPhase(0), true)
	g2.CreateGlobalBindingAt(sym, BindingTypeVariable, []*syntax.Scope{sc2}, ExactPhase(0), true)
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
	_, created := g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(0), true)
	qt.Assert(t, created, qt.IsTrue)
	// Same scopes (∅), different coordinates: a NEW slot.
	_, created = g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(0), false)
	qt.Assert(t, created, qt.IsTrue)
	// Same scopes, same coordinates: reuse.
	_, created = g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(0), false)
	qt.Assert(t, created, qt.IsFalse)
}

// Copy's per-slot coordinate carry-forward is production-live: it is the whole
// of NewSchemeReportNamespace, which builds its store by copying the parent's
// rather than minting fresh. Silently dropping the stamps would collapse every
// sealed entry in a scheme-report namespace onto the zero value
// (ExactPhase(0), mutable), where it starts colliding with real phase-0 user
// defines instead of being shadowed by them.
//
// The two slots below are the two coordinates a write path actually mints at
// phase 0 since Stage A — sealed and mutable — rather than the ambient one this
// test used to open with, which nothing writes any more.
func TestCopyPreservesCoordinates(t *testing.T) {
	sym := values.NewSymbol("v")
	g := NewGlobalEnvironmentFrame()
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(PhaseRuntime), true)
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(PhaseRuntime), false)

	c := g.Copy()

	qt.Assert(t, len(c.keys[*sym]), qt.Equals, 2)
	qt.Assert(t, c.keys[*sym][0].phase, qt.Equals, ExactPhase(PhaseRuntime))
	qt.Assert(t, c.keys[*sym][0].sealed, qt.IsTrue)
	qt.Assert(t, c.keys[*sym][1].phase, qt.Equals, ExactPhase(PhaseRuntime))
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
		// Two tiers now, not three: T3 was the ambient coordinate, and Stage A
		// deleted it. T1 must still outrank T2, which is what makes a phase-0
		// define shadow the sealed base rather than assign through it.
		g := NewGlobalEnvironmentFrame()
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(0), true)  // slot 0: T2
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(0), false) // slot 1: T1

		g.mu.RLock()
		defer g.mu.RUnlock()
		ref, ok := g.resolveRankedLocked(*sym, syntax.AllScopes(), 0)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, ref.slot, qt.Equals, 1)
	})

	t.Run("other exact phase is not a candidate", func(t *testing.T) {
		g := NewGlobalEnvironmentFrame()
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(1), false)

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
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(0), false)
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
	// Both T1 (exact phase 0, mutable); {scA} subset {scA, scB}.
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, []*syntax.Scope{scA}, ExactPhase(0), false)
	g.CreateGlobalBindingAt(sym, BindingTypeVariable, []*syntax.Scope{scA, scB}, ExactPhase(0), false)

	g.mu.RLock()
	defer g.mu.RUnlock()
	ref, ok := g.resolveRankedLocked(*sym, syntax.ScopesOf([]*syntax.Scope{scA, scB}), 0)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, ref.slot, qt.Equals, 1) // the wider {scA, scB} slot wins
}
