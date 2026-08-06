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
// (exact phase, mutable) > T2 (exact phase, sealed) > T3 (ANY, sealed); a slot
// at any OTHER exact phase is not a candidate at all; maximal scope cardinality
// ranks within the winning tier only.
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
		{name: "T1 beats T2 beats T3",
			entries: []struct {
				phase  PhaseKey
				sealed bool
			}{
				{AnyPhase(), true},     // slot 0: T3
				{ExactPhase(0), true},  // slot 1: T2 at 0
				{ExactPhase(0), false}, // slot 2: T1 at 0
			},
			probes: []probe{
				{phase: 0, wantSlot: 2, wantOK: true},
				{phase: 1, wantSlot: 0, wantOK: true}, // only T3 is a candidate at 1
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
		{name: "ambient visible from every phase",
			entries: []struct {
				phase  PhaseKey
				sealed bool
			}{
				{AnyPhase(), true},
			},
			probes: []probe{
				{phase: -1, wantSlot: 0, wantOK: true},
				{phase: 0, wantSlot: 0, wantOK: true},
				{phase: 3, wantSlot: 0, wantOK: true},
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
				slot, ok := func() (int, bool) {
					g.mu.RLock()
					defer g.mu.RUnlock()
					return g.resolveRankedLocked(*sym, syntax.EmptyScopes(), pr.phase)
				}()
				qt.Assert(t, ok, qt.Equals, pr.wantOK, qt.Commentf("phase %d", pr.phase))
				if pr.wantOK {
					qt.Assert(t, slot, qt.Equals, pr.wantSlot, qt.Commentf("phase %d", pr.phase))
				}
			}
		})
	}
}

// (ANY, mutable) is forbidden: no population produces it, so the write API
// refuses it rather than modeling a row nothing means (design §4.1). Pinned via
// errors.Is on the sentinel, not the panic text: a message-only assertion would
// keep passing even if the sentinel choice changed underneath it, which is
// exactly the identity the house error-handling rule protects.
func TestCreateGlobalBindingAtRefusesAnyMutable(t *testing.T) {
	g := NewGlobalEnvironmentFrame()
	sym := values.NewSymbol("v")

	r := capturePanic(func() {
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, AnyPhase(), false)
	})
	qt.Assert(t, r, qt.IsNotNil)
	err, ok := r.(error)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue)
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
	slot, ok := g.resolveRankedLocked(*sym, syntax.ScopesOf([]*syntax.Scope{sc1, sc2}), 0)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, slot, qt.Equals, 2)

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
// what keeps define-for-syntax over the (1, mutable) registry copy a supersede.
func TestCreateMatchesCoordinatesAndScopes(t *testing.T) {
	sym := values.NewSymbol("v")
	g := NewGlobalEnvironmentFrame()
	_, created := g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, AnyPhase(), true)
	qt.Assert(t, created, qt.IsTrue)
	// Same scopes (∅), different coordinates: a NEW slot.
	_, created = g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(0), false)
	qt.Assert(t, created, qt.IsTrue)
	// Same scopes, same coordinates: reuse.
	_, created = g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(0), false)
	qt.Assert(t, created, qt.IsFalse)
}

// Copy's coordinate carry-forward is production-live TODAY, not just probe
// scaffolding: NewSchemeReportNamespace builds its sealed base by copying the
// parent's rather than minting fresh (namespace.go, wireRuntimeFrames(q,
// p.sealedBase.global.Copy(), p.runtime.global.Copy())). Silently dropping the
// stamps would turn a scheme-report namespace's sealed base from (AnyPhase,
// sealed) — ambient at every phase — into the zero value (ExactPhase(0),
// mutable), which starts colliding with real phase-0 user defines instead of
// staying invisible to them.
func TestCopyPreservesCoordinates(t *testing.T) {
	sym := values.NewSymbol("v")
	g := NewGlobalEnvironmentFrameAt(AnyPhase(), true)
	g.CreateGlobalBinding(sym, BindingTypeVariable, nil)

	c := g.Copy()
	qt.Assert(t, c.writePhase, qt.Equals, AnyPhase())
	qt.Assert(t, c.writeSealed, qt.IsTrue)

	// The other half of what keeps CreateGlobalBindingAt's reuse check inert on
	// a copied frame: the cloned SLOTS carry their own coordinates too, not
	// just the frame's instance-level constants.
	qt.Assert(t, len(c.keys[*sym]), qt.Equals, 1)
	qt.Assert(t, c.keys[*sym][0].phase, qt.Equals, AnyPhase())
	qt.Assert(t, c.keys[*sym][0].sealed, qt.IsTrue)
}

// The q.IsAll() wildcard branch is bespoke relative to bestSlotLocked's
// wildcard path (a plain first-live-slot loop with no tier concept): it tracks
// tier across the whole slot list, applies the same phase filter the scoped
// branch does, and returns the highest tier's first live slot. Every existing
// wildcard caller of bestSlotLocked (GetGlobalIndex, DeleteBinding,
// AmbientKeys) inherits this ranked behavior the moment the fold swaps
// bestSlotLocked out from under them, so pin it here.
func TestResolveRankedWildcard(t *testing.T) {
	sym := values.NewSymbol("v")

	t.Run("tier order", func(t *testing.T) {
		g := NewGlobalEnvironmentFrame()
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, AnyPhase(), true)     // slot 0: T3
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(0), true)  // slot 1: T2
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(0), false) // slot 2: T1

		g.mu.RLock()
		defer g.mu.RUnlock()
		slot, ok := g.resolveRankedLocked(*sym, syntax.AllScopes(), 0)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, slot, qt.Equals, 2)
	})

	t.Run("other exact phase is not a candidate", func(t *testing.T) {
		g := NewGlobalEnvironmentFrame()
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(1), false)

		g.mu.RLock()
		defer g.mu.RUnlock()
		slot, ok := g.resolveRankedLocked(*sym, syntax.AllScopes(), 0)
		qt.Assert(t, ok, qt.IsFalse)
		// The slot value on the walk-exhausted return, not the len(slots)==0
		// short-circuit: this frame HAS a slot for the name, the wildcard loop
		// runs and rejects it, and bestSlot must still be the zero the caller
		// is told to ignore. Decoupling bestSlot from bestTier would show up
		// only here.
		qt.Assert(t, slot, qt.Equals, 0)
	})

	t.Run("nil'd slot is skipped", func(t *testing.T) {
		g := NewGlobalEnvironmentFrame()
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, ExactPhase(0), false)
		// A live key pointing at a nil binding — bestSlotLocked's own wildcard
		// branch guards exactly this state (a slot DeleteBinding emptied); the
		// ranked probe's wildcard branch must guard it identically.
		g.bindings[0] = nil

		g.mu.RLock()
		defer g.mu.RUnlock()
		slot, ok := g.resolveRankedLocked(*sym, syntax.AllScopes(), 0)
		qt.Assert(t, ok, qt.IsFalse)
		qt.Assert(t, slot, qt.Equals, 0)
	})

	t.Run("no candidate at all", func(t *testing.T) {
		g := NewGlobalEnvironmentFrame()

		g.mu.RLock()
		defer g.mu.RUnlock()
		slot, ok := g.resolveRankedLocked(*sym, syntax.AllScopes(), 0)
		qt.Assert(t, ok, qt.IsFalse)
		qt.Assert(t, slot, qt.Equals, 0)
	})
}

// A wider (larger-cardinality) scope set beats a narrower one INSIDE one tier.
// TestResolveRankedTiers only ever passes nil scopes, so it pins the tier gate
// but never the ranking scopedBestOf performs within the winning tier — the
// mechanism is shared with bestSlotLocked, but nothing pinned it here.
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
	slot, ok := g.resolveRankedLocked(*sym, syntax.ScopesOf([]*syntax.Scope{scA, scB}), 0)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, slot, qt.Equals, 1) // the wider {scA, scB} slot wins
}
