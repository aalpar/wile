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
				g.mu.RLock()
				slot, ok := g.resolveRankedLocked(*sym, syntax.EmptyScopes(), pr.phase)
				g.mu.RUnlock()
				qt.Assert(t, ok, qt.Equals, pr.wantOK, qt.Commentf("phase %d", pr.phase))
				if pr.wantOK {
					qt.Assert(t, slot, qt.Equals, pr.wantSlot, qt.Commentf("phase %d", pr.phase))
				}
			}
		})
	}
}

// (ANY, mutable) is forbidden: no population produces it, so the write API
// refuses it rather than modeling a row nothing means (design §4.1).
func TestCreateGlobalBindingAtRefusesAnyMutable(t *testing.T) {
	g := NewGlobalEnvironmentFrame()
	sym := values.NewSymbol("v")
	qt.Assert(t, func() {
		g.CreateGlobalBindingAt(sym, BindingTypeVariable, nil, AnyPhase(), false)
	}, qt.PanicMatches, ".*ANY, mutable.*")
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
	slot, ok := g.resolveRankedLocked(*sym, syntax.ScopesOf([]*syntax.Scope{sc1, sc2}), 0)
	g.mu.RUnlock()
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
