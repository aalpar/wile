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

package wile_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// TestPhase1DoesNotReachPhase0Sealed IS design §6.1 row five — no longer the
// behavioural twin of one.
//
// The internal test it twinned (TestNoAmbientCoordinateExists, in
// pkg/environment) censused every slot whose phase coordinate was the ANY
// wildcard. Stage B collapsed the coordinate to a bare Phase, so that census
// reads a field that no longer exists and its assertion degenerates to 0 == 0.
// It was deleted rather than rewritten: nothing replaces it, because "no slot
// carries the ANY coordinate" is now a type-level impossibility with no argument
// left to pass. This test states the same property in terms the public API can
// still express, which is why it is the one that survived.
//
// It is a STORE-reachability assertion, not a compilation one: it asks the
// engine root's frame at phase 1 for a name directly, with no transformer, no
// expander and no compiler in the path. That is what separates it from
// TestPhase1_ProceduralTransformerUnboundWithoutImport, which asks whether a
// transformer BODY compiles.
//
// The four pin names are exactly the class design §5.1 sizes the migration off
// and pkg/wile/binding_tier_census_test.go measured: procedures DEFINED in
// bootstrap Scheme, which hold a phase-0 sealed slot and no exact-phase-1 slot,
// so the ambient tier was their only route to phase 1 and Stage A left them none.
// The three control names
// are Go primitives registered at both phases (registry/apply.go phaseTargets),
// which keep an exact-phase-1 slot and must still resolve after A — without them
// a change that made every phase-1 query miss would pass.
//
// COUPLED TO Q2 (Task 10): this test is the store-level statement of D3/D4's
// headline break. If Task 10 ever declares assoc/caar/vector-map/string-map in
// the phase-1 vocabulary, D4's "a procedural transformer must import
// (for-syntax (scheme base))" is false and this test, not the vocabulary, is
// what has to change.
func TestPhase1DoesNotReachPhase0Sealed(t *testing.T) {
	eng, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)

	root := eng.Environment()
	// Expand() is the owner's first rung — the level a transformer RHS compiles
	// at (compile_transformer.go) — over the SAME store as the runtime frame, so
	// a difference between the two frames is a phase difference and nothing else.
	phase1 := root.Expand()
	qt.Assert(t, phase1.PhaseLevel(), qt.Equals, environment.PhaseExpand)
	qt.Assert(t, phase1.GlobalEnvironment(), qt.Equals, root.GlobalEnvironment())

	tcs := []struct {
		name         string
		wantAtPhase1 bool
	}{
		// Defined in bootstrap Scheme: phase-0 sealed slot only.
		{"assoc", false},
		{"caar", false},
		{"vector-map", false},
		{"string-map", false},
		// Go primitives registered at both phases: exact-phase-1 slot.
		{"car", true},
		{"cons", true},
		{"null?", true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			sym := values.NewSymbol(tc.name)
			qt.Assert(t, root.GetBinding(sym, syntax.EmptyScopes()), qt.IsNotNil,
				qt.Commentf("%q must stay bound at phase 0", tc.name))
			bnd := phase1.GetBinding(sym, syntax.EmptyScopes())
			qt.Assert(t, bnd != nil, qt.Equals, tc.wantAtPhase1,
				qt.Commentf("%q resolved at phase 1", tc.name))
		})
	}
}
