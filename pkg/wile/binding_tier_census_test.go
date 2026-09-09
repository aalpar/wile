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
	"sort"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/registry/core"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// These are ANSWER pins: they record where a fully built engine actually puts a
// binding, measured rather than reasoned. They exist because the phase/tier
// question was answered wrongly in a code comment for a long time with nothing
// contradicting it, and because plans/2026-09-08-flatt-binding-model-a-design
// (Stage A, "delete the ambient tier") sizes its migration off exactly these
// facts. Stage A is EXPECTED to flip several of them; a diff here is the signal
// that it did, not a failure.

// tierOfName reports the two coordinates that matter: whether the name has a
// slot in the ambient tier, and whether it has one at (ExactPhase(1), sealed).
// The two are independent — a name can have both, from different sources.
func tierOfName(t *testing.T, g *environment.GlobalEnvironmentFrame, name string) (ambient, exactPhase1 bool) {
	t.Helper()
	sym := values.NewSymbol(name)
	amb, ambTie := g.AmbientBinding(sym, syntax.EmptyScopes())
	qt.Assert(t, ambTie, qt.IsFalse, qt.Commentf("%q ties in the ambient tier", name))
	ex1, ex1Tie := g.ExactBindingAt(sym, syntax.EmptyScopes(), environment.PhaseExpand)
	qt.Assert(t, ex1Tie, qt.IsFalse, qt.Commentf("%q ties at exact phase 1", name))
	return amb != nil, ex1 != nil
}

// TestKeywordSlotIsNotTheExpanderSlot pins that an ambient slot for syntax-rules,
// quote or import comes from registry/core/specialforms.go's keyword rows ("needs a
// binding for library export resolution"), NOT from the primitive-expander
// registration, which is exact-phase-1 (see TestPrimitiveExpandersLandAtExactPhaseOne
// in pkg/machine/compilation). let-syntax has no keyword row and is the
// discriminating case: same registration path, no ambient slot.
func TestKeywordSlotIsNotTheExpanderSlot(t *testing.T) {
	eng, err := wile.NewEngine(context.Background())
	qt.Assert(t, err, qt.IsNil)
	g := eng.Environment().GlobalEnvironment()

	tcs := []struct {
		name            string
		wantAmbient     bool
		wantExactPhase1 bool
	}{
		// Keyword row in specialforms.go AND a primitive expander: two slots.
		{"syntax-rules", true, true},
		{"quote", true, true},
		{"import", true, true},
		// Primitive expander with no keyword row: expander slot only.
		{"let-syntax", false, true},
		// Bootstrap macro, written at phase 1: no keyword row either.
		{"when", false, true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			ambient, exact1 := tierOfName(t, g, tc.name)
			qt.Assert(t, ambient, qt.Equals, tc.wantAmbient)
			qt.Assert(t, exact1, qt.Equals, tc.wantExactPhase1)
		})
	}
}

// TestEllipsisAndUnderscoreAreAmbientOnly pins the two names that hold NO
// exact-phase slot at all. They are reachable at phase 1 today only because the
// ambient tier is phase-blind, so they are precisely what Stage A's phase-1
// declaration must supply (§9 Q2). Racket's measured phase-1 surface under
// racket/base is the same set plus syntax-rules.
func TestEllipsisAndUnderscoreAreAmbientOnly(t *testing.T) {
	eng, err := wile.NewEngine(context.Background())
	qt.Assert(t, err, qt.IsNil)
	g := eng.Environment().GlobalEnvironment()

	for _, name := range []string{"...", "_"} {
		t.Run(name, func(t *testing.T) {
			ambient, exact1 := tierOfName(t, g, name)
			qt.Assert(t, ambient, qt.IsTrue,
				qt.Commentf("%q must hold an ambient slot", name))
			qt.Assert(t, exact1, qt.IsFalse,
				qt.Commentf("%q must hold NO exact-phase-1 slot", name))
		})
	}
}

// TestStdlibDefinesAreAmbientOnly pins the asymmetry that sizes Stage A's
// phase-distinctness break (D3). Go primitives registered at both phases keep an
// exact-phase-1 slot; procedures DEFINED in bootstrap/stdlib Scheme are written at
// phase 0 sealed and reach phase 1 only through the ambient tier. Deleting ambient
// therefore costs a transformer body the Scheme layer, not the primitive layer.
func TestStdlibDefinesAreAmbientOnly(t *testing.T) {
	eng, err := wile.NewEngine(context.Background())
	qt.Assert(t, err, qt.IsNil)
	g := eng.Environment().GlobalEnvironment()

	tcs := []struct {
		name            string
		wantExactPhase1 bool
	}{
		// Go primitives registered at both phases (registry/apply.go phaseTargets).
		{"car", true},
		{"list-copy", true},
		// Defined in bootstrap/stdlib Scheme: phase 0 only.
		{"assoc", false},
		{"caar", false},
		{"vector-map", false},
		{"string-map", false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			ambient, exact1 := tierOfName(t, g, tc.name)
			qt.Assert(t, ambient, qt.IsTrue,
				qt.Commentf("%q must hold an ambient slot", tc.name))
			qt.Assert(t, exact1, qt.Equals, tc.wantExactPhase1)
		})
	}
}

// TestCorePrimitivePhaseCensus pins the SHAPE of core's phase declarations: the
// pair/list/predicate vocabulary a transformer body would reach for is registered
// at both phases, while the runtime-only names are the continuation, error-object,
// parameter and introspection family. Exemplars rather than a count, so adding a
// primitive does not fail this; the counts are logged for the record.
//
// Measured 2026-09-08 over core.AddToRegistry: 155 names at both phases, 63
// runtime-only, 0 expand-only.
func TestCorePrimitivePhaseCensus(t *testing.T) {
	reg := registry.NewRegistry()
	err := core.AddToRegistry(reg)
	qt.Assert(t, err, qt.IsNil)

	phasesOf := func(name string) (runtime, expand bool) {
		r, ok := reg.PrimitiveByName(name)
		qt.Assert(t, ok, qt.IsTrue, qt.Commentf("core does not register %q", name))
		return r.Phases.Has(environment.PhaseRuntime), r.Phases.Has(environment.PhaseExpand)
	}

	for _, name := range []string{"car", "cdr", "cons", "null?", "eq?", "list-copy"} {
		t.Run("both/"+name, func(t *testing.T) {
			runtimePhase, expandPhase := phasesOf(name)
			qt.Assert(t, runtimePhase, qt.IsTrue)
			qt.Assert(t, expandPhase, qt.IsTrue,
				qt.Commentf("%q must stay visible at phase 1", name))
		})
	}
	for _, name := range []string{"call/cc", "raise", "values", "apply", "make-parameter"} {
		t.Run("runtime-only/"+name, func(t *testing.T) {
			runtimePhase, expandPhase := phasesOf(name)
			qt.Assert(t, runtimePhase, qt.IsTrue)
			qt.Assert(t, expandPhase, qt.IsFalse,
				qt.Commentf("%q is runtime-only; a phase-1 body cannot reach it", name))
		})
	}

	var both, runtimeOnly, expandOnly int
	var expandOnlyNames []string
	for _, name := range reg.PrimitiveNames() {
		r, ok := reg.PrimitiveByName(name)
		if !ok {
			continue
		}
		hasRuntime := r.Phases.Has(environment.PhaseRuntime)
		hasExpand := r.Phases.Has(environment.PhaseExpand)
		switch {
		case hasRuntime && hasExpand:
			both++
		case hasRuntime:
			runtimeOnly++
		case hasExpand:
			expandOnly++
			expandOnlyNames = append(expandOnlyNames, name)
		}
	}
	sort.Strings(expandOnlyNames)
	t.Logf("core phase census: both=%d runtime-only=%d expand-only=%d %v",
		both, runtimeOnly, expandOnly, expandOnlyNames)
	// The dual-phase set must stay the majority: it is the thing that keeps Stage A's
	// D3 break off the primitive layer. A wholesale policy change trips this; adding
	// or removing individual primitives does not.
	qt.Assert(t, both > runtimeOnly, qt.IsTrue,
		qt.Commentf("dual-phase primitives (%d) must outnumber runtime-only (%d)", both, runtimeOnly))
}
