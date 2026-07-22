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
	"slices"
	"sync"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

func TestPhaseRegistry_Creation(t *testing.T) {
	topLevel := NewNamespaceFrame()

	qt.Assert(t, topLevel.phases, qt.IsNotNil)
	qt.Assert(t, topLevel.phases.TopLevelFrame(), qt.Equals, topLevel)

	// Phase 0 should be registered as TopLevel
	qt.Assert(t, topLevel.phases.Get(PhaseRuntime), qt.Equals, topLevel)
}

func TestPhaseRegistry_GetOrCreate(t *testing.T) {
	topLevel := NewNamespaceFrame()

	// Get phase 0 (should be TopLevel)
	phase0 := topLevel.phases.GetOrCreate(PhaseRuntime)
	qt.Assert(t, phase0, qt.Equals, topLevel)

	// Get phase 1 (should create new)
	phase1 := topLevel.phases.GetOrCreate(PhaseExpand)
	qt.Assert(t, phase1, qt.IsNotNil)
	qt.Assert(t, phase1, qt.Not(qt.Equals), topLevel)
	qt.Assert(t, phase1.phaseLevel, qt.Equals, PhaseExpand)

	// Get phase 1 again (should return same instance)
	phase1Again := topLevel.phases.GetOrCreate(PhaseExpand)
	qt.Assert(t, phase1Again, qt.Equals, phase1)

	// Get phase 2
	phase2 := topLevel.phases.GetOrCreate(PhaseCompile)
	qt.Assert(t, phase2, qt.IsNotNil)
	qt.Assert(t, phase2.phaseLevel, qt.Equals, PhaseCompile)

	// Get negative phase
	phaseMinus1 := topLevel.phases.GetOrCreate(PhaseTemplate)
	qt.Assert(t, phaseMinus1, qt.IsNotNil)
	qt.Assert(t, phaseMinus1.phaseLevel, qt.Equals, PhaseTemplate)
}

func TestPhaseRegistry_Get(t *testing.T) {
	topLevel := NewNamespaceFrame()

	// Get non-existent phase returns nil
	qt.Assert(t, topLevel.phases.Get(5), qt.IsNil)

	// After creating, Get returns the same instance
	phase5 := topLevel.phases.GetOrCreate(5)
	qt.Assert(t, topLevel.phases.Get(5), qt.Equals, phase5)
}

func TestPhaseRegistry_Phases(t *testing.T) {
	topLevel := NewNamespaceFrame()

	// Initially only phase 0
	phases := topLevel.phases.Phases()
	qt.Assert(t, len(phases), qt.Equals, 1)
	qt.Assert(t, phases[0], qt.Equals, PhaseRuntime)

	// Create more phases
	topLevel.phases.GetOrCreate(PhaseExpand)
	topLevel.phases.GetOrCreate(PhaseCompile)
	topLevel.phases.GetOrCreate(PhaseTemplate)

	phases = topLevel.phases.Phases()
	slices.SortFunc(phases, Phase.Compare)
	qt.Assert(t, len(phases), qt.Equals, 4)
	qt.Assert(t, phases, qt.DeepEquals, []Phase{PhaseTemplate, PhaseRuntime, PhaseExpand, PhaseCompile})
}

func TestPhaseRegistry_PhaseEnvParentsToSealedBase(t *testing.T) {
	topLevel := NewNamespaceFrame() // ns.runtime
	sealedBase := topLevel.Namespace().SealedBase()
	sealedExpandBase := topLevel.Namespace().SealedExpandBase()

	phase1 := topLevel.phases.GetOrCreate(PhaseExpand)
	phase2 := topLevel.phases.GetOrCreate(PhaseCompile)

	// Phase 1 (expand) parents to the sealed EXPAND base (D1), which in turn parents to
	// the phase-0 sealed base — the new middle frame. Bootstrap macros/expanders live in
	// sealedExpandBase, immutable, while a user define-syntax shadows in this mutable
	// child. Every other phase parents straight to the phase-0 sealed base (hermetic),
	// NOT the mutable runtime frame — confirming the carve is phase-1-only.
	qt.Assert(t, phase1.Parent(), qt.Equals, sealedExpandBase)     // was: sealedBase
	qt.Assert(t, sealedExpandBase.Parent(), qt.Equals, sealedBase) // NEW middle frame
	qt.Assert(t, phase2.Parent(), qt.Equals, sealedBase)           // unchanged (phase-1-only radius)
	qt.Assert(t, phase1.Parent(), qt.Not(qt.Equals), topLevel)

	// Climbing-tower hermeticity: higher phases created on demand (the climb adds
	// HIGHER siblings) must ALSO parent to the frozen sealed base, never to a lower
	// phase frame — the climb must never reintroduce a phase->phase parent edge. Only
	// phase 1 carves; phases 2/3/5 stay on the phase-0 sealed base.
	phase3 := topLevel.phases.GetOrCreate(3)
	phase5 := topLevel.phases.GetOrCreate(5)
	qt.Assert(t, phase3.Parent(), qt.Equals, sealedBase)
	qt.Assert(t, phase5.Parent(), qt.Equals, sealedBase)
	qt.Assert(t, phase3.Parent(), qt.Not(qt.Equals), phase2)
	qt.Assert(t, phase5.Parent(), qt.Not(qt.Equals), phase3)
}

func TestPhaseRegistry_PhaseEnvHasOwnGlobal(t *testing.T) {
	topLevel := NewNamespaceFrame()

	phase1 := topLevel.phases.GetOrCreate(PhaseExpand)
	phase2 := topLevel.phases.GetOrCreate(PhaseCompile)

	// Each phase has its own GlobalEnvironmentFrame
	qt.Assert(t, phase1.GlobalEnvironment(), qt.Not(qt.Equals), topLevel.GlobalEnvironment())
	qt.Assert(t, phase2.GlobalEnvironment(), qt.Not(qt.Equals), topLevel.GlobalEnvironment())
	qt.Assert(t, phase2.GlobalEnvironment(), qt.Not(qt.Equals), phase1.GlobalEnvironment())
}

func TestPhaseRegistry_Concurrent(t *testing.T) {
	topLevel := NewNamespaceFrame()

	var wg sync.WaitGroup
	const numGoroutines = 10
	const numPhases = 5

	results := make([][]*EnvironmentFrame, numGoroutines)
	for i := range numGoroutines {
		results[i] = make([]*EnvironmentFrame, numPhases)
	}

	// Concurrently access phases
	for i := range numGoroutines {
		wg.Add(1)
		go func(goroutineID int) {
			defer wg.Done()
			for phase := range numPhases {
				results[goroutineID][phase] = topLevel.phases.GetOrCreate(Phase(phase))
			}
		}(i)
	}

	wg.Wait()

	// All goroutines should get the same instances
	for phase := range numPhases {
		expected := results[0][phase]
		for goroutineID := 1; goroutineID < numGoroutines; goroutineID++ {
			qt.Assert(t, results[goroutineID][phase], qt.Equals, expected,
				qt.Commentf("goroutine %d phase %d", goroutineID, phase))
		}
	}
}

func TestPhaseConstants(t *testing.T) {
	qt.Assert(t, PhaseTemplate, qt.Equals, Phase(-1))
	qt.Assert(t, PhaseRuntime, qt.Equals, Phase(0))
	qt.Assert(t, PhaseExpand, qt.Equals, Phase(1))
	qt.Assert(t, PhaseCompile, qt.Equals, Phase(2))
}

func TestPhaseString(t *testing.T) {
	tcs := []struct {
		phase Phase
		want  string
	}{
		{PhaseTemplate, "template"},
		{PhaseRuntime, "runtime"},
		{PhaseExpand, "expand"},
		{PhaseCompile, "compile"},
		{Phase(7), "phase(7)"},
		{Phase(-2), "phase(-2)"},
	}
	for _, tc := range tcs {
		qt.Assert(t, tc.phase.String(), qt.Equals, tc.want,
			qt.Commentf("phase %d", int8(tc.phase)))
	}
}

func TestPhaseRegistry_ExpandPhaseIsHermetic(t *testing.T) {
	topLevel := NewNamespaceFrame() // ns.runtime — phase 0, mutable
	ns := topLevel.Namespace()
	expand := ns.Expand() // phase 1 (lazily created here)

	// A user define lands in the mutable runtime frame (phase 0).
	userSym := values.NewSymbol("user-x")
	ns.Runtime().MaybeCreateOwnGlobalBinding(userSym, BindingTypeVariable, nil)

	// A base binding lands in the frozen sealed base (the taproot).
	baseSym := values.NewSymbol("base-y")
	ns.SealedBase().MaybeCreateOwnGlobalBinding(baseSym, BindingTypeVariable, nil)

	// Hermeticity: phase 1 must NOT see the phase-0 user define...
	qt.Assert(t, expand.GetBinding(userSym, nil), qt.IsNil)
	// ...but MUST still see the shared taproot base binding.
	qt.Assert(t, expand.GetBinding(baseSym, nil), qt.Not(qt.IsNil))
}

func TestNextPhaseClimbsAndGuards(t *testing.T) {
	topLevel := NewNamespaceFrame()     // runtime frame, phaseLevel 0
	p1 := topLevel.AtPhase(PhaseExpand) // phaseLevel 1
	fromPhase1 := p1.NextPhase().PhaseLevel()
	if fromPhase1 != PhaseCompile {
		t.Fatalf("NextPhase from phase 1: got %d want %d", fromPhase1, PhaseCompile)
	}
	// Level-0 identity: NextPhase() from the runtime frame equals Expand().
	fromPhase0 := topLevel.NextPhase().PhaseLevel()
	if fromPhase0 != PhaseExpand {
		t.Fatalf("NextPhase from phase 0: got %d want %d (must equal Expand())", fromPhase0, PhaseExpand)
	}
	// int8 ceiling: base 127 + 1 overflows — must reject, not wrap to -128.
	q, err := p1.NextPhaseChecked(Phase(127))
	if err == nil {
		t.Fatalf("NextPhaseChecked at 127 must reject; got frame at phase %d", q.PhaseLevel())
	}
}
