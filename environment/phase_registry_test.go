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
	"sort"
	"sync"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestPhaseRegistry_Creation(t *testing.T) {
	topLevel := NewTopLevelEnvironmentFrame()

	qt.Assert(t, topLevel.phases, qt.IsNotNil)
	qt.Assert(t, topLevel.phases.TopLevelFrame(), qt.Equals, topLevel)

	// Phase 0 should be registered as TopLevel
	qt.Assert(t, topLevel.phases.Get(PhaseRuntime), qt.Equals, topLevel)
}

func TestPhaseRegistry_GetOrCreate(t *testing.T) {
	topLevel := NewTopLevelEnvironmentFrame()

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
	topLevel := NewTopLevelEnvironmentFrame()

	// Get non-existent phase returns nil
	qt.Assert(t, topLevel.phases.Get(5), qt.IsNil)

	// After creating, Get returns the same instance
	phase5 := topLevel.phases.GetOrCreate(5)
	qt.Assert(t, topLevel.phases.Get(5), qt.Equals, phase5)
}

func TestPhaseRegistry_Phases(t *testing.T) {
	topLevel := NewTopLevelEnvironmentFrame()

	// Initially only phase 0
	phases := topLevel.phases.Phases()
	qt.Assert(t, len(phases), qt.Equals, 1)
	qt.Assert(t, phases[0], qt.Equals, PhaseRuntime)

	// Create more phases
	topLevel.phases.GetOrCreate(PhaseExpand)
	topLevel.phases.GetOrCreate(PhaseCompile)
	topLevel.phases.GetOrCreate(-1)

	phases = topLevel.phases.Phases()
	sort.Ints(phases)
	qt.Assert(t, len(phases), qt.Equals, 4)
	qt.Assert(t, phases, qt.DeepEquals, []int{-1, 0, 1, 2})
}

func TestPhaseRegistry_PhaseEnvParentsToTopLevel(t *testing.T) {
	topLevel := NewTopLevelEnvironmentFrame()

	phase1 := topLevel.phases.GetOrCreate(PhaseExpand)
	phase2 := topLevel.phases.GetOrCreate(PhaseCompile)

	// All phase environments parent to TopLevel
	qt.Assert(t, phase1.Parent(), qt.Equals, topLevel)
	qt.Assert(t, phase2.Parent(), qt.Equals, topLevel)
}

func TestPhaseRegistry_PhaseEnvHasOwnGlobal(t *testing.T) {
	topLevel := NewTopLevelEnvironmentFrame()

	phase1 := topLevel.phases.GetOrCreate(PhaseExpand)
	phase2 := topLevel.phases.GetOrCreate(PhaseCompile)

	// Each phase has its own GlobalEnvironmentFrame
	qt.Assert(t, phase1.GlobalEnvironment(), qt.Not(qt.Equals), topLevel.GlobalEnvironment())
	qt.Assert(t, phase2.GlobalEnvironment(), qt.Not(qt.Equals), topLevel.GlobalEnvironment())
	qt.Assert(t, phase2.GlobalEnvironment(), qt.Not(qt.Equals), phase1.GlobalEnvironment())
}

func TestPhaseRegistry_Concurrent(t *testing.T) {
	topLevel := NewTopLevelEnvironmentFrame()

	var wg sync.WaitGroup
	const numGoroutines = 10
	const numPhases = 5

	results := make([][]*EnvironmentFrame, numGoroutines)
	for i := 0; i < numGoroutines; i++ {
		results[i] = make([]*EnvironmentFrame, numPhases)
	}

	// Concurrently access phases
	for i := 0; i < numGoroutines; i++ {
		wg.Add(1)
		go func(goroutineID int) {
			defer wg.Done()
			for phase := 0; phase < numPhases; phase++ {
				results[goroutineID][phase] = topLevel.phases.GetOrCreate(phase)
			}
		}(i)
	}

	wg.Wait()

	// All goroutines should get the same instances
	for phase := 0; phase < numPhases; phase++ {
		expected := results[0][phase]
		for goroutineID := 1; goroutineID < numGoroutines; goroutineID++ {
			qt.Assert(t, results[goroutineID][phase], qt.Equals, expected,
				qt.Commentf("goroutine %d phase %d", goroutineID, phase))
		}
	}
}

func TestPhaseConstants(t *testing.T) {
	qt.Assert(t, PhaseTemplate, qt.Equals, -1)
	qt.Assert(t, PhaseRuntime, qt.Equals, 0)
	qt.Assert(t, PhaseExpand, qt.Equals, 1)
	qt.Assert(t, PhaseCompile, qt.Equals, 2)
}
