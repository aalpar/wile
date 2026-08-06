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
	"fmt"
	"sync"
)

// Phase-dependent binding: the same symbol can bind to different values at
// different phases (runtime, expand, compile). This follows Racket's phase
// model. See Flatt 2002, "Composable and Compilable Macros".
// See BIBLIOGRAPHY.md "Composable and Compilable Macros (Flatt 2002)".

// Phase identifies a stage of compilation/evaluation. Values match
// Racket's phase numbering convention.
//
// Phase indexes PhaseRegistry.envs and serves as the typed value for
// EnvironmentFrame.phaseLevel. The companion type registry.PhaseSet
// is a bitset over non-negative Phase values used for primitive
// registration.
//
// ADDING A NEW PHASE requires updates in these locations:
//
//  1. environment/phase_registry.go (this file) — add a Phase constant
//     and a String() case.
//  2. registry/phase.go — add the matching PhaseSet<Name> bit constant
//     if the new phase is representable in a PhaseSet (i.e. phase ≥ 0
//     and phase < phaseSetBits). The init() assertion verifies the
//     bit position matches the Phase index.
//  3. registry/apply.go — extend phaseTargets if primitives may
//     register at the new phase.
//  4. wile/options.go — re-export so embedders can name the constant.
type Phase int8

const (
	PhaseTemplate Phase = -1 // for-template (template instantiation)
	PhaseRuntime  Phase = 0  // Runtime execution (phase 0)
	PhaseExpand   Phase = 1  // Macro expansion (for-syntax, phase 1)
	PhaseCompile  Phase = 2  // Compile-time (for-meta 2, phase 2)
)

// String returns a human-readable name for the phase.
func (p Phase) String() string {
	switch p {
	case PhaseTemplate:
		return "template"
	case PhaseRuntime:
		return "runtime"
	case PhaseExpand:
		return "expand"
	case PhaseCompile:
		return "compile"
	default:
		return fmt.Sprintf("phase(%d)", int8(p))
	}
}

// Compare orders two phases numerically (Template < Runtime < Expand < Compile).
// Suitable for slices.SortFunc.
func (p Phase) Compare(other Phase) int {
	return int(p) - int(other)
}

// PhaseRegistry manages phase-indexed environment frames.
// It provides O(1) access to any phase environment and supports
// lazy creation of phase environments on first access.
//
// The registry is owned by the Namespace and shared across all
// child environments via pointer. This enables any environment
// frame to access any phase directly.
//
// Thread-safe: All operations are protected by a read-write mutex
// to support concurrent macro expansion.
type PhaseRegistry struct {
	mu   sync.RWMutex
	envs map[Phase]*EnvironmentFrame
	// owner is the owning Namespace
	owner *Namespace
	// runtime is envs[PhaseRuntime], hoisted out of the map so lock-free readers
	// (ownsSealedAxis, phaseParent) never race GetOrCreate's map write.
	// Immutable after construction.
	runtime *EnvironmentFrame
	// seals is this registry's sealed axis: one immutable frame per sealedAxis row.
	// Written once by the constructor and never mutated, which is what lets
	// phaseParent read it while holding the write lock.
	//
	// It moved here from Namespace because a library env deliberately SHARES its
	// parent's namespace while needing a seal of its own; the PhaseRegistry is the
	// thing each owner already has exactly one of.
	//
	// Every owner that has a sealed axis has ALL of it. Owners differ in what gets
	// APPLIED into their seals, never in which phases they seal — a per-owner
	// subset would mean sealedAxis no longer describes the system, and "is this
	// phase sealed?" would need a "for whom?".
	seals map[Phase]*EnvironmentFrame
}

// sealAt returns this registry's seal for a phase and whether the phase has one.
// The structural and the routing question are now the same question: a phase
// either has a seal or it does not.
//
// False means the phase has no sealedAxis row (phase 2 and up), never "this owner
// skipped a row": newSealedAxisFrames builds every row for every owner.
func (p *PhaseRegistry) sealAt(phase Phase) (*EnvironmentFrame, bool) {
	frame, ok := p.seals[phase]
	return frame, ok
}

// isSeal reports whether frame is one of this registry's sealed frames.
func (p *PhaseRegistry) isSeal(frame *EnvironmentFrame) bool {
	if frame == nil {
		return false
	}
	for _, sealed := range p.seals {
		if sealed == frame {
			return true
		}
	}
	return false
}

// hasSeals reports whether this registry owns a sealed axis at all. A registry
// built without one (test scaffolding) keeps the pre-carve behavior: phase frames
// parent to the phase-0 frame.
func (p *PhaseRegistry) hasSeals() bool {
	return len(p.seals) > 0
}

// Get returns the environment for the given phase, or nil if not yet created.
func (p *PhaseRegistry) Get(phase Phase) *EnvironmentFrame {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return p.envs[phase]
}

// GetOrCreate returns the environment for the given phase, creating it if needed.
// Phase 0 always returns the runtime environment.
// Other phases are lazily created with their own GlobalEnvironmentFrame.
func (p *PhaseRegistry) GetOrCreate(phase Phase) *EnvironmentFrame {
	// Fast path: check with read lock
	p.mu.RLock()
	env := p.envs[phase]
	p.mu.RUnlock()
	if env != nil {
		return env
	}

	// Slow path: create with write lock
	p.mu.Lock()
	defer p.mu.Unlock()

	// Double-check after acquiring write lock
	env = p.envs[phase]
	if env != nil {
		return env
	}

	// Create new phase environment
	env = p.createPhaseEnv(phase)
	p.envs[phase] = env
	return env
}

// createPhaseEnv creates a new environment frame for the given phase.
// Must be called with write lock held.
func (p *PhaseRegistry) createPhaseEnv(phase Phase) *EnvironmentFrame {
	// Create a new GlobalEnvironmentFrame for this phase.
	global := NewGlobalEnvironmentFrameAt(ExactPhase(phase), false)

	q := &EnvironmentFrame{
		// A phase frame parents to its phase's SEAL, never to the mutable runtime
		// frame: that skip past user defines and phase-0 imports is the hermeticity
		// cut. See plans/2026-07-10-hermetic-phases-core-impl.local.md and
		// plans/2026-07-22-free-template-id-hygiene-impl.local.md.
		parent:     p.phaseParent(phase),
		global:     global,
		phaseLevel: phase,
		phases:     p,
		namespace:  p.owner,
	}
	return q
}

// phaseParent selects a phase frame's lexical parent: the seal for this phase when this
// registry has one, else the registry's phase-0 seal. A phase with no seal of its own
// therefore parents to the base rather than to the phase below it, which is the
// climbing-tower invariant that the mutable axis introduces no phase->phase parent edge.
//
// This holds uniformly for a namespace and for a library env. Before the library gained a
// seal this returned the library's phase-0 frame — the one phase->phase edge in the tree,
// and the shape that made a library body's phase separation unenforceable.
//
// One constraint keeps this off the general routing seam: it runs under the registry's
// WRITE lock (createPhaseEnv), so it must not call anything that can re-enter GetOrCreate;
// p.runtime and p.seals are both immutable-after-construction reads.
func (p *PhaseRegistry) phaseParent(phase Phase) *EnvironmentFrame {
	if !p.hasSeals() {
		return p.runtime
	}
	sealed, ok := p.sealAt(phase)
	if ok {
		return sealed
	}
	// The phase-0 row is the axis's root and every axis has it — newPhaseRegistry
	// refuses one that does not, which is where the old read-time mustSeal check
	// went. So this is a lookup, not a fallback that can fail.
	base, _ := p.sealAt(PhaseRuntime)
	return base
}

// Phases returns all currently instantiated phase levels.
// Useful for debugging and introspection.
func (p *PhaseRegistry) Phases() []Phase {
	p.mu.RLock()
	defer p.mu.RUnlock()

	result := make([]Phase, 0, len(p.envs))
	for phase := range p.envs {
		result = append(result, phase)
	}
	return result
}

// TopLevelFrame returns the runtime (phase 0) environment frame.
func (p *PhaseRegistry) TopLevelFrame() *EnvironmentFrame {
	return p.Get(PhaseRuntime)
}

// Namespace returns the owning Namespace.
func (p *PhaseRegistry) Namespace() *Namespace {
	return p.owner
}
