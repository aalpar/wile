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
	// runtime is envs[PhaseRuntime], the owner's ROOT view. Hoisted out of the map
	// so lock-free readers never race GetOrCreate's map write, and because
	// createPhaseEnv reads its store while holding the write lock.
	// Immutable after construction.
	runtime *EnvironmentFrame
	// sealedViews caches this owner's SEALED-WRITE views, one per sealedAxis row:
	// the same store as envs, at the same phase, sealed. They are
	// what AtPhase's climb hands a sealed-write view (design §4.5), and what
	// SealedWriteViewAt returns to a registration target.
	//
	// Written once by the constructor and never mutated, so it is read without the
	// registry lock. It lives here rather than on Namespace because a library env
	// deliberately SHARES its parent's namespace while needing its own store and
	// its own sealed-write views; the PhaseRegistry is the thing each owner has
	// exactly one of.
	sealedViews map[Phase]*EnvironmentFrame
}

// sealedViewAt returns this owner's sealed-write view for a phase and whether the
// phase has one.
//
// False means the phase has no sealedAxis row (phase 2 and up), never "this owner
// skipped a row": every owner's registry is built with every row.
func (p *PhaseRegistry) sealedViewAt(phase Phase) (*EnvironmentFrame, bool) {
	frame, ok := p.sealedViews[phase]
	return frame, ok
}

// Get returns the environment for the given phase, or nil if not yet created.
func (p *PhaseRegistry) Get(phase Phase) *EnvironmentFrame {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return p.envs[phase]
}

// GetOrCreate returns the environment for the given phase, creating it if needed.
// Phase 0 always returns the owner's root view. Other phases are lazily minted as
// views over the SAME store — the map is a view cache, and AtPhase must keep
// returning a stable pointer per (owner, phase) because local expand envs chain
// off these frames and code compares frames by pointer.
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

// createPhaseEnv mints the owner's ordinary VIEW at the given phase: the one
// owner store, at that phase, mutable (not sealed), no lexical parent.
//
// Hermeticity is no longer a parent link that skips the mutable runtime — it is
// key disjointness in the store. A phase-N read is a candidate only for slots at
// exactly phase N or at the ambient (startup) coordinate, so phase-N code cannot
// see phase-M defines for any M != N, and still reaches primitives.
//
// Must be called with the write lock held; p.runtime is an
// immutable-after-construction read, so taking the store off it here cannot
// re-enter GetOrCreate.
func (p *PhaseRegistry) createPhaseEnv(phase Phase) *EnvironmentFrame {
	q := &EnvironmentFrame{
		global:     p.runtime.global,
		phaseLevel: phase,
		phases:     p,
		namespace:  p.owner,
	}
	return q
}

// Phases returns all currently instantiated phase levels.
// Useful for debugging and introspection.
func (p *PhaseRegistry) Phases() []Phase {
	return p.appendPhases(nil)
}

// appendPhases appends the instantiated phase levels to dst. EnvironmentFrame's
// PresentPhases merges these with the store's own phases, and taking an exactly
// sized slice from Phases() only to append to it costs a second allocation on
// the macro-compilation path.
// Thread-safe: uses RLock for read-only access.
func (p *PhaseRegistry) appendPhases(dst []Phase) []Phase {
	p.mu.RLock()
	defer p.mu.RUnlock()

	for phase := range p.envs {
		dst = append(dst, phase)
	}
	return dst
}

// TopLevelFrame returns the runtime (phase 0) environment frame.
func (p *PhaseRegistry) TopLevelFrame() *EnvironmentFrame {
	return p.Get(PhaseRuntime)
}

// Namespace returns the owning Namespace.
func (p *PhaseRegistry) Namespace() *Namespace {
	return p.owner
}
