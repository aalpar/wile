// Copyright 2025 Aaron Alpar
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

import "sync"

// Phase level constants for standard Scheme phases.
// These match Racket's phase numbering convention.
const (
	PhaseTemplate = -1 // for-template (template instantiation)
	PhaseRuntime  = 0  // Runtime execution (phase 0)
	PhaseExpand   = 1  // Macro expansion (for-syntax, phase 1)
	PhaseCompile  = 2  // Compile-time (for-meta 2, phase 2)
)

// PhaseRegistry manages phase-indexed environment frames.
// It provides O(1) access to any phase environment and supports
// lazy creation of phase environments on first access.
//
// The registry is owned by the TopLevel environment and shared
// across all child environments via pointer. This enables any
// environment frame to access any phase directly.
//
// Thread-safe: All operations are protected by a read-write mutex
// to support concurrent macro expansion.
type PhaseRegistry struct {
	mu   sync.RWMutex
	envs map[int]*EnvironmentFrame
	// topLevelEnv is the owning TopLevelEnvironment (nil for legacy environments)
	topLevelEnv *TopLevelEnvironment
	// topLevelEnvFrm is the runtime (phase 0) environment frame
	topLevelEnvFrm *EnvironmentFrame
}

// Get returns the environment for the given phase, or nil if not yet created.
func (r *PhaseRegistry) Get(phase int) *EnvironmentFrame {
	r.mu.RLock()
	defer r.mu.RUnlock()
	return r.envs[phase]
}

// GetOrCreate returns the environment for the given phase, creating it if needed.
// Phase 0 always returns the TopLevel environment.
// Other phases are lazily created with their own GlobalEnvironmentFrame.
func (r *PhaseRegistry) GetOrCreate(phase int) *EnvironmentFrame {
	// Fast path: check with read lock
	r.mu.RLock()
	env := r.envs[phase]
	r.mu.RUnlock()
	if env != nil {
		return env
	}

	// Slow path: create with write lock
	r.mu.Lock()
	defer r.mu.Unlock()

	// Double-check after acquiring write lock
	env = r.envs[phase]
	if env != nil {
		return env
	}

	// Create new phase environment
	env = r.createPhaseEnv(phase)
	r.envs[phase] = env
	return env
}

// createPhaseEnv creates a new environment frame for the given phase.
// Must be called with write lock held.
func (r *PhaseRegistry) createPhaseEnv(phase int) *EnvironmentFrame {
	// Create a new GlobalEnvironmentFrame for this phase.
	global := NewGlobalEnvironmentFrame()
	global.topLevel = r.topLevelEnv

	q := &EnvironmentFrame{
		parent:     r.topLevelEnvFrm, // Phase envs parent to runtime frame
		local:      nil,
		global:     global,
		phaseLevel: phase,
		phases:     r,
		topLevel:   r.topLevelEnv,
	}
	return q
}

// Phases returns all currently instantiated phase levels.
// Useful for debugging and introspection.
func (r *PhaseRegistry) Phases() []int {
	r.mu.RLock()
	defer r.mu.RUnlock()

	result := make([]int, 0, len(r.envs))
	for phase := range r.envs {
		result = append(result, phase)
	}
	return result
}

// TopLevelFrame returns the runtime (phase 0) environment frame.
func (r *PhaseRegistry) TopLevelFrame() *EnvironmentFrame {
	return r.topLevelEnvFrm
}

// TopLevelEnv returns the owning TopLevelEnvironment.
// Returns nil for legacy environments created without TopLevelEnvironment.
func (r *PhaseRegistry) TopLevelEnv() *TopLevelEnvironment {
	return r.topLevelEnv
}
