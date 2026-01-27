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
	"sync"

	"wile/syntax"
	"wile/values"
)

// TopLevelEnvironment represents a complete Wile VM instance.
// It owns per-instance symbol interning, syntax interning, phase registry,
// and library registry. This enables multiple independent Wile VMs in a
// single Go process.
//
// Design: TopLevelEnvironment is the root of the environment hierarchy.
// Each EnvironmentFrame holds a reference back to its TopLevelEnvironment
// to access shared resources (interning, phases, libraries).
//
// Symbol interning is per-TopLevelEnvironment (not global) to support:
//   - Multiple isolated Wile VMs
//   - Clean VM teardown without affecting other instances
//   - R7RS §6.5 symbol identity: "Two symbols are identical (in the sense of eq?)
//     if and only if their names are spelled the same way."
type TopLevelEnvironment struct {
	// symbolInterns is the per-instance symbol interning table.
	symbolInterns   map[values.Symbol]*values.Symbol
	symbolInternsMu sync.RWMutex

	// syntaxInterns is the per-instance syntax object interning table.
	syntaxInterns   map[values.Value]syntax.SyntaxValue
	syntaxInternsMu sync.RWMutex

	// phases is the phase registry for O(1) access to any phase environment.
	phases *PhaseRegistry

	// libraryRegistry is the R7RS library registry.
	// Stored as any to avoid circular dependency with machine package.
	libraryRegistry any

	// runtime is the phase 0 (runtime) environment frame.
	runtime *EnvironmentFrame
}

// NewTopLevelEnvironment creates a new TopLevelEnvironment.
// This is the primary entry point for creating an isolated Wile VM instance.
func NewTopLevelEnvironment() *TopLevelEnvironment {
	q := &TopLevelEnvironment{
		symbolInterns: make(map[values.Symbol]*values.Symbol),
		syntaxInterns: make(map[values.Value]syntax.SyntaxValue),
	}

	// Create the runtime (phase 0) environment frame
	global := newGlobalEnvironmentFrameWithTopLevel(q)
	q.runtime = &EnvironmentFrame{
		parent:     nil,
		local:      nil,
		global:     global,
		phaseLevel: PhaseRuntime,
		topLevel:   q,
	}

	// Create phase registry and register runtime as phase 0
	q.phases = newPhaseRegistryWithTopLevel(q)

	// Set the phases reference on the runtime frame
	q.runtime.phases = q.phases

	return q
}

// InternSymbol returns the canonical interned version of the given symbol.
// If a symbol with the same name has been interned before, that pointer is returned.
// Otherwise, the symbol is added to the intern table and returned.
// This ensures symbol identity (eq?) works correctly per R7RS §6.5.
//
// This function is thread-safe.
func (p *TopLevelEnvironment) InternSymbol(s *values.Symbol) *values.Symbol {
	if s == nil {
		return nil
	}

	// Fast path: check if already interned with read lock
	p.symbolInternsMu.RLock()
	if v, ok := p.symbolInterns[*s]; ok {
		p.symbolInternsMu.RUnlock()
		return v
	}
	p.symbolInternsMu.RUnlock()

	// Slow path: acquire write lock and intern
	p.symbolInternsMu.Lock()
	defer p.symbolInternsMu.Unlock()

	// Double-check after acquiring write lock (another goroutine may have interned it)
	if v, ok := p.symbolInterns[*s]; ok {
		return v
	}

	p.symbolInterns[*s] = s
	return s
}

// InternSyntax returns the canonical version of the given syntax value.
// If an equivalent syntax value has been seen before, it is returned.
// Otherwise, the value is added to the intern table and returned.
//
// This function is thread-safe.
func (p *TopLevelEnvironment) InternSyntax(k values.Value, v syntax.SyntaxValue) syntax.SyntaxValue {
	p.syntaxInternsMu.RLock()
	if val, ok := p.syntaxInterns[k]; ok {
		p.syntaxInternsMu.RUnlock()
		return val
	}
	p.syntaxInternsMu.RUnlock()

	p.syntaxInternsMu.Lock()
	defer p.syntaxInternsMu.Unlock()

	// Double-check after acquiring write lock
	if val, ok := p.syntaxInterns[k]; ok {
		return val
	}

	p.syntaxInterns[k] = v
	return v
}

// Runtime returns the runtime phase environment (phase 0).
// This is the main environment where top-level bindings live.
func (p *TopLevelEnvironment) Runtime() *EnvironmentFrame {
	return p.runtime
}

// AtPhase returns the environment for the given phase level, creating it if needed.
// Phase 0 is runtime, phase 1 is expansion (for-syntax), phase 2 is compile-time, etc.
// Negative phases (e.g., -1 for for-template) are also supported.
func (p *TopLevelEnvironment) AtPhase(phase int) *EnvironmentFrame {
	return p.phases.GetOrCreate(phase)
}

// Expand returns the expand phase environment (phase 1), creating it if needed.
// This is where syntax bindings from define-syntax are stored.
func (p *TopLevelEnvironment) Expand() *EnvironmentFrame {
	return p.AtPhase(PhaseExpand)
}

// Compile returns the compile phase environment (phase 2), creating it if needed.
// This is where compile-time procedures (syntax compilers) are stored.
func (p *TopLevelEnvironment) Compile() *EnvironmentFrame {
	return p.AtPhase(PhaseCompile)
}

// Phases returns the phase registry.
func (p *TopLevelEnvironment) Phases() *PhaseRegistry {
	return p.phases
}

// LibraryRegistry returns the library registry for R7RS library loading.
// The caller must type-assert to *machine.LibraryRegistry.
// Returns nil if no registry has been set.
func (p *TopLevelEnvironment) LibraryRegistry() any {
	return p.libraryRegistry
}

// SetLibraryRegistry sets the library registry for R7RS library loading.
// The registry should be a *machine.LibraryRegistry.
func (p *TopLevelEnvironment) SetLibraryRegistry(registry any) {
	p.libraryRegistry = registry
}

// NewChildRuntime creates a new runtime environment frame that shares this
// TopLevelEnvironment for symbol and syntax interning, but has its own
// GlobalEnvironmentFrame and PhaseRegistry for isolated bindings.
//
// This is used for library environments that need to:
//   - Share symbol interning (for R7RS §6.5 symbol identity)
//   - Have isolated bindings (library definitions don't leak)
//   - Have their own phase hierarchy
func (p *TopLevelEnvironment) NewChildRuntime() *EnvironmentFrame {
	// Create a new global frame sharing this TopLevelEnvironment
	global := newGlobalEnvironmentFrameWithTopLevel(p)

	// Create the runtime frame for the child
	runtime := &EnvironmentFrame{
		parent:     nil,
		local:      nil,
		global:     global,
		phaseLevel: PhaseRuntime,
		topLevel:   p, // Share the TopLevelEnvironment
	}

	// Create a new phase registry for the child
	childPhases := &PhaseRegistry{
		envs:           make(map[int]*EnvironmentFrame),
		topLevelEnv:    p, // Share the TopLevelEnvironment
		topLevelEnvFrm: runtime,
	}
	childPhases.envs[PhaseRuntime] = runtime
	runtime.phases = childPhases

	return runtime
}

// SymbolInternCount returns the number of interned symbols.
// This is intended for testing and debugging purposes.
func (p *TopLevelEnvironment) SymbolInternCount() int {
	p.symbolInternsMu.RLock()
	defer p.symbolInternsMu.RUnlock()
	return len(p.symbolInterns)
}

// SyntaxInternCount returns the number of interned syntax objects.
// This is intended for testing and debugging purposes.
func (p *TopLevelEnvironment) SyntaxInternCount() int {
	p.syntaxInternsMu.RLock()
	defer p.syntaxInternsMu.RUnlock()
	return len(p.syntaxInterns)
}

// newGlobalEnvironmentFrameWithTopLevel creates a new GlobalEnvironmentFrame
// that references the given TopLevelEnvironment.
func newGlobalEnvironmentFrameWithTopLevel(topLevel *TopLevelEnvironment) *GlobalEnvironmentFrame {
	q := &GlobalEnvironmentFrame{
		bindings: []*Binding{},
		keys:     map[values.Symbol]int{},
		topLevel: topLevel,
	}
	return q
}

// newPhaseRegistryWithTopLevel creates a new PhaseRegistry owned by the given TopLevelEnvironment.
func newPhaseRegistryWithTopLevel(topLevel *TopLevelEnvironment) *PhaseRegistry {
	q := &PhaseRegistry{
		envs:            make(map[int]*EnvironmentFrame),
		topLevelEnv:     topLevel,
		topLevelEnvFrm:  topLevel.runtime,
	}
	// TopLevel is phase 0 (runtime)
	q.envs[PhaseRuntime] = topLevel.runtime
	return q
}
