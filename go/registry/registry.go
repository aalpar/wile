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

// Package registry provides a plugin architecture for registering Scheme primitives.
//
// The registry allows extensions to register primitives that are applied to
// environments at initialization time. Primitives can be registered for different
// phases: runtime, expand-time, and compile-time.
package registry

import (
	"sync"

	"github.com/aalpar/wile/go/machine"
)

// PrimitiveSpec defines a primitive to be registered.
type PrimitiveSpec struct {
	Name       string
	ParamCount int
	IsVariadic bool
	Impl       machine.ForeignFunction
}

// PrimitiveRegistration holds a primitive and its phases.
type PrimitiveRegistration struct {
	Spec   PrimitiveSpec
	Phases Phase
}

// InitFunc is called after primitives are registered.
type InitFunc func(ApplyContext) error

// Registry is the central registry for primitives.
type Registry struct {
	mu           sync.RWMutex
	primitives   []PrimitiveRegistration
	bindings     []string // Compile-time only bindings
	initFuncs    []InitFunc
	macroSources []string
}

// NewRegistry creates a new empty registry.
func NewRegistry() *Registry {
	q := &Registry{
		primitives:   make([]PrimitiveRegistration, 0, 128),
		bindings:     make([]string, 0, 16),
		initFuncs:    make([]InitFunc, 0, 8),
		macroSources: make([]string, 0, 4),
	}
	return q
}

// AddPrimitive registers a primitive with the given phases.
func (p *Registry) AddPrimitive(spec PrimitiveSpec, phases Phase) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.primitives = append(p.primitives, PrimitiveRegistration{
		Spec:   spec,
		Phases: phases,
	})
}

// AddPrimitives registers multiple primitives with the given phases.
func (p *Registry) AddPrimitives(specs []PrimitiveSpec, phases Phase) {
	p.mu.Lock()
	defer p.mu.Unlock()
	for _, spec := range specs {
		p.primitives = append(p.primitives, PrimitiveRegistration{
			Spec:   spec,
			Phases: phases,
		})
	}
}

// AddBinding registers a compile-time only binding (no runtime value).
func (p *Registry) AddBinding(name string) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.bindings = append(p.bindings, name)
}

// AddBindings registers multiple compile-time only bindings.
func (p *Registry) AddBindings(names []string) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.bindings = append(p.bindings, names...)
}

// AddInitFunc registers an initialization function.
func (p *Registry) AddInitFunc(f InitFunc) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.initFuncs = append(p.initFuncs, f)
}

// AddMacroSource adds Scheme source code for bootstrap macros.
func (p *Registry) AddMacroSource(source string) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.macroSources = append(p.macroSources, source)
}

// PrimitiveCount returns the number of registered primitives.
func (p *Registry) PrimitiveCount() int {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return len(p.primitives)
}

// BindingCount returns the number of compile-time bindings.
func (p *Registry) BindingCount() int {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return len(p.bindings)
}

// MacroSources returns copies of macro source strings.
func (p *Registry) MacroSources() []string {
	p.mu.RLock()
	defer p.mu.RUnlock()
	q := make([]string, len(p.macroSources))
	copy(q, p.macroSources)
	return q
}

// Primitives returns a copy of the primitive registrations.
func (p *Registry) Primitives() []PrimitiveRegistration {
	p.mu.RLock()
	defer p.mu.RUnlock()
	q := make([]PrimitiveRegistration, len(p.primitives))
	copy(q, p.primitives)
	return q
}

// Bindings returns a copy of the compile-time bindings.
func (p *Registry) Bindings() []string {
	p.mu.RLock()
	defer p.mu.RUnlock()
	q := make([]string, len(p.bindings))
	copy(q, p.bindings)
	return q
}

// InitFuncs returns a copy of the initialization functions.
func (p *Registry) InitFuncs() []InitFunc {
	p.mu.RLock()
	defer p.mu.RUnlock()
	q := make([]InitFunc, len(p.initFuncs))
	copy(q, p.initFuncs)
	return q
}

// Clone creates a copy of the registry.
func (p *Registry) Clone() *Registry {
	p.mu.RLock()
	defer p.mu.RUnlock()

	q := &Registry{
		primitives:   make([]PrimitiveRegistration, len(p.primitives)),
		bindings:     make([]string, len(p.bindings)),
		initFuncs:    make([]InitFunc, len(p.initFuncs)),
		macroSources: make([]string, len(p.macroSources)),
	}
	copy(q.primitives, p.primitives)
	copy(q.bindings, p.bindings)
	copy(q.initFuncs, p.initFuncs)
	copy(q.macroSources, p.macroSources)
	return q
}
