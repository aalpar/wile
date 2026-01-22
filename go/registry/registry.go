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

	"wile/machine"
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
func (r *Registry) AddPrimitive(spec PrimitiveSpec, phases Phase) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.primitives = append(r.primitives, PrimitiveRegistration{
		Spec:   spec,
		Phases: phases,
	})
}

// AddPrimitives registers multiple primitives with the given phases.
func (r *Registry) AddPrimitives(specs []PrimitiveSpec, phases Phase) {
	r.mu.Lock()
	defer r.mu.Unlock()
	for _, spec := range specs {
		r.primitives = append(r.primitives, PrimitiveRegistration{
			Spec:   spec,
			Phases: phases,
		})
	}
}

// AddBinding registers a compile-time only binding (no runtime value).
func (r *Registry) AddBinding(name string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.bindings = append(r.bindings, name)
}

// AddBindings registers multiple compile-time only bindings.
func (r *Registry) AddBindings(names []string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.bindings = append(r.bindings, names...)
}

// AddInitFunc registers an initialization function.
func (r *Registry) AddInitFunc(f InitFunc) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.initFuncs = append(r.initFuncs, f)
}

// AddMacroSource adds Scheme source code for bootstrap macros.
func (r *Registry) AddMacroSource(source string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.macroSources = append(r.macroSources, source)
}

// PrimitiveCount returns the number of registered primitives.
func (r *Registry) PrimitiveCount() int {
	r.mu.RLock()
	defer r.mu.RUnlock()
	return len(r.primitives)
}

// BindingCount returns the number of compile-time bindings.
func (r *Registry) BindingCount() int {
	r.mu.RLock()
	defer r.mu.RUnlock()
	return len(r.bindings)
}

// MacroSources returns copies of macro source strings.
func (r *Registry) MacroSources() []string {
	r.mu.RLock()
	defer r.mu.RUnlock()
	q := make([]string, len(r.macroSources))
	copy(q, r.macroSources)
	return q
}

// Primitives returns a copy of the primitive registrations.
func (r *Registry) Primitives() []PrimitiveRegistration {
	r.mu.RLock()
	defer r.mu.RUnlock()
	q := make([]PrimitiveRegistration, len(r.primitives))
	copy(q, r.primitives)
	return q
}

// Bindings returns a copy of the compile-time bindings.
func (r *Registry) Bindings() []string {
	r.mu.RLock()
	defer r.mu.RUnlock()
	q := make([]string, len(r.bindings))
	copy(q, r.bindings)
	return q
}

// InitFuncs returns a copy of the initialization functions.
func (r *Registry) InitFuncs() []InitFunc {
	r.mu.RLock()
	defer r.mu.RUnlock()
	q := make([]InitFunc, len(r.initFuncs))
	copy(q, r.initFuncs)
	return q
}

// Clone creates a copy of the registry.
func (r *Registry) Clone() *Registry {
	r.mu.RLock()
	defer r.mu.RUnlock()

	q := &Registry{
		primitives:   make([]PrimitiveRegistration, len(r.primitives)),
		bindings:     make([]string, len(r.bindings)),
		initFuncs:    make([]InitFunc, len(r.initFuncs)),
		macroSources: make([]string, len(r.macroSources)),
	}
	copy(q.primitives, r.primitives)
	copy(q.bindings, r.bindings)
	copy(q.initFuncs, r.initFuncs)
	copy(q.macroSources, r.macroSources)
	return q
}
