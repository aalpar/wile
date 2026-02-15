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

package registry

import (
	"sync"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
)

// PrimitiveSpec defines a primitive to be registered.
type PrimitiveSpec struct {
	Name       string
	ParamCount int
	IsVariadic bool
	Impl       machine.ForeignFunction
	Doc        string   // optional: one-line description
	ParamNames []string // optional: parameter names
	Category   string   // optional: grouping category
}

// PrimitiveRegistration holds a primitive and its phases.
type PrimitiveRegistration struct {
	Spec   PrimitiveSpec
	Phases Phase
}

// InitFunc is called after all primitives and global values are registered.
type InitFunc func() error

// GlobalValue pairs a name with a value to be registered as a global binding.
type GlobalValue struct {
	Name  string
	Value values.Value
}

// Registry is the central registry for primitives.
type Registry struct {
	mu           sync.RWMutex
	primitives   []PrimitiveRegistration
	bindings     []string // Compile-time only bindings
	initFuncs    []InitFunc
	macroSources []string
	globalValues []GlobalValue
}

// NewRegistry creates a new empty registry.
func NewRegistry() *Registry {
	q := &Registry{
		primitives:   make([]PrimitiveRegistration, 0, 128),
		bindings:     make([]string, 0, 16),
		initFuncs:    make([]InitFunc, 0, 8),
		macroSources: make([]string, 0, 4),
		globalValues: make([]GlobalValue, 0, 4),
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

// AddGlobalValue registers a named value to be bound as a global variable.
// Unlike AddPrimitive, this takes an arbitrary Value rather than a ForeignFunction.
func (p *Registry) AddGlobalValue(name string, value values.Value) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.globalValues = append(p.globalValues, GlobalValue{
		Name:  name,
		Value: value,
	})
}

// PrimitiveCount returns the number of registered primitives.
func (p *Registry) PrimitiveCount() int {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return len(p.primitives)
}

// FindPrimitive returns the first registered primitive with the given name.
// If phase is non-zero, only primitives active in that phase are considered.
// If phase is zero, any phase matches.
func (p *Registry) FindPrimitive(name string, phase Phase) (PrimitiveRegistration, bool) {
	p.mu.RLock()
	defer p.mu.RUnlock()
	for _, reg := range p.primitives {
		if reg.Spec.Name != name {
			continue
		}
		if phase != 0 && reg.Phases&phase == 0 {
			continue
		}
		return reg, true
	}
	return PrimitiveRegistration{}, false
}

// HasPrimitive reports whether a primitive with the given name is registered.
// If phase is non-zero, only primitives active in that phase are considered.
// If phase is zero, any phase matches.
func (p *Registry) HasPrimitive(name string, phase Phase) bool {
	_, ok := p.FindPrimitive(name, phase)
	return ok
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

// GlobalValues returns a copy of the global value registrations.
func (p *Registry) GlobalValues() []GlobalValue {
	p.mu.RLock()
	defer p.mu.RUnlock()
	q := make([]GlobalValue, len(p.globalValues))
	copy(q, p.globalValues)
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
		globalValues: make([]GlobalValue, len(p.globalValues)),
	}
	copy(q.primitives, p.primitives)
	copy(q.bindings, p.bindings)
	copy(q.initFuncs, p.initFuncs)
	copy(q.macroSources, p.macroSources)
	copy(q.globalValues, p.globalValues)
	return q
}

// RuntimePrimitiveNamesSince returns the names of primitives registered
// at index >= startIndex that have PhaseRuntime. If startIndex is negative
// it is treated as 0. If startIndex exceeds the primitive count, nil is returned.
func (p *Registry) RuntimePrimitiveNamesSince(startIndex int) []string {
	return p.RuntimePrimitiveNamesRange(startIndex, -1)
}

// RuntimePrimitiveNamesRange returns the names of runtime primitives registered
// in the index range [startIndex, endIndex). If endIndex is negative, all
// primitives from startIndex onward are included. Negative startIndex is
// treated as 0.
func (p *Registry) RuntimePrimitiveNamesRange(startIndex, endIndex int) []string {
	p.mu.RLock()
	defer p.mu.RUnlock()

	if startIndex < 0 {
		startIndex = 0
	}
	if startIndex >= len(p.primitives) {
		return nil
	}
	upper := len(p.primitives)
	if endIndex >= 0 && endIndex < upper {
		upper = endIndex
	}

	var names []string
	for i := startIndex; i < upper; i++ {
		if p.primitives[i].Phases&PhaseRuntime != 0 {
			names = append(names, p.primitives[i].Spec.Name)
		}
	}
	return names
}

// PrimitiveByName returns the registration for the named primitive, if any.
func (p *Registry) PrimitiveByName(name string) (PrimitiveRegistration, bool) {
	p.mu.RLock()
	defer p.mu.RUnlock()
	for _, reg := range p.primitives {
		if reg.Spec.Name == name {
			return reg, true
		}
	}
	return PrimitiveRegistration{}, false
}

// PrimitiveNames returns the names of all registered primitives in registration order.
func (p *Registry) PrimitiveNames() []string {
	p.mu.RLock()
	defer p.mu.RUnlock()
	names := make([]string, len(p.primitives))
	for i, reg := range p.primitives {
		names[i] = reg.Spec.Name
	}
	return names
}

// PrimitivesByCategory returns registered primitives grouped by category.
// Primitives with no category are grouped under the empty string key.
func (p *Registry) PrimitivesByCategory() map[string][]PrimitiveRegistration {
	p.mu.RLock()
	defer p.mu.RUnlock()
	result := make(map[string][]PrimitiveRegistration)
	for _, reg := range p.primitives {
		result[reg.Spec.Category] = append(result[reg.Spec.Category], reg)
	}
	return result
}
