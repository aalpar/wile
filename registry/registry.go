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
	"github.com/aalpar/wile/werr"
)

// PrimitiveSpec defines a primitive to be registered.
type PrimitiveSpec struct {
	Name       string
	ParamCount int
	IsVariadic bool
	Impl       machine.ForeignFunction
	Doc        string                  // optional: brief description
	ParamNames []string                // optional: parameter names
	Category   string                  // optional: grouping category
	ParamTypes []values.TypeConstraint // optional: type contract per parameter
	ReturnType values.TypeConstraint   // optional: return type (nil = unspecified)
	Keywords   []string                // optional: searchable tags
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

// BindingSpec defines a compile-time binding with optional documentation.
type BindingSpec struct {
	Name string
	Doc  string
}

// DocEntry associates a documentation string with a named binding.
type DocEntry struct {
	Name string
	Doc  string
}

// Registry is the central registry for primitives.
type Registry struct {
	mu           sync.RWMutex
	primitives   []PrimitiveRegistration
	bindingSpecs []BindingSpec // Compile-time only bindings
	docs         []DocEntry
	initFuncs    []InitFunc
	macroSources []string
	globalValues []GlobalValue
}

// NewRegistry creates a new empty registry.
func NewRegistry() *Registry {
	q := &Registry{
		primitives:   make([]PrimitiveRegistration, 0, 128),
		bindingSpecs: make([]BindingSpec, 0, 32),
		docs:         make([]DocEntry, 0, 16),
		initFuncs:    make([]InitFunc, 0, 8),
		macroSources: make([]string, 0, 4),
		globalValues: make([]GlobalValue, 0, 4),
	}
	return q
}

// AddPrimitive registers a primitive with the given phases.
func (p *Registry) AddPrimitive(spec PrimitiveSpec, phases Phase) {
	validateParamTypes(spec)
	p.mu.Lock()
	defer p.mu.Unlock()
	p.primitives = append(p.primitives, PrimitiveRegistration{
		Spec:   spec,
		Phases: phases,
	})
}

// AddPrimitives registers multiple primitives with the given phases.
func (p *Registry) AddPrimitives(specs []PrimitiveSpec, phases Phase) {
	for _, spec := range specs {
		validateParamTypes(spec)
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	for _, spec := range specs {
		p.primitives = append(p.primitives, PrimitiveRegistration{
			Spec:   spec,
			Phases: phases,
		})
	}
}

// validateParamTypes panics if ParamTypes is non-empty but inconsistent with ParamCount.
// For non-variadic: len(ParamTypes) must equal ParamCount.
// For variadic: len(ParamTypes) must be in [1, ParamCount].
func validateParamTypes(spec PrimitiveSpec) {
	n := len(spec.ParamTypes)
	if n == 0 {
		return
	}
	if spec.IsVariadic {
		if n < 1 || n > spec.ParamCount {
			panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"AddPrimitive %q: ParamTypes length %d out of range [1, %d] for variadic",
				spec.Name, n, spec.ParamCount))
		}
		return
	}
	if n != spec.ParamCount {
		panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"AddPrimitive %q: ParamTypes length %d != ParamCount %d",
			spec.Name, n, spec.ParamCount))
	}
}

// AddBinding registers a compile-time only binding (no runtime value).
func (p *Registry) AddBinding(name string) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.bindingSpecs = append(p.bindingSpecs, BindingSpec{Name: name})
}

// AddBindings registers multiple compile-time only bindings.
func (p *Registry) AddBindings(names []string) {
	p.mu.Lock()
	defer p.mu.Unlock()
	for _, name := range names {
		p.bindingSpecs = append(p.bindingSpecs, BindingSpec{Name: name})
	}
}

// AddBindingSpecs registers multiple compile-time bindings with optional documentation.
func (p *Registry) AddBindingSpecs(specs []BindingSpec) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.bindingSpecs = append(p.bindingSpecs, specs...)
}

// AddDocumentation registers a documentation entry for a named binding.
// The documentation is applied to existing bindings during ApplyDocs.
func (p *Registry) AddDocumentation(name, doc string) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.docs = append(p.docs, DocEntry{Name: name, Doc: doc})
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

// AddDocOnlyPrimitive registers a documentation-only primitive entry.
// It does not create a runtime binding — used for Scheme-defined procedures
// that are already bound in the environment but need registry visibility
// for apropos/topics. Skips registration if a primitive with the same name
// already exists (Go primitives take precedence).
func (p *Registry) AddDocOnlyPrimitive(spec PrimitiveSpec) {
	p.mu.Lock()
	defer p.mu.Unlock()

	for _, reg := range p.primitives {
		if reg.Spec.Name == spec.Name {
			return
		}
	}

	p.primitives = append(p.primitives, PrimitiveRegistration{
		Spec:   spec,
		Phases: 0, // doc-only, not applied to environments
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
	return len(p.bindingSpecs)
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

// Bindings returns the names of compile-time bindings.
func (p *Registry) Bindings() []string {
	p.mu.RLock()
	defer p.mu.RUnlock()
	q := make([]string, len(p.bindingSpecs))
	for i, spec := range p.bindingSpecs {
		q[i] = spec.Name
	}
	return q
}

// BindingSpecs returns a defensive copy of the compile-time binding specs.
func (p *Registry) BindingSpecs() []BindingSpec {
	p.mu.RLock()
	defer p.mu.RUnlock()
	q := make([]BindingSpec, len(p.bindingSpecs))
	copy(q, p.bindingSpecs)
	return q
}

// Docs returns a defensive copy of the documentation entries.
func (p *Registry) Docs() []DocEntry {
	p.mu.RLock()
	defer p.mu.RUnlock()
	q := make([]DocEntry, len(p.docs))
	copy(q, p.docs)
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
		bindingSpecs: make([]BindingSpec, len(p.bindingSpecs)),
		docs:         make([]DocEntry, len(p.docs)),
		initFuncs:    make([]InitFunc, len(p.initFuncs)),
		macroSources: make([]string, len(p.macroSources)),
		globalValues: make([]GlobalValue, len(p.globalValues)),
	}
	copy(q.primitives, p.primitives)
	copy(q.bindingSpecs, p.bindingSpecs)
	copy(q.docs, p.docs)
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

// Without returns a new Registry with the named primitives removed.
// Names that don't match any registered primitive are silently ignored.
// Compile-time bindings, init funcs, macro sources, and global values
// are copied unchanged.
func (p *Registry) Without(names ...string) *Registry {
	return p.filterPrimitives(names, func(reg PrimitiveRegistration) string {
		return reg.Spec.Name
	})
}

// WithoutCategory returns a new Registry with all primitives in the
// named categories removed. Categories are matched against PrimitiveSpec.Category.
// Compile-time bindings, init funcs, macro sources, and global values
// are copied unchanged.
func (p *Registry) WithoutCategory(categories ...string) *Registry {
	return p.filterPrimitives(categories, func(reg PrimitiveRegistration) string {
		return reg.Spec.Category
	})
}

// filterPrimitives returns a new Registry with primitives excluded when
// keyFn(reg) matches any value in exclude. Non-primitive fields are copied unchanged.
func (p *Registry) filterPrimitives(exclude []string, keyFn func(PrimitiveRegistration) string) *Registry {
	p.mu.RLock()
	defer p.mu.RUnlock()

	set := make(map[string]struct{}, len(exclude))
	for _, v := range exclude {
		set[v] = struct{}{}
	}

	q := &Registry{
		primitives:   make([]PrimitiveRegistration, 0, len(p.primitives)),
		bindingSpecs: make([]BindingSpec, len(p.bindingSpecs)),
		docs:         make([]DocEntry, len(p.docs)),
		initFuncs:    make([]InitFunc, len(p.initFuncs)),
		macroSources: make([]string, len(p.macroSources)),
		globalValues: make([]GlobalValue, len(p.globalValues)),
	}
	for _, reg := range p.primitives {
		_, ok := set[keyFn(reg)]
		if ok {
			continue
		}
		q.primitives = append(q.primitives, reg)
	}
	copy(q.bindingSpecs, p.bindingSpecs)
	copy(q.docs, p.docs)
	copy(q.initFuncs, p.initFuncs)
	copy(q.macroSources, p.macroSources)
	copy(q.globalValues, p.globalValues)
	return q
}

// WithoutBindings returns a new Registry with the named compile-time
// bindings removed. Use after Without to fully erase a name that exists
// as both a primitive and a compile-time binding (e.g., set!).
// Primitives, init funcs, macro sources, and global values are copied unchanged.
func (p *Registry) WithoutBindings(names ...string) *Registry {
	p.mu.RLock()
	defer p.mu.RUnlock()

	exclude := make(map[string]struct{}, len(names))
	for _, name := range names {
		exclude[name] = struct{}{}
	}

	q := &Registry{
		primitives:   make([]PrimitiveRegistration, len(p.primitives)),
		bindingSpecs: make([]BindingSpec, 0, len(p.bindingSpecs)),
		docs:         make([]DocEntry, len(p.docs)),
		initFuncs:    make([]InitFunc, len(p.initFuncs)),
		macroSources: make([]string, len(p.macroSources)),
		globalValues: make([]GlobalValue, len(p.globalValues)),
	}
	copy(q.primitives, p.primitives)
	for _, spec := range p.bindingSpecs {
		_, ok := exclude[spec.Name]
		if ok {
			continue
		}
		q.bindingSpecs = append(q.bindingSpecs, spec)
	}
	copy(q.docs, p.docs)
	copy(q.initFuncs, p.initFuncs)
	copy(q.macroSources, p.macroSources)
	copy(q.globalValues, p.globalValues)
	return q
}
