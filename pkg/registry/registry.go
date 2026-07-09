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
	"slices"
	"sync"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// PrimitiveSpec defines a primitive to be registered.
type PrimitiveSpec struct {
	Name       string
	ParamCount int
	IsVariadic bool
	Impl       machine.ForeignFunction
	Doc        string   // optional: brief description
	ParamNames []string // optional: parameter names
	Category   string   // optional: grouping category
	// ParamTypes is an optional type contract per parameter. When IsVariadic
	// is true, the last slot annotates the per-element type of the rest list
	// (the Tuple at mc.Arg(ParamCount-1)), not the type of the rest list
	// itself. So `+` declares ParamCount:1, IsVariadic:true, ParamTypes:[TypeNumber],
	// meaning "every element of the variadic tail must be a Number."
	ParamTypes []values.TypeConstraint
	ReturnType values.TypeConstraint // optional: return type (nil = unspecified)
	Keywords   []string              // optional: searchable tags
	// InvokesProcedure marks a primitive that may call back into a Scheme
	// procedure (apply, map, sort, for-each, call/cc, dynamic-wind, …) and could
	// thereby transitively capture a continuation. Default false = "does not invoke
	// a Scheme procedure" = capture-safe: the frame-reclaim classifier may trust a
	// tail call to it as non-capturing. The flipped default is a SOUNDNESS
	// COMMITMENT — an unannotated procedure-invoking primitive would be wrongly
	// trusted.
	//
	// REQUIRED for any primitive whose Impl reaches ApplyCallable or runs a
	// sub-context (sub.Run()): set InvokesProcedure:true. Two guards in pkg/wile
	// enforce this — TestInvokesProcedureStaticGuard discovers the requirement
	// statically (AST of the Impl's call graph) and fails CI when the annotation is
	// missing; TestInvokesProcedureCompleteness pins the curated set behaviorally.
	//
	// It flows to BindingMeta.CaptureSafe at registration; the classifier reads the
	// binding flag because pkg/internal/validate must not import pkg/registry.
	InvokesProcedure bool
}

// PrimitiveRegistration holds a primitive and its phases.
type PrimitiveRegistration struct {
	Spec   PrimitiveSpec
	Phases PhaseSet
}

// InitFunc is called after all primitives and global values are registered.
type InitFunc func() error

// NamespaceInit is a per-engine initializer run by Apply once per engine, with
// that engine's runtime environment frame. Extensions use it to build
// per-Namespace state (e.g. I/O port parameters + caches) that must not be
// shared across engines.
type NamespaceInit func(env *environment.EnvironmentFrame) error

// GlobalValue pairs a name with a value to be registered as a global binding.
type GlobalValue struct {
	Name  string
	Value values.Value
}

// BindingSpec defines a compile-time binding with optional documentation.
//
// DocOnly entries carry doc text but do not install an environment binding —
// Apply skips them. They are appended via AddDocumentation or
// AddDocOnlyPrimitive and are the post-Phase-1 unification of what used to
// be a separate `docs []DocEntry` slice (collapsed per Finding 2 of
// plans/2026-05-18-registry-structural-reduction.md).
type BindingSpec struct {
	Name    string
	Doc     string
	DocOnly bool
}

// Registry is the central registry for primitives.
//
// ADDING A NEW REGISTRY CATEGORY requires updates in these locations:
//
//  1. Registry struct (this file)       — add the slice field
//  2. NewRegistry (this file)            — initialize the slice with a reasonable capacity
//  3. Registry.deepCopy (this file)      — extend the make + copy block to include the new slice
//  4. Add<Category> / Add<Category>s     — registration entry points (mutex-locked appender +
//     optional singular forwarder)
//  5. <Category>Count / <Category>s      — accessor pair returning a count and a defensive copy
//  6. Registry.Apply (registry/apply.go) — materialize the new category into the environment
//     at the appropriate lifecycle step
//
// All eight categories today (primitives, bindingSpecs, docPrimitives, initFuncs,
// macroSources, procedureSources, globalValues, namespaceInits) follow this pattern.
// Forgetting any step is a silent aliasing or drift hazard — Clone, Without,
// WithoutCategory, and WithoutBindings all assume deepCopy covers every field.
//
// Note: the `docs []DocEntry` slice was removed in Phase 1
// (plans/2026-05-18-registry-structural-reduction.md, Finding 2):
// simple AddDocumentation entries now share bindingSpecs (DocOnly=true);
// rich AddDocOnlyPrimitive entries moved to the dedicated docPrimitives
// slice (preserves full PrimitiveSpec metadata).
type Registry struct {
	mu sync.RWMutex
	// primitives: real Scheme primitives backed by ForeignFunction impls.
	primitives []PrimitiveRegistration
	// bindingSpecs: compile-time bindings (real) + simple doc-only entries
	// (DocOnly=true) registered via AddDocumentation. Both carry only Name+Doc.
	bindingSpecs []BindingSpec
	// docPrimitives: documentation-only primitives registered via
	// AddDocOnlyPrimitive (e.g., Scheme-defined procedures discovered via
	// library import). Carry the full PrimitiveSpec metadata
	// (Category, ParamNames, ParamTypes, ReturnType, Keywords) but no Impl
	// and no environment binding. Surfaced by ,doc / ,apropos / SearchDoc /
	// PrimitivesByCategory; not visible via Primitives() or FindPrimitive
	// (those are real-primitives-only).
	docPrimitives []PrimitiveSpec
	initFuncs     []InitFunc
	macroSources  []string
	// procedureSources holds runtime-procedure source (define forms) to be loaded
	// into the sealed-base frame, separate from macroSources (define-syntax forms)
	// which load into the mutable expand frame. The file boundary is the phase boundary.
	procedureSources []string
	globalValues     []GlobalValue
	// namespaceInits are per-engine initializers run by Apply once per engine,
	// with that engine's runtime environment frame. Extensions use them to build
	// per-Namespace state (e.g. I/O port parameters + caches) that must not be
	// shared across engines.
	namespaceInits []NamespaceInit
}

// NewRegistry creates a new empty registry.
func NewRegistry() *Registry {
	q := &Registry{
		primitives:       make([]PrimitiveRegistration, 0, 128),
		bindingSpecs:     make([]BindingSpec, 0, 48),
		docPrimitives:    make([]PrimitiveSpec, 0, 16),
		initFuncs:        make([]InitFunc, 0, 8),
		macroSources:     make([]string, 0, 4),
		procedureSources: make([]string, 0, 4),
		globalValues:     make([]GlobalValue, 0, 4),
		namespaceInits:   make([]NamespaceInit, 0, 4),
	}
	return q
}

// AddPrimitive registers a primitive with the given phases. Singular
// forwarder to AddPrimitives; the plural form is the single source of
// truth for validation + mutex lifecycle.
func (p *Registry) AddPrimitive(spec PrimitiveSpec, phases PhaseSet) {
	p.AddPrimitives([]PrimitiveSpec{spec}, phases)
}

// AddPrimitives registers multiple primitives with the given phases.
func (p *Registry) AddPrimitives(specs []PrimitiveSpec, phases PhaseSet) {
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

// validateParamTypes panics if a spec is internally inconsistent.
// A variadic spec must have ParamCount >= 1: the rest parameter occupies
// slot ParamCount-1, so ParamCount:0 would make bindArgs index bnds[:-1]
// and panic on first call (machine/arity.go).
// For ParamTypes (when non-empty): non-variadic requires len == ParamCount;
// variadic requires len in [1, ParamCount].
func validateParamTypes(spec PrimitiveSpec) {
	if spec.IsVariadic && spec.ParamCount < 1 {
		panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"AddPrimitive %q: variadic spec requires ParamCount >= 1, got %d",
			spec.Name, spec.ParamCount))
	}
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
// Singular forwarder to AddBindings.
func (p *Registry) AddBinding(name string) {
	p.AddBindings([]string{name})
}

// AddBindings registers multiple compile-time only bindings. Forwarder
// to AddBindingSpecs; the spec-typed form is the single source of truth
// for mutex lifecycle.
func (p *Registry) AddBindings(names []string) {
	specs := make([]BindingSpec, len(names))
	for i, name := range names {
		specs[i] = BindingSpec{Name: name}
	}
	p.AddBindingSpecs(specs)
}

// AddBindingSpecs registers multiple compile-time bindings with optional documentation.
func (p *Registry) AddBindingSpecs(specs []BindingSpec) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.bindingSpecs = append(p.bindingSpecs, specs...)
}

// AddDocumentation registers a documentation entry for a named binding.
// The documentation is applied to existing bindings during ApplyDocs.
// Forwarder to AddBindingSpecs with DocOnly=true — the doc-only entry
// lives in the bindingSpecs slice but Apply skips installing a binding for it.
func (p *Registry) AddDocumentation(name, doc string) {
	p.AddBindingSpecs([]BindingSpec{{Name: name, Doc: doc, DocOnly: true}})
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

// AddProcedureSource registers a runtime-procedure source (define forms) to be
// loaded into the sealed-base frame, separate from macro sources (define-syntax
// forms) which load into the mutable expand frame.
func (p *Registry) AddProcedureSource(source string) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.procedureSources = append(p.procedureSources, source)
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

// AddNamespaceInit registers a per-engine initializer run by Apply once per
// engine, with that engine's runtime environment frame. Extensions use it to
// build per-Namespace state (e.g. I/O port parameters + caches) that must not
// be shared across engines.
func (p *Registry) AddNamespaceInit(fn NamespaceInit) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.namespaceInits = append(p.namespaceInits, fn)
}

// NamespaceInits returns a defensive copy of the registered per-engine
// namespace initializers.
func (p *Registry) NamespaceInits() []NamespaceInit {
	p.mu.RLock()
	defer p.mu.RUnlock()
	q := make([]NamespaceInit, len(p.namespaceInits))
	copy(q, p.namespaceInits)
	return q
}

// AddDocOnlyPrimitive registers a documentation-only primitive entry.
// It does not create a runtime binding — used for Scheme-defined procedures
// that are already bound in the environment but need registry visibility
// for apropos/topics. Skips registration if a primitive with the same name
// already exists (Go primitives take precedence).
//
// Post-Phase-1: the entry lands in the dedicated docPrimitives slice
// (separate tier from real primitives). The full PrimitiveSpec metadata —
// Category, ParamNames, ParamTypes, ReturnType, Keywords — is preserved.
// Surfaced by SearchDoc / ,doc / ,apropos / PrimitivesByCategory; not
// visible via Primitives() or FindPrimitive (those return real
// primitives only).
func (p *Registry) AddDocOnlyPrimitive(spec PrimitiveSpec) {
	p.mu.Lock()
	defer p.mu.Unlock()

	for _, reg := range p.primitives {
		if reg.Spec.Name == spec.Name {
			return
		}
	}
	for _, dp := range p.docPrimitives {
		if dp.Name == spec.Name {
			return
		}
	}
	p.docPrimitives = append(p.docPrimitives, spec)
}

// PrimitiveCount returns the number of registered primitives.
func (p *Registry) PrimitiveCount() int {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return len(p.primitives)
}

// FindPrimitive returns the first registered primitive with the given name.
// If phase is the empty PhaseSet (zero), any phase matches; otherwise only
// primitives whose Phases overlap with phase are considered.
//
// When phase is zero, doc-only primitives (registered via AddDocOnlyPrimitive,
// stored in docPrimitives with Phases=0) are returned as a fallback after
// the real-primitives search. When phase is non-zero, doc-only primitives
// are never returned — they have Phases=0 which cannot overlap.
func (p *Registry) FindPrimitive(name string, phase PhaseSet) (PrimitiveRegistration, bool) {
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
	if phase == 0 {
		for _, dp := range p.docPrimitives {
			if dp.Name == name {
				return PrimitiveRegistration{Spec: dp, Phases: 0}, true
			}
		}
	}
	return PrimitiveRegistration{}, false
}

// HasPrimitive reports whether a primitive with the given name is registered.
// If phase is the empty PhaseSet (zero), any phase matches; otherwise only
// primitives whose Phases overlap with phase are considered.
func (p *Registry) HasPrimitive(name string, phase PhaseSet) bool {
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

// ProcedureSources returns copies of procedure source strings.
func (p *Registry) ProcedureSources() []string {
	p.mu.RLock()
	defer p.mu.RUnlock()
	q := make([]string, len(p.procedureSources))
	copy(q, p.procedureSources)
	return q
}

// WithProcedureSources returns a new Registry whose bootstrap procedure sources
// are replaced by the given slice; all other fields are copied unchanged via
// deepCopy. Used by a dialect to substitute a bootstrap fragment (e.g. swap the
// mutating vector-map/string-map for a mutation-free one) without mutating the
// shared registry. The receiver is never modified.
func (p *Registry) WithProcedureSources(sources []string) *Registry {
	q := p.deepCopy()
	q.procedureSources = slices.Clone(sources)
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

// Bindings returns the names of real compile-time bindings (DocOnly=false).
// DocOnly entries are excluded — use Docs() for those. BindingSpecs() returns
// the unfiltered slice if both are needed.
func (p *Registry) Bindings() []string {
	p.mu.RLock()
	defer p.mu.RUnlock()
	var q []string
	for _, spec := range p.bindingSpecs {
		if spec.DocOnly {
			continue
		}
		q = append(q, spec.Name)
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

// Docs returns a defensive copy of the doc-only binding specs (those
// with DocOnly=true). These are simple Name+Doc entries registered via
// AddDocumentation — they carry doc text but install no environment
// binding. Real bindings (DocOnly=false) are returned by BindingSpecs.
//
// Doc-only *primitives* with full metadata (registered via
// AddDocOnlyPrimitive) live in DocPrimitives, not here.
func (p *Registry) Docs() []BindingSpec {
	p.mu.RLock()
	defer p.mu.RUnlock()
	var q []BindingSpec
	for _, bs := range p.bindingSpecs {
		if bs.DocOnly {
			q = append(q, bs)
		}
	}
	return q
}

// DocPrimitives returns a defensive copy of the doc-only primitives
// (entries registered via AddDocOnlyPrimitive). These carry the full
// PrimitiveSpec metadata (Category, ParamNames, ParamTypes, ReturnType,
// Keywords) but no Impl and no environment binding — used to surface
// Scheme-defined procedures via ,doc / ,apropos / SearchDoc.
func (p *Registry) DocPrimitives() []PrimitiveSpec {
	p.mu.RLock()
	defer p.mu.RUnlock()
	q := make([]PrimitiveSpec, len(p.docPrimitives))
	copy(q, p.docPrimitives)
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
	return p.deepCopy()
}

// deepCopy returns a Registry whose 8 category slices are independent
// copies of p's. Element types are not transitively cloned — the copy is
// one level deep (slice header + backing array). Callers may overwrite
// individual slices on the returned Registry to express filter-style
// transformations; mutating q never affects p.
//
// TestDeepCopyTouchesEverySliceField (registry_test.go) is the drift-guard:
// it fails if a new slice field is added without extending the make+copy
// block below (fail-closed) or if a field is ever shared by reference
// (fail-open aliasing).
//
// Locking is internal: deepCopy acquires p.mu.RLock for the duration of
// the copy and releases it before returning. Callers MUST NOT hold p.mu
// when invoking this method.
func (p *Registry) deepCopy() *Registry {
	p.mu.RLock()
	defer p.mu.RUnlock()
	q := &Registry{
		primitives:       make([]PrimitiveRegistration, len(p.primitives)),
		bindingSpecs:     make([]BindingSpec, len(p.bindingSpecs)),
		docPrimitives:    make([]PrimitiveSpec, len(p.docPrimitives)),
		initFuncs:        make([]InitFunc, len(p.initFuncs)),
		macroSources:     make([]string, len(p.macroSources)),
		procedureSources: make([]string, len(p.procedureSources)),
		globalValues:     make([]GlobalValue, len(p.globalValues)),
		namespaceInits:   make([]NamespaceInit, len(p.namespaceInits)),
	}
	copy(q.primitives, p.primitives)
	copy(q.bindingSpecs, p.bindingSpecs)
	copy(q.docPrimitives, p.docPrimitives)
	copy(q.initFuncs, p.initFuncs)
	copy(q.macroSources, p.macroSources)
	copy(q.procedureSources, p.procedureSources)
	copy(q.globalValues, p.globalValues)
	copy(q.namespaceInits, p.namespaceInits)
	return q
}

// RuntimePrimitiveNamesSince returns the names of primitives registered
// at index >= startIndex that have PhaseSetRuntime. If startIndex is negative
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
		if p.primitives[i].Phases.Has(environment.PhaseRuntime) {
			names = append(names, p.primitives[i].Spec.Name)
		}
	}
	return names
}

// PrimitiveByName returns the registration for the named primitive, if any.
// Real primitives take precedence; doc-only primitives (Phases=0) are
// returned as a fallback when no real primitive with that name exists.
func (p *Registry) PrimitiveByName(name string) (PrimitiveRegistration, bool) {
	p.mu.RLock()
	defer p.mu.RUnlock()
	for _, reg := range p.primitives {
		if reg.Spec.Name == name {
			return reg, true
		}
	}
	for _, dp := range p.docPrimitives {
		if dp.Name == name {
			return PrimitiveRegistration{Spec: dp, Phases: 0}, true
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
// Includes both real primitives and doc-only primitives (the latter
// surface under their declared Category for ,doc / ,topics presentation).
// Doc-only entries appear with Phases=0 in the result.
func (p *Registry) PrimitivesByCategory() map[string][]PrimitiveRegistration {
	p.mu.RLock()
	defer p.mu.RUnlock()
	result := make(map[string][]PrimitiveRegistration)
	for _, reg := range p.primitives {
		result[reg.Spec.Category] = append(result[reg.Spec.Category], reg)
	}
	for _, dp := range p.docPrimitives {
		result[dp.Category] = append(result[dp.Category], PrimitiveRegistration{
			Spec:   dp,
			Phases: 0,
		})
	}
	return result
}

// Without returns a new Registry with the named primitives removed.
// Names that don't match any registered primitive are silently ignored.
// All non-primitive fields are copied unchanged via deepCopy.
func (p *Registry) Without(names ...string) *Registry {
	return p.filterPrimitives(names, func(reg PrimitiveRegistration) string {
		return reg.Spec.Name
	})
}

// WithoutCategory returns a new Registry with all primitives in the
// named categories removed. Categories are matched against PrimitiveSpec.Category.
// All non-primitive fields are copied unchanged via deepCopy.
func (p *Registry) WithoutCategory(categories ...string) *Registry {
	return p.filterPrimitives(categories, func(reg PrimitiveRegistration) string {
		return reg.Spec.Category
	})
}

// filterPrimitives returns a new Registry with primitives excluded when
// keyFn(reg) matches any value in exclude. Non-primitive fields are copied unchanged.
func (p *Registry) filterPrimitives(exclude []string, keyFn func(PrimitiveRegistration) string) *Registry {
	set := make(map[string]struct{}, len(exclude))
	for _, v := range exclude {
		set[v] = struct{}{}
	}

	q := p.deepCopy()
	q.primitives = slices.DeleteFunc(q.primitives, func(r PrimitiveRegistration) bool {
		_, ok := set[keyFn(r)]
		return ok
	})
	return q
}

// WithoutBindings returns a new Registry with the named compile-time
// bindings removed. Use after Without to fully erase a name that exists
// as both a primitive and a compile-time binding (e.g., set!).
// All other fields are copied unchanged via deepCopy.
//
// Post-Phase-1: only real bindings (DocOnly=false) are removed. DocOnly
// entries with the same name are preserved — they carry documentation
// for names that may live elsewhere (Scheme-defined procedures, library
// exports). Removing them would silently strip docs that the embedder
// likely wants kept.
func (p *Registry) WithoutBindings(names ...string) *Registry {
	exclude := make(map[string]struct{}, len(names))
	for _, name := range names {
		exclude[name] = struct{}{}
	}

	q := p.deepCopy()
	q.bindingSpecs = slices.DeleteFunc(q.bindingSpecs, func(s BindingSpec) bool {
		if s.DocOnly {
			return false
		}
		_, ok := exclude[s.Name]
		return ok
	})
	return q
}
