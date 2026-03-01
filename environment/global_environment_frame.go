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
	"maps"
	"slices"
	"sync"
	"unsafe"

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// GlobalIndex identifies a global binding by its symbol key.
// Unlike LocalIndex which uses numeric indices, GlobalIndex uses the symbol
// directly since global bindings are accessed by name at runtime.
//
// Env records the definition-site global frame for cross-library macro hygiene.
// When a macro references a non-exported helper from its defining library,
// Env ensures the VM resolves the binding in the library's environment rather
// than the use-site environment. Nil means "use the current environment"
// (backward compatible default).
type GlobalIndex struct {
	Index *values.Symbol
	Env   *GlobalEnvironmentFrame
}

// NewGlobalIndex creates a new GlobalIndex for the given symbol.
func NewGlobalIndex(key *values.Symbol) *GlobalIndex {
	return &GlobalIndex{Index: key}
}

// SchemeString returns a string representation of this global index.
func (p *GlobalIndex) SchemeString() string {
	return fmt.Sprintf("<global-index %q>", p.Index.SchemeString())
}

// IsVoid returns true if this global index is nil.
func (p *GlobalIndex) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if this global index equals the given value.
func (p *GlobalIndex) EqualTo(value values.Value) bool {
	if value == nil || p == nil {
		return value == nil && p == nil
	}
	v, ok := value.(*GlobalIndex)
	if !ok {
		return false
	}
	if v.Index.EqualTo(p.Index) {
		return true
	}
	return false
}

// GlobalEnvironmentFrame represents global bindings for a single phase.
//
// Design: GlobalEnvironmentFrame has no hierarchy of its own. The environment
// hierarchy is managed by EnvironmentFrame via its parent field. Each phase
// (runtime, expand, compile) has its own GlobalEnvironmentFrame.
//
// Note: Symbol and syntax interning are delegated to TopLevelEnvironment,
// ensuring R7RS symbol identity works correctly across all phases.
//
// Thread safety: All access to keys and bindings is protected by mu.
// Fixes T2 from architectural review.
type GlobalEnvironmentFrame struct {
	// mu protects concurrent access to keys and bindings maps.
	// Use RLock for reads, Lock for writes and check-then-write patterns.
	mu sync.RWMutex
	// symbol to binding index lookup map
	keys     map[values.Symbol]int
	bindings []*Binding
	// topLevel is the owning TopLevelEnvironment
	topLevel *TopLevelEnvironment
}

// NewGlobalEnvironmentFrame creates a new global environment frame.
func NewGlobalEnvironmentFrame() *GlobalEnvironmentFrame {
	q := &GlobalEnvironmentFrame{
		bindings: []*Binding{},
		keys:     map[values.Symbol]int{},
	}
	return q
}

// Copy creates a deep copy of the global environment frame.
// Note that topLevel is shared (not copied) between original and copy.
// Bindings are batch-allocated (contiguous array) for cache locality
// and reduced GC pressure.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) Copy() values.Value {
	if p == nil {
		return (*GlobalEnvironmentFrame)(nil)
	}

	p.mu.RLock()
	defer p.mu.RUnlock()

	q := &GlobalEnvironmentFrame{
		topLevel: p.topLevel, // Shared, not copied
	}

	// Batch allocation: allocate all Bindings contiguously (1 allocation)
	// instead of N separate heap objects.
	allBindings := make([]Binding, len(p.bindings))
	q.bindings = make([]*Binding, len(p.bindings))
	for i, b := range p.bindings {
		allBindings[i] = Binding{
			value:       b.value,
			bindingType: b.bindingType,
			meta:        b.meta,
		}
		q.bindings[i] = &allBindings[i]
	}

	if p.keys != nil {
		q.keys = make(map[values.Symbol]int)
		maps.Copy(q.keys, p.keys)
	}
	return q
}

// Bindings returns a copy of the bindings slice.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) Bindings() []*Binding {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return slices.Clone(p.bindings)
}

// SetBindings replaces the bindings slice in this global environment.
// Thread-safe: uses full Lock for write access.
func (p *GlobalEnvironmentFrame) SetBindings(vs []*Binding) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.bindings = vs
}

// Keys returns a copy of the symbol-to-index mapping.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) Keys() map[values.Symbol]int {
	p.mu.RLock()
	defer p.mu.RUnlock()
	result := make(map[values.Symbol]int, len(p.keys))
	maps.Copy(result, p.keys)
	return result
}

// CreateGlobalBinding creates a new global binding with the given key and type.
// The key is interned before use. Returns the GlobalIndex and whether a new
// binding was created (false if the binding already existed).
// Thread-safe: uses full Lock to prevent TOCTOU races.
func (p *GlobalEnvironmentFrame) CreateGlobalBinding(key *values.Symbol, bt BindingType) (*GlobalIndex, bool) {
	r := p
	key = p.InternSymbol(key)

	// Use full Lock (not RLock) for check-then-write pattern to prevent TOCTOU
	p.mu.Lock()
	defer p.mu.Unlock()

	_, ok := r.keys[*key]
	if ok {
		q := NewGlobalIndex(key)
		return q, false
	}
	i := len(p.bindings)
	p.keys[*key] = i
	// append the new binding at index i
	p.bindings = append(p.bindings, NewBinding(values.Void, bt))
	q := NewGlobalIndex(key)
	return q, true
}

// GetGlobalIndex returns the GlobalIndex for the given symbol.
// Returns nil if the symbol is not bound in this global environment.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) GetGlobalIndex(key *values.Symbol) *GlobalIndex {
	ge := p
	key = p.InternSymbol(key)

	p.mu.RLock()
	_, ok := ge.keys[*key]
	p.mu.RUnlock()

	if !ok {
		return nil
	}
	q := NewGlobalIndex(key)
	return q
}

// GetOwnGlobalBinding returns the binding for the given GlobalIndex from this frame only.
// Unlike EnvironmentFrame.GetGlobalBinding, this does NOT traverse the parent chain.
// Returns nil if the binding does not exist in this frame.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) GetOwnGlobalBinding(gi *GlobalIndex) *Binding {
	ge := p
	key := p.InternSymbol(gi.Index)

	p.mu.RLock()
	i, ok := ge.keys[*key]
	if !ok {
		p.mu.RUnlock()
		return nil
	}
	bd := ge.bindings[i]
	p.mu.RUnlock()

	return bd
}

// SetOwnGlobalValue sets the value of the binding for the given GlobalIndex.
// Returns an error if the binding does not exist.
// Thread-safe: uses full Lock for write access.
func (p *GlobalEnvironmentFrame) SetOwnGlobalValue(gi *GlobalIndex, v values.Value) error {
	ge := p

	p.mu.Lock()
	i, ok := ge.keys[*gi.Index]
	if !ok {
		p.mu.Unlock()
		return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "no such global binding %q", gi.Index)
	}
	ge.bindings[i].value = v
	p.mu.Unlock()

	return nil
}

// IsVoid returns true if this global environment frame is nil.
func (p *GlobalEnvironmentFrame) IsVoid() bool {
	return p == nil
}

// SchemeString returns a string representation of this global environment.
func (p *GlobalEnvironmentFrame) SchemeString() string {
	return "#<global-environment>"
}

// EqualTo returns true if this global environment equals the given value.
// Two global environments are equal if they have the same bindings.
// Thread-safe: uses RLock for read-only access on both frames.
func (p *GlobalEnvironmentFrame) EqualTo(o values.Value) bool {
	if p == nil || o == nil {
		return p == nil && o == nil
	}
	v, ok := o.(*GlobalEnvironmentFrame)
	if !ok {
		return false
	}
	if p == v {
		return true
	}

	// Lock both frames in a consistent order to prevent deadlock
	// (lower pointer address first)
	first, second := p, v
	if uintptr(unsafe.Pointer(p)) > uintptr(unsafe.Pointer(v)) {
		first, second = v, p
	}

	first.mu.RLock()
	defer first.mu.RUnlock()
	second.mu.RLock()
	defer second.mu.RUnlock()

	if len(p.bindings) != len(v.bindings) {
		return false
	}
	for k, i := range p.keys {
		j, ok := v.keys[k]
		if !ok || i != j {
			return false
		}
		if !p.bindings[i].EqualTo(v.bindings[j]) {
			return false
		}
	}
	return true
}

// InternSymbol returns the canonical version of the given symbol.
// Delegates to TopLevelEnvironment.
// Per R7RS §6.5: "Two symbols are identical (in the sense of eq?) if and only
// if their names are spelled the same way."
// Panics if topLevel is nil.
func (p *GlobalEnvironmentFrame) InternSymbol(q *values.Symbol) *values.Symbol {
	if p.topLevel == nil {
		panic(werr.WrapForeignErrorf(
			werr.ErrMissingTopLevelEnvironment,
			"InternSymbol called on GlobalEnvironmentFrame without TopLevelEnvironment",
		))
	}
	return p.topLevel.InternSymbol(q)
}

// InternSyntax returns the canonical version of the given syntax value.
// If an equivalent syntax value has been seen before, it is returned.
// Otherwise, the value is added to the intern map and returned.
// Delegates to TopLevelEnvironment.
// Panics if topLevel is nil.
func (p *GlobalEnvironmentFrame) InternSyntax(k values.Value, v syntax.SyntaxValue) syntax.SyntaxValue {
	if p.topLevel == nil {
		panic(werr.WrapForeignErrorf(
			werr.ErrMissingTopLevelEnvironment,
			"InternSyntax called on GlobalEnvironmentFrame without TopLevelEnvironment",
		))
	}
	return p.topLevel.InternSyntax(k, v)
}

// LibraryRegistry returns the library registry for R7RS library loading.
// The caller must type-assert to *machine.LibraryRegistry.
// Returns nil if no registry has been set.
// Delegates to TopLevelEnvironment.
// Panics if topLevel is nil.
func (p *GlobalEnvironmentFrame) LibraryRegistry() any {
	if p.topLevel == nil {
		panic(werr.WrapForeignErrorf(
			werr.ErrMissingTopLevelEnvironment,
			"LibraryRegistry called on GlobalEnvironmentFrame without TopLevelEnvironment",
		))
	}
	return p.topLevel.LibraryRegistry()
}

// SetLibraryRegistry sets the library registry for R7RS library loading.
// The registry should be a *machine.LibraryRegistry.
// Delegates to TopLevelEnvironment.
// Panics if topLevel is nil.
func (p *GlobalEnvironmentFrame) SetLibraryRegistry(registry any) {
	if p.topLevel == nil {
		panic(werr.WrapForeignErrorf(
			werr.ErrMissingTopLevelEnvironment,
			"SetLibraryRegistry called on GlobalEnvironmentFrame without TopLevelEnvironment",
		))
	}
	p.topLevel.SetLibraryRegistry(registry)
}
