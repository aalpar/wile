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

import (
	"fmt"
	"maps"
	"slices"

	"github.com/aalpar/wile/go/syntax"
	"github.com/aalpar/wile/go/values"
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
type GlobalEnvironmentFrame struct {
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
func (p *GlobalEnvironmentFrame) Copy() values.Value {
	if p == nil {
		return (*GlobalEnvironmentFrame)(nil)
	}
	q := &GlobalEnvironmentFrame{
		topLevel: p.topLevel, // Shared, not copied
	}
	q.bindings = slices.Clone(p.bindings)
	for i := range p.bindings {
		q.bindings[i] = p.bindings[i].Copy().(*Binding)
	}
	if p.keys != nil {
		q.keys = make(map[values.Symbol]int)
		maps.Copy(q.keys, p.keys)
	}
	return q
}

// Bindings returns the slice of bindings in this global environment.
func (p *GlobalEnvironmentFrame) Bindings() []*Binding {
	return p.bindings
}

// SetBindings replaces the bindings slice in this global environment.
func (p *GlobalEnvironmentFrame) SetBindings(vs []*Binding) {
	p.bindings = vs
}

// Keys returns the symbol-to-index mapping for this global environment.
func (p *GlobalEnvironmentFrame) Keys() map[values.Symbol]int {
	return p.keys
}

// CreateGlobalBinding creates a new global binding with the given key and type.
// The key is interned before use. Returns the GlobalIndex and whether a new
// binding was created (false if the binding already existed).
func (p *GlobalEnvironmentFrame) CreateGlobalBinding(key *values.Symbol, bt BindingType) (*GlobalIndex, bool) {
	r := p
	key = p.InternSymbol(key)
	_, ok := r.keys[*key]
	if ok {
		q := NewGlobalIndex(key)
		return q, false
	}
	i := len(p.bindings)
	p.keys[*key] = i
	// append the new binding at index i
	p.SetBindings(append(p.Bindings(), NewBinding(values.Void, bt)))
	q := NewGlobalIndex(key)
	return q, true
}

// GetGlobalIndex returns the GlobalIndex for the given symbol.
// Returns nil if the symbol is not bound in this global environment.
func (p *GlobalEnvironmentFrame) GetGlobalIndex(key *values.Symbol) *GlobalIndex {
	ge := p
	key = p.InternSymbol(key)
	_, ok := ge.keys[*key]
	if !ok {
		return nil
	}
	q := NewGlobalIndex(key)
	return q
}

// GetOwnGlobalBinding returns the binding for the given GlobalIndex from this frame only.
// Unlike EnvironmentFrame.GetGlobalBinding, this does NOT traverse the parent chain.
// Returns nil if the binding does not exist in this frame.
func (p *GlobalEnvironmentFrame) GetOwnGlobalBinding(gi *GlobalIndex) *Binding {
	ge := p
	key := p.InternSymbol(gi.Index)
	i, ok := ge.keys[*key]
	if !ok {
		return nil
	}
	bd := ge.bindings[i]
	return bd
}

// SetOwnGlobalValue sets the value of the binding for the given GlobalIndex.
// Returns an error if the binding does not exist.
func (p *GlobalEnvironmentFrame) SetOwnGlobalValue(gi *GlobalIndex, v values.Value) error {
	ge := p
	i, ok := ge.keys[*gi.Index]
	if !ok {
		return values.WrapForeignErrorf(values.ErrNoSuchBinding, "no such global binding %q", gi.Index)
	}
	ge.bindings[i].value = v
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
		panic("InternSymbol called on GlobalEnvironmentFrame without TopLevelEnvironment")
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
		panic("InternSyntax called on GlobalEnvironmentFrame without TopLevelEnvironment")
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
		panic("LibraryRegistry called on GlobalEnvironmentFrame without TopLevelEnvironment")
	}
	return p.topLevel.LibraryRegistry()
}

// SetLibraryRegistry sets the library registry for R7RS library loading.
// The registry should be a *machine.LibraryRegistry.
// Delegates to TopLevelEnvironment.
// Panics if topLevel is nil.
func (p *GlobalEnvironmentFrame) SetLibraryRegistry(registry any) {
	if p.topLevel == nil {
		panic("SetLibraryRegistry called on GlobalEnvironmentFrame without TopLevelEnvironment")
	}
	p.topLevel.SetLibraryRegistry(registry)
}
