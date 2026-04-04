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
	"maps"

	"github.com/aalpar/wile/values"
)

// LocalEnvironmentFrame stores local variable bindings for a single scope.
// It maps symbols to binding indices for efficient lookup. Local environments
// are created for lambda parameters and let-bound variables.
// Note: LocalEnvironmentFrame has no hierarchy of its own; the hierarchy is
// managed by EnvironmentFrame via its parent field.
type LocalEnvironmentFrame struct {
	keys       map[values.Symbol][]int
	bindings   []Binding
	keysShared bool // true when keys map is shared with another frame (CoW)
}

// NewLocalEnvironment creates a new local environment frame with pre-allocated
// slots for the given parameter count. Each slot is initialized with a void
// binding of unknown type.
func NewLocalEnvironment(pcnt int) *LocalEnvironmentFrame {
	q := &LocalEnvironmentFrame{
		keys:     make(map[values.Symbol][]int, pcnt),
		bindings: make([]Binding, pcnt),
	}
	for i := range pcnt {
		q.bindings[i] = Binding{value: values.Void, bindingType: BindingTypeUnknown}
	}
	return q
}

// Bindings returns the slice of bindings in this local environment.
func (p *LocalEnvironmentFrame) Bindings() []Binding {
	return p.bindings
}

// SetBindings replaces the bindings slice in this local environment.
func (p *LocalEnvironmentFrame) SetBindings(v []Binding) {
	p.bindings = v
}

// Keys returns a copy of the symbol-to-index mapping for this local environment.
// Each key maps to a slice of slot indices (common case: one element). Multiple
// slots per key occur when hygienic expansion creates same-name bindings with
// different scope sets in the same frame.
// The returned map is safe to mutate without affecting internal state.
func (p *LocalEnvironmentFrame) Keys() map[values.Symbol][]int {
	result := make(map[values.Symbol][]int, len(p.keys))
	for k, v := range p.keys {
		cp := make([]int, len(v))
		copy(cp, v)
		result[k] = cp
	}
	return result
}

// EnsureLocalBinding returns the local binding for the given key, creating it if
// it does not already exist. Returns (index, true) if a new binding was created,
// or (index, false) if the binding already existed.
//
// If the keys map is shared (from Copy), it is cloned before mutation (CoW).
// In practice, EnsureLocalBinding is only called during compilation, never at
// runtime, so the CoW path is a safety net rather than a hot path.
func (p *LocalEnvironmentFrame) EnsureLocalBinding(key *values.Symbol, bt BindingType) (*LocalIndex, bool) {
	if p.keysShared {
		p.keys = maps.Clone(p.keys)
		p.keysShared = false
	}
	slots := p.keys[*key]
	if len(slots) > 0 {
		return &LocalIndex{slots[0], 0}, false
	}
	i := len(p.bindings)
	p.keys[*key] = []int{i}
	p.bindings = append(p.bindings, Binding{value: values.Void, bindingType: bt})
	return &LocalIndex{i, 0}, true
}

// GetLocalIndex returns the LocalIndex for the given symbol in this local environment.
// Returns the first slot for the key, or nil if not bound.
func (p *LocalEnvironmentFrame) GetLocalIndex(key *values.Symbol) *LocalIndex {
	slots := p.keys[*key]
	if len(slots) == 0 {
		return nil
	}
	return &LocalIndex{slots[0], 0}
}

// GetLocalBinding returns the binding at the given LocalIndex.
func (p *LocalEnvironmentFrame) GetLocalBinding(li *LocalIndex) *Binding {
	return &p.bindings[li[0]]
}

// SetLocalValue sets the value of the binding at the given LocalIndex.
func (p *LocalEnvironmentFrame) SetLocalValue(li *LocalIndex, v values.Value) error {
	p.bindings[li[0]].value = v
	return nil
}

// SchemeString returns a string representation of this local environment.
func (p *LocalEnvironmentFrame) SchemeString() string {
	return "#<Local-environment>"
}

// IsVoid returns true if this local environment frame is nil.
func (p *LocalEnvironmentFrame) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if this local environment is equal to the given value.
// Two local environments are equal if they have the same bindings.
func (p *LocalEnvironmentFrame) EqualTo(o values.Value) bool {
	v, ok := o.(*LocalEnvironmentFrame)
	if !ok {
		return false
	}
	if p == nil || v == nil {
		return p == v
	}
	if len(p.bindings) != len(v.bindings) {
		return false
	}
	for i := range p.bindings {
		if !p.bindings[i].EqualTo(&v.bindings[i]) {
			return false
		}
	}
	return true
}

// Copy creates a copy of this local environment frame. The keys map is shared
// by reference (copy-on-write) since it is only mutated during compilation.
// Copy-on-write (CoW): shares the keys map between original and copy until
// a mutation forces a clone. Most copies are never mutated, so the clone
// cost is avoided entirely. See BIBLIOGRAPHY.md "Copy-on-Write".
// Bindings are allocated as a single contiguous block to reduce GC pressure,
// and each binding's scopes slice is shared (immutable at runtime).
func (p *LocalEnvironmentFrame) Copy() values.Value {
	if p == nil {
		return (*LocalEnvironmentFrame)(nil)
	}
	bindings := make([]Binding, len(p.bindings))
	copy(bindings, p.bindings)
	return &LocalEnvironmentFrame{
		keys:       p.keys,
		keysShared: true,
		bindings:   bindings,
	}
}

// CopyForApply creates a lightweight copy optimized for the Apply hot path.
// The keys map is shared between frames and must be treated as immutable at
// runtime; callers must not mutate the shared keys map or any map returned
// by Keys(). Bindings are batch-allocated (contiguous array) for cache locality
// and reduced GC pressure. Scopes and source are shared (immutable at runtime);
// only binding values are independent between original and copy.
func (p *LocalEnvironmentFrame) CopyForApply() *LocalEnvironmentFrame {
	if p == nil {
		return nil
	}
	q := &LocalEnvironmentFrame{
		keys:       p.keys,
		keysShared: true,
	}
	p.keysShared = true

	q.bindings = make([]Binding, len(p.bindings))
	copy(q.bindings, p.bindings)
	return q
}

// copyInto copies bindings into an existing destination frame.
// The keys map is shared (copy-on-write). Used by EnvironmentFrame.Copy().
func (p *LocalEnvironmentFrame) copyInto(dst *LocalEnvironmentFrame) {
	dst.keys = p.keys
	dst.keysShared = true
	dst.bindings = make([]Binding, len(p.bindings))
	copy(dst.bindings, p.bindings)
}

// copyForApplyInto copies bindings into an existing destination frame,
// marking both source and destination as sharing keys (CoW).
// Used by EnvironmentFrame.NewApplyFrame() and InitApplyFrame().
//
// When dst already has a bindings backing array with sufficient capacity
// (the common case for pooled frames after warmup), the slice is resliced
// instead of allocated. This eliminates the per-call make([]Binding, n)
// that dominates allocation profiles in recursive workloads.
func (p *LocalEnvironmentFrame) copyForApplyInto(dst *LocalEnvironmentFrame) {
	dst.keys = p.keys
	dst.keysShared = true
	p.keysShared = true
	n := len(p.bindings)
	if cap(dst.bindings) >= n {
		dst.bindings = dst.bindings[:n]
	} else {
		dst.bindings = make([]Binding, n)
	}
	copy(dst.bindings, p.bindings)
}
