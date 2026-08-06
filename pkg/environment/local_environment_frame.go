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
	"slices"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
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
//
// Note: With multi-slot keys, this returns slots[0] without scope discrimination.
// It is only valid for single-slot keys (fresh environments for lambda params,
// syntax-case pattern variables). Do not use on frames where MaybeCreateLocalBinding
// has created scope-distinct slots for the same key.
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

// MaybeCreateLocalBinding creates a local binding with scope-aware deduplication.
// A slot is reused only by a binder carrying the SAME scope set; any other scope
// set is a different variable and gets its own slot. Nil scopes means "match any".
//
// Creation compares with scopeSetsEqual, not ScopesCompatible, for the reason
// spelled out at scopeSetsEqual (global_environment_frame.go): compatibility
// treats an empty binding scope set as matching anything, so a macro-introduced
// binder (scopes {m}) would reuse a scope-less binding of the same name instead
// of getting a slot of its own. Compatibility is the right predicate for LOOKUP,
// where a pre-hygiene binding is legitimately visible to every reference; it is
// the wrong one for deciding identity. This mirrors the global creation path
// rather than the local lookup path beside it.
//
// A reused slot backfills Source but never Scopes. Under exact equality the slot
// already carries the scope set the caller asked for (or the caller passed nil
// and asked for nothing), so there is nothing to fill in; a Scopes write here
// could only overwrite an identity, which is the clobber the predicate above
// exists to prevent. Source is independent metadata and may legitimately be
// absent on an existing slot.
//
// If the keys map is shared (from Copy), it is cloned before mutation (CoW).
// Cloning the map is not enough: the clone's slot slices alias the original's
// backing arrays, so slices.Clip forces the append to copy rather than extend
// one in place and publish a slot to the other frame.
func (p *LocalEnvironmentFrame) MaybeCreateLocalBinding(
	key *values.Symbol, bt BindingType,
	scopes []*syntax.Scope, source *syntax.SourceContext,
) (*LocalIndex, bool) {
	slots := p.keys[*key]
	matchAny := scopes == nil
	for _, i := range slots {
		binding := &p.bindings[i]
		if matchAny || scopeSetsEqual(binding.Scopes(), scopes) {
			if binding.Source() == nil && source != nil {
				binding.UpdateMeta(func(m *BindingMeta) bool {
					m.Source = source
					return true
				})
			}
			return NewLocalIndex(i, 0), false
		}
	}
	if p.keysShared {
		p.keys = maps.Clone(p.keys)
		p.keysShared = false
	}
	i := len(p.bindings)
	p.keys[*key] = append(slices.Clip(slots), i)
	b := Binding{value: values.Void, bindingType: bt}
	if scopes != nil || source != nil {
		b.meta = &BindingMeta{Scopes: scopes, Source: source}
	}
	p.bindings = append(p.bindings, b)
	return NewLocalIndex(i, 0), true
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

// copyForApplyInto copies bindings into an existing destination frame,
// aliasing the source's keys map into the destination (copy-on-write).
// Used by EnvironmentFrame.NewApplyFrame() and InitApplyFrame().
//
// Only the destination is marked keysShared. The source `p` is intentionally
// NOT marked: a frame's keys map is mutated only during compilation/expansion
// (EnsureLocalBinding / MaybeCreateLocalBinding, all called from
// internal/validate and machine/compilation), never on the runtime apply path.
// The apply source is a fully-compiled closure environment whose keys are
// immutable thereafter, so it can never reach the CoW guard — marking it is
// unnecessary. Eliminating the source-side write also removes a data race:
// the same closure applied from multiple SRFI-18 threads shares one source
// frame, and the former `p.keysShared = true` was a concurrent write to it
// (benign-but-racy; failed `go test -race`). The destination is a fresh /
// pooled per-call frame owned by one goroutine, so its write is race-free.
//
// When dst already has a bindings backing array with sufficient capacity
// (the common case for pooled frames after warmup), the slice is resliced
// instead of allocated. This eliminates the per-call make([]Binding, n)
// that dominates allocation profiles in recursive workloads.
//
// Bindings copy as whole structs, so the destination shares each source
// binding's *BindingMeta pointer. A local binding's meta is mutated IN PLACE
// (UpdateMeta) rather than published copy-on-write the way a global's is, so
// that sharing is safe only because every local UpdateMeta site is compile-time
// — MaybeCreateLocalBinding's Source backfill, and the stamps in compile_let /
// expander_body / letrec_semantics / compile_define — and none of them runs on
// an apply frame. A compile-time writer that reached one would stamp the
// closure's own compiled frame through the shared pointer.
func (p *LocalEnvironmentFrame) copyForApplyInto(dst *LocalEnvironmentFrame) {
	dst.keys = p.keys
	dst.keysShared = true
	n := len(p.bindings)
	if cap(dst.bindings) >= n {
		dst.bindings = dst.bindings[:n]
	} else {
		dst.bindings = make([]Binding, n)
	}
	copy(dst.bindings, p.bindings)
}
