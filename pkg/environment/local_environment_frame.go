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
	keys     map[values.Symbol][]int
	bindings []Binding
	// copyCount is how many LEADING slots copyForApplyInto transfers when this
	// frame is a template's shape. It is not the parameter count: internal
	// defines extend the shape too, and they carry a binding type and a
	// *BindingMeta that an apply frame must see.
	//
	// The slots it excludes are exactly the anonymous ones AppendAnonymousSlot
	// mints for a merged `let`. Their shape entry is Binding{value: Void} and
	// nothing else — no name, no type, no meta — so copying one writes the same
	// bytes voidBinding already holds, and the destination's backing array is
	// void-filled at every point it can reach copyForApplyInto (see
	// makeVoidBindings).
	//
	// Both binder-creating methods raise it to the new length and
	// AppendAnonymousSlot does not, so every NAMED slot sits below it. That is
	// the whole invariant: the excluded region need not be contiguous with the
	// end of the merge, only free of named slots. A merged `let` that opens
	// before a body define is compiled therefore keeps its slots copied rather
	// than corrupting the define's, which is the safe direction.
	//
	// int32, not int, and sized by the struct rather than by the domain: it lands
	// in padding LocalEnvironmentFrame already had, so the field is free. An int
	// would take EnvironmentFrame from 80 to 88 bytes and across a size class,
	// costing every unpooled apply frame 16 bytes to save a 40-byte copy. A slot
	// index is already bounded far below this by EncodeLocalIndex's int16 pack.
	copyCount  int32
	keysShared bool // true when keys map is shared with another frame (CoW)
}

// voidBinding is the state every unwritten binding slot holds: #!void, no type,
// no cell, no metadata. It is what a fresh slot means, and — since it holds no
// heap pointer, values.Void being an empty struct — filling with it releases
// references exactly as zeroing does.
var voidBinding = Binding{value: values.Void, bindingType: BindingTypeUnknown}

// makeVoidBindings allocates n slots and fills the whole backing array,
// capacity included, with voidBinding.
//
// Capacity rather than length because the array outlives this call: a pooled
// frame reslices into its spare capacity on a later, wider apply, and
// copyForApplyInto deliberately does not write the slots past copyCount. A nil
// values.Value reaching one of those is not a stale value, it is a Go nil that
// OpLoadLocal would hand to the value register.
func makeVoidBindings(n int) []Binding {
	q := make([]Binding, n)
	fillVoid(q[:cap(q)])
	return q
}

// fillVoid resets every slot of bs to voidBinding.
func fillVoid(bs []Binding) {
	for i := range bs {
		bs[i] = voidBinding
	}
}

// NewLocalEnvironment creates a new local environment frame with pre-allocated
// slots for the given parameter count. Each slot is initialized with a void
// binding of unknown type.
func NewLocalEnvironment(pcnt int) *LocalEnvironmentFrame {
	return &LocalEnvironmentFrame{
		keys:      make(map[values.Symbol][]int, pcnt),
		bindings:  makeVoidBindings(pcnt),
		copyCount: int32(pcnt),
	}
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
	p.copyCount = int32(len(p.bindings))
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
	p.copyCount = int32(len(p.bindings))
	return NewLocalIndex(i, 0), true
}

// AppendAnonymousSlot appends one binding slot that no symbol maps to, and
// returns its index.
//
// It is the allocator for a compile-time frame whose slots are addressed
// positionally rather than by name: `let`-slot merging (compilation's
// shapeSlotFor) gives a merged `let` binder a slot in the enclosing lambda's
// frame while the NAME stays in the `let`'s own compile-time frame, where
// scope-set resolution can still shadow it. Adding the name here instead would
// alias a same-named parameter — MaybeCreateLocalBinding dedups on scope-set
// equality, and duplicate-parameter detection is that same call.
//
// The slot is initialized to Void rather than the zero Binding, so a read that
// precedes any store sees `#!void` the way a freshly pushed frame's slot does.
// A letrec forward reference and OpBoxSlot both perform that read, so it is a
// requirement rather than tidiness.
//
// copyCount is deliberately NOT raised: voidBinding is bit-for-bit what this
// appends, so an apply frame reaches the same state by not copying the slot at
// all. That exemption is the whole point of merging a `let` into the shape —
// the slots exist for free at compile time and cost nothing per call.
func (p *LocalEnvironmentFrame) AppendAnonymousSlot() int {
	i := len(p.bindings)
	p.bindings = append(p.bindings, voidBinding)
	return i
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
// Only p.copyCount slots are transferred, not all n: the tail is the merged
// `let` slots, which are voidBinding in the shape and voidBinding already in
// dst's backing array. The reslice past them is sound only because EVERY array
// a dst can present is void-filled to its capacity — makeVoidBindings on the
// allocating paths, ResetForPool on the recycling one. Reslicing onto an array
// that was merely zeroed would hand a letrec forward reference a nil
// values.Value instead of #!void.
//
// Bindings copy as whole structs, so the destination shares each source
// binding's *BindingMeta pointer. A local binding's meta is mutated IN PLACE
// (UpdateMeta) rather than published copy-on-write the way a global's is, so
// that sharing is safe only because every local UpdateMeta site is compile-time
// — MaybeCreateLocalBinding's Source backfill, and the stamps in compile_let /
// expander_body / letrec_semantics / compile_define — and none of them runs on
// an apply frame. A compile-time writer that reached one would stamp the
// closure's own compiled frame through the shared pointer.
//
// Returns the number of slots transferred, for the VM's BindingsCopied counter.
func (p *LocalEnvironmentFrame) copyForApplyInto(dst *LocalEnvironmentFrame) int {
	dst.keys = p.keys
	dst.keysShared = true
	n := len(p.bindings)
	if cap(dst.bindings) >= n {
		dst.bindings = dst.bindings[:n]
	} else {
		dst.bindings = makeVoidBindings(n)
	}
	copy(dst.bindings, p.bindings[:p.copyCount])
	// An apply frame is a runtime frame: every one of its slots holds a live
	// value the instant the body starts storing into the merged region, so it
	// has no exempt tail of its own. Saying so keeps the field's meaning true of
	// every frame rather than only of shapes.
	dst.copyCount = int32(n)
	return int(p.copyCount)
}

// CopyCount returns how many of this frame's slots copyForApplyInto transfers.
// See the field comment: the difference from len(Bindings()) is the merged
// `let` slots, which cost nothing per call.
func (p *LocalEnvironmentFrame) CopyCount() int {
	return int(p.copyCount)
}
