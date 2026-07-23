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
	"slices"
	"sync"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
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
// Slot addresses the binding within Env.bindings directly. It is meaningful
// ONLY when Env is non-nil: the two are set together by the frame that resolved
// the lookup, and a nil Env means no frame has been chosen yet, so the zero Slot
// is never consulted. This pairing is what lets a resolved global load index the
// bindings slice instead of re-hashing the symbol at every execution.
//
// query is the hygiene key. For a deferred index (Env == nil) it is the
// reference's scope-set query, resolved against whatever environment is live
// when the instruction executes. For a PINNED index it is the query resolution
// matched on, kept so that re-resolution — which happens whenever the pinned
// slot no longer holds the binding, e.g. after a delete — stays inside the same
// hygiene boundary instead of falling back to bare name.
//
// A wildcard query (AllScopes) re-resolves by bare name; a specific or empty
// query re-resolves under its scope set even when that set is empty, or a stale
// pinned index would silently cross a hygiene boundary after a
// delete-then-recreate: DeleteBinding nils the slots and drops the name, so once
// anything re-creates it a wildcard fallback would land on whatever binding now
// holds the name — including one whose scope set the reference could never
// reach. This one ScopeSet subsumes what a nil Scopes slice plus a scopeKeyed
// bool once encoded: a nil slice could not distinguish "matched the empty set"
// from "no key at all", and those demand opposite re-resolution.
type GlobalIndex struct {
	Index *values.Symbol
	Env   *GlobalEnvironmentFrame
	Slot  int
	query syntax.ScopeSet
}

// NewGlobalIndex creates a new deferred GlobalIndex for the given symbol.
// Env is nil, so Slot is not meaningful; use newResolvedGlobalIndex when the
// owning frame and slot are known. Its query is the wildcard (AllScopes): a
// deferred bare-name index re-resolves by name.
func NewGlobalIndex(key *values.Symbol) *GlobalIndex {
	return &GlobalIndex{Index: key, query: syntax.AllScopes()}
}

// NewDeferredGlobalIndex creates a deferred GlobalIndex that carries the
// reference's scope set, so the execution-time parent-chain walk can resolve it
// hygienically rather than by bare name.
func NewDeferredGlobalIndex(key *values.Symbol, scopes []*syntax.Scope) *GlobalIndex {
	return &GlobalIndex{Index: key, query: syntax.ScopesOf(scopes)}
}

// newResolvedGlobalIndex creates a GlobalIndex pinned to the frame and slot that
// resolution landed on. Its query is the wildcard (AllScopes): a wildcard
// resolution re-resolves by bare name if its slot dies.
func newResolvedGlobalIndex(key *values.Symbol, env *GlobalEnvironmentFrame, slot int) *GlobalIndex {
	return &GlobalIndex{Index: key, Env: env, Slot: slot, query: syntax.AllScopes()}
}

// newScopeKeyedGlobalIndex is newResolvedGlobalIndex for a resolution that
// matched on a scope-set query, recording that query as the index's key. The
// scoped lookups compute it and would otherwise discard it, leaving the pinned
// index indistinguishable from a wildcard one the moment its slot dies. Callers
// reach it through GetGlobalIndexWithScopes, whose query is never the wildcard,
// so re-resolution stays inside the hygiene key even when the set is empty.
func newScopeKeyedGlobalIndex(key *values.Symbol, env *GlobalEnvironmentFrame, slot int, q syntax.ScopeSet) *GlobalIndex {
	return &GlobalIndex{Index: key, Env: env, Slot: slot, query: q}
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
//
// Env participates in the comparison, by pointer. It is not provenance metadata:
// a non-nil Env is the binding store the VM reads and writes directly, with no
// parent walk (machine_context.go, OpLoadGlobal/OpStoreGlobal via GetOwnGlobalBinding
// and SetOwnGlobalValue). Two frames are two distinct `bindings` slices, so two
// GlobalIndex pinned to different frames denote different variables even when
// their symbol keys agree.
//
// A nil Env is not "some frame we did not record" — it is a deferred lookup,
// resolved against whatever environment is live when the instruction executes.
// It is therefore never equal to a pinned index, even one whose frame today's
// walk would reach: the two are different operations, and a closure with a
// different env chain resolves them differently.
//
// Slot participates whenever Env does. Once a frame keys its bindings by scope
// set, one symbol can name several distinct bindings in the same frame, so
// (Index, Env) no longer identifies a variable — the slot is what separates a
// macro-introduced binder from a user-written one of the same name.
func (p *GlobalIndex) EqualTo(value values.Value) bool {
	if p == nil || value == nil {
		return p == nil && value == nil
	}
	v, ok := value.(*GlobalIndex)
	if !ok {
		return false
	}
	if v.Env != p.Env {
		return false
	}
	if v.Env != nil && v.Slot != p.Slot {
		return false
	}
	return v.Index.EqualTo(p.Index)
}

// GlobalEnvironmentFrame represents global bindings for a single phase.
//
// Design: GlobalEnvironmentFrame has no hierarchy of its own. The environment
// hierarchy is managed by EnvironmentFrame via its parent field. Each phase
// (runtime, expand, compile) has its own GlobalEnvironmentFrame.
//
// Note: Symbol and syntax interning are delegated to Namespace via the
// owning EnvironmentFrame, ensuring R7RS symbol identity works correctly
// across all phases. GlobalEnvironmentFrame itself does not hold a back
// reference to its Namespace; ownership flows through EnvironmentFrame.
//
// Thread safety: All access to keys and bindings is protected by mu.
// Fixes T2 from architectural review.
type GlobalEnvironmentFrame struct {
	// mu protects concurrent access to keys and bindings maps.
	// Use RLock for reads, Lock for writes and check-then-write patterns.
	mu sync.RWMutex
	// symbol to binding slot lookup map. A symbol maps to SEVERAL slots because
	// global bindings are keyed by scope set as well as by name (Flatt's sets of
	// scopes): a macro-introduced top-level binder and a user-written one share a
	// name but are different variables. Mirrors LocalEnvironmentFrame.keys.
	keys     map[values.Symbol][]int
	bindings []*Binding
}

// NewGlobalEnvironmentFrame creates a new global environment frame.
func NewGlobalEnvironmentFrame() *GlobalEnvironmentFrame {
	q := &GlobalEnvironmentFrame{
		bindings: []*Binding{},
		keys:     map[values.Symbol][]int{},
	}
	return q
}

// Copy creates a deep copy of the global environment frame.
// Bindings are batch-allocated (contiguous array) for cache locality
// and reduced GC pressure.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) Copy() *GlobalEnvironmentFrame {
	if p == nil {
		return nil
	}

	p.mu.RLock()
	defer p.mu.RUnlock()

	q := &GlobalEnvironmentFrame{}

	// Batch allocation: allocate all Bindings contiguously (1 allocation)
	// instead of N separate heap objects.
	allBindings := make([]Binding, len(p.bindings))
	q.bindings = make([]*Binding, len(p.bindings))
	for i, b := range p.bindings {
		// Each copied global binding gets its own atomicCell snapshotting the
		// source value (read via Value so it works whether or not the source
		// already uses a cell).
		// A global binding's meta lives in its cell (Meta reads it there), so
		// snapshot both value and meta into the copy's fresh cell. Sharing the
		// meta pointer is safe: it is immutable under copy-on-write (UpdateMeta).
		allBindings[i] = Binding{
			cell:        newAtomicCellWithMeta(b.Value(), b.Meta()),
			bindingType: b.bindingType,
		}
		q.bindings[i] = &allBindings[i]
	}

	if p.keys != nil {
		// Each slot list must be cloned, not shared: maps.Copy would alias the
		// slices, so a later append in either frame could be observed by the other
		// (or silently reallocate in only one).
		q.keys = make(map[values.Symbol][]int, len(p.keys))
		for k, slots := range p.keys {
			q.keys[k] = slices.Clone(slots)
		}
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

// Keys returns a copy of the symbol-to-slots mapping. A symbol may map to more
// than one slot: same-named bindings with different scope sets are different
// variables. Callers that only want names can range over the keys and ignore the
// slot lists.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) Keys() map[values.Symbol][]int {
	p.mu.RLock()
	defer p.mu.RUnlock()
	result := make(map[values.Symbol][]int, len(p.keys))
	for k, slots := range p.keys {
		result[k] = slices.Clone(slots)
	}
	return result
}

// AmbientScopes returns the ambient scope set: the empty, NON-NIL set that a
// reference written outside any macro expansion carries.
//
// It exists because nil and the empty set are opposite instructions at the read
// entry points. GetBinding, GetLocalIndex and GetGlobalIndex read nil as MATCH
// ANY — the name's first live slot whatever its hygiene — while an empty set
// means "compatible with no scopes", which only an ambient binding satisfies.
// Both spell `len(scopes) == 0`, so the distinction survives only where a site
// tests nil-ness, and a caller that means "ambient" and writes nil silently gets
// a wildcard. Every reflective read of a bare symbol wants this, not nil: a
// values.Symbol carries no scope set, so when several hygiene-distinct bindings
// share a name a wildcard resolves by slot order — an expansion-order artifact,
// not an answer to the caller's question.
func AmbientScopes() []*syntax.Scope {
	return []*syntax.Scope{}
}

// AmbientKeys returns the names holding a live binding under the ambient (empty)
// scope set: the names a reference written outside any macro expansion resolves.
//
// Keys reports every name in the frame, including binders a macro template
// introduced. Those are different variables that happen to share a name, and no
// source-written reference in this frame can reach them, so a listing built from
// Keys disagrees with every scoped read — enumerate-then-dereference fails on
// exactly those names. Resolution here goes through bestSlotLocked, the same
// call a single read makes, so the listing cannot drift from what the read
// finds.
//
// Order is unspecified: the result is built by ranging p.keys. Callers needing
// determinism must sort. (BoundSymbolNames, the only consumer, documents the
// same.)
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) AmbientKeys() []values.Symbol {
	p.mu.RLock()
	defer p.mu.RUnlock()

	q := make([]values.Symbol, 0, len(p.keys))
	for k := range p.keys {
		_, ok := p.bestSlotLocked(k, syntax.EmptyScopes())
		if !ok {
			continue
		}
		q = append(q, k)
	}
	return q
}

// scopeSetsEqual reports whether two scope sets are equal, by mutual subset.
//
// Binding CREATION compares scope sets with this, not with ScopesCompatible.
// Compatibility treats an empty binding scope set as matching anything, so a
// macro-introduced binder (scopes {m}) would reuse — and silently clobber — a
// user-written binding of the same name (scopes {}). Redefining one variable is
// precisely the equal-scope-set case; anything else is a different variable.
func scopeSetsEqual(a, b []*syntax.Scope) bool {
	if len(a) != len(b) {
		return false
	}
	return syntax.ScopesMatch(a, b) && syntax.ScopesMatch(b, a)
}

// bestSlotLocked returns the slot whose binding best matches the given
// scope-set query, per Flatt's maximal resolution: a candidate's scope set must
// be a subset of the reference's, and among the candidates the largest set wins.
// A wildcard query (q.IsAll) takes the first slot regardless of scopes, for
// introspection callers that mean "any binding of this name" rather than "the
// empty scope set".
//
// Caller MUST hold at least a read lock on p.mu.
func (p *GlobalEnvironmentFrame) bestSlotLocked(key values.Symbol, q syntax.ScopeSet) (int, bool) {
	slots := p.keys[key]
	if len(slots) == 0 {
		return 0, false
	}
	if q.IsAll() {
		// Skip nil'd slots for the same reason the scoped branch does: a live key
		// may point at a slot DeleteBinding emptied. Returning slots[0] blindly
		// would hand back a nil binding for callers to dereference.
		for _, i := range slots {
			if i < len(p.bindings) && p.bindings[i] != nil {
				return i, true
			}
		}
		return 0, false
	}
	scopes := q.Scopes()
	var best bestOf[int]
	for _, i := range slots {
		if i >= len(p.bindings) || p.bindings[i] == nil {
			continue
		}
		bindingScopes := p.bindings[i].Scopes()
		if !syntax.ScopesCompatible(bindingScopes, scopes) {
			continue
		}
		record, done := best.shouldRecord(len(bindingScopes), len(scopes))
		if record {
			best.record(i, len(bindingScopes))
		}
		if done {
			break
		}
	}
	return best.Result()
}

// CreateGlobalBinding creates a new global binding with the given key and type.
// Returns the GlobalIndex and whether a new binding was created (false if the
// binding already existed).
// Thread-safe: uses full Lock to prevent TOCTOU races.
// Reuse requires EXACT scope-set equality — see scopeSetsEqual for why
// compatibility would be a hygiene hole here.
func (p *GlobalEnvironmentFrame) CreateGlobalBinding(key *values.Symbol, bt BindingType, scopes []*syntax.Scope) (*GlobalIndex, bool) {
	r := p

	// Use full Lock (not RLock) for check-then-write pattern to prevent TOCTOU
	p.mu.Lock()
	defer p.mu.Unlock()

	for _, i := range r.keys[*key] {
		if i >= len(p.bindings) || p.bindings[i] == nil {
			continue
		}
		if scopeSetsEqual(p.bindings[i].Scopes(), scopes) {
			q := NewGlobalIndex(key)
			return q, false
		}
	}
	i := len(p.bindings)
	p.keys[*key] = append(p.keys[*key], i)
	// append the new binding at index i. Global bindings carry an atomicCell so
	// they can be read lock-free from other threads (see binding.go atomicCell).
	p.bindings = append(p.bindings, newGlobalBinding(values.Void, bt, scopes))
	q := NewGlobalIndex(key)
	return q, true
}

// NOTE (2026-07-19): returning a scope-keyed index pinned to (p, i) here — which
// is task C2b, and which both branches have the information to do — was tried and
// REVERTED. It is correct in isolation and it breaks the tree, because today's
// macro path depends on two errors cancelling: define-syntax writes the
// transformer through this deferred index (wildcard, landing on the name's FIRST
// slot rather than the one just created) and lookupMacroBinding reads it back
// wildcard, finding the same wrong slot. Fixing only the write makes the read
// miss: `(chibi diff)` fails to load with `no such binding "let*-to-let" with
// compatible scopes`.
//
// So C2b cannot land without the macro read path moving in the same commit —
// the reverse of the plan's sequencing, which lists C2b as a prerequisite OF the
// read-path work. Do not re-attempt it standalone.

// GetGlobalIndex returns the GlobalIndex for the given symbol.
// Returns nil if the symbol is not bound in this global environment.
// Thread-safe: uses RLock for read-only access.
// This is the WILDCARD form: it matches any binding of the name regardless of
// scopes, which is what introspection and REPL completion mean. Compiler callers
// must use GetGlobalIndexWithScopes so a bare reference cannot reach a
// macro-introduced binder.
func (p *GlobalEnvironmentFrame) GetGlobalIndex(key *values.Symbol) *GlobalIndex {
	ge := p

	p.mu.RLock()
	_, ok := ge.bestSlotLocked(*key, syntax.AllScopes())
	p.mu.RUnlock()

	if !ok {
		return nil
	}
	q := NewGlobalIndex(key)
	return q
}

// GetGlobalIndexWithScopes returns the GlobalIndex for the binding of key whose
// scope set maximally matches scopes. A nil scopes slice means the EMPTY scope
// set, not "any scope set" — that distinction is the whole point of the split
// from GetGlobalIndex, since a reference written outside any macro expansion must
// not resolve to a binder introduced inside one.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) GetGlobalIndexWithScopes(key *values.Symbol, q syntax.ScopeSet) *GlobalIndex {
	p.mu.RLock()
	i, ok := p.bestSlotLocked(*key, q)
	p.mu.RUnlock()

	if !ok {
		return nil
	}
	return newScopeKeyedGlobalIndex(key, p, i, q)
}

// GetOwnGlobalBinding returns the binding for the given GlobalIndex from this frame only.
// Unlike EnvironmentFrame.GetGlobalBinding, this does NOT traverse the parent chain.
// Returns nil if the binding does not exist in this frame.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) GetOwnGlobalBinding(gi *GlobalIndex) *Binding {
	ge := p
	key := gi.Index

	p.mu.RLock()
	defer p.mu.RUnlock()

	i, ok := ge.pinnedSlotLocked(gi)
	if !ok {
		i, ok = ge.bestSlotLocked(*key, gi.query)
	}
	if !ok {
		return nil
	}
	return ge.bindings[i]
}

// pinnedSlotLocked resolves a GlobalIndex through its pinned (Env, Slot) pair,
// which addresses the binding directly with no re-hash of the symbol.
//
// The emptiness check is load-bearing, not defensive. DeleteBinding nils a slot
// but leaves it in range, so a bounds check alone would hand back a nil binding
// where the name-keyed lookup this replaced would have missed and reported "no
// such binding". Falling through to bestSlotLocked on a nil slot also restores
// the self-healing the name lookup gave for free: an index pinned before a
// delete-then-redefine finds the re-created binding instead of addressing the
// emptied slot forever.
//
// Caller MUST hold at least a read lock on p.mu.
func (p *GlobalEnvironmentFrame) pinnedSlotLocked(gi *GlobalIndex) (int, bool) {
	if gi.Env != p || gi.Slot < 0 || gi.Slot >= len(p.bindings) {
		return 0, false
	}
	if p.bindings[gi.Slot] == nil {
		return 0, false
	}
	return gi.Slot, true
}

// SetOwnGlobalValue sets the value of the binding for the given GlobalIndex.
// Returns an error if the binding does not exist.
// Thread-safe: uses full Lock for write access.
func (p *GlobalEnvironmentFrame) SetOwnGlobalValue(gi *GlobalIndex, v values.Value) error {
	ge := p

	p.mu.Lock()
	i, ok := ge.pinnedSlotLocked(gi)
	if !ok {
		i, ok = ge.bestSlotLocked(*gi.Index, gi.query)
	}
	if !ok {
		p.mu.Unlock()
		return werr.WrapForeignErrorf(werr.ErrNoSuchBinding,
			"SetOwnGlobalValue: no such global binding %q", gi.Index.Key)
	}
	// Publish atomically through the binding's cell so the lock-free
	// cachedBindings reader (Binding.Value with no frame mutex) never tears the
	// two-word interface. The frame Lock still serializes writers.
	ge.bindings[i].SetValue(v)
	p.mu.Unlock()

	return nil
}

// DeleteBinding removes the global binding for sym that resolves under the
// given scope set. Returns true if one was found and removed.
//
// Resolution goes through bestSlotLocked with matchAny FALSE — the literal call
// AmbientKeys and GetGlobalIndexWithScopes make — so delete cannot drift from
// the read surface. It removes exactly the binding a scoped read would have
// returned, and deleting a name owned only by a macro-introduced binder is a
// no-op rather than the destruction of a binding the caller could not read.
//
// A nil scopes argument means NONE — the empty scope set, same as
// AmbientScopes() — and never MATCH ANY. Nil is indistinguishable from an
// uninitialized value, so resolving it permissively fails open: a caller that
// merely forgot to thread its scopes would delete across a hygiene boundary
// with nothing in the signature to flag it. Delete therefore has no wildcard
// mode at all; "remove the name and every hygiene-distinct binding under it" is
// a legitimate but different operation, and nothing asks for it.
//
// The read entry points (GetBinding, GetLocalIndex, GetGlobalIndex) still read
// nil as MATCH ANY, which is what AmbientScopes exists to work around. This
// function does not follow them. See issue #805.
//
// Note: the binding slot in p.bindings is not compacted — index-based
// references from compiled code would be stale. This is only safe for
// top-level REPL/eval bindings, not for bindings referenced by compiled
// bytecode.
//
// Thread-safe: uses full Lock for write access.
func (p *GlobalEnvironmentFrame) DeleteBinding(sym *values.Symbol, scopes []*syntax.Scope) bool {
	p.mu.Lock()
	defer p.mu.Unlock()

	i, ok := p.bestSlotLocked(*sym, syntax.ScopesOf(scopes))
	if !ok {
		return false
	}
	// Nil out the slot so a re-resolving GlobalIndex reference (OpLoadGlobal /
	// OpPushGlobal) sees nil, caught by resolveGlobal, instead of the old value.
	// NOTE: this does NOT reach compiled code that captured the *Binding pointer
	// itself at compile time (OpLoadCachedBinding / OpPushCachedBinding /
	// OpCallCachedBinding read cachedBindings[i].Value() directly, bypassing the
	// slot), so a closure over an undefined name can still read its last value.
	// Making cached reads observe deletion needs a per-read check in those hot
	// opcodes; see TODO.md "namespace-undefine! does not stop compiled code".
	p.bindings[i] = nil
	// Prune the dead index rather than leaving it in place: Keys' consumers
	// treat slots[0] as the name's representative, so a dead slot 0 would make
	// them drop a name whose remaining slots are still live.
	slots := p.keys[*sym]
	for j, s := range slots {
		if s != i {
			continue
		}
		p.keys[*sym] = append(slots[:j], slots[j+1:]...)
		break
	}
	// Drop the name once it owns no slots, so Keys stops reporting it at all.
	if len(p.keys[*sym]) == 0 {
		delete(p.keys, *sym)
	}
	return true
}
