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

// PhaseKey is a Phase plus an explicit ANY wildcard. Phase is a full int8
// domain (GetOrCreate mints any value; the tower climbs to 127), so there is no
// free in-band value to steal, and per [nil means NONE] the wildcard is a named
// value, never a sentinel.
type PhaseKey struct {
	Phase Phase
	Any   bool
}

// ExactPhase returns the key for an exact phase.
func ExactPhase(phase Phase) PhaseKey {
	return PhaseKey{Phase: phase}
}

// AnyPhase returns the ambient wildcard key: visible from every phase. Only
// sealed entries may carry it — see CreateGlobalBindingAt.
func AnyPhase() PhaseKey {
	return PhaseKey{Any: true}
}

// slotRef locates one binding of a name and carries its resolution coordinates
// (design §4.1). slot indexes bindings, as the bare int did; phase and sealed
// are resolution coordinates — nothing after resolution needs them, which is
// why they live here and not on BindingMeta (design Q1).
type slotRef struct {
	slot   int
	phase  PhaseKey
	sealed bool
}

// GlobalEnvironmentFrame is one OWNER's whole binding store: every global
// binding a namespace or library env holds, at every phase and every
// registration rank, in one scope-keyed slot table.
//
// Design: it has no hierarchy of its own, and after the store fold there is no
// hierarchy above it either — an owner's phase frames are VIEWS over this one
// instance, distinguished by the (phase, rank) coordinates their writes stamp
// and the phase their reads probe at. What used to be a parent walk across
// (layer × phase) frames is resolveRankedLocked's tier order.
//
// Note: syntax interning is delegated to Namespace via the owning
// EnvironmentFrame. (Symbols are not interned; eq? on symbols compares the
// .Key string.) GlobalEnvironmentFrame itself does not hold a back
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
	// scopes) AND by resolution coordinates: a macro-introduced top-level binder
	// and a user-written one share a name but are different variables, and so do
	// a sealed primitive and the user define that shadows it.
	keys     map[values.Symbol][]slotRef
	bindings []*Binding
}

// NewGlobalEnvironmentFrame creates a new, empty owner store.
func NewGlobalEnvironmentFrame() *GlobalEnvironmentFrame {
	q := &GlobalEnvironmentFrame{
		bindings: []*Binding{},
		keys:     map[values.Symbol][]slotRef{},
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
		q.keys = make(map[values.Symbol][]slotRef, len(p.keys))
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
		ss := make([]int, len(slots))
		for i, s := range slots {
			ss[i] = s.slot
		}
		result[k] = ss
	}
	return result
}

// AmbientScopes returns the ambient scope set: the empty, NON-NIL set that a
// reference written outside any macro expansion carries.
//
// The read entry points no longer confuse the two: GetBinding and GetLocalIndex
// take a syntax.ScopeSet, which separates wildcard (ScopeSet.IsAll) from empty
// structurally, and syntax.ScopesOf(nil) is the empty set rather than a
// wildcard. (EnvironmentFrame.GetGlobalIndex takes no scope argument at all and
// is unconditionally a wildcard.) Binding CREATION is the one surviving
// nil-as-wildcard path (MaybeCreateLocalBinding dedups on `scopes == nil`), so
// a creation caller that means "ambient" must pass this set rather than nil.
//
// Every reflective read of a bare symbol wants this, not a wildcard: a
// values.Symbol carries no scope set, so when several hygiene-distinct bindings
// share a name a wildcard resolves by slot order — an expansion-order artifact,
// not an answer to the caller's question.
func AmbientScopes() []*syntax.Scope {
	return []*syntax.Scope{}
}

// AmbientKeysAt returns the names holding a live binding under the ambient
// (empty) scope set AT phase: the names a reference written outside any macro
// expansion, in phase-N code, resolves.
//
// Enumeration goes through the same ranked probe a single read makes, so the
// listing cannot drift from what the read finds. A raw range over p.keys would
// report every name in the store, including binders a macro template introduced
// (different variables that happen to share a name, reachable by no
// source-written reference) and entries at phases the caller cannot see — and
// enumerate-then-dereference then fails on exactly those names.
//
// Order is unspecified: the result is built by ranging p.keys. Callers needing
// determinism must sort. (BoundSymbolNames, the only consumer, documents the
// same.)
//
// Cost: O(names × slots-per-name) — a full ranked probe (resolveRankedLocked)
// per name — where the pre-fold AmbientKeys this generalizes ran the cheaper
// bestSlotLocked per name (no tier walk, since a frame's own store held only
// one layer). Same asymptotic shape, larger constant per name. The only
// consumer, BoundSymbolNames, is a REPL-completion path, not a hot one; not
// restructured here.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) AmbientKeysAt(phase Phase) []values.Symbol {
	p.mu.RLock()
	defer p.mu.RUnlock()

	q := make([]values.Symbol, 0, len(p.keys))
	for k := range p.keys {
		_, ok := p.resolveRankedLocked(k, syntax.EmptyScopes(), phase)
		if !ok {
			continue
		}
		q = append(q, k)
	}
	return q
}

// NamedSlot pairs a name with one live binding of it. A name can own several
// slots (hygiene-distinct binders, or the same name at different coordinates),
// so an enumeration that must not silently drop one yields pairs rather than a
// map.
type NamedSlot struct {
	Name    values.Symbol
	Binding *Binding
}

// LiveSlots snapshots every live slot in the store: any phase, any rank. It is
// the "every binding this owner holds anywhere" enumeration that the doc/apropos
// walk wants, and it replaces the old union over every phase frame plus every
// sealed frame — which, now that all of those are views over one store, would
// range the same map once per view.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) LiveSlots() []NamedSlot {
	return p.slotsFiltered(false)
}

// SealedSlots snapshots every live SEALED-tier slot in the store, at any phase.
// This is the rank-filtered form of LiveSlots: the startup set a registry apply
// and the bootstrap load wrote, as distinct from anything user code has defined
// since.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) SealedSlots() []NamedSlot {
	return p.slotsFiltered(true)
}

// slotsFiltered is the shared body of LiveSlots and SealedSlots.
func (p *GlobalEnvironmentFrame) slotsFiltered(sealedOnly bool) []NamedSlot {
	p.mu.RLock()
	defer p.mu.RUnlock()

	q := make([]NamedSlot, 0, len(p.keys))
	for k, slots := range p.keys {
		for _, s := range slots {
			if s.slot >= len(p.bindings) || p.bindings[s.slot] == nil {
				continue
			}
			if sealedOnly && !s.sealed {
				continue
			}
			q = append(q, NamedSlot{Name: k, Binding: p.bindings[s.slot]})
		}
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
// rejectSealed skips every sealed-coordinate slot. SetOwnGlobalValue's
// stale-pin self-heal is the one caller that passes true: DeleteBindingAt never
// removes a sealed slot (its only caller, DeleteOwnGlobal, derives coordinates
// from the writing view, and namespace-undefine! writes through the mutable
// runtime view), so a pin can never be stale-BY-DELETE for a sealed slot —
// refusing the sealed tier here closes the one path left by which a
// name-resolved WRITE could otherwise reach one, without forgoing any
// legitimate re-heal. Every other caller passes false: GetOwnGlobalBinding's
// read-side fallback wants the opposite polarity — revealing the sealed
// primitive a deleted shadow uncovers is the point, not hiding it — and
// GetGlobalIndex/GetGlobalIndexWithScopes are ordinary reads with no coordinate
// question to ask.
//
// Caller MUST hold at least a read lock on p.mu.
//
// It PANICS with a wrapped werr.ErrAmbiguousBinding on an incomparable
// equal-cardinality tie. SetOwnGlobalValue, the one caller that still unlocks
// without defer, leaks p.mu on that path (resolveGlobal's own probe moved to a
// deferred release; this note no longer applies there).
func (p *GlobalEnvironmentFrame) bestSlotLocked(key values.Symbol, q syntax.ScopeSet, rejectSealed bool) (int, bool) {
	slots := p.keys[key]
	if len(slots) == 0 {
		return 0, false
	}
	if q.IsAll() {
		// Skip nil'd slots for the same reason the scoped branch does: a live key
		// may point at a slot DeleteBinding emptied. Returning slots[0] blindly
		// would hand back a nil binding for callers to dereference.
		for _, s := range slots {
			if rejectSealed && s.sealed {
				continue
			}
			if s.slot < len(p.bindings) && p.bindings[s.slot] != nil {
				return s.slot, true
			}
		}
		return 0, false
	}
	scopes := q.Scopes()
	var best scopedBestOf[int]
	for _, s := range slots {
		if rejectSealed && s.sealed {
			continue
		}
		if s.slot >= len(p.bindings) || p.bindings[s.slot] == nil {
			continue
		}
		bindingScopes := p.bindings[s.slot].Scopes()
		if !syntax.ScopesCompatible(bindingScopes, scopes) {
			continue
		}
		record, done := best.shouldRecord(bindingScopes, len(scopes))
		if record {
			best.record(s.slot, bindingScopes)
		}
		if done {
			break
		}
	}
	if best.Ambiguous() {
		panic(werr.WrapForeignErrorf(werr.ErrAmbiguousBinding,
			"bestSlotLocked: identifier %q resolves ambiguously among incomparable hygienic scope sets",
			key.Key))
	}
	return best.Result()
}

// resolveRankedLocked is the flat model's one resolution rule (design §4.3):
// a slot is a candidate iff its scopes are compatible AND its coordinates admit
// the query phase — tier T1 (exact phase, mutable), T2 (exact phase, sealed),
// T3 (ANY, sealed); any OTHER exact phase is not a candidate at all, which is
// phase hermeticity as key disjointness (P5). The highest non-empty tier wins;
// maximal scope cardinality ranks within it (rank-major, cardinality-minor —
// the ordering the frame walk this replaces already had: first-frame-wins
// across layers, maximal-cardinality within one).
//
// A wildcard query (q.IsAll) takes the first live candidate slot in tier order,
// matching the old walk's layer-major first-live behavior.
//
// Caller MUST hold at least a read lock on p.mu, and MUST release it via defer
// rather than a bare RUnlock: this function can panic mid-hold, on an
// incomparable equal-cardinality tie IN THE WINNING TIER, wrapped as
// werr.ErrAmbiguousBinding (P8; a tie in a losing tier is dead and must not
// panic).
//
// There is no perfect-match early exit — a perfect match in one tier says
// nothing about a higher tier later in the slot list. Dropping it costs
// nothing WITHIN a tier either: two slots sharing one tier necessarily carry
// distinct scope sets, because CreateGlobalBindingAt refuses a second slot
// with an equal scope set at identical coordinates.
func (p *GlobalEnvironmentFrame) resolveRankedLocked(key values.Symbol, q syntax.ScopeSet, phase Phase) (int, bool) {
	slot, _, ok := p.probeRankedLocked(key, q, phase, tierExactMutable)
	return slot, ok
}

// The probe's tiers, highest-ranked FIRST (lowest number wins). They are the
// layers the pre-fold parent-chain walk visited, in the order it visited them.
const (
	tierExactMutable  = iota // T1: the query phase, mutable
	tierExactSealed          // T2: the query phase, sealed
	tierAmbientSealed        // T3: the ambient startup set
	tierNone          = -1   // not a candidate at the query phase at all
)

// probeRankedLocked is resolveRankedLocked's body, additionally reporting the
// winning TIER and taking the highest tier it is allowed to consider.
//
// The tier answers "is this binding part of the startup set?" without a second
// lookup — tierExactMutable is the mutable tier, everything above it is sealed —
// which is what IsSealedBindingAt asks. minTier answers the other non-reference
// question: tierExactSealed skips the mutable tier, asking what the startup set
// bound a name to REGARDLESS of any user shadow (setRecognizedPrimitive's
// fallback, which the pre-fold tree spelled as a direct read of the sealed base
// frame).
//
// The ranking is one lexicographic argmax over (tier, scope cardinality) rather
// than a per-tier accumulator array: three scopedBestOf values are ~190 bytes to
// zero on every global resolution, and this is THE global resolution. The two are
// equivalent. Per-tier argmax then "first non-empty tier wins" is exactly
// lexicographic (tier major, cardinality minor), and ambiguity is flagged only
// against the current best, which is always in the winning tier — a tie in a
// losing tier is dead and must not panic. The one place the forms could differ,
// scopedBestOf's perfect-match branch recording over an equal-weight best, needs
// two slots in ONE tier whose scope sets have the query's cardinality and are
// subsets of it, hence equal to it and to each other, which
// CreateGlobalBindingAt's reuse rule refuses to create.
//
// Caller MUST hold at least a read lock on p.mu, and MUST release it via defer
// rather than a bare RUnlock: this can panic mid-hold, on an incomparable
// equal-cardinality tie IN THE WINNING TIER, wrapped as werr.ErrAmbiguousBinding
// (P8).
func (p *GlobalEnvironmentFrame) probeRankedLocked(key values.Symbol, q syntax.ScopeSet, phase Phase, minTier int) (slot int, tier int, ok bool) {
	slots := p.keys[key]
	if len(slots) == 0 {
		return 0, tierNone, false
	}
	// tierOf classifies s.phase.Any as T3 unconditionally, without consulting
	// s.sealed: (ANY, mutable) is unreachable here by construction, not by a
	// check in this function. CreateGlobalBindingAt is the ONE enforcement
	// point — it panics on (ANY, mutable) at write time — so every ANY slot
	// this ever sees is sealed. Do not add a defensive branch for a state
	// nothing can produce; this runs on the hot resolution path.
	tierOf := func(s slotRef) int {
		switch {
		case s.phase.Any:
			return tierAmbientSealed
		case s.phase.Phase != phase:
			return tierNone
		case s.sealed:
			return tierExactSealed
		default:
			return tierExactMutable
		}
	}
	bestSlot := 0
	bestTier := tierNone
	if q.IsAll() {
		// Wildcard: the highest tier's first live slot, matching the old walk's
		// layer-major first-live behavior.
		for _, s := range slots {
			if s.slot >= len(p.bindings) || p.bindings[s.slot] == nil {
				continue
			}
			t := tierOf(s)
			if t < minTier {
				continue
			}
			if bestTier < 0 || t < bestTier {
				bestTier = t
				bestSlot = s.slot
			}
		}
		return bestSlot, bestTier, bestTier >= 0
	}
	scopes := q.Scopes()
	var bestScopes []*syntax.Scope
	ambiguous := false
	for _, s := range slots {
		if s.slot >= len(p.bindings) || p.bindings[s.slot] == nil {
			continue
		}
		t := tierOf(s)
		if t < minTier {
			continue
		}
		bindingScopes := p.bindings[s.slot].Scopes()
		if !syntax.ScopesCompatible(bindingScopes, scopes) {
			continue
		}
		if bestTier < 0 || t < bestTier || (t == bestTier && len(bindingScopes) > len(bestScopes)) {
			bestSlot = s.slot
			bestTier = t
			bestScopes = bindingScopes
			ambiguous = false
			continue
		}
		// Equal tier and equal cardinality with a different set: neither is a
		// subset of the other, so neither is THE maximal match (Flatt's ambiguity,
		// per scopedBestOf). ScopesMatch(a, b) reports b ⊆ a; at equal cardinality
		// that holds iff the sets are equal, so its negation is "different set".
		//
		// This len-as-cardinality comparison rests on scope sets being
		// duplicate-free: a *Scope appearing twice would make len overstate the
		// set's true cardinality, which could flag ambiguity here that the
		// scopedBestOf array form (per-tier, deduped only by that form's own
		// comparisons) would have resolved. The premise holds because every
		// mutation path is values.AddScopeToSet, which no-ops on a scope already
		// present (scope.go) — no constructor in this tree appends a duplicate.
		if t == bestTier && len(bindingScopes) == len(bestScopes) &&
			!syntax.ScopesMatch(bindingScopes, bestScopes) {
			ambiguous = true
		}
	}
	if ambiguous {
		panic(werr.WrapForeignErrorf(werr.ErrAmbiguousBinding,
			"resolveRankedLocked: identifier %q resolves ambiguously among incomparable hygienic scope sets",
			key.Key))
	}
	return bestSlot, bestTier, bestTier >= 0
}

// resolveAtCoordsLocked resolves at EXACTLY the given coordinates — the write
// path's question ("the binding I just created"), never the read path's ranked
// question. Same scope discipline as one tier of resolveRankedLocked: subset
// compatibility, maximal cardinality wins, an incomparable tie is refused.
//
// Caller MUST hold at least a read lock on p.mu, via defer: this can panic
// mid-hold on an ambiguous tie.
func (p *GlobalEnvironmentFrame) resolveAtCoordsLocked(key values.Symbol, q syntax.ScopeSet, phase PhaseKey, sealed bool) (int, bool) {
	slots := p.keys[key]
	if len(slots) == 0 {
		return 0, false
	}
	matchAny := q.IsAll()
	scopes := q.Scopes()
	var best scopedBestOf[int]
	for _, s := range slots {
		if s.slot >= len(p.bindings) || p.bindings[s.slot] == nil {
			continue
		}
		if s.phase != phase || s.sealed != sealed {
			continue
		}
		if matchAny {
			return s.slot, true
		}
		bindingScopes := p.bindings[s.slot].Scopes()
		if !syntax.ScopesCompatible(bindingScopes, scopes) {
			continue
		}
		record, done := best.shouldRecord(bindingScopes, len(scopes))
		if record {
			best.record(s.slot, bindingScopes)
		}
		if done {
			break
		}
	}
	if best.Ambiguous() {
		panic(werr.WrapForeignErrorf(werr.ErrAmbiguousBinding,
			"resolveAtCoordsLocked: identifier %q resolves ambiguously among incomparable hygienic scope sets",
			key.Key))
	}
	return best.Result()
}

// setValueAtCoords writes v to the binding of key that resolves under q at
// EXACTLY (phase, sealed). It is the store primitive behind every write whose
// target is derived from the writing VIEW rather than from a pinned index.
// Thread-safe: uses full Lock for write access.
func (p *GlobalEnvironmentFrame) setValueAtCoords(key *values.Symbol, q syntax.ScopeSet, phase PhaseKey, sealed bool, v values.Value) error {
	p.mu.Lock()
	defer p.mu.Unlock()

	i, ok := p.resolveAtCoordsLocked(*key, q, phase, sealed)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNoSuchBinding,
			"setValueAtCoords: no such global binding %q at phase %v", key.Key, phase)
	}
	// Publish atomically through the binding's cell so the lock-free
	// cachedBindings reader (Binding.Value with no frame mutex) never tears the
	// two-word interface. The frame Lock still serializes writers.
	p.bindings[i].SetValue(v)
	return nil
}

// SealedBindingAt returns the binding key resolves to under q at phase when the
// MUTABLE tier is skipped: what the startup set bound this name to, regardless
// of any user shadow. nil means NONE — no sealed-tier binding of that name is
// visible from phase.
//
// It is the store form of the pre-fold "read the sealed base frame directly"
// fallback (setRecognizedPrimitive): with one merged store there is no narrower
// frame to address, so the narrowing is a tier floor on the probe.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) SealedBindingAt(key *values.Symbol, q syntax.ScopeSet, phase Phase) *Binding {
	p.mu.RLock()
	defer p.mu.RUnlock()

	slot, _, ok := p.probeRankedLocked(*key, q, phase, tierExactSealed)
	if !ok {
		return nil
	}
	return p.bindings[slot]
}

// IsSealedBindingAt reports whether a read of key under q at phase resolves to a
// SEALED-tier slot — "the binding this name denotes here is part of the startup
// set", which is what refusing to undefine a primitive asks. False covers both
// "resolves to a mutable slot" and "resolves to nothing".
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) IsSealedBindingAt(key *values.Symbol, q syntax.ScopeSet, phase Phase) bool {
	p.mu.RLock()
	defer p.mu.RUnlock()

	_, tier, ok := p.probeRankedLocked(*key, q, phase, tierExactMutable)
	// Every tier above the mutable one IS the sealed tier; the probe already
	// decided which, so this needs no second lookup.
	return ok && tier > tierExactMutable
}

// CreateGlobalBindingAt creates a new global binding with the given key, type
// and resolution coordinates. Returns the GlobalIndex and whether a new binding
// was created (false if the binding already existed at those coordinates).
//
// Reuse requires EXACT scope-set equality — see scopeSetsEqual for why
// compatibility (the subset predicate resolution uses) would be a hygiene hole
// here — AND coordinate equality: two entries of one name at different (phase,
// sealed) are different variables — that is what makes a phase-0 define a
// SHADOW of a sealed entry (new slot) while a define-for-syntax over the
// (1, mutable) registry copy stays a SUPERSEDE (same slot). Scope equality
// alone was sufficient only while coordinates were frame identity.
//
// (ANY, mutable) is refused: no population produces it (design §4.1), and
// modeling it would give the wildcard a mutable row that outranked nothing.
//
// The returned index is DEFERRED (no frame, no slot) — load-bearing, see the
// C2b note above GetGlobalIndex: define-syntax writes through this index
// wildcard and lookupMacroBinding reads it back wildcard.
func (p *GlobalEnvironmentFrame) CreateGlobalBindingAt(key *values.Symbol, bt BindingType, scopes []*syntax.Scope, phase PhaseKey, sealed bool) (*GlobalIndex, bool) {
	if phase.Any && !sealed {
		panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"CreateGlobalBindingAt: (ANY, mutable) is not a modeled coordinate for %q", key.Key))
	}
	// Use full Lock (not RLock) for check-then-write pattern to prevent TOCTOU
	p.mu.Lock()
	defer p.mu.Unlock()

	for _, s := range p.keys[*key] {
		if s.slot >= len(p.bindings) || p.bindings[s.slot] == nil {
			continue
		}
		if s.phase != phase || s.sealed != sealed {
			continue
		}
		if scopeSetsEqual(p.bindings[s.slot].Scopes(), scopes) {
			q := NewGlobalIndex(key)
			return q, false
		}
	}
	i := len(p.bindings)
	p.keys[*key] = append(p.keys[*key], slotRef{slot: i, phase: phase, sealed: sealed})
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
	_, ok := ge.bestSlotLocked(*key, syntax.AllScopes(), false)
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
	i, ok := p.bestSlotLocked(*key, q, false)
	p.mu.RUnlock()

	if !ok {
		return nil
	}
	return newScopeKeyedGlobalIndex(key, p, i, q)
}

// GetOwnGlobalBinding returns the binding for the given GlobalIndex from this frame only.
// Unlike EnvironmentFrame.GetGlobalBinding, this does NOT traverse the parent chain.
// Returns nil if the binding does not exist in this frame.
//
// The stale-pin fallback deliberately does NOT reject the sealed tier (unlike
// SetOwnGlobalValue's write-side fallback): a deleted user shadow should re-heal
// a READ onto the sealed primitive underneath it, the same as ordinary name
// resolution would. Only a WRITE reaching a sealed slot is the hazard.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) GetOwnGlobalBinding(gi *GlobalIndex) *Binding {
	ge := p
	key := gi.Index

	p.mu.RLock()
	defer p.mu.RUnlock()

	i, ok := ge.pinnedSlotLocked(gi)
	if !ok {
		i, ok = ge.bestSlotLocked(*key, gi.query, false)
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
//
// It is the PINNED write. Every production caller passes an index carrying
// (Env, Slot) — a compile-time re-resolve at the writing view's own coordinates
// (OwnGlobalIndex), or the VM's pinned OpStoreGlobal branch. The deferred,
// name-resolved write is a different entry point with a different reach:
// EnvironmentFrame.SetDeferredGlobalValue, restricted to the mutable tier (G13).
//
// The bestSlotLocked fallback below is therefore NOT the non-pinned write path;
// it is the STALE-pin self-heal that pinnedSlotLocked documents — reached only
// when the pinned slot has been nil'd by a delete. rejectSealed=true closes the
// one hole that self-heal would otherwise open over the merged store: without
// it, a pin emptied by namespace-undefine! could re-heal onto a SEALED slot of
// the same name and SetValue it in place — a write reaching a sealed slot by NO
// other path, and the pre-fold frame-local lookup could not reach it either
// (ErrNoSuchBinding). The filter cannot lose a legitimate re-heal: DeleteBindingAt
// never removes a sealed slot (DeleteOwnGlobal derives coordinates from the
// writing view, and namespace-undefine! writes through the mutable runtime
// view), so a pin can never be stale-BY-DELETE for a sealed coordinate — every
// candidate the filter removes here was never a delete-then-redefine target to
// begin with.
//
// The residual: this is still PHASE-blind. A pin addressing (0, mutable) can
// re-heal onto a (1, mutable) slot of the same name if both exist and the
// mutable phase-0 slot is deleted — bestSlotLocked has no phase argument, only
// a sealed/mutable one. Closing that would mean widening GlobalIndex (or this
// fallback) to carry a phase, not extending this filter; left as a named,
// accepted residual, not fixed here.
//
// Thread-safe: uses full Lock for write access.
func (p *GlobalEnvironmentFrame) SetOwnGlobalValue(gi *GlobalIndex, v values.Value) error {
	ge := p

	p.mu.Lock()
	i, ok := ge.pinnedSlotLocked(gi)
	if !ok {
		i, ok = ge.bestSlotLocked(*gi.Index, gi.query, true)
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

// DeleteBindingAt removes the global binding for sym that resolves under the
// given scope set AT EXACTLY (phase, sealed). Returns true if one was found and
// removed.
//
// Delete is a write, so it takes the writer's coordinates rather than the
// reader's tier order: over one merged store a ranked delete of `car` from the
// mutable runtime view would reach the SEALED primitive whenever no user shadow
// existed, which is precisely what namespace-undefine! refuses. Callers reach
// this through EnvironmentFrame.DeleteOwnGlobal, which derives the coordinates
// from the view.
//
// Resolution goes through resolveAtCoordsLocked with a scoped (never wildcard)
// query — the literal call AmbientKeysAt and GetGlobalIndexWithScopes make — so
// delete cannot drift from the read surface at those coordinates. It removes
// exactly the binding a scoped read there would have returned, and deleting a
// name owned only by a macro-introduced binder is a no-op rather than the
// destruction of a binding the caller could not read.
//
// A nil scopes argument means NONE — the empty scope set, same as
// AmbientScopes() — and never MATCH ANY. Nil is indistinguishable from an
// uninitialized value, so resolving it permissively fails open: a caller that
// merely forgot to thread its scopes would delete across a hygiene boundary
// with nothing in the signature to flag it. Delete therefore has no wildcard
// mode at all; "remove the name and every hygiene-distinct binding under it" is
// a legitimate but different operation, and nothing asks for it.
//
// Note: the binding slot in p.bindings is not compacted — index-based
// references from compiled code would be stale. This is only safe for
// top-level REPL/eval bindings, not for bindings referenced by compiled
// bytecode.
//
// Thread-safe: uses full Lock for write access.
func (p *GlobalEnvironmentFrame) DeleteBindingAt(sym *values.Symbol, scopes []*syntax.Scope, phase PhaseKey, sealed bool) bool {
	p.mu.Lock()
	defer p.mu.Unlock()

	i, ok := p.resolveAtCoordsLocked(*sym, syntax.ScopesOf(scopes), phase, sealed)
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
		if s.slot != i {
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
