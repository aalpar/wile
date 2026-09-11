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
	"math/bits"
	"slices"
	"sync"
	"sync/atomic"

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
// delete-then-recreate: DeleteBindingAt nils the slots and drops the name, so once
// anything re-creates it a wildcard fallback would land on whatever binding now
// holds the name — including one whose scope set the reference could never
// reach. This one ScopeSet subsumes what a nil Scopes slice plus a scopeKeyed
// bool once encoded: a nil slice could not distinguish "matched the empty set"
// from "no key at all", and those demand opposite re-resolution.
//
// phase and sealed are the pinned slot's RESOLUTION COORDINATES, set with Env
// and Slot and meaningful only alongside them. While the slot lives they are
// redundant — a slot is named by exactly one slotRef, so (Env, Slot) already
// determines them — which is why they take no part in EqualTo. They exist for
// the moment the slot STOPS living: a delete nils it, the pin falls through to
// re-resolution, and the query alone says which hygiene boundary to stay inside
// but nothing about which phase or tier. Without them re-resolution is
// phase-blind, and a pin addressing (0, mutable) re-heals onto any other slot of
// the same name — in the case that motivated recording them, the registry's
// phase-1 copy of a primitive, which sat at (1, mutable) then and is sealed now.
type GlobalIndex struct {
	Index  *values.Symbol
	Env    *GlobalEnvironmentFrame
	Slot   int
	query  syntax.ScopeSet
	phase  Phase
	sealed bool
}

// NewGlobalIndex creates a new deferred GlobalIndex for the given symbol.
// Env is nil, so Slot is not meaningful; use newResolvedGlobalIndex when the
// owning frame and slot are known. Its query is the wildcard (AllScopes): a
// deferred bare-name index re-resolves by name.
func NewGlobalIndex(key *values.Symbol) *GlobalIndex {
	return &GlobalIndex{Index: key, query: syntax.AllScopes()}
}

// newResolvedGlobalIndex creates a GlobalIndex pinned to the frame and to the
// slot resolution landed on, coordinates included. Its query is the wildcard
// (AllScopes): a wildcard resolution re-resolves by bare name if its slot dies.
func newResolvedGlobalIndex(key *values.Symbol, env *GlobalEnvironmentFrame, ref slotRef) *GlobalIndex {
	return newScopeKeyedGlobalIndex(key, env, ref, syntax.AllScopes())
}

// newScopeKeyedGlobalIndex is newResolvedGlobalIndex for a resolution that
// matched on a scope-set query, recording that query as the index's key. The
// scoped lookups compute it and would otherwise discard it, leaving the pinned
// index indistinguishable from a wildcard one the moment its slot dies. Callers
// reach it through GetGlobalIndexWithScopes, whose query is never the wildcard,
// so re-resolution stays inside the hygiene key even when the set is empty.
//
// It takes the whole slotRef rather than a bare slot so a pin cannot be minted
// without its coordinates: the two heals below both need them, and a caller that
// had only the index would have to guess.
func newScopeKeyedGlobalIndex(key *values.Symbol, env *GlobalEnvironmentFrame, ref slotRef, q syntax.ScopeSet) *GlobalIndex {
	return &GlobalIndex{Index: key, Env: env, Slot: ref.slot, query: q, phase: ref.phase, sealed: ref.sealed}
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

// slotRef locates one binding of a name and carries its resolution coordinates
// (design §4.1). slot indexes bindings, as the bare int did; phase and sealed
// are resolution coordinates — nothing after resolution needs them, which is
// why they live here and not on BindingMeta (design Q1).
type slotRef struct {
	slot   int
	phase  Phase
	sealed bool
}

// GlobalEnvironmentFrame is one OWNER's whole binding store: every global
// binding a namespace or library env holds, at every phase, sealed and mutable
// alike, in one scope-keyed slot table.
//
// Design: it has no hierarchy of its own, and after the store fold there is no
// hierarchy above it either — an owner's phase frames are VIEWS over this one
// instance, distinguished by the (phase, sealed) coordinates their writes stamp
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
	// exactPhases is the set of NON-NEGATIVE exact phases this store has ever held
	// a slot at, one bit per phase. It answers "which phases are worth searching?"
	// for the cross-phase searches (EnvironmentFrame.PresentPhases) without a scan
	// of keys, which is O(names) and sits on the macro-compilation path.
	//
	// A bitset rather than a map because Phase is an int8 and the whole
	// non-negative domain is 128 bits: two inline words, no allocation, and every
	// owner store pays for one — a startup with hundreds of library envs would
	// otherwise pay a map header each.
	//
	// It GROWS ONLY: a delete does not retract a phase. Over-approximating is
	// harmless — a search of a phase with no slots misses — while
	// under-approximating is the defect this closes, so the cheap direction is
	// also the safe one.
	//
	// EVERY created slot notes its phase, with no exemption. There used to be
	// one: a slot at the ambient coordinate was skipped, on the argument that it
	// was a candidate at whatever phase was already being searched and so added
	// nothing here. Deleting that coordinate removed the only slot whose phase
	// was not the phase it was searchable at, and with it the argument for a
	// gated write. Negative phases are still not in the set, because
	// PresentPhases excludes them by contract; noteExactPhaseLocked drops them
	// rather than each caller testing first.
	exactPhases [2]uint64
	// bulkRows holds this owner's installed bulk rows: each one a single
	// resolution candidate standing for every name some other store supplies at
	// some phase. See bulk_source.go.
	//
	// It is the store's FOURTH piece of state, and Copy must carry it — leaving
	// exactPhases empty was the defect that made a copied namespace's phase-2
	// bindings unreachable, and a dropped bulk row is the same defect with a
	// larger blast radius, since a report env would lose every bulk-supplied
	// name at once.
	bulkRows []bulkRef
	// macroPhaseRows holds row TEMPLATES to install at every macro phase the
	// owner mints a view for. See InstallMacroPhaseRow for why the tower's
	// laziness forces this shape rather than an enumeration or a wildcard.
	macroPhaseRows []bulkRef
	// macroPhasesSeen is the set of macro phases already carrying the templates.
	macroPhasesSeen map[Phase]struct{}
	// macroPhaseSeenBits mirrors macroPhasesSeen as a lock-free bitset, so the
	// per-AtPhase check costs one atomic load rather than a read lock.
	macroPhaseSeenBits [2]atomic.Uint64
	// bulkResolutions counts resolutions a bulk row answered.
	//
	// Design section 6.3 half two: the count must be non-zero and within a pinned
	// bound over a fixed representative program. Both directions are failures.
	// Without it, "rows installed but never consulted" and "silent fallback to
	// eager per-name copying" each leave the whole suite green — the change fails
	// toward the old behaviour and the old behaviour passes.
	//
	// Atomic because it is incremented under the store's READ lock, where a plain
	// field write would race two concurrent compiles.
	bulkResolutions atomic.Int64
}

// NewGlobalEnvironmentFrame creates a new, empty owner store.
func NewGlobalEnvironmentFrame() *GlobalEnvironmentFrame {
	q := &GlobalEnvironmentFrame{
		bindings: []*Binding{},
		keys:     map[values.Symbol][]slotRef{},
	}
	return q
}

// noteExactPhaseLocked records that this store holds a slot at phase.
// Caller MUST hold the write lock on p.mu.
func (p *GlobalEnvironmentFrame) noteExactPhaseLocked(phase Phase) {
	if phase < 0 {
		return
	}
	p.exactPhases[phase>>6] |= 1 << (uint(phase) & 63)
}

// appendExactPhases appends the phases this store holds slots at to dst, in
// ascending order. Appending rather than returning a fresh slice keeps
// PresentPhases — which merges this with the registry's phases on the
// macro-compilation path — allocation-free past dst's own growth.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) appendExactPhases(dst []Phase) []Phase {
	p.mu.RLock()
	defer p.mu.RUnlock()

	for i, word := range p.exactPhases {
		for word != 0 {
			bit := bits.TrailingZeros64(word)
			dst = append(dst, Phase(i*64+bit))
			word &= word - 1
		}
	}
	return dst
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
		// A DELETED slot stays nil and keeps its position. DeleteBindingAt nils
		// the slot without compacting, precisely so every surviving index still
		// addresses the binding it addressed before; compacting here would shift
		// every later slot out from under the cloned key lists. Nothing points at
		// a nil slot — delete prunes the slotRef too — so the wasted allBindings
		// entry is the whole cost.
		if b == nil {
			continue
		}
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
	// The copy holds slots at every phase the source did, so it must report them.
	// Rebuilding this from the source's keys instead would be the same set at
	// higher cost; leaving it empty was the defect that made a copied namespace's
	// phase-2 bindings unreachable to every cross-phase search.
	q.exactPhases = p.exactPhases

	// Bulk rows are carried, with self-referential rows RE-POINTED at the copy.
	//
	// Both halves are forced. Dropping the rows would cost a report env every
	// bulk-supplied name, which after Stage A is the entire base — the matrix's
	// report-env row is pinned on cond, which has a per-symbol phase-1 slot and
	// could not tell. Copying a row verbatim keeps the edge pointing at the
	// SOURCE store, which is right for a genuine import: an exporting library is
	// shared by every importer on master too, and R7RS section 5.2 refuses set!
	// on an imported binding, so the sharing is not observable as mutation.
	//
	// A row whose src is THIS store is the case that would break
	// NewSchemeReportNamespace's contract that "q aliases nothing": resolving it
	// in the copy would materialize a slot holding the ORIGINAL's *Binding, and a
	// set! through the copy would reach the parent. Re-pointing is the fix, and
	// it is cheap because a source knows the store it reads.
	//
	// moved is shared by both calls below so that a macro-phase TEMPLATE and the
	// rows installMacroRowLocked already materialized from it keep sharing ONE
	// source in the copy, as they share one in the original.
	moved := map[BulkSource]BulkSource{}
	q.bulkRows = p.carryBulkRows(q, p.bulkRows, moved)

	// The macro-phase state is THREE fields and they are carried TOGETHER.
	//
	// Splitting them trades a missing row for a duplicate one in either
	// direction. installMacroRowLocked appends UNCONDITIONALLY, so carrying the
	// templates while zeroing the seen set makes the copy's first
	// EnsureMacroPhaseRows re-install a template at a phase whose materialized
	// row q.bulkRows already holds; carrying the seen set while dropping the
	// templates leaves every phase the copy reaches FIRST with no vocabulary row
	// at all. Dropping all three — which is what Copy did — is the second of
	// those: measured, parent macroPhaseRows=1 macroPhasesSeen=1 became copy
	// macroPhaseRows=0 macroPhasesSeen=0.
	//
	// The bitset is the lock-free mirror of the seen set, so it is carried for
	// the same reason and by the same rule; it cannot be assigned as an array,
	// since copying an atomic.Uint64 is what go vet's copylocks refuses.
	q.macroPhaseRows = p.carryBulkRows(q, p.macroPhaseRows, moved)
	if p.macroPhasesSeen != nil {
		q.macroPhasesSeen = maps.Clone(p.macroPhasesSeen)
	}
	for i := range p.macroPhaseSeenBits {
		q.macroPhaseSeenBits[i].Store(p.macroPhaseSeenBits[i].Load())
	}
	return q
}

// carryBulkRows clones rows for the copy q, re-pointing every row whose source
// reads p — and only those — at q.
//
// selfStore rather than a type assertion on *storeBulkSource: the row that
// carries the macro vocabulary is a filteredBulkSource, and a renaming import
// set is a renamedBulkSource, so an assertion on the bare type re-points neither
// and leaves both reading the parent. A row left pointing at the parent is not
// merely shared, it is INERT — materializeBulkLocked requires store == p, so the
// lookup finds the parent's *Binding and resolution then reports a miss.
//
// A row over a FOREIGN store is carried verbatim, which is right for a genuine
// import: the exporting library is shared by every importer on master too.
//
// moved memoizes by source, so one source re-pointed for several rows stays one
// source in the copy.
//
// Caller MUST hold at least a read lock on p.mu.
func (p *GlobalEnvironmentFrame) carryBulkRows(q *GlobalEnvironmentFrame, rows []bulkRef, moved map[BulkSource]BulkSource) []bulkRef {
	out := make([]bulkRef, len(rows))
	for i, row := range rows {
		out[i] = row
		src, _, ok := selfStore(row.src)
		if !ok || src != p {
			continue
		}
		repointed, hit := moved[row.src]
		if !hit {
			repointed = row.src.repoint(q)
			moved[row.src] = repointed
		}
		out[i].src = repointed
	}
	return out
}

// Bindings returns a copy of the bindings slice.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) Bindings() []*Binding {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return slices.Clone(p.bindings)
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
//
// It KEEPS the name "ambient" where AmbientKeysAt lost it. This one names the
// empty scope set, a hygiene concept that is still live and still called that;
// the other named the deleted ANY-phase coordinate. Two senses of one word, and
// only one of them was deleted.
func AmbientScopes() []*syntax.Scope {
	return []*syntax.Scope{}
}

// UnscopedKeysAt returns the names holding a live binding under the EMPTY scope
// set AT phase: the names a reference written outside any macro expansion, in
// phase-N code, resolves.
//
// It was AmbientKeysAt, and the rename is the point: "ambient" named two
// unrelated things, an empty SCOPE set and the deleted ANY-phase COORDINATE, and
// this method only ever meant the first. The coordinate is gone; keeping its
// adjective on a scope-set query would leave the one surviving sense reading as
// a reference to the dead one.
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
// per name — where the pre-fold AmbientKeys this generalizes ran a cheaper
// scope-only best-of per name (no tier walk, since a frame's own store held only
// one layer). Same asymptotic shape, larger constant per name. The only
// consumer, BoundSymbolNames, is a REPL-completion path, not a hot one; not
// restructured here.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) UnscopedKeysAt(phase Phase) []values.Symbol {
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

// LiveSlots snapshots every live slot in the store: any phase, sealed or not. It is
// the "every binding this owner holds anywhere" enumeration that the doc/apropos
// walk wants, and it replaces the old union over every phase frame plus every
// sealed frame — which, now that all of those are views over one store, would
// range the same map once per view.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) LiveSlots() []NamedSlot {
	return p.slotsFiltered(false)
}

// SealedSlots snapshots every live SEALED-tier slot in the store, at any phase.
// This is the sealed-filtered form of LiveSlots: the startup set a registry apply
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

// healReadLocked re-resolves a PINNED index whose slot a delete has emptied, for
// a READ: the ranked probe at the pin's own phase, the same question ordinary
// name resolution asks there.
//
// The polarity is deliberately the opposite of healWriteLocked's. Revealing the
// sealed primitive a deleted user shadow uncovers is exactly what a read should
// do — after (namespace-undefine! … 'car) the name denotes the primitive again —
// so the probe keeps its full tier order rather than being pinned to the tier the
// dead slot sat in.
//
// RANKED-HEAL IS THE SEMANTICS FOR EVERY PIN, including one minted at exact
// coordinates, and nothing here is a fallback from a narrower rule. The
// coordinate-addressed arm this used to carry existed for the ambient key alone,
// whose premise was that it named no phase and so gave the ranked probe nothing
// to probe at. Collapsing the coordinate did not choose between two live
// behaviours: the arm was already unreachable, because createGlobalBindingAt
// refused the ambient key outright, so no slotRef — and therefore no pin, which
// copies its coordinates off one — could carry it. Every pin this store has ever
// handed out names an exact phase, and the paragraph above is the whole rule for
// all of them.
//
// Returns false for a DEFERRED index (Env == nil, or another owner's store):
// there are no coordinates to re-resolve at, and "resolve against whatever
// environment is live" is EnvironmentFrame.GetGlobalBinding's job, not this
// store's.
//
// Caller MUST hold at least a read lock on p.mu, via defer: the probe can panic
// mid-hold on an ambiguous tie (P8).
func (p *GlobalEnvironmentFrame) healReadLocked(gi *GlobalIndex) (int, bool) {
	if gi.Env != p {
		return 0, false
	}
	ref, _, ok := p.probeRankedLocked(*gi.Index, gi.query, gi.phase, tierExactMutable)
	return ref.slot, ok
}

// healWriteLocked re-resolves a PINNED index whose slot a delete has emptied, for
// a WRITE: at EXACTLY the coordinates the pin addressed, never the ranked order.
//
// This is what makes the self-heal a re-heal rather than a relocation. The pin
// named one variable, at one (phase, tier); a delete-then-redefine at those same
// coordinates re-creates that variable and the write should find it. Anything
// else with the same name is a DIFFERENT variable — the sealed primitive
// underneath a deleted shadow, or the phase-1 registry copy — and a write that
// reached one would be an escalation no other path permits. Coordinate equality
// subsumes the sealed/mutable filter this replaces AND closes the phase axis that
// filter had no argument for.
//
// Returns false for a DEFERRED index, for healReadLocked's reason;
// EnvironmentFrame.SetDeferredGlobalValue is that index's write path.
//
// KNOWN AND DELIBERATELY NOT CLOSED HERE: at a SEALED coordinate this is
// provenance-blind, for the same reason DeleteBindingAt was. (phase 0, sealed)
// holds the startup set's slot and an import's; resolveAtCoordsLocked filters on
// (phase, sealed) alone, so a sealed pin whose slot died can re-heal onto the
// other one. Closing it needs the provenance axis DeleteImportedBindingAt has —
// the imported bit threaded onto the pin and into resolveAtCoordsLocked — which
// is a change to the coordinate SHAPE and does not belong inside the collapse of
// the phase coordinate. It is latent rather than live: the base's own slot now
// survives a delete (CreateImportedGlobalBindingAt's reuse refusal), so a base
// pin no longer falls through to heal at all, and no caller was found that
// reaches here sealed with both slots present. Recorded, not fixed.
//
// Caller MUST hold the write lock on p.mu, via defer: resolveAtCoordsLocked can
// panic mid-hold on an ambiguous tie (P8). Before that was fixed (round 2
// review), a bare Unlock before each return left the lock held across the panic —
// the VM-boundary recover catches it and the process survives, so the store's
// write mutex stayed held forever, wedging every subsequent global read and write.
func (p *GlobalEnvironmentFrame) healWriteLocked(gi *GlobalIndex) (int, bool) {
	if gi.Env != p {
		return 0, false
	}
	return p.resolveAtCoordsLocked(*gi.Index, gi.query, gi.phase, gi.sealed)
}

// resolveRankedLocked is the flat model's one resolution rule (design §4.3):
// a slot is a candidate iff its scopes are compatible AND its phase IS the query
// phase — tierExactMutable, then the two sealed tiers; any OTHER phase is not a
// candidate at all, which is phase hermeticity as key disjointness (P5). The
// highest non-empty tier wins;
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
func (p *GlobalEnvironmentFrame) resolveRankedLocked(key values.Symbol, q syntax.ScopeSet, phase Phase) (slotRef, bool) {
	ref, _, ok := p.probeRankedLocked(key, q, phase, tierExactMutable)
	if ok || len(p.bulkRows) == 0 {
		return ref, ok
	}
	// THE ONE NEW RULE (design section 3.1, from Racket's
	// syntax/binding-table.rkt): a per-symbol slot and a bulk row at EQUAL tier
	// and EQUAL scope set resolve to the per-symbol SLOT. Differing scope sets
	// fall back to plain maximality. It is a TIE-BREAK, not a precedence layer —
	// a row that always lost, or always won, is a third ranking axis wearing a
	// tie-break's name, and removing the third axis is the point of this stage.
	//
	// Consulting rows only on a per-symbol MISS implements that rule exactly,
	// given TWO measured premises: every row this tree installs carries the EMPTY
	// scope set, and every one is BulkOriginLanguage. A row is sealed and
	// language-declared, so bulkTierOf puts it at tierExactSealed: a
	// tierExactMutable slot outranks it and so does a tierExactImported one; a
	// tierExactSealed slot ties it on tier and, with both scope sets empty, the
	// tie-break awards the slot; and no slot can lose on cardinality to an empty
	// row set. So a row can win only where there is no candidate slot at all,
	// which is this branch.
	//
	// This paragraph read "hence T2 … a T2 slot ties it" until 2026-09-10, and
	// both counts were wrong: inserting tierExactImported moved the sealed tier
	// from second to third and nothing went red. The conclusion was unaffected —
	// the new tier outranks a row, which only strengthens "a row wins only on a
	// miss" — but the premise a later rework of this consultation would have
	// started from was off by a tier, which is why the tiers are named here and
	// not numbered.
	//
	// The origin premise arrived with bulkRef.origin on 2026-09-10 and is the one
	// to watch: a BulkOriginImport row ranks tierExactImported and would OUTRANK
	// a tierExactSealed slot, which this branch never gives it the chance to do.
	// The first such row therefore breaks miss-only exactly as the first
	// non-empty row set does, and needs the full argmax below rather than a
	// widened tie-break. No production path installs one — imports take
	// per-symbol slots — so the premise holds; it is pinned, not assumed, by
	// TestEveryOriginRowIsLanguageDeclared (pkg/wile).
	//
	// The premise is load-bearing, so it is pinned rather than assumed:
	// TestBulkRowsCarryTheEmptyScopeSet, which lives in pkg/wile and runs over a
	// LIBRARY-BEARING engine. It moved there on 2026-09-10: a library store's
	// SLOTS do carry non-empty scope sets, so a ratchet over a bare namespace was
	// looking only where the premise was never in doubt. A row with a non-empty
	// scope set would need the full argmax below, and it does not exist yet —
	// Stage B's move of the phase INTO the scope set is what creates one.
	//
	// The miss-only shape is also why the hot path is untouched. Rows are
	// consulted per FAILED resolution, not per resolution, and probeBulkLocked's
	// per-row lookup is lock-free because the caller already holds this store's
	// read lock.
	bulkRow, bulkBnd, bulkAmbiguous, bulkOk := p.probeBulkLocked(key, q, phase, tierExactMutable)
	if bulkAmbiguous {
		// The same answer the per-symbol probe gives the same configuration.
		// Consulting rows on its MISS must not resolve where it would have
		// refused — that is the divergence Task 5 closed, and returning the
		// first-seen row here would re-open it one layer down.
		panic(werr.WrapForeignErrorf(werr.ErrAmbiguousBinding,
			"resolveRankedLocked: identifier %q is supplied by incomparable hygienic scope sets across bulk rows",
			key.Key))
	}
	if !bulkOk {
		return ref, false
	}
	materialized, mok := p.materializeBulkLocked(key, bulkRow, bulkBnd)
	if !mok {
		return ref, false
	}
	p.bulkResolutions.Add(1)
	return materialized, true
}

// materializeBulkLocked turns a winning bulk row into an ordinary slotRef, so
// every caller downstream of resolution keeps seeing a slot and a GlobalIndex is
// unchanged (D6).
//
// It has NO write path, and that is a deliberate narrowing of the design's D6
// rather than an omission. Every row Stage A installs reads THIS store — the
// base's rows are self-referential, because design section 4.1 keeps
// LoadBootstrapCore writing the base directly, so the base STORE is the source.
// For such a row the binding is already a slot here: the row exists only to
// widen which PHASE can see it, so resolution can return the source's own
// slotRef and there is nothing to copy, nothing to allocate, and no RLock to
// upgrade.
//
// That also makes the row live past resolution, not merely at it: a phase-1 read
// and a phase-0 read reach the very same *Binding, so a later write through
// either is seen by both. A materializing copy would have silently forked them.
//
// A row over a FOREIGN store — an import routed through bulk rows, which is
// Task 6 — cannot take this path: there is no slot here to return, so it needs a
// slot minted in this store, which needs the write lock and the re-check the
// design describes. That work is NOT done here, and no row in the tree reaches
// it: the false return is the honest answer for a source this cannot serve, and
// resolution falls back to the per-symbol probe.
//
// Caller MUST hold at least a read lock on p.mu.
func (p *GlobalEnvironmentFrame) materializeBulkLocked(key values.Symbol, row bulkRef, bnd *Binding) (slotRef, bool) {
	store, sourcePhase, ok := selfStore(row.src)
	if !ok || store != p {
		return slotRef{}, false
	}
	for _, s := range p.keys[key] {
		if s.slot >= len(p.bindings) || p.bindings[s.slot] == nil {
			continue
		}
		if s.phase != sourcePhase {
			continue
		}
		if p.bindings[s.slot] != bnd {
			continue
		}
		return s, true
	}
	return slotRef{}, false
}

// The probe's tiers, highest-ranked FIRST (lowest number wins). They are the
// layers the pre-fold parent-chain walk visited, in the order it visited them.
//
// A comment names a tier by its IDENTIFIER, never by an ordinal ("T2"). Nothing
// ties an ordinal in prose to this enum, so inserting tierExactImported
// renumbered everything below it and every "T2" in the tree silently changed
// referent with no test going red. An identifier moves with the constant; an
// ordinal is a second, unchecked copy of the ordering.
//
// About a hundred ordinal labels still exist and are NOT swept.
// TestTierOrdinalsHaveNotRenumbered is a tripwire on the values below: it goes
// red on exactly the event that moves a label's referent, and its failure
// message says to rewrite the labels rather than to update the test. A
// site-count ratchet would be the wrong shape here — the population does not
// change when a tier is inserted, so a count stays green through the failure it
// would be there to catch.
//
// It pins THIS ENUM, not those labels, so it certifies only that no referent has
// moved since its baseline — and it froze that baseline without validating it. A
// label that was already wrong stays wrong and stays green. The worked example is
// resolveRankedLocked's bulk-consultation tie-break premise, above this enum in
// this file; the test's own doc carries that case and two more limits, including
// why appending a tier here is label-safe but not reachability-safe.
const (
	tierExactMutable  = iota // the query phase, mutable
	tierExactImported        // the query phase, sealed, INSTALLED BY AN IMPORT
	tierExactSealed          // the query phase, sealed, the startup set
	tierCount                // NOT a tier: one past the last, so tierHighest tracks the enum
	tierNone          = -1   // not a candidate at the query phase at all
)

// tierHighest is the last ranked tier — the ceiling both probes apply.
//
// Derived from the enum rather than written as tierExactSealed, and that is the
// whole point of it existing. Until 2026-09-10 the ceiling was a `maxTier`
// PARAMETER, and all four ranked readers passed the literal tierExactSealed:
// probeRankedLocked, resolveRankedLocked, ExactBindingAt and BulkBindingAt. So
// APPENDING a tier was a silent defect — the new tier failed `t > maxTier` in
// every reader and resolved nothing, with no test red.
// TestTierOrdinalsHaveNotRenumbered says so in its own doc and cannot catch it,
// because appending renumbers nothing.
//
// With the ceiling derived from iota, appending a tier extends it, and the new
// tier is reachable the moment it exists. The parameter is gone: it had exactly
// one value at every call site, which is the same fact from the other side.
const tierHighest = tierCount - 1

// rankedArgmax is the lexicographic argmax over (tier, scope cardinality) that
// decides BOTH halves of a resolution: the per-symbol slots probeTiersLocked
// walks and the bulk rows probeBulkLocked walks.
//
// One body, because it is one RULE. Until 2026-09-10 it was two. The slot walk
// broke an equal-tier tie with `>` — the FIRST maximal candidate wins — and
// reported an incomparable equal-cardinality tie as ambiguous. The row walk
// broke it with `>=` — the LAST maximal candidate wins — and computed no
// ambiguity at all, so a reference that the per-symbol probe would have refused
// got an answer as soon as a row supplied it.
//
// Nothing made the divergence visible: flipping the slot side's `>` to `>=`
// left the entire suite green, so the tie-break OPERATOR was untested, not just
// the ambiguity half. TestBulkRowTieRanksLikeASlotTie is the pin.
//
// `>` over `>=` is justified on the SAME-STORE fact, not on the row walk's old
// comment about install order shadowing. Every live row is a storeBulkSource
// over one store at PhaseRuntime, so two rows that tie here return the
// IDENTICAL *Binding from lookupExportSameStore and the winner's identity is
// discarded by materializeBulkLocked, which keys on selfStore rather than on
// the row. Choosing first-seen therefore changes no answer today, and it makes
// the two walks agree, which is the point.
//
// It is NOT scopedBestOf, and not for its ambiguity polarity — that agrees,
// since two candidates passing ScopesCompatible at equal cardinality forces set
// equality. It is the PERFECT-MATCH EARLY EXIT: scopedBestOf returns done on
// `weight > 0 && weight == target` and every caller breaks, while a tiered walk
// must not stop, because a perfect match in one tier says nothing about a
// higher tier later in the list.
//
// It is a plain struct held in a local and mutated in place. A []candidate, an
// append-built list, or an interface-typed candidate would each spend the whole
// win: this is THE global resolution path, and it is 0 B/op, 0 allocs/op.
// BenchmarkBulkRowResolution is the gate — it is the only benchmark that reaches
// this body per iteration, since BenchmarkGlobalLookup reads a PINNED index and
// never resolves at all. The slot walk shares the body, so the gate covers it.
//
// consider does NOT inline: the slice-header store plus the ScopesMatch call put
// it at cost 126 against the inliner's budget of 80. Measured, interleaved, ten
// rounds, against c02f8a3c: BenchmarkGlobalLookup (a pinned index, which never
// reaches here) -0.92%, BenchmarkEngineStartup and ...WithImport between -0.31%
// and +0.43% against a same-binary drift floor of +/-1.3%, and
// BenchmarkBulkRowResolution +8.4% at one row to +13.4% at sixteen. That last is
// a microbenchmark of a path an engine walks 77 times over at most three rows —
// under a microsecond per engine, which is why it does not reach the end-to-end
// figure. Part of it is not the call but the RULE: under the old `>=` every
// equal-cardinality row recorded, and under `>` all but the first fall through
// to the tie test. A //go:noinline split of the ScopesMatch half was tried and
// recovered about a quarter of it; it was dropped as an unprecedented directive
// bought with an unmeasurable win.
//
// WHAT IT DOES NOT MERGE. This rule — maximal-cardinality scope-subset match,
// incomparable tie is ambiguous — is written SEVEN times in this tree. Two of
// them are this body's callers. The other five are NOT unified, and a reader who
// changes the rule here must change them by hand:
//
//   - probeTiersLocked's own q.IsAll() branch, immediately below. Deliberate,
//     and its comment says why: a wildcard has no scope set to be maximal
//     against, so tier-only first-live is a different rule, not a lazy copy.
//   - resolveAtCoordsLocked, the WRITE path's exact-coordinate question.
//   - EnvironmentFrame.localBinding (environment_frame.go), the local half of
//     GetBinding, which ranks over the lexical parent chain rather than tiers.
//   - EnvironmentFrame.GetLocalIndex (environment_frame.go), same chain,
//     returning an index instead of a binding.
//   - nameSet.shadowLookup (pkg/internal/validate/frame_reclaim_build.go), which
//     re-implements it by hand, NAME-KEYED, with a tri-state verdict, because a
//     compile-time analysis must not panic on the tie.
//
// The three middle ones use scopedBestOf, and so inherit its perfect-match early
// exit — correct for them, because none of the three is tiered. shadowLookup
// does not, and the wildcard branch has no accumulator at all. Merging any of
// them is not this type's job and is not implied by its existence.
type rankedArgmax struct {
	tier      int
	scopes    []*syntax.Scope
	ambiguous bool
}

// newRankedArgmax returns an accumulator with no candidate recorded.
func newRankedArgmax() rankedArgmax {
	return rankedArgmax{tier: tierNone}
}

// consider ranks one candidate that has already passed the tier range and the
// scope filter, and reports whether it is the new best. The caller records its
// own candidate identity — a slotRef, or a bulkRef plus the binding it
// supplied — only when this returns true, so neither walk builds a candidate it
// then discards.
func (p *rankedArgmax) consider(tier int, scopes []*syntax.Scope) bool {
	if p.tier < 0 || tier < p.tier || (tier == p.tier && len(scopes) > len(p.scopes)) {
		p.tier = tier
		p.scopes = scopes
		// A recorded tie is dead the moment a strictly better candidate appears:
		// ambiguity is asked only of the WINNING tier and cardinality, so a tie in
		// a losing tier must not raise.
		p.ambiguous = false
		return true
	}
	// Equal tier and equal cardinality with a different set: neither is a subset
	// of the other, so neither is THE maximal match (Flatt's ambiguity).
	// ScopesMatch(a, b) reports b ⊆ a; at equal cardinality that holds iff the
	// sets are equal, so its negation is exactly "different set".
	//
	// The len-as-cardinality comparison rests on scope sets being duplicate-free:
	// a *Scope appearing twice would make len overstate the set's true
	// cardinality and flag an ambiguity that is not one. Every mutation path is
	// values.AddScopeToSet, which no-ops on a scope already present (scope.go).
	// `len(scopes) > 0` is not a second rule, it is ScopesMatch's answer for the
	// case it covers: the empty set is unique, so two candidates at cardinality
	// zero are the SAME set and ∅ ⊆ ∅ holds. Every bulk row and every unscoped
	// slot takes it, so it skips the call on the common path.
	if tier == p.tier && len(scopes) == len(p.scopes) && len(scopes) > 0 &&
		!syntax.ScopesMatch(scopes, p.scopes) {
		p.ambiguous = true
	}
	return false
}

// found reports whether any candidate was recorded.
func (p *rankedArgmax) found() bool {
	return p.tier >= 0
}

// tierExactImported is the coordinate `storeBulkSource.ownInstallsOnly` was
// standing in for, and adding it is what makes the base separable from an import
// by COORDINATE rather than by predicate.
//
// Stage A relocated imports onto (phase 0, sealed), which is exactly where
// the base's own writes land once the ambient branch is gone, and recorded that
// "there is no third coordinate to move either onto". There was not; this is it.
// Drawing the line by predicate instead left the WRITE path sharing one slot:
// CreateGlobalBindingAt's reuse rule matches on (phase, sealed, scopes) and an
// import agrees with the base on all three, so `created == false`,
// SetOwnGlobalValue replaced the startup set's value in place and
// markBindingImported stamped the startup set as imported — after which
// ownInstallsOnly refused THE BASE. Measured 2026-09-09: one slot, same pointer,
// and `(import (scheme base))` alone stripped `not` from the phase-1 macro
// vocabulary and made every base primitive it covered user-deletable.
//
// With the tier, the base's source floors at tierExactSealed and an import is
// simply below the floor. ownInstallsOnly was kept for a day as a second,
// independent reason for the same answer, then deleted on 2026-09-10: the two
// reasons were not independent. The predicate was provably dead only because ONE
// constructor set the floor and the predicate in one struct literal, while
// storeBulkSource.repoint copied them as separate fields — so a source floored
// at tierExactMutable with the predicate still set would have survived a Copy
// and refused imports the floor admitted. A proof resting on an uncoupled
// coincidence is not a proof, and deleting the second field is what couples it.
//
// A row draws the same line with bulkRef.origin, which is the coordinate rather
// than a predicate over the binding a row happens to supply. See BulkOrigin.

// probeTiersLocked is the ranked probe over this store's PER-SYMBOL slots, with
// the tie REPORTED rather than raised: candidates are the slots whose tier t
// satisfies minTier <= t <= tierHighest. It is the one body every ranked read
// shares; probeRankedLocked raises on ambiguous for the readers that want the
// compile-boundary CompilationError, and ExactBindingAt returns it for the R7RS
// §4.3.2 literal pin, which carries a tie across a multi-phase descent as an
// answer.
//
// The tier answers "is this binding part of the startup set?" without a second
// lookup (tierExactMutable is the mutable tier, everything above it is sealed),
// which is what IsSealedBindingAt asks. minTier answers the other non-reference
// question: tierExactSealed as the floor asks what the STARTUP SET bound a name
// to, skipping both the mutable tier (a user shadow) and tierExactImported (an
// import), since both rank below it — setRecognizedPrimitive's fallback, which
// the pre-fold tree spelled as a direct read of the sealed base frame.
//
// The ceiling is tierHighest, which is the enum's own last tier rather than a
// parameter. Until 2026-09-10 it was a `maxTier` argument and all four ranked
// readers passed the literal tierExactSealed; see tierHighest for why a
// constant derived from iota is not the same thing as the literal it replaces.
//
// The ranking is one lexicographic argmax over (tier, scope cardinality) —
// rankedArgmax, shared with probeBulkLocked — rather than a per-tier
// accumulator array: three scopedBestOf values are ~190 bytes to zero on every
// global resolution, and this is THE global resolution. The two forms are
// equivalent. Per-tier argmax then "first non-empty tier wins" is exactly
// lexicographic (tier major, cardinality minor), and ambiguity is flagged only
// against the current best, which is always in the winning tier — a tie in a
// losing tier is dead and must not panic. The one place the forms could differ,
// scopedBestOf's perfect-match branch recording over an equal-weight best, needs
// two slots in ONE tier whose scope sets have the query's cardinality and are
// subsets of it, hence equal to it and to each other, which
// CreateGlobalBindingAt's reuse rule refuses to create.
//
// There is no perfect-match early exit and must not be: see rankedArgmax.
//
// Caller MUST hold at least a read lock on p.mu. This function does not panic.
// It returns the winning slotRef, not a bare slot: a pin records the
// coordinates it resolved at (GlobalIndex.phase/sealed), and recovering them
// from the slot afterwards would mean a second scan of the name's slot list.
func (p *GlobalEnvironmentFrame) probeTiersLocked(key values.Symbol, q syntax.ScopeSet, phase Phase, minTier int) (ref slotRef, tier int, ambiguous bool, ok bool) {
	slots := p.keys[key]
	if len(slots) == 0 {
		return slotRef{}, tierNone, false, false
	}
	// tierOf has NO ambient arm, and no longer needs one to say so. The
	// coordinate it used to refuse was a wildcard FLAG alongside the phase, whose
	// level was the zero value — so a missing arm would have let an ambient slot
	// alias phase 0 rather than being ignored, and the arm was the thing standing
	// between "the tier is deleted" and "the tier is spelled 0". With the flag
	// gone there is no denormalized state left to refuse: a slot carries a phase
	// and nothing else, and a phase either is the query's or is not.
	tierOf := func(s slotRef) int {
		switch {
		case s.phase != phase:
			return tierNone
		case !s.sealed:
			return tierExactMutable
		case s.slot < len(p.bindings) && p.bindings[s.slot] != nil && p.bindings[s.slot].IsImported():
			// A sealed slot an import created. It outranks the startup set at the
			// same coordinate, which is what makes (import (rename …)) shadow a base
			// name, and it is BELOW the base's own source floor, which is what keeps
			// the base's bulk row supplying the base rather than the import.
			return tierExactImported
		default:
			return tierExactSealed
		}
	}
	if q.IsAll() {
		// The QUERY wildcard, and it is deliberately NOT routed through
		// rankedArgmax. A wildcard expresses no scope constraint, so there is no
		// subset relation to maximise and "the maximal match" names nothing:
		// ranking by cardinality here would prefer a more-scoped binding for a
		// reference that asked for none, and an equal-cardinality tie would start
		// raising ErrAmbiguousBinding on the path that mints EVERY GlobalIndex
		// pin (NewGlobalIndex / newResolvedGlobalIndex above). Highest tier, first
		// live slot, never ambiguous is a DIFFERENT rule that happens to live in
		// the same function, and it is the pre-fold walk's layer-major
		// first-live behavior. TestResolveRankedWildcard pins it.
		bestTier := tierNone
		for _, s := range slots {
			if s.slot >= len(p.bindings) || p.bindings[s.slot] == nil {
				continue
			}
			t := tierOf(s)
			if t < minTier || t > tierHighest {
				continue
			}
			if bestTier < 0 || t < bestTier {
				bestTier = t
				ref = s
			}
		}
		return ref, bestTier, false, bestTier >= 0
	}
	scopes := q.Scopes()
	best := newRankedArgmax()
	for _, s := range slots {
		if s.slot >= len(p.bindings) || p.bindings[s.slot] == nil {
			continue
		}
		t := tierOf(s)
		if t < minTier || t > tierHighest {
			continue
		}
		bindingScopes := p.bindings[s.slot].Scopes()
		if !syntax.ScopesCompatible(bindingScopes, scopes) {
			continue
		}
		if best.consider(t, bindingScopes) {
			ref = s
		}
	}
	return ref, best.tier, best.ambiguous, best.found()
}

// bulkTierOf classifies a bulk row at the query phase, as probeTiersLocked's
// tierOf does a slot. A row is declared at ONE phase and is a candidate there
// alone: a row that answered at every phase would be the deleted ambient tier
// under another name, which is the thing this stage exists to prevent, and
// cross-phase reach is expressed by declaring a row at each phase that should
// have it.
//
// The classifier stays split by KIND while rankedArgmax ranks what it returns,
// and since 2026-09-10 the two produce the SAME three tiers. They used to
// diverge on one: a row could not rank tierExactImported, because that tier read
// a per-slot provenance stamp (Binding.IsImported) and a bulkRef carried no
// provenance for the arm to test. The divergence was a missing FACT, not a
// missing branch, and bulkRef.origin is that fact.
//
// The two classifiers still ask provenance differently, and the difference is
// not an accident to be unified away. tierOf reads a MUTABLE bit off the winning
// binding, which the R7RS §5.3.1 supersede rule clears; origin is fixed at
// install. A row stands for many bindings and cannot read any one of their
// stamps, so provenance has to be the ROW's own.
//
// No production row is BulkOriginImport today — imports take per-symbol slots —
// so the new arm is reachable only from a test. It is not dead: the arm is what
// makes "a row ranks like the slot it stands for" true by construction rather
// than by the coincidence that every row happened to be the language's.
//
// A new tier added to the enum must still be derivable from a bulkRef's own
// fields or be documented here as unreachable for rows, or the two walks
// silently start ranking the same candidate differently again. A new BulkOrigin
// needs the same care from the other side: the default arm swallows every origin
// it does not name, so a third one ranks tierExactSealed until an arm says
// otherwise.
func bulkTierOf(row bulkRef, phase Phase) int {
	switch {
	case row.phase != phase:
		return tierNone
	case !row.sealed:
		return tierExactMutable
	case row.origin == BulkOriginImport:
		return tierExactImported
	default:
		return tierExactSealed
	}
}

// probeBulkLocked finds the best bulk row supplying key at phase, under the same
// (tier, scope cardinality) argmax the per-symbol probe uses — literally the
// same, since 2026-09-10: rankedArgmax is one body, and an incomparable
// equal-cardinality tie among rows is now reported as ambiguous exactly as it is
// among slots.
//
// Among rows at equal tier and equal scope set the FIRST INSTALLED wins. It used
// to be the last, on the argument that install order let an explicit
// (import (scheme base)) shadow the language's own initial import of the same
// name; that argument does not survive inspection, because every live row is a
// storeBulkSource over ONE store at PhaseRuntime, so two tying rows return the
// identical *Binding from lookupExportSameStore and materializeBulkLocked keys
// on selfStore rather than on the winning row. The order was therefore never
// observable, and first-installed is what the slot walk does.
//
// That ordering is NOT the R7RS section 5.6 conflict rule, and does not pretend
// to be. Conflict detection runs on the per-symbol install path
// (installImportedBinding's importConflicts, machine/compilation/library_bindings.go),
// where an import that would collide with a different existing binding of the
// name is refused before the slot is written. There is no eager row-level check:
// one was written, never wired to a caller, and deleted rather than left
// standing as a claim nothing honoured. Two rows from DIFFERENT libraries
// supplying one name at one coordinate would therefore be resolved by install
// order here rather than refused. That case does not arise: routing an import
// through a row was settled against on 2026-09-10, so no row is ever a library's
// import and every row is the language's own declaration. A future change that
// installs an import as a row (BulkOriginImport) owes the check.
//
// The QUERY wildcard skips the scope FILTER but not the cardinality ranking,
// which is the one place this walk still differs from probeTiersLocked's: the
// slot walk answers a wildcard with tier alone (see its q.IsAll() branch). The
// divergence is unobservable while every row carries the empty scope set —
// pinned by TestBulkRowsCarryTheEmptyScopeSet in pkg/wile — and the row that
// breaks the premise is the one that will have to settle it.
//
// Caller MUST hold at least a read lock on p.mu. This function does not panic;
// resolveRankedLocked raises the tie it reports, as probeRankedLocked does for
// slots, and BulkBindingAt returns it as an answer.
func (p *GlobalEnvironmentFrame) probeBulkLocked(key values.Symbol, q syntax.ScopeSet, phase Phase, minTier int) (row bulkRef, bnd *Binding, ambiguous bool, ok bool) {
	best := newRankedArgmax()
	for _, r := range p.bulkRows {
		t := bulkTierOf(r, phase)
		if t < minTier || t > tierHighest {
			continue
		}
		if !q.IsAll() && !syntax.ScopesCompatible(r.scopes, q.Scopes()) {
			continue
		}
		// A row over THIS store whose source phase is the query phase cannot add
		// anything: the per-symbol probe just looked at exactly those slots and
		// missed. Skipping it is not an optimization on the margin — the default
		// dialect declares the base at phase 0 over its own store, so EVERY
		// phase-0 miss would otherwise pay a full nested probe of the same store
		// at the same phase, guaranteed to miss again. Measured at +6% engine
		// startup before this check.
		//
		// The row still earns its place as a DECLARATION — it is what says the
		// base is the language at phase 0, and the origin ratchet counts it — it
		// simply has nothing to contribute to a resolution that already failed.
		src, srcPhase, isSelf := selfStore(r.src)
		if isSelf && src == p && srcPhase == phase {
			continue
		}
		b, found := lookupExportSameStore(r.src, key)
		if !found {
			continue
		}
		if best.consider(t, r.scopes) {
			row = r
			bnd = b
		}
	}
	return row, bnd, best.ambiguous, best.found()
}

// probeRankedLocked is probeTiersLocked over every tier, raising on an
// incomparable tie in the winning tier as werr.ErrAmbiguousBinding (P8). Every
// reader except the literal pin's two comes through here.
//
// Caller MUST hold at least a read lock on p.mu, and MUST release it via defer
// rather than a bare RUnlock: this can panic mid-hold.
func (p *GlobalEnvironmentFrame) probeRankedLocked(key values.Symbol, q syntax.ScopeSet, phase Phase, minTier int) (ref slotRef, tier int, ok bool) {
	ref, tier, ambiguous, ok := p.probeTiersLocked(key, q, phase, minTier)
	if ambiguous {
		panic(werr.WrapForeignErrorf(werr.ErrAmbiguousBinding,
			"resolveRankedLocked: identifier %q resolves ambiguously among incomparable hygienic scope sets",
			key.Key))
	}
	return ref, tier, ok
}

// resolveAtCoordsLocked resolves at EXACTLY the given coordinates — the write
// path's question ("the binding I just created"), never the read path's ranked
// question. Same scope discipline as one tier of resolveRankedLocked: subset
// compatibility, maximal cardinality wins, an incomparable tie is refused.
//
// Caller MUST hold at least a read lock on p.mu, via defer: this can panic
// mid-hold on an ambiguous tie.
func (p *GlobalEnvironmentFrame) resolveAtCoordsLocked(key values.Symbol, q syntax.ScopeSet, phase Phase, sealed bool) (int, bool) {
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
//
// It is provenance-blind at the sealed tier, exactly as healWriteLocked is, and
// for this caller set that is the RIGHT answer rather than a known gap: its two
// callers are SetDeferredGlobalValue, which passes sealed == false and so never
// reaches the tier where the base and an import share a coordinate, and
// SetOwnGlobalValue's deferred arm, which takes the WRITING view's coordinates —
// and the view that writes sealed at phase 0 is the base's own writer, which is
// the slot it should reach. An import writes through the pin
// CreateImportedGlobalBindingAt hands back, never through here.
// Thread-safe: uses full Lock for write access.
func (p *GlobalEnvironmentFrame) setValueAtCoords(key *values.Symbol, q syntax.ScopeSet, phase Phase, sealed bool, v values.Value) error {
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

// SealedBindingAt returns what the STARTUP SET bound key to under q at phase,
// regardless of any user shadow AND regardless of any import. nil means NONE —
// the startup set holds no binding of that name visible from phase.
//
// The floor is tierExactSealed and the ranked ceiling, tierHighest, is that
// same tier today, so the admitted range is exactly ONE tier. That skips more
// than the mutable one: tierExactImported is below the floor too, so an import
// of the name is not a candidate here. Note which half is load-bearing: the
// FLOOR is this reader's question, the ceiling merely happens to coincide with
// it, so a tier appended to the enum widens this probe and whoever appends it
// must decide whether that is wanted. Callers that want "whatever this namespace binds the name
// to", imports included, want GetBinding's full ranked probe instead — see
// setRecognizedPrimitive (registry/core/prim_hashtables.go), which runs both and
// depends on the difference.
//
// It is the store form of the pre-fold "read the sealed base frame directly"
// fallback (setRecognizedPrimitive): with one merged store there is no narrower
// frame to address, so the narrowing is a tier floor on the probe.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) SealedBindingAt(key *values.Symbol, q syntax.ScopeSet, phase Phase) *Binding {
	p.mu.RLock()
	defer p.mu.RUnlock()

	ref, _, ok := p.probeRankedLocked(*key, q, phase, tierExactSealed)
	if !ok {
		return nil
	}
	return p.bindings[ref.slot]
}

// bindingWithinTiers is the locked, binding-returning form of probeTiersLocked:
// the resolved binding among tiers minTier..tierHighest at phase, or nil, and
// whether the winning tier tied. It does not raise. ExactBindingAt is its one
// caller.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) bindingWithinTiers(key *values.Symbol, q syntax.ScopeSet, phase Phase, minTier int) (bnd *Binding, ambiguous bool) {
	p.mu.RLock()
	defer p.mu.RUnlock()
	ref, _, ambiguous, ok := p.probeTiersLocked(*key, q, phase, minTier)
	if ambiguous {
		return nil, true
	}
	if !ok {
		return nil, false
	}
	return p.bindings[ref.slot], false
}

// ExactBindingAt resolves key under q among this store's OWN slots at phase —
// (phase, mutable), then the two sealed tiers — reporting an incomparable tie as
// an answer rather than raising it. Bulk rows are not candidates: (nil, false)
// means no slot of the name resolves under q at phase, and says nothing about
// what a row supplies there, which BulkBindingAt answers.
//
// It exists for the R7RS §4.3.2 literal pin (compilation.lookupLiteralBinding),
// and the SEPARATION is the whole reason it is not GetBinding. The pin's descent
// ranks per-symbol slots at several phases above whatever the language itself
// supplies, because a phase-1 probe for `else` must not answer the keyword
// before the descent has looked at phase 0 for a use-site shadow. The ranked
// probe — highest tier at the query phase wins, rows consulted on a miss at that
// same phase — cannot express an ordering that spans phases, so the pin runs the
// two halves itself and this is the first of them. It also carries a tie forward
// as an answer rather than unwinding the descent at the first incomparable pair.
// Every other reader wants GetBinding, which raises.
// Thread-safe: uses RLock for read-only access (taken in bindingWithinTiers).
func (p *GlobalEnvironmentFrame) ExactBindingAt(key *values.Symbol, q syntax.ScopeSet, phase Phase) (bnd *Binding, ambiguous bool) {
	return p.bindingWithinTiers(key, q, phase, tierExactMutable)
}

// SealedGlobalIndexAt is SealedBindingAt's PIN: the same tier-floored probe, but
// returning the index of the slot it landed on rather than the binding there.
// nil means NONE.
//
// The two answer different questions. A caller that only wants to read the
// startup set's value now takes the binding; a caller that wants to RECORD that
// resolution for a later compile — a synthesized reference pinned to its
// definition site, which is what the quasiquote expansion's list/cons/append
// heads need — takes the index, because a pin re-resolves inside its own query
// if its slot is later deleted while a bare *Binding cannot.
//
// The query is recorded rather than widened to the wildcard: the sealed startup
// registrations are unscoped, so q is the empty set here, and re-resolution must
// stay inside that hygiene key rather than matching any scope set of the name.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) SealedGlobalIndexAt(key *values.Symbol, q syntax.ScopeSet, phase Phase) *GlobalIndex {
	p.mu.RLock()
	defer p.mu.RUnlock()

	ref, _, ok := p.probeRankedLocked(*key, q, phase, tierExactSealed)
	if !ok {
		return nil
	}
	return newScopeKeyedGlobalIndex(key, p, ref, q)
}

// ImportedBindingAt returns the binding key resolves to under q at phase when the
// MUTABLE tier is skipped and the answer is an IMPORT: what an import bound this
// name to, regardless of any user shadow above it. nil means NONE.
//
// It is SealedBindingAt's sibling, one tier down, and the pair is the reason the
// imported tier exists as a coordinate rather than as a meta bit: "the startup
// set's binding of this name" and "an import's binding of this name" are two
// questions with two answers, and before the split they had to share one probe
// and be told apart afterwards by reading Meta.Imported off whatever came back.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) ImportedBindingAt(key *values.Symbol, q syntax.ScopeSet, phase Phase) *Binding {
	p.mu.RLock()
	defer p.mu.RUnlock()

	ref, tier, ok := p.probeRankedLocked(*key, q, phase, tierExactImported)
	if !ok || tier != tierExactImported {
		return nil
	}
	return p.bindings[ref.slot]
}

// IsImportedBindingAt reports whether a read of key under q at phase resolves to
// the IMPORTED tier: sealed-coordinate, but installed by an import rather than by
// the startup set.
//
// It is the tier form of a question the tree used to ask as
// SealedBindingAt(...).IsImported(), which stopped working the moment the sealed
// floor started excluding imports — and which was never quite the same question,
// because it asked about whatever the SEALED probe returned rather than about
// what the name actually denotes here.
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) IsImportedBindingAt(key *values.Symbol, q syntax.ScopeSet, phase Phase) bool {
	p.mu.RLock()
	defer p.mu.RUnlock()

	_, tier, ok := p.probeRankedLocked(*key, q, phase, tierExactMutable)
	return ok && tier == tierExactImported
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
// sealed) are different variables. That is what makes a phase-0 define a SHADOW
// of the sealed entry (new slot) rather than a write through it, and what makes
// a define-for-syntax over the registry's (1, sealed) expand copy a shadow for
// the same reason, at the phase above. Scope equality alone was sufficient only
// while coordinates were frame identity.
//
// There is no phase-blind coordinate to refuse any more: phase is a bare Phase,
// so every write names one phase and is a candidate at that phase alone.
//
// The returned index is PINNED to the slot this call landed on, created or
// reused, carrying the creation scope set as its re-resolution query. Callers
// may write through it directly: it needs no paired re-resolve, and unlike a
// bare-name index it cannot drift onto a different slot of the same name. See
// the history note below for why it was deferred until 2026-08-06.
func (p *GlobalEnvironmentFrame) CreateGlobalBindingAt(key *values.Symbol, bt BindingType, scopes []*syntax.Scope, phase Phase, sealed bool) (*GlobalIndex, bool) {
	return p.createGlobalBindingAt(key, bt, scopes, phase, sealed, false)
}

// CreateImportedGlobalBindingAt is CreateGlobalBindingAt for an IMPORT's install:
// the created slot is stamped Imported before it is published, and the reuse rule
// refuses to hand back a slot the import did not create.
//
// Both halves are load-bearing and neither is an optimization.
//
// The reuse refusal is the S0 repair. An import writes (phase 0, sealed)
// with an EMPTY scope set; the startup set's own binding sits at that coordinate
// with NIL scopes; scopeSetsEqual(nil, []) is true, so the plain form returned
// the BASE's slot with created == false and the caller then wrote its value and
// its provenance onto the startup set. Measured: `(import (scheme base))` left
// one slot per name, replaced the base's value with the library env's copy, and
// stamped the engine-shared base as imported.
//
// The pre-publication stamp is what makes the refusal stable. tierOf reads
// IsImported() to rank, so a slot that is created now and stamped later is a slot
// that ranks as the startup set in between. Nothing reads it in that window
// today, but the window has no reason to exist.
func (p *GlobalEnvironmentFrame) CreateImportedGlobalBindingAt(key *values.Symbol, bt BindingType, scopes []*syntax.Scope, phase Phase, sealed bool) (*GlobalIndex, bool) {
	return p.createGlobalBindingAt(key, bt, scopes, phase, sealed, true)
}

// createGlobalBindingAt is the shared body. imported selects both the reuse
// predicate and the created slot's provenance stamp, so the two cannot disagree.
func (p *GlobalEnvironmentFrame) createGlobalBindingAt(key *values.Symbol, bt BindingType, scopes []*syntax.Scope, phase Phase, sealed bool, imported bool) (*GlobalIndex, bool) {
	// There is no runtime refusal of the ambient coordinate here any more, and
	// nothing replaces it.
	//
	// Stage A deleted the ambient tier and this function grew a panic to keep the
	// deletion a property of the STORE rather than a convention: the coordinate
	// was still constructible, so a caller could still mint a slot nothing ranked
	// and nothing could read. Stage B removed the coordinate itself — phase is a
	// bare Phase — so the refusal became a check against a value that cannot be
	// built, with no argument left to pass. The invariant it guarded is now
	// carried by the type, which is why it is gone rather than reworded.
	//
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
		// Provenance is half of the coordinate AT THE SEALED TIER ONLY: an import
		// and the startup set share (phase, sealed, scopes) at phase 0 and must not
		// share a slot.
		//
		// The mutable tier is deliberately exempt, and the exemption is not a
		// concession — reuse there IS the supersede rule. A define-syntax that
		// supersedes an imported macro reuses the import's (phase 1, mutable) slot
		// and clears its provenance (R7RS §5.3.1,
		// TestDefineSyntaxSupersedesImportClearsImported), and a second import of a
		// name reuses the first's so the last import wins
		// (TestImportedMacroDocTracksTheWinningValue). Both go red if this refusal
		// reaches the mutable tier, because a refusal to reuse mints a second slot
		// and the ranking then keeps the FIRST at equal tier.
		//
		// Nothing the startup set writes is mutable, so at the mutable tier there is
		// no base to be confused with and the distinction buys nothing.
		if sealed && p.bindings[s.slot].IsImported() != imported {
			continue
		}
		if scopeSetsEqual(p.bindings[s.slot].Scopes(), scopes) {
			q := newScopeKeyedGlobalIndex(key, p, s, syntax.ScopesOf(scopes))
			return q, false
		}
	}
	i := len(p.bindings)
	ref := slotRef{slot: i, phase: phase, sealed: sealed}
	p.keys[*key] = append(p.keys[*key], ref)
	p.noteExactPhaseLocked(phase)
	// append the new binding at index i. Global bindings carry an atomicCell so
	// they can be read lock-free from other threads (see binding.go atomicCell).
	bnd := newGlobalBinding(values.Void, bt, scopes)
	if imported {
		// Stamped BEFORE publication: tierOf ranks on this, so the slot must never
		// be visible carrying the wrong tier. The caller's markBindingImported
		// still runs and adds the origin and export names; this sets only the bit
		// the coordinate depends on.
		bnd.UpdateMeta(func(m *BindingMeta) bool {
			m.Imported = true
			return true
		})
	}
	p.bindings = append(p.bindings, bnd)
	q := newScopeKeyedGlobalIndex(key, p, ref, syntax.ScopesOf(scopes))
	return q, true
}

// HISTORY (2026-07-19, resolved 2026-08-06): returning the pin above is task
// C2b. It was tried once and REVERTED, because the macro path then depended on
// two errors cancelling: define-syntax wrote the transformer through the
// DEFERRED index this used to return (wildcard, landing on the name's FIRST slot
// rather than the one just created) and lookupMacroBinding read it back
// wildcard, finding the same wrong slot. Pinning fixed the write alone, so the
// read went looking in the right place and found nothing — `(chibi diff)` failed
// to load with `no such binding "let*-to-let" with compatible scopes`.
//
// Both halves of that coupling were closed independently afterwards.
// compile_define_syntax and expander_body pair the create with an explicit
// OwnGlobalIndex re-resolve at the writing view's coordinates; lookupMacroBinding
// arm 1 resolves under the reference's own scope set instead of nil. With neither
// side wildcard the pin lands cleanly. Re-measured on the whole tree, green
// including TestChibi{Optional,Diff}Loads — the tests that caught the original
// break, and still the cheapest sensor for it.
//
// The paired re-resolves those two sites carried were redundant rather than
// load-bearing once the pin landed, and were collapsed onto this return in a
// follow-up. NO production caller now pairs a create with a second lookup: the
// remaining OwnGlobalIndex calls all ask about a slot they did not just write.
//
// Pairing them asks the same question twice, and the second answer cannot
// differ: this call's postcondition is a slot whose scopes are EXACTLY the
// creation set at those coordinates, and every candidate the re-resolve's subset
// predicate admits has cardinality at most that, so the created slot is the
// unique maximum and scopedBestOf's early exit reaches it. What it does add is a
// window in which a concurrently compiling thread can append or delete a slot of
// that name, plus a lock acquisition. DefineOwnGlobal returns this pin for the
// same reason — a caller that wants to stamp what it just defined takes it from
// there rather than resolving the name again.

// GetOwnGlobalBinding returns the binding for the given GlobalIndex from this
// store only. Unlike EnvironmentFrame.GetGlobalBinding it resolves nothing
// against the live environment: a PINNED index addresses its slot, and a stale
// pin re-heals through healReadLocked. A DEFERRED index (Env == nil) has no
// pinned slot and no coordinates, so it misses here — every production caller
// passes an index from OwnGlobalIndex or GetGlobalIndexWithScopes, and the VM
// routes a deferred one to EnvironmentFrame.GetGlobalBinding instead
// (machine_context.go, resolveGlobalBinding).
// Thread-safe: uses RLock for read-only access.
func (p *GlobalEnvironmentFrame) GetOwnGlobalBinding(gi *GlobalIndex) *Binding {
	p.mu.RLock()
	defer p.mu.RUnlock()

	i, ok := p.pinnedSlotLocked(gi)
	if !ok {
		i, ok = p.healReadLocked(gi)
	}
	if !ok {
		return nil
	}
	return p.bindings[i]
}

// pinnedSlotLocked resolves a GlobalIndex through its pinned (Env, Slot) pair,
// which addresses the binding directly with no re-hash of the symbol.
//
// The emptiness check is load-bearing, not defensive. DeleteBindingAt nils a slot
// but leaves it in range, so a bounds check alone would hand back a nil binding
// where the name-keyed lookup this replaced would have missed and reported "no
// such binding". Falling through to a heal on a nil slot also restores the
// self-healing the name lookup gave for free: an index pinned before a
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
// The healWriteLocked fallback below is therefore NOT the non-pinned write path;
// it is the STALE-pin self-heal that pinnedSlotLocked documents — reached only
// when the pinned slot has been nil'd by a delete. It re-resolves at the pin's
// own coordinates, which is what keeps a re-heal from becoming an escalation: a
// pin emptied by namespace-undefine! can reach neither the SEALED slot of the
// same name underneath it nor that name's copy at another phase, both of which a
// coordinate-blind lookup could reach and no other write path can.
//
// The error is ErrNoSuchBinding whether the name is absent entirely or merely
// absent at the pin's coordinates. Those are different facts; the message names
// the coordinates so the distinction survives to the reader.
//
// Thread-safe: uses full Lock for write access.
func (p *GlobalEnvironmentFrame) SetOwnGlobalValue(gi *GlobalIndex, v values.Value) error {
	p.mu.Lock()
	defer p.mu.Unlock()

	i, ok := p.pinnedSlotLocked(gi)
	if !ok {
		i, ok = p.healWriteLocked(gi)
	}
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNoSuchBinding,
			"SetOwnGlobalValue: no global binding %q at phase %v, sealed=%t",
			gi.Index.Key, gi.phase, gi.sealed)
	}
	// Publish atomically through the binding's cell so the lock-free
	// cachedBindings reader (Binding.Value with no frame mutex) never tears the
	// two-word interface. The frame Lock still serializes writers.
	p.bindings[i].SetValue(v)

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
// query — the literal call UnscopedKeysAt and GetGlobalIndexWithScopes make — so
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
// At a SEALED coordinate it is provenance-BLIND: an import and the startup set
// share (phase, sealed, scopes) at phase 0, and this picks whichever the name's
// slot list holds first, which is the base's. "Remove the import" is
// DeleteImportedBindingAt, the tier-addressed sibling.
//
// Two of the three DeleteOwnGlobal callers DO arrive here sealed, and the
// blindness is inert at the coordinate they reach. The transformer rollbacks
// (compile_define_syntax.go, expander_body.go) delete through p.env.NextPhase(),
// which AtPhase's §4.5 inheritance arm resolves to the phase-1 SEALED-WRITE view
// whenever the defining view is sealed — a bootstrap macro whose right-hand side
// failed to compile — so they address (phase 1, sealed). The third,
// namespace-undefine!, goes through Runtime(), the mutable phase-0 root, and
// never arrives sealed at all.
//
// Nothing an import owns sits at (phase 1, sealed) today. Every install
// above phase 0 takes the importing VIEW's own coordinates — placementInPlace on
// both propagation paths, and placementShadowable's non-phase-0 fallback — so an
// import lands sealed there only if the IMPORTING view is itself sealed-write.
// Whether one ever is was NOT established; if it can be, this needs the
// provenance axis its sibling has.
//
// It does not refuse a sealed coordinate, and nothing downstream depends on its
// declining to reach one. The write-side self-heal re-resolves at the PIN's own
// coordinates (healWriteLocked), so the worst a sealed delete can do is re-heal
// a sealed pin onto a sealed slot — the coordinate it already addressed. An
// earlier form of this note recorded the opposite, because SetOwnGlobalValue
// then re-resolved by name and leaned on an audited "no caller passes a sealed
// coordinate" premise to stay off the sealed tier. That filter is gone.
//
// Thread-safe: uses full Lock for write access.
func (p *GlobalEnvironmentFrame) DeleteBindingAt(sym *values.Symbol, scopes []*syntax.Scope, phase Phase, sealed bool) bool {
	p.mu.Lock()
	defer p.mu.Unlock()

	i, ok := p.resolveAtCoordsLocked(*sym, syntax.ScopesOf(scopes), phase, sealed)
	if !ok {
		return false
	}
	p.removeSlotLocked(*sym, i)
	return true
}

// DeleteImportedBindingAt removes the binding AN IMPORT installed for key under q
// at phase, and never the startup set's at the same coordinate. Returns true if
// one was found and removed.
//
// It is DeleteBindingAt's provenance-aware sibling, and it exists because
// (phase, sealed, scopes) stopped naming one slot when imports got a tier of
// their own. DeleteBindingAt resolves through resolveAtCoordsLocked, which
// filters on (phase, sealed) alone; an import and the startup set agree on both,
// their scope sets are both empty, so the scoped walk returns whichever sits
// FIRST in p.keys[key] — the base's, created at bootstrap. Measured: undefining
// an imported `car` nil'd the startup set's slot and left the import standing, so
// a second undefine then unbound a name a single one refuses.
//
// The axis is sealed-tier-only, exactly as in createGlobalBindingAt: this is the
// delete counterpart of CreateImportedGlobalBindingAt, not a general third
// coordinate. At the mutable tier there is no base to be confused with, and reuse
// there IS the R7RS §5.3.1 supersede rule.
//
// Resolution is the ranked probe floored at tierExactImported — ImportedBindingAt's
// probe, which is the one that can tell the two sealed slots apart — pinned to a
// winner AT that tier. The floor plus the pin is what keeps this a provenance
// filter rather than the ranked delete DeleteBindingAt's doc refuses: a mutable
// shadow is below the floor and the startup set is above the pin, so no
// reachable answer here is anything but an import.
// Thread-safe: uses full Lock for write access.
func (p *GlobalEnvironmentFrame) DeleteImportedBindingAt(key *values.Symbol, q syntax.ScopeSet, phase Phase) bool {
	p.mu.Lock()
	defer p.mu.Unlock()

	ref, tier, ok := p.probeRankedLocked(*key, q, phase, tierExactImported)
	if !ok || tier != tierExactImported {
		return false
	}
	p.removeSlotLocked(*key, ref.slot)
	return true
}

// removeSlotLocked is the removal half every coordinate- or tier-addressed delete
// shares, once its own resolution has named a slot: nil the slot, prune the dead
// index, drop the name when it owns no more.
//
// Caller MUST hold the write lock on p.mu.
func (p *GlobalEnvironmentFrame) removeSlotLocked(sym values.Symbol, i int) {
	// Nil out the slot so a re-resolving GlobalIndex reference (OpLoadGlobal /
	// OpPushGlobal) sees nil, caught by resolveGlobal, instead of the old value.
	// NOTE: this does NOT reach compiled code that captured the *Binding pointer
	// itself at compile time (OpLoadCachedBinding / OpPushCachedBinding /
	// OpCallCachedBinding read cachedBindings[i].Value() directly, bypassing the
	// slot), so a closure over an undefined name can still read its last value.
	// Making cached reads observe deletion needs a per-read check in those hot
	// opcodes; see TODO.md "namespace-undefine! does not stop compiled code".
	p.bindings[i] = nil
	// Prune the dead index rather than leaving it in place: every walker over
	// p.keys (probeRankedLocked, CreateGlobalBindingAt's dedup, LiveSlots/SealedSlots)
	// already skips a nil'd slot defensively, so leaving one in place would not
	// corrupt a lookup — but an unpruned list grows without bound across repeated
	// delete/redefine cycles, and every one of those walkers rescans the whole
	// list. Pruning keeps it sized to the name's LIVE slots.
	slots := p.keys[sym]
	for j, s := range slots {
		if s.slot != i {
			continue
		}
		p.keys[sym] = append(slots[:j], slots[j+1:]...)
		break
	}
	// Drop the name once it owns no slots, so a future lookup on it is a plain
	// map miss and UnscopedKeysAt / LiveSlots / SealedSlots stop enumerating it.
	if len(p.keys[sym]) == 0 {
		delete(p.keys, sym)
	}
}
