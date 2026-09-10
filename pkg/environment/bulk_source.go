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
	"iter"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// BulkSource is one supplier of many names, seen by resolution as a SINGLE
// candidate.
//
// It is Racket's bulk-binding-at (syntax/binding-table.rkt): an import installs
// one row, not one entry per exported name, so the cost of `(import (scheme
// base))` is one row rather than 248 per-symbol mints, and the row's identity
// is the import rather than the names it happens to carry.
//
// The interface takes NO phase parameter, and that is the whole of D11: a
// source is minted per (store, phase) and closes its phase over at
// construction. The alternative — one source per store, phase passed per
// lookup — cannot express the case that motivates the split, a name bound at
// two phases of one store (syntax-rules is the measured example: a phase-0
// SyntaxCompiler and a phase-1 PrimitiveExpander). With the phase in the
// signature, "which of the two?" would be answered at every call site; with it
// in the identity, it is answered once, where the row is installed.
//
// SourceName returns a datum, never a Go pointer, because Stage C serializes a
// row into a pre-compiled binary and a pointer does not survive that. It is the
// row's only durable identity, and the R7RS section 5.6 import-conflict
// diagnostics name both libraries through it.
type BulkSource interface {
	// LookupExport answers for one name. The returned *Binding belongs to the
	// SOURCE, not to whoever resolves through the row.
	LookupExport(name values.Symbol) (*Binding, bool)
	// ExportNames enumerates what the source supplies. For diagnostics and
	// conflict detection; resolution never walks it.
	ExportNames() iter.Seq[values.Symbol]
	// SourceName identifies the source for diagnostics and for Stage C's
	// serialization.
	SourceName() values.Value
}

// BaseSourceName is the reserved library-name datum the engine's own base
// carries as its BulkSource identity.
//
// Design section 4.1 declines to create a (wile base) .sld: LoadBootstrapCore
// writes the base directly into each owner's store, so the base STORE is the
// source and there is nothing for an .sld to declare. The base therefore has no
// importable name — (import (wile base)) fails, deliberately — and a procedural
// transformer reaches the same names through (scheme base) plus (scheme cxr)
// instead. That is Fork 4's answer (4a), and it is Racket's layering: idiomatic
// code writes (require (for-syntax racket/base)), never (require (for-syntax
// '#%kernel)), even though the latter works.
//
// The consequence, stated rather than discovered later: ORIGIN and IMPORT
// denote different things. The dialect installs the base at its own coordinate
// under this name; a migrated file imports (scheme base). They overlap and are
// not one identity, so Stage C has to serialize a row whose source no file
// names.
//
// A datum, not a Go pointer, for the reason SourceName gives — and ONE datum,
// not a fresh one per call: two rows over the base must compare equal so
// conflict detection and Stage C's serialization see one identity, not N.
// The spelling is not a legal library name, so no .sld can claim it.
var baseSourceName = values.NewSymbol("#%wile-base")

// BaseSourceName returns that datum.
func BaseSourceName() values.Value {
	return baseSourceName
}

// storeBulkSource is a BulkSource over one owner's store at one phase.
//
// It holds the store by POINTER and reads through it on every lookup: a bulk
// row is a live reference, not a snapshot. That is the property the whole stage
// rests on — a row installed at engine origin, before LoadBootstrapCore has
// written anything, resolves the names that bootstrap adds afterwards, which is
// what makes the import edge order-independent. TestBulkRowIsLiveNotSnapshot
// pins it, because on a tree with no bulk rows a snapshot and a live reference
// are indistinguishable and the property would otherwise ship untested.
type storeBulkSource struct {
	store *GlobalEnvironmentFrame
	phase Phase
	name  values.Value
	// minTier is the tier FLOOR the source's lookup probes at, and it is what
	// makes "the base" mean the base.
	//
	// The engine's own base source must be SEALED-ONLY. Its store is the
	// namespace's own, whose phase-0 mutable tier holds the user's top-level
	// defines and its phase-0 imports; a source probing from tierExactMutable
	// would make every one of those visible at phase 1 through the row, which is
	// not "the base at another phase" but the collapse of phase hermeticity. It
	// was measured: with the floor at tierExactMutable a library-private name
	// imported at phase 0 resolved inside a transformer body, and both
	// TestPhase1_ProceduralTransformerBoundWithImport and
	// TestDeclarativeMacroNeedsNoImport went red.
	//
	// A source over a FOREIGN store — a library's exports — wants the full range,
	// because a library's own defines land in its mutable tier.
	minTier int
	// ownInstallsOnly excludes IMPORTED bindings from what the source supplies.
	//
	// It is REDUNDANT as of 2026-09-09 and kept deliberately. What it was standing
	// in for is now a coordinate: tierExactImported.
	//
	// This comment used to say "there is no third coordinate to move either onto",
	// and drew the base/import distinction by PREDICATE instead — an imported
	// binding carries Imported meta and the engine's own base does not. The
	// predicate was sound; the premise was not. Both landed on ONE slot, because
	// CreateGlobalBindingAt's reuse rule matched them on (phase, sealed, scopes)
	// and the import then stamped Imported onto the base's own binding, at which
	// point this predicate refused the base. See CreateImportedGlobalBindingAt.
	//
	// With the tier, minTier = tierExactSealed already excludes every import, so
	// this predicate can only ever agree with the floor. It stays because it is a
	// second, independent reason for the same answer and costs one field read on a
	// miss path; deleting it belongs with the rest of the origin work.
	//
	// Measured: without this, a library-private name imported at phase 0 resolves
	// inside a transformer body, because the phase-1 base row supplies it.
	ownInstallsOnly bool
}

// NewStoreBulkSource mints the source for one (store, phase).
//
// Mint EAGERLY, one per declared import, at origin. There is no cache and there
// must not be one: sources are minted per DECLARED import, so the set is fixed
// and known when the engine is built, and the unbounded phase axis is never
// reached. A lazy mint would put a map write on the store's READ path, which
// concurrent library loading reaches, and buy nothing.
func NewStoreBulkSource(store *GlobalEnvironmentFrame, phase Phase, name values.Value) BulkSource {
	q := &storeBulkSource{
		store:   store,
		phase:   phase,
		name:    name,
		minTier: tierExactMutable,
	}
	return q
}

// NewSealedStoreBulkSource is NewStoreBulkSource restricted to the SEALED tier:
// the shape the engine's own base takes.
//
// Use it for any source whose store is also the importing store. See
// storeBulkSource.minTier for what the unrestricted form leaks there.
func NewSealedStoreBulkSource(store *GlobalEnvironmentFrame, phase Phase, name values.Value) BulkSource {
	q := &storeBulkSource{
		store:           store,
		phase:           phase,
		name:            name,
		minTier:         tierExactSealed,
		ownInstallsOnly: true,
	}
	return q
}

// LookupExport resolves name in the source store at this source's phase.
//
// It asks at EXACTLY the source's phase, through the same ranked probe an
// ordinary read uses, so a name the source holds only at another phase is not
// supplied. That is what keeps two sources over one store at different phases
// answering differently, which is D11's whole content.
func (p *storeBulkSource) LookupExport(name values.Symbol) (*Binding, bool) {
	p.store.mu.RLock()
	defer p.store.mu.RUnlock()
	return p.lookupExportLocked(name)
}

// lookupExportLocked is LookupExport for a caller that already holds a read lock
// on THIS source's store.
//
// The split is not an optimization. Resolution consults rows while holding the
// importing store's read lock, and every row Stage A installs reads that same
// store, so the plain entry point would RLock a mutex the caller already holds.
// Go documents recursive read locking as forbidden — it deadlocks whenever a
// writer is queued between the two acquisitions — and the failure is
// load-dependent, so it would not show up reliably in a test run.
//
// Caller MUST hold at least a read lock on the source store's mu.
func (p *storeBulkSource) lookupExportLocked(name values.Symbol) (*Binding, bool) {
	ref, _, ok := p.store.probeRankedLocked(name, syntax.EmptyScopes(), p.phase, p.minTier)
	if !ok {
		return nil, false
	}
	if ref.slot >= len(p.store.bindings) {
		return nil, false
	}
	q := p.store.bindings[ref.slot]
	if q == nil {
		return nil, false
	}
	if p.ownInstallsOnly && q.IsImported() {
		return nil, false
	}
	return q, true
}

// ExportNames yields every name the source resolves at its phase.
//
// It filters through LookupExport rather than returning the store's whole key
// set, so the sequence agrees with what a resolution through this row would
// actually find. Callers are diagnostics and conflict detection; the resolution
// path never walks it.
func (p *storeBulkSource) ExportNames() iter.Seq[values.Symbol] {
	return func(yield func(values.Symbol) bool) {
		p.store.mu.RLock()
		keys := make([]values.Symbol, 0, len(p.store.keys))
		for k := range p.store.keys {
			keys = append(keys, k)
		}
		p.store.mu.RUnlock()

		for _, k := range keys {
			_, ok := p.LookupExport(k)
			if !ok {
				continue
			}
			if !yield(k) {
				return
			}
		}
	}
}

// SourceName returns the datum this source was minted with.
func (p *storeBulkSource) SourceName() values.Value {
	return p.name
}

// bulkRef is one installed bulk row: a resolution candidate standing for many
// names.
//
// It is NOT a mirror of slotRef, despite both carrying resolution coordinates,
// and the difference matters to Stage B. slotRef is {slot, phase, sealed} and
// has no scopes field at all — a per-symbol candidate's scope set is read off
// its *Binding, which is where probeTiersLocked gets it. A bulk row stands for
// many bindings with many different scope sets, so it must carry ONE set of its
// own: scopes here is a NEW axis, not a field slotRef also has. Stage B's "one
// fold covers both" obligation is correspondingly weaker than the design states
// — it covers phase and sealed, and must handle scopes separately.
type bulkRef struct {
	// scopes is the scope set a reference must be compatible with to resolve
	// through this row: the row's own axis, not read off any binding.
	scopes []*syntax.Scope
	phase  PhaseKey
	sealed bool
	src    BulkSource
}

// InstallBulkRow adds one bulk row to this store.
//
// The row is a resolution candidate immediately; nothing is copied out of the
// source, then or ever. Rows are installed at engine origin, before the base is
// written, which is only correct because the row reads through to the source
// live.
func (p *GlobalEnvironmentFrame) InstallBulkRow(src BulkSource, scopes []*syntax.Scope, phase PhaseKey, sealed bool) {
	p.mu.Lock()
	defer p.mu.Unlock()

	p.bulkRows = append(p.bulkRows, bulkRef{
		scopes: scopes,
		phase:  phase,
		sealed: sealed,
		src:    src,
	})
}

// BulkResolutionCount reports how many resolutions a bulk row has answered.
// See the field's comment for why it is a gate.
func (p *GlobalEnvironmentFrame) BulkResolutionCount() int64 {
	return p.bulkResolutions.Load()
}

// BulkBindingAt resolves key through this store's BULK ROWS ALONE at phase,
// consulting no per-symbol slot.
//
// It is the row-only counterpart of AmbientBinding, and it exists for the one
// reader that needs the tiers separated: lookupLiteralBinding's descent probes
// its own phase, then descending phases, then — last — whatever the language
// itself supplies. That ordering is load-bearing for auxiliary syntax, because a
// phase-1 probe for else must not answer the keyword before the descent has
// looked at phase 0 for a use-site shadow. Ambient used to be the last step;
// after Stage A the rows are.
//
// Reports a tie as an ANSWER rather than raising it, matching AmbientBinding:
// this reader carries ambiguity across a multi-phase descent instead of failing
// the compile at the first incomparable pair.
func (p *GlobalEnvironmentFrame) BulkBindingAt(key *values.Symbol, q syntax.ScopeSet, phase Phase) (bnd *Binding, ambiguous bool) {
	p.mu.RLock()
	defer p.mu.RUnlock()

	_, b, ok := p.probeBulkLocked(*key, q, phase, tierExactMutable, tierAmbientSealed)
	if !ok {
		return nil, false
	}
	return b, false
}

// MacroPhasesWithRows reports how many macro phases carry the vocabulary rows.
//
// The origin ratchet needs it to state its identity exactly: total rows equals
// declarations plus one vocabulary row per macro phase reached. Without it the
// ratchet would have to become a lower bound, and a lower bound would pass the
// per-exported-name install that bulk rows exist to replace.
func (p *GlobalEnvironmentFrame) MacroPhasesWithRows() int {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return len(p.macroPhasesSeen) * len(p.macroPhaseRows)
}

// BulkRowCount reports how many bulk rows this store holds.
//
// It exists for the origin ratchet (design section 6.3, half one): the number
// of rows installed at engine origin must EQUAL the dialect's declaration
// count. A change that installed rows and never consulted them, or consulted
// them and silently fell back to eager per-name copying, passes every
// behavioural test in the suite; the two counter ratchets are what catch it.
func (p *GlobalEnvironmentFrame) BulkRowCount() int {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return len(p.bulkRows)
}

// renamedBulkSource maps the importing unit's spellings onto the source's own.
//
// An import set is not always name-preserving: (rename ...), (prefix ...),
// (only ...) and (except ...) all mean the row must answer for a name the
// exporter never declared. Wrapping rather than teaching storeBulkSource about
// renaming keeps the live-reference property intact — the inner source still
// reads through to its store on every lookup — while making the mapping ONE
// per-import table rather than one slot per name, which is the whole point of a
// bulk row.
type renamedBulkSource struct {
	inner BulkSource
	// localToSource maps a local spelling to the exporter's internal name. It
	// also DELIMITS the row: a name absent from the map is not supplied, which
	// is how (only ...) and (except ...) are expressed.
	localToSource map[string]string
	name          values.Value
}

// NewRenamedBulkSource wraps inner so it answers under the importing unit's
// spellings. localToSource must not be mutated afterwards; the row keeps it.
func NewRenamedBulkSource(inner BulkSource, localToSource map[string]string, name values.Value) BulkSource {
	q := &renamedBulkSource{
		inner:         inner,
		localToSource: localToSource,
		name:          name,
	}
	return q
}

// LookupExport translates name and delegates.
func (p *renamedBulkSource) LookupExport(name values.Symbol) (*Binding, bool) {
	src, ok := p.localToSource[name.Key]
	if !ok {
		return nil, false
	}
	return p.inner.LookupExport(*values.NewSymbol(src))
}

// lookupExportLocked is LookupExport for a caller holding the inner source's
// store lock. See storeBulkSource.lookupExportLocked for why the split exists.
func (p *renamedBulkSource) lookupExportLocked(name values.Symbol) (*Binding, bool) {
	src, ok := p.localToSource[name.Key]
	if !ok {
		return nil, false
	}
	return lookupExportSameStore(p.inner, *values.NewSymbol(src))
}

// lookupExportSameStore dispatches to the lock-free lookup when src is one of
// the two in-tree sources over this store, and falls back to the locking entry
// point otherwise. A foreign source's store is a DIFFERENT mutex, so locking it
// is correct there.
func lookupExportSameStore(src BulkSource, name values.Symbol) (*Binding, bool) {
	switch v := src.(type) {
	case *storeBulkSource:
		return v.lookupExportLocked(name)
	case *renamedBulkSource:
		return v.lookupExportLocked(name)
	case *filteredBulkSource:
		return v.lookupExportLocked(name)
	default:
		return src.LookupExport(name)
	}
}

// ExportNames yields the LOCAL spellings, which is what a caller asking "what
// does this row supply?" means. Each is filtered through LookupExport so the
// sequence cannot over-report a name whose source binding has gone.
func (p *renamedBulkSource) ExportNames() iter.Seq[values.Symbol] {
	return func(yield func(values.Symbol) bool) {
		for local := range p.localToSource {
			sym := *values.NewSymbol(local)
			_, ok := p.LookupExport(sym)
			if !ok {
				continue
			}
			if !yield(sym) {
				return
			}
		}
	}
}

// SourceName returns the importing form's library name, not the inner store's:
// the R7RS section 5.6 diagnostics must name the library the program wrote.
func (p *renamedBulkSource) SourceName() values.Value {
	return p.name
}

// selfStore reports the store a source reads, when it reads exactly one.
//
// Materialization needs it to tell the two cases apart: a row over THIS store
// has nothing to materialize, because the binding is already a slot here and
// the row exists only to widen which phase can see it.
func selfStore(src BulkSource) (*GlobalEnvironmentFrame, Phase, bool) {
	switch v := src.(type) {
	case *storeBulkSource:
		return v.store, v.phase, true
	case *renamedBulkSource:
		return selfStore(v.inner)
	case *filteredBulkSource:
		return selfStore(v.inner)
	default:
		return nil, 0, false
	}
}

// BulkRowSupplying reports whether an installed row already supplies name at
// the given coordinate, and under which source name.
//
// It is the eager half of D5's conflict detection: two rows supplying one name
// at equal tier and equal scope set is a conflict, and the import path asks this
// BEFORE installing rather than letting resolution raise later. The
// resolution-time ErrAmbiguousBinding raise stays as a backstop, because a row
// can still collide with a slot installed by a path that never went through the
// import machinery.
func (p *GlobalEnvironmentFrame) BulkRowSupplying(name values.Symbol, phase PhaseKey, sealed bool, scopes []*syntax.Scope) (values.Value, bool) {
	p.mu.RLock()
	rows := make([]bulkRef, len(p.bulkRows))
	copy(rows, p.bulkRows)
	p.mu.RUnlock()

	for _, row := range rows {
		if row.phase != phase || row.sealed != sealed {
			continue
		}
		if !scopeSetsEqual(row.scopes, scopes) {
			continue
		}
		_, ok := row.src.LookupExport(name)
		if !ok {
			continue
		}
		return row.src.SourceName(), true
	}
	return nil, false
}

// filteredBulkSource admits only the names in a set.
//
// It is how a dialect declares a VOCABULARY rather than a whole store: the
// phase-1 and higher rows carry the macro-writing kernel, not the base, which is
// what makes D4's break real. A source that carried everything at phase 1 would
// install, rank, win — and leave the phase-distinctness break unobservable,
// which is the failure design section 6.3 says this change defaults to.
type filteredBulkSource struct {
	inner BulkSource
	// admits decides membership. A predicate rather than a set because the
	// vocabulary has an open arm — the bootstrap layer's %-prefixed private
	// helpers, which no import could reach and which move with their file.
	admits func(string) bool
	// enumerable is the closed part, for ExportNames. The open arm cannot be
	// enumerated, so a diagnostic that walks this sees the named members only,
	// which is the honest answer rather than a wrong one.
	enumerable map[string]struct{}
	name       values.Value
}

// NewFilteredBulkSource wraps inner so it supplies only names admits accepts.
// enumerable is what ExportNames yields; neither may be mutated afterwards.
func NewFilteredBulkSource(inner BulkSource, admits func(string) bool, enumerable map[string]struct{}, name values.Value) BulkSource {
	q := &filteredBulkSource{
		inner:      inner,
		admits:     admits,
		enumerable: enumerable,
		name:       name,
	}
	return q
}

// LookupExport answers only for an admitted name.
func (p *filteredBulkSource) LookupExport(name values.Symbol) (*Binding, bool) {
	if !p.admits(name.Key) {
		return nil, false
	}
	return p.inner.LookupExport(name)
}

// lookupExportLocked is LookupExport for a caller holding the inner source's
// store lock. See storeBulkSource.lookupExportLocked.
func (p *filteredBulkSource) lookupExportLocked(name values.Symbol) (*Binding, bool) {
	if !p.admits(name.Key) {
		return nil, false
	}
	return lookupExportSameStore(p.inner, name)
}

// ExportNames yields the admitted names the inner source can actually supply.
func (p *filteredBulkSource) ExportNames() iter.Seq[values.Symbol] {
	return func(yield func(values.Symbol) bool) {
		for k := range p.enumerable {
			sym := *values.NewSymbol(k)
			_, ok := p.LookupExport(sym)
			if !ok {
				continue
			}
			if !yield(sym) {
				return
			}
		}
	}
}

// SourceName returns the vocabulary's own identity, not the store's.
func (p *filteredBulkSource) SourceName() values.Value {
	return p.name
}

// InstallMacroPhaseRow records a row TEMPLATE installed at every macro phase
// (phase >= 1) the owner ever mints a view for, as it mints it.
//
// The tower is lazy and unbounded — Phase is an int8 and GetOrCreate makes a
// view for any of them — so "declare this at every macro phase" cannot be an
// enumeration and must not be a wildcard coordinate, which is the tier this
// stage deleted. Installing per view as the view appears is the third answer:
// every row that exists carries an EXACT phase, and the set of phases that exist
// is exactly the set the program reached.
func (p *GlobalEnvironmentFrame) InstallMacroPhaseRow(src BulkSource, scopes []*syntax.Scope, sealed bool) {
	p.mu.Lock()
	defer p.mu.Unlock()

	p.macroPhaseRows = append(p.macroPhaseRows, bulkRef{
		scopes: scopes,
		sealed: sealed,
		src:    src,
	})
	for phase := range p.macroPhasesSeen {
		p.installMacroRowLocked(phase, len(p.macroPhaseRows)-1)
	}
}

// EnsureMacroPhaseRows installs every recorded macro-phase template at phase, if
// it has not been installed there already. Called when a phase view is minted.
func (p *GlobalEnvironmentFrame) EnsureMacroPhaseRows(phase Phase) {
	if phase < PhaseExpand {
		return
	}
	// Lock-free fast path. Every AtPhase reaches here, and AtPhase sits on the
	// macro-compilation path, so taking even a read lock per call is a real cost:
	// measured, an RLock-and-map-probe here was a large share of a +7% startup
	// regression. The bitmask is one atomic load in the steady state, and a
	// non-negative Phase is an int8 so the whole domain is two words.
	if p.macroPhaseBits(phase) {
		return
	}

	p.mu.Lock()
	defer p.mu.Unlock()

	_, seen := p.macroPhasesSeen[phase]
	if seen {
		return
	}
	if p.macroPhasesSeen == nil {
		p.macroPhasesSeen = map[Phase]struct{}{}
	}
	p.macroPhasesSeen[phase] = struct{}{}
	for i := range p.macroPhaseRows {
		p.installMacroRowLocked(phase, i)
	}
	p.setMacroPhaseBit(phase)
}

// macroPhaseBits reports whether phase already carries the macro rows, without
// taking a lock. Negative phases are never recorded, matching the guard above.
func (p *GlobalEnvironmentFrame) macroPhaseBits(phase Phase) bool {
	if phase < 0 {
		return false
	}
	word := p.macroPhaseSeenBits[phase>>6].Load()
	return word&(1<<(uint(phase)&63)) != 0
}

// setMacroPhaseBit records phase in the lock-free set. Caller MUST hold the
// write lock, so the read-modify-write cannot lose an update.
func (p *GlobalEnvironmentFrame) setMacroPhaseBit(phase Phase) {
	if phase < 0 {
		return
	}
	i := phase >> 6
	p.macroPhaseSeenBits[i].Store(p.macroPhaseSeenBits[i].Load() | 1<<(uint(phase)&63))
}

// installMacroRowLocked materializes template i as a real row at phase.
// Caller MUST hold the write lock.
func (p *GlobalEnvironmentFrame) installMacroRowLocked(phase Phase, i int) {
	tpl := p.macroPhaseRows[i]
	p.bulkRows = append(p.bulkRows, bulkRef{
		scopes: tpl.scopes,
		phase:  ExactPhase(phase),
		sealed: tpl.sealed,
		src:    tpl.src,
	})
}
