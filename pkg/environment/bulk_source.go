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
	"github.com/aalpar/wile/pkg/werr"
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
	// Repoint returns this source reading store instead of the store it was
	// minted over, carrying every restriction across rather than rebuilding it.
	// GlobalEnvironmentFrame.Copy is the only caller.
	//
	// It is on the INTERFACE, not on *storeBulkSource, because a source is
	// wrapped as often as it is bare: the macro-vocabulary row is a filter over
	// a sealed store source, and a renaming import set is a rename over one.
	// Copy's re-point used to be a `row.src.(*storeBulkSource)` type assertion,
	// which silently skipped both wrappers and left a report environment's
	// vocabulary row pointed at the PARENT — installed, ranked, and INERT,
	// because materializeBulkLocked requires store == p. Dispatch makes
	// forwarding the compiler's obligation rather than the next wrapper
	// author's; the two package-level type switches over the same three types
	// (selfStore, lookupExportSameStore) are the shape that produced that bug.
	//
	// IMPLEMENTOR'S OBLIGATION, and it is not optional: return a source that
	// reads the GIVEN store and carries every field that affects ranking or
	// admission — a tier floor, a rename table, an admission predicate, the
	// source's phase and its name. A wrapper forwards to its inner source and
	// rebuilds itself around the result.
	//
	// An implementation that returns the receiver unchanged, or that drops a
	// restriction, is not merely imprecise: the row it belongs to goes SILENTLY
	// INERT in the copied store, because materializeBulkLocked requires
	// store == p, so the lookup finds the ORIGINAL store's *Binding and
	// resolution reports a miss. Nothing panics and no count changes —
	// BulkRowCount was equal across the Copy defect this method was added to
	// fix — so the failure surfaces only as a name that stopped resolving.
	Repoint(store *GlobalEnvironmentFrame) BulkSource
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
//
// ADDING A FIELD: Repoint rebuilds this struct by literal, so a field it does
// not name is dropped from every copied row. TestRepointCarriesEveryStoreBulkSourceField
// is the ratchet.
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
	//
	// It is the ONLY restriction a source carries, and that is deliberate. Until
	// 2026-09-10 a second field, ownInstallsOnly, refused a binding carrying
	// Imported meta — the same answer by PREDICATE that tierExactImported now
	// gives by COORDINATE, since an imported slot ranks BELOW this floor and is
	// excluded before the predicate could be reached. Two independent reasons for
	// one answer read as belt-and-braces and were really a coupling: the proof
	// that the predicate was dead held only because one constructor set both
	// fields in one struct literal, and Repoint carried them separately. One
	// field cannot drift from itself.
	minTier int
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
		store:   store,
		phase:   phase,
		name:    name,
		minTier: tierExactSealed,
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

// Repoint returns this source over store, CARRYING minTier rather than
// reconstructing it.
//
// Reconstructing is what Copy used to do, through NewStoreBulkSource — the
// unrestricted constructor. Measured on a report environment: the parent's row
// 0 (minTier=tierExactSealed, the shape NewSealedStoreBulkSource mints) came out
// as minTier=tierExactMutable. A source that is supposed to mean "the base" then
// supplies the copy's phase-0 MUTABLE tier at every macro phase, which is
// exactly the leak minTier's comment records as measured.
func (p *storeBulkSource) Repoint(store *GlobalEnvironmentFrame) BulkSource {
	q := &storeBulkSource{
		store:   store,
		phase:   p.phase,
		name:    p.name,
		minTier: p.minTier,
	}
	return q
}

// BulkOrigin says WHO installed a row: the language itself, or an import
// written in the unit the row is installed into. Its zero value names NEITHER,
// and both installers refuse it — see BulkOriginUnknown.
//
// It is the row's half of the base/import separation the per-symbol path draws
// with tierExactImported, and it is what lets bulkTierOf classify a row by the
// same rule tierOf classifies a slot. A row cannot derive it: a BulkSource is a
// supplier, and the SAME supplier can be reached both ways — the default dialect
// declares the base as an initial import (defaultInitialImports), so
// SourceName() == BaseSourceName() answers "is this the base?", which is a
// different question from "did an import put this row here?".
//
// Immutable after install. It rides in the bulkRef the three install paths build
// and there is no setter, deliberately: Binding.Imported is the other half of
// this distinction and is MUTABLE — the R7RS §5.3.1 supersede rule clears it —
// and a ranking input that a later compile can flip is exactly the hazard this
// coordinate must not inherit.
type BulkOrigin uint8

const (
	// BulkOriginUnknown is the ZERO VALUE and names no origin at all: a row that
	// carries it was never told who installed it.
	//
	// It is "nil means NONE" applied to an enum. The alternative — start the
	// enumeration at a real origin — makes the zero value a CLAIM, and a bulkRef
	// literal that forgets the field then asserts that claim silently. The claim
	// it used to assert was BulkOriginLanguage, which ranks tierExactSealed; that
	// is the lowest of the three tiers and so fails safe, but it fails safe by
	// answering a question nobody asked, and there is no test that can tell a
	// forgotten field from a deliberate one.
	//
	// Nothing can rank it, and nothing has to. InstallBulkRow and
	// InstallMacroPhaseRow REFUSE it at the door — along with every other value
	// outside the declared set, see BulkOrigin.valid — which is what keeps
	// bulkTierOf from having to invent a tier for a row whose provenance is
	// unstated: the installers own the question, so the classifier never meets
	// it. Refusing at install turns a silent misranking into a panic at the call
	// site that omitted the argument, one stack frame from the mistake.
	BulkOriginUnknown BulkOrigin = iota
	// BulkOriginLanguage is the dialect's own declaration: the base row and the
	// macro-vocabulary row installInitialImports installs at engine origin.
	BulkOriginLanguage
	// BulkOriginImport is an (import ...) written in the importing unit. No
	// production path installs one yet: R7RS imports still take per-symbol slots
	// (installImportedBinding), and routing them through rows was settled
	// against. The coordinate exists so the two classifiers agree on every tier,
	// rather than diverging on one a bulkRef could not express.
	BulkOriginImport
	// bulkOriginCount is one past the last origin. It is the exclusive upper
	// bound BulkOrigin.valid reads, which is what the two installers refuse on,
	// and it is also what the renumbering ratchet counts against:
	// TestBulkOriginValuesHaveNotRenumbered's value rows catch an INSERTION,
	// because every later constant shifts, and cannot catch an APPEND, because
	// nothing shifts. A constant that moves is the only thing that can.
	// tierCount plays exactly this part for the tier enum.
	bulkOriginCount
)

// valid reports whether p is one of the origins this package declares: strictly
// above the zero value, strictly below the count.
//
// It exists because BulkOrigin is EXPORTED and both installers take it
// positionally, so an out-of-tree caller can hand over BulkOrigin(99). Refusing
// only the zero value would let that through, and bulkTierOf's default arm used
// to rank an unrecognised origin tierExactSealed — silently treating it as
// language-declared, which is a WRONG answer rather than an inert one, and which
// breaks the second premise resolveRankedLocked's miss-only bulk consultation
// rests on (every installed row is language-origin) with every ratchet green.
//
// Reachable only since 2026-09-11. The type was exported before that too, but
// the zero value was BulkOriginLanguage, so the one value a caller could produce
// by accident was benign. Opening the BulkSource interface and renumbering the
// enum in the same pass is what made the out-of-range case worth a door.
func (p BulkOrigin) valid() bool {
	return p > BulkOriginUnknown && p < bulkOriginCount
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
	phase  Phase
	sealed bool
	// origin is the row's provenance: bulkTierOf ranks by it, EachBulkRow hands
	// it out for the ratchet, and nothing writes it after the install that
	// creates the row.
	origin BulkOrigin
	src    BulkSource
}

// InstallBulkRow adds one bulk row to this store.
//
// The row is a resolution candidate immediately; nothing is copied out of the
// source, then or ever. Rows are installed at engine origin, before the base is
// written, which is only correct because the row reads through to the source
// live.
//
// origin is a PARAMETER rather than something the row derives, because it is a
// fact about this install and not about src: the same source can be declared by
// the language and reached again by an import. See BulkOrigin.
//
// Installing a BulkOriginImport row, or a row with a NON-EMPTY scope set, breaks
// resolveRankedLocked's miss-only bulk consultation and owes the full argmax
// described there — see its "THE ONE NEW RULE" paragraph. This method and
// BulkOrigin are exported while both ratchets
// (TestEveryOriginRowIsLanguageDeclared, TestBulkRowsCarryTheEmptyScopeSet) are
// in-tree, so an out-of-tree caller can arm that with nothing going red.
//
// PANICS on any origin outside the declared set (BulkOrigin.valid): the zero
// value, which is the field nobody set, and an out-of-range value, which an
// out-of-tree caller can construct because the type is exported. A row nobody
// can rank is a programming error at the call site, not a state the store should
// hold: bulkTierOf would have to invent a tier for it, and every answer it could
// invent is wrong in silence. Refusing here reports it one frame from the
// mistake.
func (p *GlobalEnvironmentFrame) InstallBulkRow(src BulkSource, scopes []*syntax.Scope, phase Phase, sealed bool, origin BulkOrigin) {
	if !origin.valid() {
		panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"InstallBulkRow: origin %d is not a declared BulkOrigin; name the installer (BulkOriginLanguage or BulkOriginImport)", origin))
	}

	p.mu.Lock()
	defer p.mu.Unlock()

	p.bulkRows = append(p.bulkRows, bulkRef{
		scopes: scopes,
		phase:  phase,
		sealed: sealed,
		origin: origin,
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
// It is ExactBindingAt's other half, and it exists for the one reader that needs
// the two separated: lookupLiteralBinding's descent probes its own phase, then
// descending phases, then — last — whatever the language itself supplies. That
// ordering is load-bearing for auxiliary syntax, because a phase-1 probe for
// else must not answer the keyword before the descent has looked at phase 0 for
// a use-site shadow, and the ranked probe cannot express an ordering that spans
// phases. The ambient tier used to be that last step; the rows are.
//
// Reports a tie as an ANSWER rather than raising it, matching ExactBindingAt:
// this reader carries ambiguity across a multi-phase descent instead of failing
// the compile at the first incomparable pair.
//
// Until 2026-09-10 the doc above said that and the body returned a literal
// false, because probeBulkLocked computed no ambiguity at all. The doc was the
// spec and the body was the defect: two rows at equal tier and equal
// cardinality with incomparable scope sets resolved to whichever was installed
// last, silently, where two SLOTS in the identical configuration raise.
// TestBulkRowTieRanksLikeASlotTie pins it.
func (p *GlobalEnvironmentFrame) BulkBindingAt(key *values.Symbol, q syntax.ScopeSet, phase Phase) (bnd *Binding, ambiguous bool) {
	p.mu.RLock()
	defer p.mu.RUnlock()

	_, b, ambiguous, ok := p.probeBulkLocked(*key, q, phase, tierExactMutable)
	if ambiguous {
		// nil alongside true, as bindingWithinTiers does: a tie has no winner to
		// report, and a binding beside the flag invites a caller to use it.
		return nil, true
	}
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

// EachBulkRow calls fn once per installed row, in install order, with that row's
// resolution coordinates, its scope set and its origin. fn returns false to stop
// the walk.
//
// It exists so a ratchet OUTSIDE this package can state the TWO premises
// resolveRankedLocked's miss-only bulk consultation rests on — every installed
// row carries the empty scope set, and every installed row is
// BulkOriginLanguage — over a real engine rather than over a bare namespace,
// which is the only place either was ever in doubt.
// TestBulkRowsCarryTheEmptyScopeSet and TestEveryOriginRowIsLanguageDeclared
// (both pkg/wile) are those ratchets.
//
// Snapshot, then walk: fn runs with no lock held, so it may call back into this
// store, and it never sees p.bulkRows itself. scopes is the caller's own slice,
// handed back for inspection — treat it as read-only; mutating it would mutate
// the installed row.
func (p *GlobalEnvironmentFrame) EachBulkRow(fn func(scopes []*syntax.Scope, phase Phase, sealed bool, origin BulkOrigin) bool) {
	p.mu.RLock()
	rows := make([]bulkRef, len(p.bulkRows))
	copy(rows, p.bulkRows)
	p.mu.RUnlock()

	for _, row := range rows {
		cont := fn(row.scopes, row.phase, row.sealed, row.origin)
		if !cont {
			return
		}
	}
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

// Repoint rebuilds the wrapper around a re-pointed inner source. The mapping and
// the row's identity belong to the importing unit and do not move with the
// store, so they are shared rather than cloned — both are already documented as
// immutable after construction.
func (p *renamedBulkSource) Repoint(store *GlobalEnvironmentFrame) BulkSource {
	return NewRenamedBulkSource(p.inner.Repoint(store), p.localToSource, p.name)
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

// Repoint rebuilds the wrapper around a re-pointed inner source. The admission
// predicate and its enumerable half are the VOCABULARY, which is the dialect's
// and not the store's, so they are shared.
func (p *filteredBulkSource) Repoint(store *GlobalEnvironmentFrame) BulkSource {
	return NewFilteredBulkSource(p.inner.Repoint(store), p.admits, p.enumerable, p.name)
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
//
// PANICS on an undeclared origin, for InstallBulkRow's reason and one more of
// its own: the template is copied into a row at every macro phase the store ever
// reaches, so an unrankable origin installed here is replicated rather than
// isolated.
func (p *GlobalEnvironmentFrame) InstallMacroPhaseRow(src BulkSource, scopes []*syntax.Scope, sealed bool, origin BulkOrigin) {
	if !origin.valid() {
		panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"InstallMacroPhaseRow: origin %d is not a declared BulkOrigin; name the installer (BulkOriginLanguage or BulkOriginImport)", origin))
	}

	p.mu.Lock()
	defer p.mu.Unlock()

	p.macroPhaseRows = append(p.macroPhaseRows, bulkRef{
		scopes: scopes,
		sealed: sealed,
		origin: origin,
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
		phase:  phase,
		sealed: tpl.sealed,
		origin: tpl.origin,
		src:    tpl.src,
	})
}
