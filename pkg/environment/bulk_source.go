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
		store: store,
		phase: phase,
		name:  name,
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

	ref, _, ok := p.store.probeRankedLocked(name, syntax.EmptyScopes(), p.phase, tierExactMutable)
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
