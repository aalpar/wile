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
	"sync/atomic"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// OriginRef identifies the provenance ROOT of a binding with library identity:
// the defining library's KEY (RootLib), the DEFINING name inside it (RootName,
// invariant to any export/import renaming), and the phase that library stores
// the binding at (RootPhase, invariant to any import shift). One library can
// bind one name at several phases, as distinct bindings; a base binding
// (BaseOriginLib) has no phase. It is value-identity: a
// library define and every import of it, however renamed, re-exported or
// shifted, carry equal OriginRefs. Set once (library finalization for a define, propagation at
// import) and never mutated, so it is safe to share across the copy-on-write
// BindingMeta path. A nil *OriginRef means NO library identity: a
// program-top-level (define ...), never imported, so only ever compared as the
// identical object. Identity assumes RootLib (a LibraryName.Key()) names its
// library uniquely, the same key assumption ScopeKey/FreeIdKey rely on.
type OriginRef struct {
	RootLib   string
	RootName  string
	RootPhase Phase
}

// BaseOriginLib is the RootLib of a binding no library defines: a registry
// primitive, a bootstrap definition, or a core form keyword. Every library
// environment is built from the engine's one base, so each library's copy of
// such a binding is the same definition, and an export of it is rooted here
// rather than at the exporting library. The base binds a name at several phases
// as one definition (syntax-rules is a *SyntaxCompiler at phase 0 and a
// *PrimitiveExpander at phase 1, both the one form, as racket/base's phase-1
// syntax-rules is its phase-0 binding shifted), so a base root leaves RootPhase
// zero. The #% prefix keeps it from reading as a library name part.
const BaseOriginLib = "#%base"

// BindingMeta holds compile-time metadata (scopes, source location) never read
// during VM execution, but read and written concurrently across SRFI-18 threads
// at compile time (two threads compiling a "define" of the same top-level name).
// A global binding therefore publishes it copy-on-write through its atomicCell
// (see UpdateMeta), so a reader always sees a complete, immutable snapshot.
// Behind a pointer so runtime Binding copies (the hot path) move a pointer.
type BindingMeta struct {
	Scopes syntax.Scopes
	Source *syntax.SourceContext
	Doc    string
	// Stable is the conclusion of a rebind-stability proof (this binding will
	// not be rebound), never a synonym for the evidence. Imported is standing
	// evidence, since R7RS forbids set! on imports, and IsStable() ORs it in;
	// this flag carries the conclusion when a proof discharges it otherwise
	// (defined-once ∧ ¬set! ∧ unit-closed for a top-level define).
	//
	// Two writers set it, both only under WithImmutableTopLevel (the default),
	// which makes unit-closure hold by enforcement rather than inference (set!
	// gate + redefine guard in compile_validated.go): the compiler, for a
	// top-level define, from the validator's in-unit evidence (StableInUnit);
	// and registry.WithStableBasePrimitives, for the ambient core primitives
	// (+, car, <, …) at registration (registry/apply.go). Under
	// WithMutableTopLevel it stays false for non-imported bindings.
	//
	// Read by the frame-reclaim classifier (validate.classifyCallee). The
	// redefine guard treats a Stable ambient primitive as frozen, stricter than
	// Imported, which a top-level define may still supersede per R7RS §5.3.1.
	Imported, Stable bool
	// Assigned records that the unit which OWNS this binding contains a set!
	// targeting it — syntactically, whether or not that set! ever runs. It is
	// owner-side by construction: set! on an imported binding is refused
	// (compile_validated.go), so the only set! that can stamp a binding is one
	// compiled against the binding itself.
	//
	// It exists because of the import edge. An imported binding shares its
	// exporter's value LOCATION (valueLoc), so Imported stopped being evidence
	// that the value will not change: R7RS §5.2 forbids the IMPORTER's set!,
	// and says nothing about the exporting library's own. IsStable() therefore
	// withdraws the Imported conclusion for a binding carrying this flag, and
	// markBindingImported propagates it across the edge beside CaptureSafe.
	//
	// NOT StableInUnit, which cannot answer this question at library
	// granularity: a library body is validated FORM BY FORM
	// (processFormsWithLetrecSemantics), so the unit holding (define c 1) does
	// not contain the sibling form's (set! c 2) and StableInUnit answers "stable"
	// for an assigned variable. This flag is stamped at the set! site instead,
	// where the binding is in hand, which also picks up a set! that arrived
	// through include or a macro expansion.
	Assigned bool
	// CaptureSafe marks a binding whose callee cannot invoke a Scheme procedure and
	// therefore cannot transitively capture a continuation. Two writers stamp it: a
	// Go primitive at registration from !PrimitiveSpec.InvokesProcedure
	// (registry/apply.go), and a Scheme procedure proven capture-safe at compile
	// time (compile_define.go via validate.ProcedureBodyIsCaptureSafe). It is a
	// field because pkg/internal/validate cannot import pkg/registry, the same
	// boundary Stable crosses. The frame-reclaim classifier trusts a callee only
	// when CaptureSafe AND Stable both hold: "cannot capture", plus "cannot be
	// rebound to something that can".
	//
	// INVARIANT: do NOT fold a sibling flag into IsCaptureSafe() the way IsStable
	// ORs in Imported. Imported does not imply capture-safe (an imported apply or
	// map is Imported yet invokes a procedure). Likewise a user redefinition of a
	// primitive name is stamped ONLY if its own body proves capture-safe, which is
	// how the classifier avoids trusting a capturing shadow by name.
	CaptureSafe bool
	// InlineHOF marks a curated higher-order procedure whose single-sequence
	// case-lambda clause may be inlined at a call site that independently proves
	// the callback capture-safe (callback specialization Strategy A). Stamped,
	// never auto-derived, on the sealed-base tail HOFs post-bootstrap (for-each,
	// vector-map, vector-for-each, string-map, string-for-each) and on import-gated
	// ones from their library (fold, srfi/1); the curated set lives in
	// compilation.inlineHOFSpecs. Consumed by the compiler's inline-HOF dispatch.
	//
	// ORTHOGONAL to CaptureSafe: an inline HOF applies its callback, so it is
	// itself NOT capture-safe (for-each.IsCaptureSafe() is false, pinned by
	// capture_safety_test.go). This flag says "inlinable WHEN the callback is
	// proven safe".
	//
	// A bool, not a -1-sentinel int, keeps the capability zero-value-correct: a
	// sentinel would read 0 ("callback param 0") on every binding that calls
	// UpdateMeta, i.e. every primitive (registry/apply.go), falsely marking them
	// inline HOFs. With the bool, &BindingMeta{} means "not an inline HOF", so
	// adding a metadata field still needs no constructor edits.
	InlineHOF bool
	// InlineHOFCallbackParam is the callback's parameter index, read ONLY when
	// InlineHOF is true. Stored as data (not hardcoded 0) so a future HOF whose
	// callback is not the first parameter is handled by the same path.
	InlineHOFCallbackParam int
	// InlineHOFName is the CANONICAL name of the curated HOF this binding is (the
	// inlineHOFSpecs key: "fold", "map", …), recorded at stamp time and read ONLY
	// when InlineHOF is true. The inline dispatch selects the template by THIS
	// identity, never by the call-site surface name — otherwise a curated HOF
	// imported-and-renamed onto another curated HOF's name (e.g. fold as fold-right)
	// would inline the wrong template. Empty on the zero value / unstamped bindings.
	InlineHOFName string
	// Origin is the provenance root of this binding: the root (define ...) it
	// ultimately came from (see OriginRef); nil for a program-top-level define,
	// which has no library identity. free-identifier=? AND ER-compare read it via
	// SameBinding — a library define and its imports share one root and denote the
	// same variable, while two distinct defines of one name have different roots.
	// Set at the library's finalization for a library define (stampLibraryExport-
	// Origins) or propagated at import (markBindingImported); the nil zero value is
	// meaningful, so like the sibling flags above it needs no constructor edit.
	Origin *OriginRef
	// Predeclared marks a slot minted by a letrec* PRE-SCAN whose define has not
	// been compiled yet — a location that exists so a forward reference can
	// resolve, carrying no claim about what the name DENOTES.
	//
	// It rides in the meta rather than in Binding.bindingType because bindingType
	// is a plain field outside the atomicCell, read lock-free by cachedBindings
	// and by the frame optimizer's IsStable/IsCaptureSafe; promoting a type in
	// place would be an unsynchronized write to it. UpdateMeta's copy-on-write CAS
	// is the existing race-safe channel, and both writers (predeclareBinding,
	// compile_define) already call it for their other stamps.
	//
	// Set ONLY on the global arm of predeclareBinding (top-level and library
	// bodies). A lambda/let body's predeclaration is local and keeps plain
	// letrec* per R7RS §5.3.2, so it never carries this.
	Predeclared bool
}

// Binding represents a variable binding in the environment.
// It stores the bound value, the binding type (variable, syntax, or primitive),
// and an optional pointer to compile-time metadata (scopes, source location).
type Binding struct {
	value       values.Value // local bindings (single-threaded, plain field)
	cell        *atomicCell  // global bindings (thread-shared, atomic); nil for locals
	bindingType BindingType
	meta        *BindingMeta // local bindings only; a global's meta lives in cell
}

// valueLoc is a global variable's VALUE LOCATION, one level below the binding
// that names it. It exists so that two bindings can BE the same variable: an
// import points its cell at the exporting library's loc, so the library's own
// assignment is visible through the import and the importer is not reading a
// snapshot taken at import time.
//
// It holds only the value. Everything that distinguishes the two bindings —
// provenance, scopes, the optimizer's capability stamps — stays per-binding in
// atomicCell.m, which is why the sharing is sound: see BindingMeta.Assigned for
// the one meta field that is a claim about the VALUE and therefore has to travel
// across the edge.
type valueLoc struct {
	v atomic.Pointer[values.Value]
}

// atomicCell holds a global binding's mutable, thread-shared state — WHICH
// location its value lives in, and its own compile-time metadata — behind atomic
// pointers. Global bindings are shared across SRFI-18 threads two ways:
//
//   - The VM's cachedBindings cache holds the *Binding and reads its value
//     lock-free (no frame mutex) while set! publishes a new value.
//   - Concurrent compiles (two threads eval'ing a define of the same top-level
//     name) mutate the metadata (Imported/Stable/Scopes/…) while the frame
//     optimizer's IsStable()/IsCaptureSafe() read it.
//
// values.Value and *BindingMeta are each published as a whole pointer, so an
// unsynchronized reader/writer pair on either would tear (corrupt interface ->
// nil-deref/bad type; half-updated meta). The cell makes both publishes atomic:
// value via a lock-free load/store, meta via copy-on-write CAS (updateMeta) so a
// reader always sees a complete, immutable BindingMeta snapshot.
//
// loc is an atomic pointer rather than a plain field for ONE reason: the import
// edge retargets it, and it retargets it on a binding that is already published
// in a global frame and may already sit in another thread's cachedBindings. A
// plain pointer store there is a data race that -race cannot be relied on to
// show (see the Binding.meta → cell migration that preceded this). Keeping loc
// atomic is what makes shareValueLocWith callable at any time instead of only
// before publication, so the import path needs no second rule for a slot that
// already existed.
//
// The noCopy inside atomic.Pointer lives HERE, in a heap object that is never
// value-copied, so Binding itself stays copylocks-clean for the value-embedded
// local frame ([]Binding). Local bindings are single-threaded and never share a
// *Binding, so they keep the plain value/meta fields (cell == nil) and pay no
// atomic op or box allocation on the hot Apply arg-bind path.
type atomicCell struct {
	loc atomic.Pointer[valueLoc]
	m   atomic.Pointer[BindingMeta]
}

func newAtomicCell(v values.Value) *atomicCell {
	return newAtomicCellWithMeta(v, nil)
}

// newAtomicCellWithMeta builds a cell holding both an initial value and an
// initial metadata pointer. meta may be nil (no metadata yet). Used when a
// global binding is created (newGlobalBinding) or a global frame is copied
// (GlobalEnvironmentFrame.Copy), so the meta travels inside the cell.
//
// The loc it mints is this binding's own and unshared; an import replaces it
// afterwards through shareValueLocWith.
func newAtomicCellWithMeta(v values.Value, meta *BindingMeta) *atomicCell {
	q := &atomicCell{}
	loc := &valueLoc{}
	loc.v.Store(&v)
	q.loc.Store(loc)
	if meta != nil {
		q.m.Store(meta)
	}
	return q
}

func (p *atomicCell) load() values.Value {
	boxed := p.loc.Load().v.Load()
	if boxed == nil {
		return nil
	}
	return *boxed
}

func (p *atomicCell) store(v values.Value) {
	p.loc.Load().v.Store(&v)
}

// shareValueLocWith makes p address the same value location as src, so a later
// store through either cell is seen by both. Metadata is untouched.
func (p *atomicCell) shareValueLocWith(src *atomicCell) {
	p.loc.Store(src.loc.Load())
}

// unshareValueLoc gives p a private location again, carrying the current value
// across so the de-aliasing itself changes nothing observable. A store through
// p afterwards is invisible to whoever p was sharing with.
func (p *atomicCell) unshareValueLoc() {
	loc := &valueLoc{}
	// The box is immutable once stored, so handing the new loc the SAME box is
	// safe: a later store through either loc replaces that loc's box pointer and
	// leaves the other's alone. Copying the box would be the same answer, one
	// allocation dearer.
	loc.v.Store(p.loc.Load().v.Load())
	p.loc.Store(loc)
}

// loadMeta returns the current metadata snapshot, or nil if none is attached.
// The returned pointer is immutable: mutate only via updateMeta.
func (p *atomicCell) loadMeta() *BindingMeta {
	return p.m.Load()
}

// updateMeta applies fn to a private copy of the current metadata and publishes
// it with a compare-and-swap, retrying if a concurrent writer won the race. fn
// reports whether it changed anything; when it returns false updateMeta
// publishes nothing and returns false. Because a CAS retry re-runs fn, fn must
// depend only on the *BindingMeta it is handed — no captured cross-call state.
func (p *atomicCell) updateMeta(fn func(*BindingMeta) bool) bool {
	for {
		cur := p.m.Load()
		next := &BindingMeta{}
		if cur != nil {
			*next = *cur
		}
		changed := fn(next)
		if !changed {
			return false
		}
		if p.m.CompareAndSwap(cur, next) {
			return true
		}
	}
}

// newGlobalBinding creates a binding whose value lives in an atomicCell, for
// installation into a GlobalEnvironmentFrame where it may be read lock-free from
// multiple threads. Every binding in a global frame carries a cell, minted here
// or by GlobalEnvironmentFrame.Copy, which snapshots value and meta into a fresh
// one; that is the "in a global frame => has a cell" invariant.
// scopes are the hygiene scope set the binding is created under; they become
// part of its identity within the frame, so a macro-introduced top-level binder
// and a user-written one of the same name stay distinct variables. The set is
// written once here and never mutated afterwards, so it needs no interaction
// with the copy-on-write UpdateMeta path.
func newGlobalBinding(value values.Value, bindingType BindingType, scopes syntax.Scopes) *Binding {
	if scopes.IsEmpty() {
		return &Binding{
			cell:        newAtomicCell(value),
			bindingType: bindingType,
		}
	}
	return &Binding{
		// No defensive copy: Scopes is immutable and persistent, so the caller
		// cannot mutate what it handed us. slices.Clone here was guarding against
		// a shared backing array that no longer exists.
		cell:        newAtomicCellWithMeta(value, &BindingMeta{Scopes: scopes}),
		bindingType: bindingType,
	}
}

// NewBinding creates a new binding with the given value and type.
// The binding has no scopes (for backward compatibility with non-hygienic code).
func NewBinding(value values.Value, bindingType BindingType) *Binding {
	return &Binding{
		value:       value,
		bindingType: bindingType,
	}
}

// NewBindingWithScopes creates a binding with associated scopes (for hygiene)
func NewBindingWithScopes(value values.Value, bindingType BindingType, scopes syntax.Scopes) *Binding {
	return &Binding{
		value:       value,
		bindingType: bindingType,
		meta: &BindingMeta{
			Scopes: scopes,
		},
	}
}

// NewBindingWithSource creates a binding with source location information.
func NewBindingWithSource(value values.Value, bindingType BindingType, scopes syntax.Scopes, source *syntax.SourceContext) *Binding {
	return &Binding{
		value:       value,
		bindingType: bindingType,
		meta: &BindingMeta{
			Scopes: scopes,
			Source: source,
		},
	}
}

// Value returns the value stored in this binding. Global bindings (cell != nil)
// read atomically so a lock-free cachedBindings reader is safe against a
// concurrent set!; local bindings read the plain field.
func (p *Binding) Value() values.Value {
	if p.cell != nil {
		return p.cell.load()
	}
	return p.value
}

// BindingType returns the type of this binding (variable, syntax, or primitive).
func (p *Binding) BindingType() BindingType {
	return p.bindingType
}

// ShareValueLocationWith makes this binding and src BE the same variable: both
// address one value location, so a store through either is seen by the other.
// Metadata stays per-binding, which is the point — the importer keeps its own
// provenance, scopes and capability stamps while reading the exporter's value.
//
// This is the import edge (installImportedBinding). It replaces a value COPY,
// which left the importer reading a snapshot taken at import time: a library that
// assigns its own exported variable was invisible to its importers. R7RS is
// silent (§5.2 constrains the importer, not the exporter); Racket reads
// through, Chez refuses the library.
//
// Both bindings must be global (in a global frame, hence cell != nil). A local
// binding has no cell to share and is single-threaded by construction, so asking
// is a caller bug rather than a runtime condition.
//
// Safe on a published binding: the retarget is one atomic store, which is why
// the import path needs no separate rule for a slot that already existed.
func (p *Binding) ShareValueLocationWith(src *Binding) error {
	if p.cell == nil || src == nil || src.cell == nil {
		return werr.WrapForeignErrorf(werr.ErrInternal,
			"ShareValueLocationWith: both bindings must be global")
	}
	p.cell.shareValueLocWith(src.cell)
	return nil
}

// UnshareValueLocation gives this binding a private value location again,
// carrying the current value across so the call itself changes nothing
// observable.
//
// It is the supersede path's de-alias (compile_define.go): a top-level define
// over an imported name drops the import provenance and then writes the
// define's value into the slot, which with a shared location would land on the
// exporting library's variable. One call before the provenance is cleared keeps
// supersede meaning "this name is mine now" rather than "assign the library's
// variable". A no-op on a local binding and on a global that shares with no one.
func (p *Binding) UnshareValueLocation() {
	if p.cell == nil {
		return
	}
	p.cell.unshareValueLoc()
}

// SetValue updates the value stored in this binding. Global bindings publish
// atomically (paired with the lock-free reader in Value); local bindings write
// the plain field.
func (p *Binding) SetValue(value values.Value) {
	if p.cell != nil {
		p.cell.store(value)
		return
	}
	p.value = value
}

// Meta returns the current BindingMeta snapshot, or nil if no metadata has been
// attached. For a global binding (cell != nil) the snapshot is immutable — do
// not write through it; use UpdateMeta. Callers that read metadata fields should
// nil-check the returned pointer; the convenience getters (Scopes, Source, Doc,
// IsImported, IsStable) wrap this pattern.
func (p *Binding) Meta() *BindingMeta {
	if p.cell != nil {
		return p.cell.loadMeta()
	}
	return p.meta
}

// UpdateMeta mutates this binding's compile-time metadata and is the only
// metadata mutator API. fn receives a *BindingMeta to modify and returns whether
// it changed anything; UpdateMeta returns that same result. Adding a new
// metadata field thus requires editing only the BindingMeta struct itself; no
// parallel getter/setter accessor pair is needed. Usage:
//
//	b.UpdateMeta(func(m *BindingMeta) bool {
//		m.Imported = true
//		return true
//	})
//
// For a global binding (cell != nil) the update is copy-on-write under an atomic
// CAS, so it is safe against concurrent compiles and lock-free readers, and fn
// may be re-run on CAS contention — fn MUST therefore depend only on the
// *BindingMeta it is handed, never on captured cross-call state (return the
// "did I change it?" answer instead of recording it in a closed-over variable).
// For a local binding (single-threaded) fn runs exactly once, in place.
func (p *Binding) UpdateMeta(fn func(*BindingMeta) bool) bool {
	if p.cell != nil {
		return p.cell.updateMeta(fn)
	}
	if p.meta == nil {
		p.meta = &BindingMeta{}
	}
	return fn(p.meta)
}

// Scopes returns the hygiene scopes associated with this binding.
//
// A binding with no hygiene information returns the EMPTY set, not a
// distinguishable absence. Before the Scopes flip this returned a nil slice
// while SyntaxSymbol.Scopes() normalized its own nil away, so the two accessors
// for one concept had opposite nil discipline; they now agree by construction.
func (p *Binding) Scopes() syntax.Scopes {
	m := p.Meta()
	if m == nil {
		return syntax.Scopes{}
	}
	return m.Scopes
}

// Source returns the source location where this binding was defined.
// Returns nil for bindings without source information.
func (p *Binding) Source() *syntax.SourceContext {
	m := p.Meta()
	if m == nil {
		return nil
	}
	return m.Source
}

// Doc returns the documentation string for this binding.
// Returns empty string for bindings without documentation.
func (p *Binding) Doc() string {
	m := p.Meta()
	if m == nil {
		return ""
	}
	return m.Doc
}

// IsImported returns whether this binding was imported from a library.
func (p *Binding) IsImported() bool {
	m := p.Meta()
	if m == nil {
		return false
	}
	return m.Imported
}

// IsStable reports the rebind-stability conclusion: the binding will not be
// rebound. Imported is standing evidence for that conclusion (R7RS §5.2
// forbids set! on imports) UNLESS the exporting unit assigns the variable
// itself, which that sentence does not forbid and which the shared value
// location makes visible here; Stable carries the conclusion when a proof
// discharges it by other means. This is NOT a set!-permission — that is
// IsImported alone (R7RS §5.2). Read by the frame-reclaim classifier
// (validate.classifyCallee) and by checkCallArity, which reads the binding's
// VALUE at compile time and so is the consumer the Assigned withdrawal exists
// for. Renamed from the retired IsConstant, which falsely asserted "value known
// at compile time".
func (p *Binding) IsStable() bool {
	m := p.Meta()
	if m == nil {
		return false
	}
	return m.Stable || (m.Imported && !m.Assigned)
}

// IsAssigned reports whether the unit owning this binding assigns it with set!
// (BindingMeta.Assigned). Read by the import path, which carries the fact across
// the edge so the importer's IsStable() answers from the same evidence.
func (p *Binding) IsAssigned() bool {
	m := p.Meta()
	if m == nil {
		return false
	}
	return m.Assigned
}

// IsCaptureSafe reports whether this binding's callee cannot invoke a Scheme
// procedure — a Go primitive stamped from !PrimitiveSpec.InvokesProcedure at
// registration, or a Scheme procedure proven capture-safe at compile time
// (ProcedureBodyIsCaptureSafe). The frame-reclaim classifier pairs it with
// IsStable() — capture-safe AND non-rebindable — to trust a callee. Returns false
// when no metadata is set: the conservative default, an unstamped binding is never
// trusted. Unlike IsStable (which ORs in Imported), this reads CaptureSafe alone:
// Imported does NOT imply capture-safe (see the BindingMeta.CaptureSafe invariant).
func (p *Binding) IsCaptureSafe() bool {
	m := p.Meta()
	if m == nil {
		return false
	}
	return m.CaptureSafe
}

// InlineHOFParam reports the callback parameter index of a curated inline-HOF
// binding (callback specialization Strategy A), or -1 when this binding is not a
// curated inline HOF. The gating BindingMeta.InlineHOF flag makes -1 the correct
// answer for an unstamped binding and for a binding whose meta exists but carries
// no inline-HOF stamp (the UpdateMeta zero value). Read by the compiler's
// inline-HOF dispatch to decide whether to attempt call-site specialization.
func (p *Binding) InlineHOFParam() int {
	m := p.Meta()
	if m == nil || !m.InlineHOF {
		return -1
	}
	return m.InlineHOFCallbackParam
}

// InlineHOFName returns the canonical name of the curated HOF this binding is, or
// "" if it is not a stamped inline HOF. The inline dispatch selects the template
// by this identity rather than the call-site surface name, so an import-renamed
// curated HOF inlines its OWN template.
func (p *Binding) InlineHOFName() string {
	m := p.Meta()
	if m == nil || !m.InlineHOF {
		return ""
	}
	return m.InlineHOFName
}

// Origin returns the import-provenance root of this binding, or nil if it is a
// plain (non-imported) define, which is its own root.
func (p *Binding) Origin() *OriginRef {
	m := p.Meta()
	if m == nil {
		return nil
	}
	return m.Origin
}

// SameBinding reports whether two bindings denote the same variable for the
// purpose of identifier equality (free-identifier=? and ER-compare): the same
// binding object, or two bindings that share one provenance root — a library
// define and its imports, under any rename or re-export. Two distinct defines of
// one name have different roots and are NOT the same, so this deliberately does
// not collapse into "same value" the way pointer- or value-equality would.
// Bindings with no origin (program-top-level defines) match only as the identical
// object.
//
// A library define carries its self-root from finalization (stampLibraryExport-
// Origins), so comparing a library-internal binding against an IMPORT of itself
// matches — which is what lets ER-compare, resolving renames at the definition
// site, adopt this (option B in plan 2026-07-24-free-identifier-origin-provenance-
// design).
func SameBinding(a, b *Binding) bool {
	if a == b {
		return true
	}
	if a == nil || b == nil {
		return false
	}
	ao, bo := a.Origin(), b.Origin()
	return ao != nil && bo != nil && *ao == *bo
}
