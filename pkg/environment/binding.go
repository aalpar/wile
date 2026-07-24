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
	"slices"
	"sync/atomic"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// OriginRef identifies the provenance ROOT of an imported binding: the
// (define ...) that ultimately created it, addressed by the KEY of the library
// that defines it (RootLib) and the DEFINING name inside that library
// (RootName, invariant to any export/import renaming). It is value-identity —
// two imports of one binding, however renamed or re-exported, carry equal
// OriginRefs — and is set once at import and never mutated, so it is safe to
// share across the copy-on-write BindingMeta path. A nil *OriginRef means "no
// import provenance": a plain (define ...) is its own root. Identity assumes
// RootLib (a LibraryName.Key()) names its library uniquely, the same key
// assumption ScopeKey/FreeIdKey already rely on.
type OriginRef struct {
	RootLib  string
	RootName string
}

// BindingMeta holds compile-time metadata (scopes and source location) that is
// never read during VM execution — but IS read and written concurrently across
// SRFI-18 threads at compile time (two threads compiling a define of the same
// top-level name). For a global binding it is therefore published copy-on-write
// through the binding's atomicCell (see UpdateMeta); a reader always sees a
// complete, immutable snapshot. Stored behind a pointer so that runtime Binding
// copies (the hot path) move a pointer instead of the whole struct.
type BindingMeta struct {
	Scopes   []*syntax.Scope
	Source   *syntax.SourceContext
	Doc      string
	Imported bool
	// Stable is the conclusion of a rebind-stability proof: the binding will
	// not be rebound. It is set ONLY by a completed proof, never as a synonym
	// for evidence. Imported (above) is *evidence* sufficient for that
	// conclusion — R7RS forbids set! on imports — so IsStable() treats Imported
	// as standing evidence and this flag carries the conclusion when a proof
	// discharges it by other means (defined-once ∧ ¬set! ∧ unit-closed for a
	// top-level define). The WithImmutableTopLevel engine option (the default)
	// discharges it for top-level defines: the compiler sets this from the
	// validator's in-unit evidence (StableInUnit) and the language then forbids
	// the cross-unit set!/redefine that evidence alone could not rule out (set!
	// gate + redefine guard in compile_validated.go), making unit-closure hold
	// by enforcement rather than inference. When the option is off (WithMutableTopLevel),
	// this flag stays false for non-imported bindings — asserting it from
	// partial evidence would be a false conclusion. Read by the frame
	// optimizer's MayCapture. Distinct from set!-permission (Imported alone,
	// unless the option is on) and from the retired "Constant" flag (which
	// conflated provenance, stability, and compile-time-value-known).
	//
	// A second writer also discharges it: under the same WithImmutableTopLevel
	// option, registry.WithStableBasePrimitives stamps the ambient capture-safe
	// core primitives (+, car, <, …) Stable at registration (registry/apply.go),
	// backed by the same set!/redefine enforcement. Both writers mean the same
	// thing — "non-rebindable" — which is why the redefine guard treats a Stable
	// ambient primitive as frozen, stricter than an Imported binding (which a
	// top-level define may still supersede per R7RS §5.3.1).
	Stable bool
	// CaptureSafe marks a binding whose callee cannot invoke a Scheme procedure and
	// therefore cannot transitively capture a continuation. Two writers stamp it:
	// a Go primitive at registration from !PrimitiveSpec.InvokesProcedure
	// (registry/apply.go), and a Scheme procedure proven capture-safe at compile
	// time (compile_define.go via validate.ProcedureBodyIsCaptureSafe — stdlib like
	// zero?/not, or a user helper). It is the classifier-readable form of that
	// capability, because pkg/internal/validate cannot import pkg/registry —
	// mirroring how Stable carries the rebind-stability conclusion across the same
	// boundary. The frame-reclaim classifier trusts a callee only when CaptureSafe
	// AND Stable both hold: CaptureSafe is the "cannot capture" capability, Stable
	// the "cannot be rebound to something that can" guarantee.
	//
	// INVARIANT — do NOT fold any sibling flag into IsCaptureSafe() the way IsStable
	// ORs in Imported: Imported does NOT imply capture-safe (an imported `apply` or
	// `map` is Imported yet invokes a procedure). IsCaptureSafe() reads this field
	// alone, by design. A user redefinition of a primitive name (e.g.
	// (define car <lambda>)) carries this flag ONLY if its own body proves
	// capture-safe — a capturing redefinition is never stamped, which is how the
	// classifier avoids trusting a capturing shadow by name.
	CaptureSafe bool
	// InlineHOF marks a curated higher-order procedure whose single-sequence
	// case-lambda clause may be inlined at a call site that independently proves
	// the callback capture-safe (callback specialization Strategy A). Stamped on
	// the sealed-base tail HOFs post-bootstrap (for-each, vector-map,
	// vector-for-each, string-map, string-for-each) and on import-gated ones from
	// their library (fold, srfi/1); NOT auto-derived. The curated set lives in
	// compilation.inlineHOFSpecs. Consumed by the compiler's inline-HOF dispatch.
	//
	// ORTHOGONAL to CaptureSafe: an inline HOF is itself NOT capture-safe — it
	// applies the callback, which may capture (for-each.IsCaptureSafe() is false,
	// pinned by capture_safety_test.go). This flag says "inlinable WHEN the
	// callback is proven safe," a different question about the same binding.
	//
	// The gating bool is what keeps the capability zero-value-correct: a plain
	// -1-sentinel int would read 0 ("callback param 0") on every binding that ever
	// calls UpdateMeta (which is every primitive — see registry/apply.go), falsely
	// marking them inline HOFs. With the bool, the &BindingMeta{} zero value
	// (InlineHOF=false) correctly means "not an inline HOF," preserving the
	// invariant that adding a metadata field needs no constructor edits.
	InlineHOF bool
	// InlineHOFCallbackParam is the callback's parameter index, read ONLY when
	// InlineHOF is true. Stored as data (not hardcoded 0) so a future HOF whose
	// callback is not the first parameter is handled by the same path.
	InlineHOFCallbackParam int
	// Origin is the import-provenance root of this binding: for an imported
	// binding, the root (define ...) it ultimately came from (see OriginRef);
	// nil for a plain define, which is its own root. free-identifier=? reads it
	// via SameBinding — two imports of one binding share one root and denote the
	// same variable, while two distinct defines of one name have different roots.
	// (ER-compare does NOT yet read it; see erBindingsEqual for why and the
	// deferred option B.) Set once at import by the propagation fold in
	// markBindingImported; the nil zero value is meaningful, so like the sibling
	// flags above it needs no constructor edit.
	Origin *OriginRef
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

// atomicCell holds a global binding's mutable, thread-shared state — its value
// and its compile-time metadata — behind atomic pointers. Global bindings are
// shared across SRFI-18 threads two ways:
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
// The noCopy inside atomic.Pointer lives HERE, in a heap object that is never
// value-copied, so Binding itself stays copylocks-clean for the value-embedded
// local frame ([]Binding). Local bindings are single-threaded and never share a
// *Binding, so they keep the plain value/meta fields (cell == nil) and pay no
// atomic op or box allocation on the hot Apply arg-bind path.
type atomicCell struct {
	v atomic.Pointer[values.Value]
	m atomic.Pointer[BindingMeta]
}

func newAtomicCell(v values.Value) *atomicCell {
	q := &atomicCell{}
	q.v.Store(&v)
	return q
}

// newAtomicCellWithMeta builds a cell holding both an initial value and an
// initial metadata pointer. meta may be nil (no metadata yet). Used when a
// binding is promoted or copied into a global frame and its existing meta must
// travel into the cell rather than the (now global-invisible) plain field.
func newAtomicCellWithMeta(v values.Value, meta *BindingMeta) *atomicCell {
	q := &atomicCell{}
	q.v.Store(&v)
	if meta != nil {
		q.m.Store(meta)
	}
	return q
}

func (p *atomicCell) load() values.Value {
	boxed := p.v.Load()
	if boxed == nil {
		return nil
	}
	return *boxed
}

func (p *atomicCell) store(v values.Value) {
	p.v.Store(&v)
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
// multiple threads. Every binding entering a global frame must go through this
// (or ensureGlobalCell) so the "in a global frame => has a cell" invariant holds.
// scopes are the hygiene scope set the binding is created under; they become
// part of its identity within the frame, so a macro-introduced top-level binder
// and a user-written one of the same name stay distinct variables. The set is
// written once here and never mutated afterwards, so it needs no interaction
// with the copy-on-write UpdateMeta path.
func newGlobalBinding(value values.Value, bindingType BindingType, scopes []*syntax.Scope) *Binding {
	if len(scopes) == 0 {
		return &Binding{
			cell:        newAtomicCell(value),
			bindingType: bindingType,
		}
	}
	return &Binding{
		cell:        newAtomicCellWithMeta(value, &BindingMeta{Scopes: slices.Clone(scopes)}),
		bindingType: bindingType,
	}
}

// ensureGlobalCell migrates a binding's value into an atomicCell if it does not
// already have one, so it is safe to publish into a global frame. Must be called
// before the *Binding becomes reachable from other threads (i.e. before it is
// stored into the global bindings slice), so the migration itself is unraced.
func (p *Binding) ensureGlobalCell() {
	if p.cell != nil {
		return
	}
	// Migrate BOTH value and meta into the cell: once cell != nil the plain
	// fields are global-invisible (Value/Meta read only the cell), so leaving
	// meta behind would silently drop this binding's scopes/provenance.
	p.cell = newAtomicCellWithMeta(p.value, p.meta)
	p.value = nil
	p.meta = nil
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
func NewBindingWithScopes(value values.Value, bindingType BindingType, scopes []*syntax.Scope) *Binding {
	return &Binding{
		value:       value,
		bindingType: bindingType,
		meta: &BindingMeta{
			Scopes: scopes,
		},
	}
}

// NewBindingWithSource creates a binding with source location information.
func NewBindingWithSource(value values.Value, bindingType BindingType, scopes []*syntax.Scope, source *syntax.SourceContext) *Binding {
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
// Returns nil for bindings without hygiene information.
func (p *Binding) Scopes() []*syntax.Scope {
	m := p.Meta()
	if m == nil {
		return nil
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
// rebound. Imported is standing evidence for that conclusion (R7RS forbids set!
// on imports); Stable carries it when a proof discharges it by other means.
// This is NOT a set!-permission — that is IsImported alone (R7RS §5.2). Read by
// the frame optimizer's MayCapture (sibling escape-gated plan). Renamed from the
// retired IsConstant, which falsely asserted "value known at compile time".
func (p *Binding) IsStable() bool {
	m := p.Meta()
	if m == nil {
		return false
	}
	return m.Imported || m.Stable
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
// purpose of identifier equality (free-identifier=?): the same binding object,
// or two imports that share one import-provenance root (a binding imported under
// several names, directly or through re-exports). Two distinct defines of one
// name have different roots and are NOT the same, so this deliberately does not
// collapse into "same value" the way pointer- or value-equality would. Bindings
// with no origin (plain defines) match only as the identical object.
//
// NOTE the asymmetry this cannot yet bridge: a library-internal define has no
// origin, so comparing it against an IMPORT of itself returns false. That is why
// ER-compare (which resolves renames at the definition site) keeps its value
// fallback instead of adopting this; see erBindingsEqual and option B in plan
// 2026-07-24-free-identifier-origin-provenance-design.
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

// Copy creates a deep copy of this binding. The meta struct is copied so
// that mutations through UpdateMeta on the original do not affect the
// copy. This method is only used during compilation/expansion, never on
// the runtime hot path.
func (p *Binding) Copy() *Binding {
	b := &Binding{
		bindingType: p.bindingType,
	}
	var m *BindingMeta
	src := p.Meta()
	if src != nil {
		m = &BindingMeta{
			Scopes:                 src.Scopes,
			Source:                 src.Source,
			Doc:                    src.Doc,
			Imported:               src.Imported,
			Stable:                 src.Stable,
			CaptureSafe:            src.CaptureSafe,
			InlineHOF:              src.InlineHOF,
			InlineHOFCallbackParam: src.InlineHOFCallbackParam,
			// Origin is an immutable pointer set once at import; copying the
			// pointer avoids an allocation. Value-equality (*a == *b in
			// SameBinding) holds regardless of whether the pointer is shared, so
			// identity stays stable through CloneForMatch either way.
			Origin: src.Origin,
		}
	}
	// A global binding's value and meta live in its atomicCell; give the copy a
	// fresh cell holding snapshots of both. A local binding copies the plain
	// fields.
	if p.cell != nil {
		b.cell = newAtomicCellWithMeta(p.cell.load(), m)
	} else {
		b.value = p.value
		b.meta = m
	}
	return b
}
