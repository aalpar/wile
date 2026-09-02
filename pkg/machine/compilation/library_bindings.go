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

package compilation

// library_bindings.go implements import set processing and binding installation.
//
// This file provides ImportSet (the parsed representation of R7RS import
// modifiers like only, except, prefix, rename) and the functions that
// resolve import sets and copy library bindings into target environments.

import (
	"context"
	"math"

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// markBindingImported records import provenance on a target binding installed by
// library import, propagating the source binding's capture-safety. A nil target is
// silently ignored.
//
// Imported (the evidence) is set on the target. The Stable conclusion is NOT stored
// here: IsStable() already treats Imported as standing evidence for stability, so
// setting Stable too would conflate evidence with the proof result. The Stable flag
// is reserved for a completed rebind-stability proof of a non-imported binding.
//
// CaptureSafe IS propagated from source, unlike Stable: it is a static capability of
// the underlying primitive (does it invoke a Scheme procedure?), not a provenance or
// a stability conclusion, so an imported primitive has exactly the same capture-safety
// as its source. The frame-reclaim classifier reads IsCaptureSafe() on the imported
// binding; without this propagation an (import (scheme base)) program loses the frame
// optimization that the ambient WithStableBasePrimitives path keeps (the import path
// creates a fresh binding, so the registration-time CaptureSafe stamp does not carry
// over on its own).
//
// source is expected non-nil: every live caller resolves it from findLibraryBinding's
// found==true path. A nil source is therefore a caller bug — it would silently drop
// capture-safety (a leak-safe miss that surfaces only as an unexplained perf
// regression on an imported program, never as corruption) — so the guard below is
// defensive, not a supported "no propagation" mode.
//
// exportName is the binding's name in the library (the external/internal export
// name, NOT the local alias). It establishes the curated inline-HOF capability on
// the imported target via stampImportedInlineHOF, which stamps only when the
// target's provenance ROOT is exactly (spec.homeLib, exportName) — export name
// selects the spec, the root is the identity gate — and applies the stamp to the
// fresh per-import target (not the shared source) so it is race-safe under
// concurrent imports. stampImportedInlineHOF stamps ONLY import-gated HOFs (fold),
// so a same-named re-export of a SEALED-BASE HOF — e.g. SRFI-13's string-map, a
// different procedure from R7RS string-map — is never stamped here and never
// mis-inlined with the R7RS template. The sealed-base HOFs are stamped only at
// their real home (StampInlineHOFs). The import path is also the only library
// seam, so a user's own (define …) of a HOF name is never stamped here. Any stale
// stamp from a prior import of this slot is reset below before re-deriving it.
func markBindingImported(target, source *environment.Binding, exportName, internalName string, sourceLib LibraryName) {
	if target == nil {
		return
	}
	// Import-provenance root (plan 2026-07-24-free-identifier-origin): propagate
	// the source's already-resolved root when the source is itself imported (a
	// re-export hop), else synthesize the root from the defining name in the
	// source library. Keyed on internalName — the name inside sourceLib that
	// defines the binding — so a renamed export or import does not fork one
	// binding's identity. This is the signal SameBinding reads for
	// free-identifier=? and ER-compare. Computed ONCE here, outside the UpdateMeta closure
	// below, so the fold stays a pure function of the *BindingMeta it is handed
	// even when a CAS retry re-runs it (source.Origin() is a cross-binding read).
	var origin *environment.OriginRef
	if source != nil {
		srcOrigin := source.Origin()
		if srcOrigin != nil {
			// Propagate the source's already-resolved root. A library define is
			// pre-stamped with its self-root at finalization (stampLibraryExport-
			// Origins), so this branch carries both a re-export hop's root and a
			// direct import's define-site root.
			origin = srcOrigin
		} else {
			// Fallback for a source with no pre-stamped origin (e.g. a synthetic
			// library that skipped stampLibraryExportOrigins): synthesize the root
			// from the defining name in the source library.
			origin = &environment.OriginRef{RootLib: sourceLib.Key(), RootName: internalName}
		}
	}
	target.UpdateMeta(func(m *environment.BindingMeta) bool {
		m.Imported = true
		// Reset any inline-HOF stamp before re-deriving it below: a re-import can
		// overwrite target's value (R7RS §5.6 last-import-wins under the
		// sameImportedBinding name-conflation), and the stamp — since dispatch
		// selects the template by it — must track the CURRENT value, never a stale
		// template from a prior import of this slot. stampImportedInlineHOF re-adds
		// it iff THIS import qualifies. Harmless on a first import (already unset).
		m.InlineHOF = false
		m.InlineHOFName = ""
		m.InlineHOFCallbackParam = 0
		if source != nil {
			m.CaptureSafe = source.IsCaptureSafe()
			// Carry the docstring across the import boundary so ,doc and the doc
			// tooling find it on the imported binding (e.g. a (wile control) macro
			// documented at its define-syntax site). The copy path installs only
			// the value, so without this the docstring would be lost on import.
			//
			// Assigned unconditionally, for the same reason the inline-HOF stamp is
			// reset above: a re-import can replace target's value under the
			// sameImportedBinding name-conflation (R7RS §5.6 last-import-wins), and
			// a docstring left over from the displaced value documents a binding
			// that is no longer there. A procedure carries its own docstring on its
			// template and so tracks its value for free; a macro has no template,
			// making this field the macro path's only carrier — and the only one
			// that could go stale.
			m.Doc = source.Doc()
			m.Origin = origin
		}
		return true
	})
	// The inline-HOF stamp is gated on the binding's provenance ROOT (origin), not
	// just the export name — so the real srfi-1 fold is stamped even through a
	// re-export chain, while a same-named HOF from another library is not (and the
	// reset above drops a stale stamp when a conflation re-import replaced the
	// value). See stampImportedInlineHOF.
	stampImportedInlineHOF(target, exportName, origin)
}

// stampLibraryExportOrigins gives each library-DEFINED export its own provenance
// root {lib.Key(), internalName}, intrinsically at library finalization — before
// any import. So a define-site binding carries the same origin an import of it
// would otherwise synthesize, which is what makes identifier equality
// (free-identifier=?, ER-compare's definition-site rename) match a library's
// internal binding against an import of itself.
//
// Only a genuine define in THIS library is stamped: a re-exported binding already
// carries the propagated root of its true source (it was imported into this
// library, so its Origin is non-nil), and is left untouched. Runs once per
// library compile, single-threaded; the nil-guard keeps it idempotent and
// preserves a re-export's root.
func stampLibraryExportOrigins(lib *CompiledLibrary) {
	for _, internalName := range lib.Exports {
		binding, _, found := findLibraryBinding(lib, internalName)
		if !found || binding == nil || binding.Origin() != nil {
			continue
		}
		root := &environment.OriginRef{RootLib: lib.Name.Key(), RootName: internalName}
		binding.UpdateMeta(func(m *environment.BindingMeta) bool {
			if m.Origin != nil {
				return false
			}
			m.Origin = root
			return true
		})
	}
}

// ImportSet represents a parsed import specification.
// It can be a simple library reference or include modifiers.
//
// PhaseShift supports Racket-style phased imports:
//   - (import (scheme base))                    ; Phase 0 (runtime) - default
//   - (import (for-syntax (scheme base)))       ; Phase +1 (expand)
//   - (import (for-template (scheme base)))     ; Phase -1
//   - (import (for-meta 2 (scheme base)))       ; Phase +2
//   - (import (for-meta -1 (scheme base)))      ; Phase -1 (same as for-template)
//
// Phase shifts compose additively: (for-syntax (for-syntax lib)) = phase +2
//
// Modifiers preserves the written nesting order of only/except/prefix/rename so
// ApplyToExports can fold them INSIDE-OUT, as R7RS §5.6 requires. The innermost
// (textually deepest) modifier is Modifiers[0]; each later modifier operates on the
// output of the one before it. A flat representation (separate Only/Except/Prefix/
// Renames fields) cannot express this — it both loses the ordering between different
// modifier kinds and silently overwrites a repeated kind, so e.g.
// (prefix (prefix LIB a-) b-) would bind b-car instead of b-a-car.
type ImportSet struct {
	LibraryName LibraryName       // Base library to import from
	Modifiers   []importModifier  // only/except/prefix/rename, innermost first
	PhaseShift  environment.Phase // Phase offset: 0=runtime, 1=for-syntax, -1=for-template
}

// importModifierKind discriminates the four R7RS import-set modifier forms.
//
// ADDING A NEW IMPORT MODIFIER requires updates in these locations:
//   - this iota block (the kind constant)
//   - the importModifier struct (a field for the modifier's payload, if any)
//   - an Add* builder method on *ImportSet
//   - the parser dispatch in ParseImportSetFromDatum (import_set_datum.go)
//   - the switch in (*importModifier).apply
type importModifierKind int

const (
	importModOnly importModifierKind = iota
	importModExcept
	importModRename
	importModPrefix
)

// importModifier is a single only/except/prefix/rename step. Only the field for its
// kind is populated.
type importModifier struct {
	kind    importModifierKind
	ids     values.StringSet  // only / except
	prefix  string            // prefix
	renames map[string]string // rename: old-name -> new-name
}

// NewImportSet creates a new import set for a library, with no modifiers.
func NewImportSet(name LibraryName) *ImportSet {
	return &ImportSet{
		LibraryName: name,
	}
}

// AddOnly appends an `only` modifier restricting the import to ids. An empty/nil ids
// set installs a modifier that imports NOTHING: R7RS §5.6 grammar is
// (only <import-set> <identifier> …) with zero-or-more identifiers, so (only LIB)
// with no identifiers denotes the empty subset. AddOnly is called exactly once per
// syntactic `only` form, so the empty case is a real "import nothing", not "no filter".
func (p *ImportSet) AddOnly(ids values.StringSet) {
	p.Modifiers = append(p.Modifiers, importModifier{kind: importModOnly, ids: ids})
}

// AddExcept appends an `except` modifier removing ids from the import. Empty/nil is a
// no-op.
func (p *ImportSet) AddExcept(ids values.StringSet) {
	if len(ids) == 0 {
		return
	}
	p.Modifiers = append(p.Modifiers, importModifier{kind: importModExcept, ids: ids})
}

// AddPrefix appends a `prefix` modifier prepending prefix to every imported name. An
// empty prefix is a no-op.
func (p *ImportSet) AddPrefix(prefix string) {
	if prefix == "" {
		return
	}
	p.Modifiers = append(p.Modifiers, importModifier{kind: importModPrefix, prefix: prefix})
}

// AddRename appends a `rename` modifier mapping old names to new names. Empty/nil is a
// no-op.
func (p *ImportSet) AddRename(renames map[string]string) {
	if len(renames) == 0 {
		return
	}
	p.Modifiers = append(p.Modifiers, importModifier{kind: importModRename, renames: renames})
}

// ApplyToExports applies the import modifiers inside-out and returns the final
// bindings as a map of local-name -> external-name (the name in the library).
func (p *ImportSet) ApplyToExports(lib *CompiledLibrary) (map[string]string, error) {
	result := make(map[string]string)

	// Start with all exports.
	for externalName := range lib.Exports {
		result[externalName] = externalName
	}

	// Fold modifiers in written nesting order (innermost first); each step sees the
	// output local names of the previous step, e.g. an `only` after a `prefix`
	// matches against the already-prefixed names.
	for i := range p.Modifiers {
		next, err := p.Modifiers[i].apply(result, lib)
		if err != nil {
			return nil, err
		}
		result = next
	}

	return result, nil
}

// apply transforms a local-name -> external-name map by one import modifier.
func (p *importModifier) apply(result map[string]string, lib *CompiledLibrary) (map[string]string, error) {
	switch p.kind {
	case importModOnly:
		filtered := make(map[string]string)
		for name := range p.ids {
			externalName, ok := result[name]
			if !ok {
				return nil, werr.WrapForeignErrorf(werr.ErrUnexportedIdentifier,
					"applyToExports: identifier %q not exported by %s", name, lib.Name.SchemeString())
			}
			filtered[name] = externalName
		}
		return filtered, nil
	case importModExcept:
		for name := range p.ids {
			_, ok := result[name]
			if !ok {
				return nil, werr.WrapForeignErrorf(werr.ErrUnexportedIdentifier,
					"applyToExports: identifier %q not exported by %s", name, lib.Name.SchemeString())
			}
			delete(result, name)
		}
		return result, nil
	case importModRename:
		// Validate that every rename SOURCE name is in the current name set, mirroring
		// the only/except checks. R7RS §5.6: rename maps an exported (post-prior-modifier)
		// identifier to a new name; a source name that is absent denotes nothing, so
		// silently no-op'ing it would mask a user error. Reject instead.
		for oldName := range p.renames {
			_, ok := result[oldName]
			if !ok {
				return nil, werr.WrapForeignErrorf(werr.ErrUnexportedIdentifier,
					"applyToExports: rename source %q not exported by %s", oldName, lib.Name.SchemeString())
			}
		}
		renamed := make(map[string]string)
		for localName, externalName := range result {
			newName, ok := p.renames[localName]
			if !ok {
				newName = localName
			}
			// Two source names collapsing to one target (e.g. (rename LIB (car kar)
			// (cdr kar)), or a rename target shadowing a pass-through name) would bind
			// one name to two different exports. R7RS §5.6 forbids importing a name with
			// two different bindings; reject rather than silently drop one by map order.
			existing, dup := renamed[newName]
			if dup && existing != externalName {
				return nil, werr.WrapForeignErrorf(werr.ErrDuplicateBinding,
					"applyToExports: rename binds %q to two different exports (%q and %q) in %s",
					newName, existing, externalName, lib.Name.SchemeString())
			}
			renamed[newName] = externalName
		}
		return renamed, nil
	case importModPrefix:
		prefixed := make(map[string]string)
		for localName, externalName := range result {
			prefixed[p.prefix+localName] = externalName
		}
		return prefixed, nil
	}
	return nil, werr.WrapForeignErrorf(werr.ErrInternal,
		"applyToExports: unknown import modifier kind %d", int(p.kind))
}

// CopyLibraryBindingsToEnv copies exported bindings from a library to an environment.
// bindings is the map from localName -> externalName produced by ApplyToExports.
// Both runtime and syntax bindings are copied.
// This is a convenience wrapper that imports to phase 0 (runtime).
func CopyLibraryBindingsToEnv(lib *CompiledLibrary, bindings map[string]string, targetEnv *environment.EnvironmentFrame) error {
	return CopyLibraryBindingsToEnvAtPhase(lib, bindings, targetEnv, environment.PhaseRuntime)
}

// ResolvedImportSet holds the result of parsing and loading an import set.
// This is the shared prefix of all import processing: parse the import set
// datum, load the named library, and apply modifiers (only, except, prefix,
// rename) to produce the final binding map.
type ResolvedImportSet struct {
	ImportSet *ImportSet
	Library   *CompiledLibrary
	Bindings  map[string]string // localName -> externalName
}

// resolveImportSet parses an import set datum, loads the library, and applies
// modifiers to produce the resolved binding map.
//
// The env parameter is used only for library loading (to find the library
// registry and resolve paths). It is NOT the target for binding installation.
func resolveImportSet(ctx context.Context, datum values.Value, env *environment.EnvironmentFrame, evaluator machine.MacroEvaluator) (*ResolvedImportSet, error) {
	importSet, err := ParseImportSetFromDatum(ctx, datum)
	if err != nil {
		return nil, err
	}

	lib, err := LoadLibrary(ctx, importSet.LibraryName, env, evaluator)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "import: failed to load library %s",
			importSet.LibraryName.SchemeString())
	}

	bindings, err := importSet.ApplyToExports(lib)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "import: error applying modifiers for %s",
			importSet.LibraryName.SchemeString())
	}

	return &ResolvedImportSet{
		ImportSet: importSet,
		Library:   lib,
		Bindings:  bindings,
	}, nil
}

// ResolveAndInstallImportSet resolves an import set and installs bindings into
// env. Used for top-level imports (both expander and compiler). Library-internal
// imports share the resolution step (resolveImportSet) but use
// copyLibraryBindingsDirect for installation.
//
// The phase argument is import-observer metadata only; it does NOT select the
// install phase. That comes from composePhaseShift below, which combines the
// environment's own phase level with the import set's for-syntax/for-meta shift.
func ResolveAndInstallImportSet(ctx context.Context, datum values.Value, env *environment.EnvironmentFrame, stage ImportStage, evaluator machine.MacroEvaluator) error {
	res, err := resolveImportSet(ctx, datum, env, evaluator)
	if err != nil {
		return err
	}

	fireImportObserver(env, res.Library, res.Bindings, LibraryName{}, stage)

	// Compose the parsed for-syntax/for-meta shift with the current expansion
	// phase, not a hardcoded 0: an (import (for-syntax M)) written inside a
	// phase-N transformer body places M at phase N+1. At the top level
	// env.PhaseLevel() == 0, so the composed shift equals PhaseShift and behavior
	// is unchanged (level-0 identity). composePhaseShift reuses the int8 overflow
	// guard (werr.ErrInvalidArgument).
	targetPhase, err := composePhaseShift("import", env.PhaseLevel(), res.ImportSet.PhaseShift)
	if err != nil {
		return err
	}

	err = CopyLibraryBindingsToEnvAtPhase(res.Library, res.Bindings, env, targetPhase)
	if err != nil {
		return werr.WrapForeignErrorf(err, "import: error copying bindings from %s",
			res.ImportSet.LibraryName.SchemeString())
	}

	return nil
}

// ExportedBinding resolves one of the library's exportable bindings by its
// internal name, using the same hygienic rule the import path uses. Callers
// outside this package (the doc-registration observer) must go through this
// rather than reaching into lib.Env with a bare-name lookup, or they will
// disagree with what an import actually installs.
func (p *CompiledLibrary) ExportedBinding(internalName string) (*environment.Binding, bool) {
	binding, _, found := findLibraryBinding(p, internalName)
	return binding, found
}

// findLibraryBinding searches every phase the library's OWN registry has
// actually instantiated — not just runtime, expand, and compile — for a
// binding with the given internal name. The boolean reports whether a binding
// was found; when false, the binding pointer is nil and the phase value is
// meaningless. Callers must check the boolean (or the binding pointer) before
// relying on the returned phase — the phase return cannot carry a sentinel
// "not found" value because every non-negative Phase is a valid result.
//
// The phase list comes from lib.Env.PresentPhases(), ascending, so a name a
// library binds via nested begin-for-syntax at phase 3 or above is reachable
// too; the old {runtime, expand, compile} literal truncated the tower (design
// Phase D, closing memory/2026-08-04-library-phase-isolation-impl.local.md's Q2
// residual). Ascending order is what makes the first hit the LOWEST phase,
// preserving runtime-first precedence when a name is bound at more than one —
// pinned by TestFindLibraryBindingPrefersRuntimeOverExpand.
//
// Resolution is HYGIENIC, keyed on the library's own scope (CompiledLibrary.Scope),
// not by bare name. Three cases, all decided by maximal subset resolution rather
// than by slot-insertion order:
//
//   - the library defines the name: the binder carries {libScope} and outranks
//     an ambient import of the same name sitting at {};
//   - the library only re-exports the name: only the import's {} slot exists,
//     and {} ⊆ {libScope}, so it still resolves;
//   - the name was introduced by a macro TEMPLATE inside the library body: the
//     binder carries the intro scope, which is not a subset of {libScope}, so
//     it does NOT resolve and cannot be exported. That is deliberate (R7RS
//     §4.3.2): an identifier the library author never wrote is not part of the
//     library's interface. validateLibraryExports turns the miss into an
//     eager error at define-library time.
//
// A name arriving through a macro PATTERN VARIABLE (define-record-type's
// accessors, any (mk name v) form) carries {libScope} like a hand-written
// binder, so those stay exportable.
func findLibraryBinding(lib *CompiledLibrary, internalName string) (*environment.Binding, environment.Phase, bool) {
	// exportScopes stays a concrete slice: nil and empty are the same query under
	// ScopeSet (values.ScopesOf), so this is the ambient (empty) set, never the
	// wildcard.
	exportScopes := []*syntax.Scope{}
	if lib.Scope != nil {
		exportScopes = append(exportScopes, lib.Scope)
	}

	libSym := values.NewSymbol(internalName)
	for _, phase := range lib.Env.PresentPhases() {
		env := lib.Env.AtPhase(phase)
		if env == nil {
			continue
		}
		binding := env.GetBinding(libSym, syntax.ScopesOf(exportScopes))
		if binding != nil {
			return binding, phase, true
		}
	}
	return nil, environment.PhaseRuntime, false
}

// importConflicts reports whether installing incoming under a local name whose
// own-frame binding already exists would bind one identifier to two DIFFERENT
// bindings — an error per R7RS §5.6 ("it is an error to import the same identifier
// more than once with different bindings"). Only a prior IMPORTED binding counts:
//
//   - a re-import of the same binding (a diamond — two libraries re-exporting one
//     source, or re-importing the same library) is permitted;
//   - a pre-existing user definition is not an import and is left to shadow.
//
// The second bullet was FALSE until the base install moved to T2: the define and
// the import shared one (ExactPhase(0), mutable) slot, so the import assigned
// through the define instead of being shadowed by it — measured, (define map 1)
// then (import (scheme base)) left one slot whose value went 1 ->
// #<case-lambda-closure> and whose meta went imported=false -> imported=true, and
// this guard never fired because the pre-existing binding was not IsImported().
// Now the two live at different coordinates, `!created` can only mean "an import
// already sits here", and the guard's own precondition is what makes the bullet
// true rather than aspirational.
//
// Whether the two denote the same definition (diamond) or two definitions of one name
// (conflict) is decided by sameImportedBinding — see its doc for the by-name comparison
// and why it is used instead of value identity.
func importConflicts(existing, incoming *environment.Binding) bool {
	if existing == nil || incoming == nil {
		return false
	}
	if !existing.IsImported() {
		return false
	}
	ev := existing.Value()
	iv := incoming.Value()
	if ev == nil || iv == nil {
		// Defensive: a found binding's value is never Go-nil in practice (a freshly
		// created binding holds values.Void, not nil), so this guards an upstream-bug
		// shape rather than a reachable path; treat an absent value as "cannot prove a
		// conflict" rather than risk a spurious one.
		return false
	}
	return !sameImportedBinding(ev, iv)
}

// sameImportedBinding reports whether two imported values denote the same underlying
// definition (a diamond / re-export) rather than two different definitions sharing one
// name (a conflict). Closures compare by NAME; everything else by EqualTo.
//
// Why by name and not value identity for closures: a re-export does not preserve a
// single closure value. An ambient definition (a bootstrap procedure or macro) is
// RECOMPILED into each manifest library that re-exports it, so the copies have distinct
// template/env/pointer and EqualTo would wrongly report a legitimate re-export as a
// conflict (verified: (scheme base) cddr vs (scheme cxr) cddr; delay across (scheme
// base)/(scheme lazy)/(scheme r5rs)). The name is the signal that survives
// recompilation, so equal names mark these as the one logical binding (a diamond).
//
// The EqualTo default still does real work for non-closure values: a case-lambda
// re-exported through an importing library SHARES its value pointer (EqualTo identity →
// diamond), while two genuinely different case-lambdas differ structurally (EqualTo
// unequal → conflict). This is what catches the one genuine stdlib collision — (scheme
// base) string-map vs (srfi 13) string-map, both name-less CaseLambdaClosures.
//
// Deliberate, IRREDUCIBLE gap: two DIFFERENT definitions under one name that the name
// cannot distinguish are treated as a diamond and silently last-import-wins. This covers
// name-less closures (macro transformers and var-form-defined procedures, whose template
// name is empty, so "" == "" reads as same) and same-named function-form procedures. The
// only signal that could separate "same definition, recompiled-and-re-exported" from
// "different definition, same name" is a definition origin (source location) — and that
// was rejected because it falsely flags the ubiquitous, legal define-over-import shadow
// ((import (scheme base)) then (define (zero? x) …)). No such hidden clash exists in the
// bundled stdlib (the one real collision, string-map, is caught via EqualTo above).
func sameImportedBinding(a, b values.Value) bool {
	switch av := a.(type) {
	case *machine.ForeignClosure:
		bv, ok := b.(*machine.ForeignClosure)
		return ok && av.Name() == bv.Name()
	case *machine.MachineClosure:
		bv, ok := b.(*machine.MachineClosure)
		return ok && av.Name() == bv.Name()
	default:
		return a.EqualTo(b)
	}
}

// importPlacement selects the store TIER an import install lands on. It is a
// parameter rather than a property of the frame because the two tiers differ in
// whether a later define of the same name SUPERSEDES the import or SHADOWS it,
// and only one install site can safely take the shadowable tier — see
// installImportedBinding's doc.
type importPlacement int

const (
	// placementShadowable puts the import at T2, (ExactPhase(0), sealed), where a
	// user top-level define's own (ExactPhase(0), mutable) T1 slot outranks it. A
	// define then shadows the import instead of assigning through it.
	placementShadowable importPlacement = iota
	// placementInPlace keeps the historical T1 coordinates,
	// (ExactPhase(N), mutable), where a same-name define shares the slot and
	// supersedes the import by assignment.
	placementInPlace
)

// installImportedBinding installs source into env under localSym, and is the
// single implementation behind every import install site (base phase, propagated
// phase, direct library-internal, and the expand-phase copy of a syntax binding).
//
// The binding is created AMBIENT, under the empty scope set. That is what makes
// an imported name behave like one: a plain top-level reference carries the empty
// set and reaches it, and the importing unit's own (define ...) of the same name
// carries the empty set too.
//
// # Which TIER, and why it is not the same answer at every site
//
// Historically every install went through the view, i.e.
// MaybeCreateOwnGlobalBinding, whose writeCoordinates yield
// (ExactPhase(N), mutable) — T1, the same coordinates a user top-level define
// writes. Sharing the slot made a define an ASSIGNMENT through the import, which
// is how (define map 1) followed by (import (scheme base)) silently clobbered the
// define: one slot, value 1 -> #<case-lambda-closure>, meta imported false ->
// true. importConflicts' own doc comment says "a pre-existing user definition is
// not an import and is left to shadow" — that sentence was FALSE, and moving the
// base install to T2 is what makes it true.
//
// placementShadowable therefore writes (ExactPhase(0), sealed), bypassing the
// view, because no view can produce that coordinate: writeCoordinates maps
// sealed-at-phase-0 to AnyPhase() (T3, the ambient startup set). T1 mutable
// outranks T2 sealed, so a define shadows while the import stays visible when no
// define exists.
//
// # THE HAZARD, and why only ONE site takes the shadowable tier
//
// (ExactPhase(0), sealed) is an EMPTY coordinate — that is the whole reason it is
// safe. (ExactPhase(1), sealed) is NOT: bootstrap macros and primitive expanders
// live there (primitive_expanders_registry.go registers through
// SealedWriteViewAt(PhaseExpand); `when` is present in SealedSlots()). Relocating
// a phase-1 install would land an imported macro on exactly a bootstrap macro's
// coordinates with the same ambient scope set, so CreateGlobalBindingAt REUSES
// the slot, created == false, importConflicts returns false (the bootstrap macro
// is not IsImported()), and SetOwnGlobalValue overwrites the sealed ambient
// transformer IN PLACE, ENGINE-WIDE — then markBindingImported stamps the startup
// set as imported. Every compiled pin to it would then see the import's value,
// and no test would name it: from the outside, the import "works".
//
// So the phase is re-checked here rather than trusted from the call site: a
// placementShadowable install at any phase but 0 falls back to the view. A
// for-syntax import ((import (for (lib) expand))) routes its BASE install through
// AtPhase(1) and would otherwise reach the hazard through the safe-looking site.
//
// # One slot, four operations
//
// Creation, the conflict check, the value write, and the provenance stamp all
// address ONE slot — the one the create PINNED, under that ambient key. They used
// to be four separate lookups, three of them wildcard by bare name, which agree
// only while a name has a single slot per frame: a wildcard answer can be a
// hygienically distinct binding of the same name, or a parent frame's binding
// while `created` reports on this frame, putting the guard and the write on
// different variables.
func installImportedBinding(
	env *environment.EnvironmentFrame,
	localSym *values.Symbol,
	bt environment.BindingType,
	source *environment.Binding,
	exportName string,
	internalName string,
	sourceLib LibraryName,
	phaseContext string,
	placement importPlacement,
) error {
	ambient := []*syntax.Scope{}
	var idx *environment.GlobalIndex
	var created bool
	if placement == placementShadowable && env.PhaseLevel() == environment.PhaseRuntime {
		idx, created = env.GlobalEnvironment().CreateGlobalBindingAt(
			localSym, bt, ambient, environment.ExactPhase(env.PhaseLevel()), true)
	} else {
		idx, created = env.MaybeCreateOwnGlobalBinding(localSym, bt, ambient)
	}

	own := env.GlobalEnvironment()
	target := own.GetOwnGlobalBinding(idx)

	// A previously-imported binding of this local name that resolves to a
	// different binding is a conflicting import (R7RS §5.6): reject rather than
	// silently last-wins. `created` and `target` now come from the same predicate,
	// so the guard cannot be asked about a binding other than the one it protects.
	if !created && importConflicts(target, source) {
		return werr.WrapForeignErrorf(werr.ErrDuplicateBinding,
			"import: identifier %q from %s conflicts with a different existing import; disambiguate with (except ...), (prefix ...), or (rename ...)",
			localSym.Key, sourceLib.SchemeString())
	}

	err := own.SetOwnGlobalValue(idx, source.Value())
	if err != nil {
		return werr.WrapForeignErrorf(err,
			"import: failed to set binding for %s%s", localSym.Key, phaseContext)
	}
	markBindingImported(target, source, exportName, internalName, sourceLib)
	return nil
}

// CopyLibraryBindingsToEnvAtPhase copies exported bindings from a library to a specific phase.
// bindings is the map from localName -> externalName produced by ApplyToExports.
//
// Phase semantics:
//   - targetPhase == 0: Runtime import (default). Runtime bindings go to phase 0.
//     A syntax binding that came from the library's expand phase skips the phase-0
//     install (skipBase below) and lands only at phase 1, so it cannot shadow the
//     importer's own define-syntax.
//   - targetPhase > 0: For-syntax import. Bindings are shifted to the target phase.
//     Runtime bindings become available during macro expansion at targetPhase.
//     Syntax bindings follow the same skipBase rule: targetPhase+1 only.
//   - targetPhase < 0: For-template import. Bindings shifted to negative phase
//     (used for generating code that will run at a lower phase).
func CopyLibraryBindingsToEnvAtPhase(lib *CompiledLibrary, bindings map[string]string, targetEnv *environment.EnvironmentFrame, targetPhase environment.Phase) error {
	for localName, externalName := range bindings {
		internalName := lib.GetInternalName(externalName)
		if internalName == "" {
			internalName = externalName
		}

		libBinding, sourcePhase, found := findLibraryBinding(lib, internalName)
		if !found {
			return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "library %s exports %q but binding not found",
				lib.Name.SchemeString(), internalName)
		}

		// A syntax (macro) binding is an expand-phase concept: skip the base
		// (runtime, phase 0 at a top-level import) install so findLibraryBinding's
		// runtime-first probe cannot return it over the importer's own
		// define-syntax, which lands in the expand phase — the same shadowing
		// copyLibraryBindingsDirect avoids, so an imported macro the importer then
		// re-defines does not win. The source-phase propagation below is then its
		// sole install (sourcePhase == PhaseExpand for a macro). Skip only when
		// that propagation will run (sourcePhase > 0); a syntax binding with no
		// source phase still needs a home, so it falls through to the base install.
		skipBase := libBinding.BindingType() == environment.BindingTypeSyntax && sourcePhase > 0
		if !skipBase {
			// Create binding in the target at the base phase. This is the ONE site
			// that takes the shadowable tier: at targetPhase 0 it resolves to
			// (ExactPhase(0), sealed), an empty coordinate, so a user top-level
			// define gets its own T1 slot and shadows rather than assigning through
			// the import. At any other targetPhase installImportedBinding falls back
			// to the view — see the hazard in its doc.
			phaseEnv := targetEnv.AtPhase(targetPhase)
			localSym := values.NewSymbol(localName)
			err := installImportedBinding(phaseEnv, localSym, libBinding.BindingType(),
				libBinding, externalName, internalName, lib.Name, " at phase "+targetPhase.String(),
				placementShadowable)
			if err != nil {
				return err
			}
		}

		// Propagate to the source phase in the target so the binding is available
		// in the same phase it originated from. Syntax bindings (phase 1) need to
		// be in the expand phase for macro expansion; an auxiliary keyword is
		// ambient in its library env and is found at phase 0, so it never takes
		// this branch.
		if sourcePhase > 0 {
			// Phase is int8; a high for-meta target phase plus the source-phase
			// shift can overflow (e.g. 127+1 wraps to -128) and silently route the
			// binding into the wrong phase registry. Guard the sum at int width
			// before narrowing, mirroring composePhaseShift's parse-time check.
			phaseSum := int(targetPhase) + int(sourcePhase)
			if phaseSum > math.MaxInt8 {
				return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
					"import: propagation phase %d (target %d + source %d) exceeds max phase %d for %q from %s",
					phaseSum, int(targetPhase), int(sourcePhase), math.MaxInt8, localName, lib.Name.SchemeString())
			}
			propagatePhase := environment.Phase(phaseSum)
			// Same conflict guard as the base phase: the base phase catches most
			// clashes first; this closes the case where the base entry is created
			// fresh but the propagated (e.g. expand) entry already exists.
			propagateEnv := targetEnv.AtPhase(propagatePhase)
			propagateSym := values.NewSymbol(localName)
			// DELIBERATELY placementInPlace, and this is a REFUSAL, not an omission.
			//
			// The whole point of the propagation is that propagatePhase > 0, so the
			// shadowable tier would resolve to (ExactPhase(1), sealed) — which, unlike
			// (ExactPhase(0), sealed), is NOT an empty coordinate. Bootstrap macros
			// and primitive expanders live there (`when` is in SealedSlots()). An
			// imported macro of the same name would land on exactly those coordinates
			// with the same ambient scope set, so CreateGlobalBindingAt REUSES the
			// slot, created == false, importConflicts returns false (the bootstrap
			// macro is not IsImported()), SetOwnGlobalValue overwrites the sealed
			// ambient transformer IN PLACE and ENGINE-WIDE, and markBindingImported
			// stamps the startup set as imported. From the outside the import would
			// simply "work"; no existing test names it.
			//
			// Relocating this site is a SEPARATE DECISION that needs its own way to
			// keep imports off the startup set's coordinates — a distinct rank, or a
			// scope set that is not the ambient one. It is not a matter of passing the
			// other constant here. Until then a phase-1 define-syntax over an import
			// still supersedes in place, which is the known residual recorded in
			// TODO.md against the Imported arm of IsStable().
			err := installImportedBinding(propagateEnv, propagateSym, libBinding.BindingType(),
				libBinding, externalName, internalName, lib.Name, " propagated to phase "+propagatePhase.String(),
				placementInPlace)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

// ImportSpecInto parses a single import-spec datum, loads the named library,
// applies the import set's modifiers (only/except/prefix/rename), and copies
// the resulting bindings into targetEnv at the spec's phase shift. It is the
// shared core of the (environment ...), (make-namespace ...), and
// (namespace-require ...) primitives; op names the calling primitive for error
// context. callerEnv supplies the library registry for resolution.
func ImportSpecInto(ctx context.Context, specVal values.Value, callerEnv, targetEnv *environment.EnvironmentFrame, evaluator machine.MacroEvaluator, op string) error {
	importSet, err := ParseImportSetFromDatum(ctx, specVal)
	if err != nil {
		return werr.WrapForeignErrorf(err, "%s: invalid import spec", op)
	}

	lib, err := LoadLibrary(ctx, importSet.LibraryName, callerEnv, evaluator)
	if err != nil {
		return werr.WrapForeignErrorf(err, "%s: failed to load %s", op, importSet.LibraryName.SchemeString())
	}

	bindings, err := importSet.ApplyToExports(lib)
	if err != nil {
		return werr.WrapForeignErrorf(err, "%s: error in import set for %s", op, importSet.LibraryName.SchemeString())
	}

	err = CopyLibraryBindingsToEnvAtPhase(lib, bindings, targetEnv, importSet.PhaseShift)
	if err != nil {
		return werr.WrapForeignErrorf(err, "%s: error copying bindings from %s", op, importSet.LibraryName.SchemeString())
	}
	return nil
}

// copyLibraryBindingsDirect installs bindings from lib into targetEnv without
// AtPhase routing. This is used for library-internal imports where targetEnv
// is a child runtime frame whose AtPhase() would route to the parent's phase
// registry rather than the library's own environment.
//
// Imported syntax (macro) bindings install into targetEnv.Expand() only;
// variable bindings install into targetEnv (the runtime frame). See the phase
// selection below.
func copyLibraryBindingsDirect(lib *CompiledLibrary, bindings map[string]string, targetEnv *environment.EnvironmentFrame) error {
	for localName, externalName := range bindings {
		internalName := lib.GetInternalName(externalName)
		if internalName == "" {
			internalName = externalName
		}

		importedBinding, _, found := findLibraryBinding(lib, internalName)
		if !found {
			return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "import: %s exports %q but binding not found",
				lib.Name.SchemeString(), internalName)
		}

		// A syntax (macro) binding is an expand-phase concept: install it into the
		// expand frame only. Mirroring an imported macro into the runtime frame
		// too made findLibraryBinding's runtime-first probe — and the library
		// body's own macro resolution — return the imported macro even when the
		// importing library re-defined that name. A library-body define-syntax
		// stores into the expand frame at the library scope, so the runtime mirror
		// shadowed it, and the library exported (and its own body resolved) the
		// imported macro instead of its own. A variable binding needs no such care
		// because it and the library's own define share the one runtime frame, so
		// the local define already shadows the import.
		//
		// Conflict detection (installImportedBinding) still mirrors
		// CopyLibraryBindingsToEnvAtPhase, so a library declaration importing two
		// libraries with different bindings for one name is rejected per R7RS §5.6,
		// not just a top-level program import.
		localSym := values.NewSymbol(localName)
		installEnv := targetEnv
		phaseNote := ""
		if importedBinding.BindingType() == environment.BindingTypeSyntax {
			installEnv = targetEnv.Expand()
			phaseNote = " in expand phase"
		}
		// DELIBERATELY placementInPlace for BOTH arms, and this is a REFUSAL.
		//
		// The syntax arm installs into targetEnv.Expand(), so it carries exactly the
		// (ExactPhase(1), sealed) hazard spelled out at the propagated install above:
		// an imported macro would overwrite a same-named bootstrap transformer in the
		// sealed startup set, engine-wide and silently.
		//
		// The variable arm is at phase 0 and would be coordinate-safe, but it is left
		// alone on purpose. targetEnv here is a library body's own child runtime
		// frame, where the shadow-vs-supersede question this relocation answers does
		// not arise the same way: the library's own define carries the LIBRARY scope
		// and already gets its own slot, so it shadows the ambient import without any
		// tier change (the comment above says so). Moving it would change a working
		// resolution for no stated defect, and split one function's two arms across
		// two tiers on no principle. It is a separate decision.
		err := installImportedBinding(installEnv, localSym, importedBinding.BindingType(),
			importedBinding, externalName, internalName, lib.Name, phaseNote,
			placementInPlace)
		if err != nil {
			return err
		}
	}
	return nil
}
