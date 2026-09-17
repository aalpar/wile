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
	"maps"
	"math"
	"slices"

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
func markBindingImported(target, source *environment.Binding, exportName, internalName string, sourceLib *CompiledLibrary, sourcePhase environment.Phase) {
	if target == nil {
		return
	}
	// Import-provenance root (plan 2026-07-24-free-identifier-origin): propagate
	// the source's already-resolved root when the source is itself imported (a
	// re-export hop), else synthesize the root from the defining name in the
	// source library. Keyed on internalName — the name inside sourceLib that
	// defines the binding — and sourcePhase, the phase sourceLib stores it at, so
	// a renamed export or a shifted import does not fork one binding's identity. This is the signal SameBinding reads for
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
			// Fallback for a source with no pre-stamped origin (a Go extension
			// library, which skips stampLibraryExportOrigins): the same root the
			// stamp would have given it.
			origin = exportRoot(sourceLib, source, internalName, sourcePhase)
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

// stampLibraryExportOrigins gives each export not imported into lib its
// provenance root (exportRoot), intrinsically at library finalization — before
// any import. So a define-site binding carries the same origin an import of it
// would otherwise synthesize, which is what makes identifier equality
// (free-identifier=?, ER-compare's definition-site rename) match a library's
// internal binding against an import of itself.
//
// A re-exported binding already carries the propagated root of its true source
// (it was imported into this library, so its Origin is non-nil), and is left
// untouched. Runs once per library compile, single-threaded; the nil-guard keeps
// it idempotent and preserves a re-export's root.
func stampLibraryExportOrigins(lib *CompiledLibrary) {
	for key, internalName := range lib.Exports {
		binding, phase, found := findLibraryBinding(lib, internalName, key.Phase)
		if !found || binding == nil || binding.Origin() != nil {
			continue
		}
		root := exportRoot(lib, binding, internalName, phase)
		binding.UpdateMeta(func(m *environment.BindingMeta) bool {
			if m.Origin != nil {
				return false
			}
			m.Origin = root
			return true
		})
	}
}

// exportRoot returns the provenance root of a binding lib exports that was not
// imported into it: internalName, stored at phase. With no origin it is either
// defined by lib or supplied by the engine base lib's environment is built from,
// and only a definition in lib carries lib's scope. A base binding is rooted at
// environment.BaseOriginLib, so every library's copy of it is one binding.
func exportRoot(lib *CompiledLibrary, binding *environment.Binding, internalName string, phase environment.Phase) *environment.OriginRef {
	rootLib := environment.BaseOriginLib
	if lib.Scope != nil && slices.Contains(binding.Scopes(), lib.Scope) {
		rootLib = lib.Name.Key()
	}
	return &environment.OriginRef{RootLib: rootLib, RootName: internalName, RootPhase: phase}
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
//   - the parser dispatch in parseLibraryImportSetFromDatum (import_set_datum.go)
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
// bindings as a map of (export phase, local name) -> external name (the name in
// the library). The modifiers act on names at every phase at once, as Racket's
// only-in, except-in, prefix-in and rename-in do: (only LIB x) keeps x at each
// phase LIB exports it.
func (p *ImportSet) ApplyToExports(lib *CompiledLibrary) (map[ExportKey]string, error) {
	result := make(map[ExportKey]string, len(lib.Exports))

	// Start with all exports.
	for key := range lib.Exports {
		result[key] = key.Name
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

// apply transforms a (phase, local name) -> external name map by one import
// modifier. A name the modifier lists must be present at some phase.
func (p *importModifier) apply(result map[ExportKey]string, lib *CompiledLibrary) (map[ExportKey]string, error) {
	switch p.kind {
	case importModOnly:
		filtered := make(map[ExportKey]string)
		for key, externalName := range result {
			if p.ids.ContainsOne(key.Name) {
				filtered[key] = externalName
			}
		}
		err := p.requireListed(filtered, lib)
		if err != nil {
			return nil, err
		}
		return filtered, nil
	case importModExcept:
		err := p.requireListed(result, lib)
		if err != nil {
			return nil, err
		}
		maps.DeleteFunc(result, func(key ExportKey, _ string) bool {
			return p.ids.ContainsOne(key.Name)
		})
		return result, nil
	case importModRename:
		// Validate that every rename SOURCE name is in the current name set, mirroring
		// the only/except checks. R7RS §5.6: rename maps an exported (post-prior-modifier)
		// identifier to a new name; a source name that is absent denotes nothing, so
		// silently no-op'ing it would mask a user error. Reject instead.
		for oldName := range p.renames {
			if !hasExportName(result, oldName) {
				return nil, werr.WrapForeignErrorf(werr.ErrUnexportedIdentifier,
					"applyToExports: rename source %q not exported by %s", oldName, lib.Name.SchemeString())
			}
		}
		renamed := make(map[ExportKey]string)
		for key, externalName := range result {
			newName, ok := p.renames[key.Name]
			if !ok {
				newName = key.Name
			}
			newKey := ExportKey{Phase: key.Phase, Name: newName}
			// Two source names collapsing to one target at one phase (e.g. (rename LIB
			// (car kar) (cdr kar)), or a rename target shadowing a pass-through name)
			// would bind one name to two different exports. R7RS §5.6 forbids importing a
			// name with two different bindings; reject rather than silently drop one by
			// map order.
			existing, dup := renamed[newKey]
			if dup && existing != externalName {
				return nil, werr.WrapForeignErrorf(werr.ErrDuplicateBinding,
					"applyToExports: rename binds %q to two different exports (%q and %q) in %s",
					newName, existing, externalName, lib.Name.SchemeString())
			}
			renamed[newKey] = externalName
		}
		return renamed, nil
	case importModPrefix:
		prefixed := make(map[ExportKey]string)
		for key, externalName := range result {
			prefixed[ExportKey{Phase: key.Phase, Name: p.prefix + key.Name}] = externalName
		}
		return prefixed, nil
	}
	return nil, werr.WrapForeignErrorf(werr.ErrInternal,
		"applyToExports: unknown import modifier kind %d", int(p.kind))
}

// requireListed rejects a name in p.ids that names nothing in result at any phase.
func (p *importModifier) requireListed(result map[ExportKey]string, lib *CompiledLibrary) error {
	for name := range p.ids {
		if !hasExportName(result, name) {
			return werr.WrapForeignErrorf(werr.ErrUnexportedIdentifier,
				"applyToExports: identifier %q not exported by %s", name, lib.Name.SchemeString())
		}
	}
	return nil
}

// hasExportName reports whether result binds name at any phase.
func hasExportName(result map[ExportKey]string, name string) bool {
	for key := range result {
		if key.Name == name {
			return true
		}
	}
	return false
}

// CopyLibraryBindingsToEnv copies exported bindings from a library to an environment.
// bindings is the map from (phase, localName) -> externalName produced by ApplyToExports.
// Both runtime and syntax bindings are copied.
// This is a convenience wrapper that imports to phase 0 (runtime).
func CopyLibraryBindingsToEnv(lib *CompiledLibrary, bindings map[ExportKey]string, targetEnv *environment.EnvironmentFrame) error {
	return CopyLibraryBindingsToEnvAtPhase(lib, bindings, targetEnv, environment.PhaseRuntime)
}

// ResolvedImportSet holds the result of parsing and loading an import set.
// This is the shared prefix of all import processing: parse the import set
// datum, load the named library, and apply modifiers (only, except, prefix,
// rename) to produce the final binding map.
type ResolvedImportSet struct {
	ImportSet *ImportSet
	Library   *CompiledLibrary
	Bindings  map[ExportKey]string // (export phase, localName) -> externalName
}

// resolveImportSets parses an import set datum, loads each library it denotes,
// and applies modifiers to produce the resolved binding maps, one per library
// (a phase shift may name several; see ParseImportSetsFromDatum).
//
// The env parameter is used only for library loading (to find the library
// registry and resolve paths). It is NOT the target for binding installation.
func resolveImportSets(ctx context.Context, datum values.Value, env *environment.EnvironmentFrame, evaluator machine.MacroEvaluator) ([]*ResolvedImportSet, error) {
	importSets, err := ParseImportSetsFromDatum(ctx, datum)
	if err != nil {
		return nil, err
	}

	q := make([]*ResolvedImportSet, 0, len(importSets))
	for _, importSet := range importSets {
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

		q = append(q, &ResolvedImportSet{
			ImportSet: importSet,
			Library:   lib,
			Bindings:  bindings,
		})
	}
	return q, nil
}

// ResolveAndInstallImportSet resolves an import set and installs bindings into
// env. Used for top-level imports (both expander and compiler). Library-internal
// imports share the resolution step (resolveImportSets) but use
// copyLibraryBindingsDirect for installation.
//
// The stage argument is import-observer metadata only; it does NOT select the
// install phase. That comes from composePhaseShift below, which combines the
// environment's own phase level with the import set's for-syntax/for-meta shift.
func ResolveAndInstallImportSet(ctx context.Context, datum values.Value, env *environment.EnvironmentFrame, stage ImportStage, evaluator machine.MacroEvaluator) error {
	resolved, err := resolveImportSets(ctx, datum, env, evaluator)
	if err != nil {
		return err
	}
	for _, res := range resolved {
		err = installResolvedImportSet(env, res, stage)
		if err != nil {
			return err
		}
	}
	return nil
}

// installResolvedImportSet installs one resolved library import into env for
// ResolveAndInstallImportSet.
func installResolvedImportSet(env *environment.EnvironmentFrame, res *ResolvedImportSet, stage ImportStage) error {
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

// ExportedBinding resolves the binding a phase-0 export of internalName denotes,
// using the same hygienic rule the import path uses. Callers outside this
// package (the doc-registration observer) must go through this rather than
// reaching into lib.Env with a bare-name lookup, or they will disagree with what
// an import actually installs.
func (p *CompiledLibrary) ExportedBinding(internalName string) (*environment.Binding, bool) {
	binding, _, found := findLibraryBinding(p, internalName, environment.PhaseRuntime)
	return binding, found
}

// findLibraryBinding resolves the binding an export of internalName at phase
// denotes, and the phase that binding is stored at. The boolean reports whether
// one was found; when false, the binding pointer is nil and the phase value is
// meaningless.
//
// Two frames can hold it, because Wile stores a define-syntax keyword one phase
// above the code that uses it:
//
//   - at phase itself, any binding but a define-syntax keyword: a variable, or a
//     primitive keyword such as if (FormKeyword) or syntax-rules (SyntaxCompiler);
//   - at phase+1, a keyword: a define-syntax transformer, or a primitive
//     expander, which is registered at absolute phase 1 for phase-0 use
//     (let-syntax).
//
// So a define-syntax keyword stored AT phase, which serves phase-1 code, and a
// variable stored at phase+1 are refused. That is Racket's rule: a plain
// provide sees phase 0 only, (for-syntax x) phase 1 only, and a
// begin-for-syntax define is not exportable without for-syntax. The second
// bullet cannot tell a primitive keyword stored at phase+1 for phase+1 code
// (if at phase 1) from one stored there for phase code (let-syntax). Measured
// on the default startup set, it does not have to: the only primitive keywords
// present at a phase but not the phase below are the phase-1 primitive
// expanders, which are the second kind.
//
// Only phases the library's own registry has instantiated are probed, so an
// export lookup never creates a phase frame.
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
func findLibraryBinding(lib *CompiledLibrary, internalName string, phase environment.Phase) (*environment.Binding, environment.Phase, bool) {
	// exportScopes stays a concrete slice: nil and empty are the same query under
	// ScopeSet (values.ScopesOf), so this is the ambient (empty) set, never the
	// wildcard.
	exportScopes := []*syntax.Scope{}
	if lib.Scope != nil {
		exportScopes = append(exportScopes, lib.Scope)
	}
	scopes := syntax.ScopesOf(exportScopes)
	libSym := values.NewSymbol(internalName)
	present := lib.Env.PresentPhases()

	binding := libraryBindingAt(lib, present, libSym, scopes, phase)
	if binding != nil && binding.BindingType() != environment.BindingTypeSyntax {
		return binding, phase, true
	}
	keywordPhase, err := composePhaseShift("export", phase, environment.PhaseExpand)
	if err != nil {
		return nil, phase, false
	}
	binding = libraryBindingAt(lib, present, libSym, scopes, keywordPhase)
	if binding != nil && binding.BindingType() != environment.BindingTypeVariable {
		return binding, keywordPhase, true
	}
	return nil, phase, false
}

// libraryBindingAt resolves sym in lib's phase frame, or returns nil when present
// (lib.Env.PresentPhases()) does not list phase: AtPhase would create the frame.
func libraryBindingAt(lib *CompiledLibrary, present []environment.Phase, sym *values.Symbol, scopes syntax.ScopeSet, phase environment.Phase) *environment.Binding {
	_, ok := slices.BinarySearch(present, phase)
	if !ok {
		return nil
	}
	return lib.Env.AtPhase(phase).GetBinding(sym, scopes)
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
// The second bullet was FALSE until the base install moved off tierExactMutable
// to (phase 0, sealed): the define and
// the import shared one (phase 0, mutable) slot, so the import assigned
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
	// One library's name at two phases is two bindings, and nothing in the
	// values can say so: a procedure compares by name and a macro has none.
	// The roots can. Roots from different libraries still go to the value
	// comparison below.
	eo, io := existing.Origin(), incoming.Origin()
	if eo != nil && io != nil && eo.RootLib == io.RootLib && eo.RootName == io.RootName {
		return eo.RootPhase != io.RootPhase
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
	// placementShadowable puts the import at (phase 0, sealed) stamped Imported,
	// so it ranks tierExactImported, where a user top-level define's own
	// (phase 0, mutable) tierExactMutable slot outranks it. A
	// define then shadows the import instead of assigning through it.
	placementShadowable importPlacement = iota
	// placementInPlace keeps the historical mutable-tier coordinates,
	// (phase N, mutable), where a same-name define shares the slot and
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
// (phase N, mutable) — tierExactMutable, the same coordinates a user top-level define
// writes. Sharing the slot made a define an ASSIGNMENT through the import, which
// is how (define map 1) followed by (import (scheme base)) silently clobbered the
// define: one slot, value 1 -> #<case-lambda-closure>, meta imported false ->
// true. importConflicts' own doc comment says "a pre-existing user definition is
// not an import and is left to shadow" — that sentence was FALSE, and moving the
// base install to (phase 0, sealed) is what makes it true.
//
// placementShadowable therefore writes (phase 0, sealed) directly rather
// than through the view. tierExactMutable outranks tierExactImported, so a
// define shadows while the import stays visible when no define exists.
//
// # THE HAZARD, and why only ONE site takes the shadowable tier
//
// This doc used to argue that (phase 0, sealed) is safe BECAUSE IT IS
// EMPTY, and that no view could produce it since writeCoordinates mapped a
// sealed phase-0 write to the ANY coordinate. Stage A falsified both halves: the ambient
// tier is deleted, writeCoordinates produces (phase 0, sealed) for exactly
// that write, and the sealed base now lives at this very coordinate. The
// coordinate is the most crowded one in the store.
//
// What makes it safe NOW is a THIRD TIER, and this paragraph used to say
// something else that was measurably false.
//
// It said a predicate did the job: the engine's own base row is a SEALED,
// OWN-INSTALLS-ONLY BulkSource that refuses any binding carrying Imported meta,
// "so an import landing BESIDE a base slot at this coordinate cannot be mistaken
// for the base … The two coexist because they are distinguishable, not because
// one of them is absent."
//
// There was no beside. A predicate can only separate two slots, and the WRITE
// path was producing one: CreateGlobalBindingAt's reuse rule matches on
// (phase, sealed, scopes), an import writes an EMPTY scope set where the base
// carries NIL, and scopeSetsEqual(nil, []) is true. So `created == false`, the
// SetOwnGlobalValue below replaced the startup set's value with the library
// env's copy, and markBindingImported stamped the engine-shared base as
// imported — after which the own-installs-only predicate refused THE BASE.
// Measured 2026-09-09 (verified against master, bisected to 183171a1): one slot,
// same pointer, and `(import (scheme base))` alone stripped `not` from the
// phase-1 macro vocabulary and made every base primitive it covered
// user-deletable.
//
// environment.tierExactImported is the third coordinate the old paragraph said
// did not exist. An import now takes a slot of its OWN at this coordinate via
// CreateImportedGlobalBindingAt, ranks between the user's mutable tier and the
// startup set's, and the base's row floors above it. The predicate survives as a
// second, independent reason for the same answer rather than as the only one.
//
// The plan's D12 proposed instead routing every import through a bulk row of its
// own and deleting this placement. That was not taken: a library's exports are
// resolved with the LIBRARY's scope in the query (findLibraryBinding), so an
// import row needs a library-scoped source plus per-source-phase grouping —
// strictly more machinery for the same separation the predicate already gives.
// Recorded in the plan's Task 11.
//
// (phase 1, sealed) is still NOT available: bootstrap macros and primitive
// expanders live there (primitive_expanders_registry.go registers through
// SealedWriteViewAt(PhaseExpand); `when` is present in SealedSlots()). Relocating
// a phase-1 install would land an imported macro on exactly a bootstrap macro's
// coordinates with the same ambient scope set, so CreateGlobalBindingAt REUSES
// the slot, created == false, importConflicts returns false (the bootstrap macro
// is not IsImported()), and SetOwnGlobalValue overwrites the sealed transformer
// IN PLACE, ENGINE-WIDE — then markBindingImported stamps the startup set as
// imported. Every compiled pin to it would then see the import's value, and no
// test would name it: from the outside, the import "works".
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
	sourceLib *CompiledLibrary,
	sourcePhase environment.Phase,
	phaseContext string,
	placement importPlacement,
) error {
	ambient := []*syntax.Scope{}
	var idx *environment.GlobalIndex
	var created bool
	if placement == placementShadowable && env.PhaseLevel() == environment.PhaseRuntime {
		// CreateImportedGlobalBindingAt, not the plain form: the base's own
		// binding sits at exactly this coordinate with an equal scope set, so the
		// plain reuse rule hands back the STARTUP SET's slot and this function then
		// writes its value and its provenance onto the base. See that method's doc
		// for the measurement.
		idx, created = env.GlobalEnvironment().CreateImportedGlobalBindingAt(
			localSym, bt, ambient, env.PhaseLevel(), true)
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
			localSym.Key, sourceLib.Name.SchemeString())
	}

	err := own.SetOwnGlobalValue(idx, source.Value())
	if err != nil {
		return werr.WrapForeignErrorf(err,
			"import: failed to set binding for %s%s", localSym.Key, phaseContext)
	}
	markBindingImported(target, source, exportName, internalName, sourceLib, sourcePhase)
	return nil
}

// CopyLibraryBindingsToEnvAtPhase copies exported bindings from a library to a specific phase.
// bindings is the map from (phase, localName) -> externalName produced by ApplyToExports.
//
// Every phase below is shifted by the export's own phase: a (for-syntax x) export
// lands one phase above where a plain export of x would.
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
func CopyLibraryBindingsToEnvAtPhase(lib *CompiledLibrary, bindings map[ExportKey]string, targetEnv *environment.EnvironmentFrame, targetPhase environment.Phase) error {
	for localKey, externalName := range bindings {
		localName := localKey.Name
		exportPhase := localKey.Phase
		internalName := lib.GetInternalName(ExportKey{Phase: exportPhase, Name: externalName})
		if internalName == "" {
			internalName = externalName
		}

		libBinding, sourcePhase, found := findLibraryBinding(lib, internalName, exportPhase)
		if !found {
			return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "library %s exports %q but binding not found",
				lib.Name.SchemeString(), internalName)
		}
		basePhase, err := composePhaseShift("import", targetPhase, exportPhase)
		if err != nil {
			return err
		}

		// A syntax (macro) binding is an expand-phase concept: skip the base
		// (runtime, phase 0 at a top-level import) install, where it would be a
		// keyword for the phase below, and would not be shadowed by the importer's
		// own define-syntax, which lands in the expand phase. The source-phase
		// propagation below is then its sole install. findLibraryBinding returns a
		// syntax binding only from one phase above the export phase, so
		// sourcePhase > exportPhase always holds for one; the check keeps the skip
		// tied to the propagation that replaces it.
		skipBase := libBinding.BindingType() == environment.BindingTypeSyntax && sourcePhase > exportPhase
		if !skipBase {
			// Create binding in the target at the base phase. This is the ONE site
			// that takes the shadowable tier: at base phase 0 it resolves to
			// (phase 0, sealed) stamped Imported, which is tierExactImported — NOT
			// an empty coordinate, the startup set is at the same (phase, sealed)
			// pair and is separated by the stamp alone. A user top-level define
			// gets its own tierExactMutable slot above both and shadows rather than
			// assigning through the import. At any other base phase
			// installImportedBinding falls back to the view — see the hazard in its
			// doc, which is where the "empty coordinate" argument was falsified.
			phaseEnv := targetEnv.AtPhase(basePhase)
			localSym := values.NewSymbol(localName)
			err := installImportedBinding(phaseEnv, localSym, libBinding.BindingType(),
				libBinding, externalName, internalName, lib, sourcePhase, " at phase "+basePhase.String(),
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
		if sourcePhase > exportPhase {
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
			// shadowable tier would resolve to (phase 1, sealed) — which, unlike
			// (phase 0, sealed), is NOT an empty coordinate. Bootstrap macros
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
				libBinding, externalName, internalName, lib, sourcePhase, " propagated to phase "+propagatePhase.String(),
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
	importSets, err := ParseImportSetsFromDatum(ctx, specVal)
	if err != nil {
		return werr.WrapForeignErrorf(err, "%s: invalid import spec", op)
	}

	for _, importSet := range importSets {
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
	}
	return nil
}

// copyLibraryBindingsDirect installs bindings from lib into targetEnv at
// targetPhase. It is the DECLARATION-position install inside a define-library;
// the top-level path is ResolveAndInstallImportSet.
//
// Imported syntax (macro) bindings install one phase above targetPhase; variable
// bindings install at targetPhase itself. See the phase selection below.
//
// targetPhase composes the import set's for-syntax/for-template/for-meta shift
// with the library env's own phase, exactly as ResolveAndInstallImportSet does.
// It used to take no phase at all: ImportSet.PhaseShift was parsed, accepted and
// then read nowhere on this path, so every shift collapsed to 0 and a
// declaration-position (import (for-syntax (helper))) bound helper's exports at
// PHASE 0 — where a correctly shifted import leaves them unbound — while the
// phase that asked for them stayed empty. for-template's -1 was dropped
// identically, making a negative shift indistinguishable from no modifier, and
// nothing reported any of it. Filed 2026-09-09; the body position, which routes
// through ResolveAndInstallImportSet, was always correct.
//
// WHY NOT just call ResolveAndInstallImportSet here. Not because of the env:
// lib.Env.AtPhase(n) is the LIBRARY's own phase-n frame — Namespace.NewChildRuntime
// wires a registry onto the child precisely so it is not the parent's — and this
// function's syntax arm has always gone through AtPhase via Expand(). (An earlier
// version of this comment claimed the opposite; it was measurably false.) The real
// reason is the PLACEMENT TIER. CopyLibraryBindingsToEnvAtPhase installs
// placementShadowable, which would give a library env a sealed phase-0 answer,
// and TestBindingModelMatrix/imported_rename_shadows_set!_special_form pins that
// a library env is deliberately a flat island with no sealed tier of its own.
// Collapsing the two paths takes that decision as a silent side effect.
func copyLibraryBindingsDirect(lib *CompiledLibrary, bindings map[ExportKey]string, targetEnv *environment.EnvironmentFrame, targetPhase environment.Phase) error {
	for localKey, externalName := range bindings {
		localName := localKey.Name
		internalName := lib.GetInternalName(ExportKey{Phase: localKey.Phase, Name: externalName})
		if internalName == "" {
			internalName = externalName
		}

		importedBinding, sourcePhase, found := findLibraryBinding(lib, internalName, localKey.Phase)
		if !found {
			return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "import: %s exports %q but binding not found",
				lib.Name.SchemeString(), internalName)
		}
		// The import lands at targetPhase shifted by the export's own phase, and a
		// syntax binding is an expand-phase concept RELATIVE to that, so one above
		// it, not a hardcoded 1. Both composed through the int8 guard the
		// propagation install uses, so a for-meta near the ceiling is refused
		// rather than wrapping negative.
		basePhase, err := composePhaseShift("import", targetPhase, localKey.Phase)
		if err != nil {
			return err
		}
		syntaxPhase, err := composePhaseShift("import", basePhase, environment.PhaseExpand)
		if err != nil {
			return err
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
		installEnv := targetEnv.AtPhase(basePhase)
		phaseNote := " at phase " + basePhase.String()
		if importedBinding.BindingType() == environment.BindingTypeSyntax {
			installEnv = targetEnv.AtPhase(syntaxPhase)
			phaseNote = " in expand phase " + syntaxPhase.String()
		}
		// DELIBERATELY placementInPlace for BOTH arms, and this is a REFUSAL.
		//
		// The syntax arm installs into targetEnv.Expand(), so it carries exactly the
		// (phase 1, sealed) hazard spelled out at the propagated install above:
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
		err = installImportedBinding(installEnv, localSym, importedBinding.BindingType(),
			importedBinding, externalName, internalName, lib, sourcePhase, phaseNote,
			placementInPlace)
		if err != nil {
			return err
		}
	}
	return nil
}
