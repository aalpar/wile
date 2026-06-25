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

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/environment"
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
// the imported target via stampImportedInlineHOF: keyed by export name so a
// renamed import still carries it, and applied to the fresh per-import target (not
// the shared source) so it is race-safe under concurrent imports. stampImported-
// InlineHOF stamps ONLY import-gated HOFs (fold), so a same-named re-export of a
// SEALED-BASE HOF — e.g. SRFI-13's string-map, a different procedure from R7RS
// string-map — is never stamped here and never mis-inlined with the R7RS template.
// The sealed-base HOFs are stamped only at their real home (StampInlineHOFs). The
// import path is also the only library seam, so a user's own (define …) of a HOF
// name is never stamped here.
func markBindingImported(target, source *environment.Binding, exportName string, sourceLib LibraryName) {
	if target == nil {
		return
	}
	m := target.EnsureMeta()
	m.Imported = true
	if source != nil {
		m.CaptureSafe = source.IsCaptureSafe()
		// Carry the docstring across the import boundary so ,doc and the doc
		// tooling find it on the imported binding (e.g. a (wile control) macro
		// documented at its define-syntax site). The copy path installs only the
		// value, so without this the docstring would be lost on import.
		doc := source.Doc()
		if doc != "" {
			m.Doc = doc
		}
	}
	// sourceLib is the library being imported FROM; the inline-HOF stamp is gated
	// on it (identity), not just the export name — see stampImportedInlineHOF.
	stampImportedInlineHOF(target, exportName, sourceLib)
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
	ids     map[string]struct{} // only / except
	prefix  string              // prefix
	renames map[string]string   // rename: old-name -> new-name
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
func (p *ImportSet) AddOnly(ids map[string]struct{}) {
	p.Modifiers = append(p.Modifiers, importModifier{kind: importModOnly, ids: ids})
}

// AddExcept appends an `except` modifier removing ids from the import. Empty/nil is a
// no-op.
func (p *ImportSet) AddExcept(ids map[string]struct{}) {
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

// CopyLibraryBindingsToEnvAtPhase copies exported bindings from a library to a specific phase.
// bindings is the map from localName -> externalName produced by ApplyToExports.
//
// Phase semantics:
//   - targetPhase == 0: Runtime import (default). Runtime bindings go to phase 0,
//     syntax bindings go to both phase 0 (for export) and phase 1 (for use in macros).
//   - targetPhase > 0: For-syntax import. Bindings are shifted to the target phase.
//     Runtime bindings become available during macro expansion at targetPhase.
//     Syntax bindings go to targetPhase and targetPhase+1.
//   - targetPhase < 0: For-template import. Bindings shifted to negative phase
//     (used for generating code that will run at a lower phase).

// ResolveAndInstallImportSet parses an import set datum, loads the library,
// applies modifiers (only, except, prefix, rename), fires the import observer,
// and copies the resulting bindings into env at the appropriate phase.
//
// This is the common path for top-level imports (both expander and compiler).
// Library-internal imports (processLibraryImport) diverge at installation and
// use their own loop.

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
// env at the appropriate phase. Used for top-level imports (both expander and
// compiler). Library-internal imports share the resolution step (resolveImportSet)
// but use copyLibraryBindingsDirect for installation.
func ResolveAndInstallImportSet(ctx context.Context, datum values.Value, env *environment.EnvironmentFrame, phase environment.Phase, evaluator machine.MacroEvaluator) error {
	res, err := resolveImportSet(ctx, datum, env, evaluator)
	if err != nil {
		return err
	}

	fireImportObserver(env, res.Library, res.Bindings, LibraryName{}, phase)

	err = CopyLibraryBindingsToEnvAtPhase(res.Library, res.Bindings, env, res.ImportSet.PhaseShift)
	if err != nil {
		return werr.WrapForeignErrorf(err, "import: error copying bindings from %s",
			res.ImportSet.LibraryName.SchemeString())
	}

	return nil
}

// findLibraryBinding searches the library's runtime, expand, and compile
// environments for a binding with the given internal name. The boolean
// reports whether a binding was found; when false, the binding pointer is
// nil and the phase value is meaningless. Callers must check the boolean
// (or the binding pointer) before relying on the returned phase — the
// phase return cannot carry a sentinel "not found" value because every
// non-negative Phase is a valid result.
func findLibraryBinding(lib *CompiledLibrary, internalName string) (*environment.Binding, environment.Phase, bool) {
	sourceEnvs := []struct {
		env   *environment.EnvironmentFrame
		phase environment.Phase
	}{
		{lib.Env, environment.PhaseRuntime},
		{lib.Env.Expand(), environment.PhaseExpand},
		{lib.Env.Compile(), environment.PhaseCompile},
	}

	libSym := values.NewSymbol(internalName)
	for _, src := range sourceEnvs {
		if src.env == nil {
			continue
		}
		binding := src.env.GetBinding(libSym, nil)
		if binding != nil {
			return binding, src.phase, true
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
		return false
	}
	return !sameImportedBinding(ev, iv)
}

// sameImportedBinding reports whether two imported values denote the same underlying
// definition (a diamond / re-export) rather than two different definitions sharing one
// name (a conflict). Plain EqualTo is insufficient: Wile import/re-export preserves NO
// runtime identity for procedures — a re-export RECOMPILES, producing a fresh closure
// with its own template and env (verified: (scheme base) cddr and (scheme cxr) cddr,
// both re-exports of the ambient bootstrap cddr, share neither template, env, nor
// pointer). The only signal that survives recompilation is the procedure NAME, so:
//
//   - foreign closures (primitives): the registry is name-unique per engine, so equal
//     names denote the same primitive (e.g. nan? from (scheme base) vs (scheme inexact)).
//   - machine (Scheme) closures: equal names denote the same re-exported procedure
//     (e.g. cddr from (scheme base) vs (scheme cxr)).
//
// A primitive vs a Scheme redefinition of one name (the only genuine stdlib collision —
// (scheme base) string-map vs (srfi 13) string-map) is cross-kind, so it falls to the
// EqualTo default and is reported as a conflict.
//
// Why name and not the definition source location: a source-location origin is airtight
// for import-vs-import clashes but FALSELY flags the common, legal define-over-import
// shadow — a program that (import (scheme base)) then (define (zero? x) …) would see its
// own zero? (user source) "conflict" with the imported zero? (library source). Comparing
// by name treats both as the one logical zero?, preserving shadowing. The cost is that
// two DIFFERENT Scheme procedures exported under the same name by two libraries are not
// flagged (silent last-import-wins); no such case exists in the bundled stdlib, whereas
// define-over-import is everywhere — so name is the right trade-off for Wile.
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

		// Create binding in the target at the base phase. A previously-imported
		// binding of the same local name that resolves to a different binding is a
		// conflicting import (R7RS §5.6) — reject rather than silently last-wins.
		phaseEnv := targetEnv.AtPhase(targetPhase)
		localSym := values.NewSymbol(localName)
		_, created := phaseEnv.MaybeCreateOwnGlobalBinding(localSym, libBinding.BindingType())
		if !created && importConflicts(phaseEnv.GetBinding(localSym, nil), libBinding) {
			return werr.WrapForeignErrorf(werr.ErrDuplicateBinding,
				"import: identifier %q from %s conflicts with a different existing import; disambiguate with (except ...), (prefix ...), or (rename ...)",
				localName, lib.Name.SchemeString())
		}
		globalIdx := phaseEnv.GetGlobalIndex(localSym)
		if globalIdx != nil {
			err := phaseEnv.SetOwnGlobalValue(globalIdx, libBinding.Value())
			if err != nil {
				return werr.WrapForeignErrorf(err, "failed to set binding for %s at phase %s", localName, targetPhase)
			}
		}
		markBindingImported(phaseEnv.GetBinding(localSym, nil), libBinding, externalName, lib.Name)

		// Propagate to the source phase in the target so the binding is available
		// in the same phase it originated from. Syntax bindings (phase 1) need to
		// be in the expand phase for macro expansion; compile-phase bindings
		// (auxiliary syntax, phase 2) need to be in the compile phase.
		if sourcePhase > 0 {
			propagateEnv := targetEnv.AtPhase(targetPhase + sourcePhase)
			propagateSym := values.NewSymbol(localName)
			_, _ = propagateEnv.MaybeCreateOwnGlobalBinding(propagateSym, libBinding.BindingType())
			propagateIdx := propagateEnv.GetGlobalIndex(propagateSym)
			if propagateIdx != nil {
				_ = propagateEnv.SetOwnGlobalValue(propagateIdx, libBinding.Value())
			}
			markBindingImported(propagateEnv.GetBinding(propagateSym, nil), libBinding, externalName, lib.Name)
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
// Syntax bindings are additionally copied to targetEnv.Expand() so they are
// available during macro expansion of the library body.
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

		// Install in the target environment directly.
		localSym := values.NewSymbol(localName)
		_, _ = targetEnv.MaybeCreateOwnGlobalBinding(localSym, importedBinding.BindingType())
		globalIdx := targetEnv.GetGlobalIndex(localSym)
		if globalIdx != nil {
			err := targetEnv.SetOwnGlobalValue(globalIdx, importedBinding.Value())
			if err != nil {
				return werr.WrapForeignErrorf(err, "import: failed to set binding for %s", localName)
			}
		}
		markBindingImported(targetEnv.GetBinding(localSym, nil), importedBinding, externalName, lib.Name)

		// Syntax bindings must also be available in the expand phase.
		if importedBinding.BindingType() == environment.BindingTypeSyntax {
			expandEnv := targetEnv.Expand()
			_, _ = expandEnv.MaybeCreateOwnGlobalBinding(localSym, environment.BindingTypeSyntax)
			expandIdx := expandEnv.GetGlobalIndex(localSym)
			if expandIdx != nil {
				_ = expandEnv.SetOwnGlobalValue(expandIdx, importedBinding.Value())
			}
			markBindingImported(expandEnv.GetBinding(localSym, nil), importedBinding, externalName, lib.Name)
		}
	}
	return nil
}
