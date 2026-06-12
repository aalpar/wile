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

	"github.com/aalpar/wile/machine"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// markBindingImported sets the Imported and Stable flags on a binding
// installed by library import. A nil binding is silently ignored.
//
// Stable is about *rebinding*, not value materialization — an imported binding
// will not be rebound regardless of whether its value is realized yet, so
// Stable is set unconditionally (unlike the retired Constant, which keyed on
// b.Value() != nil). Imported already implies Stable via IsStable; setting the
// field too keeps the stored state self-consistent for the clone path.
func markBindingImported(b *environment.Binding) {
	if b == nil {
		return
	}
	m := b.EnsureMeta()
	m.Imported = true
	m.Stable = true
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
type ImportSet struct {
	LibraryName LibraryName         // Base library to import from
	Only        map[string]struct{} // If non-nil, only import these names
	Except      map[string]struct{} // If non-nil, import all except these
	Prefix      string              // If non-empty, add this prefix to all names
	Renames     map[string]string   // old-name -> new-name
	PhaseShift  environment.Phase   // Phase offset: 0=runtime, 1=for-syntax, -1=for-template
}

// NewImportSet creates a new import set for a library.
func NewImportSet(name LibraryName) *ImportSet {
	return &ImportSet{
		LibraryName: name,
		Renames:     make(map[string]string),
	}
}

// ApplyToExports applies the import modifiers and returns the final bindings.
// Returns a map of local-name -> external-name (the name in the library).
func (p *ImportSet) ApplyToExports(lib *CompiledLibrary) (map[string]string, error) {
	result := make(map[string]string)

	// Start with all exports
	for externalName := range lib.Exports {
		result[externalName] = externalName
	}

	// Apply 'only' filter
	if p.Only != nil {
		filtered := make(map[string]string)
		for name := range p.Only {
			_, ok := result[name]
			if !ok {
				return nil, werr.WrapForeignErrorf(werr.ErrUnexportedIdentifier, "applyToExports: identifier %q not exported by %s",
					name, lib.Name.SchemeString())
			}
			filtered[name] = name
		}
		result = filtered
	}

	// Apply 'except' filter
	if p.Except != nil {
		for name := range p.Except {
			_, ok := result[name]
			if !ok {
				return nil, werr.WrapForeignErrorf(werr.ErrUnexportedIdentifier, "applyToExports: identifier %q not exported by %s",
					name, lib.Name.SchemeString())
			}
			delete(result, name)
		}
	}

	// Apply renames
	if len(p.Renames) > 0 {
		renamed := make(map[string]string)
		for localName, externalName := range result {
			newName, ok := p.Renames[localName]
			if ok {
				renamed[newName] = externalName
			} else {
				renamed[localName] = externalName
			}
		}
		result = renamed
	}

	// Apply prefix
	if p.Prefix != "" {
		prefixed := make(map[string]string)
		for localName, externalName := range result {
			prefixed[p.Prefix+localName] = externalName
		}
		result = prefixed
	}

	return result, nil
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

		// Create binding in the target at the base phase.
		phaseEnv := targetEnv.AtPhase(targetPhase)
		localSym := values.NewSymbol(localName)
		_, _ = phaseEnv.MaybeCreateOwnGlobalBinding(localSym, libBinding.BindingType())
		globalIdx := phaseEnv.GetGlobalIndex(localSym)
		if globalIdx != nil {
			err := phaseEnv.SetOwnGlobalValue(globalIdx, libBinding.Value())
			if err != nil {
				return werr.WrapForeignErrorf(err, "failed to set binding for %s at phase %s", localName, targetPhase)
			}
		}
		markBindingImported(phaseEnv.GetBinding(localSym, nil))

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
			markBindingImported(propagateEnv.GetBinding(propagateSym, nil))
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
		markBindingImported(targetEnv.GetBinding(localSym, nil))

		// Syntax bindings must also be available in the expand phase.
		if importedBinding.BindingType() == environment.BindingTypeSyntax {
			expandEnv := targetEnv.Expand()
			_, _ = expandEnv.MaybeCreateOwnGlobalBinding(localSym, environment.BindingTypeSyntax)
			expandIdx := expandEnv.GetGlobalIndex(localSym)
			if expandIdx != nil {
				_ = expandEnv.SetOwnGlobalValue(expandIdx, importedBinding.Value())
			}
			markBindingImported(expandEnv.GetBinding(localSym, nil))
		}
	}
	return nil
}
