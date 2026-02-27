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

package machine

// library.go implements R7RS library support.
//
// This file provides the core data structures and registry for managing
// Scheme libraries per R7RS Section 5.6.
//
// A library is a named collection of definitions with explicit imports
// and exports. Libraries provide namespace isolation and modular code
// organization.
//
// Example:
//   (define-library (my-lib)
//     (export public-fn)
//     (import (scheme base))
//     (begin
//       (define (private-fn x) (* x 2))
//       (define (public-fn x) (private-fn x))))

import (
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
)

// LibraryName represents an R7RS library name like (scheme base) or (my lib).
// Library names are lists of identifiers used to uniquely identify a library.
type LibraryName struct {
	Parts []string // e.g., ["scheme", "base"]
}

// NewLibraryName creates a LibraryName from a list of string parts.
func NewLibraryName(parts ...string) LibraryName {
	return LibraryName{Parts: parts}
}

// String returns a human-readable representation like "scheme/base".
func (p LibraryName) String() string {
	return strings.Join(p.Parts, "/")
}

// SchemeString returns the Scheme representation like "(scheme base)".
func (p LibraryName) SchemeString() string {
	return "(" + strings.Join(p.Parts, " ") + ")"
}

// Key returns a unique string key for map lookups.
func (p LibraryName) Key() string {
	return strings.Join(p.Parts, "/")
}

// ToFilePath converts a library name to a file path.
// (scheme base) -> "scheme/base.sld"
func (p LibraryName) ToFilePath() string {
	return strings.Join(p.Parts, string(os.PathSeparator)) + ".sld"
}

// CompiledLibrary holds a loaded and compiled library.
type CompiledLibrary struct {
	Name       LibraryName                   // Library name
	Env        *environment.EnvironmentFrame // Library's private environment
	Exports    map[string]string             // external-name -> internal-name
	SourceFile string                        // Path to .sld file (for error messages)
	Template   *NativeTemplate               // Compiled bytecode (for execution)
}

// NewCompiledLibrary creates a new compiled library.
func NewCompiledLibrary(name LibraryName, env *environment.EnvironmentFrame) *CompiledLibrary {
	return &CompiledLibrary{
		Name:    name,
		Env:     env,
		Exports: make(map[string]string),
	}
}

// AddExport adds an export to the library.
// If internalName is empty, it defaults to externalName (no rename).
func (p *CompiledLibrary) AddExport(externalName, internalName string) {
	if internalName == "" {
		internalName = externalName
	}
	p.Exports[externalName] = internalName
}

// IsExported returns true if the given external name is exported.
func (p *CompiledLibrary) IsExported(externalName string) bool {
	_, ok := p.Exports[externalName]
	return ok
}

// GetInternalName returns the internal name for an exported external name.
// Returns empty string if not exported.
func (p *CompiledLibrary) GetInternalName(externalName string) string {
	return p.Exports[externalName]
}

// LibraryImportEvent records what happened when a library was imported.
type LibraryImportEvent struct {
	Library    []string // imported library name parts, e.g., ["scheme", "base"]
	SourceFile string   // path to .sld file (empty for synthetic libraries)
	Exports    []string // all names exported by the library
	Imported   []string // names that actually landed in the importer (after only/except/prefix/rename)
	Importer   []string // importing library name (nil for top-level import)
}

// LibraryImportObserver is called when a library is imported.
// Observers are read-only — they cannot influence the import.
type LibraryImportObserver func(LibraryImportEvent)

// LibraryRegistry manages loaded libraries and handles library loading.
type LibraryRegistry struct {
	libraries      map[string]*CompiledLibrary // key: library name as "scheme/base"
	loading        map[string]bool             // libraries currently being loaded (cycle detection)
	searchPaths    []string                    // directories to search for library files
	importObserver LibraryImportObserver       // optional: called on each library import
}

// DefaultLibraryPaths are the default directories to search for libraries.
var DefaultLibraryPaths = []string{
	".",
	"./lib",
}

// NewLibraryRegistry creates a new library registry with default search paths.
func NewLibraryRegistry() *LibraryRegistry {
	return &LibraryRegistry{
		libraries:   make(map[string]*CompiledLibrary),
		loading:     make(map[string]bool),
		searchPaths: DefaultLibraryPaths,
	}
}

// SetSearchPaths sets the library search paths.
func (p *LibraryRegistry) SetSearchPaths(paths []string) {
	p.searchPaths = paths
}

// GetSearchPaths returns the current library search paths.
func (p *LibraryRegistry) GetSearchPaths() []string {
	return p.searchPaths
}

// AddSearchPath adds a path to the beginning of the search path list.
func (p *LibraryRegistry) AddSearchPath(path string) {
	p.searchPaths = append([]string{path}, p.searchPaths...)
}

// SetImportObserver sets an optional observer that is called each time
// a library is imported. The observer is read-only and cannot influence
// the import. Pass nil to remove the observer.
func (p *LibraryRegistry) SetImportObserver(obs LibraryImportObserver) {
	p.importObserver = obs
}

// ImportObserver returns the current import observer, or nil.
func (p *LibraryRegistry) ImportObserver() LibraryImportObserver {
	return p.importObserver
}

// fireImportObserver calls the import observer if one is set on the
// registry stored in env. bindings maps local name -> external name
// (as returned by ApplyToExports). importer is the importing library's
// name parts, or nil for top-level imports.
func fireImportObserver(env *environment.EnvironmentFrame, lib *CompiledLibrary, bindings map[string]string, importer []string) {
	regAny := env.LibraryRegistry()
	if regAny == nil {
		return
	}
	reg, ok := regAny.(*LibraryRegistry)
	if !ok || reg.importObserver == nil {
		return
	}

	exports := make([]string, 0, len(lib.Exports))
	for name := range lib.Exports {
		exports = append(exports, name)
	}
	sort.Strings(exports)

	imported := make([]string, 0, len(bindings))
	for name := range bindings {
		imported = append(imported, name)
	}
	sort.Strings(imported)

	libraryParts := make([]string, len(lib.Name.Parts))
	copy(libraryParts, lib.Name.Parts)

	var importerCopy []string
	if importer != nil {
		importerCopy = make([]string, len(importer))
		copy(importerCopy, importer)
	}

	reg.importObserver(LibraryImportEvent{
		Library:    libraryParts,
		SourceFile: lib.SourceFile,
		Exports:    exports,
		Imported:   imported,
		Importer:   importerCopy,
	})
}

// Register adds a compiled library to the registry.
func (p *LibraryRegistry) Register(lib *CompiledLibrary) error {
	key := lib.Name.Key()
	_, exists := p.libraries[key]
	if exists {
		return values.WrapForeignErrorf(values.ErrDuplicateBinding, "register: library %s already registered", lib.Name.SchemeString())
	}
	p.libraries[key] = lib
	return nil
}

// Lookup returns a library by name, or nil if not found.
func (p *LibraryRegistry) Lookup(name LibraryName) *CompiledLibrary {
	return p.libraries[name.Key()]
}

// IsLoading returns true if the library is currently being loaded.
// Used to detect circular dependencies.
func (p *LibraryRegistry) IsLoading(name LibraryName) bool {
	return p.loading[name.Key()]
}

// StartLoading marks a library as being loaded.
func (p *LibraryRegistry) StartLoading(name LibraryName) {
	p.loading[name.Key()] = true
}

// FinishLoading marks a library as finished loading.
func (p *LibraryRegistry) FinishLoading(name LibraryName) {
	delete(p.loading, name.Key())
}

// FindLibraryFile searches for a library file in the search paths.
// Returns the full path to the file, or an error if not found.
func (p *LibraryRegistry) FindLibraryFile(name LibraryName) (string, error) {
	relativePath := name.ToFilePath()

	for _, searchPath := range p.searchPaths {
		fullPath := filepath.Join(searchPath, relativePath)
		_, err := os.Stat(fullPath)
		if err == nil {
			return fullPath, nil
		}
	}

	// Also try .scm extension
	relativePathScm := strings.TrimSuffix(relativePath, ".sld") + ".scm"
	for _, searchPath := range p.searchPaths {
		fullPath := filepath.Join(searchPath, relativePathScm)
		_, err := os.Stat(fullPath)
		if err == nil {
			return fullPath, nil
		}
	}

	return "", values.WrapForeignErrorf(values.ErrLibraryNotFound, "findLibraryFile: library %s not found in search paths: %v",
		name.SchemeString(), p.searchPaths)
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
	LibraryName LibraryName       // Base library to import from
	Only        []string          // If non-nil, only import these names
	Except      []string          // If non-nil, import all except these
	Prefix      string            // If non-empty, add this prefix to all names
	Renames     map[string]string // old-name -> new-name
	PhaseShift  int               // Phase offset: 0=runtime, 1=for-syntax, -1=for-template
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
		for _, name := range p.Only {
			_, ok := result[name]
			if !ok {
				return nil, values.WrapForeignErrorf(values.ErrUnexportedIdentifier, "applyToExports: identifier %q not exported by %s",
					name, lib.Name.SchemeString())
			}
			filtered[name] = name
		}
		result = filtered
	}

	// Apply 'except' filter
	if p.Except != nil {
		for _, name := range p.Except {
			_, ok := result[name]
			if !ok {
				return nil, values.WrapForeignErrorf(values.ErrUnexportedIdentifier, "applyToExports: identifier %q not exported by %s",
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
	return CopyLibraryBindingsToEnvAtPhase(lib, bindings, targetEnv, 0)
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
func CopyLibraryBindingsToEnvAtPhase(lib *CompiledLibrary, bindings map[string]string, targetEnv *environment.EnvironmentFrame, targetPhase int) error {
	// Source environments to search for exported bindings, in priority order.
	// Phase 0 (runtime) holds variables, phase 1 (expand) holds syntax bindings
	// from define-syntax, phase 2 (compile) holds auxiliary syntax (else, =>, ..., _).
	sourceEnvs := []struct {
		env   *environment.EnvironmentFrame
		phase int
	}{
		{lib.Env, environment.PhaseRuntime},
		{lib.Env.Expand(), environment.PhaseExpand},
		{lib.Env.Compile(), environment.PhaseCompile},
	}

	for localName, externalName := range bindings {
		internalName := lib.GetInternalName(externalName)
		if internalName == "" {
			internalName = externalName
		}

		// Search source environments in phase order for the binding.
		libSym := lib.Env.InternSymbol(values.NewSymbol(internalName))
		var libBinding *environment.Binding
		sourcePhase := 0
		for _, src := range sourceEnvs {
			if src.env == nil {
				continue
			}
			libBinding = src.env.GetBinding(libSym)
			if libBinding != nil {
				sourcePhase = src.phase
				break
			}
		}
		if libBinding == nil {
			return values.WrapForeignErrorf(values.ErrNoSuchBinding, "library %s exports %q but binding not found",
				lib.Name.SchemeString(), internalName)
		}

		// Create binding in the target at the base phase.
		phaseEnv := targetEnv.AtPhase(targetPhase)
		localSym := phaseEnv.InternSymbol(values.NewSymbol(localName))
		_, _ = phaseEnv.MaybeCreateOwnGlobalBinding(localSym, libBinding.BindingType())
		globalIdx := phaseEnv.GetGlobalIndex(localSym)
		if globalIdx != nil {
			err := phaseEnv.SetOwnGlobalValue(globalIdx, libBinding.Value())
			if err != nil {
				return values.WrapForeignErrorf(err, "failed to set binding for %s at phase %d", localName, targetPhase)
			}
		}

		// Propagate to the source phase in the target so the binding is available
		// in the same phase it originated from. Syntax bindings (phase 1) need to
		// be in the expand phase for macro expansion; compile-phase bindings
		// (auxiliary syntax, phase 2) need to be in the compile phase.
		if sourcePhase > 0 {
			propagateEnv := targetEnv.AtPhase(targetPhase + sourcePhase)
			propagateSym := propagateEnv.InternSymbol(values.NewSymbol(localName))
			_, _ = propagateEnv.MaybeCreateOwnGlobalBinding(propagateSym, libBinding.BindingType())
			propagateIdx := propagateEnv.GetGlobalIndex(propagateSym)
			if propagateIdx != nil {
				_ = propagateEnv.SetOwnGlobalValue(propagateIdx, libBinding.Value())
			}
		}
	}
	return nil
}
