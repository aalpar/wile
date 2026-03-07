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

// library_registry.go implements R7RS library data structures and registry.
//
// This file provides the core types (LibraryName, CompiledLibrary,
// LibraryRegistry) and the registry that manages loaded libraries,
// search paths, and cycle detection.

import (
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/werr"
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
	Name         LibraryName                   // Library name
	Env          *environment.EnvironmentFrame // Library's private environment
	Exports      map[string]string             // external-name -> internal-name
	SourceFile   string                        // Path to .sld file (for error messages)
	Template     *NativeTemplate               // Compiled bytecode (for execution)
	LibraryScope *syntax.Scope                 // Unique scope for cross-library macro hygiene
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
	Library    LibraryName // imported library name, e.g., (scheme base)
	SourceFile string      // path to .sld file (empty for synthetic libraries)
	Exports    []string    // all names exported by the library
	Imported   []string    // names that actually landed in the importer (after only/except/prefix/rename)
	Importer   LibraryName // importing library name (zero value for top-level import)
	Phase      int         // pipeline phase: environment.PhaseExpand or environment.PhaseCompile
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

// PrependSearchPath adds a path to the beginning of the search path list.
func (p *LibraryRegistry) PrependSearchPath(path string) {
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
// name, or zero value for top-level imports.
func fireImportObserver(env *environment.EnvironmentFrame, lib *CompiledLibrary, bindings map[string]string, importer LibraryName, phase int) {
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

	reg.importObserver(LibraryImportEvent{
		Library:    lib.Name,
		SourceFile: lib.SourceFile,
		Exports:    exports,
		Imported:   imported,
		Importer:   importer,
		Phase:      phase,
	})
}

// Register adds a compiled library to the registry.
func (p *LibraryRegistry) Register(lib *CompiledLibrary) error {
	key := lib.Name.Key()
	_, exists := p.libraries[key]
	if exists {
		return werr.WrapForeignErrorf(werr.ErrDuplicateBinding, "register: library %s already registered", lib.Name.SchemeString())
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

	return "", werr.WrapForeignErrorf(werr.ErrLibraryNotFound, "findLibraryFile: library %s not found in search paths: %v",
		name.SchemeString(), p.searchPaths)
}
