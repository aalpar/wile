// Copyright 2025 Aaron Alpar
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
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"wile/environment"
	"wile/values"
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
func (n LibraryName) String() string {
	return strings.Join(n.Parts, "/")
}

// SchemeString returns the Scheme representation like "(scheme base)".
func (n LibraryName) SchemeString() string {
	return "(" + strings.Join(n.Parts, " ") + ")"
}

// Key returns a unique string key for map lookups.
func (n LibraryName) Key() string {
	return strings.Join(n.Parts, "/")
}

// ToFilePath converts a library name to a file path.
// (scheme base) -> "scheme/base.sld"
func (n LibraryName) ToFilePath() string {
	return strings.Join(n.Parts, string(os.PathSeparator)) + ".sld"
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
func (lib *CompiledLibrary) AddExport(externalName, internalName string) {
	if internalName == "" {
		internalName = externalName
	}
	lib.Exports[externalName] = internalName
}

// IsExported returns true if the given external name is exported.
func (lib *CompiledLibrary) IsExported(externalName string) bool {
	_, ok := lib.Exports[externalName]
	return ok
}

// GetInternalName returns the internal name for an exported external name.
// Returns empty string if not exported.
func (lib *CompiledLibrary) GetInternalName(externalName string) string {
	return lib.Exports[externalName]
}

// LibraryRegistry manages loaded libraries and handles library loading.
type LibraryRegistry struct {
	libraries   map[string]*CompiledLibrary // key: library name as "scheme/base"
	loading     map[string]bool             // libraries currently being loaded (cycle detection)
	searchPaths []string                    // directories to search for library files
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
func (r *LibraryRegistry) SetSearchPaths(paths []string) {
	r.searchPaths = paths
}

// GetSearchPaths returns the current library search paths.
func (r *LibraryRegistry) GetSearchPaths() []string {
	return r.searchPaths
}

// AddSearchPath adds a path to the beginning of the search path list.
func (r *LibraryRegistry) AddSearchPath(path string) {
	r.searchPaths = append([]string{path}, r.searchPaths...)
}

// Register adds a compiled library to the registry.
func (r *LibraryRegistry) Register(lib *CompiledLibrary) error {
	key := lib.Name.Key()
	if _, exists := r.libraries[key]; exists {
		return fmt.Errorf("library %s already registered", lib.Name.SchemeString())
	}
	r.libraries[key] = lib
	return nil
}

// Lookup returns a library by name, or nil if not found.
func (r *LibraryRegistry) Lookup(name LibraryName) *CompiledLibrary {
	return r.libraries[name.Key()]
}

// IsLoading returns true if the library is currently being loaded.
// Used to detect circular dependencies.
func (r *LibraryRegistry) IsLoading(name LibraryName) bool {
	return r.loading[name.Key()]
}

// StartLoading marks a library as being loaded.
func (r *LibraryRegistry) StartLoading(name LibraryName) {
	r.loading[name.Key()] = true
}

// FinishLoading marks a library as finished loading.
func (r *LibraryRegistry) FinishLoading(name LibraryName) {
	delete(r.loading, name.Key())
}

// FindLibraryFile searches for a library file in the search paths.
// Returns the full path to the file, or an error if not found.
func (r *LibraryRegistry) FindLibraryFile(name LibraryName) (string, error) {
	relativePath := name.ToFilePath()

	for _, searchPath := range r.searchPaths {
		fullPath := filepath.Join(searchPath, relativePath)
		if _, err := os.Stat(fullPath); err == nil {
			return fullPath, nil
		}
	}

	// Also try .scm extension
	relativePathScm := strings.TrimSuffix(relativePath, ".sld") + ".scm"
	for _, searchPath := range r.searchPaths {
		fullPath := filepath.Join(searchPath, relativePathScm)
		if _, err := os.Stat(fullPath); err == nil {
			return fullPath, nil
		}
	}

	return "", fmt.Errorf("library %s not found in search paths: %v",
		name.SchemeString(), r.searchPaths)
}

// ImportSet represents a parsed import specification.
// It can be a simple library reference or include modifiers.
type ImportSet struct {
	LibraryName LibraryName       // Base library to import from
	Only        []string          // If non-nil, only import these names
	Except      []string          // If non-nil, import all except these
	Prefix      string            // If non-empty, add this prefix to all names
	Renames     map[string]string // old-name -> new-name
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
func (is *ImportSet) ApplyToExports(lib *CompiledLibrary) (map[string]string, error) {
	result := make(map[string]string)

	// Start with all exports
	for externalName := range lib.Exports {
		result[externalName] = externalName
	}

	// Apply 'only' filter
	if is.Only != nil {
		filtered := make(map[string]string)
		for _, name := range is.Only {
			if _, ok := result[name]; !ok {
				return nil, fmt.Errorf("identifier %q not exported by %s",
					name, lib.Name.SchemeString())
			}
			filtered[name] = name
		}
		result = filtered
	}

	// Apply 'except' filter
	if is.Except != nil {
		for _, name := range is.Except {
			if _, ok := result[name]; !ok {
				return nil, fmt.Errorf("identifier %q not exported by %s",
					name, lib.Name.SchemeString())
			}
			delete(result, name)
		}
	}

	// Apply renames
	if len(is.Renames) > 0 {
		renamed := make(map[string]string)
		for localName, externalName := range result {
			if newName, ok := is.Renames[localName]; ok {
				renamed[newName] = externalName
			} else {
				renamed[localName] = externalName
			}
		}
		result = renamed
	}

	// Apply prefix
	if is.Prefix != "" {
		prefixed := make(map[string]string)
		for localName, externalName := range result {
			prefixed[is.Prefix+localName] = externalName
		}
		result = prefixed
	}

	return result, nil
}

// CopyLibraryBindingsToEnv copies exported bindings from a library to an environment.
// bindings is the map from localName -> externalName produced by ApplyToExports.
// Both runtime and syntax bindings are copied.
func CopyLibraryBindingsToEnv(lib *CompiledLibrary, bindings map[string]string, targetEnv *environment.EnvironmentFrame) error {
	for localName, externalName := range bindings {
		internalName := lib.GetInternalName(externalName)
		if internalName == "" {
			internalName = externalName
		}

		// Get the binding from the library's environment
		// First check the runtime environment, then the expand environment for syntax bindings
		libSym := lib.Env.InternSymbol(values.NewSymbol(internalName))
		libBinding := lib.Env.GetBinding(libSym)
		if libBinding == nil {
			// Syntax bindings (define-syntax) are stored in the expand environment
			expandEnv := lib.Env.Expand()
			if expandEnv != nil {
				libBinding = expandEnv.GetBinding(libSym)
			}
		}
		if libBinding == nil {
			return values.NewForeignErrorf("library %s exports %q but binding not found",
				lib.Name.SchemeString(), internalName)
		}

		// Create binding in the target environment
		localSym := targetEnv.InternSymbol(values.NewSymbol(localName))
		_, _ = targetEnv.MaybeCreateOwnGlobalBinding(localSym, libBinding.BindingType())
		globalIdx := targetEnv.GetGlobalIndex(localSym)
		if globalIdx != nil {
			if err := targetEnv.SetOwnGlobalValue(globalIdx, libBinding.Value()); err != nil {
				return values.WrapForeignErrorf(err, "failed to set binding for %s", localName)
			}
		}

		// If it's a syntax binding, also copy to expand phase
		if libBinding.BindingType() == environment.BindingTypeSyntax {
			expandEnv := targetEnv.Expand()
			_, _ = expandEnv.MaybeCreateOwnGlobalBinding(localSym, environment.BindingTypeSyntax)
			expandIdx := expandEnv.GetGlobalIndex(localSym)
			if expandIdx != nil {
				_ = expandEnv.SetOwnGlobalValue(expandIdx, libBinding.Value())
			}
		}
	}
	return nil
}

// Error types for library operations
var (
	ErrLibraryNotFound      = errors.New("library not found")
	ErrCircularDependency   = errors.New("circular library dependency")
	ErrDuplicateExport      = errors.New("duplicate export")
	ErrDuplicateImport      = errors.New("duplicate import")
	ErrUnexportedIdentifier = errors.New("identifier not exported")
)
