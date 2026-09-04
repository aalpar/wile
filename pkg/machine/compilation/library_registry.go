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

// library_registry.go implements R7RS library data structures and registry.
//
// This file provides the core types (LibraryName, CompiledLibrary,
// LibraryRegistry) and the registry that manages loaded libraries,
// search paths, and cycle detection.

import (
	"cmp"
	"context"
	"fmt"
	"io/fs"
	"maps"
	"slices"
	"strconv"
	"strings"
	"sync"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/machine/compilation/resolver"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// libraryExtensions caches the recognized library file extensions at init time.
var libraryExtensions = resolver.LibraryExtensions()

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

// ToFSPath returns the library name as a forward-slash-separated path
// with .sld extension, suitable for fs.FS and FileResolver operations.
func (p LibraryName) ToFSPath() string {
	return strings.Join(p.Parts, "/") + ".sld"
}

// ToSchemeValue converts a LibraryName to a Scheme list.
// Parts that parse as nonnegative integers become exact integers;
// all others become symbols. Matches R7RS library name syntax.
func (p LibraryName) ToSchemeValue() values.Value {
	elems := make([]values.Value, len(p.Parts))
	for i, part := range p.Parts {
		n, err := strconv.ParseInt(part, 10, 64)
		if err == nil && n >= 0 {
			elems[i] = values.NewInteger(n)
		} else {
			elems[i] = values.NewSymbol(part)
		}
	}
	return values.List(elems...)
}

// CompiledLibrary holds a loaded and compiled library.
type CompiledLibrary struct {
	Name        LibraryName                   // Library name
	Description string                        // from (description ...) clause; "" if absent
	Env         *environment.EnvironmentFrame // Library's private environment
	Exports     map[string]string             // external-name -> internal-name
	SourceFile  string                        // Path to .sld file (for error messages)
	Template    *machine.NativeTemplate       // Compiled bytecode (for execution)

	// Scope is the library's own hygiene scope, the one every binder written in
	// the library body carries. It is the KEY the export lookup resolves under
	// (findLibraryBinding), which is what lets a library define a name it also
	// imports: the define lands at {Scope}, the import at {}, and maximal
	// resolution prefers the define while {} ⊆ {Scope} still reaches a name the
	// library only re-exports. Nil for a library built without a body scope, in
	// which case the export lookup degrades to the empty set.
	Scope *syntax.Scope
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

// ImportStage names which pipeline pass observed an import. A top-level
// (import …) is seen twice: once by the expander, which resolves it so the
// imported macros are available to expansion (expandImportForm), and once by the
// compiler, which installs the bindings (CompileImport). The observer fires
// on both. A library-body import is seen by the compiler only.
//
// This is not a Phase. The pass does not move along the macro tower: the
// bindings land at env.PhaseLevel() composed with the import set's shift
// regardless of which pass installs them (ResolveAndInstallImportSet).
//
// The zero value is no stage. An event a caller built without one says so
// (String renders it import-stage(0)) rather than reading as the expander's.
type ImportStage uint8

const (
	ImportStageExpand  ImportStage = iota + 1 // the expander's pass
	ImportStageCompile                        // the compiler's pass
)

// String returns the pass name.
func (s ImportStage) String() string {
	switch s {
	case ImportStageExpand:
		return "expand"
	case ImportStageCompile:
		return "compile"
	default:
		return fmt.Sprintf("import-stage(%d)", uint8(s))
	}
}

// LibraryImportEvent records what happened when a library was imported.
type LibraryImportEvent struct {
	Library    LibraryName // imported library name, e.g., (scheme base)
	SourceFile string      // path to .sld file (empty for synthetic libraries)
	Exports    []string    // all names exported by the library
	Imported   []string    // names that actually landed in the importer (after only/except/prefix/rename)
	Importer   LibraryName // importing library name (zero value for top-level import)
	Stage      ImportStage // which pipeline pass saw the import; see ImportStage
}

// LibraryImportObserver is called when a library is imported.
// Observers are read-only — they cannot influence the import.
type LibraryImportObserver func(LibraryImportEvent)

// LibraryCompileObserver is called once per library, after its body has
// compiled and before that body executes. It sees the library's Template
// while every top-level form is still unexecuted, which is the window an
// instrumenter (coverage) needs: instrumentation attached any later would
// miss the definitions themselves and report every library form as never
// run. Observers must not alter the library's bindings or exports.
type LibraryCompileObserver func(*CompiledLibrary)

// LibraryRegistry manages loaded libraries and handles library loading.
//
// All fields are guarded by mu so the registry is safe for concurrent use by
// SRFI-18 threads that load libraries via (environment …) / (eval '(import …)).
// Read methods take RLock; methods that mutate a map, the search paths, or the
// observer take Lock. mu is not reentrant — public methods must not call other
// locking methods while holding the lock (AllNames uses the unlocked all()
// helper for this reason).
type LibraryRegistry struct {
	mu              sync.RWMutex
	libraries       map[string]*CompiledLibrary // key: library name as "scheme/base"
	loading         map[string]chan struct{}    // in-flight loads → latch closed on completion
	searchPaths     []string                    // directories to search for library files
	importObserver  LibraryImportObserver       // optional: called on each library import
	compileObserver LibraryCompileObserver      // optional: called once per library, between compile and execute
}

// DefaultLibraryPaths are the default directories to search for libraries.
//
// Only the current directory is searched by default. The embedded standard
// library is the real source of the stdlib (resolved via the FileResolver chain
// over stdlib.FS); the former "./pkg/stdlib/lib" entry was a development-tree
// convenience that resolves nothing in a deployed binary.
var DefaultLibraryPaths = []string{
	".",
}

// NewLibraryRegistry creates a new library registry with default search paths.
func NewLibraryRegistry() *LibraryRegistry {
	return &LibraryRegistry{
		libraries:   make(map[string]*CompiledLibrary),
		loading:     make(map[string]chan struct{}),
		searchPaths: DefaultLibraryPaths,
	}
}

// SetSearchPaths sets the library search paths. The slice is copied so the
// registry owns its searchPaths memory: a caller mutating the slice it passed
// in must not be able to race a concurrent reader of the registry's state.
func (p *LibraryRegistry) SetSearchPaths(paths []string) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.searchPaths = slices.Clone(paths)
}

// GetSearchPaths returns a copy of the current library search paths. Returning
// a copy keeps the registry's internal slice immutable from outside, so a
// caller cannot mutate the backing array and race a concurrent reader.
func (p *LibraryRegistry) GetSearchPaths() []string {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return slices.Clone(p.searchPaths)
}

// PrependSearchPath adds a path to the beginning of the search path list.
func (p *LibraryRegistry) PrependSearchPath(path string) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.searchPaths = append([]string{path}, p.searchPaths...)
}

// SetImportObserver sets an optional observer that is called each time
// a library is imported. The observer is read-only and cannot influence
// the import. Pass nil to remove the observer.
func (p *LibraryRegistry) SetImportObserver(obs LibraryImportObserver) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.importObserver = obs
}

// ImportObserver returns the current import observer, or nil.
func (p *LibraryRegistry) ImportObserver() LibraryImportObserver {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return p.importObserver
}

// SetCompileObserver sets an optional observer that is called each time a
// library body finishes compiling, before it executes. Pass nil to remove
// the observer.
func (p *LibraryRegistry) SetCompileObserver(obs LibraryCompileObserver) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.compileObserver = obs
}

// CompileObserver returns the current compile observer, or nil.
func (p *LibraryRegistry) CompileObserver() LibraryCompileObserver {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return p.compileObserver
}

// registryOf returns the *LibraryRegistry stored on env's namespace, or nil
// when none is configured or the stored searcher is some other type. For
// callers that treat both cases as "no registry"; LoadLibrary reports them
// as distinct errors and does its own lookup.
func registryOf(env *environment.EnvironmentFrame) *LibraryRegistry {
	reg, _ := env.LibraryRegistry().(*LibraryRegistry)
	return reg
}

// fireCompileObserver calls the compile observer if one is set on the
// registry stored in env.
func fireCompileObserver(env *environment.EnvironmentFrame, lib *CompiledLibrary) {
	reg := registryOf(env)
	if reg == nil {
		return
	}
	obs := reg.CompileObserver()
	if obs == nil {
		return
	}
	obs(lib)
}

// fireImportObserver calls the import observer if one is set on the
// registry stored in env. bindings maps local name -> external name
// (as returned by ApplyToExports). importer is the importing library's
// name, or zero value for top-level imports.
func fireImportObserver(env *environment.EnvironmentFrame, lib *CompiledLibrary, bindings map[string]string, importer LibraryName, stage ImportStage) {
	reg := registryOf(env)
	if reg == nil {
		return
	}
	obs := reg.ImportObserver()
	if obs == nil {
		return
	}

	exports := slices.Sorted(maps.Keys(lib.Exports))
	imported := slices.Sorted(maps.Keys(bindings))

	obs(LibraryImportEvent{
		Library:    lib.Name,
		SourceFile: lib.SourceFile,
		Exports:    exports,
		Imported:   imported,
		Importer:   importer,
		Stage:      stage,
	})
}

// Register adds a compiled library to the registry.
func (p *LibraryRegistry) Register(lib *CompiledLibrary) error {
	p.mu.Lock()
	defer p.mu.Unlock()
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
	p.mu.RLock()
	defer p.mu.RUnlock()
	return p.libraries[name.Key()]
}

// all returns all loaded libraries sorted by key. Caller must hold at least
// RLock. Exists so AllNames can reuse it without recursively locking (mu is
// not reentrant).
func (p *LibraryRegistry) all() []*CompiledLibrary {
	return slices.SortedFunc(maps.Values(p.libraries), func(a, b *CompiledLibrary) int {
		return cmp.Compare(a.Name.Key(), b.Name.Key())
	})
}

// All returns all loaded libraries, sorted by name key for determinism.
func (p *LibraryRegistry) All() []*CompiledLibrary {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return p.all()
}

// AllNames returns the names of all registered libraries, sorted by key.
func (p *LibraryRegistry) AllNames() []LibraryName {
	p.mu.RLock()
	defer p.mu.RUnlock()
	libs := p.all()
	names := make([]LibraryName, len(libs))
	for i, lib := range libs {
		names[i] = lib.Name
	}
	return names
}

// IsLoading reports whether the library is currently being loaded by some
// goroutine. Diagnostic only: it does not distinguish which goroutine, and it
// is not part of the claim protocol (that is LookupClaimOrWait → FinishLoading).
// Cycle detection — re-entry on the caller's own load chain — is handled in
// LoadLibrary via the ctx-borne load chain, not here.
func (p *LibraryRegistry) IsLoading(name LibraryName) bool {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return p.loading[name.Key()] != nil
}

// FinishLoading marks a library as finished loading and wakes every goroutine
// waiting on its latch (whether the load succeeded and Registered or failed).
// Waiters re-consult the registry: a successful load is now cached; a failed
// one is neither cached nor loading, so a waiter re-claims and retries.
func (p *LibraryRegistry) FinishLoading(name LibraryName) {
	p.mu.Lock()
	defer p.mu.Unlock()
	key := name.Key()
	ch := p.loading[key]
	if ch == nil {
		// No latch installed: the legitimate "name was never claimed" case (e.g.
		// FinishLoading on a slot whose load never started). The protocol has one
		// owner per claim, each deferring exactly one FinishLoading, so a double
		// finish should not occur; this guard simply makes the absent case a no-op
		// rather than a close-of-nil-channel panic.
		return
	}
	close(ch)
	delete(p.loading, key)
}

// LookupClaimOrWait atomically resolves a library, claims its loading slot, or
// returns a latch to wait on. It is the single check-and-mark decision point so
// the lookup → claim sequence cannot interleave across threads. The two return
// values encode three outcomes; "claimed" is the both-nil case, not a separate
// flag, so the contradictory states a third flag would admit are unrepresentable:
//   - cached != nil              ⇒ already loaded; use it.
//   - cached == nil, wait != nil ⇒ another goroutine is loading this exact
//     library; block on wait, then re-call to read the now-cached result (or
//     re-claim if that load failed).
//   - cached == nil, wait == nil ⇒ the caller now owns the loading slot and MUST
//     load, Register, and FinishLoading (which closes the latch installed here).
//
// Unlike the former LookupOrClaim, a concurrent same-library load is NOT a
// circular-dependency error: that false positive is what blocked concurrent
// shared-dependency loads. A genuine import cycle (A→B→A) is re-entry on one
// goroutine's own synchronous load chain and is caught earlier, in LoadLibrary,
// via the ctx-borne load chain — before reaching this method — so a goroutine
// never waits on a latch it installed itself.
func (p *LibraryRegistry) LookupClaimOrWait(name LibraryName) (cached *CompiledLibrary, wait <-chan struct{}) {
	p.mu.Lock()
	defer p.mu.Unlock()
	key := name.Key()
	lib := p.libraries[key]
	if lib != nil {
		return lib, nil
	}
	ch := p.loading[key]
	if ch != nil {
		return nil, ch
	}
	p.loading[key] = make(chan struct{})
	return nil, nil
}

// FilePathToLibraryName converts a forward-slash-separated file path with
// .sld or .scm extension to a LibraryName. This is the inverse of ToFSPath().
// Returns an error if the path has no recognized extension or is empty.
func FilePathToLibraryName(path string) (LibraryName, error) {
	if path == "" {
		return LibraryName{}, werr.WrapForeignErrorf(
			werr.ErrInvalidArgument, "filePathToLibraryName: empty path",
		)
	}
	for _, ext := range libraryExtensions {
		trimmed, ok := strings.CutSuffix(path, ext)
		if ok {
			parts := strings.Split(trimmed, "/")
			return NewLibraryName(parts...), nil
		}
	}
	return LibraryName{}, werr.WrapForeignErrorf(
		werr.ErrInvalidArgument,
		"filePathToLibraryName: unrecognized extension in %q", path,
	)
}

// ResolveLibraryFile resolves a library name to an open file handle,
// trying each extension in libraryExtensions order.
// Non-file-not-found errors (security denial, I/O) propagate immediately: only
// the ABSENCE of the .sld licenses trying the .scm, or an unreadable .sld would
// silently be replaced by a readable file of the same name.
func ResolveLibraryFile(ctx context.Context, res FileResolver, name LibraryName) (fs.File, string, error) {
	basePath := name.Key()
	var lastErr error
	for _, ext := range libraryExtensions {
		f, filePath, err := res.ResolveAndOpen(ctx, basePath+ext)
		if err == nil {
			return f, filePath, nil
		}
		if !resolver.IsNotFound(err) {
			return nil, "", err
		}
		lastErr = err
	}
	return nil, "", lastErr
}
