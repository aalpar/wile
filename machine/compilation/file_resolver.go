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

import (
	"context"
	"errors"
	"io/fs"
	"os"
	pathpkg "path"
	"path/filepath"
	"sort"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/werr"
)

// FileResolver resolves and opens files for include/load operations.
// The interface is defined in the environment package; this alias keeps
// the name available in compilation without re-declaration.
type FileResolver = environment.FileResolver

// LibraryEnumerator is an optional interface that FileResolvers can implement
// to support library discovery. Enumeration is the inverse of resolution:
// same directories, same priority, but walking files instead of looking up
// a specific name.
type LibraryEnumerator interface {
	EnumerateLibraries() ([]LibraryName, error)
}

// isLibraryFile reports whether the filename has a .sld or .scm extension.
func isLibraryFile(name string) bool {
	return strings.HasSuffix(name, ".sld") || strings.HasSuffix(name, ".scm")
}

// isHidden reports whether the name starts with ".".
func isHidden(name string) bool {
	return len(name) > 0 && name[0] == '.'
}

// isAuthorized reports whether the security authorizer permits loading the
// given path. Returns true when no authorizer is configured (open sandbox).
func isAuthorized(auth security.Authorizer, target string) bool {
	return security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionLoad,
		Target:   target,
	}) == nil
}

// --- Shared helpers ---

// osSearchDirs returns the fallback directory list for OS-based file search.
// Search order: library registry paths → SCHEME_INCLUDE_PATH → CWD.
// This order is shared by OSFileResolver.ResolveAndOpen and EnumerateLibraries.
func osSearchDirs(env *environment.EnvironmentFrame) []string {
	var dirs []string
	reg := env.LibraryRegistry()
	if reg != nil {
		dirs = append(dirs, reg.GetSearchPaths()...)
	}
	includePath := os.Getenv(SchemeIncludePathEnv)
	if includePath != "" {
		dirs = append(dirs, filepath.SplitList(includePath)...)
	}
	cwd, err := os.Getwd()
	if err == nil {
		dirs = append(dirs, cwd)
	}
	return dirs
}

// openAuthorized performs security authorization then opens absPath on the OS filesystem.
func openAuthorized(auth security.Authorizer, absPath string) (fs.File, string, error) {
	err := security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionLoad,
		Target:   absPath,
	})
	if err != nil {
		return nil, "", err
	}
	f, err := os.Open(absPath)
	if err != nil {
		sentinel := werr.ErrFileOpen
		if errors.Is(err, os.ErrNotExist) {
			sentinel = werr.ErrFileNotFound
		}
		return nil, "", werr.WrapForeignErrorWithCause(sentinel, err, "open %s", absPath)
	}
	return f, absPath, nil
}

// walkOSLibraries walks baseDir on the OS filesystem, calling fn with the
// slash-separated path of each .sld/.scm file relative to baseDir.
// Hidden directories and unauthorized files are silently skipped.
// Returns the WalkDir error so callers can observe unexpected walk failures.
func walkOSLibraries(baseDir string, auth security.Authorizer, fn func(relPath string)) error {
	return filepath.WalkDir(baseDir, func(path string, d fs.DirEntry, walkErr error) error {
		if d == nil {
			return fs.SkipAll
		}
		if d.IsDir() {
			if walkErr != nil {
				return filepath.SkipDir
			}
			if path != baseDir && isHidden(d.Name()) {
				return filepath.SkipDir
			}
			return nil
		}
		absPath, absErr := filepath.Abs(path)
		if walkErr != nil || absErr != nil || !isLibraryFile(d.Name()) || !isAuthorized(auth, absPath) {
			return nil //nolint:nilerr // skip unreadable/irrelevant/denied files, continue walking
		}
		rel, relErr := filepath.Rel(baseDir, path)
		if relErr == nil {
			fn(filepath.ToSlash(rel))
		}
		return nil
	})
}

// walkFSDir walks baseDir in fsys, calling fn with the path of each
// .sld/.scm file relative to baseDir. Hidden directories and unauthorized
// files are silently skipped. Non-existent directories are skipped (fs.SkipAll).
// If skipSubdir is non-nil, subdirectory paths returning true are also skipped.
// Returns the WalkDir error so callers can observe unexpected walk failures.
func walkFSDir(fsys fs.FS, baseDir string, auth security.Authorizer, skipSubdir func(string) bool, fn func(relPath string)) error {
	prefix := baseDir
	if prefix == "." {
		prefix = ""
	}
	return fs.WalkDir(fsys, baseDir, func(path string, d fs.DirEntry, walkErr error) error {
		if d == nil {
			return fs.SkipAll
		}
		if d.IsDir() {
			if walkErr != nil {
				return fs.SkipDir
			}
			if path != baseDir && (isHidden(d.Name()) || (skipSubdir != nil && skipSubdir(path))) {
				return fs.SkipDir
			}
			return nil
		}
		if walkErr != nil || !isLibraryFile(d.Name()) {
			return nil //nolint:nilerr // skip unreadable/irrelevant files, continue walking
		}
		relPath := strings.TrimPrefix(path, prefix+"/")
		if prefix == "" {
			relPath = path
		}
		if !isAuthorized(auth, relPath) {
			return nil
		}
		fn(relPath)
		return nil
	})
}

// --- OSFileResolver ---

// OSFileResolver resolves files from the operating system filesystem,
// using the load path stack, library registry, SCHEME_INCLUDE_PATH,
// and CWD as fallback directories. It also enforces security authorization.
type OSFileResolver struct {
	env *environment.EnvironmentFrame
}

// NewOSFileResolver creates a resolver that finds files on the OS filesystem.
func NewOSFileResolver(env *environment.EnvironmentFrame) *OSFileResolver {
	return &OSFileResolver{env: env}
}

func (p *OSFileResolver) ResolveAndOpen(ctx context.Context, path string) (fs.File, string, error) {
	if path == "" {
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound, "resolve: empty filename")
	}
	absPath, err := environment.ResolveFile(p.env.LoadPathStack(), path, osSearchDirs(p.env))
	if err != nil {
		return nil, "", err
	}
	return openAuthorized(p.env.Namespace().Authorizer(), absPath)
}

// EnumerateLibraries walks the OS filesystem to discover importable libraries.
// Searches the same directories as ResolveAndOpen: library registry paths,
// SCHEME_INCLUDE_PATH, and CWD. First directory wins on duplicate library names.
//
// Best-effort: non-existent directories and unauthorized files are skipped.
// Walk errors are joined and returned alongside partial results.
func (p *OSFileResolver) EnumerateLibraries() ([]LibraryName, error) {
	auth := p.env.Namespace().Authorizer()
	seen := make(map[string]bool)
	var result []LibraryName
	var walkErrs []error

	for _, dir := range osSearchDirs(p.env) {
		err := walkOSLibraries(dir, auth, func(relPath string) {
			name, nameErr := FilePathToLibraryName(relPath)
			if nameErr != nil {
				return
			}
			key := name.Key()
			if !seen[key] {
				seen[key] = true
				result = append(result, name)
			}
		})
		if err != nil {
			walkErrs = append(walkErrs, err)
		}
	}

	sort.Slice(result, func(i, j int) bool {
		return result[i].Key() < result[j].Key()
	})
	return result, errors.Join(walkErrs...)
}

// --- EmbedFileResolver ---

// EmbedFileResolver resolves files from an embedded filesystem (or any fs.FS).
// No path resolution or security checks — paths are looked up directly.
type EmbedFileResolver struct {
	fsys fs.FS
}

// NewEmbedFileResolver creates a resolver backed by the given filesystem.
func NewEmbedFileResolver(fsys fs.FS) *EmbedFileResolver {
	return &EmbedFileResolver{fsys: fsys}
}

func (p *EmbedFileResolver) ResolveAndOpen(_ context.Context, path string) (fs.File, string, error) {
	if path == "" {
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound, "resolve: empty filename")
	}
	f, err := p.fsys.Open(path)
	if err != nil {
		return nil, "", werr.WrapForeignErrorWithCause(werr.ErrFileNotFound, err, "resolve: %s", path)
	}
	return f, path, nil
}

// --- FSFileResolver ---

// FSFileResolver resolves files from a virtual filesystem (fs.FS).
// Used when an embedder provides WithSourceFS. All paths are relative
// to the FS root. Absolute paths are rejected.
//
// Resolution priority:
//  1. Relative to current load directory (from LoadPathStack)
//  2. Library registry search paths
//  3. Relative to FS root (path as-is)
type FSFileResolver struct {
	fsys fs.FS
	env  *environment.EnvironmentFrame
}

// NewFSFileResolver creates a resolver backed by the given filesystem.
// Panics if fsys is nil.
func NewFSFileResolver(fsys fs.FS, env *environment.EnvironmentFrame) *FSFileResolver {
	if fsys == nil {
		panic(werr.WrapForeignErrorf(werr.ErrEngineInit, "NewFSFileResolver: fsys must not be nil"))
	}
	return &FSFileResolver{
		fsys: fsys,
		env:  env,
	}
}

// fsDirs returns the ordered list of base directories to search within the
// virtual filesystem: load-path stack current dir (if set), then registry
// search paths. The FS root (".") is appended by callers as the final fallback.
func (p *FSFileResolver) fsDirs() []string {
	var dirs []string
	stack := p.env.LoadPathStack()
	if stack != nil {
		d := stack.CurrentDir()
		if d != "" && d != "." {
			dirs = append(dirs, d)
		}
	}
	reg := p.env.LibraryRegistry()
	if reg != nil {
		for _, d := range reg.GetSearchPaths() {
			if d != "" {
				dirs = append(dirs, d)
			}
		}
	}
	return dirs
}

func (p *FSFileResolver) ResolveAndOpen(_ context.Context, path string) (fs.File, string, error) {
	if path == "" {
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound, "resolve: empty filename")
	}
	if filepath.IsAbs(path) {
		return nil, "", werr.WrapForeignErrorf(
			werr.ErrFileNotFound,
			"resolve: absolute paths not supported in virtual filesystem: %s",
			path,
		)
	}

	var searched []string
	for _, dir := range append(p.fsDirs(), ".") {
		candidate := pathpkg.Join(dir, path)
		if !fs.ValidPath(candidate) {
			if dir == "." {
				searched = append(searched, "<fs-root>/")
			} else {
				searched = append(searched, dir+"/")
			}
			continue
		}
		switch _, err := fs.Stat(p.fsys, candidate); {
		case err == nil:
			return p.openChecked(candidate)
		case errors.Is(err, fs.ErrNotExist):
			// not here; try next dir
		default:
			return nil, "", werr.WrapForeignErrorWithCause(
				werr.ErrFileNotFound, err,
				"stat %s in virtual filesystem", candidate,
			)
		}
		if dir == "." {
			searched = append(searched, "<fs-root>/")
		} else {
			searched = append(searched, dir+"/")
		}
	}

	return nil, "", werr.WrapForeignErrorf(
		werr.ErrFileNotFound,
		"file %q not found in virtual filesystem; searched: %s",
		path,
		strings.Join(searched, ", "),
	)
}

// openChecked performs security authorization and opens a resolved FS path.
func (p *FSFileResolver) openChecked(resolvedPath string) (fs.File, string, error) {
	err := security.CheckWithAuthorizer(p.env.Namespace().Authorizer(), security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionLoad,
		Target:   resolvedPath,
	})
	if err != nil {
		return nil, "", err
	}
	f, err := p.fsys.Open(resolvedPath)
	if err != nil {
		sentinel := werr.ErrFileOpen
		if errors.Is(err, fs.ErrNotExist) {
			sentinel = werr.ErrFileNotFound
		}
		return nil, "", werr.WrapForeignErrorWithCause(sentinel, err, "open %s", resolvedPath)
	}
	return f, resolvedPath, nil
}

// EnumerateLibraries walks the virtual filesystem to discover all libraries.
// Library registry search paths are walked first (each with its own relPath
// base), then the FS root is walked to find files not under any search path.
// Duplicate library names (same key) are deduplicated with earlier discovery
// winning. Hidden directories (starting with ".") are always skipped.
//
// Best-effort: non-existent directories and unauthorized files are skipped.
// Walk errors are joined and returned alongside partial results.
func (p *FSFileResolver) EnumerateLibraries() ([]LibraryName, error) {
	auth := p.env.Namespace().Authorizer()
	seen := make(map[string]bool)
	var result []LibraryName
	var walkErrs []error

	addLib := func(relPath string) {
		name, nameErr := FilePathToLibraryName(relPath)
		if nameErr != nil {
			return
		}
		key := name.Key()
		if !seen[key] {
			seen[key] = true
			result = append(result, name)
		}
	}

	// Walk each registry search path first (relPath relative to each search dir).
	var searchPaths []string
	reg := p.env.LibraryRegistry()
	if reg != nil {
		searchPaths = reg.GetSearchPaths()
	}
	walkedPaths := make(map[string]bool, len(searchPaths))
	for _, dir := range searchPaths {
		if dir == "" || dir == "." {
			continue
		}
		walkedPaths[dir] = true
		err := walkFSDir(p.fsys, dir, auth, nil, addLib)
		if err != nil {
			walkErrs = append(walkErrs, err)
		}
	}

	// Walk FS root, skipping subdirs already covered by search paths above
	// to avoid producing incorrect relPath values from path prefix artifacts.
	err := walkFSDir(p.fsys, ".", auth, func(dir string) bool {
		return walkedPaths[dir]
	}, addLib)
	if err != nil {
		walkErrs = append(walkErrs, err)
	}

	sort.Slice(result, func(i, j int) bool {
		return result[i].Key() < result[j].Key()
	})
	return result, errors.Join(walkErrs...)
}

// --- ChainFileResolver ---

// ChainFileResolver tries multiple resolvers in order, falling through
// to the next on ErrFileNotFound. Non-file-not-found errors (security
// denials, I/O errors) propagate immediately.
type ChainFileResolver struct {
	resolvers []FileResolver
}

// NewChainFileResolver creates a resolver that tries each resolver in order.
// Panics if resolvers is empty.
func NewChainFileResolver(resolvers []FileResolver) *ChainFileResolver {
	if len(resolvers) == 0 {
		panic(werr.WrapForeignErrorf(werr.ErrEngineInit, "NewChainFileResolver: resolvers must not be empty"))
	}
	return &ChainFileResolver{resolvers: resolvers}
}

func (p *ChainFileResolver) ResolveAndOpen(ctx context.Context, path string) (fs.File, string, error) {
	var lastErr error
	for _, r := range p.resolvers {
		f, resolved, err := r.ResolveAndOpen(ctx, path)
		if err == nil {
			return f, resolved, nil
		}
		if !errors.Is(err, werr.ErrFileNotFound) {
			return nil, "", err
		}
		lastErr = err
	}
	return nil, "", lastErr
}

// EnumerateLibraries unions library enumerations from all child resolvers
// that implement LibraryEnumerator. First resolver wins on duplicate keys,
// matching the resolution priority order. Errors from child resolvers
// propagate immediately, matching ChainFileResolver.ResolveAndOpen behavior.
func (p *ChainFileResolver) EnumerateLibraries() ([]LibraryName, error) {
	seen := make(map[string]bool)
	var result []LibraryName

	for _, r := range p.resolvers {
		enumerator, ok := r.(LibraryEnumerator)
		if !ok {
			continue
		}
		libs, err := enumerator.EnumerateLibraries()
		if err != nil {
			return nil, err
		}
		for _, lib := range libs {
			key := lib.Key()
			if seen[key] {
				continue
			}
			seen[key] = true
			result = append(result, lib)
		}
	}

	sort.Slice(result, func(i, j int) bool {
		return result[i].Key() < result[j].Key()
	})
	return result, nil
}
