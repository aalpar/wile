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
// Implementations control where files are found: the OS filesystem,
// an embedded filesystem, or any other fs.FS.
type FileResolver interface {
	// ResolveAndOpen finds a file by name and returns an open handle plus
	// the resolved path (used for load-path-stack tracking and error messages).
	ResolveAndOpen(ctx context.Context, path string) (fs.File, string, error)
}

// LibraryEnumerator is an optional interface that FileResolvers can implement
// to support library discovery. Enumeration is the inverse of resolution:
// same directories, same priority, but walking files instead of looking up
// a specific name.
type LibraryEnumerator interface {
	EnumerateLibraries() ([]LibraryName, error)
}

// isLibraryFile returns true if the filename has a .sld or .scm extension.
func isLibraryFile(name string) bool {
	return strings.HasSuffix(name, ".sld") || strings.HasSuffix(name, ".scm")
}

// isHidden returns true if the name starts with ".".
func isHidden(name string) bool {
	return len(name) > 0 && name[0] == '.'
}

// isAuthorized returns true if the security authorizer permits loading the
// given path. Returns true when no authorizer is configured (open sandbox).
func isAuthorized(auth security.Authorizer, target string) bool {
	return security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionLoad,
		Target:   target,
	}) == nil
}

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

	stack := p.env.LoadPathStack()

	// Build fallback directories from all configured sources.
	// Priority: library registry > SCHEME_INCLUDE_PATH > CWD.
	var fallbackDirs []string

	// Library registry search paths (shared with import).
	regAny := p.env.LibraryRegistry()
	if regAny != nil {
		reg, ok := regAny.(*LibraryRegistry)
		if ok {
			fallbackDirs = append(fallbackDirs, reg.GetSearchPaths()...)
		}
	}

	// SCHEME_INCLUDE_PATH env var (backward compatibility).
	includePath := os.Getenv(SchemeIncludePathEnv)
	if includePath != "" {
		fallbackDirs = append(fallbackDirs, filepath.SplitList(includePath)...)
	}

	// CWD as final fallback (matches Chez source-directories default, Racket current-directory).
	cwd, cwdErr := os.Getwd()
	if cwdErr == nil {
		fallbackDirs = append(fallbackDirs, cwd)
	}

	absPath, err := environment.ResolveFile(stack, path, fallbackDirs)
	if err != nil {
		return nil, "", err
	}

	auth, _ := p.env.Namespace().Authorizer().(security.Authorizer)
	err = security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionLoad,
		Target:   absPath,
	})
	if err != nil {
		return nil, "", err
	}

	f, err := os.Open(absPath)
	if err != nil {
		return nil, "", werr.WrapForeignErrorWithCause(werr.ErrFileNotFound, err, "open %s", absPath)
	}
	return f, absPath, nil
}

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

	// Strategy 1: Try relative to current load directory.
	stack := p.env.LoadPathStack()
	if stack != nil {
		currentDir := stack.CurrentDir()
		if currentDir != "" && currentDir != "." {
			candidate, err := p.statCandidate(currentDir, path)
			if err != nil {
				return nil, "", err
			}
			if candidate != "" {
				return p.openChecked(candidate)
			}
			searched = append(searched, currentDir+"/")
		}
	}

	// Strategy 2: Try library registry search paths.
	regAny := p.env.LibraryRegistry()
	if regAny != nil {
		reg, ok := regAny.(*LibraryRegistry)
		if ok {
			for _, dir := range reg.GetSearchPaths() {
				if dir == "" {
					continue
				}
				candidate, err := p.statCandidate(dir, path)
				if err != nil {
					return nil, "", err
				}
				if candidate != "" {
					return p.openChecked(candidate)
				}
				searched = append(searched, dir+"/")
			}
		}
	}

	// Strategy 3: Try path as-is at FS root.
	_, err := fs.Stat(p.fsys, path)
	if err == nil {
		return p.openChecked(path)
	}
	if !errors.Is(err, fs.ErrNotExist) {
		return nil, "", werr.WrapForeignErrorWithCause(
			werr.ErrFileNotFound, err,
			"stat %s in virtual filesystem", path,
		)
	}
	searched = append(searched, "<fs-root>/")

	searchedList := strings.Join(searched, ", ")
	return nil, "", werr.WrapForeignErrorf(
		werr.ErrFileNotFound,
		"file %q not found in virtual filesystem; searched: %s",
		path,
		searchedList,
	)
}

// statCandidate joins dir and file, validates the result is a legal fs.FS
// path, and stats it. Returns the resolved path on success, empty string
// if the file does not exist. Returns an error for non-ErrNotExist
// failures (I/O errors, permission errors).
func (p *FSFileResolver) statCandidate(dir, file string) (string, error) {
	candidate := pathpkg.Join(dir, file)
	if !fs.ValidPath(candidate) {
		return "", nil
	}
	_, err := fs.Stat(p.fsys, candidate)
	if err == nil {
		return candidate, nil
	}
	if !errors.Is(err, fs.ErrNotExist) {
		return "", werr.WrapForeignErrorWithCause(
			werr.ErrFileNotFound, err,
			"stat %s in virtual filesystem", candidate,
		)
	}
	return "", nil
}

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

// openChecked performs security authorization and opens a resolved path.
func (p *FSFileResolver) openChecked(resolvedPath string) (fs.File, string, error) {
	auth, _ := p.env.Namespace().Authorizer().(security.Authorizer)
	err := security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionLoad,
		Target:   resolvedPath,
	})
	if err != nil {
		return nil, "", err
	}

	f, err := p.fsys.Open(resolvedPath)
	if err != nil {
		return nil, "", werr.WrapForeignErrorWithCause(
			werr.ErrFileNotFound,
			err,
			"open %s",
			resolvedPath,
		)
	}
	return f, resolvedPath, nil
}

// EnumerateLibraries walks the OS filesystem to discover importable libraries.
// Walks library registry search paths, SCHEME_INCLUDE_PATH directories, and CWD,
// matching the same search order used by ResolveAndOpen.
//
// Best-effort: non-existent directories are skipped gracefully, and
// unreadable subdirectories are skipped without failing the whole walk.
// Only the security authorizer can exclude individual files.
func (p *OSFileResolver) EnumerateLibraries() ([]LibraryName, error) {
	auth, _ := p.env.Namespace().Authorizer().(security.Authorizer)
	seen := make(map[string]bool)
	var result []LibraryName

	walkDir := func(baseDir string) {
		_ = filepath.WalkDir(baseDir, func(path string, d fs.DirEntry, walkErr error) error {
			if d == nil {
				// Root does not exist or is unreadable — skip this search path.
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
			if walkErr != nil || !isLibraryFile(d.Name()) || !isAuthorized(auth, path) {
				return nil //nolint:nilerr // skip unreadable/irrelevant/denied files, continue walking
			}

			relPath, relErr := filepath.Rel(baseDir, path)
			if relErr == nil {
				relPath = filepath.ToSlash(relPath)
				name, nameErr := FilePathToLibraryName(relPath)
				if nameErr == nil {
					key := name.Key()
					if !seen[key] {
						seen[key] = true
						result = append(result, name)
					}
				}
			}
			return nil
		})
	}

	// Library registry search paths (same priority as ResolveAndOpen).
	regAny := p.env.LibraryRegistry()
	if regAny != nil {
		reg, ok := regAny.(*LibraryRegistry)
		if ok {
			for _, dir := range reg.GetSearchPaths() {
				walkDir(dir)
			}
		}
	}

	// SCHEME_INCLUDE_PATH (same as ResolveAndOpen).
	includePath := os.Getenv(SchemeIncludePathEnv)
	if includePath != "" {
		for _, dir := range filepath.SplitList(includePath) {
			walkDir(dir)
		}
	}

	// CWD fallback (same as ResolveAndOpen).
	cwd, err := os.Getwd()
	if err == nil {
		walkDir(cwd)
	}

	sort.Slice(result, func(i, j int) bool {
		return result[i].Key() < result[j].Key()
	})
	return result, nil
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

// EnumerateLibraries walks the virtual filesystem to discover all libraries.
// It uses the same search paths as resolution: library registry paths first,
// then the FS root as fallback. Hidden directories (starting with ".") are
// skipped. Files with .sld or .scm extensions are converted to library names
// via FilePathToLibraryName. Duplicate library names (same key from different
// paths or extensions) are deduplicated, with earlier discovery winning.
//
// Best-effort: non-existent directories are skipped gracefully, and
// unreadable subdirectories are skipped without failing the whole walk.
// Only the security authorizer can exclude individual files.
func (p *FSFileResolver) EnumerateLibraries() ([]LibraryName, error) {
	auth, _ := p.env.Namespace().Authorizer().(security.Authorizer)
	seen := make(map[string]bool)
	var result []LibraryName

	walkDir := func(baseDir string) error {
		prefix := baseDir
		if prefix == "." {
			prefix = ""
		}
		return fs.WalkDir(p.fsys, baseDir, func(path string, d fs.DirEntry, walkErr error) error {
			if d == nil {
				return fs.SkipAll
			}
			if d.IsDir() {
				if walkErr != nil {
					return fs.SkipDir
				}
				if path != baseDir && isHidden(d.Name()) {
					return fs.SkipDir
				}
				return nil
			}

			relPath := path
			if prefix != "" {
				relPath = strings.TrimPrefix(path, prefix+"/")
			}

			if walkErr != nil || !isLibraryFile(d.Name()) || !isAuthorized(auth, relPath) {
				return nil //nolint:nilerr // skip unreadable/irrelevant/denied files, continue walking
			}

			name, nameErr := FilePathToLibraryName(relPath)
			if nameErr == nil {
				key := name.Key()
				if !seen[key] {
					seen[key] = true
					result = append(result, name)
				}
			}
			return nil
		})
	}

	// Collect search paths and walk them first (same priority as resolution).
	var searchPaths []string
	regAny := p.env.LibraryRegistry()
	if regAny != nil {
		reg, ok := regAny.(*LibraryRegistry)
		if ok {
			searchPaths = reg.GetSearchPaths()
			for _, dir := range searchPaths {
				if dir == "" || dir == "." {
					continue
				}
				_ = walkDir(dir)
			}
		}
	}

	// Walk FS root as fallback (matches resolution strategy 3).
	// Skip subdirectories that are already covered by search paths
	// to avoid producing incorrect library names from path artifacts.
	walkedPaths := make(map[string]bool, len(searchPaths))
	for _, sp := range searchPaths {
		if sp != "" && sp != "." {
			walkedPaths[sp] = true
		}
	}

	_ = fs.WalkDir(p.fsys, ".", func(path string, d fs.DirEntry, walkErr error) error {
		if d == nil {
			return fs.SkipAll
		}
		if d.IsDir() {
			if walkErr != nil {
				return fs.SkipDir
			}
			if path != "." && (isHidden(d.Name()) || walkedPaths[path]) {
				return fs.SkipDir
			}
			return nil
		}
		if walkErr != nil || !isLibraryFile(d.Name()) || !isAuthorized(auth, path) {
			return nil //nolint:nilerr // skip unreadable/irrelevant/denied files, continue walking
		}

		name, nameErr := FilePathToLibraryName(path)
		if nameErr == nil {
			key := name.Key()
			if !seen[key] {
				seen[key] = true
				result = append(result, name)
			}
		}
		return nil
	})

	sort.Slice(result, func(i, j int) bool {
		return result[i].Key() < result[j].Key()
	})
	return result, nil
}
