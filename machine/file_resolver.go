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
