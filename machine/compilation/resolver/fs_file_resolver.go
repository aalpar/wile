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

package resolver

import (
	"context"
	"errors"
	"io/fs"
	pathpkg "path"
	"path/filepath"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/werr"
)

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

// ResolveAndOpen finds a file by name and returns an open handle plus the
// resolved path.
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

// EnumerateFiles walks the virtual filesystem to discover all .sld/.scm files.
// Library registry search paths are walked first (each with its own relPath
// base), then the FS root is walked to find files not under any search path.
// Hidden directories (starting with ".") are always skipped.
//
// Best-effort: non-existent directories and unauthorized files are skipped.
// Walk errors are joined and returned alongside partial results.
func (p *FSFileResolver) EnumerateFiles() ([]string, error) {
	auth := p.env.Namespace().Authorizer()
	var result []string
	var walkErrs []error

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
		err := WalkFSSchemeFiles(p.fsys, dir, auth, nil, func(relPath string) {
			result = append(result, relPath)
		})
		if err != nil {
			walkErrs = append(walkErrs, err)
		}
	}

	// Walk FS root, skipping subdirs already covered by search paths above
	// to avoid producing incorrect relPath values from path prefix artifacts.
	err := WalkFSSchemeFiles(p.fsys, ".", auth, func(dir string) bool {
		return walkedPaths[dir]
	}, func(relPath string) {
		result = append(result, relPath)
	})
	if err != nil {
		walkErrs = append(walkErrs, err)
	}

	return result, errors.Join(walkErrs...)
}
