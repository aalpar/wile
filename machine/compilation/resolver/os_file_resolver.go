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
	"os"
	"path/filepath"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine/compilation/sourceload"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/werr"
)

// OSFileResolver resolves files from the operating system filesystem,
// using the load path stack, library registry, SCHEME_INCLUDE_PATH,
// and CWD as fallback directories. It also enforces security authorization.
type OSFileResolver struct {
	env *environment.EnvironmentFrame
}

// NewOSFileResolver creates a resolver that finds files on the OS filesystem.
func NewOSFileResolver(env *environment.EnvironmentFrame) *OSFileResolver {
	return &OSFileResolver{
		env: env,
	}
}

// ResolveAndOpen finds a file by name and returns an open handle plus the
// resolved path.
//
// Absolute paths bypass the Finder — fs.FS does not support them — and are
// opened directly via openAuthorized. Relative paths are searched through a
// sourceload.Finder backed by os.DirFS("/"), which searches the current load
// directory (from the load path stack), library registry paths,
// SCHEME_INCLUDE_PATH, and CWD in that order.
func (p *OSFileResolver) ResolveAndOpen(_ context.Context, path string) (fs.File, string, error) {
	if path == "" {
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound, "resolve: empty filename")
	}
	if filepath.IsAbs(path) {
		return openAuthorized(p.env.Namespace().Authorizer(), path)
	}
	return p.resolveRelative(path)
}

// resolveRelative uses sourceload.Finder to locate a relative path across
// OS search directories, then enforces security authorization on the result.
//
// The search list is built as:
//  1. Current load directory from the load path stack (stack-relative, highest priority)
//  2. Configured OS search dirs (library registry, SCHEME_INCLUDE_PATH, CWD)
//
// All absolute OS paths are stripped of their leading "/" before being passed
// to the Finder, because os.DirFS("/") requires relative paths. The
// canonicalize function adds "/" back and resolves the final absolute path.
func (p *OSFileResolver) resolveRelative(path string) (fs.File, string, error) {
	searchDirs := p.osFSSearchDirs()
	finder := sourceload.NewFinder(
		os.DirFS("/"),
		searchDirs,
		sourceload.WithCanonicalize(canonicalizeOSPath),
	)

	f, resolved, err := finder.Open(path)
	if err != nil {
		if errors.Is(err, sourceload.ErrNotFound) {
			searched := p.buildSearchedList(searchDirs)
			if len(searched) == 0 {
				return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound,
					"file %q not found (no search paths available)", path)
			}
			return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound,
				"file %q not found; searched: %s", path, strings.Join(searched, ", "))
		}
		return nil, "", err
	}

	// Authorization check on the resolved absolute path. Close the file
	// on denial — the caller never sees it.
	authErr := security.CheckWithAuthorizer(p.env.Namespace().Authorizer(), security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionLoad,
		Target:   resolved,
	})
	if authErr != nil {
		f.Close() //nolint:errcheck // closing on denial; error irrelevant
		return nil, "", authErr
	}

	return f, resolved, nil
}

// osFSSearchDirs returns the ordered list of fs.FS-compatible search directories
// for os.DirFS("/"): load-path-stack current dir first (if set), then the
// configured OS search dirs. All paths are absolutized (to resolve ".." etc.)
// then have their leading "/" stripped for fs.FS compatibility.
func (p *OSFileResolver) osFSSearchDirs() []string {
	var dirs []string

	tracker := p.env.LoadPathStack()
	if tracker != nil {
		cur := tracker.CurrentDir()
		if cur != "" && cur != "." {
			dirs = append(dirs, cur)
		}
	}

	dirs = append(dirs, osSearchDirs(p.env)...)
	return absolutizeAndStrip(dirs)
}

// buildSearchedList returns the OS-absolute directories that were searched,
// for inclusion in not-found error messages.
func (p *OSFileResolver) buildSearchedList(fsDirs []string) []string {
	q := make([]string, len(fsDirs))
	for i, d := range fsDirs {
		q[i] = "/" + d + "/"
	}
	return q
}

// canonicalizeOSPath converts an fs.FS-relative path (no leading slash) back
// to an absolute OS path, then resolves it to a clean absolute form via
// filepath.Abs. The leading slash was stripped so os.DirFS("/") could accept
// the path; this function restores it.
func canonicalizeOSPath(fsPath string) string {
	abs := "/" + fsPath
	resolved, err := filepath.Abs(abs)
	if err != nil {
		return abs
	}
	return resolved
}

// absolutizeAndStrip converts OS paths to fs.FS-compatible paths for os.DirFS("/").
// Each path is made absolute via filepath.Abs (resolving ".." and relative paths),
// then the leading "/" is stripped. Paths that fail Abs are dropped.
func absolutizeAndStrip(dirs []string) []string {
	q := make([]string, 0, len(dirs))
	for _, d := range dirs {
		abs, err := filepath.Abs(d)
		if err != nil {
			continue
		}
		q = append(q, strings.TrimPrefix(abs, "/"))
	}
	return q
}

// EnumerateFiles walks the OS filesystem to discover .sld/.scm files.
// Searches the same directories as ResolveAndOpen: library registry paths,
// SCHEME_INCLUDE_PATH, and CWD.
//
// Best-effort: non-existent directories and unauthorized files are skipped.
// Walk errors are joined and returned alongside partial results.
func (p *OSFileResolver) EnumerateFiles() ([]string, error) {
	auth := p.env.Namespace().Authorizer()
	var result []string
	var walkErrs []error

	for _, dir := range osSearchDirs(p.env) {
		err := WalkOSSchemeFiles(dir, auth, func(relPath string) {
			result = append(result, relPath)
		})
		if err != nil {
			walkErrs = append(walkErrs, err)
		}
	}

	return result, errors.Join(walkErrs...)
}
