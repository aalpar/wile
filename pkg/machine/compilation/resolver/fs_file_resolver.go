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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine/compilation/sourceload"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/werr"
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

// fsRegistryDirs returns the registry search paths (non-empty entries only).
// The load-path-stack current dir is NOT included here — it is injected into
// the Finder via WithStack so the Finder handles directory-relative resolution.
func (p *FSFileResolver) fsRegistryDirs() []string {
	reg := p.env.LibraryRegistry()
	if reg == nil {
		return nil
	}
	var dirs []string
	for _, d := range reg.GetSearchPaths() {
		if d != "" {
			dirs = append(dirs, d)
		}
	}
	return dirs
}

// ResolveAndOpen finds a file by name and returns an open handle plus the
// resolved path.
func (p *FSFileResolver) ResolveAndOpen(ctx context.Context, path string) (fs.File, string, error) {
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

	registryDirs := p.fsRegistryDirs()
	var opts []sourceload.FinderOption
	s := SelectLoadStack(ctx)
	if s != nil {
		opts = append(opts, sourceload.WithStack(s))
	}
	finder := sourceload.NewFinder(p.fsys, registryDirs, opts...)

	// The candidate is authorized before the Finder is allowed to stat or open
	// it. The Finder is deliberately used in two halves here rather than through
	// Open: fs.Stat falls back to Open for an fs.FS that is not a fs.StatFS, so
	// "gate before the open" and "gate before the stat" are the same requirement.
	//
	// FSFileResolver sets no canonicalize function, so the candidate the helper
	// returns is already the resolved path.
	opener := func(candidate string) (fs.File, error) {
		q, _, openErr := finder.OpenCandidate(candidate)
		return q, openErr
	}
	auth := p.env.Namespace().Authorizer()
	f, resolved, err := authorizeCandidates(auth, security.SourceVirtualFS, finder.Candidates(path), opener)
	if err == nil {
		return f, resolved, nil
	}

	if !errors.Is(err, sourceload.ErrNotFound) {
		return nil, "", err
	}

	// Build the searched-dirs list for the not-found error, mirroring what
	// Finder.buildSearchDirs() would have tried: stack dir, registry dirs, root.
	searched := p.buildSearchedList(ctx, registryDirs)
	return nil, "", werr.WrapForeignErrorf(
		werr.ErrFileNotFound,
		"file %q not found in virtual filesystem; searched: %s",
		path,
		strings.Join(searched, ", "),
	)
}

// buildSearchedList returns human-readable labels for the directories that
// were searched, matching Finder.buildSearchDirs() order: stack dir (if any),
// registry dirs, then root.
func (p *FSFileResolver) buildSearchedList(ctx context.Context, registryDirs []string) []string {
	var searched []string
	s := SelectLoadStack(ctx)
	if s != nil {
		cur := s.CurrentDir()
		if cur != "" && cur != "." {
			searched = append(searched, cur+"/")
		}
	}
	for _, d := range registryDirs {
		searched = append(searched, d+"/")
	}
	searched = append(searched, "<fs-root>/")
	return searched
}

// EnumerateFiles walks the virtual filesystem to discover all .sld/.scm files.
// When registry search paths are configured, only those directories are walked.
// When no registry paths exist, the FS root "." is walked as a fallback.
// Hidden directories (starting with ".") are skipped.
//
// Best-effort: non-existent directories and unauthorized files are skipped.
// Walk errors are joined and returned alongside partial results.
func (p *FSFileResolver) EnumerateFiles() ([]string, error) {
	auth := p.env.Namespace().Authorizer()
	var result []string

	// Build deduplicated search dirs: registry paths (cleaned) then "." root.
	// Dedup is needed so that a registry path of "." doesn't cause duplicate
	// results when "." is also appended as the fallback root.
	searchDirs := p.buildEnumSearchDirs()

	err := sourceload.Walk(p.fsys, searchDirs, isSchemeFile, func(relPath string) {
		if isAuthorized(auth, relPath, security.SourceVirtualFS) {
			result = append(result, relPath)
		}
	})

	return result, err
}

// buildEnumSearchDirs returns the deduplicated, ordered list of directories
// to walk for EnumerateFiles. Registry paths (cleaned) are used when present;
// "." is the fallback when no registry paths are available. This avoids
// walking both a named search path and the FS root, which would yield the
// same file under two different relative paths.
func (p *FSFileResolver) buildEnumSearchDirs() []string {
	reg := p.env.LibraryRegistry()
	if reg == nil {
		return []string{"."}
	}

	var dirs []string
	seen := make(map[string]bool)
	for _, raw := range reg.GetSearchPaths() {
		dir := pathpkg.Clean(raw)
		if dir == "" {
			continue
		}
		if !seen[dir] {
			seen[dir] = true
			dirs = append(dirs, dir)
		}
	}
	if len(dirs) == 0 {
		return []string{"."}
	}
	return dirs
}
