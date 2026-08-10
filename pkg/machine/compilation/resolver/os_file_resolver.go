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
	"syscall"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine/compilation/sourceload"
	"github.com/aalpar/wile/pkg/werr"
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
// Both arms — absolute path and search-path-relative — order identically:
// authorize the candidate, then open it. The open goes through os.Root when the
// authorizer confines filesystem access (see confined.go), so a path component
// swapped between the check and the open cannot redirect the open out of the
// confinement root.
func (p *OSFileResolver) ResolveAndOpen(ctx context.Context, path string) (fs.File, string, error) {
	if path == "" {
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound, "resolve: empty filename")
	}
	if filepath.IsAbs(path) {
		return openAuthorized(SelectAuthorizer(ctx, p.env), path)
	}
	return p.resolveRelative(ctx, path)
}

// AuthorizedSource reports the source every candidate is authorized under: the
// host OS filesystem, which security.AccessRequest spells as the zero value.
// See SourceGate.
func (*OSFileResolver) AuthorizedSource() string {
	return ""
}

// resolveRelative locates a relative path across the OS search directories.
//
// The search list is built as:
//  1. Current load directory from the load path stack (stack-relative, highest priority)
//  2. Configured OS search dirs (library registry, SCHEME_INCLUDE_PATH, CWD)
//
// The searched set is exactly what the not-found diagnostic reports. There is
// deliberately no filesystem-root last resort: it was justified as matching
// sourceload.Finder's "." fallback, but Finder's "." is the root of the virtual
// FS it searches, whose OS analogue is the CWD — already in the list. Joining a
// relative path onto "/" instead reinterprets it as an absolute host path, so
// from a CWD with no tmp/ an (include "tmp/x/oracle.scm") loaded and ran
// /tmp/x/oracle.scm, and (include "etc/passwd") read /etc/passwd, under the
// CLI's default unauthorized profile. See reviews/2026-08-07/REVIEW.md 2.1.5.
//
// Every candidate is authorized BEFORE it is stat'd, which is one step earlier
// than the obvious ordering and is what closes the existence oracle: statting
// first made a denied path that exists ("access denied") answer differently
// from one that does not ("not found"). The cost is that an authorizer now sees
// one request per search directory even for names that do not exist anywhere,
// so authorizer logs are noisier than the number of files actually opened.
//
// A stat failure that leaves existence undecided (EACCES on a parent
// directory) is propagated rather than swallowed: it says nothing about whether
// the file is there, so reporting not-found would both lie and let the search
// fall through to a lower-priority directory. A stat failure that DECIDES
// existence — see absenceErrnos — is absence and continues the search.
func (p *OSFileResolver) resolveRelative(ctx context.Context, path string) (fs.File, string, error) {
	auth := SelectAuthorizer(ctx, p.env)
	searchDirs := p.osAbsSearchDirs(ctx)

	candidates := make([]string, 0, len(searchDirs))
	for _, dir := range searchDirs {
		candidates = append(candidates, filepath.Join(dir, path))
	}

	opener := func(candidate string) (fs.File, error) {
		fi, statErr := os.Stat(candidate)
		if statErr != nil {
			return nil, classifyStatFailure(candidate, statErr)
		}
		if fi.IsDir() {
			return nil, werr.WrapForeignErrorWithCause(werr.ErrFileNotFound, fs.ErrNotExist,
				"candidate %s is a directory", candidate)
		}
		return openConfined(auth, candidate)
	}

	f, resolved, err := authorizeCandidates(auth, "", candidates, opener)
	if err == nil {
		return f, resolved, nil
	}
	if !errors.Is(err, sourceload.ErrNotFound) {
		return nil, "", err
	}

	searched := p.buildSearchedList(searchDirs)
	if len(searched) == 0 {
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound,
			"file %q not found (no search paths available)", path)
	}
	return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound,
		"file %q not found; searched: %s", path, strings.Join(searched, ", "))
}

// absenceErrnos are the stat failures that PROVE the candidate is not there, as
// opposed to leaving the question open. Each is decided by the name rather than
// by the file: ENOTDIR means a path component is not a directory, ENAMETOOLONG
// that no such name is representable, ELOOP that the name cannot be resolved at
// all. None can become a readable file, so each continues the search exactly as
// ENOENT does.
//
// The distinction is load-bearing because the search is ordered: treating
// ENOTDIR as a hard error stopped at the first directory whose parent component
// happened to be a regular file, so the copy sitting in a later search
// directory — or behind the next resolver in a chain — was never reached.
var absenceErrnos = []error{fs.ErrNotExist, syscall.ENOTDIR, syscall.ENAMETOOLONG, syscall.ELOOP}

// classifyStatFailure labels a stat failure for authorizeCandidates: absence
// (which continues the search) or a probe failure (which stops it).
//
// The sentinel names the operation that actually failed. Reporting a stat
// through werr.ErrFileOpen would tell a caller a descriptor was requested and
// refused, when nothing was opened.
func classifyStatFailure(candidate string, statErr error) error {
	for _, absent := range absenceErrnos {
		if errors.Is(statErr, absent) {
			return werr.WrapForeignErrorWithCause(werr.ErrFileNotFound, statErr, "stat %s", candidate)
		}
	}
	return werr.WrapForeignErrorWithCause(werr.ErrFileStat, statErr, "stat %s", candidate)
}

// osAbsSearchDirs returns the ordered, absolute search directories:
// load-path-stack current dir first (if set), then the configured OS search
// dirs. Paths that fail filepath.Abs are dropped.
//
// Duplicates are dropped after absolutization, keeping the first occurrence.
// The library registry's default "." and the CWD entry absolutize to the same
// directory, so without this the CWD was stat'd twice and named twice in the
// not-found diagnostic ("searched: /home/x/, /home/x/").
func (p *OSFileResolver) osAbsSearchDirs(ctx context.Context) []string {
	var dirs []string

	s := SelectLoadStack(ctx)
	if s != nil {
		cur := s.CurrentDir()
		if cur != "" && cur != "." {
			dirs = append(dirs, cur)
		}
	}

	dirs = append(dirs, osSearchDirs(p.env)...)

	seen := make(map[string]struct{}, len(dirs))
	q := make([]string, 0, len(dirs))
	for _, d := range dirs {
		abs, err := filepath.Abs(d)
		if err != nil {
			continue
		}
		_, dup := seen[abs]
		if dup {
			continue
		}
		seen[abs] = struct{}{}
		q = append(q, abs)
	}
	return q
}

// buildSearchedList returns the searched directories in display form, for
// inclusion in not-found error messages.
func (p *OSFileResolver) buildSearchedList(dirs []string) []string {
	q := make([]string, len(dirs))
	for i, d := range dirs {
		q[i] = strings.TrimSuffix(d, string(filepath.Separator)) + string(filepath.Separator)
	}
	return q
}

// EnumerateFiles walks the OS filesystem to discover .sld/.scm files.
// Searches osSearchDirs: library registry paths, SCHEME_INCLUDE_PATH, and CWD.
// Unlike ResolveAndOpen it does NOT consult the load-path stack's current
// directory, nor fall back to the filesystem root.
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
