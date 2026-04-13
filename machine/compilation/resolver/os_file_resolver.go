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

	"github.com/aalpar/wile/environment"
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
func (p *OSFileResolver) ResolveAndOpen(_ context.Context, path string) (fs.File, string, error) {
	if path == "" {
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound, "resolve: empty filename")
	}
	absPath, err := environment.ResolveFile(p.env.LoadPathStack(), path, OSSearchDirs(p.env))
	if err != nil {
		return nil, "", err
	}
	return OpenAuthorized(p.env.Namespace().Authorizer(), absPath)
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

	for _, dir := range OSSearchDirs(p.env) {
		err := WalkOSSchemeFiles(dir, auth, func(relPath string) {
			result = append(result, relPath)
		})
		if err != nil {
			walkErrs = append(walkErrs, err)
		}
	}

	return result, errors.Join(walkErrs...)
}
