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
	"io/fs"
	"os"
	"path/filepath"

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
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound, "include: empty filename")
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

	err = security.Check(ctx, security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionLoad,
		Target:   absPath,
	})
	if err != nil {
		return nil, "", err
	}

	f, err := os.Open(absPath)
	if err != nil {
		return nil, "", err
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
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound, "include: empty filename")
	}
	f, err := p.fsys.Open(path)
	if err != nil {
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound, "include: %s", path)
	}
	return f, path, nil
}
