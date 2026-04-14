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

	"github.com/aalpar/wile/werr"
)

// EmbedFileResolver resolves files from an embedded filesystem (or any fs.FS).
// No path resolution or security checks — paths are looked up directly.
//
// EmbedFileResolver does NOT implement FileEnumerator because embedded
// filesystems typically contain a known, fixed set of files that do not
// need runtime discovery.
type EmbedFileResolver struct {
	fsys fs.FS
}

// NewEmbedFileResolver creates a resolver backed by the given filesystem.
// Panics if fsys is nil.
func NewEmbedFileResolver(fsys fs.FS) *EmbedFileResolver {
	if fsys == nil {
		panic(werr.WrapForeignErrorf(werr.ErrEngineInit, "NewEmbedFileResolver: fsys must not be nil"))
	}
	return &EmbedFileResolver{
		fsys: fsys,
	}
}

// ResolveAndOpen finds a file by name and returns an open handle plus the
// resolved path.
func (p *EmbedFileResolver) ResolveAndOpen(_ context.Context, path string) (fs.File, string, error) {
	if path == "" {
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound, "resolve: empty filename")
	}
	f, err := p.fsys.Open(path)
	if err != nil {
		sentinel := werr.ErrFileOpen
		if errors.Is(err, fs.ErrNotExist) {
			sentinel = werr.ErrFileNotFound
		}
		return nil, "", werr.WrapForeignErrorWithCause(sentinel, err, "resolve: %s", path)
	}
	return f, path, nil
}
