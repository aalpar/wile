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
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/werr"
)

// sldPermissionFS answers alpha.sld with a hard fs.ErrPermission and every
// other name with not-exist. It deliberately does not implement fs.StatFS so
// that fs.Stat and Open surface the same failure.
type sldPermissionFS struct{}

func (sldPermissionFS) Open(name string) (fs.File, error) {
	if name == "alpha.sld" {
		return nil, &fs.PathError{Op: "open", Path: name, Err: fs.ErrPermission}
	}
	return nil, &fs.PathError{Op: "open", Path: name, Err: fs.ErrNotExist}
}

// TestResolveLibraryFile_HardSldErrorDoesNotFallThroughToScm is the extension
// twin of the chain fall-through: ResolveLibraryFile tries .sld then .scm, and
// only ABSENCE of the .sld may license the .scm. A hard error on alpha.sld used
// to be relabelled as not-found, so a readable alpha.scm was loaded in its
// place and the import silently got a different library.
func TestResolveLibraryFile_HardSldErrorDoesNotFallThroughToScm(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	backup := fstest.MapFS{"alpha.scm": {Data: []byte("(define-library (alpha))")}}
	chain := NewChainFileResolver([]environment.FileResolver{
		NewFSFileResolver(sldPermissionFS{}, env),
		NewEmbedFileResolver(backup),
	})

	f, filePath, err := ResolveLibraryFile(context.Background(), chain, NewLibraryName("alpha"))
	qt.Assert(t, f, qt.IsNil,
		qt.Commentf("an unreadable alpha.sld must not yield alpha.scm, got %q", filePath))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, fs.ErrPermission), qt.IsTrue,
		qt.Commentf("the cause must survive the wrap"))
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsFalse,
		qt.Commentf("a permission failure is not an absence"))
}
