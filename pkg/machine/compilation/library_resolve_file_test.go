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
	"strings"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine/compilation/resolver"
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

// pathReportingAbsence reports every path absent and names the path it was
// asked for, so a test can see which extension probes survived into the error.
type pathReportingAbsence struct{}

func (pathReportingAbsence) ResolveAndOpen(_ context.Context, path string) (fs.File, string, error) {
	return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound, "probe %s", path)
}

// TestResolveLibraryFile_TotalMissJoinsEveryExtensionProbe pins that a library
// miss names the probe for every recognized extension, .sld before .scm, not
// only the last one tried. The early return on a non-absence error is pinned by
// the test above.
func TestResolveLibraryFile_TotalMissJoinsEveryExtensionProbe(t *testing.T) {
	_, _, err := ResolveLibraryFile(context.Background(), pathReportingAbsence{}, NewLibraryName("scheme", "nonexistent-xyz"))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue)
	msg := err.Error()
	sld := strings.Index(msg, "probe scheme/nonexistent-xyz.sld")
	scm := strings.Index(msg, "probe scheme/nonexistent-xyz.scm")
	qt.Assert(t, sld >= 0, qt.IsTrue, qt.Commentf(".sld probe absent from %q", msg))
	qt.Assert(t, scm > sld, qt.IsTrue, qt.Commentf(".scm probe not after .sld in %q", msg))
}

// sldAbsentScmUnreadable reports the .sld absent and the .scm hard-unreadable.
type sldAbsentScmUnreadable struct{}

func (sldAbsentScmUnreadable) ResolveAndOpen(_ context.Context, path string) (fs.File, string, error) {
	if strings.HasSuffix(path, ".sld") {
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound, "probe %s", path)
	}
	return nil, "", &fs.PathError{Op: "open", Path: path, Err: fs.ErrPermission}
}

// TestResolveLibraryFile_HardScmErrorDropsTheEarlierSldAbsence pins that a hard
// error on the second probe is returned alone: it is not an absence, and the
// .sld absence collected before it must not ride along and make it look like one.
func TestResolveLibraryFile_HardScmErrorDropsTheEarlierSldAbsence(t *testing.T) {
	_, _, err := ResolveLibraryFile(context.Background(), sldAbsentScmUnreadable{}, NewLibraryName("alpha"))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, fs.ErrPermission), qt.IsTrue)
	qt.Assert(t, resolver.IsNotFound(err), qt.IsFalse,
		qt.Commentf("an earlier absence rode into a hard error: %v", err))
	qt.Assert(t, strings.Contains(err.Error(), ".sld"), qt.IsFalse)
}
