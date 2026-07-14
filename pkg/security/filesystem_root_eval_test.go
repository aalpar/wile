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

package security_test

import (
	"errors"
	"os"
	"path/filepath"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/security"
)

// TestFilesystemRootDeniesCodeEvalRegardlessOfCWD is the real assertion behind
// "FilesystemRoot denies code:eval", and the one that was missing.
//
// code:eval arrives with the Target "<eval>" — a LABEL, not a path. FilesystemRoot
// used to hand it to containedInRoot anyway, which resolves a relative string
// against the process working directory. So the answer depended on where the host
// happened to be running: eval was denied because the CWD is *usually* outside the
// sandbox root, and a host whose CWD sat INSIDE its own confinement root silently
// got eval for free.
//
// A test that only checks the deny from a CWD outside the root passes on the broken
// code — the denial is right for the wrong reason, and the reason is what breaks.
// So this runs the SAME assertion from both working directories. The second
// subtest is the one that used to fail.
func TestFilesystemRootDeniesCodeEvalRegardlessOfCWD(t *testing.T) {
	root, err := filepath.EvalSymlinks(t.TempDir())
	qt.Assert(t, err, qt.IsNil)

	assertEvalDenied := func(t *testing.T) {
		t.Helper()
		auth := security.FilesystemRoot(root)
		for _, target := range []string{"<eval>", "<compile>"} {
			err := security.CheckWithAuthorizer(auth, security.AccessRequest{
				Resource: security.ResourceCode,
				Action:   security.ActionEval,
				Target:   target,
			})
			qt.Assert(t, err, qt.IsNotNil,
				qt.Commentf("code:eval on %q must be denied", target))
			qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
		}
	}

	t.Run("cwd outside the root", func(t *testing.T) {
		assertEvalDenied(t)
	})

	t.Run("cwd inside the root", func(t *testing.T) {
		prev, err := os.Getwd()
		qt.Assert(t, err, qt.IsNil)
		t.Cleanup(func() {
			_ = os.Chdir(prev)
		})
		qt.Assert(t, os.Chdir(root), qt.IsNil)

		// With the old path-containment gate, "<eval>" now resolved to
		// <root>/<eval> — inside the root — and the sandbox handed out eval.
		assertEvalDenied(t)
	})
}

// TestFilesystemRootStillGatesFilesByPath guards the other direction: fixing the
// eval arm must not turn FilesystemRoot into a blanket deny for the resource it
// actually exists to confine.
func TestFilesystemRootStillGatesFilesByPath(t *testing.T) {
	root, err := filepath.EvalSymlinks(t.TempDir())
	qt.Assert(t, err, qt.IsNil)
	auth := security.FilesystemRoot(root)

	inside := filepath.Join(root, "ok.txt")
	err = security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionRead,
		Target:   inside,
	})
	qt.Assert(t, err, qt.IsNil, qt.Commentf("a path inside the root must still be allowed"))

	err = security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionRead,
		Target:   "/etc/passwd",
	})
	qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue,
		qt.Commentf("a path outside the root must still be denied"))
}
