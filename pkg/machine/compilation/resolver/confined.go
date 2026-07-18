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
	"os"
	"path/filepath"
	"strings"

	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/werr"
)

// The source-load path (include / import / load) opens through os.Root whenever
// the engine's authorizer confines filesystem access to a root, mirroring the
// file primitives (extensions/files/confined.go). The authorizer gate runs on a
// lexical path, so between the check and the open a component of that path can
// be swapped for a symlink pointing elsewhere; os.Root resolves each component
// against a directory descriptor and refuses symlink/".." escapes at the syscall
// level, so the swapped path can no longer redirect the open outside the root.
// Without a confinement root there is no root to escape, and the plain open is
// used.

// relWithinRoot expresses target relative to root, tolerating a symlinked root
// (macOS /tmp -> /private/tmp) by retrying against the resolved root. os.Root
// enforces containment itself; this only computes the name handed to it.
func relWithinRoot(root, target string) (string, error) {
	abs, err := filepath.Abs(target)
	if err != nil {
		return "", err
	}
	rel, relErr := filepath.Rel(root, abs)
	if relErr == nil && rel != ".." && !strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
		return rel, nil
	}
	realRoot, evalErr := filepath.EvalSymlinks(root)
	if evalErr != nil {
		return rel, relErr
	}
	return filepath.Rel(realRoot, abs)
}

// openUnconfined opens absPath when auth imposes no os.Root confinement. A
// custom (non-RootConfined) path-gating authorizer may authorize absPath
// lexically while a symlink at that path redirects the open to a file outside
// the authorized subtree; a plain os.Open would follow it. Resolving symlinks
// and re-gating the real path keeps the gate target and the open target the
// same file. When the path does not resolve (missing or broken link) the plain
// open reports the underlying error.
func openUnconfined(auth security.Authorizer, absPath string) (*os.File, error) {
	realPath, err := filepath.EvalSymlinks(absPath)
	if err != nil {
		return os.Open(absPath)
	}
	if realPath == absPath {
		return os.Open(absPath)
	}
	err = security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionLoad,
		Target:   realPath,
	})
	if err != nil {
		return nil, err
	}
	return os.Open(realPath)
}

// confinedOpenFile opens absPath for reading, race-free when auth confines
// filesystem access to a root. Under confinement a non-regular file (FIFO,
// device) is refused: a sandboxed load has no business blocking on a planted
// pipe, and the confined caller can never have meant /dev/stdin.
func confinedOpenFile(auth security.Authorizer, absPath string) (*os.File, error) {
	dir, confined := security.ConfinementRootOf(auth)
	if !confined {
		return openUnconfined(auth, absPath)
	}
	root, err := os.OpenRoot(dir)
	if err != nil {
		return nil, err
	}
	defer root.Close() //nolint:errcheck // read-only root handle; close error is irrelevant
	rel, err := relWithinRoot(dir, absPath)
	if err != nil {
		return nil, err
	}
	f, err := root.Open(rel)
	if err != nil {
		return nil, err
	}
	fi, err := f.Stat()
	if err != nil {
		f.Close() //nolint:errcheck // failed open; close error is irrelevant
		return nil, err
	}
	if !fi.Mode().IsRegular() {
		f.Close() //nolint:errcheck // refused open; close error is irrelevant
		return nil, werr.WrapForeignErrorf(werr.ErrFileOpen,
			"open %s: not a regular file", absPath)
	}
	return f, nil
}
