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

//go:build unix

package resolver

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/security"
)

// TestOSFileResolver_RelativeArmGatesBeforeOpen proves the ordering rather than
// asserting it indirectly: the only candidate in the search directory is a FIFO,
// whose open() blocks until a writer appears. If the resolver opened before
// authorizing (the old relative arm did, via os.DirFS), this call would hang
// forever on a path the authorizer denies. Gating first, it returns promptly.
func TestOSFileResolver_RelativeArmGatesBeforeOpen(t *testing.T) {
	dir := realDir(t, t.TempDir())
	err := syscall.Mkfifo(filepath.Join(dir, "trap.scm"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	t.Setenv(SchemeIncludePathEnv, dir)

	ns := environment.NewNamespace()
	ns.SetAuthorizer(security.DenyAll())
	r := NewOSFileResolver(ns.Runtime())

	errc := make(chan error, 1)
	go func() {
		_, _, resolveErr := r.ResolveAndOpen(context.Background(), "trap.scm")
		errc <- resolveErr
	}()

	select {
	case resolveErr := <-errc:
		qt.Assert(t, resolveErr, qt.IsNotNil)
		qt.Assert(t, errors.Is(resolveErr, security.ErrAccessDenied), qt.IsTrue)
	case <-time.After(5 * time.Second):
		// The goroutine is stuck inside open(2) on the FIFO; it never returns,
		// so the test binary will leak it. That is the defect, made visible.
		t.Fatal("ResolveAndOpen blocked: the file was opened before the authorizer denied it")
	}
}

// TestOSFileResolver_SymlinkEscapeDeniedUnconfined proves the non-confining arm
// (a custom, non-RootConfined path-gating authorizer) does not follow a
// pre-planted symlink out of the authorized subtree. The authorizer allows only
// targets lexically under allowedDir; a symlink inside it points at a file
// outside it. The lexical gate on the symlink path passes, but the open must be
// denied because the real target escapes the subtree.
func TestOSFileResolver_SymlinkEscapeDeniedUnconfined(t *testing.T) {
	allowedDir := realDir(t, t.TempDir())
	outsideDir := realDir(t, t.TempDir())

	secret := filepath.Join(outsideDir, "secret.scm")
	err := os.WriteFile(secret, []byte("secret"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	link := filepath.Join(allowedDir, "link.scm")
	err = os.Symlink(secret, link)
	qt.Assert(t, err, qt.IsNil)

	ns := environment.NewNamespace()
	ns.SetAuthorizer(security.AuthorizerFunc(func(req security.AccessRequest) error {
		if strings.HasPrefix(req.Target, allowedDir+string(filepath.Separator)) {
			return nil
		}
		return security.ErrAccessDenied
	}))
	r := NewOSFileResolver(ns.Runtime())

	_, _, err = r.ResolveAndOpen(context.Background(), link)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}
