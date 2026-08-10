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
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/werr"
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

// TestFSFileResolver_GatesBeforeOpen is the FS twin of the test above, and
// proves the same ordering the same way: the only candidate is a FIFO, so an
// open that precedes the gate blocks until a writer appears. The FS resolver
// used to find the file through sourceload.Finder — which opens to decide
// existence — and consult the authorizer only afterwards.
func TestFSFileResolver_GatesBeforeOpen(t *testing.T) {
	dir := realDir(t, t.TempDir())
	err := syscall.Mkfifo(filepath.Join(dir, "trap.scm"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	ns := environment.NewNamespace()
	ns.SetAuthorizer(security.DenyAll())
	r := NewFSFileResolver(os.DirFS(dir), ns.Runtime())

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

// TestOSFileResolver_StatPermissionIsNotNotFound pins the other half of the
// fall-through protocol on the OS side. A stat that fails with EACCES says
// nothing about whether the file exists, so reporting it as not-found both lies
// to the caller and lets the search continue into a lower-priority directory.
func TestOSFileResolver_StatPermissionIsNotNotFound(t *testing.T) {
	if os.Geteuid() == 0 {
		t.Skip("root bypasses directory permission bits")
	}
	base := realDir(t, t.TempDir())
	locked := filepath.Join(base, "locked")
	err := os.Mkdir(locked, 0o755)
	qt.Assert(t, err, qt.IsNil)
	err = os.WriteFile(filepath.Join(locked, "hidden.scm"), []byte("(display 1)"), 0o644)
	qt.Assert(t, err, qt.IsNil)
	err = os.Chmod(locked, 0o000)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = os.Chmod(locked, 0o755)
	})

	t.Setenv(SchemeIncludePathEnv, base)
	ns := environment.NewNamespace()
	r := NewOSFileResolver(ns.Runtime())

	_, _, err = r.ResolveAndOpen(context.Background(), "locked/hidden.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsFalse,
		qt.Commentf("EACCES on the parent directory is not an absence"))
	qt.Assert(t, errors.Is(err, fs.ErrPermission), qt.IsTrue,
		qt.Commentf("the cause must survive the wrap"))
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
