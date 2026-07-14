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
	"io"
	"os"
	"path/filepath"
	"sync/atomic"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/security"
)

// permissiveRootAuthorizer reports a confinement root but authorizes every
// request. It models the exact TOCTOU window the os.Root layer exists to close:
// the policy check passed on the lexical path it was handed, and the question is
// what the *open* can still reach. A real authorizer (FilesystemRoot) would also
// reject a statically-escaping symlink at the policy layer, which would mask the
// syscall-level containment this test is about.
type permissiveRootAuthorizer struct {
	root string
}

func (permissiveRootAuthorizer) Authorize(_ security.AccessRequest) error {
	return nil
}

func (p permissiveRootAuthorizer) ConfinementRoot() (string, bool) {
	return p.root, true
}

// countingAuthorizer records the code:load targets it was asked about.
type countingAuthorizer struct {
	calls   atomic.Int64
	targets chan string
	err     error
}

func (p *countingAuthorizer) Authorize(req security.AccessRequest) error {
	p.calls.Add(1)
	select {
	case p.targets <- req.Target:
	default:
	}
	return p.err
}

// TestOSFileResolver_ConfinedOpenRefusesEscapingSymlink pins the check/open
// inode mismatch: even when the authorizer permits the lexical path, the open
// goes through os.Root and cannot follow a symlink out of the confinement root.
func TestOSFileResolver_ConfinedOpenRefusesEscapingSymlink(t *testing.T) {
	outside := realDir(t, t.TempDir())
	root := realDir(t, t.TempDir())

	secret := filepath.Join(outside, "secret.scm")
	err := os.WriteFile(secret, []byte("(define stolen 1)"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	link := filepath.Join(root, "escape.scm")
	err = os.Symlink(secret, link)
	qt.Assert(t, err, qt.IsNil)

	ns := environment.NewNamespace()
	ns.SetAuthorizer(permissiveRootAuthorizer{root: root})
	r := NewOSFileResolver(ns.Runtime())

	f, _, err := r.ResolveAndOpen(context.Background(), link)
	if err == nil {
		defer f.Close() //nolint:errcheck // failure path; the test already lost
		content, readErr := io.ReadAll(f)
		qt.Assert(t, readErr, qt.IsNil)
		t.Fatalf("escaping symlink was followed out of the confinement root: %q", content)
	}
}

// TestOSFileResolver_ConfinedOpenRefusesEscapingSymlinkRelative is the same
// property on the relative (search-path) arm, which previously opened via
// os.DirFS("/") — a symlink-following FS — before consulting the authorizer.
func TestOSFileResolver_ConfinedOpenRefusesEscapingSymlinkRelative(t *testing.T) {
	outside := realDir(t, t.TempDir())
	root := realDir(t, t.TempDir())

	secret := filepath.Join(outside, "secret.scm")
	err := os.WriteFile(secret, []byte("(define stolen 1)"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	err = os.Symlink(secret, filepath.Join(root, "escape.scm"))
	qt.Assert(t, err, qt.IsNil)

	t.Setenv(SchemeIncludePathEnv, root)

	ns := environment.NewNamespace()
	ns.SetAuthorizer(permissiveRootAuthorizer{root: root})
	r := NewOSFileResolver(ns.Runtime())

	f, _, err := r.ResolveAndOpen(context.Background(), "escape.scm")
	if err == nil {
		defer f.Close() //nolint:errcheck // failure path; the test already lost
		content, readErr := io.ReadAll(f)
		qt.Assert(t, readErr, qt.IsNil)
		t.Fatalf("escaping symlink was followed out of the confinement root: %q", content)
	}
}

// TestOSFileResolver_SymlinkSwapRace runs the live swap the lexical gate cannot
// see: a name in the search directory alternates between a benign file and a
// symlink escaping the root while resolution runs. Either outcome is acceptable
// (the benign file, or an error) — what must never happen is the outside file's
// bytes reaching the caller.
func TestOSFileResolver_SymlinkSwapRace(t *testing.T) {
	outside := realDir(t, t.TempDir())
	root := realDir(t, t.TempDir())

	secret := filepath.Join(outside, "secret.scm")
	err := os.WriteFile(secret, []byte("STOLEN"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	benign := filepath.Join(root, "benign.scm")
	err = os.WriteFile(benign, []byte("BENIGN"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	target := filepath.Join(root, "target.scm")
	staging := filepath.Join(root, "staging.scm")

	t.Setenv(SchemeIncludePathEnv, root)

	ns := environment.NewNamespace()
	ns.SetAuthorizer(permissiveRootAuthorizer{root: root})
	r := NewOSFileResolver(ns.Runtime())

	done := make(chan struct{})
	go func() {
		defer close(done)
		for i := range 400 {
			// Rename is atomic, so the resolver always sees one of the two
			// files at target.scm — never a partially built one.
			os.Remove(staging) //nolint:errcheck // best-effort staging cleanup
			if i%2 == 0 {      //nolint:gocritic // alternation is the point
				os.Symlink(secret, staging) //nolint:errcheck // racing writer; failures are fine
			} else {
				os.Link(benign, staging) //nolint:errcheck // racing writer; failures are fine
			}
			os.Rename(staging, target) //nolint:errcheck // racing writer; failures are fine
		}
	}()

	for range 400 {
		f, _, resolveErr := r.ResolveAndOpen(context.Background(), "target.scm")
		if resolveErr != nil {
			continue
		}
		content, readErr := io.ReadAll(f)
		f.Close() //nolint:errcheck // read complete
		qt.Assert(t, readErr, qt.IsNil)
		qt.Assert(t, string(content), qt.Not(qt.Equals), "STOLEN")
	}
	<-done
}

// TestOSFileResolver_RelativeArmDeniedTargetIsCandidatePath pins that the
// relative arm consults the authorizer with the resolved candidate path (not
// the bare name), and surfaces the denial rather than a not-found error.
func TestOSFileResolver_RelativeArmDeniedTargetIsCandidatePath(t *testing.T) {
	dir := realDir(t, t.TempDir())
	target := filepath.Join(dir, "denied.scm")
	err := os.WriteFile(target, []byte("secret"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	t.Setenv(SchemeIncludePathEnv, dir)

	auth := &countingAuthorizer{targets: make(chan string, 4), err: security.ErrAccessDenied}
	ns := environment.NewNamespace()
	ns.SetAuthorizer(auth)
	r := NewOSFileResolver(ns.Runtime())

	_, _, err = r.ResolveAndOpen(context.Background(), "denied.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
	qt.Assert(t, <-auth.targets, qt.Equals, target)
}

// TestFilesystemRootDeniesCodeEval pins the property the resolver's confinement
// story depends on: a root-confined engine must not be able to sidestep the
// file gate by calling (eval)/(compile) on an in-memory datum. It lives beside
// the resolver tests because the resolver is the consumer of that guarantee.
//
// The gate decides code:eval on the ACTION. It used to compare the literal target
// "<eval>" against the root as if it were a path, which resolved it against the
// process CWD and made the denial an accident of the working directory — a host
// whose CWD sat inside its own confinement root got eval for free. That is pinned
// where it belongs, in pkg/security:
// TestFilesystemRootDeniesCodeEvalRegardlessOfCWD.
func TestFilesystemRootDeniesCodeEval(t *testing.T) {
	root := realDir(t, t.TempDir())
	auth := security.FilesystemRoot(root)

	for _, target := range []string{"<eval>", "<compile>"} {
		err := security.CheckWithAuthorizer(auth, security.AccessRequest{
			Resource: security.ResourceCode,
			Action:   security.ActionEval,
			Target:   target,
		})
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
	}
}
