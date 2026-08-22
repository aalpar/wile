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

package process_test

import (
	"context"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
	"testing"

	qt "github.com/frankban/quicktest"

	extprocess "github.com/aalpar/wile/extensions/process"
	extio "github.com/aalpar/wile/pkg/extensions/io"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"
)

// confinedExec permits process execution outright and confines every file:*
// request to root, reporting root as its ConfinementRoot.
//
// It has to be written here rather than composed from built-ins, and that is the
// point of the test: security.All is an intersection, and the built-in
// FilesystemRoot denies ResourceProcess, so any composition of built-ins that
// confines files also blocks the capability gate. Only an authorizer shaped like
// this one can tell the two gates apart — it says yes at process:exec and is then
// the sole thing standing between the program and an arbitrary host binary.
type confinedExec struct {
	root string
	mu   sync.Mutex
	seen []security.AccessRequest
}

func (p *confinedExec) Authorize(req security.AccessRequest) error {
	p.mu.Lock()
	p.seen = append(p.seen, req)
	p.mu.Unlock()
	if req.Resource != security.ResourceFile {
		return nil
	}
	if !underRoot(p.root, req.Target) {
		return security.ErrAccessDenied
	}
	return nil
}

func (p *confinedExec) ConfinementRoot() (string, bool) {
	return p.root, true
}

// requestsFor returns every recorded request matching resource and action.
func (p *confinedExec) requestsFor(resource, action string) []security.AccessRequest {
	p.mu.Lock()
	defer p.mu.Unlock()
	var q []security.AccessRequest
	for _, req := range p.seen {
		if req.Resource == resource && req.Action == action {
			q = append(q, req)
		}
	}
	return q
}

// rootReporter allows everything but reports a confinement root, so a test can
// observe where a spawned child is pinned without any denial interfering.
type rootReporter struct {
	root string
}

func (rootReporter) Authorize(security.AccessRequest) error {
	return nil
}

func (p rootReporter) ConfinementRoot() (string, bool) {
	return p.root, true
}

// underRoot is the test's own containment check, deliberately independent of
// pkg/security's, so a containment bug there cannot make these tests pass.
// Both sides are symlink-resolved: macOS /tmp is /private/tmp, and t.TempDir
// hands back a path under a symlinked /var.
func underRoot(root, target string) bool {
	realRoot, err := filepath.EvalSymlinks(root)
	if err != nil {
		realRoot = root
	}
	realTarget, err := filepath.EvalSymlinks(target)
	if err != nil {
		realTarget = target
	}
	if realTarget == realRoot {
		return true
	}
	return strings.HasPrefix(realTarget, realRoot+string(filepath.Separator))
}

func newProcessEngine(t *testing.T, auth security.Authorizer) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extio.Extension),
		wile.WithExtension(extprocess.Extension),
		wile.WithAuthorizer(auth),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

// TestSystemGatesTheShellAsAFile pins that (system …) files the shell it runs as
// file:exec, so an authorizer confining files to a root refuses it.
//
// Before this gate the shell was invisible to the policy layer: process:exec-shell
// carried the COMMAND STRING as its target, and /bin/sh — the actual host binary,
// and a general-purpose unconfined file accessor — was never named in any request.
func TestSystemGatesTheShellAsAFile(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("system uses /bin/sh")
	}
	c := qt.New(t)
	auth := &confinedExec{root: t.TempDir()}
	engine := newProcessEngine(t, auth)

	outside := filepath.Join(t.TempDir(), "escaped")
	expr, err := engine.Parse(context.Background(), `(system "echo x > `+outside+`")`)
	c.Assert(err, qt.IsNil)
	_, err = engine.Eval(context.Background(), expr)
	c.Assert(err, qt.IsNotNil)

	_, statErr := os.Stat(outside)
	c.Assert(os.IsNotExist(statErr), qt.IsTrue,
		qt.Commentf("the shell ran despite the denial: %q exists", outside))

	execs := auth.requestsFor(security.ResourceFile, security.ActionExec)
	c.Assert(len(execs) > 0, qt.IsTrue, qt.Commentf("no file:exec request was made"))
	var sawShell bool
	for _, req := range execs {
		if req.Target == "/bin/sh" {
			sawShell = true
		}
	}
	c.Assert(sawShell, qt.IsTrue,
		qt.Commentf("file:exec never named /bin/sh; got %v", execs))
}

// TestSpawnGatesTheResolvedBinary pins that the file:exec target is the path the
// OS will execute, not the string the program wrote.
//
// exec.Command LookPath-resolves a bare name into cmd.Path, so gating
// command.Value would authorize a name while running a file — and PATH decides
// which one. The assertion is that an absolute, resolved path reaches the
// authorizer.
func TestSpawnGatesTheResolvedBinary(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix commands")
	}
	c := qt.New(t)
	auth := &confinedExec{root: t.TempDir()}
	engine := newProcessEngine(t, auth)

	expr, err := engine.Parse(context.Background(), `(process-spawn "echo" "hi")`)
	c.Assert(err, qt.IsNil)
	_, err = engine.Eval(context.Background(), expr)
	c.Assert(err, qt.IsNotNil, qt.Commentf("echo lives outside the root and must be refused"))

	execs := auth.requestsFor(security.ResourceFile, security.ActionExec)
	var sawResolved bool
	for _, req := range execs {
		if filepath.IsAbs(req.Target) && filepath.Base(req.Target) == "echo" {
			sawResolved = true
		}
	}
	c.Assert(sawResolved, qt.IsTrue,
		qt.Commentf("file:exec did not carry a resolved absolute path for echo; got %v", execs))
}

// TestSpawnUnderRootIsPermitted is the anti-over-denial ratchet: a binary INSIDE
// the confinement root still runs. Without it, a gate that denied everything
// would pass every other test in this file.
func TestSpawnUnderRootIsPermitted(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses a shell script")
	}
	c := qt.New(t)
	root := t.TempDir()
	script := filepath.Join(root, "hello.sh")
	err := os.WriteFile(script, []byte("#!/bin/sh\necho hello\n"), 0o755)
	c.Assert(err, qt.IsNil)

	engine := newProcessEngine(t, &confinedExec{root: root})
	result := eval(t, engine, `
		(let ((proc (process-spawn "`+script+`")))
		  (let ((line (read-line (process-stdout proc))))
		    (process-wait proc)
		    line))
	`)
	c.Assert(result.Internal().(*values.String).Value, qt.Equals, "hello")
}

// TestSpawnStartsInTheConfinementRoot pins the cmd.Dir pinning.
//
// cmd.Dir defaults to the empty string, which means "inherit the caller's working
// directory" — so a sandboxed engine whose host runs outside its own root handed
// the child a starting point no policy ever saw, and every relative path the child
// opened resolved there. The child now starts at the root the authorizer reports.
func TestSpawnStartsInTheConfinementRoot(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses /bin/pwd")
	}
	c := qt.New(t)
	root := t.TempDir()

	// The host process is NOT in root, so an unpinned child would report the
	// test binary's working directory instead.
	wd, err := os.Getwd()
	c.Assert(err, qt.IsNil)
	c.Assert(underRoot(root, wd), qt.IsFalse,
		qt.Commentf("test precondition: the host must run outside root"))

	engine := newProcessEngine(t, rootReporter{root: root})
	result := eval(t, engine, `
		(let ((proc (process-spawn "/bin/pwd")))
		  (let ((line (read-line (process-stdout proc))))
		    (process-wait proc)
		    line))
	`)
	// Compared by containment, not by string: pwd reports the LOGICAL path
	// (/tmp/x), t.TempDir and EvalSymlinks disagree with it on macOS
	// (/private/tmp/x), and neither difference is the property under test.
	got := result.Internal().(*values.String).Value
	c.Assert(underRoot(root, got), qt.IsTrue,
		qt.Commentf("child started at %q, outside the confinement root %q", got, root))
}

// TestStartDirectoryIsGated pins that the pinned directory is itself authorized —
// POSIX x on a directory is traverse, and a child entering one is a traversal the
// policy gets to refuse.
func TestStartDirectoryIsGated(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix commands")
	}
	c := qt.New(t)
	root := t.TempDir()
	auth := &confinedExec{root: root}
	engine := newProcessEngine(t, auth)

	expr, err := engine.Parse(context.Background(), `(process-spawn "echo")`)
	c.Assert(err, qt.IsNil)
	_, _ = engine.Eval(context.Background(), expr)

	execs := auth.requestsFor(security.ResourceFile, security.ActionExec)
	var sawDir bool
	for _, req := range execs {
		if req.Target == root {
			sawDir = true
		}
	}
	c.Assert(sawDir, qt.IsTrue,
		qt.Commentf("the start directory %q was never gated; got %v", root, execs))
}
