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

package security

import (
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestDenyAll(t *testing.T) {
	c := qt.New(t)
	auth := DenyAll()

	tcs := []struct {
		name string
		req  AccessRequest
	}{
		{"file read", AccessRequest{Resource: ResourceFile, Action: ActionRead, Target: "/tmp/x"}},
		{"code load", AccessRequest{Resource: ResourceCode, Action: ActionLoad, Target: "lib.sld"}},
		{"env read", AccessRequest{Resource: ResourceEnv, Action: ActionRead, Target: "PATH"}},
		{"process exit", AccessRequest{Resource: ResourceProcess, Action: ActionExit, Target: ""}},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := auth.Authorize(tc.req)
			c.Assert(errors.Is(err, ErrAccessDenied), qt.IsTrue)
		})
	}
}

func TestReadOnly(t *testing.T) {
	c := qt.New(t)
	auth := ReadOnly()

	tcs := []struct {
		name  string
		req   AccessRequest
		allow bool
	}{
		{"file read", AccessRequest{Resource: ResourceFile, Action: ActionRead, Target: "/tmp/x"}, true},
		{"file stat", AccessRequest{Resource: ResourceFile, Action: ActionStat, Target: "/tmp/x"}, true},
		// ReadOnly applies no path confinement: it reads any path the host can.
		{"file read any path (no confinement)", AccessRequest{Resource: ResourceFile, Action: ActionRead, Target: "/etc/passwd"}, true},
		// R11: ReadOnly must NOT permit loading (and running) code.
		{"code load denied", AccessRequest{Resource: ResourceCode, Action: ActionLoad, Target: "lib.sld"}, false},
		{"file write", AccessRequest{Resource: ResourceFile, Action: ActionWrite, Target: "/tmp/x"}, false},
		{"file delete", AccessRequest{Resource: ResourceFile, Action: ActionDelete, Target: "/tmp/x"}, false},
		{"process exit", AccessRequest{Resource: ResourceProcess, Action: ActionExit, Target: ""}, false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := auth.Authorize(tc.req)
			if tc.allow {
				c.Assert(err, qt.IsNil)
			} else {
				c.Assert(errors.Is(err, ErrAccessDenied), qt.IsTrue)
			}
		})
	}
}

// TestReadOnlyWithLoad pins R11's explicit opt-in: ReadOnlyWithLoad is ReadOnly
// plus ActionLoad, for callers that genuinely need to load code under an
// otherwise read-only policy.
func TestReadOnlyWithLoad(t *testing.T) {
	c := qt.New(t)
	auth := ReadOnlyWithLoad()

	tcs := []struct {
		name  string
		req   AccessRequest
		allow bool
	}{
		{"file read", AccessRequest{Resource: ResourceFile, Action: ActionRead, Target: "/tmp/x"}, true},
		{"file stat", AccessRequest{Resource: ResourceFile, Action: ActionStat, Target: "/tmp/x"}, true},
		{"code load allowed", AccessRequest{Resource: ResourceCode, Action: ActionLoad, Target: "lib.sld"}, true},
		{"file write", AccessRequest{Resource: ResourceFile, Action: ActionWrite, Target: "/tmp/x"}, false},
		{"file delete", AccessRequest{Resource: ResourceFile, Action: ActionDelete, Target: "/tmp/x"}, false},
		{"process exit", AccessRequest{Resource: ResourceProcess, Action: ActionExit, Target: ""}, false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := auth.Authorize(tc.req)
			if tc.allow {
				c.Assert(err, qt.IsNil)
			} else {
				c.Assert(errors.Is(err, ErrAccessDenied), qt.IsTrue)
			}
		})
	}
}

func TestFilesystemRoot(t *testing.T) {
	c := qt.New(t)
	auth := FilesystemRoot("/app/data")

	tcs := []struct {
		name  string
		req   AccessRequest
		allow bool
	}{
		{"file inside root", AccessRequest{Resource: ResourceFile, Action: ActionRead, Target: "/app/data/file.txt"}, true},
		{"file at root", AccessRequest{Resource: ResourceFile, Action: ActionRead, Target: "/app/data"}, true},
		{"file in subdir", AccessRequest{Resource: ResourceFile, Action: ActionRead, Target: "/app/data/sub/deep.txt"}, true},
		{"code inside root", AccessRequest{Resource: ResourceCode, Action: ActionLoad, Target: "/app/data/lib.sld"}, true},
		{"file outside root", AccessRequest{Resource: ResourceFile, Action: ActionRead, Target: "/etc/passwd"}, false},
		{"file parent traversal", AccessRequest{Resource: ResourceFile, Action: ActionRead, Target: "/app/data/../secret"}, false},
		{"file sibling", AccessRequest{Resource: ResourceFile, Action: ActionRead, Target: "/app/other/file"}, false},
		{"exec inside root", AccessRequest{Resource: ResourceFile, Action: ActionExec, Target: "/app/data/tool"}, true},
		{"exec outside root", AccessRequest{Resource: ResourceFile, Action: ActionExec, Target: "/bin/sh"}, false},
		// Deny-by-default: an unmodelled resource is refused, not waved through.
		// These three were the escape routes behind the old `default: return nil`.
		{"env read (unmodelled)", AccessRequest{Resource: ResourceEnv, Action: ActionRead, Target: "PATH"}, false},
		{"process exit (unmodelled)", AccessRequest{Resource: ResourceProcess, Action: ActionExit, Target: ""}, false},
		{"shell exec (unmodelled)", AccessRequest{Resource: ResourceProcess, Action: ActionExecShell, Target: "echo x > /outside"}, false},
		{"namespace create (unmodelled)", AccessRequest{Resource: ResourceNamespace, Action: ActionCreate, Target: "kitchen-sink"}, false},
		// The one exemption: streams carry no path, and All() could never
		// re-widen a denial here. Matches ReadOnly and SandboxAuthorizer.
		{"stream write (exempt)", AccessRequest{Resource: ResourceStream, Action: ActionWrite, Target: StreamStdout}, true},
		{"stream read (exempt)", AccessRequest{Resource: ResourceStream, Action: ActionRead, Target: StreamStdin}, true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := auth.Authorize(tc.req)
			if tc.allow {
				c.Assert(err, qt.IsNil, qt.Commentf("expected allow for %s", tc.name))
			} else {
				c.Assert(errors.Is(err, ErrAccessDenied), qt.IsTrue,
					qt.Commentf("expected deny for %s", tc.name))
			}
		})
	}
}

// TestBuiltinsDenyProcessExecution enumerates every built-in Authorizer
// constructor and pins that none of them permits process execution.
//
// It enumerates rather than spot-checks because the property being pinned is a
// property of the whole SET: checkProfileWidening
// (pkg/internal/bootstrap/profile_containment.go) rests on "built-in
// authorizers deny unknown resources by default", and until 2026-08-21 that
// premise was false for exactly two members — FilesystemRoot and its
// virtual-sources sibling, whose `default: return nil` let
// KitchenSink + FilesystemRoot("/tmp") run (system "echo x > /outside")
// directly, no (environment …) call required. A new constructor that reopens
// the hole fails here rather than in a review two releases later.
//
// namespace:create and file:exec are recorded in the same table: the shell
// escape had two routes, and both must stay shut.
func TestBuiltinsDenyProcessExecution(t *testing.T) {
	c := qt.New(t)

	builtins := []struct {
		name string
		auth Authorizer
	}{
		{"DenyAll", DenyAll()},
		{"ReadOnly", ReadOnly()},
		{"ReadOnlyWithLoad", ReadOnlyWithLoad()},
		{"FilesystemRoot", FilesystemRoot("/app/data")},
		{"FilesystemRootWithVirtualSources", FilesystemRootWithVirtualSources("/app/data")},
		{"ConsoleAuthorizer", ConsoleAuthorizer()},
		{"ConsoleWithLoadAuthorizer", ConsoleWithLoadAuthorizer()},
		{"ConsoleWithLoadAllowingVirtualSources", ConsoleWithLoadAllowingVirtualSources()},
		{"SandboxAuthorizer", SandboxAuthorizer("WILE_")},
	}

	// Every request here names something outside any root the table constructs,
	// so a containment authorizer denies on containment and a resource-switching
	// one denies on the resource. Both are the answer this test wants.
	denied := []struct {
		name string
		req  AccessRequest
	}{
		{"process:exec-shell", AccessRequest{Resource: ResourceProcess, Action: ActionExecShell, Target: "echo x > /outside"}},
		{"process:exec", AccessRequest{Resource: ResourceProcess, Action: ActionExec, Target: "/bin/sh"}},
		{"namespace:create", AccessRequest{Resource: ResourceNamespace, Action: ActionCreate, Target: "kitchen-sink"}},
		{"file:exec outside root", AccessRequest{Resource: ResourceFile, Action: ActionExec, Target: "/bin/sh"}},
	}

	for _, b := range builtins {
		for _, d := range denied {
			t.Run(b.name+"/"+d.name, func(t *testing.T) {
				err := b.auth.Authorize(d.req)
				c.Assert(errors.Is(err, ErrAccessDenied), qt.IsTrue,
					qt.Commentf("%s must deny %s, got %v", b.name, d.name, err))
			})
		}
	}
}

func TestAll_Empty(t *testing.T) {
	c := qt.New(t)
	auth := All()
	err := auth.Authorize(AccessRequest{Resource: ResourceFile, Action: ActionWrite, Target: "/tmp/x"})
	c.Assert(err, qt.IsNil)
}

func TestAll_SinglePassthrough(t *testing.T) {
	c := qt.New(t)
	inner := ReadOnly()
	auth := All(inner)
	// All with single arg returns that arg directly
	err := auth.Authorize(AccessRequest{Resource: ResourceFile, Action: ActionRead, Target: "/tmp/x"})
	c.Assert(err, qt.IsNil)
	err = auth.Authorize(AccessRequest{Resource: ResourceFile, Action: ActionWrite, Target: "/tmp/x"})
	c.Assert(errors.Is(err, ErrAccessDenied), qt.IsTrue)
}

func TestAll_Composition(t *testing.T) {
	c := qt.New(t)
	// ReadOnly allows reads; FilesystemRoot restricts to /app/data.
	// Composed: only reads inside /app/data are allowed.
	auth := All(ReadOnly(), FilesystemRoot("/app/data"))

	tcs := []struct {
		name  string
		req   AccessRequest
		allow bool
	}{
		{"read inside", AccessRequest{Resource: ResourceFile, Action: ActionRead, Target: "/app/data/f.txt"}, true},
		{"read outside", AccessRequest{Resource: ResourceFile, Action: ActionRead, Target: "/etc/passwd"}, false},
		{"write inside", AccessRequest{Resource: ResourceFile, Action: ActionWrite, Target: "/app/data/f.txt"}, false},
		{"write outside", AccessRequest{Resource: ResourceFile, Action: ActionWrite, Target: "/etc/passwd"}, false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := auth.Authorize(tc.req)
			if tc.allow {
				c.Assert(err, qt.IsNil)
			} else {
				c.Assert(errors.Is(err, ErrAccessDenied), qt.IsTrue)
			}
		})
	}
}

func TestAll_ShortCircuit(t *testing.T) {
	c := qt.New(t)
	calls := 0
	counter := AuthorizerFunc(func(_ AccessRequest) error {
		calls++
		return nil
	})
	auth := All(DenyAll(), counter)
	err := auth.Authorize(AccessRequest{Resource: ResourceFile, Action: ActionRead, Target: "/tmp/x"})
	c.Assert(errors.Is(err, ErrAccessDenied), qt.IsTrue)
	c.Assert(calls, qt.Equals, 0) // counter never reached
}
