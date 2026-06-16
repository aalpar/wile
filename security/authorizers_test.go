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
		{"file read", AccessRequest{ResourceFile, ActionRead, "/tmp/x"}},
		{"code load", AccessRequest{ResourceCode, ActionLoad, "lib.sld"}},
		{"env read", AccessRequest{ResourceEnv, ActionRead, "PATH"}},
		{"process exit", AccessRequest{ResourceProcess, ActionExit, ""}},
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
		{"file read", AccessRequest{ResourceFile, ActionRead, "/tmp/x"}, true},
		{"file stat", AccessRequest{ResourceFile, ActionStat, "/tmp/x"}, true},
		// ReadOnly applies no path confinement: it reads any path the host can.
		{"file read any path (no confinement)", AccessRequest{ResourceFile, ActionRead, "/etc/passwd"}, true},
		// R11: ReadOnly must NOT permit loading (and running) code.
		{"code load denied", AccessRequest{ResourceCode, ActionLoad, "lib.sld"}, false},
		{"file write", AccessRequest{ResourceFile, ActionWrite, "/tmp/x"}, false},
		{"file delete", AccessRequest{ResourceFile, ActionDelete, "/tmp/x"}, false},
		{"process exit", AccessRequest{ResourceProcess, ActionExit, ""}, false},
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
		{"file read", AccessRequest{ResourceFile, ActionRead, "/tmp/x"}, true},
		{"file stat", AccessRequest{ResourceFile, ActionStat, "/tmp/x"}, true},
		{"code load allowed", AccessRequest{ResourceCode, ActionLoad, "lib.sld"}, true},
		{"file write", AccessRequest{ResourceFile, ActionWrite, "/tmp/x"}, false},
		{"file delete", AccessRequest{ResourceFile, ActionDelete, "/tmp/x"}, false},
		{"process exit", AccessRequest{ResourceProcess, ActionExit, ""}, false},
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
		{"file inside root", AccessRequest{ResourceFile, ActionRead, "/app/data/file.txt"}, true},
		{"file at root", AccessRequest{ResourceFile, ActionRead, "/app/data"}, true},
		{"file in subdir", AccessRequest{ResourceFile, ActionRead, "/app/data/sub/deep.txt"}, true},
		{"code inside root", AccessRequest{ResourceCode, ActionLoad, "/app/data/lib.sld"}, true},
		{"file outside root", AccessRequest{ResourceFile, ActionRead, "/etc/passwd"}, false},
		{"file parent traversal", AccessRequest{ResourceFile, ActionRead, "/app/data/../secret"}, false},
		{"file sibling", AccessRequest{ResourceFile, ActionRead, "/app/other/file"}, false},
		{"env read (non-file)", AccessRequest{ResourceEnv, ActionRead, "PATH"}, true},
		{"process exit (non-file)", AccessRequest{ResourceProcess, ActionExit, ""}, true},
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

func TestAll_Empty(t *testing.T) {
	c := qt.New(t)
	auth := All()
	err := auth.Authorize(AccessRequest{ResourceFile, ActionWrite, "/tmp/x"})
	c.Assert(err, qt.IsNil)
}

func TestAll_SinglePassthrough(t *testing.T) {
	c := qt.New(t)
	inner := ReadOnly()
	auth := All(inner)
	// All with single arg returns that arg directly
	err := auth.Authorize(AccessRequest{ResourceFile, ActionRead, "/tmp/x"})
	c.Assert(err, qt.IsNil)
	err = auth.Authorize(AccessRequest{ResourceFile, ActionWrite, "/tmp/x"})
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
		{"read inside", AccessRequest{ResourceFile, ActionRead, "/app/data/f.txt"}, true},
		{"read outside", AccessRequest{ResourceFile, ActionRead, "/etc/passwd"}, false},
		{"write inside", AccessRequest{ResourceFile, ActionWrite, "/app/data/f.txt"}, false},
		{"write outside", AccessRequest{ResourceFile, ActionWrite, "/etc/passwd"}, false},
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
	err := auth.Authorize(AccessRequest{ResourceFile, ActionRead, "/tmp/x"})
	c.Assert(errors.Is(err, ErrAccessDenied), qt.IsTrue)
	c.Assert(calls, qt.Equals, 0) // counter never reached
}
