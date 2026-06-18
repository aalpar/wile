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
	"os"
	"path/filepath"
	"testing"
)

// A symlink staged inside the root that points outside it must NOT be
// considered contained — this is the sandbox-escape the lexical check missed.
func TestContainedInRoot_SymlinkEscapeRejected(t *testing.T) {
	dir, err := os.MkdirTemp("/tmp", "wile_pc_")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)

	// /tmp/wile_pc_*/escape -> /etc  (a directory outside /tmp)
	link := filepath.Join(dir, "escape")
	if err := os.Symlink("/etc", link); err != nil {
		t.Fatal(err)
	}

	escaped := filepath.Join(link, "passwd") // /tmp/.../escape/passwd -> /etc/passwd
	if containedInRoot("/tmp", escaped) {
		t.Errorf("symlink escape %q must NOT be contained in /tmp", escaped)
	}

	// A real path under /tmp (the temp dir itself) IS contained.
	if !containedInRoot("/tmp", dir) {
		t.Errorf("%q should be contained in /tmp", dir)
	}
	// A not-yet-existing file under /tmp IS contained (creation must be allowed).
	newFile := filepath.Join(dir, "newfile.txt")
	if !containedInRoot("/tmp", newFile) {
		t.Errorf("to-be-created %q should be contained in /tmp", newFile)
	}
	// Creating a new file THROUGH the escaping symlink is NOT contained.
	escapedNew := filepath.Join(link, "newfile.txt")
	if containedInRoot("/tmp", escapedNew) {
		t.Errorf("creation through escaping symlink %q must NOT be contained", escapedNew)
	}
}

// The Console and ConsoleWithLoad authorizers must deny the symlink escape.
func TestConsoleAuthorizers_SymlinkEscapeDenied(t *testing.T) {
	dir, err := os.MkdirTemp("/tmp", "wile_pc_")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)
	link := filepath.Join(dir, "escape")
	if err := os.Symlink("/etc", link); err != nil {
		t.Fatal(err)
	}
	escaped := filepath.Join(link, "passwd")

	for _, auth := range []struct {
		name string
		a    Authorizer
	}{
		{"console", ConsoleAuthorizer()},
		{"console-with-load", ConsoleWithLoadAuthorizer()},
		{"filesystem-root", FilesystemRoot("/tmp")},
	} {
		req := AccessRequest{Resource: ResourceFile, Action: ActionRead, Target: escaped}
		if err := auth.a.Authorize(req); err == nil {
			t.Errorf("%s: expected escape via symlink to be denied, got allow", auth.name)
		}
		// A genuine /tmp path stays allowed.
		ok := AccessRequest{Resource: ResourceFile, Action: ActionRead, Target: filepath.Join(dir, "x")}
		if err := auth.a.Authorize(ok); err != nil {
			t.Errorf("%s: genuine /tmp path should be allowed, got: %v", auth.name, err)
		}
	}
}

// code:eval is allowed under ConsoleWithLoad (preserves sandboxed eval) but
// denied under Console (which denies all code).
func TestEvalAction_Posture(t *testing.T) {
	evalReq := AccessRequest{Resource: ResourceCode, Action: ActionEval, Target: "<eval>"}

	if err := ConsoleWithLoadAuthorizer().Authorize(evalReq); err != nil {
		t.Errorf("ConsoleWithLoad should allow code:eval, got: %v", err)
	}
	if err := ConsoleAuthorizer().Authorize(evalReq); !errors.Is(err, ErrAccessDenied) {
		t.Errorf("Console should deny code:eval, got: %v", err)
	}
	// code:load to a non-/tmp path stays denied under ConsoleWithLoad.
	loadReq := AccessRequest{Resource: ResourceCode, Action: ActionLoad, Target: "/etc/evil.scm"}
	if err := ConsoleWithLoadAuthorizer().Authorize(loadReq); !errors.Is(err, ErrAccessDenied) {
		t.Errorf("ConsoleWithLoad should deny code:load outside /tmp, got: %v", err)
	}
}
