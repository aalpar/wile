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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/security"
)

func TestConsoleWithLoadAuthorizer(t *testing.T) {
	c := qt.New(t)
	auth := security.ConsoleWithLoadAuthorizer()

	tests := []struct {
		name    string
		req     security.AccessRequest
		allowed bool
	}{
		// File ops: same /tmp envelope as Console
		{"read /tmp/foo", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/tmp/foo"}, true},
		{"write /tmp/bar", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionWrite, Target: "/tmp/bar"}, true},
		{"delete /tmp/baz", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionDelete, Target: "/tmp/baz"}, true},
		{"read /etc/passwd", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/etc/passwd"}, false},
		// Env: same as Console -- read-only
		{"read env", security.AccessRequest{Resource: security.ResourceEnv, Action: security.ActionRead, Target: "APP_MODE"}, true},
		{"write env", security.AccessRequest{Resource: security.ResourceEnv, Action: security.ActionWrite, Target: "APP_MODE"}, false},
		{"delete env", security.AccessRequest{Resource: security.ResourceEnv, Action: security.ActionDelete, Target: "APP_MODE"}, false},
		// Code load: NEW capability vs Console -- allowed under /tmp
		{"load /tmp/lib.scm", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "/tmp/lib.scm"}, true},
		{"load /tmp/sub/lib.scm", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "/tmp/sub/lib.scm"}, true},
		{"load /etc/lib.scm", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "/etc/lib.scm"}, false},
		{"load path traversal /tmp/../etc/lib.scm", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "/tmp/../etc/lib.scm"}, false},
		// Code eval: allowed unconditionally (no path to confine); the doc'd
		// sandboxed (eval ...) capability. Side effects gate at their own sinks.
		{"eval datum (no target)", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionEval}, true},
		// Prefix-trap cases: applies to both file and code — sibling dirs must not match.
		{"prefix trap /tmp2 file", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/tmp2/foo"}, false},
		{"prefix trap /tmpfoo code", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "/tmpfoo/lib.scm"}, false},
		// Process: still denied
		{"exec process", security.AccessRequest{Resource: security.ResourceProcess, Action: security.ActionExec, Target: "ls"}, false},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			err := auth.Authorize(tt.req)
			if tt.allowed {
				c.Assert(err, qt.IsNil)
			} else {
				c.Assert(err, qt.IsNotNil)
			}
		})
	}
}

// TestConsoleWithLoadDeniesVirtualSourceRegardlessOfCWD is the ConsoleWithLoad
// twin of TestFilesystemRootDeniesVirtualSourceRegardlessOfCWD: this profile
// confines code:load to /tmp, so a virtual path was admitted whenever the
// process CWD sat under /tmp — which, for a program staging its sources there,
// is the normal case rather than the exotic one.
func TestConsoleWithLoadDeniesVirtualSourceRegardlessOfCWD(t *testing.T) {
	req := security.AccessRequest{
		Resource:     security.ResourceCode,
		Action:       security.ActionLoad,
		Target:       "evil.scm",
		TargetSource: security.SourceVirtualFS,
	}

	assertVerdicts := func(t *testing.T) {
		t.Helper()
		err := security.CheckWithAuthorizer(security.ConsoleWithLoadAuthorizer(), req)
		qt.Assert(t, err, qt.IsNotNil,
			qt.Commentf("a virtual target has no relation to /tmp"))
		qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)

		err = security.CheckWithAuthorizer(security.ConsoleWithLoadAllowingVirtualSources(), req)
		qt.Assert(t, err, qt.IsNil,
			qt.Commentf("the opt-in variant must serve a virtual target"))
	}

	t.Run("cwd outside /tmp", func(t *testing.T) {
		assertVerdicts(t)
	})

	t.Run("cwd under /tmp", func(t *testing.T) {
		dir, err := os.MkdirTemp("/tmp", "wile-cwl-")
		qt.Assert(t, err, qt.IsNil)
		t.Cleanup(func() {
			_ = os.RemoveAll(dir)
		})
		prev, err := os.Getwd()
		qt.Assert(t, err, qt.IsNil)
		t.Cleanup(func() {
			_ = os.Chdir(prev)
		})
		qt.Assert(t, os.Chdir(dir), qt.IsNil)

		assertVerdicts(t)
	})

	// The opt-in stays a virtual-source exemption, not a blanket allow.
	err := security.CheckWithAuthorizer(security.ConsoleWithLoadAllowingVirtualSources(),
		security.AccessRequest{
			Resource: security.ResourceCode,
			Action:   security.ActionLoad,
			Target:   "/etc/lib.scm",
		})
	qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue,
		qt.Commentf("an OS path must still be confined to /tmp"))
}

// TestConsoleWithLoad_DelegatesNonCodeToConsole pins the composition invariant:
// for every non-code resource, ConsoleWithLoad returns the SAME verdict as
// Console, because it delegates those arms rather than re-implementing them.
// This is the guard that makes the #10-class drift (env-write gate fixed in one
// copy but not the other) unrepresentable — a change to Console now propagates.
func TestConsoleWithLoad_DelegatesNonCodeToConsole(t *testing.T) {
	c := qt.New(t)
	console := security.ConsoleAuthorizer()
	withLoad := security.ConsoleWithLoadAuthorizer()

	resources := []string{security.ResourceFile, security.ResourceEnv, security.ResourceProcess}
	actions := []string{
		security.ActionRead, security.ActionWrite, security.ActionDelete,
		security.ActionExec, security.ActionLoad,
	}
	targets := []string{"/tmp/x", "/etc/x", "APP_MODE", ""}

	for _, res := range resources {
		for _, act := range actions {
			for _, tgt := range targets {
				req := security.AccessRequest{Resource: res, Action: act, Target: tgt}
				base := console.Authorize(req)
				derived := withLoad.Authorize(req)
				// Compare allow/deny outcome, not error identity (both return the
				// shared ErrAccessDenied on deny).
				c.Assert(derived == nil, qt.Equals, base == nil,
					qt.Commentf("resource=%v action=%v target=%q", res, act, tgt))
			}
		}
	}

	// And the confinement root is the promoted (identical) one.
	baseRoot, baseOK := security.ConfinementRootOf(console)
	derRoot, derOK := security.ConfinementRootOf(withLoad)
	c.Assert(derRoot, qt.Equals, baseRoot)
	c.Assert(derOK, qt.Equals, baseOK)
}
