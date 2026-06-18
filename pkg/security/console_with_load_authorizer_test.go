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
		// Env: same as Console
		{"read env", security.AccessRequest{Resource: security.ResourceEnv, Action: security.ActionRead, Target: "APP_MODE"}, true},
		// Code load: NEW capability vs Console -- allowed under /tmp
		{"load /tmp/lib.scm", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "/tmp/lib.scm"}, true},
		{"load /tmp/sub/lib.scm", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "/tmp/sub/lib.scm"}, true},
		{"load /etc/lib.scm", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "/etc/lib.scm"}, false},
		{"load path traversal /tmp/../etc/lib.scm", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "/tmp/../etc/lib.scm"}, false},
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
