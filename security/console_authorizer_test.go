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

	"github.com/aalpar/wile/security"
	qt "github.com/frankban/quicktest"
)

func TestConsoleAuthorizer(t *testing.T) {
	c := qt.New(t)
	auth := security.ConsoleAuthorizer()

	tests := []struct {
		name    string
		req     security.AccessRequest
		allowed bool
	}{
		{"read /tmp/foo", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/tmp/foo"}, true},
		{"write /tmp/bar", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionWrite, Target: "/tmp/bar"}, true},
		{"delete /tmp/baz", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionDelete, Target: "/tmp/baz"}, true},
		{"read /tmp subdir", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/tmp/sub/dir/file"}, true},
		{"read /etc/passwd", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/etc/passwd"}, false},
		{"write /home/user", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionWrite, Target: "/home/user/file"}, false},
		{"read env", security.AccessRequest{Resource: security.ResourceEnv, Action: security.ActionRead, Target: "APP_MODE"}, true},
		{"load code", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "file.scm"}, false},
		{"exec process", security.AccessRequest{Resource: security.ResourceProcess, Action: security.ActionExec, Target: "ls"}, false},
		{"path traversal /tmp/../etc", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/tmp/../etc/passwd"}, false},
		// Prefix-trap cases: the /tmp/ check must not admit sibling directories.
		{"prefix trap /tmp2", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/tmp2/foo"}, false},
		{"prefix trap /tmpfoo", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/tmpfoo"}, false},
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
