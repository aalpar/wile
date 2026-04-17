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

	"github.com/aalpar/wile/security"
)

func TestSandboxAuthorizer_DefaultPrefix(t *testing.T) {
	c := qt.New(t)
	auth := security.SandboxAuthorizer("WILE_")

	tests := []struct {
		name    string
		req     security.AccessRequest
		allowed bool
	}{
		{"read file", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/any/file"}, true},
		{"write file", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionWrite, Target: "/any/file"}, false},
		{"delete file", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionDelete, Target: "/any/file"}, false},
		{"stat file", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionStat, Target: "/any/file"}, true},
		{"read env WILE_MODE", security.AccessRequest{Resource: security.ResourceEnv, Action: security.ActionRead, Target: "WILE_MODE"}, true},
		{"read env HOME", security.AccessRequest{Resource: security.ResourceEnv, Action: security.ActionRead, Target: "HOME"}, false},
		{"load code", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "file.scm"}, false},
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
