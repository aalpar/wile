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

package io

import (
	"os"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/werr"
)

// The host streams are named by the stream they are, on a granted port and on
// a refused one, so a stream denial says which stream.
func TestGatedStreamPortsCarryTheStreamName(t *testing.T) {
	deny := security.AuthorizerFunc(func(security.AccessRequest) error {
		return werr.WrapForeignErrorf(security.ErrAccessDenied, "test: deny every stream")
	})
	tests := []struct {
		name string
		auth security.Authorizer
		port func(security.Authorizer) interface{ SourceName() string }
		want string
	}{
		{
			name: "granted stdin",
			port: func(a security.Authorizer) interface{ SourceName() string } {
				return gatedInputPort(a, security.StreamStdin, os.Stdin)
			},
			want: "stdin",
		},
		{
			name: "refused stdin",
			auth: deny,
			port: func(a security.Authorizer) interface{ SourceName() string } {
				return gatedInputPort(a, security.StreamStdin, os.Stdin)
			},
			want: "stdin",
		},
		{
			name: "granted stdout",
			port: func(a security.Authorizer) interface{ SourceName() string } {
				return gatedOutputPort(a, security.StreamStdout, os.Stdout)
			},
			want: "stdout",
		},
		{
			name: "refused stderr",
			auth: deny,
			port: func(a security.Authorizer) interface{ SourceName() string } {
				return gatedOutputPort(a, security.StreamStderr, os.Stderr)
			},
			want: "stderr",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			qt.New(t).Assert(tc.port(tc.auth).SourceName(), qt.Equals, tc.want)
		})
	}
}
