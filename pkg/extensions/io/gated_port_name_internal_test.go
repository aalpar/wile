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
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// A host stream's port is labelled "<stream>" on a granted port and on a
// refused one. The label is a display name, never a path: it has no separator,
// so include resolution cannot mistake it for a file location, and the angle
// brackets keep it apart from a file called stdin.
func TestGatedStreamPortsCarryTheStreamLabel(t *testing.T) {
	deny := security.AuthorizerFunc(func(security.AccessRequest) error {
		return werr.WrapForeignErrorf(security.ErrAccessDenied, "test: deny every stream")
	})
	auths := []struct {
		name string
		auth security.Authorizer
	}{
		{"granted", nil},
		{"refused", deny},
	}
	ports := []struct {
		name string
		make func(security.Authorizer) *values.PortObject
		want string
	}{
		{
			name: "stdin",
			make: func(a security.Authorizer) *values.PortObject {
				return gatedInputPort(a, security.StreamStdin, os.Stdin)
			},
			want: "<stdin>",
		},
		{
			name: "stdout",
			make: func(a security.Authorizer) *values.PortObject {
				return gatedOutputPort(a, security.StreamStdout, os.Stdout)
			},
			want: "<stdout>",
		},
		{
			name: "stderr",
			make: func(a security.Authorizer) *values.PortObject {
				return gatedOutputPort(a, security.StreamStderr, os.Stderr)
			},
			want: "<stderr>",
		},
	}
	for _, pt := range ports {
		for _, at := range auths {
			t.Run(at.name+" "+pt.name, func(t *testing.T) {
				qt.New(t).Assert(pt.make(at.auth).PortName(), qt.Equals, pt.want)
			})
		}
	}
}
