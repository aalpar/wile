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
	"context"
	"errors"
	"fmt"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestCheck_NoAuthorizer(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	err := Check(ctx, AccessRequest{
		Resource: ResourceFile,
		Action:   ActionRead,
		Target:   "/etc/passwd",
	})
	c.Assert(err, qt.IsNil)
}

func TestCheck_AllowAll(t *testing.T) {
	c := qt.New(t)
	ctx := WithAuthorizer(context.Background(), AuthorizerFunc(func(_ AccessRequest) error {
		return nil
	}))

	err := Check(ctx, AccessRequest{
		Resource: ResourceFile,
		Action:   ActionRead,
		Target:   "/tmp/safe",
	})
	c.Assert(err, qt.IsNil)
}

func TestCheck_DenyAll(t *testing.T) {
	c := qt.New(t)
	ctx := WithAuthorizer(context.Background(), AuthorizerFunc(func(_ AccessRequest) error {
		return ErrAccessDenied
	}))

	err := Check(ctx, AccessRequest{
		Resource: ResourceFile,
		Action:   ActionWrite,
		Target:   "/etc/shadow",
	})
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, ErrAccessDenied), qt.IsTrue)
}

func TestCheck_DenyWrapsContext(t *testing.T) {
	c := qt.New(t)
	ctx := WithAuthorizer(context.Background(), AuthorizerFunc(func(_ AccessRequest) error {
		return fmt.Errorf("outside allowed root: %w", ErrAccessDenied)
	}))

	err := Check(ctx, AccessRequest{
		Resource: ResourceFile,
		Action:   ActionRead,
		Target:   "/etc/passwd",
	})
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, ErrAccessDenied), qt.IsTrue)
	c.Assert(err.Error(), qt.Contains, `read file "/etc/passwd"`)
}

func TestCheck_SelectiveDeny(t *testing.T) {
	c := qt.New(t)
	ctx := WithAuthorizer(context.Background(), AuthorizerFunc(func(req AccessRequest) error {
		if req.Action == ActionWrite || req.Action == ActionDelete {
			return ErrAccessDenied
		}
		return nil
	}))

	tcs := []struct {
		name   string
		action string
		deny   bool
	}{
		{"read allowed", ActionRead, false},
		{"stat allowed", ActionStat, false},
		{"write denied", ActionWrite, true},
		{"delete denied", ActionDelete, true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := Check(ctx, AccessRequest{
				Resource: ResourceFile,
				Action:   tc.action,
				Target:   "/tmp/file",
			})
			if tc.deny {
				c.Assert(errors.Is(err, ErrAccessDenied), qt.IsTrue)
			} else {
				c.Assert(err, qt.IsNil)
			}
		})
	}
}

func TestFromContext_NoAuthorizer(t *testing.T) {
	c := qt.New(t)
	auth := FromContext(context.Background())
	c.Assert(auth, qt.IsNil)
}

func TestFromContext_RoundTrip(t *testing.T) {
	c := qt.New(t)
	orig := AuthorizerFunc(func(_ AccessRequest) error {
		return nil
	})
	ctx := WithAuthorizer(context.Background(), orig)
	got := FromContext(ctx)
	c.Assert(got, qt.IsNotNil)
}

func TestAuthorizerFunc_Implements(t *testing.T) {
	c := qt.New(t)
	var auth Authorizer = AuthorizerFunc(func(_ AccessRequest) error {
		return nil
	})
	err := auth.Authorize(AccessRequest{})
	c.Assert(err, qt.IsNil)
}

func TestAccessRequest_Constants(t *testing.T) {
	c := qt.New(t)

	c.Assert(ResourceFile, qt.Equals, "file")
	c.Assert(ResourceCode, qt.Equals, "code")
	c.Assert(ResourceEnv, qt.Equals, "env")
	c.Assert(ResourceProcess, qt.Equals, "process")
	c.Assert(ActionRead, qt.Equals, "read")
	c.Assert(ActionWrite, qt.Equals, "write")
	c.Assert(ActionDelete, qt.Equals, "delete")
	c.Assert(ActionStat, qt.Equals, "stat")
	c.Assert(ActionLoad, qt.Equals, "load")
	c.Assert(ActionExit, qt.Equals, "exit")
}
