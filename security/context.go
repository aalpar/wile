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

	"github.com/aalpar/wile/werr"
)

type contextKey struct{}

// WithAuthorizer returns a child context carrying the given Authorizer.
// Primitives retrieve it via FromContext or Check.
func WithAuthorizer(ctx context.Context, auth Authorizer) context.Context {
	return context.WithValue(ctx, contextKey{}, auth)
}

// FromContext returns the Authorizer stored in ctx, or nil if none.
func FromContext(ctx context.Context) Authorizer {
	auth, _ := ctx.Value(contextKey{}).(Authorizer)
	return auth
}

// Check authorizes req against the Authorizer in ctx. If no Authorizer
// is set, the operation is allowed (open by default). Returns nil on
// success or a wrapped ErrAccessDenied on denial.
func Check(ctx context.Context, req AccessRequest) error {
	auth := FromContext(ctx)
	if auth == nil {
		return nil
	}
	err := auth.Authorize(req)
	if err == nil {
		return nil
	}
	return werr.WrapForeignErrorf(err, "%s %s %q", req.Action, req.Resource, req.Target)
}
