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

import "github.com/aalpar/wile/werr"

// ErrAccessDenied is the sentinel error returned when an Authorizer
// denies an operation. Use errors.Is to check for it; the error may
// be wrapped with additional context by callers.
var ErrAccessDenied = werr.NewStaticError("access denied")

// Authorizer decides whether an operation is allowed. Implementations
// must be safe for concurrent use.
//
// Authorize returns nil to allow the operation, or an error wrapping
// ErrAccessDenied to deny it. Returning a non-nil error that does not
// wrap ErrAccessDenied is treated as a deny with an unexpected cause.
type Authorizer interface {
	Authorize(req AccessRequest) error
}

// AuthorizerFunc adapts a plain function to the Authorizer interface.
type AuthorizerFunc func(AccessRequest) error

// Authorize implements Authorizer.
func (p AuthorizerFunc) Authorize(req AccessRequest) error {
	return p(req)
}
