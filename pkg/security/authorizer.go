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

import "github.com/aalpar/wile/pkg/werr"

// ErrAccessDenied is the sentinel error returned when an Authorizer
// denies an operation. Use errors.Is to check for it. See the Authorizer
// doc for the deny-error wrapping convention.
var ErrAccessDenied = werr.NewStaticError("access denied")

// Authorizer decides whether an operation is allowed. Implementations
// must be safe for concurrent use.
//
// Authorize returns nil to allow the operation, or an error wrapping
// ErrAccessDenied to deny it. Returning a non-nil error that does not
// wrap ErrAccessDenied is treated as a deny with an unexpected cause.
//
// Deny-error wrapping convention: an Authorizer should return the bare
// ErrAccessDenied sentinel. CheckWithAuthorizer wraps every denial with the
// operation's action, resource, and target, so that context is always present
// without each authorizer repeating it. An Authorizer should add an inner
// reason (still wrapping ErrAccessDenied) only when the reason is not derivable
// from action+resource+target — for example FilesystemRoot reports the
// confining root ("path %q outside root %q"), which the operation fields alone
// cannot convey. errors.Is(err, ErrAccessDenied) matches either form.
type Authorizer interface {
	Authorize(req AccessRequest) error
}

// AuthorizerFunc adapts a plain function to the Authorizer interface.
type AuthorizerFunc func(AccessRequest) error

// Authorize implements Authorizer.
func (p AuthorizerFunc) Authorize(req AccessRequest) error {
	return p(req)
}
