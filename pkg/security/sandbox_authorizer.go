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

import "strings"

// SandboxAuthorizer returns an Authorizer that allows read-only file
// access, env reads with a prefix filter, and denies code loading
// and process execution.
//
// The host's standard streams are allowed: this is the env-map modifier
// WithSandbox() installs on top of a profile, and taking away the profile's
// stdio is not what it is for. Compose with DenyAll (or a custom authorizer)
// via All() to refuse the streams.
//
// Intended as a restrictive modifier that can be composed with a
// profile's built-in authorizer via All() to produce an intersection
// (most-restrictive-wins).
func SandboxAuthorizer(envPrefix string) Authorizer {
	return AuthorizerFunc(func(req AccessRequest) error {
		switch req.Resource {
		case ResourceFile:
			if req.Action == ActionRead || req.Action == ActionStat {
				return nil
			}
			return ErrAccessDenied
		case ResourceEnv:
			if req.Action == ActionRead && strings.HasPrefix(req.Target, envPrefix) {
				return nil
			}
			return ErrAccessDenied
		case ResourceStream:
			return nil
		default:
			return ErrAccessDenied
		}
	})
}
