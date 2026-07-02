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

// ConsoleWithLoadAuthorizer returns an Authorizer for the ConsoleWithLoad
// profile. File operations and code loading are both restricted to /tmp.
// Environment variable reads are allowed. Process execution is denied.
//
// This is the security envelope wile-goast and similar embedders use to
// run sandboxed (eval ...) and (load ...) on Scheme files staged in /tmp.
//
// Containment is symlink-resolved (see containedInRoot), so a symlink staged
// inside /tmp that points outside /tmp does not escape the sandbox. Dynamic
// code evaluation (code:eval, from (eval <datum>)/(compile <datum>)) has no
// path to restrict and is allowed here so the profile's documented sandboxed
// (eval ...) keeps working; the side effects of evaluated code remain gated
// at their own file/process/env sinks.
func ConsoleWithLoadAuthorizer() Authorizer {
	return consoleWithLoadAuthorizer{}
}

type consoleWithLoadAuthorizer struct{}

func (consoleWithLoadAuthorizer) Authorize(req AccessRequest) error {
	switch req.Resource {
	case ResourceCode:
		if req.Action == ActionEval {
			return nil
		}
		if !containedInRoot("/tmp", req.Target) {
			return ErrAccessDenied
		}
		return nil
	case ResourceFile:
		if !containedInRoot("/tmp", req.Target) {
			return ErrAccessDenied
		}
		return nil
	case ResourceEnv:
		// Read-only: env reads are allowed (see doc), writes/deletes are not.
		// Matches the sibling SandboxAuthorizer's env gate.
		if req.Action == ActionRead {
			return nil
		}
		return ErrAccessDenied
	default:
		return ErrAccessDenied
	}
}

func (consoleWithLoadAuthorizer) ConfinementRoot() (string, bool) {
	return "/tmp", true
}
