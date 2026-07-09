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

// consoleRoot is the single filesystem confinement root shared by the Console
// and ConsoleWithLoad authorizers — the one source of truth for the path these
// profiles pin to, so the file arm and ConfinementRoot cannot disagree. The
// configurable-root variant is filesystemRootAuthorizer; the Console profiles
// are deliberately the /tmp-fixed convenience.
const consoleRoot = "/tmp"

// ConsoleAuthorizer returns an Authorizer for the Console profile.
// File operations are restricted to /tmp. Environment variable reads
// are allowed (the envvars primitive handles virtual-vs-OS routing).
// Code loading and process execution are denied.
//
// Containment is symlink-resolved (see containedInRoot), so a symlink staged
// inside /tmp that points outside /tmp does not escape the sandbox. The
// authorizer also reports /tmp as its ConfinementRoot, so file primitives
// open through os.Root for race-free containment.
func ConsoleAuthorizer() Authorizer {
	return consoleAuthorizer{}
}

type consoleAuthorizer struct{}

func (consoleAuthorizer) Authorize(req AccessRequest) error {
	switch req.Resource {
	case ResourceFile:
		if !containedInRoot(consoleRoot, req.Target) {
			return ErrAccessDenied
		}
		return nil
	case ResourceEnv:
		// Read-only: env reads are allowed (see doc), writes/deletes are not.
		// Same read-only principle as SandboxAuthorizer's env gate, which
		// additionally prefix-filters the target.
		if req.Action == ActionRead {
			return nil
		}
		return ErrAccessDenied
	default:
		return ErrAccessDenied
	}
}

func (consoleAuthorizer) ConfinementRoot() (string, bool) {
	return consoleRoot, true
}
