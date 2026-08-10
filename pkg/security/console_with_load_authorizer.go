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
//
// A target drawn from a virtual filesystem (AccessRequest.TargetSource set) is
// denied outright: its path names a file inside an embedder's fs.FS and has no
// relation to /tmp. Use ConsoleWithLoadAllowingVirtualSources to serve one —
// which is what an embedder that stages its Scheme sources in an fs.FS rather
// than under /tmp now needs.
func ConsoleWithLoadAuthorizer() Authorizer {
	return consoleWithLoadAuthorizer{}
}

// ConsoleWithLoadAllowingVirtualSources returns a ConsoleWithLoad authorizer
// that additionally serves targets drawn from a virtual filesystem. It applies
// NO path confinement to those targets: the embedder is asserting that the
// fs.FS it supplied is itself the boundary. Host-filesystem targets stay
// confined to /tmp exactly as under ConsoleWithLoadAuthorizer.
func ConsoleWithLoadAllowingVirtualSources() Authorizer {
	return consoleWithLoadAuthorizer{allowVirtualSources: true}
}

// consoleWithLoadAuthorizer is Console plus a sandboxed code arm. It embeds
// consoleAuthorizer so the file, env, and default policy — and ConfinementRoot —
// are the SAME code, not a hand-copied twin. This is what closed the #10 drift
// (the env-write gate had to be fixed in both copies); composition makes that
// class of divergence unrepresentable. Only ResourceCode is genuinely ours.
type consoleWithLoadAuthorizer struct {
	consoleAuthorizer
	allowVirtualSources bool
}

func (a consoleWithLoadAuthorizer) Authorize(req AccessRequest) error {
	if req.Resource != ResourceCode {
		return a.consoleAuthorizer.Authorize(req)
	}
	// code:eval (dynamic (eval <datum>)/(compile <datum>)) has no path to
	// confine; code:load is confined to the shared root like file access. A
	// virtual-filesystem target has no path to confine either — containedInRoot
	// would resolve it against the process CWD, making the verdict a coincidence
	// of where the host runs — so it is decided on the discriminator in BOTH
	// directions and never reaches containedInRoot.
	if req.Action == ActionEval {
		return nil
	}
	if req.TargetSource != "" {
		if a.allowVirtualSources {
			return nil
		}
		return ErrAccessDenied
	}
	if !containedInRoot(consoleRoot, req.Target) {
		return ErrAccessDenied
	}
	return nil
}
