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
	"github.com/aalpar/wile/pkg/werr"
)

// FilesystemRoot returns an Authorizer that restricts file and code
// operations to paths under root. Non-file/code resources are allowed.
//
// Containment is symlink-resolved (see containedInRoot): both root and target
// are canonicalised, so a symlink under root that points outside it is
// followed and rejected, while the root itself may legitimately be a symlink.
// Paths that do not exist yet (e.g. a file about to be created) are still
// admitted as long as their existing ancestry stays within root.
//
// code:eval (dynamic (eval <datum>)/(compile <datum>)) is denied outright: its
// Target is a label, not a path, so there is nothing to confine. Use
// ConsoleWithLoadAuthorizer if sandboxed eval is required.
//
// A target drawn from a virtual filesystem (AccessRequest.TargetSource set) is
// likewise denied outright. Use FilesystemRootWithVirtualSources to serve one.
func FilesystemRoot(root string) Authorizer {
	return &filesystemRootAuthorizer{root: root}
}

// FilesystemRootWithVirtualSources returns a FilesystemRoot authorizer that
// additionally serves targets drawn from a virtual filesystem (an embedder's
// WithSourceFS). It applies NO path confinement to those targets: a virtual
// path has no relation to the host filesystem, so root cannot bound it. The
// embedder is asserting that the fs.FS it supplied is itself the boundary.
//
// Host-filesystem targets are confined to root exactly as under FilesystemRoot.
func FilesystemRootWithVirtualSources(root string) Authorizer {
	return &filesystemRootAuthorizer{root: root, allowVirtualSources: true}
}

type filesystemRootAuthorizer struct {
	root                string
	allowVirtualSources bool
}

func (p *filesystemRootAuthorizer) Authorize(req AccessRequest) error {
	switch req.Resource {
	case ResourceFile, ResourceCode:
		// gate these
	default:
		return nil
	}

	// code:eval is decided on the ACTION, never by path containment. Its Target is
	// the literal "<eval>" / "<compile>" — a label, not a path — and feeding a label
	// to containedInRoot resolves it RELATIVE TO THE PROCESS CWD. That made the
	// answer depend on where the host happened to be running: eval was denied only
	// because the CWD is usually outside the root, and a host whose CWD sat INSIDE
	// its own confinement root silently got eval for free. A sandbox decision must
	// not be a coincidence of the working directory.
	//
	// Deny is the explicit stance, matching the de-facto behaviour for every host
	// whose CWD is outside the root, and failing safe for the ones where it is not.
	// (consoleWithLoadAuthorizer answers the same question the same way — on the
	// action — and ALLOWS, because permitting eval under /tmp is that profile's
	// whole point. What neither may do is ask containedInRoot about a non-path.)
	//
	// A virtual-filesystem target is the same mistake in the other axis: "evil.scm"
	// inside an embedder's WithSourceFS is a complete path in ITS namespace, and
	// containedInRoot resolves it against the process CWD, so an untrusted fs.FS
	// was admitted whenever the host happened to run inside its own root. So this
	// too is decided on the DISCRIMINATOR and never reaches containedInRoot —
	// including in the allow direction, or the opt-in variant would inherit the
	// same coincidence it exists to escape.
	if req.Action == ActionEval {
		return werr.WrapForeignErrorf(ErrAccessDenied,
			"code:eval is not permitted under a filesystem-root authorizer")
	}

	if req.TargetSource != "" {
		if p.allowVirtualSources {
			return nil
		}
		return werr.WrapForeignErrorf(ErrAccessDenied,
			"target %q comes from virtual source %q, not the host filesystem",
			req.Target, req.TargetSource)
	}

	if !containedInRoot(p.root, req.Target) {
		return werr.WrapForeignErrorf(ErrAccessDenied, "path %q outside root %q", req.Target, p.root)
	}
	return nil
}

func (p *filesystemRootAuthorizer) ConfinementRoot() (string, bool) {
	return p.root, true
}
