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
func FilesystemRoot(root string) Authorizer {
	return &filesystemRootAuthorizer{root: root}
}

type filesystemRootAuthorizer struct {
	root string
}

func (p *filesystemRootAuthorizer) Authorize(req AccessRequest) error {
	switch req.Resource {
	case ResourceFile, ResourceCode:
		// gate these
	default:
		return nil
	}
	if !containedInRoot(p.root, req.Target) {
		return werr.WrapForeignErrorf(ErrAccessDenied, "path %q outside root %q", req.Target, p.root)
	}
	return nil
}

func (p *filesystemRootAuthorizer) ConfinementRoot() (string, bool) {
	return p.root, true
}
