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
	"fmt"
	"path/filepath"
	"strings"
)

// FilesystemRoot returns an Authorizer that restricts file and code
// operations to paths under root. Non-file/code resources are allowed.
//
// Paths are cleaned and resolved to absolute form before comparison.
// Symlink traversal is NOT followed — this is a lexical check only.
// For production use with symlinks, resolve the root and targets with
// filepath.EvalSymlinks before constructing the authorizer.
func FilesystemRoot(root string) Authorizer {
	absRoot, err := filepath.Abs(root)
	if err != nil {
		absRoot = filepath.Clean(root)
	}
	if !strings.HasSuffix(absRoot, string(filepath.Separator)) {
		absRoot += string(filepath.Separator)
	}
	return &filesystemRootAuthorizer{root: absRoot}
}

type filesystemRootAuthorizer struct {
	root string // always absolute, always ends with separator
}

func (p *filesystemRootAuthorizer) Authorize(req AccessRequest) error {
	switch req.Resource {
	case ResourceFile, ResourceCode:
		// gate these
	default:
		return nil
	}
	absTarget, err := filepath.Abs(req.Target)
	if err != nil {
		return fmt.Errorf("resolve %q: %w", req.Target, ErrAccessDenied)
	}
	// Target is allowed if it equals the root dir itself or is under it.
	targetWithSep := absTarget + string(filepath.Separator)
	if !strings.HasPrefix(targetWithSep, p.root) {
		return fmt.Errorf("path %q outside root %q: %w", absTarget, strings.TrimSuffix(p.root, string(filepath.Separator)), ErrAccessDenied)
	}
	return nil
}
