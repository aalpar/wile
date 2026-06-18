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
	"path/filepath"
	"strings"
)

// resolveForContainment resolves p through symlinks as far as it exists,
// returning the real path of the longest existing ancestor joined with the
// remaining (not-yet-existing) path components.
//
// This is what lets containment checks be both symlink-safe AND usable for
// paths that do not exist yet (e.g. a file about to be created): the existing
// part is canonicalised (so a symlink that escapes the root is followed and
// caught), while the non-existent tail is appended verbatim (so creating a new
// file under the root is still allowed).
func resolveForContainment(p string) string {
	abs, err := filepath.Abs(p)
	if err != nil {
		abs = filepath.Clean(p)
	}
	remainder := ""
	cur := abs
	for {
		resolved, err := filepath.EvalSymlinks(cur)
		if err == nil {
			if remainder == "" {
				return resolved
			}
			return filepath.Join(resolved, remainder)
		}
		parent := filepath.Dir(cur)
		if parent == cur {
			// Reached the filesystem root without resolving anything;
			// fall back to the cleaned absolute path.
			return abs
		}
		remainder = filepath.Join(filepath.Base(cur), remainder)
		cur = parent
	}
}

// containedInRoot reports whether target is at, or under, root after resolving
// symlinks on both. Resolving the root as well is required because the root
// itself may be a symlink (e.g. macOS /tmp -> /private/tmp); without it every
// /tmp path would resolve to /private/tmp and fail a literal "/tmp" prefix
// check. A symlink inside the root that points outside it resolves to its real
// target and is correctly rejected.
func containedInRoot(root, target string) bool {
	realRoot := resolveForContainment(root)
	realTarget := resolveForContainment(target)
	if realTarget == realRoot {
		return true
	}
	sep := string(filepath.Separator)
	if !strings.HasSuffix(realRoot, sep) {
		realRoot += sep
	}
	return strings.HasPrefix(realTarget, realRoot)
}
