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
	"os"
	"path/filepath"
	"strings"
)

// containmentStep is the directory reached after one path component.
type containmentStep struct {
	path     string // real path when resolved; otherwise the parent's path joined with the name
	link     bool   // the component is a symlink, resolvable or dangling
	resolved bool   // path exists and is symlink-free
}

// resolveForContainment resolves p through symlinks as far as it exists,
// returning the real path of the longest existing prefix joined with the
// remaining (not-yet-existing) components. ok is false when a ".." backs out
// of a symlink.
//
// This is what lets containment checks be both symlink-safe AND usable for
// paths that do not exist yet (e.g. a file about to be created): the existing
// part is canonicalised (so a symlink that escapes the root is followed and
// caught), while the non-existent tail is appended verbatim (so creating a new
// file under the root is still allowed).
//
// Components are taken in order and p is never cleaned first: filepath.Abs or
// Clean would fold <root>/link/.. to <root> by its spelling, while chdir and
// open follow link and land in the parent of its target.
//
// A ".." that backs out of a symlink is refused rather than resolved. Resolving
// it would agree with the kernel but not with the os.Root consumers
// (extensions/files, resolver), which hand os.Root a lexically cleaned name;
// os.Root itself refuses such a path even when the link stays inside the root.
// Refused, every admitted path names the same file under lexical cleaning, the
// kernel, and os.Root.
func resolveForContainment(p string) (string, bool) {
	abs := p
	if !filepath.IsAbs(abs) {
		wd, err := os.Getwd()
		if err != nil {
			return "", false
		}
		abs = wd + string(filepath.Separator) + abs
	}
	vol := filepath.VolumeName(abs)
	sep := string(filepath.Separator)
	steps := []containmentStep{{path: vol + sep, resolved: true}}
	for name := range strings.SplitSeq(filepath.FromSlash(abs[len(vol):]), sep) {
		top := steps[len(steps)-1]
		switch name {
		case "", ".":
		case "..":
			if top.link {
				return "", false
			}
			if len(steps) > 1 {
				steps = steps[:len(steps)-1]
			}
		default:
			steps = append(steps, nextContainmentStep(top, name))
		}
	}
	return steps[len(steps)-1].path, true
}

// nextContainmentStep descends from parent into name. Once a component fails to
// resolve, every later one is appended verbatim.
func nextContainmentStep(parent containmentStep, name string) containmentStep {
	next := filepath.Join(parent.path, name)
	if !parent.resolved {
		return containmentStep{path: next}
	}
	resolved, err := filepath.EvalSymlinks(next)
	if err == nil {
		// parent.path is symlink-free, so only name can have redirected.
		return containmentStep{path: resolved, link: resolved != next, resolved: true}
	}
	info, err := os.Lstat(next)
	return containmentStep{path: next, link: err == nil && info.Mode()&os.ModeSymlink != 0}
}

// containedInRoot reports whether target is at, or under, root after resolving
// symlinks on both. Resolving the root as well is required because the root
// itself may be a symlink (e.g. macOS /tmp -> /private/tmp); without it every
// /tmp path would resolve to /private/tmp and fail a literal "/tmp" prefix
// check. A symlink inside the root that points outside it resolves to its real
// target and is correctly rejected, as is any path resolveForContainment
// refuses.
func containedInRoot(root, target string) bool {
	realRoot, ok := resolveForContainment(root)
	if !ok {
		return false
	}
	realTarget, ok := resolveForContainment(target)
	if !ok {
		return false
	}
	if realTarget == realRoot {
		return true
	}
	sep := string(filepath.Separator)
	if !strings.HasSuffix(realRoot, sep) {
		realRoot += sep
	}
	return strings.HasPrefix(realTarget, realRoot)
}
