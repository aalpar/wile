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

package files

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/aalpar/wile/pkg/security"
)

// When the engine's authorizer confines file access to a root directory, the
// file primitives open paths through os.Root instead of the bare os package.
// os.Root resolves every component relative to a directory descriptor and
// refuses symlink/.. escapes atomically, which closes the TOCTOU gap between
// the policy check (security.CheckWithAuthorizer) and the open: a path
// component swapped after the check can no longer redirect the open outside
// the root. When the authorizer imposes no root (e.g. an unrestricted engine),
// the helpers fall back to the plain os operation on the original path.
//
// Exception: set-current-directory! has no os.Root counterpart for chdir and
// calls os.Chdir directly after its file:write gate; the TOCTOU window there is
// not closed.

// relWithinRoot expresses target relative to root. It tolerates root itself
// being a symlink (e.g. macOS /tmp -> /private/tmp) by retrying against the
// resolved root, so a target written either way maps to a clean relative name.
// os.Root enforces real containment at the syscall level regardless; this only
// computes the name handed to it.
func relWithinRoot(root, target string) (string, error) {
	abs, err := filepath.Abs(target)
	if err != nil {
		return "", err
	}
	rel, relErr := filepath.Rel(root, abs)
	if relErr == nil && rel != ".." && !strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
		return rel, nil
	}
	realRoot, evalErr := filepath.EvalSymlinks(root)
	if evalErr != nil {
		return rel, relErr
	}
	return filepath.Rel(realRoot, abs)
}

// openConfinementRoot opens an *os.Root for the authorizer's confinement root
// and returns target expressed relative to it. confined is false when auth
// imposes no root — callers then use the plain os operation on the original
// path. When confined and err is nil the caller MUST Close the returned root.
func openConfinementRoot(auth security.Authorizer, target string) (root *os.Root, rel string, confined bool, err error) {
	dir, ok := security.ConfinementRootOf(auth)
	if !ok {
		return nil, "", false, nil
	}
	root, err = os.OpenRoot(dir)
	if err != nil {
		return nil, "", false, err
	}
	rel, err = relWithinRoot(dir, target)
	if err != nil {
		root.Close() //nolint:errcheck
		return nil, "", false, err
	}
	return root, rel, true, nil
}

// unconfinedTarget returns the path to operate on when auth imposes no os.Root
// confinement. A custom (non-RootConfined) path-gating authorizer authorizes the
// lexical path the caller supplied; a symlink at that path would redirect the
// operation to a file outside the authorized subtree, and the plain os call
// follows it. Resolving symlinks and re-gating the real path under action keeps
// the gate target and the operation target the same file.
//
// When the full path does not resolve — most often because the leaf does not
// exist yet (a create/write target) — a symlink in the PARENT chain is still
// followed by the plain os call, so the parent is resolved on its own and the
// operation re-gated on realParent/leaf. This closes the create-time escape:
// os.Create("<allowed>/evil/new") where <allowed>/evil links outside the subtree
// writes outside it, yet EvalSymlinks fails on the missing leaf. If the parent
// itself does not resolve, the path is handed back unchanged so the plain os call
// reports the underlying error (ENOENT) and create can still make a new leaf.
func unconfinedTarget(auth security.Authorizer, name, action string) (string, error) {
	realPath, err := filepath.EvalSymlinks(name)
	if err != nil {
		parent, perr := filepath.EvalSymlinks(filepath.Dir(name))
		if perr != nil {
			// Parent unresolvable: defer to the os call, which reports ENOENT.
			return name, nil //nolint:nilerr // resolution failure defers to the os call
		}
		realPath = filepath.Join(parent, filepath.Base(name))
	}
	if realPath == name {
		return name, nil
	}
	err = security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   action,
		Target:   realPath,
	})
	if err != nil {
		return "", err
	}
	return realPath, nil
}

// confinedOpen opens name for reading, race-free when auth confines file
// access. The returned *os.File holds an independent descriptor and stays
// valid after the root is closed.
func confinedOpen(auth security.Authorizer, name string) (*os.File, error) {
	root, rel, confined, err := openConfinementRoot(auth, name)
	if err != nil {
		return nil, err
	}
	if !confined {
		target, terr := unconfinedTarget(auth, name, security.ActionRead)
		if terr != nil {
			return nil, terr
		}
		return os.Open(target)
	}
	defer root.Close() //nolint:errcheck
	return root.Open(rel)
}

// confinedCreate creates/truncates name for writing, race-free when confined.
func confinedCreate(auth security.Authorizer, name string) (*os.File, error) {
	root, rel, confined, err := openConfinementRoot(auth, name)
	if err != nil {
		return nil, err
	}
	if !confined {
		target, terr := unconfinedTarget(auth, name, security.ActionWrite)
		if terr != nil {
			return nil, terr
		}
		return os.Create(target)
	}
	defer root.Close() //nolint:errcheck
	return root.Create(rel)
}

// confinedOpenForAction dispatches to confinedOpen or confinedCreate by action.
func confinedOpenForAction(auth security.Authorizer, name, action string) (*os.File, error) {
	if action == security.ActionWrite {
		return confinedCreate(auth, name)
	}
	return confinedOpen(auth, name)
}

// confinedStat stats name, race-free when confined.
func confinedStat(auth security.Authorizer, name string) (os.FileInfo, error) {
	root, rel, confined, err := openConfinementRoot(auth, name)
	if err != nil {
		return nil, err
	}
	if !confined {
		target, terr := unconfinedTarget(auth, name, security.ActionStat)
		if terr != nil {
			return nil, terr
		}
		return os.Stat(target)
	}
	defer root.Close() //nolint:errcheck
	return root.Stat(rel)
}

// confinedRemove removes name, race-free when confined.
func confinedRemove(auth security.Authorizer, name string) error {
	root, rel, confined, err := openConfinementRoot(auth, name)
	if err != nil {
		return err
	}
	if !confined {
		target, terr := unconfinedTarget(auth, name, security.ActionDelete)
		if terr != nil {
			return terr
		}
		return os.Remove(target)
	}
	defer root.Close() //nolint:errcheck
	return root.Remove(rel)
}

// confinedMkdir creates directory name, race-free when confined.
func confinedMkdir(auth security.Authorizer, name string, perm os.FileMode) error {
	root, rel, confined, err := openConfinementRoot(auth, name)
	if err != nil {
		return err
	}
	if !confined {
		target, terr := unconfinedTarget(auth, name, security.ActionWrite)
		if terr != nil {
			return terr
		}
		return os.Mkdir(target, perm)
	}
	defer root.Close() //nolint:errcheck
	return root.Mkdir(rel, perm)
}

// confinedReadDir lists directory entries of name, race-free when confined.
// os.Root has no ReadDir, so the directory is opened within the root and read
// through its descriptor.
func confinedReadDir(auth security.Authorizer, name string) ([]os.DirEntry, error) {
	root, rel, confined, err := openConfinementRoot(auth, name)
	if err != nil {
		return nil, err
	}
	if !confined {
		target, terr := unconfinedTarget(auth, name, security.ActionRead)
		if terr != nil {
			return nil, terr
		}
		return os.ReadDir(target)
	}
	defer root.Close() //nolint:errcheck
	dir, err := root.Open(rel)
	if err != nil {
		return nil, err
	}
	defer dir.Close() //nolint:errcheck
	return dir.ReadDir(-1)
}
