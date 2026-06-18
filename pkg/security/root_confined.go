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

// RootConfined is implemented by authorizers that confine filesystem access
// to a single directory subtree. When an authorizer reports a confinement
// root, file primitives open paths through os.Root, giving race-free,
// syscall-level containment that closes the TOCTOU gap between the policy
// check (Authorize) and the open.
type RootConfined interface {
	// ConfinementRoot returns the directory file access is confined to, and
	// ok=true, when this authorizer is root-confining. ok=false means the
	// authorizer imposes no single-root file confinement.
	ConfinementRoot() (root string, ok bool)
}

// ConfinementRootOf reports the filesystem confinement root that applies to
// auth, if any. It unwraps All() composites, returning the first member that
// confines to a root.
//
// This is a defense-in-depth bound, not the exact confinement. A composite is
// an intersection, so the true confinement is the intersection of every
// member's root, which is always a subset of the first member's root. The
// os.Root layer this drives is therefore never wider than that member's
// subtree, but for a multi-root composite it may be looser than the full
// intersection. That is safe: the policy layer (Authorize) enforces the full
// intersection and runs before any open, so returning the first root cannot
// widen access. (If multi-root composites ever need exact os.Root containment,
// return the deepest containing root for nested members and ok=false for
// disjoint ones — the profiles currently construct only single-root
// composites, so first-member-wins suffices.)
//
// Returns ok=false when nothing confines auth to a root — callers then fall
// back to unconfined os operations, still gated by the policy layer.
func ConfinementRootOf(auth Authorizer) (string, bool) {
	switch a := auth.(type) {
	case RootConfined:
		return a.ConfinementRoot()
	case *compositeAuthorizer:
		for _, m := range a.authorizers {
			root, ok := ConfinementRootOf(m)
			if ok {
				return root, true
			}
		}
	}
	return "", false
}
