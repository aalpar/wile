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

package values

import "strings"

// deepMarker is emitted by SchemeString when a value nests deeper than
// DefaultMaxWriteDepth. Unlike the writer (which returns an error and refuses
// to emit unreadable output), SchemeString satisfies the Value interface's
// SchemeString() string contract and cannot raise, so it degrades to this
// marker instead of overflowing the host Go stack. It is intentionally
// distinct from the cycle marker "..." so a truncated-because-deep subtree is
// distinguishable from a truncated-because-cyclic one in debugging output.
// The nesting bound reuses DefaultMaxWriteDepth (the single host-safe nesting
// number, counted root = 1, +1 per container descent, matching the writer and
// readSyntax); like every SchemeString marker it is not intended to re-read.
const deepMarker = "#<deep>"

// formatIndexable builds the Scheme external representation for a
// fixed-size indexable collection.  Format: prefix<elem1> <elem2> ... )
// with elements separated by single spaces and no padding around elements.
//
// visited threads the cycle-detection set into nested Pair/Vector elements
// so a self-referential or cross-referential structure renders a bounded
// "..." marker instead of overflowing the Go stack. Callers whose elements
// can never be compound (e.g. ByteVector, whose elements are bytes) may pass
// nil; schemeStringChild only writes to the set for *Pair/*Vector children.
//
// depth is the nesting level of the *elements* (the container's own level + 1),
// threaded into schemeStringChild so a deeply nested acyclic structure is
// bounded at DefaultMaxWriteDepth. Every element is a sibling at the same level,
// so length does not consume depth: a flat collection of any size renders in
// full (only genuine nesting counts, matching the writer).
func formatIndexable(prefix string, length int, get func(int) Value, visited MapSet[Value], depth int) string {
	q := &strings.Builder{}
	q.WriteString(prefix)
	if length > 0 {
		q.WriteString(schemeStringChild(get(0), visited, depth))
		for i := 1; i < length; i++ {
			q.WriteString(" ")
			q.WriteString(schemeStringChild(get(i), visited, depth))
		}
	}
	q.WriteString(")")
	return q.String()
}

// schemeStringChild renders a single element of a compound structure,
// threading the shared visited set through nested Pair/Vector/Hashtable/Box/
// AtomicBox so that cross-collection cycles (vector→pair→vector,
// pair→hashtable→pair, and the like) are bounded. Marking is path-scoped: each
// compound renderer unmarks itself on exit, so structural sharing (an acyclic
// DAG) renders in full and only a true self-cycle collapses to "...".
// Non-compound elements render via SchemeString; void and nil pointers render
// as "#<void>".
//
// The visited set is keyed on pointer-identity Value types only (*Pair,
// *Vector, *Hashtable, *Box, *AtomicBox — the only types inserted). Never
// insert a value-equality or non-comparable Value: the former would alias
// distinct nodes to one slot (false cycles), the latter would panic on map
// insertion.
//
// A nil visited set means "no cycle tracking requested" — used by callers
// whose elements can never be compound (e.g. ByteVector, whose elements are
// bytes). Should a compound child be encountered anyway, a fresh set is
// allocated lazily for that subtree, so a nil set can never cause a nil-map
// write. This keeps the nil contract a safe "no tracking" rather than an
// unchecked "I promise no compound children" caller obligation.
func schemeStringChild(child Value, visited MapSet[Value], depth int) string {
	// Host-safety bound: this is the single chokepoint through which every
	// compound descent flows (Pair car / improper cdr, Vector element,
	// Hashtable key/value), so one guard here bounds all container types.
	// depth is this child's nesting level; beyond the bound we degrade to
	// deepMarker rather than recurse another Go frame. Guarding before the
	// type switch also uniformly caps leaves at the same level (a leaf can
	// only reach this depth nested under >DefaultMaxWriteDepth containers,
	// which are being truncated anyway).
	if depth > DefaultMaxWriteDepth {
		return deepMarker
	}
	switch c := child.(type) {
	case *Pair:
		if c == nil {
			return "#<void>"
		}
		if visited == nil {
			visited = NewMapSet[Value](0)
		}
		return c.schemeStringWithVisited(visited, depth)
	case *Vector:
		if c == nil {
			return "#<void>"
		}
		if visited == nil {
			visited = NewMapSet[Value](0)
		}
		return c.schemeStringWithVisited(visited, depth)
	case *Hashtable:
		if c == nil {
			return "#<void>"
		}
		if visited == nil {
			visited = NewMapSet[Value](0)
		}
		return c.schemeStringWithVisited(visited, depth)
	case *Box:
		if c == nil {
			return "#<box:void>"
		}
		if visited == nil {
			visited = NewMapSet[Value](0)
		}
		return c.schemeStringWithVisited(visited, depth)
	case *AtomicBox:
		if c == nil {
			return "#<atomic:void>"
		}
		if visited == nil {
			visited = NewMapSet[Value](0)
		}
		return c.schemeStringWithVisited(visited, depth)
	}
	if IsVoid(child) {
		return "#<void>"
	}
	return child.SchemeString()
}
