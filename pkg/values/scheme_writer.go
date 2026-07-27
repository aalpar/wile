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

import (
	"fmt"
	"sort"
	"strings"

	"github.com/aalpar/wile/pkg/werr"
)

// DefaultMaxWriteDepth bounds structural nesting depth during writing.
//
// The writer descends recursively into the car of each pair and into vector
// elements (the cdr-spine of a list is walked iteratively, so list *length*
// is unbounded — only nesting *depth* is capped). Without a bound, a deeply
// nested value — necessarily one built programmatically, since the reader caps
// textual input at parser.DefaultMaxParseDepth — overflows the host Go stack
// with a fatal, unrecoverable crash.
//
// The default deliberately equals the parser's DefaultMaxParseDepth: the
// guiding invariant is "anything the writer emits must be valid on read." A
// value nested deeper than the reader accepts could not be read back, so the
// writer refuses it with ErrWriteDepthExceeded rather than emit unreadable
// output. The depth count matches readSyntax exactly (root = 1, +1 per
// container descent), so the write limit and the read limit trip on the same
// structures. 0 means unlimited. Mirrors the VM's DefaultMaxCallDepth, the
// parser's DefaultMaxParseDepth, and the expander's DefaultMaxExpandDepth.
const DefaultMaxWriteDepth int = 10000

// WriteMode controls how the SchemeWriter handles shared structure.
//
// R7RS §6.13.3 specifies three output procedures with different sharing semantics:
//   - write: datum labels only for circular references (WriteModeWrite)
//   - write-shared: datum labels for all shared references (WriteModeWriteShared)
//   - write-simple: no datum labels at all (handled separately via SchemeString)
type WriteMode int

const (
	// WriteModeWrite labels only circular references.
	// R7RS §6.13.3: write outputs datum labels only for objects that are part of a cycle.
	WriteModeWrite WriteMode = iota
	// WriteModeWriteShared labels all multiply-referenced objects.
	// R7RS §6.13.3: write-shared outputs datum labels for all shared structure.
	WriteModeWriteShared
)

// SchemeWriter provides cycle-aware writing of Scheme values.
// Two-pass datum label output (R7RS §2.4): pass 1 (findShared) traverses
// the value graph to identify multiply-referenced objects; pass 2 (write)
// emits #n= definitions on first encounter and #n# references thereafter.
// See BIBLIOGRAPHY.md "Two-Pass Datum Label Output".
//
// Implementation note: Uses maps with concrete *Pair, *Vector, *Box and *Hashtable keys
// (not Tuple/Indexable interfaces) because:
// 1. Go map keys must be comparable types - interfaces are not suitable
// 2. Cycle/sharing detection requires pointer identity tracking
// 3. Each concrete type needs separate tracking for proper label assignment
type SchemeWriter struct {
	// seenPairs maps pair pointers to their assigned label numbers.
	// A value of -1 means the object has been seen but not yet labeled.
	// Must use *Pair (not Tuple) because Go map keys must be concrete comparable types.
	seenPairs map[*Pair]int
	// seenVectors maps vector pointers to their assigned label numbers.
	seenVectors map[*Vector]int
	// seenBoxes maps box pointers to their assigned label numbers. A Box is a
	// mutable one-slot container, so set-box! can build a cycle through it
	// exactly as set-car!/vector-set! can; it needs the same identity tracking.
	seenBoxes map[*Box]int
	// nextLabel is the next datum label number to assign.
	nextLabel int
	// needsLabelPair tracks which pairs need labels (referenced more than once).
	// Must use *Pair (not Tuple) for map key comparability.
	needsLabelPair map[*Pair]bool
	// needsLabelVector tracks which vectors need labels.
	needsLabelVector map[*Vector]bool
	// needsLabelBox tracks which boxes need labels.
	needsLabelBox map[*Box]bool
	// seenHashtables maps hashtable pointers to their assigned label numbers.
	// A Hashtable value slot is mutable, so a cycle can pass through it exactly
	// as it can through a pair car, vector element, or box; it needs the same
	// pointer-identity tracking to both label sharing and terminate on cycles.
	seenHashtables map[*Hashtable]int
	// needsLabelHashtable tracks which hashtables need labels.
	needsLabelHashtable map[*Hashtable]bool
	// displayMode indicates whether to use display format (no quotes on strings).
	displayMode bool
	// writeMode controls whether to label all shared or only circular references.
	writeMode WriteMode
	// maxDepth caps structural nesting depth; 0 = unlimited. See DefaultMaxWriteDepth.
	maxDepth int
	// err records the first depth-limit violation seen during the findShared
	// pass; the traversal unwinds and WriteString reports it. It is per-call
	// working state, reset at the start of each WriteString (see there).
	err error
}

// NewSchemeWriter creates a new SchemeWriter for cycle-aware output.
// Default mode is WriteModeWrite (labels only circular references).
func NewSchemeWriter() *SchemeWriter {
	q := &SchemeWriter{
		seenPairs:           make(map[*Pair]int),
		seenVectors:         make(map[*Vector]int),
		seenBoxes:           make(map[*Box]int),
		needsLabelPair:      make(map[*Pair]bool),
		needsLabelVector:    make(map[*Vector]bool),
		needsLabelBox:       make(map[*Box]bool),
		seenHashtables:      make(map[*Hashtable]int),
		needsLabelHashtable: make(map[*Hashtable]bool),
		nextLabel:           0,
		writeMode:           WriteModeWrite,
		maxDepth:            DefaultMaxWriteDepth,
	}
	return q
}

// SetMaxDepth sets the maximum structural nesting depth the writer will descend
// before reporting ErrWriteDepthExceeded. A value of 0 means unlimited; negative
// values are clamped to 0. See DefaultMaxWriteDepth for the rationale. Mirrors
// the parser's SetMaxDepth.
func (p *SchemeWriter) SetMaxDepth(n int) {
	if n < 0 {
		n = 0
	}
	p.maxDepth = n
}

// WriteString writes a Scheme value to a string with cycle detection.
// Circular and shared structures are represented using datum labels. It
// returns ErrWriteDepthExceeded if the value nests deeper than maxDepth (see
// DefaultMaxWriteDepth); on that error the returned string is empty.
func (p *SchemeWriter) WriteString(v Value) (string, error) {
	// Reset all per-call working state so a SchemeWriter is safe to reuse across
	// WriteString calls. Resetting err alone would not suffice — the label
	// counter and the cycle/share maps also carry per-value state — so reuse
	// would otherwise leak labels and sharing decisions from the prior value.
	p.err = nil
	p.nextLabel = 0
	p.seenPairs = make(map[*Pair]int)
	p.seenVectors = make(map[*Vector]int)
	p.seenBoxes = make(map[*Box]int)
	p.needsLabelPair = make(map[*Pair]bool)
	p.needsLabelVector = make(map[*Vector]bool)
	p.needsLabelBox = make(map[*Box]bool)
	p.seenHashtables = make(map[*Hashtable]int)
	p.needsLabelHashtable = make(map[*Hashtable]bool)

	// First pass: identify which objects are referenced multiple times. This
	// is also the depth-enforcing pass — it is the first traversal to descend
	// the car/element recursion, so it trips the nesting bound before any
	// deeper pass runs. Pass 2 below prunes on the same visited sets as pass 1,
	// so its recursion tree is the same shape and it inherits the bound. Pass 3
	// does NOT: it short-circuits only on an assigned datum label, so in
	// WriteModeWrite (and display mode), where filterToCircular strips
	// non-circular labels, a shared acyclic subtree is re-expanded on every
	// path and write can recurse deeper than the depth findShared measured.
	p.findShared(v, 1)
	if p.err != nil {
		return "", p.err
	}

	// For WriteModeWrite, filter to only circular references
	if p.writeMode == WriteModeWrite {
		p.filterToCircular(v)
	}

	// Reset seen maps for the output pass
	p.seenPairs = make(map[*Pair]int)
	p.seenVectors = make(map[*Vector]int)
	p.seenBoxes = make(map[*Box]int)
	p.seenHashtables = make(map[*Hashtable]int)

	// Third pass: generate output with labels
	q := &strings.Builder{}
	p.write(q, v)
	return q.String(), nil
}

// findShared traverses the value to find objects that are referenced multiple
// times, and enforces the nesting-depth bound. depth is the structural nesting
// level of v (the root is 1, incremented once per container descent), counted
// identically to the parser's readSyntax so the write and read limits trip on
// the same structures.
//
// The cdr-spine of a list is walked iteratively: list length is not nesting
// depth (a flat list of any length reads back at depth 1), so it must not
// consume the depth budget nor the Go stack. Only descent into a car or vector
// element — genuine nesting — recurses and increments depth. On the first
// depth violation p.err is set and the traversal unwinds.
func (p *SchemeWriter) findShared(v Value, depth int) {
	if p.maxDepth > 0 && depth > p.maxDepth {
		// "writer:" not "write:" — this path also serves display/write-shared.
		p.err = werr.WrapForeignErrorf(werr.ErrWriteDepthExceeded,
			"writer: nesting depth exceeds maximum of %d", p.maxDepth)
		return
	}
	switch val := v.(type) {
	case *Pair:
		// Walk the cdr-spine in a loop; recurse only into cars (one level deeper).
		for cur := val; cur != nil; {
			_, found := p.seenPairs[cur]
			if found {
				// Seen before - mark as needing a label
				p.needsLabelPair[cur] = true
				return
			}
			// Mark as seen (with placeholder -1)
			p.seenPairs[cur] = -1
			p.findShared(cur.Car(), depth+1)
			if p.err != nil {
				return
			}
			cdr := cur.Cdr()
			nextPair, ok := cdr.(*Pair)
			if !ok {
				// Improper tail: a non-pair, non-empty cdr is a sibling element
				// at the same nesting level as the cars (depth+1).
				if cdr != nil && !IsEmptyList(cdr) {
					p.findShared(cdr, depth+1)
				}
				return
			}
			cur = nextPair
		}

	case *Vector:
		if val == nil || len(*val) == 0 {
			return
		}
		_, found := p.seenVectors[val]
		if found {
			p.needsLabelVector[val] = true
			return
		}
		p.seenVectors[val] = -1
		for _, elem := range *val {
			p.findShared(elem, depth+1)
			if p.err != nil {
				return
			}
		}

	case *Box:
		if val == nil {
			return
		}
		_, found := p.seenBoxes[val]
		if found {
			p.needsLabelBox[val] = true
			return
		}
		p.seenBoxes[val] = -1
		p.findShared(val.Value, depth+1)

	case *Hashtable:
		if val == nil {
			return
		}
		_, found := p.seenHashtables[val]
		if found {
			p.needsLabelHashtable[val] = true
			return
		}
		p.seenHashtables[val] = -1
		// Only values recurse: no container type is Hashable, so a key is always
		// a leaf and cannot carry sharing or a cycle (TestNoContainerIsHashable).
		for _, e := range val.snapshot() {
			p.findShared(e.value, depth+1)
			if p.err != nil {
				return
			}
		}
	}
}

// filterToCircular replaces the four needsLabel* maps with only the circular entries.
// An object is circular if it is reachable from itself — i.e., it appears on the
// DFS recursion stack when revisited. This uses gray/black DFS coloring.
func (p *SchemeWriter) filterToCircular(v Value) {
	circularPairs := make(map[*Pair]bool)
	circularVectors := make(map[*Vector]bool)
	circularBoxes := make(map[*Box]bool)
	onStackPairs := make(map[*Pair]bool)
	onStackVectors := make(map[*Vector]bool)
	onStackBoxes := make(map[*Box]bool)
	visitedPairs := make(map[*Pair]bool)
	visitedVectors := make(map[*Vector]bool)
	visitedBoxes := make(map[*Box]bool)
	circularHashtables := make(map[*Hashtable]bool)
	onStackHashtables := make(map[*Hashtable]bool)
	visitedHashtables := make(map[*Hashtable]bool)

	var walk func(v Value)
	walk = func(v Value) {
		switch val := v.(type) {
		case *Pair:
			// Walk the cdr-spine iteratively. Every pair on the spine is an
			// ancestor of the cars explored below it, so all stay on the DFS
			// stack until the spine terminates, then pop together. A back-edge
			// into a still-on-stack pair (via cdr here, or via a car in the
			// recursive walk) is the cycle. Only car descent recurses, so the
			// Go stack tracks nesting depth — already bounded by pass 1 — not
			// list length.
			spine := []*Pair{}
			for cur := val; cur != nil; {
				if onStackPairs[cur] {
					circularPairs[cur] = true
					break
				}
				if visitedPairs[cur] {
					break
				}
				visitedPairs[cur] = true
				onStackPairs[cur] = true
				spine = append(spine, cur)
				walk(cur.Car())
				cdr := cur.Cdr()
				nextPair, ok := cdr.(*Pair)
				if !ok {
					if cdr != nil && !IsEmptyList(cdr) {
						walk(cdr)
					}
					break
				}
				cur = nextPair
			}
			for _, pr := range spine {
				delete(onStackPairs, pr)
			}

		case *Vector:
			if val == nil || len(*val) == 0 {
				return
			}
			if onStackVectors[val] {
				circularVectors[val] = true
				return
			}
			if visitedVectors[val] {
				return
			}
			visitedVectors[val] = true
			onStackVectors[val] = true
			for _, elem := range *val {
				walk(elem)
			}
			delete(onStackVectors, val)

		case *Box:
			if val == nil {
				return
			}
			if onStackBoxes[val] {
				circularBoxes[val] = true
				return
			}
			if visitedBoxes[val] {
				return
			}
			visitedBoxes[val] = true
			onStackBoxes[val] = true
			walk(val.Value)
			delete(onStackBoxes, val)

		case *Hashtable:
			if val == nil {
				return
			}
			if onStackHashtables[val] {
				circularHashtables[val] = true
				return
			}
			if visitedHashtables[val] {
				return
			}
			visitedHashtables[val] = true
			onStackHashtables[val] = true
			for _, e := range val.snapshot() {
				walk(e.value)
			}
			delete(onStackHashtables, val)
		}
	}

	walk(v)

	p.needsLabelPair = circularPairs
	p.needsLabelVector = circularVectors
	p.needsLabelBox = circularBoxes
	p.needsLabelHashtable = circularHashtables
}

// write outputs a value, handling cycles with datum labels.
func (p *SchemeWriter) write(sb *strings.Builder, v Value) {
	switch val := v.(type) {
	case *Pair:
		p.writePair(sb, val)
	case *Vector:
		p.writeVector(sb, val)
	case *Box:
		p.writeBox(sb, val)
	case *Hashtable:
		p.writeHashtable(sb, val)
	case *String:
		// In display mode, strings are printed without quotes
		if p.displayMode {
			sb.WriteString(val.Value)
		} else {
			sb.WriteString(val.SchemeString())
		}
	case *Character:
		// In display mode, characters are printed as-is without #\
		if p.displayMode {
			sb.WriteRune(val.Value)
		} else {
			sb.WriteString(val.SchemeString())
		}
	case *Symbol:
		// In display mode, symbols are printed without |…| bar-escaping
		// (R7RS §6.13.3: display emits the human-readable representation).
		// write uses the re-readable external representation via SchemeString,
		// which bars names that cannot be written bare.
		if p.displayMode {
			sb.WriteString(val.Key)
		} else {
			sb.WriteString(val.SchemeString())
		}
	default:
		// For non-compound types, just use SchemeString
		if v != nil {
			sb.WriteString(v.SchemeString())
		} else {
			sb.WriteString("#<void>")
		}
	}
}

// writePair writes a pair with cycle detection.
//
// Implementation note: Must accept *Pair (not Tuple) because:
// 1. It needs map lookup with concrete pointer (seenPairs, needsLabelPair)
// 2. Pointer identity is used for tracking shared/circular structure
func (p *SchemeWriter) writePair(sb *strings.Builder, pr *Pair) {
	if pr == nil {
		sb.WriteString("#<void>")
		return
	}

	// Check if this is a back-reference
	label, found := p.seenPairs[pr]
	if found && label >= 0 {
		// Already labeled and written - output reference
		fmt.Fprintf(sb, "#%d#", label)
		return
	}

	// Check if this needs a label
	if p.needsLabelPair[pr] {
		label := p.nextLabel
		p.nextLabel++
		p.seenPairs[pr] = label
		fmt.Fprintf(sb, "#%d=", label)
	}

	// Write the pair content
	sb.WriteString("(")
	p.writePairContents(sb, pr)
	sb.WriteString(")")
}

// writePairContents writes the contents of a list (without outer parens).
//
// Implementation note: Must accept *Pair and check for *Pair in the loop because:
// 1. Need to check if cdr is in seenPairs/needsLabelPair maps (requires *Pair)
// 2. Pointer identity tracking for back-references
// 3. Cannot use Tuple because we need access to concrete pointer for map lookup
func (p *SchemeWriter) writePairContents(sb *strings.Builder, pr *Pair) {
	first := true
	curr := pr

	for curr != nil {
		if !first {
			sb.WriteString(" ")
		}
		first = false

		// Write car
		p.write(sb, curr.Car())

		// Check cdr
		cdr := curr.Cdr()
		if cdr == nil || IsEmptyList(cdr) {
			break
		}

		// Check if cdr is a pair - type assertion required for map lookups below
		nextPair, ok := cdr.(*Pair)
		if !ok {
			// Improper list
			sb.WriteString(" . ")
			p.write(sb, cdr)
			break
		}

		// Check if the cdr pair needs special handling (shared/circular)
		label, found := p.seenPairs[nextPair]
		if found && label >= 0 {
			// Back-reference in cdr position
			sb.WriteString(" . ")
			fmt.Fprintf(sb, "#%d#", label)
			break
		}

		if p.needsLabelPair[nextPair] {
			// The cdr needs its own label - write as dotted pair
			sb.WriteString(" . ")
			p.writePair(sb, nextPair)
			break
		}

		curr = nextPair
	}
}

// writeVector writes a vector with cycle detection.
func (p *SchemeWriter) writeVector(sb *strings.Builder, vec *Vector) {
	if vec == nil {
		sb.WriteString("#()")
		return
	}

	// Check if this is a back-reference
	label, found := p.seenVectors[vec]
	if found && label >= 0 {
		fmt.Fprintf(sb, "#%d#", label)
		return
	}

	// Check if this needs a label
	if p.needsLabelVector[vec] {
		label := p.nextLabel
		p.nextLabel++
		p.seenVectors[vec] = label
		fmt.Fprintf(sb, "#%d=", label)
	}

	sb.WriteString("#(")
	for i, elem := range *vec {
		if i > 0 {
			sb.WriteString(" ")
		}
		p.write(sb, elem)
	}
	sb.WriteString(")")
}

// writeBox writes a box with cycle detection, mirroring writeVector. Without
// this arm the default write branch would fall through to Box.SchemeString,
// whose recursion is not label-aware and overflows the host stack on a cycle.
func (p *SchemeWriter) writeBox(sb *strings.Builder, bx *Box) {
	if bx == nil {
		sb.WriteString("#<box:void>")
		return
	}

	label, found := p.seenBoxes[bx]
	if found && label >= 0 {
		fmt.Fprintf(sb, "#%d#", label)
		return
	}

	if p.needsLabelBox[bx] {
		label := p.nextLabel
		p.nextLabel++
		p.seenBoxes[bx] = label
		fmt.Fprintf(sb, "#%d=", label)
	}

	sb.WriteString("#&")
	p.write(sb, bx.Value)
}

// writeHashtable writes a hashtable with cycle detection, mirroring writeVector.
// Without this arm the default write branch falls through to
// Hashtable.SchemeString, whose path-scoped recursion emits "..." on a cycle and
// never assigns datum labels, so sharing or a cycle routed through a hashtable
// value would be under-represented (R7RS 6.13.3). Only values recurse: no
// container type is Hashable, so a key is always a leaf. Entries are sorted by
// rendered key for deterministic output, matching SchemeString.
func (p *SchemeWriter) writeHashtable(sb *strings.Builder, h *Hashtable) {
	if h == nil {
		sb.WriteString("#hash()")
		return
	}

	label, found := p.seenHashtables[h]
	if found && label >= 0 {
		fmt.Fprintf(sb, "#%d#", label)
		return
	}

	if p.needsLabelHashtable[h] {
		label := p.nextLabel
		p.nextLabel++
		p.seenHashtables[h] = label
		fmt.Fprintf(sb, "#%d=", label)
	}

	entries := h.snapshot()
	sort.Slice(entries, func(i, j int) bool {
		return entries[i].key.SchemeString() < entries[j].key.SchemeString()
	})
	sb.WriteString("#hash(")
	for i, e := range entries {
		if i > 0 {
			sb.WriteString(" ")
		}
		sb.WriteString("(")
		p.write(sb, e.key)
		sb.WriteString(" . ")
		p.write(sb, e.value)
		sb.WriteString(")")
	}
	sb.WriteString(")")
}

// WriteValueToString writes a Scheme value to a string with cycle detection.
// Uses WriteModeWrite: datum labels only for circular references.
// R7RS §6.13.3: write outputs datum labels only for objects that are part of a cycle.
// Returns ErrWriteDepthExceeded for values nested deeper than DefaultMaxWriteDepth.
func WriteValueToString(v Value) (string, error) {
	w := NewSchemeWriter()
	return w.WriteString(v)
}

// WriteSharedValueToString writes a Scheme value to a string with shared structure detection.
// Uses WriteModeWriteShared: datum labels for all multiply-referenced objects.
// R7RS §6.13.3: write-shared outputs datum labels for all shared structure.
// Returns ErrWriteDepthExceeded for values nested deeper than DefaultMaxWriteDepth.
func WriteSharedValueToString(v Value) (string, error) {
	w := NewSchemeWriter()
	w.writeMode = WriteModeWriteShared
	return w.WriteString(v)
}

// DisplayValueToString writes a Scheme value to a string for display with cycle detection.
// Unlike WriteValueToString, strings are printed without quotes and characters without #\.
// Returns ErrWriteDepthExceeded for values nested deeper than DefaultMaxWriteDepth.
func DisplayValueToString(v Value) (string, error) {
	w := NewSchemeWriter()
	w.displayMode = true
	return w.WriteString(v)
}
