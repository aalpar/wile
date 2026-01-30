// Copyright 2025 Aaron Alpar
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
	"strings"
)

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
// It detects shared and circular structures and outputs them using
// datum labels (#n= for definitions and #n# for references) per R7RS §2.4.
type SchemeWriter struct {
	// seenPairs maps pair pointers to their assigned label numbers.
	// A value of -1 means the object has been seen but not yet labeled.
	seenPairs map[*Pair]int
	// seenVectors maps vector pointers to their assigned label numbers.
	seenVectors map[*Vector]int
	// nextLabel is the next datum label number to assign.
	nextLabel int
	// needsLabelPair tracks which pairs need labels (referenced more than once).
	needsLabelPair map[*Pair]bool
	// needsLabelVector tracks which vectors need labels.
	needsLabelVector map[*Vector]bool
	// displayMode indicates whether to use display format (no quotes on strings).
	displayMode bool
	// writeMode controls whether to label all shared or only circular references.
	writeMode WriteMode
}

// NewSchemeWriter creates a new SchemeWriter for cycle-aware output.
// Default mode is WriteModeWrite (labels only circular references).
func NewSchemeWriter() *SchemeWriter {
	q := &SchemeWriter{
		seenPairs:        make(map[*Pair]int),
		seenVectors:      make(map[*Vector]int),
		needsLabelPair:   make(map[*Pair]bool),
		needsLabelVector: make(map[*Vector]bool),
		nextLabel:        0,
		writeMode:        WriteModeWrite,
	}
	return q
}

// WriteString writes a Scheme value to a string with cycle detection.
// Circular and shared structures are represented using datum labels.
func (w *SchemeWriter) WriteString(v Value) string {
	// First pass: identify which objects are referenced multiple times
	w.findShared(v)

	// For WriteModeWrite, filter to only circular references
	if w.writeMode == WriteModeWrite {
		w.filterToCircular(v)
	}

	// Reset seen maps for the output pass
	w.seenPairs = make(map[*Pair]int)
	w.seenVectors = make(map[*Vector]int)

	// Second pass: generate output with labels
	q := &strings.Builder{}
	w.write(q, v)
	return q.String()
}

// findShared traverses the value to find objects that are referenced multiple times.
func (w *SchemeWriter) findShared(v Value) {
	switch val := v.(type) {
	case *Pair:
		if val == nil || val.IsEmptyList() {
			return
		}
		if _, found := w.seenPairs[val]; found {
			// Seen before - mark as needing a label
			w.needsLabelPair[val] = true
			return
		}
		// Mark as seen (with placeholder -1)
		w.seenPairs[val] = -1
		// Recurse into car and cdr
		w.findShared(val.Car())
		w.findShared(val.Cdr())

	case *Vector:
		if val == nil || len(*val) == 0 {
			return
		}
		if _, found := w.seenVectors[val]; found {
			w.needsLabelVector[val] = true
			return
		}
		w.seenVectors[val] = -1
		for _, elem := range *val {
			w.findShared(elem)
		}
	}
}

// filterToCircular removes non-circular entries from needsLabelPair/needsLabelVector.
// An object is circular if it is reachable from itself — i.e., it appears on the
// DFS recursion stack when revisited. This uses gray/black DFS coloring.
func (w *SchemeWriter) filterToCircular(v Value) {
	circularPairs := make(map[*Pair]bool)
	circularVectors := make(map[*Vector]bool)
	onStackPairs := make(map[*Pair]bool)
	onStackVectors := make(map[*Vector]bool)
	visitedPairs := make(map[*Pair]bool)
	visitedVectors := make(map[*Vector]bool)

	var walk func(v Value)
	walk = func(v Value) {
		switch val := v.(type) {
		case *Pair:
			if val == nil || val.IsEmptyList() {
				return
			}
			if onStackPairs[val] {
				// Found a cycle — this object is circular
				circularPairs[val] = true
				return
			}
			if visitedPairs[val] {
				return
			}
			visitedPairs[val] = true
			onStackPairs[val] = true
			walk(val.Car())
			walk(val.Cdr())
			delete(onStackPairs, val)

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
		}
	}

	walk(v)

	w.needsLabelPair = circularPairs
	w.needsLabelVector = circularVectors
}

// write outputs a value, handling cycles with datum labels.
func (w *SchemeWriter) write(sb *strings.Builder, v Value) {
	switch val := v.(type) {
	case *Pair:
		w.writePair(sb, val)
	case *Vector:
		w.writeVector(sb, val)
	case *String:
		// In display mode, strings are printed without quotes
		if w.displayMode {
			sb.WriteString(val.Value)
		} else {
			sb.WriteString(val.SchemeString())
		}
	case *Character:
		// In display mode, characters are printed as-is without #\
		if w.displayMode {
			sb.WriteRune(val.Value)
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
func (w *SchemeWriter) writePair(sb *strings.Builder, p *Pair) {
	if p == nil {
		sb.WriteString("#<void>")
		return
	}
	if p.IsEmptyList() {
		sb.WriteString("()")
		return
	}

	// Check if this is a back-reference
	if label, found := w.seenPairs[p]; found && label >= 0 {
		// Already labeled and written - output reference
		fmt.Fprintf(sb, "#%d#", label)
		return
	}

	// Check if this needs a label
	if w.needsLabelPair[p] {
		label := w.nextLabel
		w.nextLabel++
		w.seenPairs[p] = label
		fmt.Fprintf(sb, "#%d=", label)
	}

	// WriteByte the pair content
	sb.WriteString("(")
	w.writePairContents(sb, p)
	sb.WriteString(")")
}

// writePairContents writes the contents of a list (without outer parens).
func (w *SchemeWriter) writePairContents(sb *strings.Builder, p *Pair) {
	first := true
	curr := p

	for curr != nil && !curr.IsEmptyList() {
		if !first {
			sb.WriteString(" ")
		}
		first = false

		// WriteByte car
		w.write(sb, curr.Car())

		// Check cdr
		cdr := curr.Cdr()
		if cdr == nil || IsEmptyList(cdr) {
			break
		}

		// Check if cdr is a pair
		nextPair, ok := cdr.(*Pair)
		if !ok {
			// Improper list
			sb.WriteString(" . ")
			w.write(sb, cdr)
			break
		}

		// Check if the cdr pair needs special handling (shared/circular)
		if label, found := w.seenPairs[nextPair]; found && label >= 0 {
			// Back-reference in cdr position
			sb.WriteString(" . ")
			fmt.Fprintf(sb, "#%d#", label)
			break
		}

		if w.needsLabelPair[nextPair] {
			// The cdr needs its own label - write as dotted pair
			sb.WriteString(" . ")
			w.writePair(sb, nextPair)
			break
		}

		curr = nextPair
	}
}

// writeVector writes a vector with cycle detection.
func (w *SchemeWriter) writeVector(sb *strings.Builder, vec *Vector) {
	if vec == nil {
		sb.WriteString("#()")
		return
	}

	// Check if this is a back-reference
	if label, found := w.seenVectors[vec]; found && label >= 0 {
		fmt.Fprintf(sb, "#%d#", label)
		return
	}

	// Check if this needs a label
	if w.needsLabelVector[vec] {
		label := w.nextLabel
		w.nextLabel++
		w.seenVectors[vec] = label
		fmt.Fprintf(sb, "#%d=", label)
	}

	sb.WriteString("#(")
	for i, elem := range *vec {
		if i > 0 {
			sb.WriteString(" ")
		}
		w.write(sb, elem)
	}
	sb.WriteString(")")
}

// WriteValueToString writes a Scheme value to a string with cycle detection.
// Uses WriteModeWrite: datum labels only for circular references.
// R7RS §6.13.3: write outputs datum labels only for objects that are part of a cycle.
func WriteValueToString(v Value) string {
	w := NewSchemeWriter()
	return w.WriteString(v)
}

// WriteSharedValueToString writes a Scheme value to a string with shared structure detection.
// Uses WriteModeWriteShared: datum labels for all multiply-referenced objects.
// R7RS §6.13.3: write-shared outputs datum labels for all shared structure.
func WriteSharedValueToString(v Value) string {
	w := NewSchemeWriter()
	w.writeMode = WriteModeWriteShared
	return w.WriteString(v)
}

// DisplayValueToString writes a Scheme value to a string for display with cycle detection.
// Unlike WriteValueToString, strings are printed without quotes and characters without #\.
func DisplayValueToString(v Value) string {
	w := NewSchemeWriter()
	w.displayMode = true
	return w.WriteString(v)
}
