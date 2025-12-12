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

package machine

import "wile/syntax"

// SourceMapEntry maps a range of bytecode PCs to a source location.
// Uses range-based encoding for efficiency (most operations from same source).
type SourceMapEntry struct {
	StartPC int                   // First PC covered by this entry
	EndPC   int                   // Last PC covered (exclusive)
	Source  *syntax.SourceContext // Source location
}

// SourceMap provides PC → source location lookup.
type SourceMap struct {
	entries []SourceMapEntry
}

// NewSourceMap creates an empty source map.
func NewSourceMap() *SourceMap {
	return &SourceMap{entries: make([]SourceMapEntry, 0, 16)}
}

// Add records a source location for a PC range.
// Merges with previous entry if source is the same.
func (sm *SourceMap) Add(startPC, endPC int, source *syntax.SourceContext) {
	if startPC >= endPC {
		return // Empty range
	}
	if len(sm.entries) > 0 {
		last := &sm.entries[len(sm.entries)-1]
		if last.EndPC == startPC && sourceEqual(last.Source, source) {
			// Extend previous entry
			last.EndPC = endPC
			return
		}
	}
	sm.entries = append(sm.entries, SourceMapEntry{
		StartPC: startPC,
		EndPC:   endPC,
		Source:  source,
	})
}

// Lookup finds the source location for a given PC.
// Returns nil if no mapping exists.
func (sm *SourceMap) Lookup(pc int) *syntax.SourceContext {
	if sm == nil || len(sm.entries) == 0 {
		return nil
	}
	// Binary search for efficiency
	lo, hi := 0, len(sm.entries)
	for lo < hi {
		mid := (lo + hi) / 2
		entry := &sm.entries[mid]
		if pc < entry.StartPC {
			hi = mid
		} else if pc >= entry.EndPC {
			lo = mid + 1
		} else {
			return entry.Source
		}
	}
	return nil
}

// Len returns the number of entries in the source map.
func (sm *SourceMap) Len() int {
	if sm == nil {
		return 0
	}
	return len(sm.entries)
}

// sourceEqual compares two source contexts for equality (by location only).
func sourceEqual(a, b *syntax.SourceContext) bool {
	if a == nil || b == nil {
		return a == b
	}
	return a.File == b.File &&
		a.Start.Line() == b.Start.Line() &&
		a.Start.Column() == b.Start.Column()
}
