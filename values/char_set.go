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
	"iter"
	"slices"

	"github.com/aalpar/wile/werr"
)

var _ Value = (*CharSet)(nil)

// MaxCodepoint is the largest valid Unicode codepoint (U+10FFFF).
const MaxCodepoint rune = 0x10FFFF

// CharSet is an immutable set of Unicode codepoints stored as a sorted
// inversion list of disjoint, non-adjacent ranges. SRFI-14 char-set type.
//
// Canonical form invariants (enforced by every constructor):
//  1. Sorted: ranges[i].Lo > ranges[i-1].Hi
//  2. Disjoint and non-adjacent: ranges[i].Lo > ranges[i-1].Hi + 1
//  3. Non-empty: Lo <= Hi
//  4. Codepoint-valid: 0 <= Lo, Hi <= MaxCodepoint
type CharSet struct {
	// ranges holds canonical inversion-list form.
	//
	// INVARIANT: never mutated after construction. All() and Codepoints()
	// yield directly from this slice without defensive copy; the
	// process-global namedCharSets cache and eq? identity rely on this.
	// If you find yourself adding a Mutate/AddRange/SetRanges method,
	// stop — return a new *CharSet via NewCharSetFromUnsortedRanges instead.
	ranges []CharSetRange
}

// CharSetRange is an inclusive-endpoint codepoint range.
type CharSetRange struct {
	Lo, Hi rune
}

// NewCharSetFromRanges constructs a CharSet from an already-canonical range
// slice. The caller asserts the slice is sorted, disjoint, non-adjacent, and
// codepoint-valid. Used internally by primitives that produce canonical output
// (set-algebra ops). External callers should prefer NewCharSetFromUnsortedRanges.
//
// Panics on invariant violation — this is an internal contract assertion,
// wrapped per CLAUDE.md "NEVER panic with raw errors" imperative.
func NewCharSetFromRanges(rs []CharSetRange) *CharSet {
	if len(rs) == 0 {
		return &CharSet{}
	}
	for i, r := range rs {
		if r.Lo < 0 {
			panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"NewCharSetFromRanges: range[%d].Lo=%d < 0", i, r.Lo))
		}
		if r.Hi > MaxCodepoint {
			panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"NewCharSetFromRanges: range[%d].Hi=%d > MaxCodepoint (%d)", i, r.Hi, MaxCodepoint))
		}
		if r.Lo > r.Hi {
			panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"NewCharSetFromRanges: range[%d]: Lo=%d > Hi=%d", i, r.Lo, r.Hi))
		}
		if i > 0 && r.Lo <= rs[i-1].Hi+1 {
			panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"NewCharSetFromRanges: range[%d] (Lo=%d) not strictly after range[%d] (Hi=%d): ranges overlap or are adjacent",
				i, r.Lo, i-1, rs[i-1].Hi))
		}
	}
	out := make([]CharSetRange, len(rs))
	copy(out, rs)
	return &CharSet{ranges: out}
}

// NewCharSetFromUnsortedRanges constructs a CharSet from arbitrary range input.
// Invalid ranges (Lo > Hi or out-of-bounds) are dropped. Overlapping and
// adjacent ranges are merged. Result is in canonical form.
func NewCharSetFromUnsortedRanges(rs []CharSetRange) *CharSet {
	if len(rs) == 0 {
		return &CharSet{}
	}

	// Validate and copy.
	clean := make([]CharSetRange, 0, len(rs))
	for _, r := range rs {
		if r.Lo > r.Hi || r.Lo < 0 || r.Hi > MaxCodepoint {
			continue
		}
		clean = append(clean, r)
	}
	if len(clean) == 0 {
		return &CharSet{}
	}

	// Sort by Lo.
	slices.SortFunc(clean, func(a, b CharSetRange) int {
		return int(a.Lo - b.Lo)
	})

	// Merge adjacent and overlapping.
	out := []CharSetRange{clean[0]}
	for _, r := range clean[1:] {
		last := &out[len(out)-1]
		if r.Lo <= last.Hi+1 {
			if r.Hi > last.Hi {
				last.Hi = r.Hi
			}
		} else {
			out = append(out, r)
		}
	}
	return &CharSet{ranges: out}
}

// NewCharSetFromRunes builds a CharSet from a slice of codepoints (no
// canonicalization assumption). Each rune becomes a unit range, then
// canonicalized via NewCharSetFromUnsortedRanges.
func NewCharSetFromRunes(runes []rune) *CharSet {
	rs := make([]CharSetRange, len(runes))
	for i, r := range runes {
		rs[i] = CharSetRange{Lo: r, Hi: r}
	}
	return NewCharSetFromUnsortedRanges(rs)
}

// Ranges returns a copy of the canonical range slice. Caller may mutate
// the returned slice without affecting the CharSet.
//
// Most read-only iteration callers should prefer All or Codepoints —
// those avoid the O(n) defensive slice copy this method performs (they
// allocate a single iterator closure instead, which is cheaper for any
// non-empty CharSet). Ranges is retained for callers that genuinely
// need a slice: dual-cursor merge algorithms (intersect, difference)
// and the union builder that uses append on the result.
func (p *CharSet) Ranges() []CharSetRange {
	if p == nil || len(p.ranges) == 0 {
		return nil
	}
	out := make([]CharSetRange, len(p.ranges))
	copy(out, p.ranges)
	return out
}

// All returns an iter.Seq that yields each canonical range in codepoint
// ascending order. Caller breaks the loop with `break` to early-exit.
//
// Cost: one closure allocation per accessor call (the iterator captures
// p). No O(n) slice copy — yields directly from the internal slice, which
// is safe because *CharSet is immutable. Strictly cheaper than Ranges()
// for any non-empty CharSet.
//
// Naming follows Go stdlib convention (slices.All, maps.All).
func (p *CharSet) All() iter.Seq[CharSetRange] {
	return func(yield func(CharSetRange) bool) {
		if p == nil {
			return
		}
		for _, r := range p.ranges {
			if !yield(r) {
				return
			}
		}
	}
}

// Codepoints returns an iter.Seq that yields every codepoint in the set,
// in codepoint ascending order. Caller breaks the loop with `break` to
// early-exit.
func (p *CharSet) Codepoints() iter.Seq[rune] {
	return func(yield func(rune) bool) {
		if p == nil {
			return
		}
		for _, r := range p.ranges {
			for c := r.Lo; c <= r.Hi; c++ {
				if !yield(c) {
					return
				}
			}
		}
	}
}

// Size returns the total number of codepoints in the set.
func (p *CharSet) Size() int {
	if p == nil {
		return 0
	}
	n := 0
	for _, r := range p.ranges {
		n += int(r.Hi - r.Lo + 1)
	}
	return n
}

// Contains reports whether the given codepoint is in the set, via binary
// search over the inversion list.
func (p *CharSet) Contains(ch rune) bool {
	if p == nil {
		return false
	}
	rs := p.ranges
	lo, hi := 0, len(rs)
	for lo < hi {
		mid := (lo + hi) / 2
		switch {
		case ch < rs[mid].Lo:
			hi = mid
		case ch > rs[mid].Hi:
			lo = mid + 1
		default:
			return true
		}
	}
	return false
}

// SchemeString implements Value (R7RS §6.13.3 write).
func (p *CharSet) SchemeString() string {
	if p == nil {
		return "#<char-set:void>"
	}
	return fmt.Sprintf("#<char-set: %d ranges, %d chars>", len(p.ranges), p.Size())
}

// IsVoid implements Value.
func (p *CharSet) IsVoid() bool {
	return p == nil
}

// EqualTo implements Value (R7RS §6.1 equal?).
//
// Two char-sets are equal iff their canonical range slices are equal — the
// invariants of canonical form make logical equality and structural equality
// the same thing.
func (p *CharSet) EqualTo(v Value) bool {
	other, ok := v.(*CharSet)
	if !ok {
		return false
	}
	if p == nil && other == nil {
		return true
	}
	if p == nil || other == nil {
		return false
	}
	if p == other {
		return true
	}
	return slices.Equal(p.ranges, other.ranges)
}
