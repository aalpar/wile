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

package charsets_test

import (
	"math/rand"
	"reflect"
	"slices"
	"testing"
	"testing/quick"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/extensions/charsets"
	"github.com/aalpar/wile/pkg/values"
)

// runeSlice is a quick.Generator-friendly wrapper that produces small
// random rune slices in the BMP for property testing.
type runeSlice []rune

func (runeSlice) Generate(r *rand.Rand, size int) reflect.Value {
	n := r.Intn(50)
	out := make(runeSlice, n)
	for i := range out {
		cp := rune(r.Intn(0x10000)) // BMP only — keeps tests fast
		// Surrogates (U+D800..U+DFFF) are not Unicode scalar values and are
		// never stored in a char-set (constructors strip them), so a storage
		// round-trip property must not generate them; remap into the preceding
		// valid block.
		if cp >= 0xD800 && cp <= 0xDFFF {
			cp -= 0x0800
		}
		out[i] = cp
	}
	return reflect.ValueOf(out)
}

// dedupAndSort returns the sorted, deduplicated codepoints in rs.
func dedupAndSort(rs runeSlice) []rune {
	out := make([]rune, 0, len(rs))
	seen := make(map[rune]struct{}, len(rs))
	for _, r := range rs {
		_, ok := seen[r]
		if !ok {
			seen[r] = struct{}{}
			out = append(out, r)
		}
	}
	slices.Sort(out)
	return out
}

func TestPropertyRoundtrip_listToCharSet(t *testing.T) {
	c := qt.New(t)
	prop := func(rs runeSlice) bool {
		cs := values.NewCharSetFromRunes(rs)
		// Ranges() yields codepoint-ascending sorted, merged, unique ranges.
		// Expanding them must equal the dedup-sorted input runes.
		dedupSorted := dedupAndSort(rs)
		got := make([]rune, 0, cs.Size())
		for _, r := range cs.Ranges() {
			for cp := r.Lo; cp <= r.Hi; cp++ {
				got = append(got, cp)
			}
		}
		return slices.Equal(got, dedupSorted)
	}
	c.Assert(quick.Check(prop, nil), qt.IsNil)
}

func TestPropertyComplementInvolution(t *testing.T) {
	c := qt.New(t)
	prop := func(rs runeSlice) bool {
		cs := values.NewCharSetFromRunes(rs)
		twice := charsets.ExportedComplementOne(charsets.ExportedComplementOne(cs))
		return cs.EqualTo(twice)
	}
	c.Assert(quick.Check(prop, nil), qt.IsNil)
}

func TestPropertyDeMorgan(t *testing.T) {
	c := qt.New(t)
	prop := func(a, b runeSlice) bool {
		csA := values.NewCharSetFromRunes(a)
		csB := values.NewCharSetFromRunes(b)
		// ¬(A ∪ B) = ¬A ∩ ¬B
		left := charsets.ExportedComplementOne(charsets.ExportedUnionTwo(csA, csB))
		right := charsets.ExportedIntersectTwo(
			charsets.ExportedComplementOne(csA),
			charsets.ExportedComplementOne(csB),
		)
		return left.EqualTo(right)
	}
	c.Assert(quick.Check(prop, nil), qt.IsNil)
}

func TestPropertyIdempotency(t *testing.T) {
	c := qt.New(t)

	propUnion := func(rs runeSlice) bool {
		cs := values.NewCharSetFromRunes(rs)
		return charsets.ExportedUnionTwo(cs, cs).EqualTo(cs)
	}
	c.Assert(quick.Check(propUnion, nil), qt.IsNil)

	propIntersect := func(rs runeSlice) bool {
		cs := values.NewCharSetFromRunes(rs)
		return charsets.ExportedIntersectTwo(cs, cs).EqualTo(cs)
	}
	c.Assert(quick.Check(propIntersect, nil), qt.IsNil)
}

func TestPropertyIdentityElement(t *testing.T) {
	c := qt.New(t)
	empty := values.NewCharSetFromRanges(nil)
	full := values.NewCharSetFromRanges([]values.CharSetRange{{Lo: 0, Hi: values.MaxCodepoint}})

	propUnionEmpty := func(rs runeSlice) bool {
		cs := values.NewCharSetFromRunes(rs)
		return charsets.ExportedUnionTwo(cs, empty).EqualTo(cs)
	}
	c.Assert(quick.Check(propUnionEmpty, nil), qt.IsNil)

	propIntersectFull := func(rs runeSlice) bool {
		cs := values.NewCharSetFromRunes(rs)
		return charsets.ExportedIntersectTwo(cs, full).EqualTo(cs)
	}
	c.Assert(quick.Check(propIntersectFull, nil), qt.IsNil)
}

func TestPropertyAnnihilator(t *testing.T) {
	c := qt.New(t)
	empty := values.NewCharSetFromRanges(nil)

	propIntersectEmpty := func(rs runeSlice) bool {
		cs := values.NewCharSetFromRunes(rs)
		return charsets.ExportedIntersectTwo(cs, empty).EqualTo(empty)
	}
	c.Assert(quick.Check(propIntersectEmpty, nil), qt.IsNil)

	propDiffSelf := func(rs runeSlice) bool {
		cs := values.NewCharSetFromRunes(rs)
		diff := charsets.ExportedDifferenceTwo(cs, cs)
		return diff.EqualTo(empty)
	}
	c.Assert(quick.Check(propDiffSelf, nil), qt.IsNil)
}

func TestPropertyDeMorganDual(t *testing.T) {
	c := qt.New(t)
	prop := func(a, b runeSlice) bool {
		csA := values.NewCharSetFromRunes(a)
		csB := values.NewCharSetFromRunes(b)
		// ¬(A ∩ B) = ¬A ∪ ¬B
		left := charsets.ExportedComplementOne(charsets.ExportedIntersectTwo(csA, csB))
		right := charsets.ExportedUnionTwo(
			charsets.ExportedComplementOne(csA),
			charsets.ExportedComplementOne(csB),
		)
		return left.EqualTo(right)
	}
	c.Assert(quick.Check(prop, nil), qt.IsNil)
}
