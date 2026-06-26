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

package values_test

import (
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"
)

func TestCharSet_EmptySet(t *testing.T) {
	c := qt.New(t)
	cs := values.NewCharSetFromRanges(nil)
	c.Assert(cs.Size(), qt.Equals, 0)
	c.Assert(cs.SchemeString(), qt.Equals, "#<char-set: 0 ranges, 0 chars>")
	c.Assert(cs.IsVoid(), qt.Equals, false)
}

func TestCharSet_SingleRange(t *testing.T) {
	c := qt.New(t)
	cs := values.NewCharSetFromRanges([]values.CharSetRange{{Lo: 'a', Hi: 'c'}})
	c.Assert(cs.Size(), qt.Equals, 3)
	c.Assert(cs.Contains('a'), qt.Equals, true)
	c.Assert(cs.Contains('b'), qt.Equals, true)
	c.Assert(cs.Contains('c'), qt.Equals, true)
	c.Assert(cs.Contains('d'), qt.Equals, false)
	c.Assert(cs.Contains('A'), qt.Equals, false)
}

func TestCharSet_AdjacentRangesMerged(t *testing.T) {
	c := qt.New(t)
	// Inputs (5..7) and (8..10) are adjacent; canonical form merges to (5..10).
	cs := values.NewCharSetFromUnsortedRanges([]values.CharSetRange{
		{Lo: 5, Hi: 7}, {Lo: 8, Hi: 10},
	})
	c.Assert(len(cs.Ranges()), qt.Equals, 1)
	c.Assert(cs.Ranges()[0], qt.Equals, values.CharSetRange{Lo: 5, Hi: 10})
}

func TestCharSet_OverlappingRangesMerged(t *testing.T) {
	c := qt.New(t)
	cs := values.NewCharSetFromUnsortedRanges([]values.CharSetRange{
		{Lo: 5, Hi: 10}, {Lo: 8, Hi: 15},
	})
	c.Assert(len(cs.Ranges()), qt.Equals, 1)
	c.Assert(cs.Ranges()[0], qt.Equals, values.CharSetRange{Lo: 5, Hi: 15})
}

func TestCharSet_UnsortedInputSorted(t *testing.T) {
	c := qt.New(t)
	cs := values.NewCharSetFromUnsortedRanges([]values.CharSetRange{
		{Lo: 'x', Hi: 'z'}, {Lo: 'a', Hi: 'c'},
	})
	c.Assert(cs.Ranges(), qt.DeepEquals, []values.CharSetRange{
		{Lo: 'a', Hi: 'c'}, {Lo: 'x', Hi: 'z'},
	})
}

func TestCharSet_EqualTo(t *testing.T) {
	c := qt.New(t)
	cs1 := values.NewCharSetFromUnsortedRanges([]values.CharSetRange{
		{Lo: 5, Hi: 7}, {Lo: 8, Hi: 10},
	})
	cs2 := values.NewCharSetFromRanges([]values.CharSetRange{{Lo: 5, Hi: 10}})
	// Same canonical form, different construction paths.
	c.Assert(cs1.EqualTo(cs2), qt.Equals, true)
	c.Assert(values.EqualTo(cs1, cs2), qt.Equals, true)
}

func TestCharSet_Value_NilHandling(t *testing.T) {
	c := qt.New(t)
	var cs *values.CharSet
	c.Assert(cs.IsVoid(), qt.Equals, true)
}

func TestCharSet_SchemeStringFormat(t *testing.T) {
	c := qt.New(t)
	cs := values.NewCharSetFromRanges([]values.CharSetRange{
		{Lo: 'a', Hi: 'c'}, {Lo: 'x', Hi: 'z'},
	})
	c.Assert(cs.SchemeString(), qt.Equals, "#<char-set: 2 ranges, 6 chars>")
	_ = valuestest.SchemeEquals // keep import live for later tests
}

func TestCharSet_All_VisitOrder(t *testing.T) {
	c := qt.New(t)
	cs := values.NewCharSetFromRanges([]values.CharSetRange{
		{Lo: 'a', Hi: 'c'}, {Lo: 'x', Hi: 'z'},
	})
	var got []values.CharSetRange
	for r := range cs.All() {
		got = append(got, r)
	}
	c.Assert(got, qt.DeepEquals, []values.CharSetRange{
		{Lo: 'a', Hi: 'c'}, {Lo: 'x', Hi: 'z'},
	})
}

func TestCharSet_All_EarlyExit(t *testing.T) {
	c := qt.New(t)
	cs := values.NewCharSetFromRanges([]values.CharSetRange{
		{Lo: '0', Hi: '9'}, {Lo: 'a', Hi: 'c'}, {Lo: 'x', Hi: 'z'},
	})
	count := 0
	for range cs.All() {
		count++
		if count == 1 {
			break
		}
	}
	c.Assert(count, qt.Equals, 1)
}

func TestCharSet_All_NilReceiver(t *testing.T) {
	c := qt.New(t)
	var cs *values.CharSet
	count := 0
	for range cs.All() {
		count++
	}
	c.Assert(count, qt.Equals, 0)
}

func TestCharSet_All_EmptySet(t *testing.T) {
	c := qt.New(t)
	cs := values.NewCharSetFromRanges(nil)
	count := 0
	for range cs.All() {
		count++
	}
	c.Assert(count, qt.Equals, 0)
}

func TestCharSet_Codepoints_VisitOrder(t *testing.T) {
	c := qt.New(t)
	cs := values.NewCharSetFromRanges([]values.CharSetRange{
		{Lo: 'a', Hi: 'c'}, {Lo: 'x', Hi: 'z'},
	})
	var got []rune
	for r := range cs.Codepoints() {
		got = append(got, r)
	}
	c.Assert(got, qt.DeepEquals, []rune{'a', 'b', 'c', 'x', 'y', 'z'})
}

func TestCharSet_Codepoints_EarlyExit(t *testing.T) {
	c := qt.New(t)
	cs := values.NewCharSetFromRanges([]values.CharSetRange{
		{Lo: 'a', Hi: 'z'},
	})
	var got []rune
	for r := range cs.Codepoints() {
		got = append(got, r)
		if len(got) == 3 {
			break
		}
	}
	c.Assert(got, qt.DeepEquals, []rune{'a', 'b', 'c'})
}

func TestCharSet_Codepoints_NilReceiver(t *testing.T) {
	c := qt.New(t)
	var cs *values.CharSet
	count := 0
	for range cs.Codepoints() {
		count++
	}
	c.Assert(count, qt.Equals, 0)
}

func TestCharSet_Codepoints_EmptySet(t *testing.T) {
	c := qt.New(t)
	cs := values.NewCharSetFromRanges(nil)
	count := 0
	for range cs.Codepoints() {
		count++
	}
	c.Assert(count, qt.Equals, 0)
}

func TestCharSet_All_Reiterable(t *testing.T) {
	c := qt.New(t)
	cs := values.NewCharSetFromRanges([]values.CharSetRange{
		{Lo: 'a', Hi: 'c'}, {Lo: 'x', Hi: 'z'},
	})
	var first, second []values.CharSetRange
	for r := range cs.All() {
		first = append(first, r)
	}
	for r := range cs.All() {
		second = append(second, r)
	}
	c.Assert(first, qt.DeepEquals, second)
	c.Assert(len(first), qt.Equals, 2)
}

func TestCharSet_Codepoints_Reiterable(t *testing.T) {
	c := qt.New(t)
	cs := values.NewCharSetFromRanges([]values.CharSetRange{{Lo: 'a', Hi: 'c'}})
	var first, second []rune
	for r := range cs.Codepoints() {
		first = append(first, r)
	}
	for r := range cs.Codepoints() {
		second = append(second, r)
	}
	c.Assert(first, qt.DeepEquals, second)
	c.Assert(len(first), qt.Equals, 3)
}

// TestNewCharSetFromRanges_PanicWrapsSentinel pins F7's panic-shape change:
// invariant violations panic with a project error type so a deferred
// recover() can match via errors.Is.
func TestNewCharSetFromRanges_PanicWrapsSentinel(t *testing.T) {
	tcs := []struct {
		name   string
		ranges []values.CharSetRange
	}{
		{"Lo<0", []values.CharSetRange{{Lo: -1, Hi: 5}}},
		{"Hi>MaxCodepoint", []values.CharSetRange{{Lo: 0, Hi: values.MaxCodepoint + 1}}},
		{"Lo>Hi", []values.CharSetRange{{Lo: 'z', Hi: 'a'}}},
		{"adjacent ranges", []values.CharSetRange{{Lo: 'a', Hi: 'c'}, {Lo: 'd', Hi: 'f'}}},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			defer func() {
				r := recover()
				c.Assert(r, qt.IsNotNil)
				err, ok := r.(error)
				c.Assert(ok, qt.IsTrue, qt.Commentf("panic value should be an error type, got %T", r))
				c.Assert(errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue,
					qt.Commentf("panic should wrap ErrInvalidArgument, got %v", err))
			}()
			values.NewCharSetFromRanges(tc.ranges)
		})
	}
}

// TestCharSet_ExcludesSurrogates verifies the no-surrogate invariant: every
// constructor strips the UTF-16 surrogate block (U+D800..U+DFFF), splitting any
// straddling range. Surrogates are not Unicode scalar values, so a char-set must
// never contain them — otherwise iteration via integer->char crashes (SRFI-14 6D).
func TestCharSet_ExcludesSurrogates(t *testing.T) {
	c := qt.New(t)

	contains := func(cs *values.CharSet, cp rune) bool {
		for _, r := range cs.Ranges() {
			if cp >= r.Lo && cp <= r.Hi {
				return true
			}
		}
		return false
	}

	// A range straddling the surrogate block is split into below/above parts.
	straddle := values.NewCharSetFromRanges([]values.CharSetRange{{Lo: 0xD7FE, Hi: 0xE002}})
	c.Assert(contains(straddle, 0xD800), qt.IsFalse)
	c.Assert(contains(straddle, 0xDFFF), qt.IsFalse)
	c.Assert(contains(straddle, 0xD7FF), qt.IsTrue)
	c.Assert(contains(straddle, 0xE000), qt.IsTrue)

	// A range entirely within the block collapses to empty.
	allSurrogate := values.NewCharSetFromUnsortedRanges([]values.CharSetRange{{Lo: 0xD900, Hi: 0xDA00}})
	c.Assert(len(allSurrogate.Ranges()), qt.Equals, 0)

	// "Full" excludes the 2048 surrogate code points.
	full := values.NewCharSetFromRanges([]values.CharSetRange{{Lo: 0, Hi: values.MaxCodepoint}})
	c.Assert(full.Size(), qt.Equals, int(values.MaxCodepoint)+1-2048)
	c.Assert(contains(full, 0xD800), qt.IsFalse)
	c.Assert(contains(full, 0x10FFFF), qt.IsTrue)

	// Surrogate-free input is returned unchanged (no spurious splitting).
	ascii := values.NewCharSetFromRanges([]values.CharSetRange{{Lo: 'a', Hi: 'z'}})
	c.Assert(len(ascii.Ranges()), qt.Equals, 1)
}
