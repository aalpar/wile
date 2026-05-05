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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
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
