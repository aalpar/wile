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

import (
	"wile/syntax"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestSourceMap_NewSourceMap(t *testing.T) {
	c := qt.New(t)
	sm := NewSourceMap()
	c.Assert(sm, qt.IsNotNil)
	c.Assert(sm.Len(), qt.Equals, 0)
}

func TestSourceMap_Add(t *testing.T) {
	c := qt.New(t)
	sm := NewSourceMap()

	source := &syntax.SourceContext{File: "test.scm"}
	sm.Add(0, 5, source)

	c.Assert(sm.Len(), qt.Equals, 1)
}

func TestSourceMap_Add_EmptyRange(t *testing.T) {
	c := qt.New(t)
	sm := NewSourceMap()

	source := &syntax.SourceContext{File: "test.scm"}
	sm.Add(5, 5, source) // Empty range (start == end)
	sm.Add(10, 5, source) // Invalid range (start > end)

	c.Assert(sm.Len(), qt.Equals, 0)
}

func TestSourceMap_Add_Merge(t *testing.T) {
	c := qt.New(t)
	sm := NewSourceMap()

	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(1, 1, 0),
	}

	// Add consecutive ranges with same source - should merge
	sm.Add(0, 5, source)
	sm.Add(5, 10, source)

	c.Assert(sm.Len(), qt.Equals, 1)

	// Verify merged range covers both
	c.Assert(sm.Lookup(0), qt.IsNotNil)
	c.Assert(sm.Lookup(4), qt.IsNotNil)
	c.Assert(sm.Lookup(5), qt.IsNotNil)
	c.Assert(sm.Lookup(9), qt.IsNotNil)
}

func TestSourceMap_Add_NoMerge_DifferentSource(t *testing.T) {
	c := qt.New(t)
	sm := NewSourceMap()

	source1 := &syntax.SourceContext{
		File:  "test1.scm",
		Start: syntax.NewSourceIndexes(1, 1, 0),
	}
	source2 := &syntax.SourceContext{
		File:  "test2.scm",
		Start: syntax.NewSourceIndexes(1, 1, 0),
	}

	sm.Add(0, 5, source1)
	sm.Add(5, 10, source2)

	c.Assert(sm.Len(), qt.Equals, 2)
}

func TestSourceMap_Lookup(t *testing.T) {
	c := qt.New(t)
	sm := NewSourceMap()

	source1 := &syntax.SourceContext{File: "test1.scm"}
	source2 := &syntax.SourceContext{File: "test2.scm"}

	sm.Add(0, 10, source1)
	sm.Add(10, 20, source2)

	// Test lookups within ranges
	c.Assert(sm.Lookup(0).File, qt.Equals, "test1.scm")
	c.Assert(sm.Lookup(5).File, qt.Equals, "test1.scm")
	c.Assert(sm.Lookup(9).File, qt.Equals, "test1.scm")
	c.Assert(sm.Lookup(10).File, qt.Equals, "test2.scm")
	c.Assert(sm.Lookup(15).File, qt.Equals, "test2.scm")
	c.Assert(sm.Lookup(19).File, qt.Equals, "test2.scm")

	// Test lookups outside ranges
	c.Assert(sm.Lookup(20), qt.IsNil)
	c.Assert(sm.Lookup(100), qt.IsNil)
}

func TestSourceMap_Lookup_Empty(t *testing.T) {
	c := qt.New(t)
	sm := NewSourceMap()

	c.Assert(sm.Lookup(0), qt.IsNil)
	c.Assert(sm.Lookup(100), qt.IsNil)
}

func TestSourceMap_Lookup_NilSourceMap(t *testing.T) {
	c := qt.New(t)
	var sm *SourceMap

	c.Assert(sm.Lookup(0), qt.IsNil)
}

func TestSourceMap_Lookup_BinarySearch(t *testing.T) {
	c := qt.New(t)
	sm := NewSourceMap()

	// Add many non-contiguous ranges to test binary search
	for i := 0; i < 100; i++ {
		source := &syntax.SourceContext{File: "test.scm"}
		sm.Add(i*10, i*10+5, source)
	}

	// Test lookups at various positions
	c.Assert(sm.Lookup(0), qt.IsNotNil)
	c.Assert(sm.Lookup(4), qt.IsNotNil)
	c.Assert(sm.Lookup(5), qt.IsNil)  // Gap
	c.Assert(sm.Lookup(10), qt.IsNotNil)
	c.Assert(sm.Lookup(500), qt.IsNotNil)
	c.Assert(sm.Lookup(505), qt.IsNil) // Gap
	c.Assert(sm.Lookup(990), qt.IsNotNil)
	c.Assert(sm.Lookup(995), qt.IsNil) // After last range
}

func TestSourceMap_Len_Nil(t *testing.T) {
	c := qt.New(t)
	var sm *SourceMap

	c.Assert(sm.Len(), qt.Equals, 0)
}

func TestSourceEqual(t *testing.T) {
	c := qt.New(t)

	// Both nil
	c.Assert(sourceEqual(nil, nil), qt.IsTrue)

	// One nil
	source := &syntax.SourceContext{File: "test.scm"}
	c.Assert(sourceEqual(nil, source), qt.IsFalse)
	c.Assert(sourceEqual(source, nil), qt.IsFalse)

	// Same source
	source1 := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(1, 1, 0),
	}
	source2 := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(1, 1, 0),
	}
	c.Assert(sourceEqual(source1, source2), qt.IsTrue)

	// Different file
	source3 := &syntax.SourceContext{
		File:  "other.scm",
		Start: syntax.NewSourceIndexes(1, 1, 0),
	}
	c.Assert(sourceEqual(source1, source3), qt.IsFalse)

	// Different line
	source4 := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(2, 1, 10),
	}
	c.Assert(sourceEqual(source1, source4), qt.IsFalse)

	// Different column
	source5 := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(1, 5, 4),
	}
	c.Assert(sourceEqual(source1, source5), qt.IsFalse)
}
