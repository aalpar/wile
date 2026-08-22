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

package machine

import (
	"fmt"
	"testing"

	"github.com/aalpar/wile/pkg/syntax"

	qt "github.com/frankban/quicktest"
)

func TestSourceAt_FreshTemplate(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)

	// Fresh template has no operations, so any PC returns nil
	c.Assert(tpl.SourceAt(0), qt.IsNil)
	c.Assert(tpl.SourceAt(100), qt.IsNil)
	c.Assert(tpl.SourceAt(-1), qt.IsNil)
}

func TestSourceAt_WithSource(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)

	source := &syntax.SourceContext{File: "test.scm"}
	tpl.AppendOperationsWithSource(source,
		NewOperationLoadVoid(),
		NewOperationLoadVoid(),
	)

	c.Assert(tpl.SourceAt(0), qt.IsNotNil)
	c.Assert(tpl.SourceAt(0).File, qt.Equals, "test.scm")
	c.Assert(tpl.SourceAt(1), qt.IsNotNil)
	c.Assert(tpl.SourceAt(1).File, qt.Equals, "test.scm")
	c.Assert(tpl.SourceAt(2), qt.IsNil) // Out of bounds
}

func TestSourceAt_NilSource(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)

	// AppendOperations without source (nil)
	tpl.AppendOperations(NewOperationLoadVoid())

	c.Assert(tpl.SourceAt(0), qt.IsNil) // index 0 in sourceTable is nil
}

func TestSourceAt_MixedSources(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)

	source1 := &syntax.SourceContext{File: "file1.scm"}
	source2 := &syntax.SourceContext{File: "file2.scm"}

	tpl.AppendOperationsWithSource(source1, NewOperationLoadVoid())
	tpl.AppendOperationsWithSource(source2, NewOperationLoadVoid())
	tpl.AppendOperations(NewOperationLoadVoid()) // nil source

	c.Assert(tpl.SourceAt(0).File, qt.Equals, "file1.scm")
	c.Assert(tpl.SourceAt(1).File, qt.Equals, "file2.scm")
	c.Assert(tpl.SourceAt(2), qt.IsNil)
}

func TestInternSource_Deduplication(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)

	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(1, 1, 0),
	}

	// Same pointer should return same index
	idx1 := tpl.internSource(source)
	idx2 := tpl.internSource(source)
	c.Assert(idx1, qt.Equals, idx2)

	// Different pointer, same content should also deduplicate
	source2 := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(1, 1, 0),
	}
	idx3 := tpl.internSource(source2)
	c.Assert(idx1, qt.Equals, idx3)

	// Different content should get different index
	source3 := &syntax.SourceContext{
		File:  "other.scm",
		Start: syntax.NewSourceIndexes(1, 1, 0),
	}
	idx4 := tpl.internSource(source3)
	c.Assert(idx4, qt.Not(qt.Equals), idx1)
}

func TestInternSource_Nil(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)

	idx := tpl.internSource(nil)
	c.Assert(idx, qt.Equals, uint32(0))
}

func TestInternSource_LargeIndex(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)

	// Verify that source indices above MaxUint16 work correctly now
	// that sourceTableRefs uses uint32. Fill beyond the old uint16 limit.
	const count = 70000
	for i := 1; i <= count; i++ {
		src := &syntax.SourceContext{
			File:  fmt.Sprintf("file%d.scm", i),
			Start: syntax.NewSourceIndexes(i, 1, 0),
		}
		tpl.internSource(src)
	}

	// sourceTable should have count+1 entries (index 0 = nil sentinel).
	if len(tpl.sourceTable) != count+1 {
		t.Fatalf("expected %d entries, got %d", count+1, len(tpl.sourceTable))
	}
}

// TestInternSource_DedupSurvivesCopy pins the one hazard the hash index adds
// over the linear scan it replaced.
//
// Copy clones sourceTable and NOT sourceIndex, so a copied template starts with
// a table and no index. If the rebuild were skipped, interning a context the
// copy already holds would append a duplicate entry rather than finding it —
// silently, since SourceAt would still report the right location. The table
// length is what catches it.
func TestInternSource_DedupSurvivesCopy(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)

	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(11, 7, 4),
	}
	tpl.AppendOperationsWithSource(source, NewOperationLoadVoid())
	before := len(tpl.sourceTable)

	copied := tpl.Copy()
	c.Assert(len(copied.sourceTable), qt.Equals, before)

	// A DISTINCT pointer with the same location: sourceEqual's relation, which is
	// what the index must key on.
	equal := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(11, 7, 4),
	}
	c.Assert(copied.internSource(equal), qt.Equals, tpl.internSource(source))
	c.Assert(len(copied.sourceTable), qt.Equals, before)

	// And a genuinely new location still extends it.
	other := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(11, 7, 5),
	}
	copied.internSource(other)
	c.Assert(len(copied.sourceTable), qt.Equals, before+1)
}

// TestSourceKeyMatchesSourceEqual pins that the map key and the predicate decide
// the same relation. They are two spellings of one rule, and a field added to
// one without the other would make interning silently coarser or finer.
func TestSourceKeyMatchesSourceEqual(t *testing.T) {
	c := qt.New(t)

	base := &syntax.SourceContext{
		Text:  "original text",
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(11, 7, 4),
		End:   syntax.NewSourceIndexes(24, 20, 4),
	}
	tcs := []struct {
		name  string
		other *syntax.SourceContext
	}{
		{
			// Text, End and the absolute byte INDEX all differ; file, line and
			// column do not. sourceEqual is line/column-only, so these ARE the
			// same source — which is also what pins index out of the relation.
			name: "differing text, end and byte index are still equal",
			other: &syntax.SourceContext{
				Text:  "rewritten by a macro",
				File:  "test.scm",
				Start: syntax.NewSourceIndexes(99, 7, 4),
				End:   syntax.NewSourceIndexes(99, 9, 9),
			},
		},
		{name: "differing file", other: &syntax.SourceContext{
			File: "other.scm", Start: syntax.NewSourceIndexes(11, 7, 4)}},
		{name: "differing line", other: &syntax.SourceContext{
			File: "test.scm", Start: syntax.NewSourceIndexes(11, 7, 5)}},
		{name: "differing column", other: &syntax.SourceContext{
			File: "test.scm", Start: syntax.NewSourceIndexes(11, 8, 4)}},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(newSourceKey(base) == newSourceKey(tc.other),
				qt.Equals, sourceEqual(base, tc.other))
		})
	}
}

func TestCopy_PreservesSourceTableRefs(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)

	source := &syntax.SourceContext{File: "test.scm"}
	tpl.AppendOperationsWithSource(source, NewOperationLoadVoid())
	tpl.AppendOperations(NewOperationLoadVoid())

	copied := tpl.Copy()
	c.Assert(copied.SourceAt(0), qt.IsNotNil)
	c.Assert(copied.SourceAt(0).File, qt.Equals, "test.scm")
	c.Assert(copied.SourceAt(1), qt.IsNil)
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
