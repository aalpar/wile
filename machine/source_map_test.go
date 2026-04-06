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
	"math"
	"testing"

	"github.com/aalpar/wile/internal/syntax"

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
	c.Assert(idx, qt.Equals, uint16(0))
}

func TestInternSource_OverflowPanics(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)

	// Fill the source table to capacity (index 0 is the nil sentinel).
	// Valid indices are 0..math.MaxUint16, so we fill through index 65535.
	for i := 1; i <= math.MaxUint16; i++ {
		src := &syntax.SourceContext{
			File:  fmt.Sprintf("file%d.scm", i),
			Start: syntax.NewSourceIndexes(i, 1, 0),
		}
		tpl.internSource(src)
	}

	// The next intern should panic rather than silently wrapping.
	defer func() {
		r := recover()
		if r == nil {
			t.Fatal("expected panic on source table overflow")
		}
	}()
	tpl.internSource(&syntax.SourceContext{File: "overflow.scm"})
	t.Fatal("should not reach here")
}

func TestCopy_PreservesSourceRefs(t *testing.T) {
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
