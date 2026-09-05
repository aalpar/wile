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

package coverage

import (
	"bytes"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestWriteGoCover_HeaderModeSet(t *testing.T) {
	c := qt.New(t)
	col := NewCollector()
	var buf bytes.Buffer

	err := WriteGoCover(&buf, col)

	c.Assert(err, qt.IsNil)
	c.Assert(buf.String(), qt.Equals, "mode: set\n")
}

func TestWriteGoCover_OneEntry(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(mkSrc("a.scm", 1, 1, 1, 5))
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true

	var buf bytes.Buffer
	err := WriteGoCover(&buf, col)

	c.Assert(err, qt.IsNil)
	c.Assert(buf.String(), qt.Equals, "mode: set\na.scm:1.2,1.6 1 1\n")
}

func TestWriteGoCover_CountZeroForUncovered(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(mkSrc("a.scm", 2, 1, 2, 10))
	col := NewCollector()
	col.Track(tpl)

	var buf bytes.Buffer
	err := WriteGoCover(&buf, col)

	c.Assert(err, qt.IsNil)
	c.Assert(buf.String(), qt.Equals, "mode: set\na.scm:2.2,2.11 1 0\n")
}

func TestWriteGoCover_ExcludesStdlibByDefault(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(
		mkSrc("myapp.scm", 1, 1, 1, 5),
		mkSrc("scheme/base.sld", 10, 1, 10, 5),
	)
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true
	tpl.Executed()[1] = true

	var buf bytes.Buffer
	err := WriteGoCover(&buf, col)

	c.Assert(err, qt.IsNil)
	c.Assert(buf.String(), qt.Equals, "mode: set\nmyapp.scm:1.2,1.6 1 1\n")
}

func TestWriteGoCover_IncludeStdlibWhenRequested(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(
		mkSrc("myapp.scm", 1, 1, 1, 5),
		mkSrc("scheme/base.sld", 10, 1, 10, 5),
	)
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true
	tpl.Executed()[1] = true

	var buf bytes.Buffer
	err := WriteGoCoverIncludingStdlib(&buf, col)

	c.Assert(err, qt.IsNil)
	c.Assert(buf.String(), qt.Equals, "mode: set\nmyapp.scm:1.2,1.6 1 1\nscheme/base.sld:10.2,10.6 1 1\n")
}

// TestWriteGoCover_ColumnsAreOneBased pins the column convention at the
// export boundary. SourceIndexes columns are 0-based (the tokenizer's
// convention, shared by every diagnostic); Go cover profiles are 1-based.
// A profile that copies the 0-based value through renders every span one
// character to the left in `go tool cover -html`.
func TestWriteGoCover_ColumnsAreOneBased(t *testing.T) {
	c := qt.New(t)
	// `define` inside "(define x 1)": 0-based half-open [1, 7).
	tpl := newTplWithSources(mkSrc("a.scm", 1, 1, 1, 7))
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true

	var buf bytes.Buffer
	err := WriteGoCover(&buf, col)

	c.Assert(err, qt.IsNil)
	c.Assert(buf.String(), qt.Equals, "mode: set\na.scm:1.2,1.8 1 1\n")
}
