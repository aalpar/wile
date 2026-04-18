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
	c.Assert(buf.String(), qt.Equals, "mode: set\na.scm:1.1,1.5 1 1\n")
}

func TestWriteGoCover_CountZeroForUncovered(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(mkSrc("a.scm", 2, 1, 2, 10))
	col := NewCollector()
	col.Track(tpl)

	var buf bytes.Buffer
	err := WriteGoCover(&buf, col)

	c.Assert(err, qt.IsNil)
	c.Assert(buf.String(), qt.Equals, "mode: set\na.scm:2.1,2.10 1 0\n")
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
	c.Assert(buf.String(), qt.Equals, "mode: set\nmyapp.scm:1.1,1.5 1 1\n")
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
	c.Assert(buf.String(), qt.Equals, "mode: set\nmyapp.scm:1.1,1.5 1 1\nscheme/base.sld:10.1,10.5 1 1\n")
}
