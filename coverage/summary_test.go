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
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestWriteSummary_PerLineRollup(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(
		mkSrc("a.scm", 1, 1, 1, 5),
		mkSrc("a.scm", 1, 7, 1, 11),
		mkSrc("a.scm", 1, 13, 1, 17),
	)
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true
	tpl.Executed()[1] = true
	// third sexpr not executed

	var buf bytes.Buffer
	err := WriteSummary(&buf, col)

	c.Assert(err, qt.IsNil)
	out := buf.String()
	c.Assert(strings.Contains(out, "a.scm:1"), qt.IsTrue, qt.Commentf("got: %s", out))
	c.Assert(strings.Contains(out, "2/3"), qt.IsTrue, qt.Commentf("got: %s", out))
	c.Assert(strings.Contains(out, "max_col_reached=7"), qt.IsTrue, qt.Commentf("got: %s", out))
}

func TestWriteSummary_TotalFooter(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(
		mkSrc("a.scm", 1, 1, 1, 5),
		mkSrc("a.scm", 1, 7, 1, 11),
		mkSrc("a.scm", 2, 1, 2, 5),
	)
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true
	tpl.Executed()[1] = true
	tpl.Executed()[2] = true

	var buf bytes.Buffer
	err := WriteSummary(&buf, col)

	c.Assert(err, qt.IsNil)
	out := buf.String()
	c.Assert(strings.Contains(out, "TOTAL"), qt.IsTrue, qt.Commentf("got: %s", out))
	c.Assert(strings.Contains(out, "3/3"), qt.IsTrue, qt.Commentf("got: %s", out))
}

func TestWriteSummary_ExcludesStdlib(t *testing.T) {
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
	err := WriteSummary(&buf, col)

	c.Assert(err, qt.IsNil)
	out := buf.String()
	c.Assert(strings.Contains(out, "myapp.scm"), qt.IsTrue)
	c.Assert(strings.Contains(out, "scheme/base.sld"), qt.IsFalse)
}
