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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
)

func newTplWithSources(sources ...*syntax.SourceContext) *machine.NativeTemplate {
	tpl := machine.NewNativeTemplate(0, 0, false)
	for _, src := range sources {
		tpl.AppendInstructionWithSource(src, machine.Instruction{Op: machine.OpPush})
	}
	return tpl
}

func mkSrc(file string, sl, sc, el, ec int) *syntax.SourceContext {
	return &syntax.SourceContext{
		File:  file,
		Start: syntax.NewSourceIndexes(0, sc, sl),
		End:   syntax.NewSourceIndexes(0, ec, el),
	}
}

func TestCollector_TrackEnablesCoverage(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(mkSrc("a.scm", 1, 1, 1, 5))
	col := NewCollector()

	col.Track(tpl)

	c.Assert(tpl.IsCoverageEnabled(), qt.IsTrue)
}

func TestCollector_EntriesEmptyWhenNothingExecuted(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(mkSrc("a.scm", 1, 1, 1, 5))
	col := NewCollector()
	col.Track(tpl)

	entries := col.Entries()

	c.Assert(entries, qt.HasLen, 1)
	c.Assert(entries[0].Count, qt.Equals, 0)
}

func TestCollector_EntriesReflectExecutedPCs(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(
		mkSrc("a.scm", 1, 1, 1, 5),
		mkSrc("a.scm", 1, 7, 1, 11),
	)
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true // first sexpr covered; second not

	entries := col.Entries()

	c.Assert(entries, qt.HasLen, 2)
	c.Assert(entries[0].Count, qt.Equals, 1)
	c.Assert(entries[0].StartCol, qt.Equals, 1)
	c.Assert(entries[1].Count, qt.Equals, 0)
	c.Assert(entries[1].StartCol, qt.Equals, 7)
}

func TestCollector_NilSourceContextSkipped(t *testing.T) {
	c := qt.New(t)
	tpl := machine.NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(machine.Instruction{Op: machine.OpPush}) // no source
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true

	entries := col.Entries()

	c.Assert(entries, qt.HasLen, 0, qt.Commentf("instructions without sources produce no entries"))
}

func TestCollector_MultipleInstructionsSameSource_SingleEntryCountsOnce(t *testing.T) {
	c := qt.New(t)
	src := mkSrc("a.scm", 1, 1, 1, 5)
	tpl := newTplWithSources(src, src, src)
	col := NewCollector()
	col.Track(tpl)
	tpl.Executed()[0] = true
	tpl.Executed()[2] = true

	entries := col.Entries()

	// mode=set: same SourceContext collapses to one entry with count=1
	c.Assert(entries, qt.HasLen, 1)
	c.Assert(entries[0].Count, qt.Equals, 1)
}

func TestCollector_EntriesSortedLexicographically(t *testing.T) {
	c := qt.New(t)
	tpl := newTplWithSources(
		mkSrc("b.scm", 1, 1, 1, 5),
		mkSrc("a.scm", 10, 1, 10, 5),
		mkSrc("a.scm", 2, 1, 2, 5),
	)
	col := NewCollector()
	col.Track(tpl)

	entries := col.Entries()

	c.Assert(entries, qt.HasLen, 3)
	c.Assert(entries[0].File, qt.Equals, "a.scm")
	c.Assert(entries[0].StartLine, qt.Equals, 2)
	c.Assert(entries[1].File, qt.Equals, "a.scm")
	c.Assert(entries[1].StartLine, qt.Equals, 10)
	c.Assert(entries[2].File, qt.Equals, "b.scm")
}
