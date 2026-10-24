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
	"io/fs"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
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

// TestEveryEmbeddedStdlibPathIsExcluded pins the exclusion set to the embedded
// stdlib itself: a file the embed FS carries is stdlib, whatever its first
// path element and whichever root names it (`srfi/1.sld` from stdlib.FS,
// `lib/srfi/1.sld` from stdlib.LibFS), so a plain --cover report never lists it.
//
// The walk repeats embeddedStdlibPaths on purpose: it is a mirror oracle, and a
// test that iterated the production set would check the set against itself.
func TestEveryEmbeddedStdlibPathIsExcluded(t *testing.T) {
	c := qt.New(t)
	var unexcluded []string
	for _, root := range []fs.FS{stdlib.FS, stdlib.LibFS} {
		err := fs.WalkDir(root, ".", func(name string, d fs.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if d.IsDir() {
				return nil
			}
			if !isStdlibPath(name) {
				unexcluded = append(unexcluded, name)
			}
			return nil
		})
		c.Assert(err, qt.IsNil)
	}
	c.Assert(unexcluded, qt.HasLen, 0, qt.Commentf("embedded stdlib paths a plain --cover report would list: %v", unexcluded))
}

// TestIsStdlibPathIsExact pins the match as an exact name: no suffix, prefix,
// case fold, directory or path-form variation of an embedded name is stdlib.
func TestIsStdlibPathIsExact(t *testing.T) {
	tests := []struct {
		name string
		path string
		want bool
	}{
		{"engine-rooted name", "scheme/base.sld", true},
		{"embedder-rooted name", "lib/scheme/base.sld", true},
		{"chibi", "chibi/test.sld", true},
		{"rnrs", "rnrs/hashtables.sld", true},
		{"backup suffix", "scheme/base.sld.bak", false},
		{"truncated extension", "srfi/1", false},
		{"upper case", "SCHEME/BASE.SLD", false},
		{"vendored under a user directory", "vendor/scheme/base.sld", false},
		{"other directory, same base", "other/base.sld", false},
		{"dot-slash form", "./scheme/base.sld", false},
		{"directory name", "scheme", false},
		{"directory with slash", "scheme/", false},
		{"empty", "", false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(isStdlibPath(tt.path), qt.Equals, tt.want)
		})
	}
}

// TestNonEmbeddedWileGoastIsNotExcluded pins the other direction: wile/goast
// lives in the wile-goast repository, not in the embed FS, so a user program
// under that name is user code. The old "wile/" prefix excluded it.
func TestNonEmbeddedWileGoastIsNotExcluded(t *testing.T) {
	c := qt.New(t)
	_, err := fs.Stat(stdlib.FS, "wile/goast/ast.sld")
	c.Assert(err, qt.Not(qt.IsNil), qt.Commentf("wile/goast is embedded now; pick another non-embedded path"))
	c.Assert(isStdlibPath("wile/goast/ast.sld"), qt.IsFalse)
}
