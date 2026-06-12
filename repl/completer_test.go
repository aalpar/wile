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

package repl_test

import (
	"context"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
	"github.com/aalpar/wile/repl"
)

func TestCompleter_MetaCommand(t *testing.T) {
	c := qt.New(t)
	sc := repl.NewCompleter(nil, []string{"help", "doc", "edit"})

	line := []rune(",he")
	newLines, length := sc.Do(line, len(line))
	c.Assert(length, qt.Equals, 2)
	c.Assert(len(newLines) > 0, qt.IsTrue)
	c.Assert(string(newLines[0]), qt.Equals, "lp")
}

func TestCompleter_SchemeBinding(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	sc := repl.NewCompleter(eng, nil)
	line := []rune("(ca")
	newLines, length := sc.Do(line, len(line))
	c.Assert(length, qt.Equals, 2)
	c.Assert(len(newLines) > 0, qt.IsTrue)
}

func TestCompleter_EmptyInput(t *testing.T) {
	c := qt.New(t)
	sc := repl.NewCompleter(nil, []string{"help"})
	line := []rune("")
	newLines, _ := sc.Do(line, 0)
	c.Assert(len(newLines), qt.Equals, 0)
}

func TestCompleter_CommaOnly(t *testing.T) {
	c := qt.New(t)
	sc := repl.NewCompleter(nil, []string{"help", "doc", "edit"})
	line := []rune(",")
	newLines, length := sc.Do(line, len(line))
	c.Assert(length, qt.Equals, 0)
	c.Assert(len(newLines), qt.Equals, 3)
}

func TestCompleter_FileCompletion(t *testing.T) {
	c := qt.New(t)
	sc := repl.NewCompleter(nil, []string{"edit"})

	dir := t.TempDir()
	err := os.WriteFile(filepath.Join(dir, "foo.scm"), []byte(""), 0644)
	c.Assert(err, qt.IsNil)
	err = os.WriteFile(filepath.Join(dir, "foobar.scm"), []byte(""), 0644)
	c.Assert(err, qt.IsNil)

	prefix := filepath.Join(dir, "foo")
	line := []rune(",edit " + prefix)
	newLines, length := sc.Do(line, len(line))
	c.Assert(length, qt.Equals, len(prefix))
	c.Assert(len(newLines), qt.Equals, 2)
}

func TestCompleter_FileCompletionDir(t *testing.T) {
	c := qt.New(t)
	sc := repl.NewCompleter(nil, nil)

	dir := t.TempDir()
	err := os.Mkdir(filepath.Join(dir, "subdir"), 0755)
	c.Assert(err, qt.IsNil)

	prefix := filepath.Join(dir, "sub")
	line := []rune(",edit " + prefix)
	newLines, length := sc.Do(line, len(line))
	c.Assert(length, qt.Equals, len(prefix))
	c.Assert(len(newLines), qt.Equals, 1)
	// Directory completions end with "/"
	c.Assert(strings.HasSuffix(string(newLines[0]), "/"), qt.IsTrue)
}

func TestCompleter_BindingNames_NilEngine(t *testing.T) {
	c := qt.New(t)
	sc := repl.NewCompleter(nil, nil)
	names := sc.BindingNames()
	c.Assert(names, qt.IsNil)
}

func TestCompleter_BindingNames_WithEngine(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	sc := repl.NewCompleter(eng, nil)
	names := sc.BindingNames()
	c.Assert(len(names) > 0, qt.IsTrue)
	c.Assert(slices.Contains(names, "car"), qt.IsTrue,
		qt.Commentf("expected 'car' in binding names"))
}

func TestCompleter_SchemeBindingNilEngine(t *testing.T) {
	c := qt.New(t)
	sc := repl.NewCompleter(nil, nil)

	// Scheme binding completion with nil engine returns nothing
	line := []rune("(ca")
	newLines, length := sc.Do(line, len(line))
	c.Assert(length, qt.Equals, 2)
	c.Assert(len(newLines), qt.Equals, 0)
}
