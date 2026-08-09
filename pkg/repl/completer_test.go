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

	"github.com/aalpar/wile/pkg/repl"
	"github.com/aalpar/wile/pkg/wile"
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
	// Regression guard for the sealed-base carve: completion must include names that
	// live ONLY in the sealed base — bootstrap procedures like caar/cadr — not just
	// dual-registered expand-phase primitives like `car`. Before the fix,
	// collectBindingNames walked only the phase frames' own key maps (no parent walk),
	// so every sealed-base name was silently dropped while `car` survived via the
	// expand phase — which is exactly why asserting only `car` failed to catch it.
	for _, name := range []string{"car", "caar", "cadr"} {
		c.Assert(slices.Contains(names, name), qt.IsTrue,
			qt.Commentf("expected %q in completion binding names (sealed-base carve regression)", name))
	}
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

// TestCompleter_DoReturnsRuneOffset pins the *units* of Do's second return value.
//
// readline indexes a []rune buffer by that value (ergochat/readline runebuf.go,
// RuneSlice) and the AutoCompleter.Do contract documents it as a rune length. Both
// return sites used to hand back len(prefix) in BYTES, so a non-ASCII prefix made
// readline slice a rune buffer by a byte count and panic with
// "slice bounds out of range" once the excess exceeded the runes preceding the
// symbol on the line.
//
// The assertion has to be on the offset, not on "does not panic": Do returns
// (matches, len(prefix)) and returns cleanly no matter what, so a no-panic test
// passes even with the byte-length bug and is a blind guard by construction.
//
// Gate NOT covered here: the readline-side panic itself. Reproducing it needs a
// pty driving EnterCompleteMode with two or more candidates that share no common
// suffix prefix, which is out of scope for a unit test.
func TestCompleter_DoReturnsRuneOffset(t *testing.T) {
	tests := []struct {
		name string
		line string
		pos  int
		want int
	}{{
		// Scheme-binding context. "éé" is 2 runes / 4 bytes; observed 4 before the fix.
		name: "scheme binding prefix counted in runes",
		line: "éé",
		pos:  2,
		want: 2,
	}, {
		// Filename context. "日本語日本語" is 6 runes / 18 bytes; observed 18 before the fix.
		name: "filename prefix counted in runes",
		line: ",edit 日本語日本語",
		pos:  12,
		want: 6,
	}, {
		// Meta-command context, ASCII: bytes and runes agree, so this arm pins that
		// the fix does not move the common case.
		name: "ascii meta command unchanged",
		line: ",he",
		pos:  3,
		want: 2,
	}}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			sc := repl.NewCompleter(nil, []string{"help", "doc", "edit"})
			_, length := sc.Do([]rune(tt.line), tt.pos)
			c.Assert(length, qt.Equals, tt.want)
		})
	}
}
