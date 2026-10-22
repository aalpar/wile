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

package tokenizer

import (
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/syntax"
)

// TestNewTokenizerAtStartsTheFirstTokenAtTheOrigin: a tokenizer built for a
// reader that is not at the start of its source reports its first token AT the
// origin, not one rune after it. The constructor reads the first rune itself, so
// positioning must happen before that read, never by SeekTo afterwards.
func TestNewTokenizerAtStartsTheFirstTokenAtTheOrigin(t *testing.T) {
	c := qt.New(t)
	origin := syntax.NewSourceIndexes(10, 4, 3)
	tk := NewTokenizerAt(strings.NewReader("(x y)"), false, origin, 0)

	tok, err := tk.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(tok.Start(), qt.Equals, origin)
}

// TestNewTokenizerAtPrevCompletesACRLFPair: a reader standing between the '\r'
// and the '\n' of a CRLF must count the '\n' as the pair's second half, which it
// can only know from the rune consumed before the origin.
func TestNewTokenizerAtPrevCompletesACRLFPair(t *testing.T) {
	c := qt.New(t)
	origin := syntax.NewSourceIndexes(5, 0, 2)
	tk := NewTokenizerAt(strings.NewReader("\n(x)"), false, origin, '\r')

	tok, err := tk.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(tok.Start(), qt.Equals, syntax.NewSourceIndexes(6, 0, 2))
}

// TestSeekToRepositionsAReleasedTokenizer: after a release the tokenizer holds
// no lookahead, and SeekTo moves it to where another consumer left the reader.
func TestSeekToRepositionsAReleasedTokenizer(t *testing.T) {
	c := qt.New(t)
	r := strings.NewReader("(a)\n  (b)")
	tk := NewTokenizer(r, false)

	tok, err := tk.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(tok.String(), qt.Equals, "(")
	_, err = tk.Next()
	c.Assert(err, qt.IsNil)
	tok, err = tk.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(tok.String(), qt.Equals, ")")
	c.Assert(tk.ReleaseLookahead(r), qt.IsNil)

	// Another consumer reads "\n  " off the shared reader.
	for range 3 {
		_, _, err = r.ReadRune()
		c.Assert(err, qt.IsNil)
	}
	c.Assert(tk.SeekTo(syntax.NewSourceIndexes(6, 2, 2), ' '), qt.IsTrue)

	tok, err = tk.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(tok.String(), qt.Equals, "(")
	c.Assert(tok.Start(), qt.Equals, syntax.NewSourceIndexes(6, 2, 2))
}

// TestSeekToDoesNothingToATokenizerHoldingLookahead: a tokenizer that has not
// released its lookahead already counted that rune in the reader's position, and
// seeking would drop it.
func TestSeekToDoesNothingToATokenizerHoldingLookahead(t *testing.T) {
	c := qt.New(t)
	tk := NewTokenizer(strings.NewReader("ab cd"), false)

	tok, err := tk.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(tok.String(), qt.Equals, "ab")

	c.Assert(tk.SeekTo(syntax.NewSourceIndexes(40, 40, 9), 'z'), qt.IsFalse,
		qt.Commentf("a tokenizer holding its lookahead reports that it did not move"))

	tok, err = tk.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(tok.String(), qt.Equals, "cd")
	c.Assert(tok.Start(), qt.Equals, syntax.NewSourceIndexes(3, 3, 1))
}
