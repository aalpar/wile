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
	"errors"
	"io"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
)

// TestReleaseLookaheadLeavesDelimiterOnReader: after a release the reader stands
// at the end of the token, so another consumer of it reads the delimiter, and
// the tokenizer resumes from wherever that consumer left the reader.
func TestReleaseLookaheadLeavesDelimiterOnReader(t *testing.T) {
	c := qt.New(t)
	r := strings.NewReader("ab  cd")
	tk := NewTokenizer(r, false)

	tok, err := tk.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(tok.String(), qt.Equals, "ab")
	c.Assert(tk.ReleaseLookahead(r), qt.IsNil)
	c.Assert(tk.ReleaseLookahead(r), qt.IsNil, qt.Commentf("a second release holds nothing"))

	ch, _, err := r.ReadRune()
	c.Assert(err, qt.IsNil)
	c.Assert(ch, qt.Equals, ' ')

	tok, err = tk.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(tok.String(), qt.Equals, "cd")
	c.Assert(tk.ReleaseLookahead(r), qt.IsNil, qt.Commentf("end of input consumed nothing to unread"))
	_, err = tk.Next()
	c.Assert(errors.Is(err, io.EOF), qt.IsTrue, qt.Commentf("err = %v", err))
}

// TestReleaseLookaheadPreservesPositions: releasing after every token and
// re-reading the same rune must reproduce the positions of an uninterrupted
// scan, across every line-ending spelling and a tab stop, since a released rune
// is often the whitespace those adjustments apply to.
func TestReleaseLookaheadPreservesPositions(t *testing.T) {
	srcs := []string{
		"foo\n  (bar baz) qux",
		"a\r\nb c",
		"\"s\"\r\n\tb",
		"a\tb\rc",
	}
	for _, src := range srcs {
		t.Run(src, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(spans(t, src, true), qt.DeepEquals, spans(t, src, false))
		})
	}
}

func spans(t *testing.T, src string, release bool) []string {
	t.Helper()
	r := strings.NewReader(src)
	tk := NewTokenizer(r, false)
	var q []string
	for {
		tok, err := tk.Next()
		if errors.Is(err, io.EOF) {
			return q
		}
		qt.Assert(t, err, qt.IsNil)
		q = append(q, tok.String()+" "+tok.Start().String()+"-"+tok.End().String())
		if release {
			qt.Assert(t, tk.ReleaseLookahead(r), qt.IsNil)
		}
	}
}

// TestReleaseLookaheadKeepsPendingScannerFault: a fault stamped while scanning
// ("#b109" stops at the 9) cannot be reproduced by re-reading one rune, so a
// release leaves it pending and the next Next reports the same located error.
func TestReleaseLookaheadKeepsPendingScannerFault(t *testing.T) {
	c := qt.New(t)
	firstError := func(release bool) error {
		r := strings.NewReader("#b109")
		tk := NewTokenizer(r, false)
		for {
			_, err := tk.Next()
			if err != nil {
				return err
			}
			if release {
				c.Assert(tk.ReleaseLookahead(r), qt.IsNil)
			}
		}
	}
	want := firstError(false)
	var terr *TokenizerError
	c.Assert(errors.As(want, &terr), qt.IsTrue, qt.Commentf("control: err = %v", want))
	// Error() renders the stamp: message, index, line, column, rune, and state.
	c.Assert(firstError(true).Error(), qt.Equals, want.Error())
}

// TestReleaseLookaheadReportsUnreadFailure: when the rune cannot go back the
// reader's position is already wrong, so the failure is returned, not absorbed.
func TestReleaseLookaheadReportsUnreadFailure(t *testing.T) {
	c := qt.New(t)
	tk := NewTokenizer(strings.NewReader("ab cd"), false)
	_, err := tk.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(errors.Is(tk.ReleaseLookahead(failingUnreader{}), werr.ErrInvalidArgument), qt.IsTrue)
}

type failingUnreader struct{}

func (failingUnreader) UnreadRune() error {
	return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "failingUnreader: refused")
}
