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

package values

import (
	"bytes"
	"io"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestAdvanceIndexes(t *testing.T) {
	tcs := []struct {
		name string
		from SourceIndexes
		prev rune
		cur  rune
		n    int
		want SourceIndexes
	}{
		{"ascii steps one column and one byte",
			NewSourceIndexes(3, 3, 1), 'b', 'c', 1, NewSourceIndexes(4, 4, 1)},
		{"a multibyte rune is many bytes and one column",
			NewSourceIndexes(0, 0, 1), 0, 'é', 2, NewSourceIndexes(2, 1, 1)},
		{"newline starts a line",
			NewSourceIndexes(5, 5, 1), 'x', '\n', 1, NewSourceIndexes(6, 0, 2)},
		{"bare return starts a line",
			NewSourceIndexes(5, 5, 1), 'x', '\r', 1, NewSourceIndexes(6, 0, 2)},
		{"the newline of a CRLF pair adds no second line",
			NewSourceIndexes(6, 0, 2), '\r', '\n', 1, NewSourceIndexes(7, 0, 2)},
		{"a newline after a newline is a line of its own",
			NewSourceIndexes(6, 0, 2), '\n', '\n', 1, NewSourceIndexes(7, 0, 3)},
		{"a return after a return is a line of its own",
			NewSourceIndexes(6, 0, 2), '\r', '\r', 1, NewSourceIndexes(7, 0, 3)},
		{"tab at column 0 lands on the first stop",
			NewSourceIndexes(0, 0, 1), 0, '\t', 1, NewSourceIndexes(1, 8, 1)},
		{"tab at column 3 lands on the first stop",
			NewSourceIndexes(3, 3, 1), 'c', '\t', 1, NewSourceIndexes(4, 8, 1)},
		{"tab on a stop moves a whole stop",
			NewSourceIndexes(8, 8, 1), 'x', '\t', 1, NewSourceIndexes(9, 16, 1)},
		{"the rune just above CR is ordinary",
			NewSourceIndexes(2, 2, 1), 'x', '\x0e', 1, NewSourceIndexes(3, 3, 1)},
		{"CR itself is a line ending",
			NewSourceIndexes(2, 2, 1), 'x', '\r', 1, NewSourceIndexes(3, 0, 2)},
		{"LF itself is a line ending",
			NewSourceIndexes(2, 2, 1), 'x', '\n', 1, NewSourceIndexes(3, 0, 2)},
		{"vertical tab is ordinary",
			NewSourceIndexes(2, 2, 1), 'x', '\v', 1, NewSourceIndexes(3, 3, 1)},
		{"form feed is ordinary",
			NewSourceIndexes(2, 2, 1), 'x', '\f', 1, NewSourceIndexes(3, 3, 1)},
		{"NUL is ordinary",
			NewSourceIndexes(2, 2, 1), 'x', 0, 1, NewSourceIndexes(3, 3, 1)},
		{"a three-byte rune is three bytes and one column",
			NewSourceIndexes(2, 2, 1), 'x', '\u20ac', 3, NewSourceIndexes(5, 3, 1)},
		{"a four-byte rune above the BMP is four bytes and one column",
			NewSourceIndexes(2, 2, 1), 'x', '\U0001F600', 4, NewSourceIndexes(6, 3, 1)},
		{"tab at column 7 does not over-advance (the Tab method would)",
			NewSourceIndexes(7, 7, 1), 'x', '\t', 1, NewSourceIndexes(8, 8, 1)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			got := AdvanceIndexes(tc.from, tc.prev, tc.cur, tc.n)
			c.Assert(got, qt.Equals, tc.want)
		})
	}
}

func newTextPort(s string) *PortObject {
	return NewStringInputPortWithBuffer(bytes.NewBufferString(s))
}

func readRunes(t *testing.T, p *PortObject, k int) {
	t.Helper()
	rr, ok := p.AsRuneReader()
	qt.New(t).Assert(ok, qt.IsTrue)
	for range k {
		_, _, err := rr.ReadRune()
		qt.New(t).Assert(err, qt.IsNil)
	}
}

func TestPortReadPosition(t *testing.T) {
	t.Run("a fresh port stands at the origin", func(t *testing.T) {
		c := qt.New(t)
		at, prev := newTextPort("abc").ReadPosition()
		c.Assert(at, qt.Equals, NewSourceIndexes(0, 0, 1))
		c.Assert(prev, qt.Equals, rune(0))
	})

	t.Run("a nil port stands at the origin", func(t *testing.T) {
		c := qt.New(t)
		var p *PortObject
		at, prev := p.ReadPosition()
		c.Assert(at, qt.Equals, NewSourceIndexes(0, 0, 1))
		c.Assert(prev, qt.Equals, rune(0))
	})

	t.Run("reads advance it and name the last rune", func(t *testing.T) {
		c := qt.New(t)
		p := newTextPort("ab\ncd")
		readRunes(t, p, 4)
		at, prev := p.ReadPosition()
		c.Assert(at, qt.Equals, NewSourceIndexes(4, 1, 2))
		c.Assert(prev, qt.Equals, 'c')
	})

	t.Run("a multibyte rune is one column and its byte width", func(t *testing.T) {
		c := qt.New(t)
		p := newTextPort("ééx")
		readRunes(t, p, 2)
		at, _ := p.ReadPosition()
		c.Assert(at, qt.Equals, NewSourceIndexes(4, 2, 1))
	})

	t.Run("unread restores the position and the rune before it", func(t *testing.T) {
		c := qt.New(t)
		p := newTextPort("abc")
		readRunes(t, p, 2)
		urr, ok := p.AsRuneUnreader()
		c.Assert(ok, qt.IsTrue)
		err := urr.UnreadRune()
		c.Assert(err, qt.IsNil)
		at, prev := p.ReadPosition()
		c.Assert(at, qt.Equals, NewSourceIndexes(1, 1, 1))
		c.Assert(prev, qt.Equals, 'a')
	})

	t.Run("peeking across a CRLF counts one line ending", func(t *testing.T) {
		c := qt.New(t)
		p := newTextPort("a\r\nb")
		readRunes(t, p, 2) // 'a', '\r'
		rr, _ := p.AsRuneReader()
		urr, _ := p.AsRuneUnreader()
		_, _, err := rr.ReadRune() // peek the '\n' ...
		c.Assert(err, qt.IsNil)
		err = urr.UnreadRune() // ... and put it back
		c.Assert(err, qt.IsNil)
		_, _, err = rr.ReadRune() // read it for real
		c.Assert(err, qt.IsNil)
		at, prev := p.ReadPosition()
		c.Assert(at, qt.Equals, NewSourceIndexes(3, 0, 2))
		c.Assert(prev, qt.Equals, '\n')
	})

	t.Run("end of input does not move it", func(t *testing.T) {
		c := qt.New(t)
		p := newTextPort("a")
		readRunes(t, p, 1)
		rr, _ := p.AsRuneReader()
		_, _, err := rr.ReadRune()
		c.Assert(err, qt.Equals, io.EOF)
		at, prev := p.ReadPosition()
		c.Assert(at, qt.Equals, NewSourceIndexes(1, 1, 1))
		c.Assert(prev, qt.Equals, 'a')
	})

	t.Run("a failed unread leaves it where it was", func(t *testing.T) {
		c := qt.New(t)
		p := newTextPort("ab")
		readRunes(t, p, 1)
		urr, _ := p.AsRuneUnreader()
		err := urr.UnreadRune()
		c.Assert(err, qt.IsNil)
		err = urr.UnreadRune() // bytes.Buffer allows one
		c.Assert(err, qt.IsNotNil)
		at, _ := p.ReadPosition()
		c.Assert(at, qt.Equals, NewSourceIndexes(0, 0, 1))
	})

	t.Run("a closed port does not move", func(t *testing.T) {
		c := qt.New(t)
		p := newTextPort("ab")
		readRunes(t, p, 1)
		err := p.Close()
		c.Assert(err, qt.IsNil)
		rr, _ := p.AsRuneReader()
		_, _, err = rr.ReadRune()
		c.Assert(err, qt.IsNotNil)
		at, _ := p.ReadPosition()
		c.Assert(at, qt.Equals, NewSourceIndexes(1, 1, 1))
	})

	t.Run("an invalid byte counts as one column and one byte", func(t *testing.T) {
		c := qt.New(t)
		p := NewStringInputPortWithBuffer(bytes.NewBuffer([]byte{0xff, 'a'}))
		readRunes(t, p, 1)
		at, _ := p.ReadPosition()
		c.Assert(at, qt.Equals, NewSourceIndexes(1, 1, 1))
	})
}

func TestPortReadPositionAfterAFailedUnread(t *testing.T) {
	c := qt.New(t)
	p := NewStringInputPortWithBuffer(bytes.NewBufferString("a"))
	rr, _ := p.AsRuneReader()
	urr, _ := p.AsRuneUnreader()
	_, _, err := rr.ReadRune()
	c.Assert(err, qt.IsNil)
	_, _, err = rr.ReadRune() // EOF: bytes.Buffer forgets the last ReadRune
	c.Assert(err, qt.IsNotNil)
	err = urr.UnreadRune()
	c.Assert(err, qt.IsNotNil)
	at, prev := p.ReadPosition()
	c.Assert(at, qt.Equals, NewSourceIndexes(1, 1, 1))
	c.Assert(prev, qt.Equals, 'a')
}
