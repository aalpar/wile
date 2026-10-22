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
	"fmt"
	"strings"
)

// SourceIndexes tracks position within a source file (index, column, line).
//
// It is deliberately NOT a Value. No SourceIndexes ever reaches Scheme: the
// syntax-location accessors project an int out of one and box that
// (registry/core/prim_syntax_loc.go), and every other holder is a Go-side
// position (the tokenizer's cursor, SourceContext.Start/End, and the pair
// (*PortObject).ReadPosition returns). Implementing Value would enrol
// it in the exhaustiveness set tools/cmd/typeswitchlint derives from this package,
// which enumerates the types a Scheme datum can be.
//
// The three int fields make it comparable, so == is the equality operator;
// there is no EqualTo.
type SourceIndexes struct {
	index  int
	column int
	line   int
}

// NewSourceIndexes creates a new SourceIndexes with the given position.
func NewSourceIndexes(index, column, line int) SourceIndexes {
	q := SourceIndexes{
		index:  index,
		column: column,
		line:   line,
	}
	return q
}

// Index returns the absolute byte position in the source.
func (p SourceIndexes) Index() int {
	return p.index
}

// Column returns the column number within the current line (0-based).
func (p SourceIndexes) Column() int {
	return p.column
}

// Line returns the line number (1-based).
func (p SourceIndexes) Line() int {
	return p.line
}

// Inc advances the position by n characters on the same line.
func (p *SourceIndexes) Inc(n int) int {
	p.index += n
	p.column += n
	return p.index
}

// NewLine updates column and line tracking for a newline character.
// The index should already have been advanced by Inc(n) before calling this.
func (p *SourceIndexes) NewLine() int {
	p.column = 0
	p.line++
	return p.index
}

// Tab advances the column to the next 8-column tab stop, assuming the column
// still points AT the tab (i.e. has not been stepped past it). Do not call it
// after Inc(1) for the tab character: Inc advances the column too, so the stop
// is computed one column late and over-advances by a whole stop when the tab
// lands on a column congruent to 7 mod 8. See tabStop in this file, which computes
// the stop itself rather than using this method.
func (p *SourceIndexes) Tab() int {
	p.column += 8 - (p.column % 8)
	return p.index
}

// String renders the position for diagnostics. It is fmt.Stringer, not
// values.Value: this text is for a Go-side reader, not a Scheme datum.
func (p SourceIndexes) String() string {
	q := &strings.Builder{}
	q.WriteString("<indexes ")
	fmt.Fprintf(q, "%d:%d:%d", p.index, p.column, p.line)
	q.WriteString(">")
	return q.String()
}

// AdvanceIndexes returns the position after one rune has been consumed. cur is
// the rune just read, n the bytes it occupied, and prev the rune read before it
// (0 when none). It is the single definition of how a reader counts: the
// tokenizer counts with it as it scans, and a port counts with it as its rune
// reader yields runes (so read-char, peek-char, read-line, read-string and the
// tokenizer reading through the port all move it), so the two agree by
// construction. A read of a port's underlying io.Reader does not go through it.
//
// Index counts bytes and column counts characters, so a multi-byte rune is one
// column. A '\n' or a lone '\r' ends a line and CRLF ends exactly one: the
// '\n' whose prev is '\r' only holds the column at the new line's origin, which
// is why prev is a parameter and the caller must keep TWO runes of history.
// A tab moves the column to the next 8-column stop.
func AdvanceIndexes(si SourceIndexes, prev, cur rune, n int) SourceIndexes {
	// Every rune above '\r' is an ordinary character: one byte-width and one
	// column. Keeping that case this small lets the compiler inline it into the
	// per-rune loops (the tokenizer's scan and every port read); the line-ending
	// and tab rules live out of line in advanceControl.
	if cur > '\r' {
		si.index += n
		si.column++
		return si
	}
	return advanceControl(si, prev, cur, n)
}

// advanceControl is AdvanceIndexes for the runes at or below '\r', which is
// where '\t', '\n' and '\r' live. cur above '\r' never reaches it from
// AdvanceIndexes, but it is correct for any rune.
func advanceControl(si SourceIndexes, prev, cur rune, n int) SourceIndexes {
	q := NewSourceIndexes(si.Index()+n, si.Column()+1, si.Line())
	if cur == '\n' && prev == '\r' {
		return NewSourceIndexes(q.Index(), 0, q.Line())
	}
	if cur == '\n' || cur == '\r' {
		q.NewLine()
		return q
	}
	if cur == '\t' {
		return tabStop(q)
	}
	return q
}

// tabStop advances si's column to the next 8-column tab stop. si's column was
// already stepped one past the tab's own column by AdvanceIndexes, so the stop
// is computed from the tab's column (column-1). SourceIndexes.Tab() adds a whole
// stop on top of that step instead, over-advancing by 8 whenever the tab sits on
// a column congruent to 7 (mod 8).
func tabStop(si SourceIndexes) SourceIndexes {
	col := si.Column() - 1
	return NewSourceIndexes(si.Index(), col+8-col%8, si.Line())
}
