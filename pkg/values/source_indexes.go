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
// (registry/core/prim_syntax_loc.go), and every other holder is internal (the
// tokenizer's cursor, SourceContext.Start/End). Implementing Value would enrol
// it in the exhaustiveness set cmd/typeswitchlint derives from this package,
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
// lands on a column congruent to 7 mod 8. See tokenizer.tabStop, which computes
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
