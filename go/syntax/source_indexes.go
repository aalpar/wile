// Copyright 2025 Aaron Alpar
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

package syntax

import (
	"fmt"
	"strings"

	"wile/values"
)

var _ values.Value = SourceIndexes{}

// SourceIndexes tracks position within a source file (index, column, line).
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

// NewLine advances to the start of a new line.
func (p *SourceIndexes) NewLine() int {
	p.index++
	p.column = 0
	p.line++
	return p.index
}

// Tab advances the position by n tab stops on the same line.
func (p *SourceIndexes) Tab() int {
	p.index++
	p.column += 8 - (p.column % 8)
	return p.index
}

// SchemeString returns a string representation of the position.
func (p SourceIndexes) SchemeString() string {
	q := &strings.Builder{}
	q.WriteString("<indexes ")
	fmt.Fprintf(q, "%d:%d:%d", p.index, p.column, p.line)
	q.WriteString(">")
	return q.String()
}

// IsVoid returns false; SourceIndexes is never void.
func (p SourceIndexes) IsVoid() bool {
	return false
}

// EqualTo returns true if the positions are equal.
func (p SourceIndexes) EqualTo(o values.Value) bool {
	v, ok := o.(SourceIndexes)
	if !ok {
		return false
	}
	if p.index != v.index {
		return false
	}
	if p.column != v.column {
		return false
	}
	if p.line != v.line {
		return false
	}
	return true
}
