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
func (i SourceIndexes) Index() int {
	return i.index
}

// Column returns the column number within the current line (0-based).
func (i SourceIndexes) Column() int {
	return i.column
}

// Line returns the line number (1-based).
func (i SourceIndexes) Line() int {
	return i.line
}

// Inc advances the position by n characters on the same line.
func (i *SourceIndexes) Inc(n int) int {
	i.index += n
	i.column += n
	return i.index
}

// NewLine advances to the start of a new line.
func (i *SourceIndexes) NewLine() int {
	i.index++
	i.column = 0
	i.line++
	return i.index
}

// Tab advances the position by n tab stops on the same line.
func (i *SourceIndexes) Tab() int {
	i.index++
	i.column += 8 - (i.column % 8)
	return i.index
}

// SchemeString returns a string representation of the position.
func (i SourceIndexes) SchemeString() string {
	q := &strings.Builder{}
	q.WriteString("<indexes ")
	fmt.Fprintf(q, "%d:%d:%d", i.index, i.column, i.line)
	q.WriteString(">")
	return q.String()
}

// IsVoid returns false; SourceIndexes is never void.
func (p SourceIndexes) IsVoid() bool {
	return false
}

// EqualTo returns true if the positions are equal.
func (i SourceIndexes) EqualTo(o values.Value) bool {
	v, ok := o.(SourceIndexes)
	if !ok {
		return false
	}
	if i.index != v.index {
		return false
	}
	if i.column != v.column {
		return false
	}
	if i.line != v.line {
		return false
	}
	return true
}
