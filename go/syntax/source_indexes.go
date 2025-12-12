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
	"wile/values"
	"strings"
)

var (
	_ values.Value = SourceIndexes{}
)

type SourceIndexes struct {
	index  int
	column int
	line   int
}

func NewSourceIndexes(index, column, line int) SourceIndexes {
	q := SourceIndexes{
		index:  index,
		column: column,
		line:   line,
	}
	return q
}

func (i SourceIndexes) Index() int {
	return i.index
}

func (i SourceIndexes) Column() int {
	return i.column
}

func (i SourceIndexes) Line() int {
	return i.line
}

func (i *SourceIndexes) Inc(n int) int {
	i.index += n
	i.column += n
	return i.index
}

func (i *SourceIndexes) NewLine() int {
	i.index++
	i.column = 0
	i.line++
	return i.index
}

func (i SourceIndexes) SchemeString() string {
	q := &strings.Builder{}
	q.WriteString("<indexes ")
	q.WriteString(fmt.Sprintf("%d:%d:%d", i.index, i.column, i.line))
	q.WriteString(">")
	return q.String()
}

func (p SourceIndexes) IsVoid() bool {
	return false
}

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
