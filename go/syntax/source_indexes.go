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
