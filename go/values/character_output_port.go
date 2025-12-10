package values

import (
	"fmt"
	"io"
)

var (
	_ Value = (*CharacterOutputPort)(nil)
)

type CharacterOutputPort struct {
	Value io.Writer
}

func NewCharacterOutputPort(wrt io.Writer) *CharacterOutputPort {
	q := &CharacterOutputPort{Value: wrt}
	return q
}

func (q *CharacterOutputPort) Datum() io.Writer {
	return q.Value
}

func (p *CharacterOutputPort) IsVoid() bool {
	return p == nil
}

func (p *CharacterOutputPort) EqualTo(v Value) bool {
	if other, ok := v.(*CharacterOutputPort); ok {
		return p.Value == other.Value
	}
	return false
}

func (p *CharacterOutputPort) SchemeString() string {
	return fmt.Sprintf("<character-output-port %p>", p.Value)
}
