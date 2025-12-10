package values

import (
	"bufio"
	"fmt"
	"io"
)

var (
	_ Value = (*CharacterInputPort)(nil)
)

type CharacterInputPort struct {
	Value io.RuneReader
}

func NewCharacterInputPortFromReader(rdr io.Reader) *CharacterInputPort {
	q := &CharacterInputPort{Value: bufio.NewReader(rdr)}
	return q
}

func NewCharacterInputPort(rdr io.RuneReader) *CharacterInputPort {
	q := &CharacterInputPort{Value: rdr}
	return q
}

func (p *CharacterInputPort) Datum() io.RuneReader {
	return p.Value
}

func (p *CharacterInputPort) IsVoid() bool {
	return p == nil
}

func (p *CharacterInputPort) EqualTo(v Value) bool {
	if other, ok := v.(*CharacterInputPort); ok {
		return p.Value == other.Value
	}
	return false
}

func (p *CharacterInputPort) SchemeString() string {
	return fmt.Sprintf("<character-input-port %p>", p.Value)
}
