package values

import (
	"fmt"
	"unicode/utf8"
)

var (
	_ Value = (*Character)(nil)
)

type Character struct {
	Value rune
}

func NewCharacter(v rune) *Character {
	q := &Character{Value: v}
	return q
}

func (p *Character) Datum() rune {
	return p.Value
}

func (p *Character) IsVoid() bool {
	return p == nil
}

func (p *Character) EqualTo(v Value) bool {
	if other, ok := v.(*Character); ok {
		return p.Value == other.Value
	}
	return false
}

func (p *Character) SchemeString() string {
	return fmt.Sprintf(`#\%c`, p.Value)
}

func (p *Character) String() string {
	buf := make([]byte, 0, utf8.UTFMax)
	q := utf8.AppendRune(buf, p.Value)
	return string(q)
}
