package values

import "fmt"

var (
	_ Value = (*Byte)(nil)
)

type Byte struct {
	Value uint8
}

func NewByte(v uint8) *Byte {
	q := &Byte{Value: v}
	return q
}

func (p *Byte) Datum() uint8 {
	return p.Value
}

func (p *Byte) IsVoid() bool {
	return p == nil
}

func (p *Byte) EqualTo(v Value) bool {
	if other, ok := v.(*Byte); ok {
		return p.Value == other.Value
	}
	return false
}

func (p *Byte) SchemeString() string {
	return fmt.Sprintf("%d", p.Value)
}
