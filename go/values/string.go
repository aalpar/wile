package values

import "fmt"

var (
	_ Value        = (*String)(nil)
	_ fmt.Stringer = (*String)(nil)
)

type String struct {
	Value string
}

func NewString(str string) *String {
	q := &String{Value: str}
	return q
}

func (p *String) Datum() string {
	return p.Value
}

func (p *String) IsVoid() bool {
	return p == nil
}

func (p *String) EqualTo(v Value) bool {
	if other, ok := v.(*String); ok {
		return p.Value == other.Value
	}
	return false
}

func (p *String) SchemeString() string {
	return fmt.Sprintf("%q", p.Value)
}

func (p *String) String() string {
	return p.Value
}
