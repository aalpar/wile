package values

import "fmt"

var (
	_ Value = (*Box)(nil)
)

type Box struct {
	Value Value
}

func NewBox(v Value) *Box {
	q := &Box{
		Value: v,
	}
	return q
}

func (p *Box) Datum() Value {
	return p.Value
}

func (p *Box) Unbox() Value {
	return p.Value
}

func (p *Box) IsVoid() bool {
	return p == nil
}

func (p *Box) EqualTo(v Value) bool {
	other, ok := v.(*Box)
	if !ok {
		return false
	}
	if p == other {
		return true
	}
	if p == nil && other == nil {
		return true
	}
	if p == nil || other == nil {
		return false
	}
	if p.Value == nil && other.Value == nil {
		return true
	}
	if p.Value == nil || other.Value == nil {
		return false
	}
	return p.Value.EqualTo(other.Value)
}

func (p *Box) SchemeString() string {
	return fmt.Sprintf("#&%s", p.Value.SchemeString())
}
