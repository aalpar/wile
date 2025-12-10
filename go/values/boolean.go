package values

var (
	_ Value = (*Boolean)(nil)

	FalseValue = NewBoolean(false)
	TrueValue  = NewBoolean(true)
)

type Boolean struct {
	Value bool
}

func NewBoolean(v bool) *Boolean {
	q := &Boolean{Value: v}
	return q
}

func (p *Boolean) Datum() bool {
	return p.Value
}

func (p *Boolean) IsVoid() bool {
	return p == nil
}

func (p *Boolean) EqualTo(v Value) bool {
	if other, ok := v.(*Boolean); ok {
		return p.Value == other.Value
	}
	return false
}

func (p *Boolean) SchemeString() string {
	if !p.Value {
		return "#f"
	}
	return "#t"
}
