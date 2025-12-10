package values

var (
	_ Value = (*Symbol)(nil)
)

type Symbol struct {
	Key string
}

func NewSymbol(key string) *Symbol {
	q := &Symbol{Key: key}
	return q
}

func (p *Symbol) Datum() string {
	return p.Key
}

func (p *Symbol) Copy() Value {
	q := &Symbol{Key: p.Key}
	return q
}

func (p *Symbol) IsVoid() bool {
	return p == nil
}

func (p *Symbol) EqualTo(v Value) bool {
	if other, ok := v.(*Symbol); ok {
		return p.Key == other.Key
	}
	return false
}

func (p *Symbol) SchemeString() string {
	return p.Key
}
