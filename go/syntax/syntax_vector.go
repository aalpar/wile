package syntax

import (
	"wile/values"
	"strings"
)

var (
	_ values.Value = (*SyntaxVector)(nil)
	_ SyntaxValue  = (*SyntaxVector)(nil)
)

type SyntaxVector struct {
	Values        []SyntaxValue
	sourceContext *SourceContext
}

func NewSyntaxVector(sc *SourceContext, vs ...SyntaxValue) *SyntaxVector {
	q := &SyntaxVector{
		Values:        vs,
		sourceContext: sc,
	}
	return q
}

func (p *SyntaxVector) SourceContext() *SourceContext {
	return p.sourceContext
}

func (p *SyntaxVector) UnwrapAll() values.Value {
	if p.IsVoid() {
		return values.Void
	}
	vq := make([]values.Value, len(p.Values))
	for i, v := range p.Values {
		vq[i] = v.UnwrapAll()
	}
	return values.NewVector(vq...)
}

func (p *SyntaxVector) Unwrap() values.Value {
	if p.IsVoid() {
		return values.Void
	}
	vq := make([]values.Value, len(p.Values))
	for i, v := range p.Values {
		vq[i] = v
	}
	q := values.NewVector(vq...)
	return q
}

func (p *SyntaxVector) IsVoid() bool {
	return p == nil
}

func (p *SyntaxVector) SchemeString() string {
	if p.IsVoid() {
		return "#'<void>"
	}
	q := strings.Builder{}
	q.WriteString("#'(")
	for i, v := range p.Values {
		if i > 0 {
			q.WriteString(" ")
		}
		q.WriteString(v.SchemeString())
	}
	q.WriteString(")")
	return q.String()
}

// EqualTo performs pointer comparison only, matching Chez Scheme/Racket behavior.
// Two syntax objects are equal? only if they are the same object.
// For value comparison of syntax objects, use bound-identifier=? or free-identifier=?.
func (p *SyntaxVector) EqualTo(o values.Value) bool {
	v, ok := o.(*SyntaxVector)
	if !ok {
		return false
	}
	return p == v
}
