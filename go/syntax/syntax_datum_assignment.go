package syntax

import (
	"fmt"
	"wile/values"
)

var (
	_ values.Value = (*SyntaxDatumLabelAssignment)(nil)
	_ SyntaxValue  = (*SyntaxDatumLabelAssignment)(nil)
)

type SyntaxDatumLabelAssignment struct {
	Label         int
	Value         values.Value
	sourceContext *SourceContext
}

func (p *SyntaxDatumLabelAssignment) SourceContext() *SourceContext {
	return p.sourceContext
}

func NewSyntaxDatumLabelAssignment(label int, value values.Value, sctx *SourceContext) *SyntaxDatumLabelAssignment {
	return &SyntaxDatumLabelAssignment{
		Label:         label,
		Value:         value,
		sourceContext: sctx,
	}
}

func (p *SyntaxDatumLabelAssignment) IsVoid() bool {
	return p == nil
}

func (p *SyntaxDatumLabelAssignment) Unwrap() values.Value {
	return p.Value
}

func (p *SyntaxDatumLabelAssignment) UnwrapAll() values.Value {
	q := p.Value
	sv, ok := p.Value.(SyntaxValue)
	if ok {
		q = sv.UnwrapAll()
	}
	return q
}

func (p *SyntaxDatumLabelAssignment) EqualTo(v values.Value) bool {
	other, ok := v.(*SyntaxDatumLabelAssignment)
	if !ok {
		return false
	}
	return p == other
}

func (p *SyntaxDatumLabelAssignment) SchemeString() string {
	return fmt.Sprintf("%d", p.Label)
}
