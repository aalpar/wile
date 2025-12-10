package syntax

import (
	"fmt"
	"wile/values"
)

var (
	_ values.Value = (*SyntaxDatumComment)(nil)
	_ SyntaxValue  = (*SyntaxDatumComment)(nil)
)

type SyntaxDatumComment struct {
	Label         string
	Value         SyntaxValue
	sourceContext *SourceContext
}

func (p *SyntaxDatumComment) SourceContext() *SourceContext {
	return p.sourceContext
}

func NewSyntaxDatumComment(label string, value SyntaxValue, sctx *SourceContext) *SyntaxDatumComment {
	return &SyntaxDatumComment{
		Label:         label,
		Value:         value,
		sourceContext: sctx,
	}
}

func (p *SyntaxDatumComment) IsVoid() bool {
	return p == nil
}

func (p *SyntaxDatumComment) Unwrap() values.Value {
	return p.Value
}

func (p *SyntaxDatumComment) UnwrapAll() values.Value {
	sv, ok := p.Value.(SyntaxValue)
	if ok {
		return sv.UnwrapAll()
	}
	return p.Value
}

func (p *SyntaxDatumComment) EqualTo(v values.Value) bool {
	other, ok := v.(*SyntaxDatumComment)
	if !ok {
		return false
	}
	if p.Label != other.Label {
		return false
	}
	return p.Value.UnwrapAll().EqualTo(other.Value.UnwrapAll())
}

func (p *SyntaxDatumComment) SchemeString() string {
	return fmt.Sprintf("%s %s", p.Label, p.Value.SchemeString())
}
