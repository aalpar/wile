package syntax

import (
	"fmt"
	"skeme/values"
)

var (
	_ values.Value = (*SyntaxDatumLabel)(nil)
	_ SyntaxValue  = (*SyntaxDatumLabel)(nil)
)

type SyntaxDatumLabel struct {
	Label         int
	sourceContext *SourceContext
}

func (p *SyntaxDatumLabel) SourceContext() *SourceContext {
	return p.sourceContext
}

func (p *SyntaxDatumLabel) Unwrap() values.Value {
	return values.NewInteger(int64(p.Label))
}

func (p *SyntaxDatumLabel) UnwrapAll() values.Value {
	return values.NewInteger(int64(p.Label))
}

func NewSyntaxDatumLabel(label int, sctx *SourceContext) *SyntaxDatumLabel {
	return &SyntaxDatumLabel{
		Label:         label,
		sourceContext: sctx,
	}
}

func (p *SyntaxDatumLabel) IsVoid() bool {
	return p == nil
}

func (p *SyntaxDatumLabel) EqualTo(v values.Value) bool {
	other, ok := v.(*SyntaxDatumLabel)
	if !ok {
		return false
	}
	if p.Label != other.Label {
		return false
	}
	return true
}

func (p *SyntaxDatumLabel) SchemeString() string {
	return fmt.Sprintf("%d", p.Label)
}
