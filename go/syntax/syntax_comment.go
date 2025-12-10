package syntax

import (
	"fmt"
	"skeme/values"
)

var (
	_ values.Value = (*SyntaxComment)(nil)
	_ SyntaxValue  = (*SyntaxComment)(nil)
)

type SyntaxComment struct {
	Text          string
	sourceContext *SourceContext
}

func (p *SyntaxComment) SourceContext() *SourceContext {
	return p.sourceContext
}

func (p *SyntaxComment) Unwrap() values.Value {
	return values.NewString(p.Text)
}

func (p *SyntaxComment) UnwrapAll() values.Value {
	return values.NewString(p.Text)
}

func NewSyntaxComment(text string, sctx *SourceContext) *SyntaxComment {
	return &SyntaxComment{
		Text:          text,
		sourceContext: sctx,
	}
}

func (p *SyntaxComment) IsVoid() bool {
	return p == nil
}

func (p *SyntaxComment) EqualTo(v values.Value) bool {
	other, ok := v.(*SyntaxComment)
	if !ok {
		return false
	}
	if p.Text != other.Text {
		return false
	}
	return true
}

func (p *SyntaxComment) SchemeString() string {
	return fmt.Sprintf("%s", p.Text)
}
