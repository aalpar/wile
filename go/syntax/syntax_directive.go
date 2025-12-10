package syntax

import (
	"fmt"
	"skeme/values"
)

var (
	_ values.Value = (*SyntaxDirective)(nil)
	_ SyntaxValue  = (*SyntaxDirective)(nil)
)

type SyntaxDirective struct {
	Name          string
	sourceContext *SourceContext
}

func (p *SyntaxDirective) SourceContext() *SourceContext {
	return p.sourceContext
}

func (p *SyntaxDirective) Unwrap() values.Value {
	return values.NewString(p.Name)
}

func (p *SyntaxDirective) UnwrapAll() values.Value {
	return values.NewString(p.Name)
}

func NewSyntaxDirective(name string, sctx *SourceContext) *SyntaxDirective {
	return &SyntaxDirective{
		Name:          name,
		sourceContext: sctx,
	}
}

func (p *SyntaxDirective) IsVoid() bool {
	return p == nil
}

func (p *SyntaxDirective) EqualTo(v values.Value) bool {
	other, ok := v.(*SyntaxDirective)
	if !ok {
		return false
	}
	if p.Name != other.Name {
		return false
	}
	return true
}

func (p *SyntaxDirective) SchemeString() string {
	return fmt.Sprintf("%s", p.Name)
}
