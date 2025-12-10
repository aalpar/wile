package syntax

import "wile/values"

// syntaxVoidType is a sentinel value representing the absence of a syntax value.
type syntaxVoidType struct{}

func (syntaxVoidType) SchemeString() string        { return values.SpecialVoid }
func (syntaxVoidType) IsVoid() bool                { return true }
func (syntaxVoidType) EqualTo(v values.Value) bool { return v != nil && v.IsVoid() }
func (syntaxVoidType) SourceContext() *SourceContext { return nil }
func (syntaxVoidType) Unwrap() values.Value        { return values.Void }
func (syntaxVoidType) UnwrapAll() values.Value     { return values.Void }

// SyntaxVoid is the singleton syntax void value.
var SyntaxVoid SyntaxValue = syntaxVoidType{}

type SyntaxValue interface {
	values.Value
	SourceContext() *SourceContext
	Unwrap() values.Value
	UnwrapAll() values.Value
}
