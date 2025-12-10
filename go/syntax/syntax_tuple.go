package syntax

import "wile/values"

type SyntaxTuple interface {
	values.Tuple
	SyntaxValue
	SyntaxCar() SyntaxValue
	SyntaxCdr() SyntaxValue
	SetSyntaxCar(SyntaxValue)
	SetSyntaxCdr(SyntaxValue)
	AsSyntaxVector() *SyntaxVector
	SyntaxAppend(value SyntaxValue) SyntaxValue
	SyntaxForEach(func(i int, hasNext bool, v SyntaxValue) error) (SyntaxValue, error)
}
