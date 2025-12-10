package parser

import (
	"wile/tokenizer"
)

type ParserError struct {
	err  error
	mess string
	tok  tokenizer.Token
}

func NewTokenizerError(tok tokenizer.Token, mess string) *ParserError {
	q := &ParserError{
		mess: mess,
		tok:  tok,
	}
	return q
}

func NewTokenizerErrorWithWrap(err error, tok tokenizer.Token, mess string) *ParserError {
	q := &ParserError{
		err:  err,
		mess: mess,
		tok:  tok,
	}
	return q
}

func (p *ParserError) Is(err error) bool {
	_, ok := err.(*ParserError)
	return ok
}

func (p *ParserError) Error() string {
	return p.mess
}

func (p *ParserError) Unwrap() error {
	return p.err
}
