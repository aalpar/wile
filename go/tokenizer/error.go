package tokenizer

import (
	"skeme/syntax"
)

type ErrorCode int

type TokenizerError struct {
	err   error
	start syntax.SourceIndexes
	end   syntax.SourceIndexes
	mess  string
}

func NewTokenizerError(mess string, start, end syntax.SourceIndexes) *TokenizerError {
	q := &TokenizerError{
		start: start,
		end:   end,
		mess:  mess,
	}
	return q
}

func NewTokenizerErrorWithWrap(err error, mess string, start, end syntax.SourceIndexes) *TokenizerError {
	q := &TokenizerError{
		err:   err,
		start: start,
		end:   end,
		mess:  mess,
	}
	return q
}

func (p *TokenizerError) Is(err error) bool {
	_, ok := err.(*TokenizerError)
	return ok
}

func (p *TokenizerError) Error() string {
	return p.mess
}

func (p *TokenizerError) Unwrap() error {
	return p.err
}
