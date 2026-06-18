// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package parser

import (
	"fmt"

	"github.com/aalpar/wile/pkg/internal/tokenizer"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

var (
	// ErrUnknownTokenType is returned when the parser encounters an unrecognized token.
	ErrUnknownTokenType = werr.NewStaticError("unknown token type")
	ErrAlreadyClosed    = werr.NewStaticError("parser already closed")
)

// ParserError represents an error that occurred during parsing.
type ParserError struct {
	err  error
	mess string
	tok  tokenizer.Token
}

// NewParserError creates a new parser error for the given token.
func NewParserError(tok tokenizer.Token, mess string) *ParserError {
	q := &ParserError{
		mess: mess,
		tok:  tok,
	}
	return q
}

// NewParserErrorf creates a new parser error for the given token.
func NewParserErrorf(tok tokenizer.Token, mess string, vs ...any) *ParserError {
	q := NewParserError(tok, fmt.Sprintf(mess, vs...))
	return q
}

// NewParserErrorWithWrap creates a new parser error wrapping another error.
func NewParserErrorWithWrap(err error, tok tokenizer.Token, mess string) *ParserError {
	q := &ParserError{
		err:  err,
		mess: mess,
		tok:  tok,
	}
	return q
}

// NewParserErrorWithWrapf creates a new parser error wrapping another error.
func NewParserErrorWithWrapf(err error, tok tokenizer.Token, mess string, vs ...any) *ParserError {
	q := NewParserErrorWithWrap(err, tok, fmt.Sprintf(mess, vs...))
	return q
}

// Is implements errors.Is for ParserError. Two parser errors match when they
// carry the same message (the token is positional and is not part of error
// identity — see EqualTo for value-level equality that includes it), so
// errors.Is can distinguish parser failures; a wrapped cause is still reached
// through Unwrap.
func (p *ParserError) Is(err error) bool {
	other, ok := err.(*ParserError)
	if !ok {
		return false
	}
	return p.mess == other.mess
}

func (p *ParserError) Error() string {
	return p.mess
}

func (p *ParserError) Unwrap() error {
	return p.err
}

func (p *ParserError) SchemeString() string {
	return fmt.Sprintf("ParserError at %s: %s", p.tok.String(), p.mess)
}

func (p *ParserError) IsVoid() bool {
	return p == nil
}

func (p *ParserError) EqualTo(v values.Value) bool {
	other, ok := v.(*ParserError)
	return ok && p.mess == other.mess && p.tok == other.tok
}
