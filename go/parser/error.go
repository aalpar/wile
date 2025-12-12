// Copyright 2025 Aaron Alpar
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
