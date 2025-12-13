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

package tokenizer

import (
	"wile/syntax"
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
