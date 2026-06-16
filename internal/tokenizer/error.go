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

package tokenizer

// ErrorCode represents a tokenizer error classification.
type ErrorCode int

// TokenizerError represents an error that occurred during tokenization.
type TokenizerError struct {
	err  error
	mess string
}

// NewTokenizerError creates a new tokenizer error with the given message.
func NewTokenizerError(mess string) *TokenizerError {
	q := &TokenizerError{
		mess: mess,
	}
	return q
}

// NewTokenizerErrorWithWrap creates a new tokenizer error that wraps another error.
func NewTokenizerErrorWithWrap(err error, mess string) *TokenizerError {
	q := &TokenizerError{
		err:  err,
		mess: mess,
	}
	return q
}

// Is implements errors.Is for TokenizerError. Two tokenizer errors match
// when they carry the same message, so errors.Is can distinguish the message
// constants (e.g. errors.Is(err, NewTokenizerError(MessageUnterminatedString)));
// a wrapped cause is still reached through Unwrap.
func (p *TokenizerError) Is(err error) bool {
	other, ok := err.(*TokenizerError)
	if !ok {
		return false
	}
	return p.mess == other.mess
}

func (p *TokenizerError) Error() string {
	return p.mess
}

func (p *TokenizerError) Unwrap() error {
	return p.err
}
