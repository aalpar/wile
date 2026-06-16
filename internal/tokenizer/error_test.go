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

import (
	"errors"
	"io"
	"testing"
)

// TestTokenizerErrorIsHonorsMessage pins the corrected semantics: two
// tokenizer errors are errors.Is-equal iff they carry the same message.
// Previously Is returned true for any *TokenizerError, making all 25 message
// constants mutually indistinguishable.
func TestTokenizerErrorIsHonorsMessage(t *testing.T) {
	a := NewTokenizerError(MessageExpectingNumber)
	b := NewTokenizerError(MessageExpectingNumber)
	c := NewTokenizerError(MessageUnterminatedString)

	if !errors.Is(a, b) {
		t.Errorf("same-message tokenizer errors should be errors.Is-equal")
	}
	if errors.Is(a, c) {
		t.Errorf("%q and %q should NOT be errors.Is-equal", a, c)
	}
}

// TestTokenizerErrorIsMatchesThroughWrap shows message identity survives
// wrapping, and that the Unwrap chain still matches a wrapped cause.
func TestTokenizerErrorIsMatchesThroughWrap(t *testing.T) {
	wrapped := NewTokenizerErrorWithWrap(io.EOF, MessageExpectingToken)

	if !errors.Is(wrapped, io.EOF) {
		t.Errorf("tokenizer error wrapping io.EOF should match io.EOF via Unwrap")
	}
	if !errors.Is(wrapped, NewTokenizerError(MessageExpectingToken)) {
		t.Errorf("wrapped error should match a same-message sentinel by message identity")
	}
	if errors.Is(wrapped, NewTokenizerError(MessageExpectingNumber)) {
		t.Errorf("wrapped error should not match a different-message sentinel")
	}
}
