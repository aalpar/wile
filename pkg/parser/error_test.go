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
	"errors"
	"io"
	"testing"

	"github.com/aalpar/wile/pkg/internal/tokenizer"
)

// TestParserErrorIsHonorsMessage pins the corrected semantics: two parser
// errors are errors.Is-equal iff they carry the same message. Previously Is
// returned true for any *ParserError.
func TestParserErrorIsHonorsMessage(t *testing.T) {
	var tok tokenizer.Token
	a := NewParserError(tok, "unexpected close paren")
	b := NewParserError(tok, "unexpected close paren")
	c := NewParserError(tok, "unexpected dot")

	if !errors.Is(a, b) {
		t.Errorf("same-message parser errors should be errors.Is-equal")
	}
	if errors.Is(a, c) {
		t.Errorf("%q and %q should NOT be errors.Is-equal", a, c)
	}
}

// TestParserErrorIsMatchesThroughWrap shows message identity survives
// wrapping and the Unwrap chain still reaches a wrapped cause.
func TestParserErrorIsMatchesThroughWrap(t *testing.T) {
	var tok tokenizer.Token
	wrapped := NewParserErrorWithWrap(io.EOF, tok, "incomplete form")

	if !errors.Is(wrapped, io.EOF) {
		t.Errorf("parser error wrapping io.EOF should match io.EOF via Unwrap")
	}
	if !errors.Is(wrapped, NewParserError(tok, "incomplete form")) {
		t.Errorf("wrapped error should match a same-message sentinel by message identity")
	}
	if errors.Is(wrapped, NewParserError(tok, "other message")) {
		t.Errorf("wrapped error should not match a different-message sentinel")
	}
}
