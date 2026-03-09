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
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// Test helper functions
func TestIsLetter(t *testing.T) {
	c := qt.New(t)
	c.Assert(isUnicodeLetter('a'), qt.IsTrue)
	c.Assert(isUnicodeLetter('z'), qt.IsTrue)
	c.Assert(isUnicodeLetter('A'), qt.IsTrue)
	c.Assert(isUnicodeLetter('Z'), qt.IsTrue)
	c.Assert(isUnicodeLetter('0'), qt.IsFalse)
	c.Assert(isUnicodeLetter('-'), qt.IsFalse)
}

func TestIsDigit(t *testing.T) {
	c := qt.New(t)
	// Binary
	c.Assert(isDigit(2, '0'), qt.IsTrue)
	c.Assert(isDigit(2, '1'), qt.IsTrue)
	c.Assert(isDigit(2, '2'), qt.IsFalse)
	// Octal
	c.Assert(isDigit(8, '7'), qt.IsTrue)
	c.Assert(isDigit(8, '8'), qt.IsFalse)
	// Decimal
	c.Assert(isDigit(10, '9'), qt.IsTrue)
	c.Assert(isDigit(10, 'a'), qt.IsFalse)
	// Hex
	c.Assert(isDigit(16, '9'), qt.IsTrue)
	c.Assert(isDigit(16, 'a'), qt.IsTrue)
	c.Assert(isDigit(16, 'f'), qt.IsTrue)
	c.Assert(isDigit(16, 'A'), qt.IsTrue)
	c.Assert(isDigit(16, 'F'), qt.IsTrue)
	c.Assert(isDigit(16, 'g'), qt.IsFalse)
}

func TestIsDelimiter(t *testing.T) {
	c := qt.New(t)
	c.Assert(isDelimiter(' '), qt.IsTrue)
	c.Assert(isDelimiter('\t'), qt.IsTrue)
	c.Assert(isDelimiter('|'), qt.IsTrue)
	c.Assert(isDelimiter('\n'), qt.IsTrue)
	c.Assert(isDelimiter('\r'), qt.IsTrue)
	c.Assert(isDelimiter('a'), qt.IsFalse)
}

func TestIsSpecialInitial(t *testing.T) {
	c := qt.New(t)
	specials := []rune{'!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '^', '_', '~'}
	for _, s := range specials {
		c.Assert(isSpecialInitial(s), qt.IsTrue)
	}
	c.Assert(isSpecialInitial('a'), qt.IsFalse)
	c.Assert(isSpecialInitial('1'), qt.IsFalse)
}

func TestIsExplicitSign(t *testing.T) {
	c := qt.New(t)
	c.Assert(isExplicitSign('+'), qt.IsTrue)
	c.Assert(isExplicitSign('-'), qt.IsTrue)
	c.Assert(isExplicitSign('*'), qt.IsFalse)
}

func TestIsNumberInitial(t *testing.T) {
	c := qt.New(t)
	c.Assert(isNumberInitial(10, '+'), qt.IsTrue)
	c.Assert(isNumberInitial(10, '-'), qt.IsTrue)
	c.Assert(isNumberInitial(10, '.'), qt.IsTrue)
	c.Assert(isNumberInitial(10, '5'), qt.IsTrue)
	c.Assert(isNumberInitial(10, 'a'), qt.IsFalse)
	c.Assert(isNumberInitial(16, 'a'), qt.IsTrue)
	c.Assert(isNumberInitial(16, 'F'), qt.IsTrue)
	c.Assert(isNumberInitial(2, '0'), qt.IsTrue)
	c.Assert(isNumberInitial(2, '2'), qt.IsFalse)
	c.Assert(isNumberInitial(8, '7'), qt.IsTrue)
}

func TestIsLineEnding(t *testing.T) {
	c := qt.New(t)
	c.Assert(isLineEnding('\n'), qt.IsTrue)
	c.Assert(isLineEnding('\r'), qt.IsTrue)
	c.Assert(isLineEnding(' '), qt.IsFalse)
}

func TestIsInitial(t *testing.T) {
	c := qt.New(t)
	c.Assert(isInitial('a'), qt.IsTrue)
	c.Assert(isInitial('!'), qt.IsTrue)
	c.Assert(isInitial('1'), qt.IsFalse)
}

func TestIsSubsequent(t *testing.T) {
	c := qt.New(t)
	c.Assert(isSubsequent('a'), qt.IsTrue)
	c.Assert(isSubsequent('1'), qt.IsTrue)
	c.Assert(isSubsequent('.'), qt.IsTrue)
	c.Assert(isSubsequent('+'), qt.IsTrue)
	c.Assert(isSubsequent('@'), qt.IsTrue)
}

// TestIsCommentToken tests the isCommentToken function
func TestIsCommentToken(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{
			input:        "; comment\n123",
			expectedType: TokenizerStateLineCommentBody,
		},
		{
			input:        "#| comment |#456",
			expectedType: TokenizerStateBlockCommentBody,
		},
		{
			input:        "#;(datum)",
			expectedType: TokenizerStateDatumCommentBegin,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizerWithComments(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}
