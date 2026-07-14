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
	"fmt"
	"io"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
)

func TestDatumComment(t *testing.T) {
	c := qt.New(t)

	tok := NewTokenizer(strings.NewReader("#;"), false)
	token, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token.Type(), qt.Equals, TokenizerStateDatumCommentBegin)
}

func TestLineCommentEmitTokens(t *testing.T) {
	c := qt.New(t)

	// Simple line comment: ; comment
	tok := NewTokenizer(strings.NewReader("; this is a comment\n"), false)

	// Should get LineCommentBody (the comment content)
	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateLineCommentBody)
	c.Assert(token1.String(), qt.Equals, "; this is a comment")

	// Should get EOF
	_, err = tok.Next()
	c.Assert(err, qt.Equals, io.EOF)
}

func TestLineCommentMultipleSemicolons(t *testing.T) {
	c := qt.New(t)

	// Multiple semicolons: ;;; comment
	tok := NewTokenizer(strings.NewReader(";;; triple\n"), false)

	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateLineCommentBody)
	c.Assert(token1.String(), qt.Equals, ";;; triple")
}

func TestLineCommentAtEOF(t *testing.T) {
	c := qt.New(t)

	// Comment without trailing newline (EOF terminates)
	tok := NewTokenizer(strings.NewReader("; no newline"), false)

	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateLineCommentBody)
	c.Assert(token1.String(), qt.Equals, "; no newline")

	// No End token at EOF - just returns EOF directly
	_, err = tok.Next()
	c.Assert(err, qt.Equals, io.EOF)
}

func TestLineCommentEmpty(t *testing.T) {
	c := qt.New(t)

	// Empty comment (just semicolon and newline)
	tok := NewTokenizer(strings.NewReader(";\n"), false)
	token2, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token2.Type(), qt.Equals, TokenizerStateLineCommentBody)
	c.Assert(token2.String(), qt.Equals, ";")
}

func TestBlockCommentEmitTokens(t *testing.T) {
	c := qt.New(t)

	// Simple block comment: #| comment |#
	tok := NewTokenizer(strings.NewReader("#| block comment |#"), false)

	// Should get BlockCommentBody (the content)
	token2, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token2.Type(), qt.Equals, TokenizerStateBlockCommentBody)
	c.Assert(token2.String(), qt.Equals, "#| block comment |#")
}

func TestBlockCommentEmpty(t *testing.T) {
	c := qt.New(t)

	// Empty block comment: #||#
	tok := NewTokenizer(strings.NewReader("#||#"), false)
	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateBlockCommentBody)
	c.Assert(token1.String(), qt.Equals, "#||#")
}

func TestBlockCommentMultiline(t *testing.T) {
	c := qt.New(t)

	// Multiline block comment
	tok := NewTokenizer(strings.NewReader("#| line1\nline2\nline3 |#"), false)
	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateBlockCommentBody)
	c.Assert(token1.String(), qt.Equals, "#| line1\nline2\nline3 |#")
}

func TestBlockCommentNested(t *testing.T) {
	c := qt.New(t)

	// Nested block comment: #| outer #| inner |# outer |#
	tok := NewTokenizer(strings.NewReader("#| outer #| inner |# outer |#"), false)

	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateBlockCommentBody)
	c.Assert(token1.String(), qt.Equals, "#| outer #| inner |# outer |#")
}

func TestBlockCommentUnclosed(t *testing.T) {
	c := qt.New(t)

	// Unclosed block comment (EOF before |#). R7RS §2.2: the comment must be
	// closed, so EOF inside one is a read error, not a clean end of input.
	tok := NewTokenizer(strings.NewReader("#| unclosed"), false)
	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateBlockCommentBody)
	c.Assert(token1.String(), qt.Equals, "#| unclosed")

	_, err = tok.Next()
	c.Assert(errors.Is(err, io.EOF), qt.IsFalse)
	c.Assert(errors.Is(err, NewTokenizerError(MessageUnterminatedBlockComment)), qt.IsTrue)
	c.Assert(errors.Is(err, werr.ErrIncompleteInput), qt.IsTrue)
}

func TestDatumCommentEmitTokens(t *testing.T) {
	c := qt.New(t)

	// Datum comment: #; datum
	tok := NewTokenizer(strings.NewReader("#;42"), false)

	// Should get DatumCommentBegin
	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateDatumCommentBegin)
	c.Assert(token1.String(), qt.Equals, "#;")

	// The datum itself follows (parser would handle this)
	token2, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token2.Type(), qt.Equals, TokenizerStateUnsignedInteger)
	c.Assert(token2.String(), qt.Equals, "42")
}

func TestCommentFollowedByCode(t *testing.T) {
	c := qt.New(t)

	// Comment followed by code
	tok := NewTokenizer(strings.NewReader("; comment\n42"), false)

	token1, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token1.Type(), qt.Equals, TokenizerStateLineCommentBody)

	// Then the code
	token4, err := tok.Next()
	c.Assert(err, qt.IsNil)
	c.Assert(token4.Type(), qt.Equals, TokenizerStateUnsignedInteger)
	c.Assert(token4.String(), qt.Equals, "42")
}

// TestCommentPhases tests multi-phase comment tokenization
func TestCommentPhases(t *testing.T) {
	tcs := []struct {
		input          string
		expectedPhases []struct {
			typ TokenizerState
			str string
		}
	}{
		{
			input: "; line comment\n123",
			expectedPhases: []struct {
				typ TokenizerState
				str string
			}{
				{TokenizerStateLineCommentBody, "; line comment"},
				{TokenizerStateUnsignedInteger, "123"},
			},
		},
		{
			input: "#| block |#456",
			expectedPhases: []struct {
				typ TokenizerState
				str string
			}{
				{TokenizerStateBlockCommentBody, "#| block |#"},
				{TokenizerStateUnsignedInteger, "456"},
			},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			for j, expected := range tc.expectedPhases {
				token, err := tok.Next()
				c.Check(err, qt.IsNil, qt.Commentf("phase %d", j))
				c.Check(token.Type(), qt.Equals, expected.typ, qt.Commentf("phase %d", j))
				c.Check(token.String(), qt.Equals, expected.str, qt.Commentf("phase %d", j))
			}
		})
	}
}

// TestContinueCommentToken tests comment continuation
func TestContinueCommentToken(t *testing.T) {
	tcs := []struct {
		input string
		count int // number of tokens expected
	}{
		{
			input: "; comment\n",
			count: 2, // body, EOF
		},
		{
			input: "#| block |#",
			count: 2, // body, EOF
		},
	}
	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			count := 0
			for {
				q, err := tok.Next()
				if q == nil && err == nil {
					c.Fatal("Expected result or error, but got none")
				}
				count++
				if err == io.EOF {
					break
				}
				c.Check(err, qt.IsNil)
			}
			c.Check(count, qt.Equals, tc.count) // -1 because we don't count EOF
		})
	}
}

// TestContinueLineComment tests line comment phases
func TestContinueLineComment(t *testing.T) {
	input := "; this is a line comment\n"
	tok := NewTokenizer(strings.NewReader(input), false)

	// Phase 1: Body
	token2, err2 := tok.Next()
	qt.Check(t, err2, qt.IsNil)
	qt.Check(t, token2.Type(), qt.Equals, TokenizerStateLineCommentBody)
}

// TestContinueBlockComment tests block comment phases
func TestContinueBlockComment(t *testing.T) {
	input := "#| block comment |#"
	tok := NewTokenizer(strings.NewReader(input), false)

	// Phase 1: Body
	token2, err2 := tok.Next()
	qt.Check(t, err2, qt.IsNil)
	qt.Check(t, token2.Type(), qt.Equals, TokenizerStateBlockCommentBody)
}

// TestDatumCommentExtended tests datum comment tokenization
func TestDatumCommentExtended(t *testing.T) {
	tcs := []struct {
		input        string
		expectedType TokenizerState
	}{
		{input: "#;", expectedType: TokenizerStateDatumCommentBegin},
		{input: "#; ", expectedType: TokenizerStateDatumCommentBegin},
	}
	for i, tc := range tcs {
		qt.New(t).Run(fmt.Sprintf("%d: %q", i, tc.input), func(c *qt.C) {
			tok := NewTokenizer(strings.NewReader(tc.input), false)
			token, err := tok.Next()
			c.Check(err, qt.IsNil)
			c.Check(token.Type(), qt.Equals, tc.expectedType)
		})
	}
}

// Test continueCommentToken for better coverage
func TestTokenizer_ContinueCommentToken(t *testing.T) {
	// Line comment with content
	p := NewTokenizer(strings.NewReader("; a comment\n"), false)
	tok1, _ := p.Next()
	qt.Assert(t, tok1.Type(), qt.Equals, TokenizerStateLineCommentBody)

	// Block comment
	p = NewTokenizer(strings.NewReader("#| block |#"), false)
	tok1, _ = p.Next()
	qt.Assert(t, tok1.Type(), qt.Equals, TokenizerStateBlockCommentBody)
}

// Test block comment with multi-token mode
func TestTokenizer_BlockCommentMultiToken(t *testing.T) {
	p := NewTokenizer(strings.NewReader("#| content |#"), false)
	tok1, _ := p.Next()
	qt.Assert(t, tok1.Type(), qt.Equals, TokenizerStateBlockCommentBody)
}

// Test datum comments
func TestTokenizer_DatumComment(t *testing.T) {
	p := NewTokenizer(strings.NewReader("#;foo bar"), false)
	// In non-emit mode, datum comment is skipped
	tok, _ := p.Next()
	qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateDatumCommentBegin)

	p = NewTokenizer(strings.NewReader("#;foo bar"), false)
	tok, _ = p.Next()
	qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateDatumCommentBegin)
}

// Test additional comment scenarios
func TestTokenizer_CommentScenarios(t *testing.T) {
	// Line comment at EOF
	p := NewTokenizer(strings.NewReader("; comment"), false)
	tok1, _ := p.Next()
	qt.Assert(t, tok1.Type(), qt.Equals, TokenizerStateLineCommentBody)
	// No End token since there's no newline - should get EOF

	// Block comment without closing (incomplete)
	p = NewTokenizer(strings.NewReader("#| incomplete"), false)
	tok1, _ = p.Next()
	qt.Assert(t, tok1.Type(), qt.Equals, TokenizerStateBlockCommentBody)
}

// Test isCommentToken helper
func TestTokenizer_IsCommentTokenHelper(t *testing.T) {
	// Datum comment without emit mode
	p := NewTokenizer(strings.NewReader("#; comment\nfoo"), false)
	tok, _ := p.Next()
	// In non-emit mode, datum comment gets a single token
	qt.Assert(t, tok.Type(), qt.Equals, TokenizerStateDatumCommentBegin)
}

// TestInvalidComments tests error handling for malformed comments
func TestInvalidComments(t *testing.T) {
	tcs := []struct {
		name  string
		input string
	}{
		{
			name:  "unclosed_block_comment",
			input: "#| unclosed",
		},
		{
			name:  "unclosed_nested_block_comment",
			input: "#| outer #| inner |#",
		},
		{
			name:  "block_comment_only_open",
			input: "#|",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			p := NewTokenizer(strings.NewReader(tc.input), false)
			p.mark()
			p.read()
			// R7RS §2.2: an unclosed block comment is a read error, not EOF.
			c.Check(errors.Is(p.err, io.EOF), qt.IsFalse)
			c.Check(errors.Is(p.err, NewTokenizerError(MessageUnterminatedBlockComment)), qt.IsTrue)
		})
	}
}
