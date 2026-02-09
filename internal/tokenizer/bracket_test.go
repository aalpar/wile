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
	"io"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestTokenizer_Brackets tests R7RS §2.1 square bracket support.
// Square brackets [ and ] are equivalent to ( and ) but must match.
func TestTokenizer_Brackets(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name   string
		in     string
		tokens []TokenizerState
		src    []string
	}{
		{
			name:   "open bracket",
			in:     "[",
			tokens: []TokenizerState{TokenizerStateOpenBracket},
			src:    []string{"["},
		},
		{
			name:   "close bracket",
			in:     "]",
			tokens: []TokenizerState{TokenizerStateCloseBracket},
			src:    []string{"]"},
		},
		{
			name:   "empty bracket list",
			in:     "[]",
			tokens: []TokenizerState{TokenizerStateEmptyList},
			src:    []string{"[]"},
		},
		{
			name:   "bracket list with elements",
			in:     "[1 2 3]",
			tokens: []TokenizerState{TokenizerStateOpenBracket, TokenizerStateUnsignedInteger, TokenizerStateUnsignedInteger, TokenizerStateUnsignedInteger, TokenizerStateCloseBracket},
			src:    []string{"[", "1", "2", "3", "]"},
		},
		{
			name:   "bracket improper list",
			in:     "[a . b]",
			tokens: []TokenizerState{TokenizerStateOpenBracket, TokenizerStateSymbol, TokenizerStateCons, TokenizerStateSymbol, TokenizerStateCloseBracket},
			src:    []string{"[", "a", ".", "b", "]"},
		},
		{
			name:   "nested brackets",
			in:     "[[a] [b]]",
			tokens: []TokenizerState{TokenizerStateOpenBracket, TokenizerStateOpenBracket, TokenizerStateSymbol, TokenizerStateCloseBracket, TokenizerStateOpenBracket, TokenizerStateSymbol, TokenizerStateCloseBracket, TokenizerStateCloseBracket},
			src:    []string{"[", "[", "a", "]", "[", "b", "]", "]"},
		},
		{
			name:   "mixed parens and brackets",
			in:     "([a] (b))",
			tokens: []TokenizerState{TokenizerStateOpenParen, TokenizerStateOpenBracket, TokenizerStateSymbol, TokenizerStateCloseBracket, TokenizerStateOpenParen, TokenizerStateSymbol, TokenizerStateCloseParen, TokenizerStateCloseParen},
			src:    []string{"(", "[", "a", "]", "(", "b", ")", ")"},
		},
		{
			name:   "quote with bracket",
			in:     "'[a b]",
			tokens: []TokenizerState{TokenizerStateQuote, TokenizerStateOpenBracket, TokenizerStateSymbol, TokenizerStateSymbol, TokenizerStateCloseBracket},
			src:    []string{"'", "[", "a", "b", "]"},
		},
		{
			name:   "quasiquote with bracket",
			in:     "`[,a ,@b]",
			tokens: []TokenizerState{TokenizerStateQuasiquote, TokenizerStateOpenBracket, TokenizerStateUnquote, TokenizerStateSymbol, TokenizerStateUnquoteSplicing, TokenizerStateSymbol, TokenizerStateCloseBracket},
			src:    []string{"`", "[", ",", "a", ",@", "b", "]"},
		},
		{
			name:   "nested empty list",
			in:     "[[]]",
			tokens: []TokenizerState{TokenizerStateOpenBracket, TokenizerStateEmptyList, TokenizerStateCloseBracket},
			src:    []string{"[", "[]", "]"},
		},
		{
			name:   "bracket with paren inside",
			in:     "[(a)]",
			tokens: []TokenizerState{TokenizerStateOpenBracket, TokenizerStateOpenParen, TokenizerStateSymbol, TokenizerStateCloseParen, TokenizerStateCloseBracket},
			src:    []string{"[", "(", "a", ")", "]"},
		},
	}

	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			p := NewTokenizer(strings.NewReader(tc.in), false)
			var tokens []TokenizerState
			var srcs []string
			for {
				tok, err := p.Next()
				if err != nil {
					c.Assert(err, qt.ErrorIs, io.EOF)
					break
				}
				tokens = append(tokens, tok.Type())
				srcs = append(srcs, tok.String())
			}
			c.Assert(tokens, qt.DeepEquals, tc.tokens)
			c.Assert(srcs, qt.DeepEquals, tc.src)
		})
	}
}
