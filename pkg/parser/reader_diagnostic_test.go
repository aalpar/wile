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
	"context"
	"errors"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/tokenizer"
)

// readUntilError reads data from src until a read fails, and returns that
// failure. A fault after a well-formed datum ("(a b)\n#b19") surfaces on a
// later ReadSyntax, so a single call would report success and prove nothing.
func readUntilError(t *testing.T, src string) error {
	t.Helper()
	env := environment.NewNamespace().Runtime()
	p := NewParserWithFile(env, true, strings.NewReader(src), "in.scm")
	for {
		_, err := p.ReadSyntax(context.TODO())
		if err != nil {
			return err
		}
	}
}

// TestReadErrorNamesPositionCharacterAndState pins the reader-diagnostic
// contract for the surface a user actually reads: every read error states where
// it failed, what it failed on, and which lexical state it failed in.
//
// The motivating report is the "#b109" row. It rendered as the shapeless
// "parse error: malformed input" with no location at all, because the tokenizer
// error carried no position and ParserError.Error never printed its cause.
func TestReadErrorNamesPositionCharacterAndState(t *testing.T) {
	tcs := []struct {
		name     string
		src      string
		wantLoc  string
		wantText []string
	}{{
		name: "out_of_radix_digit", src: "#b109",
		wantLoc: "in.scm:1:4",
		wantText: []string{
			tokenizer.MessageExpectingDelimiterAfterNumber,
			"index 4", "line 1", "column 4",
			`'9'`, "U+0039",
			"unsigned-integer",
		},
	}, {
		name: "out_of_radix_digit_inside_list", src: "(#b109)",
		wantLoc: "in.scm:1:5",
		wantText: []string{
			tokenizer.MessageExpectingDelimiterAfterNumber,
			"index 5", `'9'`, "unsigned-integer",
		},
	}, {
		// The location must follow the line, not just the byte index.
		name: "fault_on_second_line", src: "(a b)\n#b19",
		wantLoc: "in.scm:2:3",
		wantText: []string{
			tokenizer.MessageExpectingDelimiterAfterNumber,
			"index 9", "line 2", "column 3", `'9'`,
		},
	}, {
		// A lexical fault that ends a token rather than following one: the
		// scanner's reason must survive, not be flattened to "unknown token
		// type", which describes only the token's shape.
		name: "unterminated_string", src: `"abc`,
		wantLoc: "in.scm:1:0",
		wantText: []string{
			tokenizer.MessageUnterminatedString,
			"index 4", "<end of input>",
		},
	}, {
		// A parser-level fault has no offending rune; the token is its
		// analogue, and its type is the state.
		name: "unknown_character_mnemonic", src: `#\qqq`,
		wantLoc: "in.scm:1:0",
		wantText: []string{
			"unknown character mnemonic",
			"index 0", "line 1", "column 0",
			`#\\qqq`, "char-mnemonic",
		},
	}}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)

			err := readUntilError(t, tc.src)

			var perr *ParserError
			c.Assert(errors.As(err, &perr), qt.IsTrue,
				qt.Commentf("reading %q gave %T: %v", tc.src, err, err))
			c.Check(perr.Location(), qt.Equals, tc.wantLoc)

			rendered := err.Error()
			for _, want := range tc.wantText {
				c.Check(strings.Contains(rendered, want), qt.IsTrue,
					qt.Commentf("rendered %q lacks %q", rendered, want))
			}
			// The generic phrase must never be the whole diagnostic: if the
			// specific cause is missing, the text is back to "malformed input".
			c.Check(rendered, qt.Not(qt.Equals), messageReadFailed)
		})
	}
}

// TestReadErrorTextAgreesWithUnwrapChain pins REVIEW.md's Error Chain
// Losslessness rule 1 on the reader: text that appears in Error() must be
// reachable through errors.As, and a cause reachable through errors.As must
// appear in the text. ParserError.Error used to return only its own message,
// so the tokenizer's diagnostic was traversable but invisible.
func TestReadErrorTextAgreesWithUnwrapChain(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	p := NewParserWithFile(env, true, strings.NewReader("#b109"), "in.scm")
	_, err := p.ReadSyntax(context.TODO())

	var terr *tokenizer.TokenizerError
	c.Assert(errors.As(err, &terr), qt.IsTrue,
		qt.Commentf("reader error lost its tokenizer cause: %v", err))
	c.Check(strings.Contains(err.Error(), terr.Error()), qt.IsTrue,
		qt.Commentf("cause %q reachable via errors.As but absent from text %q", terr, err))

	// The sentinel identity the message constants provide must survive too.
	c.Check(errors.Is(err, tokenizer.NewTokenizerError(
		tokenizer.MessageExpectingDelimiterAfterNumber)), qt.IsTrue)
}

// TestReadErrorDoesNotRepeatItsCause guards the readability of the rule above.
// The wrap that attaches ErrUnknownTokenType carries the sentinel's own text in
// its message already; rendering the cause unconditionally produced
// "unknown token type: \"#q\": unknown token type".
func TestReadErrorDoesNotRepeatItsCause(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	p := NewParserWithFile(env, true, strings.NewReader("#q"), "in.scm")
	_, err := p.ReadSyntax(context.TODO())

	c.Assert(err, qt.IsNotNil)
	c.Check(strings.Count(err.Error(), "unknown token type"), qt.Equals, 1,
		qt.Commentf("rendered %q", err))
}

// TestUnknownTokenKeepsIncompleteInputSignal pins the reason the unknown-token
// cause is a join rather than a replacement: pkg/wile's IsIncompleteInput
// matches ErrUnknownTokenType as the proxy for a token truncated by end of
// input, and that is what makes the REPL wait for a continuation line. Swapping
// in the tokenizer's error would have silently ended multi-line input.
func TestUnknownTokenKeepsIncompleteInputSignal(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	p := NewParserWithFile(env, true, strings.NewReader(`"abc`), "in.scm")
	_, err := p.ReadSyntax(context.TODO())

	c.Check(errors.Is(err, ErrUnknownTokenType), qt.IsTrue,
		qt.Commentf("lost the truncated-token signal: %v", err))
	c.Check(errors.Is(err, tokenizer.NewTokenizerError(
		tokenizer.MessageUnterminatedString)), qt.IsTrue,
		qt.Commentf("lost the specific lexical cause: %v", err))
}
