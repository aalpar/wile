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
	"io"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/tokenizer"
	"github.com/aalpar/wile/pkg/werr"
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
			"at in.scm:1:0", "index 0",
			`#\\qqq`, "char-mnemonic",
		},
	}, {
		// #z and #m carry their own rule and state, not the radix numeral's:
		// the diagnostic must say which rule broke and name the big-number state.
		name: "bigint_rational_leaves_a_second_datum", src: "#z1/2",
		wantLoc: "in.scm:1:3",
		wantText: []string{
			tokenizer.MessageExpectingDelimiterAfterBigNumber,
			"index 3", "line 1", "column 3",
			`'/'`, "U+002F",
			"big-integer",
		},
	}, {
		// A fault after a well-formed datum, on the second line.
		name: "bigfloat_complex_after_a_datum", src: "(a b)\n#m2+3i",
		wantLoc: "in.scm:2:3",
		wantText: []string{
			tokenizer.MessageExpectingDelimiterAfterBigNumber,
			"index 9", "line 2", "column 3",
			`'+'`, "U+002B",
			"big-float",
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

// TestReadErrorTextNamesTheFile pins that a read error's text names the file it
// failed in, not only its line and column: an error raised inside an included
// file or on a file port is rendered under some other file's prefix, or none,
// so the text is the only place the reader's own file can appear.
//
// A parser-level fault renders file:line:col in its suffix. A lexical fault
// yields its position to the tokenizer's located rune and adds only the file
// name, so the position is not reported twice. Unnamed input renders as before.
func TestReadErrorTextNamesTheFile(t *testing.T) {
	tcs := []struct {
		name    string
		file    string
		src     string
		want    []string
		mustNot []string
	}{{
		name: "parser_fault_names_file_line_col", file: "in.scm", src: "(a b)\n(foo ]",
		want:    []string{`(at in.scm:2:5, index 11, token "]", tokenizer state close-bracket)`},
		mustNot: []string{"line 2", "column 5"},
	}, {
		name: "lexical_fault_names_the_file_once", file: "in.scm", src: "(a b)\n#b19",
		want:    []string{"index 9, line 2, column 3", "(in file in.scm)"},
		mustNot: []string{"in.scm:2:3"},
	}, {
		name: "unnamed_input_is_unchanged", file: "", src: "(a b)\n(foo ]",
		want:    []string{`(at index 11, line 2, column 5, token "]", tokenizer state close-bracket)`},
		mustNot: []string{"in file"},
	}}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParserWithFile(env, true, strings.NewReader(tc.src), tc.file)
			var err error
			for err == nil {
				_, err = p.ReadSyntax(context.TODO())
			}
			rendered := err.Error()
			for _, want := range tc.want {
				c.Check(strings.Contains(rendered, want), qt.IsTrue,
					qt.Commentf("rendered %q lacks %q", rendered, want))
			}
			for _, bad := range tc.mustNot {
				c.Check(strings.Contains(rendered, bad), qt.IsFalse,
					qt.Commentf("rendered %q repeats %q", rendered, bad))
			}
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

// TestStampedFaultWinsOverTheTokenItCut pins the precedence rule for a token a
// scanner fault cut short and that then fails to parse in its own right. The
// fault is the diagnosis; the token's parse failure is only its consequence
// ("#m1ex" stops at the x, and "#m1e" is not a number). ReadSyntax used to
// consult the pending fault only after a successful datum, so these reported
// the truncated token at its start and the stamped rune never appeared.
//
// The parser's own error stays in the chain: its sentinel is still matchable.
// Only a delimiter fault takes precedence; the unknown_character_mnemonic row of
// TestReadErrorNamesPositionCharacterAndState is the control for the rest.
func TestStampedFaultWinsOverTheTokenItCut(t *testing.T) {
	tcs := []struct {
		name      string
		src       string
		wantLoc   string
		wantMess  string
		wantText  []string
		wantCause error
	}{{
		name: "bigfloat_exponent_without_digits", src: "#m1ex",
		wantLoc:   "in.scm:1:4",
		wantMess:  tokenizer.MessageExpectingDelimiterAfterBigNumber,
		wantText:  []string{"index 4", `'x'`, "big-float"},
		wantCause: werr.ErrInvalidFormat,
	}, {
		name: "bigint_second_sign", src: "#z--5",
		wantLoc:  "in.scm:1:3",
		wantMess: tokenizer.MessageExpectingDelimiterAfterBigNumber,
		wantText: []string{"index 3", `'-'`, "big-integer"},
	}, {
		name: "bigint_fraction_then_letter", src: "#z1.5x",
		wantLoc:  "in.scm:1:5",
		wantMess: tokenizer.MessageExpectingDelimiterAfterBigNumber,
		wantText: []string{"index 5", `'x'`, "big-integer"},
	}, {
		name: "radix_sign_then_out_of_radix_digit", src: "#b-2",
		wantLoc:  "in.scm:1:3",
		wantMess: tokenizer.MessageExpectingDelimiterAfterNumber,
		wantText: []string{"index 3", `'2'`},
	}, {
		name: "inside_a_list", src: "(1 #m1ex)",
		wantLoc:  "in.scm:1:7",
		wantMess: tokenizer.MessageExpectingDelimiterAfterBigNumber,
		wantText: []string{"index 7", `'x'`},
	}, {
		// The token fails to parse here whether cut or not (a BigInteger is not
		// a byte); the lexical fault still precedes it, as in racket and petite.
		name: "bytevector_bigint_then_hash", src: "#u8(#z1#t)",
		wantLoc:  "in.scm:1:7",
		wantMess: tokenizer.MessageExpectingDelimiterAfterBigNumber,
		wantText: []string{"index 7", `'#'`, "big-integer"},
	}, {
		// A fault in the token after a dotted tail: the improper-list error
		// does not shadow it either.
		name: "dotted_tail_then_cut_token", src: "(a . b #z1#t)",
		wantLoc:  "in.scm:1:10",
		wantMess: tokenizer.MessageExpectingDelimiterAfterBigNumber,
		wantText: []string{"index 10", `'#'`, "big-integer"},
	}, {
		// The radix complex token does not parse yet (strconv on "1+2i"); the
		// '#' that cut it is the reported fault, as racket ("misplaced") and
		// petite ("invalid number syntax") reject the input.
		name: "radix_complex_then_hash", src: "#x1+2i#t",
		wantLoc:  "in.scm:1:6",
		wantMess: tokenizer.MessageExpectingDelimiterAfterNumber,
		wantText: []string{"index 6", `'#'`, "unsigned-complex"},
	}, {
		// Control: a token that parses with a fault pending still reports the
		// fault (the success path, which takes any pending fault).
		name: "control_code_point_after_a_valid_token", src: `#\x110000`,
		wantLoc:  "in.scm:1:9",
		wantMess: tokenizer.MessageCodePointExceedsUnicodeMaximum,
		wantText: []string{"index 9", "char-hex-escape"},
	}}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)

			err := readUntilError(t, tc.src)

			// An error matching io.EOF reads as a clean end, and the fault is lost.
			c.Assert(errors.Is(err, io.EOF), qt.IsFalse, qt.Commentf("%v", err))
			var perr *ParserError
			c.Assert(errors.As(err, &perr), qt.IsTrue,
				qt.Commentf("reading %q gave %T: %v", tc.src, err, err))
			c.Check(perr.Location(), qt.Equals, tc.wantLoc)
			c.Check(errors.Is(err, tokenizer.NewTokenizerError(tc.wantMess)), qt.IsTrue,
				qt.Commentf("stamped fault not in the chain: %v", err))
			rendered := err.Error()
			for _, want := range append([]string{tc.wantMess}, tc.wantText...) {
				c.Check(strings.Contains(rendered, want), qt.IsTrue,
					qt.Commentf("rendered %q lacks %q", rendered, want))
			}
			if tc.wantCause != nil {
				c.Check(errors.Is(err, tc.wantCause), qt.IsTrue,
					qt.Commentf("parser's own cause dropped: %v", err))
			}
		})
	}
}
