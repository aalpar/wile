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
	"fmt"
	"io"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/tokenizer"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// TestReader_InvalidUTF8IsLocatedError pins the boundary contract for lexical
// errors: a lexical fault must surface as a located *ParserError, not leak the
// lower-layer *tokenizer.TokenizerError. The original cause stays matchable
// through the wrap chain, and rendering the error must not panic, including
// when the failure has no located token. Found by FuzzReadSyntax on "\x8b",
// which then raised a rune error before any token; an invalid byte now reads
// as U+FFFD, which starts no token, so the tokenless path is driven by a
// reader that fails before its first rune.
func TestReader_InvalidUTF8IsLocatedError(t *testing.T) {
	closed := werr.WrapForeignErrorf(werr.ErrPortClosed, "string-input-port is closed")
	tcs := []struct {
		name string
		rdr  io.RuneReader
		// tokenless says the fault precedes every token.
		tokenless bool
	}{
		{name: "invalid byte", rdr: strings.NewReader("\x8b")},
		{name: "reader fault before the first rune", rdr: &faultAfterReader{rdr: strings.NewReader("abc"), fault: closed}, tokenless: true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			recovered, err := readCatchingFrom(tc.rdr)
			if recovered != nil {
				t.Fatalf("reader panicked: %v", recovered)
			}
			var perr *ParserError
			if !errors.As(err, &perr) {
				t.Fatalf("got %T, want *ParserError: %v", err, err)
			}
			var terr *tokenizer.TokenizerError
			if !errors.As(err, &terr) {
				t.Errorf("error lost its *tokenizer.TokenizerError cause: %v", err)
			}
			if tc.tokenless && perr.tok != nil {
				t.Errorf("fault carries token %q, want none: the tokenless path is not exercised", perr.tok.String())
			}
			// Must not nil-deref, and must still carry a position. Without a
			// token the position comes from the TokenizerError asserted above,
			// which stamps it at the throw site.
			s := perr.SchemeString()
			if !strings.HasPrefix(s, "ParserError at 1:0:") {
				t.Errorf("render = %q, want a position recovered from the tokenizer error", s)
			}
			if perr.Location() != "1:0" {
				t.Errorf("Location() = %q, want 1:0 from the tokenizer error", perr.Location())
			}
		})
	}
}

// TestReader_EmptyGraphicCharIsLocatedError pins the empty-graphic-char crasher
// (#5): "#\0" reads as the character 0, then the trailing NUL — which starts no
// token at all — previously inherited the PREVIOUS token's CharGraphic type from
// the tokenizer's un-reset state field, producing a rune-less character literal
// that indexed rs[0] out of range. The tokenizer now resets its state per token,
// so the NUL is rejected as an unknown token rather than mis-typed; the property
// this test exists for is unchanged — a located *ParserError, never a panic and
// never acceptance. It surfaces on the datum AFTER "#\0", so a one-datum read
// would miss it — this reads to the first error like the fuzz target does.
// Found by FuzzReadSyntax.
func TestReader_EmptyGraphicCharIsLocatedError(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader("#\\0\x00"))
	var got error
	for range 4 {
		_, err := p.ReadSyntax(context.TODO())
		if err != nil {
			got = err
			break
		}
	}
	var perr *ParserError
	if !errors.As(got, &perr) {
		t.Fatalf("got %T, want a located *ParserError: %v", got, got)
	}
	if !errors.Is(got, ErrUnknownTokenType) {
		t.Errorf("got %v; want errors.Is ErrUnknownTokenType", got)
	}
}

// readCatching parses the first datum from src, recovering any panic so that a
// reader crash on malformed input registers as a test failure instead of
// aborting the whole test binary. The reader is on the runtime path via
// (read ...), which accepts untrusted input (R7RS §6.13.2), so a malformed
// datum must surface as a located error — never a Go panic.
func readCatching(src string) (recovered any, err error) {
	return readCatchingFrom(strings.NewReader(src))
}

// readCatchingFrom is readCatching for any reader.
func readCatchingFrom(rdr io.RuneReader) (recovered any, err error) {
	defer func() {
		recovered = recover()
	}()
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, rdr)
	_, err = p.ReadSyntax(context.TODO())
	return recovered, err
}

// TestReader_MalformedInputIsLocatedErrorNotPanic pins the untrusted-read
// robustness contract: each malformed datum must yield a located *ParserError,
// never a Go panic and never a silently mis-parsed value.
//
// Each case here is a confirmed defect on the parent commit:
//   - "#u8(1 2]"  nil-deref SIGSEGV in readByteVector (close bracket read as a
//     nil element, then stx.Unwrap()).
//   - "(( . ))"   SetCdr(nil) panic: an empty dotted pair has no car, and the
//     ')' after the dot is read as a nil cdr.
//   - "((1 . ))"  SetCdr(nil) panic: the cdr datum is missing; ')' is read as a
//     nil cdr that flows into SetCdr.
//   - "( . 5)"    silently mis-parses to (#<void>), discarding the 5: a dotted
//     pair with no car is accepted instead of rejected.
func TestReader_MalformedInputIsLocatedErrorNotPanic(t *testing.T) {
	cases := []struct {
		name string
		src  string
		// wantSentinel pins why the input is rejected, not merely that it is —
		// errors.As(*ParserError) alone would pass for an unrelated downstream
		// failure that rejected the right input for the wrong reason.
		wantSentinel error
	}{
		{"bytevector closed by bracket", "#u8(1 2]", werr.ErrNotACloseParen},
		{"bytevector unterminated at EOF", "#u8(1 2 3", io.ErrUnexpectedEOF},
		{"empty dotted pair (no car)", "(( . ))", werr.ErrNotACons},
		{"dotted pair missing cdr datum", "((1 . ))", werr.ErrNotACons},
		{"dot with no car", "( . 5)", werr.ErrNotACons},
		{"labeled list missing cdr datum", "#0=(1 . )", werr.ErrNotACons},
		{"labeled list trailing missing cdr", "#0=(1 2 . )", werr.ErrNotACons},
		// Found by FuzzReadWriteRoundTrip: a label assignment whose datum is a
		// close/cons delimiter was silently mis-parsed into a nil-datum label
		// (which then wrote as the non-re-readable "#<void>").
		{"label assignment no datum (close paren)", "#0=)", werr.ErrNotACons},
		{"label assignment no datum (close bracket)", "#0=]", werr.ErrNotACons},
		// Found by FuzzReadSyntax (host panics / leaks on the parent commit). The
		// fuzz corpus pins no-panic; these rows pin the *sentinel* so a regression
		// that rejects the right input for the wrong reason is also caught.
		{"quote form no datum", "' )", werr.ErrNotACons},
		// "#d" is a decimal radix marker with no number. It is now rejected at the
		// radix arm with the precise ErrNotANumber, in both plain and labeled lists
		// (previously the labeled path reported the generic ErrNotACons and the
		// plain path silently mis-parsed "(#d)" to "()").
		{"labeled list nil element (#d)", "#0=(#d)", werr.ErrNotANumber},
		{"plain list nil element (#d)", "(#d)", werr.ErrNotANumber},
		{"exactness marker no number", "#e)", werr.ErrNotANumber},
		{"rational zero denominator", "#b0/0", werr.ErrDivisionByZero},
		// (the empty-graphic-char crasher "#\0<NUL>" surfaces on a later datum,
		// not the first, so it is pinned by TestReader_EmptyGraphicCharIsLocatedError
		// rather than this one-datum table.)
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			recovered, err := readCatching(tc.src)
			if recovered != nil {
				t.Fatalf("reader panicked on %q: %v\n(must return a located error, not crash the host)", tc.src, recovered)
			}
			if err == nil {
				t.Fatalf("reader accepted malformed %q (want a located *ParserError)", tc.src)
			}
			var perr *ParserError
			if !errors.As(err, &perr) {
				t.Fatalf("reader returned %T for %q, want *ParserError: %v", err, tc.src, err)
			}
			if !errors.Is(err, tc.wantSentinel) {
				t.Errorf("reader error for %q = %v; want errors.Is %v", tc.src, err, tc.wantSentinel)
			}
		})
	}
}

// TestReader_EmptyListFormsAreEmptyList pins R7RS §7.1.2: a list form with zero
// datums reads as the empty list. Unlike the bare "()" — which the tokenizer
// emits as a single EmptyList token — "( )" with intervening whitespace, and
// comment-only forms, flow through readList with the element loop running zero
// times. Those must yield the empty list, not a (#<void>) orphan pair (which
// makes pair? #t, null? #f, length 1 — all wrong).
func TestReader_EmptyListFormsAreEmptyList(t *testing.T) {
	cases := []struct {
		name string
		src  string
	}{
		{"whitespace empty list", "( )"},
		{"whitespace empty bracket", "[ ]"},
		{"comment-only list", "( #;9 )"},
		{"multiple datum comments only", "(#;1 #;2)"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.src))
			q, err := p.ReadSyntax(context.TODO())
			if err != nil {
				t.Fatalf("ReadSyntax(%q) unexpected error: %v", tc.src, err)
			}
			if !syntax.IsSyntaxEmptyList(q) {
				t.Fatalf("ReadSyntax(%q) = %s (%T), want the empty list", tc.src, q.SchemeString(), q)
			}
		})
	}
}

// TestReader_ValidDottedAndCompoundFormsAccepted guards the dotted-pair and
// bytevector rejection guards against over-rejecting well-formed input. The
// circular labeled list is the load-bearing case: its cdr is a back-reference
// (#0#), not a missing datum, so the cdr == nil guard must NOT fire on it.
func TestReader_ValidDottedAndCompoundFormsAccepted(t *testing.T) {
	cases := []struct {
		name string
		src  string
	}{
		{"dotted pair", "(1 . 2)"},
		{"improper list", "(1 2 . 3)"},
		{"bytevector", "#u8(1 2 3)"},
		{"circular labeled list", "#0=(1 . #0#)"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.src))
			_, err := p.ReadSyntax(context.TODO())
			if err != nil {
				t.Fatalf("ReadSyntax(%q) rejected a valid form: %v", tc.src, err)
			}
		})
	}
}

// TestReader_EOFAfterPrefixIsReadError pins that a prefix or quote mark with
// nothing after it is malformed input, not a clean end of input. Returning a
// bare io.EOF made the caller treat "'" or "#e" as an empty source: `wile -e "'"`
// printed nothing and exited 0.
func TestReader_EOFAfterPrefixIsReadError(t *testing.T) {
	tcs := []struct {
		name string
		src  string
	}{
		{name: "quote", src: "'"},
		{name: "quasiquote", src: "`"},
		{name: "unquote", src: ","},
		{name: "syntax", src: "#'"},
		{name: "exact", src: "#e"},
		{name: "inexact", src: "#i"},
		{name: "binary", src: "#b"},
		{name: "octal", src: "#o"},
		{name: "decimal", src: "#d"},
		{name: "hex", src: "#x"},
		{name: "hex_then_exact", src: "#x#e"},
		{name: "label_assignment", src: "#0="},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := environment.NewNamespace().Runtime()
			p := NewParserWithFile(env, true, strings.NewReader(tc.src), "in.scm")
			_, err := p.ReadSyntax(context.TODO())
			if err == nil {
				t.Fatalf("reader accepted %q (want a located read error)", tc.src)
			}
			if errors.Is(err, io.EOF) {
				t.Fatalf("reader returned a clean io.EOF for %q; want a read error", tc.src)
			}
			var perr *ParserError
			if !errors.As(err, &perr) {
				t.Fatalf("reader returned %T for %q, want *ParserError: %v", err, tc.src, err)
			}
			if !errors.Is(err, io.ErrUnexpectedEOF) {
				t.Errorf("reader error for %q = %v; want errors.Is io.ErrUnexpectedEOF", tc.src, err)
			}
			// The prefix token is the only located token still in hand at EOF, so
			// it is what carries the error's provenance.
			if perr.Location() != "in.scm:1:0" {
				t.Errorf("reader error for %q located at %q; want in.scm:1:0", tc.src, perr.Location())
			}
		})
	}
}

// TestReader_MidParseEOFReportsOpenerLocation pins the opener's position on an
// unterminated compound form. wrapMidParseEOF used to be handed p.cur, which the
// tokenizer sets to nil at EOF, so ParserError.Location() came back empty and the
// error's provenance was lost (REVIEW.md "Error Chain Losslessness").
func TestReader_MidParseEOFReportsOpenerLocation(t *testing.T) {
	tcs := []struct {
		name string
		src  string
		want string
	}{
		{name: "list", src: "(1 2", want: "in.scm:1:0"},
		{name: "bracket_list", src: "  [1 2", want: "in.scm:1:2"},
		{name: "vector", src: "\n  #(1 2", want: "in.scm:2:2"},
		{name: "bytevector", src: "#u8(1 2", want: "in.scm:1:0"},
		{name: "nested_inner_opener", src: "(a (b", want: "in.scm:1:3"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := environment.NewNamespace().Runtime()
			p := NewParserWithFile(env, true, strings.NewReader(tc.src), "in.scm")
			_, err := p.ReadSyntax(context.TODO())
			var perr *ParserError
			if !errors.As(err, &perr) {
				t.Fatalf("reader returned %T for %q, want *ParserError: %v", err, tc.src, err)
			}
			if !errors.Is(err, io.ErrUnexpectedEOF) {
				t.Errorf("reader error for %q = %v; want errors.Is io.ErrUnexpectedEOF", tc.src, err)
			}
			if perr.Location() != tc.want {
				t.Errorf("reader error for %q located at %q; want %q", tc.src, perr.Location(), tc.want)
			}
		})
	}
}

// TestReader_EOFAfterIntroducerAndCommentIsReadError pins that an introducer
// whose datum never arrives is malformed input even when intertoken space, a
// comment, sits between the marker and the end of input. Only the introducer's
// first advance was guarded; its inner readSyntax skipped the comment, hit EOF
// and handed back a bare io.EOF, so `wile -e '#z;c'` printed nothing and exited
// 0, like an empty file. Racket and Chez both reject every row.
//
// Comments are skipped, the mode every production reader runs in. With comments
// preserved an introducer takes the comment itself as its datum, which is a
// separate defect; only the rows whose datum cannot arrive in either mode run
// there too, since #; is readDatumComment in that mode and readSyntax's skip
// loop in this one.
func TestReader_EOFAfterIntroducerAndCommentIsReadError(t *testing.T) {
	tcs := []struct {
		name string
		src  string
		// preserving also runs the row with comments preserved.
		preserving bool
	}{
		{name: "big_integer", src: "#z;c"},
		{name: "big_float", src: "#m;c"},
		{name: "exact", src: "#e;c"},
		{name: "inexact_block_comment", src: "#i #|c|#"},
		{name: "quote", src: "' ;c"},
		{name: "quasiquote", src: "` ;c"},
		{name: "unquote_splicing", src: ",@;c"},
		{name: "box", src: "#&;c"},
		{name: "datum_comment_then_comment", src: "#;;c"},
		{name: "datum_comment_alone", src: "#;", preserving: true},
		{name: "datum_comment_then_space", src: "#; ", preserving: true},
		{name: "datum_comment_of_datum_comment", src: "#;#;5"},
		{name: "label_assignment", src: "#0=;c"},
	}
	for _, skip := range fuzzSkipModes {
		for _, tc := range tcs {
			if !skip && !tc.preserving {
				continue
			}
			t.Run(fmt.Sprintf("%s/skip=%t", tc.name, skip), func(t *testing.T) {
				env := environment.NewNamespace().Runtime()
				p := NewParserWithFile(env, skip, strings.NewReader(tc.src), "in.scm")
				_, err := p.ReadSyntax(context.TODO())
				var perr *ParserError
				if !errors.As(err, &perr) {
					t.Fatalf("ReadSyntax(%q) = %v (%T); want a located *ParserError", tc.src, err, err)
				}
				if !errors.Is(err, io.ErrUnexpectedEOF) {
					t.Errorf("ReadSyntax(%q) = %v; want errors.Is io.ErrUnexpectedEOF", tc.src, err)
				}
				if errors.Is(err, io.EOF) {
					t.Errorf("ReadSyntax(%q) = %v; reads as a clean end of input", tc.src, err)
				}
				// The marker is the only located token in hand at EOF.
				if perr.Location() != "in.scm:1:0" {
					t.Errorf("ReadSyntax(%q) located at %q; want in.scm:1:0", tc.src, perr.Location())
				}
			})
		}
	}
}

// TestReader_TrailingCommentIsCleanEOF is the control for the test above: the
// wrap belongs at the introducer, not in readSyntax's comment loop, so a comment
// or a completed datum comment before the end of input is still a clean end.
func TestReader_TrailingCommentIsCleanEOF(t *testing.T) {
	for _, src := range []string{";c", "#|c|#", "#;5", "#;5 ;c", "#; ;c\n5", "#!fold-case"} {
		t.Run(src, func(t *testing.T) {
			env := environment.NewNamespace().Runtime()
			p := NewParserWithFile(env, true, strings.NewReader(src), "in.scm")
			_, err := p.ReadSyntax(context.TODO())
			if !errors.Is(err, io.EOF) {
				t.Fatalf("ReadSyntax(%q) = %v; want a clean io.EOF", src, err)
			}
			var perr *ParserError
			if errors.As(err, &perr) {
				t.Fatalf("ReadSyntax(%q) = %v; want a clean io.EOF, not a read error", src, err)
			}
		})
	}
}

// TestParserErrorLocationEscapesFileName pins the render-time file-name escape
// on ParserError.Location, the parser's own "file:line:col" renderer: a newline
// in the name must not split the diagnostic. A plain name prints unquoted.
func TestParserErrorLocationEscapesFileName(t *testing.T) {
	tcs := []struct {
		name, file, want string
	}{
		{name: "plain", file: "in.scm", want: "in.scm:1:0"},
		{name: "newline", file: "a\nb.scm", want: `"a\nb.scm":1:0`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := environment.NewNamespace().Runtime()
			p := NewParserWithFile(env, true, strings.NewReader("(1 2"), tc.file)
			_, err := p.ReadSyntax(context.TODO())
			var perr *ParserError
			if !errors.As(err, &perr) {
				t.Fatalf("reader returned %T, want *ParserError: %v", err, err)
			}
			if perr.Location() != tc.want {
				t.Errorf("located at %q; want %q", perr.Location(), tc.want)
			}
		})
	}
}

// TestReader_EOFAfterIntroducerInsideAFormNamesTheIntroducer pins the location
// inside an open form: the innermost introducer, not the enclosing list, is
// what is cut off (racket: "expected a commented-out element for `#;`").
func TestReader_EOFAfterIntroducerInsideAFormNamesTheIntroducer(t *testing.T) {
	tcs := []struct{ src, want string }{
		{"(1 #;", "in.scm:1:3"},
		{"(' ;c", "in.scm:1:1"},
	}
	for _, tc := range tcs {
		t.Run(tc.src, func(t *testing.T) {
			env := environment.NewNamespace().Runtime()
			p := NewParserWithFile(env, true, strings.NewReader(tc.src), "in.scm")
			_, err := p.ReadSyntax(context.TODO())
			var perr *ParserError
			if !errors.As(err, &perr) {
				t.Fatalf("reader returned %T for %q, want *ParserError: %v", err, tc.src, err)
			}
			if perr.Location() != tc.want {
				t.Errorf("%q located at %q; want %q", tc.src, perr.Location(), tc.want)
			}
		})
	}
}
