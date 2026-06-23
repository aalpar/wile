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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// readCatching parses the first datum from src, recovering any panic so that a
// reader crash on malformed input registers as a test failure instead of
// aborting the whole test binary. The reader is on the runtime path via
// (read ...), which accepts untrusted input (R7RS §6.13.2), so a malformed
// datum must surface as a located error — never a Go panic.
func readCatching(src string) (recovered any, err error) {
	defer func() {
		recovered = recover()
	}()
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader(src))
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
