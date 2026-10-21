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

package io_test

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestReadSyntaxCountsWhatTheOtherReadersConsumed pins I185: a textual port owns
// its read position, so read-syntax reports where the PORT is, not where a cached
// tokenizer last stopped. Each row evaluates to a list of (line column) pairs read
// with syntax-line / syntax-column. The first three rows' expected positions were
// measured against Racket 9.2 with port-count-lines! on; the rest follow from the
// same rule (one position per port, advanced by every consumer).
//
// The first two rows are the gate (RED on master: (1 1) and (1 0 1 5)). The third
// is a pin: datum 1 at line 1 column 0 passes today and catches a SeekTo that
// moves a freshly constructed tokenizer one rune forward.
func TestReadSyntaxCountsWhatTheOtherReadersConsumed(t *testing.T) {
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want string
	}{
		{"read-char before the first read-syntax",
			`(let ((p (open-input-string "abc (x y)")))
			   (read-char p) (read-char p) (read-char p)
			   (let ((s (read-syntax p)))
			     (list (syntax-line s) (syntax-column s))))`,
			"(1 4)"},
		{"read-char between two read-syntax calls crosses a line",
			`(let ((p (open-input-string "(a b)\n  (c d)\n(e f)")))
			   (let ((s1 (read-syntax p)))
			     (read-char p) (read-char p) (read-char p)
			     (let ((s2 (read-syntax p)))
			       (list (syntax-line s1) (syntax-column s1)
			             (syntax-line s2) (syntax-column s2)))))`,
			"(1 0 2 2)"},
		{"pin: the first datum of an untouched port is at line 1 column 0",
			`(let ((s (read-syntax (open-input-string "(x y)"))))
			   (list (syntax-line s) (syntax-column s)))`,
			"(1 0)"},
		{"read also advances the shared position",
			`(let ((p (open-input-string "(a)\n(b) (c)")))
			   (read p)
			   (read-char p)
			   (read p)
			   (let ((s (read-syntax p)))
			     (list (syntax-line s) (syntax-column s))))`,
			"(2 4)"},
		{"peek-char leaves the position alone",
			`(let ((p (open-input-string "(a)  (b)")))
			   (read-syntax p)
			   (peek-char p) (peek-char p)
			   (let ((s (read-syntax p)))
			     (list (syntax-line s) (syntax-column s))))`,
			"(1 5)"},
		{"read-line consumes through the newline",
			`(let ((p (open-input-string "junk line\n  (z)")))
			   (read-line p)
			   (let ((s (read-syntax p)))
			     (list (syntax-line s) (syntax-column s))))`,
			"(2 2)"},
		{"read-string counts every character it returned",
			`(let ((p (open-input-string "12345 (w)")))
			   (read-string 6 p)
			   (let ((s (read-syntax p)))
			     (list (syntax-line s) (syntax-column s))))`,
			"(1 6)"},
		{"a tab read by read-char moves to the next tab stop",
			"(let ((p (open-input-string \"\\t(t)\")))\n" +
				"   (read-char p)\n" +
				"   (let ((s (read-syntax p)))\n" +
				"     (list (syntax-line s) (syntax-column s))))",
			"(1 8)"},
		{"a multibyte character is one column",
			"(let ((p (open-input-string \"\u00e9\u00e9 (m)\")))\n" +
				"   (read-char p) (read-char p)\n" +
				"   (let ((s (read-syntax p)))\n" +
				"     (list (syntax-line s) (syntax-column s))))",
			"(1 3)"},
		{"a CRLF read by read-char is one line ending",
			"(let ((p (open-input-string \"a\\r\\n  (c)\")))\n" +
				"   (read-char p) (read-char p) (read-char p)\n" +
				"   (let ((s (read-syntax p)))\n" +
				"     (list (syntax-line s) (syntax-column s))))",
			"(2 2)"},
		{"peek-char across a CRLF does not count the line twice",
			"(let ((p (open-input-string \"a\\r\\n  (c)\")))\n" +
				"   (read-char p) (read-char p)\n" +
				"   (peek-char p)\n" +
				"   (read-char p)\n" +
				"   (let ((s (read-syntax p)))\n" +
				"     (list (syntax-line s) (syntax-column s))))",
			"(2 2)"},
		{"read-char stops between the CR and the LF of a pair (first read)",
			"(let ((p (open-input-string \"a\\r\\n  (c)\")))\n" +
				"   (read-char p) (read-char p)\n" +
				"   (let ((s (read-syntax p)))\n" +
				"     (list (syntax-line s) (syntax-column s))))",
			"(2 2)"},
		{"read-char stops between the CR and the LF of a pair (later read)",
			"(let ((p (open-input-string \"(a)\\r\\n(b)\")))\n" +
				"   (read-syntax p)\n" +
				"   (read-char p)\n" +
				"   (let ((s (read-syntax p)))\n" +
				"     (list (syntax-line s) (syntax-column s))))",
			"(2 0)"},
		{"a bare CR is a line ending",
			"(let ((p (open-input-string \"a\\r  (c)\")))\n" +
				"   (read-char p) (read-char p)\n" +
				"   (let ((s (read-syntax p)))\n" +
				"     (list (syntax-line s) (syntax-column s))))",
			"(2 2)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal().SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestReadSyntaxAfterEOFKeepsCounting pins that evicting a port's cached parser
// at end of input does not reset the position: a port that hit EOF and is read
// again reports the same position the port itself holds.
func TestReadSyntaxAfterEOFKeepsCounting(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	result := eval(t, engine, `(let ((p (open-input-string "(a)")))
	  (read-syntax p)
	  (read-syntax p)
	  (eof-object? (read-syntax p)))`)
	c.Assert(result.Internal().SchemeString(), qt.Equals, "#t")
}
