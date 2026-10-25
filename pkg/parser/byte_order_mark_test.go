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
	"github.com/aalpar/wile/pkg/syntax"
)

const bom = "\uFEFF"

// readAllSyntax reads every datum from p, stopping at end of input or at the
// first error, and reports the data read with the first datum's line and column.
func readAllSyntax(p *Parser) (data []string, line, col int, err error) {
	for {
		stx, rerr := p.ReadSyntax(context.TODO())
		if errors.Is(rerr, io.EOF) {
			return data, line, col, nil
		}
		if rerr != nil {
			return data, line, col, rerr
		}
		if len(data) == 0 {
			line = stx.SourceContext().Start.Line()
			col = stx.SourceContext().Start.Column()
		}
		data = append(data, stx.UnwrapAll().SchemeString())
	}
}

// TestReadSyntaxSkipsLeadingByteOrderMark pins X12: a UTF-8 byte order mark
// that opens the parser's input is skipped once, before any datum, and counted
// in positions (racket reports the first datum of such a file at column 1).
// A U+FEFF anywhere else is still the error it was: racket reads it as
// whitespace everywhere and petite as symbol text past the start, so only the
// start is common ground. Master raised "unknown token type" at 1:0 for all.
func TestReadSyntaxSkipsLeadingByteOrderMark(t *testing.T) {
	tcs := []struct {
		name     string
		src      string
		want     []string
		line     int
		col      int
		wantFail bool // the read stops on an unknown-token error at a U+FEFF
	}{
		{name: "before a datum", src: bom + "(define x 1)", want: []string{"(define x 1)"}, line: 1, col: 1},
		{name: "before several data", src: bom + "1 2", want: []string{"1", "2"}, line: 1, col: 1},
		{name: "before a newline", src: bom + "\n(a)", want: []string{"(a)"}, line: 2, col: 0},
		{name: "before a comment", src: bom + "; c\n(a)", want: []string{"(a)"}, line: 2, col: 0},
		{name: "alone", src: bom, want: nil},
		{name: "no mark", src: "(a)", want: []string{"(a)"}, line: 1, col: 0},
		{name: "literal U+FFFD in a string still reads", src: bom + "\"\uFFFD\"", want: []string{"\"\uFFFD\""}, line: 1, col: 1},
		// Not at the start: unchanged.
		{name: "two marks: only one is skipped", src: bom + bom + "(a)", wantFail: true},
		{name: "after whitespace", src: " " + bom + "(a)", wantFail: true},
		{name: "before the second datum", src: "(x) " + bom + "(y)", want: []string{"(x)"}, line: 1, col: 0, wantFail: true},
		{name: "inside a list", src: "(a" + bom + "b)", wantFail: true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			p := NewParser(environment.NewNamespace().Runtime(), true, strings.NewReader(tc.src))
			data, line, col, err := readAllSyntax(p)
			c.Check(data, qt.DeepEquals, tc.want)
			if len(tc.want) > 0 {
				c.Check([]int{line, col}, qt.DeepEquals, []int{tc.line, tc.col})
			}
			if !tc.wantFail {
				c.Assert(err, qt.IsNil)
				return
			}
			c.Assert(err, qt.IsNotNil)
			c.Check(errors.Is(err, tokenizer.NewTokenizerError(tokenizer.MessageExpectingToken)), qt.IsTrue, qt.Commentf("got: %v", err))
			c.Check(strings.Contains(err.Error(), "U+FEFF"), qt.IsTrue, qt.Commentf("got: %v", err))
		})
	}
}

// TestReadSyntaxKeepsByteOrderMarkPastTheOrigin pins that the skip is keyed to
// the start of the source, not to the parser's first read: a parser placed
// mid-stream (a port read after read-char took a character) reads a U+FEFF it
// meets first as it would anywhere else.
func TestReadSyntaxKeepsByteOrderMarkPastTheOrigin(t *testing.T) {
	c := qt.New(t)
	p := NewParser(environment.NewNamespace().Runtime(), true, strings.NewReader(bom+"(a)"))
	c.Assert(p.SeekTo(syntax.NewSourceIndexes(1, 1, 1), ' '), qt.IsTrue)
	_, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNotNil)
	c.Check(errors.Is(err, tokenizer.NewTokenizerError(tokenizer.MessageExpectingToken)), qt.IsTrue, qt.Commentf("got: %v", err))
}
