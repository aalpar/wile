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

package parser_test

import (
	"context"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/syntax"
)

// TestParserSeekToBeforeTheFirstRead: a parser over a reader that has already
// been partly consumed starts its positions where the reader stands.
func TestParserSeekToBeforeTheFirstRead(t *testing.T) {
	c := qt.New(t)
	p := parser.NewParser(nil, true, strings.NewReader("(x)"))
	c.Assert(p.SeekTo(syntax.NewSourceIndexes(12, 7, 3), 0), qt.IsTrue)

	got, err := p.ReadSyntax(context.Background())
	c.Assert(err, qt.IsNil)
	c.Assert(got.SourceContext().Start, qt.Equals, syntax.NewSourceIndexes(12, 7, 3))
}

// TestParserSeekToBetweenReads: after a datum and a release, SeekTo repositions
// the next datum to where another consumer left the reader.
func TestParserSeekToBetweenReads(t *testing.T) {
	c := qt.New(t)
	r := strings.NewReader("(a)\n  (b)")
	p := parser.NewParser(nil, true, r)

	_, err := p.ReadSyntax(context.Background())
	c.Assert(err, qt.IsNil)
	c.Assert(p.ReleaseLookahead(r), qt.IsNil)
	for range 3 {
		_, _, err = r.ReadRune()
		c.Assert(err, qt.IsNil)
	}
	c.Assert(p.SeekTo(syntax.NewSourceIndexes(6, 2, 2), ' '), qt.IsTrue)

	got, err := p.ReadSyntax(context.Background())
	c.Assert(err, qt.IsNil)
	c.Assert(got.SourceContext().Start, qt.Equals, syntax.NewSourceIndexes(6, 2, 2))
}

// TestParserSeekToReportsAStaleTokenizer: a parser whose tokenizer still holds
// its lookahead (the release was never done or failed) cannot be repositioned,
// and says so.
func TestParserSeekToReportsAStaleTokenizer(t *testing.T) {
	c := qt.New(t)
	p := parser.NewParser(nil, true, strings.NewReader("(a) (b)"))

	_, err := p.ReadSyntax(context.Background())
	c.Assert(err, qt.IsNil)

	c.Assert(p.SeekTo(syntax.NewSourceIndexes(40, 40, 9), 'z'), qt.IsFalse)
}

// TestParserSeekToIgnoresADeadParser: death is permanent, so a seek neither
// revives the parser nor reports it stale.
func TestParserSeekToIgnoresADeadParser(t *testing.T) {
	c := qt.New(t)
	p := parser.NewParser(nil, true, strings.NewReader("(1 2] (3 4)"))

	_, err := p.ReadSyntax(context.Background())
	c.Assert(err, qt.IsNotNil)

	c.Assert(p.SeekTo(syntax.NewSourceIndexes(0, 0, 1), 0), qt.IsTrue)
	_, err = p.ReadSyntax(context.Background())
	c.Assert(err, qt.IsNotNil, qt.Commentf("a dead parser stays dead"))
}
