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
	p.SeekTo(syntax.NewSourceIndexes(12, 7, 3), 0)

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
	for i := 0; i < 3; i++ {
		_, _, err = r.ReadRune()
		c.Assert(err, qt.IsNil)
	}
	p.SeekTo(syntax.NewSourceIndexes(6, 2, 2), ' ')

	got, err := p.ReadSyntax(context.Background())
	c.Assert(err, qt.IsNil)
	c.Assert(got.SourceContext().Start, qt.Equals, syntax.NewSourceIndexes(6, 2, 2))
}
