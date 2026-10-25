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
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
)

// TestCompoundDatumSpansToItsCloser pins a compound datum's source span: its
// context was minted from the opener token and kept the opener's End, so
// syntax-span answered 1 for (a (b c)) (racket: 9). A list, vector, bytevector,
// box, quote form and cyclic label wrapper each end where their datum ends;
// Start is the opener's, so syntax-line and syntax-column are unchanged.
func TestCompoundDatumSpansToItsCloser(t *testing.T) {
	tcs := []struct {
		src         string
		start, span int
	}{
		{"(a (b c))", 0, 9},
		{"#(a b c)", 0, 8},
		{"'(a b)", 0, 6},
		{"(a . b)", 0, 7},
		{"[a b]", 0, 5},
		{"#u8(1 2)", 0, 8},
		{"#u8()", 0, 5},
		{"#&(a)", 0, 5},
		{"#0=(a . #0#)", 0, 12},
		{"  (x)", 2, 3},
		{"'()", 0, 3},
		{"#&()", 0, 4},
		{"'#0=(a . #0#)", 0, 13},
		{"abcdef", 0, 6}, // control: an atom already spanned its token
	}
	for _, tc := range tcs {
		t.Run(tc.src, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.src))
			stx, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			sc := stx.SourceContext()
			c.Assert(sc, qt.IsNotNil)
			c.Assert(sc.Start.Index(), qt.Equals, tc.start)
			c.Assert(sc.End.Index()-sc.Start.Index(), qt.Equals, tc.span)
		})
	}
}

// TestQuoteOfALabelReferenceSpansItsOwnText: '#0# hands back the labelled node,
// which ends before the quote mark, so a span taken from the child came out
// negative. The form spans from ' to the reference's last token.
func TestQuoteOfALabelReferenceSpansItsOwnText(t *testing.T) {
	for _, src := range []string{"(#0=(a) '#0#)", "(#0=(a) #&#0#)", "(#0=(a) `#0#)"} {
		t.Run(src, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(src))
			stx, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			second := stx.(*syntax.SyntaxPair).SyntaxCdr().(*syntax.SyntaxPair).SyntaxCar()
			sc := second.SourceContext()
			c.Assert(sc.Start.Index(), qt.Equals, 8)
			c.Assert(sc.End.Index()-sc.Start.Index() > 0, qt.IsTrue, qt.Commentf("span %d", sc.End.Index()-sc.Start.Index()))
		})
	}
}
