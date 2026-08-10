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
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// TestReader_DatumCommentNodeAlwaysCarriesADatum pins that the comment-preserving
// mode never hands back a *SyntaxDatumComment whose Value is nil.
//
// This gate cannot be a Scheme program. readDatumComment is reached only when
// skipComments is false, and every production construction but one passes true;
// the one that does not (ParseLibrarySummary) type-switches the node away
// without touching Value. So the broken node is reachable only through the
// public pkg/parser API, by an embedder.
//
// It also cannot be written against "(#;)": inside a list the closer is
// consumed by the list reader's own advance and the form dies as an
// unterminated list. Only a *top-level* "#;" before a close delimiter lets the
// node escape.
//
// SyntaxDatumComment.SchemeString and .EqualTo both dereference Value, so the
// escaped node panics on any use.
func TestReader_DatumCommentNodeAlwaysCarriesADatum(t *testing.T) {
	cases := []struct {
		name string
		src  string
	}{
		{"top level, the shape that escapes", "#;)"},
		{"top level, bracket closer", "#;]"},
		{"inside a list", "(#;)"},
		{"inside a vector", "#(#;)"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, false, strings.NewReader(tc.src))
			q, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNotNil, qt.Commentf("ReadSyntax(%q) = %T with a nil error", tc.src, q))
			var perr *ParserError
			c.Assert(errors.As(err, &perr), qt.IsTrue,
				qt.Commentf("ReadSyntax(%q) returned %T, want a located *ParserError: %v", tc.src, err, err))
		})
	}
}

// TestReader_DatumCommentNodeIsUsable is the other half: when a datum comment
// does have its datum, the preserved node must survive the operations that used
// to panic on the nil-Value one.
func TestReader_DatumCommentNodeIsUsable(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, false, strings.NewReader("#;9 7"))
	q, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	dc, ok := q.(*syntax.SyntaxDatumComment)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", q))
	c.Assert(dc.Value, qt.IsNotNil)
	c.Assert(dc.SchemeString(), qt.Equals, "#; #'9")
	c.Assert(dc.EqualTo(dc), qt.IsTrue)
}

// TestReader_DatumCommentWithNoDatumUsesTheSharedSentinel keeps the new arm on
// the same sentinel as its siblings (readBoxInto's "box marker requires a
// datum", the numeric introducers', readQuoteForm's), so an embedder matching
// reader faults by class catches it.
func TestReader_DatumCommentWithNoDatumUsesTheSharedSentinel(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, false, strings.NewReader("#;)"))
	_, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrInvalidSyntax), qt.IsTrue, qt.Commentf("err = %v", err))
}
