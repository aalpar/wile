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
)

// readOneRendered reads a single datum and renders it, so a table can compare
// reader output as text.
func readOneRendered(t *testing.T, p *Parser) (string, error) {
	t.Helper()
	q, err := p.ReadSyntax(context.TODO())
	if err != nil {
		return "", err
	}
	return q.UnwrapAll().SchemeString(), nil
}

// TestReader_DatumCommentDoesNotSwallowCloser pins that a datum comment with no
// datum leaves the close delimiter in place for the enclosing reader.
//
// The reader tolerates "#;" before a closer (R7RS §7.1.1 ⟨comment⟩ requires a
// following datum; the leniency is deliberate and preserved). It used to
// implement that tolerance by *consuming* the closer, which stepped the
// delimiter past checkDelimiterMatch: "[1 #;) 2]" returned (1 2) while the
// control "[1 2)" correctly errored, i.e. the reader silently accepted
// mismatched brackets.
func TestReader_DatumCommentDoesNotSwallowCloser(t *testing.T) {
	cases := []struct {
		name    string
		src     string
		want    string
		wantErr string
	}{
		{
			name:    "bracket mismatch behind a datum comment",
			src:     "[1 #;) 2]",
			wantErr: "mismatched delimiters",
		},
		{
			name:    "control: bracket mismatch with no datum comment",
			src:     "[1 2)",
			wantErr: "mismatched delimiters",
		},
		{
			name: "datum comment before the closer ends the list there",
			src:  "(1 #;) 2)",
			want: "(1)",
		},
		{
			name: "leniency preserved: a datum comment with no datum is not an error",
			src:  "(1 #;)",
			want: "(1)",
		},
		{
			name: "control: a datum comment with a datum still elides it",
			src:  "(1 #;2 3)",
			want: "(1 3)",
		},
		{
			name: "datum-comment-only list is the empty list",
			src:  "(#;)",
			want: "()",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.src))
			got, err := readOneRendered(t, p)
			if tc.wantErr != "" {
				c.Assert(err, qt.IsNotNil, qt.Commentf("ReadSyntax(%q) = %q, want an error", tc.src, got))
				c.Assert(err.Error(), qt.Contains, tc.wantErr)
				return
			}
			c.Assert(err, qt.IsNil)
			c.Assert(got, qt.Equals, tc.want)
		})
	}
}

// TestReader_DatumCommentClosesOnlyItsOwnList pins the rest of "(1 #;) 2)":
// the swallowed closer used to splice the following datum into the list, so the
// stream read as one form (1 2) instead of a list, a datum, and a stray closer.
func TestReader_DatumCommentClosesOnlyItsOwnList(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader("(1 #;) 2)"))

	first, err := readOneRendered(t, p)
	c.Assert(err, qt.IsNil)
	c.Assert(first, qt.Equals, "(1)")

	second, err := readOneRendered(t, p)
	c.Assert(err, qt.IsNil)
	c.Assert(second, qt.Equals, "2")

	_, err = p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "unexpected close )")
}
