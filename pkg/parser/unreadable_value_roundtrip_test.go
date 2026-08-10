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
	"github.com/aalpar/wile/pkg/values"
)

// TestWriter_UnreadableValuesDoNotRenderAsDirectives pins that the writer never
// emits a "#!name" token for a value that has no read syntax.
//
// docs/reference/scheme.md states there is no #!void or #!eof read syntax, and
// every unrecognized "#!name" is read as a directive and discarded. The writer
// used to render both singletons as exactly such a token, so the writer
// manufactured input the reader silently deletes: (write (vector 1 (if #f #f) 2))
// printed "#(1 #!void 2)", which reads back as a *two*-element vector with no
// diagnostic. The unreadable-object convention the rest of the tree already uses
// ("#<record:point>", "#<char-set: ...>", "#<void>" for a nil vector slot) does
// not collide with the directive grammar.
func TestWriter_UnreadableValuesDoNotRenderAsDirectives(t *testing.T) {
	cases := []struct {
		name string
		v    values.Value
		want string
	}{
		{"void", values.Void, "#<void>"},
		{"eof object", values.EOFObject, "#<eof>"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			got, err := values.WriteValueToString(tc.v)
			c.Assert(err, qt.IsNil)
			c.Assert(got, qt.Equals, tc.want)
			c.Assert(strings.HasPrefix(got, values.PrefixDirective), qt.IsFalse,
				qt.Commentf("%q is read as a directive and discarded", got))
		})
	}
}

// TestWriter_VectorWithVoidElementSurvivesReadBack is the defect in its
// end-to-end form: a vector holding the void value must not print as something
// the reader accepts with a different length. The element count is the
// assertion; the rendering of the element itself is TestWriter_Unreadable...'s
// job.
func TestWriter_VectorWithVoidElementSurvivesReadBack(t *testing.T) {
	c := qt.New(t)
	vec := values.NewVector(values.NewInteger(1), values.Void, values.NewInteger(2))

	written, err := values.WriteValueToString(vec)
	c.Assert(err, qt.IsNil)

	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader(written))
	q, rerr := p.ReadSyntax(context.TODO())
	if rerr != nil {
		// Refusing to read back an unreadable object is the correct outcome:
		// what must never happen is reading it back as a *shorter* vector.
		return
	}
	reread, ok := q.UnwrapAll().(*values.Vector)
	c.Assert(ok, qt.IsTrue, qt.Commentf("re-read %q as %T", written, q.UnwrapAll()))
	c.Assert(reread.Length(), qt.Equals, 3,
		qt.Commentf("writer emitted %q, which silently drops an element on read-back", written))
}
