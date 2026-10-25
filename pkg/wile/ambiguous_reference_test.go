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

package wile_test

import (
	"errors"
	"fmt"
	"strings"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/werr"
)

// ambiguousDefM is Flatt's ambiguous-binding program (sets of scopes, 2016),
// with the inner macro's reference left as %s. def-m introduces x under its
// intro scope {a}; (m) defines given-x, which is the user's x, under m's intro
// scope {b}; the reference in m's template carries {a, b}. Both binders are
// subsets of it, neither of the other, and they have one scope each: no binding
// is maximal. Racket reports "x: identifier's binding is ambiguous" at the
// reference; Chez answers 1 (R6RS, not this model).
const ambiguousDefM = `(define-syntax def-m
  (syntax-rules ()
    ((_ m given-x)
     (begin
       (define x 1)
       (define-syntax m
         (syntax-rules ()
           ((_) (begin (define given-x 2) %s))))))))
`

// TestAmbiguousReferenceIsALocatedCompilationError pins B45 (X21): a reference
// whose scope set ties among incomparable binders is refused with
// ErrAmbiguousBinding located at the identifier. It used to panic in the
// environment's resolver and come back from the engine's recover as ErrInternal
// with no location. The reference is a template identifier, so its location is
// the (m) use that introduced it, as an unbound template identifier's is. The three rows are the three first resolver calls a
// reference meets: the global probe of CompileSymbol's scoped arm, its local
// probe, and set!'s.
func TestAmbiguousReferenceIsALocatedCompilationError(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "global reference", code: fmt.Sprintf(ambiguousDefM, "(display x)") + "(def-m m x)\n(m)",
			want: "includer.scm:10:1: "},
		{name: "local reference", code: "(define (f)\n" + fmt.Sprintf(ambiguousDefM, "x") + "(def-m m x)\n(m))\n(f)",
			want: "includer.scm:11:1: "},
		{name: "set! target", code: fmt.Sprintf(ambiguousDefM, "(set! x 3)") + "(def-m m x)\n(m)",
			want: "includer.scm:10:1: "},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := evalIncludeProgram(t, fstest.MapFS{}, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Check(t, errors.Is(err, werr.ErrAmbiguousBinding), qt.IsTrue, qt.Commentf("%v", err))
			qt.Check(t, errors.Is(err, werr.ErrInternal), qt.IsFalse, qt.Commentf("%v", err))
			qt.Check(t, err.Error(), qt.Contains, `"x"`)
			qt.Check(t, strings.Count(err.Error(), tc.want), qt.Equals, 1, qt.Commentf("%v", err))
			qt.Check(t, err.Error(), qt.Contains, "expanded from 'm' at "+strings.TrimSuffix(tc.want, ": "))
			var deepest, se *compilation.SourcedError
			for e := err; errors.As(e, &se); e = se.Cause {
				deepest = se
			}
			qt.Assert(t, deepest, qt.IsNotNil)
			got := fmt.Sprintf("%s:%d:%d: ", deepest.Source.File, deepest.Source.Start.Line(), deepest.Source.Start.Column())
			qt.Check(t, got, qt.Equals, tc.want)
		})
	}
}
