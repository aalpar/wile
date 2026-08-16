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
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// Gates for N-1 slices S2 and S3, the two remaining losslessness violations the
// unified-error-representation design measured:
//
//   - V2, irritants flattened: (syntax-error "msg" x 42) rendered its irritants
//     into the message text and error-object-irritants answered (), the one place
//     R7RS mandates the structured shape (§4.3.1).
//   - V3, the location rendered twice to the embedder: CompilationError.Error()
//     prefixed Source while the SourcedError chain underneath prefixed the same
//     location again.
//
// S3 also gives CompilationError the Condition field RuntimeError has carried
// since the exception-escape path was structured, so a Go caller reaches the same
// condition object a Scheme guard would catch.

// syntaxErrorEngine writes a file whose macro raises (syntax-error "expected a
// pair" <irritant> 42) at expansion time, and returns an engine plus that path.
// The error is raised by the expander, so load is what makes it catchable: a
// guard in the same top-level form would be compiled after the failure.
func syntaxErrorEngine(t *testing.T) (*wile.Engine, string) {
	t.Helper()
	dir := t.TempDir()
	path := filepath.Join(dir, "synerr.scm")
	src := "(define-syntax must-be-pair\n" +
		"  (syntax-rules ()\n" +
		"    ((must-be-pair (a . b)) 'ok)\n" +
		"    ((must-be-pair x) (syntax-error \"expected a pair\" x 42))))\n" +
		"(must-be-pair hello)\n"
	err := os.WriteFile(path, []byte(src), 0o600)
	qt.Assert(t, err, qt.IsNil)

	eng, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng, path
}

// S2's gate: error-object-irritants on a caught syntax-error is the two-element
// list the form named, holding the stripped datums rather than the syntax
// objects the expander was carrying.
func TestSyntaxErrorIrritantsReachScheme(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, path := syntaxErrorEngine(t)

	v, err := eng.EvalMultiple(ctx,
		`(guard (e (#t (error-object-irritants e))) (load "`+path+`"))`)
	c.Assert(err, qt.IsNil)

	tup, ok := v.Internal().(values.Tuple)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v.Internal()))

	var got []values.Value
	cur := values.Value(tup)
	for {
		pair, isPair := cur.(*values.Pair)
		if !isPair {
			break
		}
		got = append(got, pair.Car())
		cur = pair.Cdr()
	}
	c.Assert(got, qt.HasLen, 2, qt.Commentf("irritants: %s", v.Internal().SchemeString()))

	sym, ok := got[0].(*values.Symbol)
	c.Assert(ok, qt.IsTrue, qt.Commentf("first irritant is %T, want the stripped datum", got[0]))
	c.Assert(sym.Key, qt.Equals, "hello")

	num, ok := got[1].(*values.Integer)
	c.Assert(ok, qt.IsTrue, qt.Commentf("second irritant is %T", got[1]))
	c.Assert(num.SchemeString(), qt.Equals, "42")
}

// S3's gate, part (i): the Go envelope carries the condition, and it is the same
// object Scheme would see — same location, and the compiler's own error still
// reachable underneath it.
func TestCompilationErrorCarriesCondition(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})

	expr, err := eng.ParseWithSource(ctx, `(car)`, "<eval>")
	c.Assert(err, qt.IsNil)
	_, err = eng.Compile(ctx, expr)
	c.Assert(err, qt.IsNotNil)

	var ce *wile.CompilationError
	c.Assert(errors.As(err, &ce), qt.IsTrue, qt.Commentf("got %T", err))
	c.Assert(ce.Source, qt.Equals, "<eval>:1:1")
	c.Assert(ce.Condition, qt.IsNotNil,
		qt.Commentf("a compile failure must reach the embedder as a condition too"))

	ne, ok := ce.Condition.Internal().(*values.NativeError)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", ce.Condition.Internal()))
	c.Assert(ne.SourceLocation(), qt.Equals, ce.Source,
		qt.Commentf("the condition and the envelope must agree on the location"))
	c.Assert(errors.Is(error(ne), ce.Cause), qt.IsTrue,
		qt.Commentf("the condition must keep the compiler's error chain reachable"))
}

// S3's gate, part (ii): the §4.3 regression guard. A parse error's location comes
// from parser.ParserError, not from a SourcedError, and a funnel that routes the
// location through the condition builder instead re-opens the hole REVIEW.md
// records as closed on 2026-07-31.
func TestParseErrorStillCarriesSource(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})

	_, err = eng.Parse(ctx, `(car`)
	c.Assert(err, qt.IsNotNil)

	var ce *wile.CompilationError
	c.Assert(errors.As(err, &ce), qt.IsTrue, qt.Commentf("got %T", err))
	c.Assert(ce.Source, qt.Not(qt.Equals), "",
		qt.Commentf("the parser knew the position; dropping it is the closed defect"))
	c.Assert(strings.Count(ce.Error(), ce.Source), qt.Equals, 1)
}

// S3's third gate: the location renders exactly once. Both shapes V3 measured are
// covered — a direct compile, where the envelope and the innermost SourcedError
// hold the same location, and an include, where the failure is one file deeper
// than the form that triggered it.
func TestCompilationErrorRendersLocationOnce(t *testing.T) {
	ctx := context.Background()
	dir := t.TempDir()
	path := filepath.Join(dir, "bad.scm")
	err := os.WriteFile(path, []byte("(define (f) (car))\n"), 0o600)
	qt.Assert(t, err, qt.IsNil)

	tcs := []struct {
		name string
		code string
	}{
		{name: "direct compile", code: `(car)`},
		{name: "include", code: `(include "` + path + `")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
			c.Assert(err, qt.IsNil)
			t.Cleanup(func() {
				_ = eng.Close()
			})

			expr, err := eng.ParseWithSource(ctx, tc.code, "<eval>")
			c.Assert(err, qt.IsNil)
			_, err = eng.Compile(ctx, expr)
			c.Assert(err, qt.IsNotNil)

			var ce *wile.CompilationError
			c.Assert(errors.As(err, &ce), qt.IsTrue, qt.Commentf("got %T", err))
			c.Assert(ce.Source, qt.Not(qt.Equals), "")
			c.Assert(strings.Count(ce.Error(), ce.Source), qt.Equals, 1,
				qt.Commentf("rendered: %s", ce.Error()))
		})
	}
}
