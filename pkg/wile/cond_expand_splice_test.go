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
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// Gate (P5.8, I166-R2): the selected clause of a cond-expand occurs in place of
// the form (R7RS §4.2.1), so what it defines is visible to the rest of the
// includer's unit. Before the splice the body expanded in a unit of its own.
func TestCondExpandSplicesInPlace(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			name: "a define-syntax in the selected clause binds for the forms after it",
			code: `(cond-expand (else (define-syntax mm (syntax-rules () ((_) 42))))) (mm)`,
			want: "42",
		},
		{
			name: "a define in the selected clause is accepted by a later set!",
			code: `(cond-expand (else (define yy 1))) (set! yy 2) yy`,
			want: "2",
		},
		{
			name: "a body-position macro from the clause binds",
			code: `(define (f) (cond-expand (else (define-syntax mm (syntax-rules () ((_) 7))))) (mm)) (f)`,
			want: "7",
		},
		{
			name: "a body-position define is a body define",
			code: `(define (f) (cond-expand (else (define a 1))) a) (f)`,
			want: "1",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fstest.MapFS{}, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

// Decision (P5.8): an expression-position cond-expand gets the same body
// treatment as (begin ...) in any position. graph.scm's
// (or (cond-expand ...) ...) is that shape.
func TestCondExpandInExpressionPosition(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "selected clause value inside or", code: `(or (cond-expand (nosuchfeature 1) (else #f)) 5)`, want: "5"},
		{name: "selected clause value is the or's", code: `(or (cond-expand (r7rs 3) (else #f)) 5)`, want: "3"},
		{name: "last body form is the value", code: `(+ 1 (cond-expand (else 2 3)))`, want: "4"},
		{name: "empty selected body is a void between forms", code: `(begin (cond-expand (else)) 1)`, want: "1"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fstest.MapFS{}, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

// Pin (P5.8, RED after a naive splice rather than on master): the splice head is
// a begin with a FRESH scope, so a user's begin cannot capture it.
func TestCondExpandSpliceHeadIsNotCapturable(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "top-level (define begin 9)", code: `(define begin 9) (cond-expand (else (define zz 3))) zz`, want: "3"},
		{name: "a let-bound begin", code: `(let ((begin 5)) (cond-expand (else begin)))`, want: "5"},
		{name: "a let-bound begin as a procedure", code: `(let ((begin (lambda args 'shadowed))) (cond-expand (else 1 2)))`, want: "2"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fstest.MapFS{}, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

// Pin (P5.8, W3-condexpand-overmark): a mention in an UNSELECTED clause no longer
// turns top-level immutability off for that name. The set! in a later unit
// (setter.scm, loaded) was accepted on master because opaque_subtree.go marked
// every name any clause mentions; it is now refused, as the same program without
// the cond-expand line is. The deliberate tightening, in the restrictive
// direction.
func TestCondExpandUnselectedClauseDoesNotWithdrawImmutability(t *testing.T) {
	fsys := fstest.MapFS{
		"setter.scm": &fstest.MapFile{Data: []byte(`(set! cx 3)`)},
	}
	_, err := evalIncludeProgram(t, fsys, `(define cx 1) (cond-expand (nosuchfeature cx) (else 0)) (load "setter.scm") cx`)
	qt.Assert(t, err, qt.ErrorMatches, `(?s).*immutable top-level binding.*`)

	_, err = evalIncludeProgram(t, fsys, `(define cx 1) (load "setter.scm") cx`)
	qt.Assert(t, err, qt.ErrorMatches, `(?s).*immutable top-level binding.*`, qt.Commentf("control: without the cond-expand line"))
}

// Pin (P5.8): a relative include under cond-expand in an included file resolves
// against the file's directory, on the OS filesystem, with a decoy of the same
// name in the includer's directory.
func TestCondExpandIncludeChainsRelativeToTheIncludedFile(t *testing.T) {
	fsys := fstest.MapFS{
		"sub/b.scm": &fstest.MapFile{Data: []byte(`(cond-expand (else (include "c.scm")))`)},
		"sub/c.scm": &fstest.MapFile{Data: []byte(`(define cv 9)`)},
		"c.scm":     &fstest.MapFile{Data: []byte(`(define cv 1)`)},
	}
	got, err := evalIncludeProgram(t, fsys, `(include "sub/b.scm") cv`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "9")
}

// Entry requirement (P5.6): resolveCondExpandClause refuses an improper clause
// list, on the expander path and on the library-body path, both of which reach it
// with no validator in front.
func TestCondExpandRefusesAnImproperClauseList(t *testing.T) {
	t.Run("top level", func(t *testing.T) {
		_, err := evalIncludeProgram(t, fstest.MapFS{}, `(cond-expand (r7rs 1) . 5)`)
		qt.Assert(t, err, qt.ErrorMatches, `(?s).*cond-expand.*proper list.*`)
	})

	t.Run("library body", func(t *testing.T) {
		fsys := fstest.MapFS{
			"probe/improper.sld": &fstest.MapFile{Data: []byte(`(define-library (probe improper)
  (import (scheme base))
  (export a)
  (cond-expand (r7rs (begin (define a 1))) . 5))`)},
		}
		ctx := context.Background()
		engine, err := wile.NewEngine(ctx,
			wile.WithProfile(wile.KitchenSink),
			wile.WithAuthorizer(nil),
			wile.WithSourceFS(stdlib.FS),
			wile.WithSourceFS(fsys),
			wile.WithLibraryPaths("."),
		)
		qt.Assert(t, err, qt.IsNil)
		_, err = engine.EvalMultiple(ctx, `(import (probe improper)) a`)
		qt.Assert(t, err, qt.ErrorMatches, `(?s).*cond-expand.*proper list.*`)
	})
}

// Pin (P5.8): the shapes the retired validator used to refuse are refused by the
// expander, each at the form.
func TestCondExpandMalformedForms(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "no clauses", code: `(cond-expand)`, want: `(?s).*cond-expand: no clauses.*`},
		{name: "a clause that is not a list", code: `(cond-expand 42)`, want: `(?s).*cond-expand: clause must be a list.*`},
		{name: "no clause matches", code: `(cond-expand (nosuchfeature 1))`, want: `(?s).*cond-expand: no matching clause.*`},
		{name: "else is not last", code: `(cond-expand (else 1) (r7rs 2))`, want: `(?s).*else must be the last clause.*`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := evalIncludeProgram(t, fstest.MapFS{}, tc.code)
			qt.Assert(t, err, qt.ErrorMatches, tc.want)
		})
	}
}
