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
	"strings"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// evalIncludeProgram runs code as ONE compilation unit (EvalProgram, the file
// semantics) against an in-memory source tree. Whole-unit evaluation is the
// point: the include splice is an expand-time rewrite, so everything it fixes
// shows only when the includer and the included file are expanded together.
func evalIncludeProgram(t *testing.T, fsys fstest.MapFS, code string) (string, error) {
	t.Helper()
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithAuthorizer(nil), // the test exercises include resolution, not the sandbox
		wile.WithSourceFS(fsys),
	)
	qt.Assert(t, err, qt.IsNil)
	result, err := engine.EvalProgram(ctx, code, "includer.scm")
	if err != nil {
		return "", err
	}
	return result.SchemeString(), nil
}

// TestIncludeSplicesAtExpandTime pins I166-R1: (expand ...) of a form holding an
// (include ...) returns the included forms in its place, with no include head
// surviving. R7RS §4.1.7: the included expressions occur in place of the include.
func TestIncludeSplicesAtExpandTime(t *testing.T) {
	fsys := fstest.MapFS{
		"t6.scm": &fstest.MapFile{Data: []byte(`(define zz 5)`)},
	}
	tcs := []struct {
		name string
		head string
	}{
		{name: "include", head: "include"},
		{name: "include-ci", head: "include-ci"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fsys,
				`(syntax->datum (expand (datum->syntax #f '(lambda () (`+tc.head+` "t6.scm") 1))))`)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, strings.Contains(got, tc.head), qt.IsFalse, qt.Commentf("include head survived: %s", got))
			qt.Assert(t, strings.Contains(got, "(define zz 5)"), qt.IsTrue, qt.Commentf("included form missing: %s", got))
		})
	}
}

// TestMacroFromIncludedFileIsBound pins I166-R3: a macro defined by an included
// file is bound for the forms after the include in the SAME unit. On master the
// expander never saw the file, so (foo) failed as an unbound variable, while the
// REPL (one form per unit) answered 42.
func TestMacroFromIncludedFileIsBound(t *testing.T) {
	fsys := fstest.MapFS{
		"m.scm": &fstest.MapFile{Data: []byte(`(define-syntax foo (syntax-rules () ((_) 42)))`)},
	}
	got, err := evalIncludeProgram(t, fsys, `(include "m.scm") (foo)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "42")
}

// TestIncludeInBodyBoxesCapturedBinder pins I029: a binder an included file
// defines inside a body, captured by a closure the same file defines, is seen by
// the escape/box analysis. Master left it unboxed, so the closure captured the
// slot's initial #<void>.
func TestIncludeInBodyBoxesCapturedBinder(t *testing.T) {
	fsys := fstest.MapFS{
		"d2.scm": &fstest.MapFile{Data: []byte(`(define (h) x) (define x 1)`)},
		"d3.scm": &fstest.MapFile{Data: []byte(`(define (h) x) (define x 1) (define (h2) (h))`)},
	}
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "closure then binder, both read", code: `(define (f) (include "d2.scm") (list (h) x)) (f)`, want: "(1 1)"},
		{name: "closure only", code: `(define (f) (include "d2.scm") (h)) (f)`, want: "1"},
		{name: "closure through a second closure", code: `(define (f) (include "d3.scm") (list (h2) x)) (f)`, want: "(1 1)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fsys, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

// TestIncludeInBodyForwardCall pins I030: a define in an included file that calls
// a procedure defined LATER in the same file runs with letrec* semantics. Master
// compiled the body forms before the include's, and died with "application:
// expected a procedure, got #<void>".
func TestIncludeInBodyForwardCall(t *testing.T) {
	fsys := fstest.MapFS{
		"d4.scm": &fstest.MapFile{Data: []byte(`(define (a) (b)) (define (b) 42) (define r (a))`)},
	}
	got, err := evalIncludeProgram(t, fsys, `(define (f) (include "d4.scm") r) (f)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "42")
}

// TestTopLevelIncludeKeepsSetEvidence pins I136: top-level immutability evidence
// (a define and the set! on it) crosses the include boundary in every direction.
// On master the include was opaque to the unit's define/set! scan, so the set!
// was refused as a mutation of an immutable top-level binding.
func TestTopLevelIncludeKeepsSetEvidence(t *testing.T) {
	fsys := fstest.MapFS{
		"def.scm":  &fstest.MapFile{Data: []byte(`(define cnt 0)`)},
		"set.scm":  &fstest.MapFile{Data: []byte(`(set! cnt 1)`)},
		"both.scm": &fstest.MapFile{Data: []byte(`(define cnt 0) (set! cnt 1)`)},
	}
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "define in include, set! in includer", code: `(include "def.scm") (set! cnt 3) cnt`, want: "3"},
		{name: "define in includer, set! in include", code: `(define cnt 0) (include "set.scm") cnt`, want: "1"},
		{name: "define and set! both in include", code: `(include "both.scm") cnt`, want: "1"},
		{name: "define in one include, set! in another", code: `(include "def.scm") (include "set.scm") cnt`, want: "1"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fsys, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

// TestIncludedFormsSeeIncluderLocals pins the includer-to-included direction:
// the included forms carry the include form's scopes, so a binding local to the
// includer (a let variable, a body-local macro) is visible to them. Master read
// the file with an empty scope set and refused the reference.
func TestIncludedFormsSeeIncluderLocals(t *testing.T) {
	fsys := fstest.MapFS{
		"f.scm": &fstest.MapFile{Data: []byte(`(list x (m))`)},
	}
	tcs := []struct {
		name string
		code string
	}{
		{name: "let variable and body-local macro", code: `(let ((x 5)) (define-syntax m (syntax-rules () ((_) 42))) (include "f.scm"))`},
		{name: "let-syntax macro", code: `(let ((x 5)) (let-syntax ((m (syntax-rules () ((_) 42)))) (include "f.scm")))`},
		{name: "body define", code: `(let () (define x 5) (define-syntax m (syntax-rules () ((_) 42))) (include "f.scm"))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fsys, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, "(5 42)")
		})
	}
}

// TestIncludeSplicePins are the I166-risk pins, written before the splice and
// RED after a naive one.
//
//   - The splice head is dispatched by the binding it resolves to, so it carries a
//     fresh scope; without it (define begin 9) turns the splice into a call to 9.
//   - A relative include inside an included file resolves against THAT file's
//     directory, so the splice must keep the load stack pushed while the child
//     expander runs, not just while the file is read.
//   - W3-x10-macro-half: asserts the VALUE half only. The macro half (an `early`
//     body-local macro use before the include) stays red even with a literal
//     (begin ...), which is why no include-aware pre-scan exists.
func TestIncludeSplicePins(t *testing.T) {
	fsys := fstest.MapFS{
		"x10.scm":   &fstest.MapFile{Data: []byte(`(define (inside) 7)`)},
		"chain.scm": &fstest.MapFile{Data: []byte(`(include "sub/b.scm") (define chainv (+ bv 1))`)},
		"sub/b.scm": &fstest.MapFile{Data: []byte(`(include "c.scm") (define bv (+ cv 1))`)},
		"sub/c.scm": &fstest.MapFile{Data: []byte(`(define cv 9)`)},
	}
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "begin shadowed by the user", code: `(define begin 9) (include "x10.scm") (inside)`, want: "7"},
		{name: "chained relative include", code: `(include "chain.scm") chainv`, want: "11"},
		{name: "value half of the early-call shape", code: `(define (k) (define (early) (inside)) (include "x10.scm") (early)) (k)`, want: "7"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fsys, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}
