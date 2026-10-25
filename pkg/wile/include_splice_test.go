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
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/wile"
)

// evalIncludeProgram runs code as ONE compilation unit (EvalProgram, the file
// semantics) against an in-memory source tree. Whole-unit evaluation is the
// point: the include splice is an expand-time rewrite, so everything it fixes
// shows only when the includer and the included file are expanded together.
func evalIncludeProgram(t *testing.T, fsys fstest.MapFS, code string, opts ...wile.EngineOption) (string, error) {
	t.Helper()
	ctx := context.Background()
	opts = append([]wile.EngineOption{
		wile.WithProfile(wile.KitchenSink),
		wile.WithAuthorizer(nil), // the test exercises include resolution, not the sandbox
		wile.WithSourceFS(fsys),
	}, opts...)
	engine, err := wile.NewEngine(ctx, opts...)
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

// TestIncludePassedThroughAMacro pins the use-site pruning of spliced forms: an
// include handed to a macro as an operand has the macro's use-site scope on its
// head, and the file's forms inherit it. Unpruned, the spliced binders carry the
// scope and a reference outside the macro use cannot see them (Chez, Racket and
// master all answer 7).
func TestIncludePassedThroughAMacro(t *testing.T) {
	fsys := fstest.MapFS{
		"x.scm": &fstest.MapFile{Data: []byte(`(define (inside) 7)`)},
	}
	fsys["y.scm"] = &fstest.MapFile{Data: []byte(`(define (other) 8)`)}
	fsys["xu.scm"] = &fstest.MapFile{Data: []byte(`(DEFINE (INSIDE) 7)`)}
	fsys["chain.scm"] = &fstest.MapFile{Data: []byte(`(include "x.scm")`)}
	pass := `(define-syntax pass (syntax-rules () ((_ e) e))) `
	// The rows after the first two are the routes by which an include reaches a
	// splice position of the macro's output (X21): each marks the head, and a
	// lost mark leaves inside unbound.
	tcs := []struct {
		name string
		code string
	}{
		{name: "body position", code: pass + `(define (f) (pass (include "x.scm")) (inside)) (f)`},
		{name: "top level", code: pass + `(pass (include "x.scm")) (inside)`},
		{name: "inside a begin", code: pass + `(pass (begin (include "x.scm"))) (inside)`},
		{name: "inside a cond-expand clause", code: pass + `(pass (cond-expand (else (include "x.scm")))) (inside)`},
		{name: "include-ci", code: pass + `(pass (include-ci "xu.scm")) (inside)`},
		{name: "second of two files", code: pass + `(pass (include "y.scm" "x.scm")) (inside)`},
		{name: "through two macros", code: pass + `(pass (pass (include "x.scm"))) (inside)`},
		{name: "nested include", code: pass + `(pass (include "chain.scm")) (inside)`},
		{name: "body, inside a begin", code: pass + `(define (f) (pass (begin (include "x.scm"))) (inside)) (f)`},
		{name: "body, second of two files", code: pass + `(define (f) (pass (include "y.scm" "x.scm")) (inside)) (f)`},
		{name: "body, nested include", code: pass + `(define (f) (pass (include "chain.scm")) (inside)) (f)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fsys, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, "7")
		})
	}
}

// TestIncludePassedToAMacroDoesNotCaptureTheMacrosBinder is the include half of
// TestCondExpandPassedToAMacroDoesNotCaptureTheMacrosBinder (X21): an include a
// macro receives DIRECTLY as an argument and places in its own body keeps its
// use-site scope, so a define in the file cannot capture the macro's binder.
// One macro layer between them defeats it; see the known-gap row. Chez and
// Racket (racket/include) answer top and macro. The macro-local row used to
// raise ErrAmbiguousBinding: the pruned define tied with the macro's let binder.
// The last row splices one include twice, once in the macro's body; Racket, with
// syntax-case for the ER macro, answers macro user.
func TestIncludePassedToAMacroDoesNotCaptureTheMacrosBinder(t *testing.T) {
	fsys := fstest.MapFS{
		"u.scm": &fstest.MapFile{Data: []byte(`(define a 'user)`)},
	}
	const prelude = `(define a 'top)
(define-syntax m (syntax-rules () ((_ f) (let () f a))))
`
	tcs := []struct {
		name string
		code string
		want string
		opts []wile.EngineOption
	}{
		{name: "include argument", code: prelude + `(m (include "u.scm"))`, want: "top"},
		{name: "begin argument (control)", code: prelude + `(m (begin (define a 'user)))`, want: "top"},
		{name: "macro-local binder", code: `(define-syntax m2 (syntax-rules () ((_ f) (let ((a 'macro)) (let () f a)))))
(m2 (include "u.scm"))`, want: "macro"},
		{name: "one include spliced twice", code: `(import (for-syntax (scheme base)))
(define-syntax pass (syntax-rules () ((_ e) e)))
(define-syntax twice
  (er-macro-transformer
    (lambda (f r c)
      (list (r 'begin) (cadr f)
            (list (r 'let) (list (list (r 'a) ''macro))
                  (list (r 'let) '() (cadr f) (r 'a)))))))
(list (pass (twice (include "u.scm"))) a)`, want: "(macro user)", opts: withStdlibResolver()},
		// KNOWN GAP, pinned at today's answer: racket and petite answer top. pass's
		// output is the include at a splice position, so it is marked, and the
		// boundary prune strips every use-site scope the run minted, m's included.
		// (m (pass (define a 'user))) answers user too, without an include. Filed
		// from the X21 crosscheck; flip this row when that row is fixed.
		{name: "one macro layer between (known gap)", code: prelude + `(define-syntax pass (syntax-rules () ((_ e) e)))
(m (pass (include "u.scm")))`, want: "user"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fsys, tc.code, tc.opts...)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

// TestMacroIntroducedIncludeHidesItsDefinitions pins a deliberate change: an
// include a macro's TEMPLATE introduces is hygienic, so the file's definitions
// carry the macro's introduction scope and are invisible at the use site. Master
// exposed them; Chez and Racket refuse, as does the splice. Pinned so it is not
// "fixed" back.
func TestMacroIntroducedIncludeHidesItsDefinitions(t *testing.T) {
	fsys := fstest.MapFS{
		"x.scm": &fstest.MapFile{Data: []byte(`(define (inside) 7)`)},
	}
	_, err := evalIncludeProgram(t, fsys,
		`(define-syntax inc (syntax-rules () ((_) (include "x.scm")))) (inc) (inside)`)
	qt.Assert(t, err, qt.IsNotNil)
}

// TestIncludeSplicePinsLoadStackAndHead pins the two properties the splice must
// keep that TestIncludeSplicePins does not reach.
//
//   - The head drops the include head's scopes: with a let-bound begin, a head
//     that kept them would resolve to the local begin.
//   - The load stack is popped after a nested include, so a SIBLING include that
//     follows resolves against the includer's directory, not the nested file's.
func TestIncludeSplicePinsLoadStackAndHead(t *testing.T) {
	fsys := fstest.MapFS{
		"v.scm":      &fstest.MapFile{Data: []byte(`5`)},
		"chain2.scm": &fstest.MapFile{Data: []byte(`(include "sub/b2.scm") (include "d.scm")`)},
		"sub/b2.scm": &fstest.MapFile{Data: []byte(`(define b2 1)`)},
		"d.scm":      &fstest.MapFile{Data: []byte(`(define dv 'root)`)},
		"sub/d.scm":  &fstest.MapFile{Data: []byte(`(define dv 'sub)`)},
	}
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "begin bound by the includer's let", code: `(let ((begin (lambda args 'shadowed))) (include "v.scm"))`, want: "5"},
		{name: "sibling include after a nested one", code: `(include "chain2.scm") dv`, want: "root"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fsys, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

// TestIncludeUnitBoundaryShapes pins two further shapes the splice changes:
// defining the same name from two includes is an ordinary redefinition (master
// refused it as a mutation of an immutable binding), and a macro an included
// file defines is bound in a body after the include.
func TestIncludeUnitBoundaryShapes(t *testing.T) {
	fsys := fstest.MapFS{
		"a.scm": &fstest.MapFile{Data: []byte(`(define dup 1)`)},
		"b.scm": &fstest.MapFile{Data: []byte(`(define dup 2)`)},
		"m.scm": &fstest.MapFile{Data: []byte(`(define-syntax foo (syntax-rules () ((_) 42)))`)},
	}
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "same name defined by two includes", code: `(include "a.scm") (include "b.scm") dup`, want: "2"},
		{name: "macro from an include used in a body", code: `(define (f) (include "m.scm") (foo)) (f)`, want: "42"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fsys, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

// TestIncludeInLibraryBeginBody pins an include inside a library (begin ...)
// body: that include reaches the expander (a declaration-level one goes through
// CompileInclude), and a macro it defines is bound for the forms after it.
func TestIncludeInLibraryBeginBody(t *testing.T) {
	fsys := fstest.MapFS{
		"lib/m.sld": &fstest.MapFile{Data: []byte(`(define-library (m)
  (export get)
  (begin
    (define (helper) 42)
    (include "mac.scm")
    (define (get) (use-it))))`)},
		"lib/mac.scm": &fstest.MapFile{Data: []byte(`(define-syntax use-it (syntax-rules () ((_) (helper))))`)},
	}
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.Console),
		wile.WithAuthorizer(nil), // the test exercises include resolution, not the sandbox
		wile.WithSourceFS(fsys),
		wile.WithLibraryPaths("lib"),
	)
	qt.Assert(t, err, qt.IsNil)
	result, err := engine.EvalMultiple(ctx, `(import (m)) (get)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "42")
}

// TestIncludeErrorsKeepTheirLocations pins where include errors point: a missing
// file at the include form's own line, and a form of the file that fails to
// expand at the file's own line (the includer's coarser location does not
// replace it). A read error in the file renders under the include form's prefix,
// so its own text must name the file its line and column belong to.
func TestIncludeErrorsKeepTheirLocations(t *testing.T) {
	fsys := fstest.MapFS{
		"badexp.scm":  &fstest.MapFile{Data: []byte("(define ok 1)\n(syntax-error \"boom\")")},
		"badread.scm": &fstest.MapFile{Data: []byte("(define ok 1)\n(define open (\n")},
		"badlex.scm":  &fstest.MapFile{Data: []byte("(define ok 1)\n#b109\n")},
	}
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "missing file names the include's line", code: "(define a 1)\n(define b 2)\n(include \"absent.scm\")", want: "includer.scm:3:"},
		{name: "expansion failure keeps the file's line", code: `(include "badexp.scm")`, want: "badexp.scm:2:"},
		{name: "parse error names the included file's position", code: "(define a 1)\n(include \"badread.scm\")", want: "(at badread.scm:2:13, index 27,"},
		{name: "lexical error names the included file", code: "(define a 1)\n(include \"badlex.scm\")", want: "(at badlex.scm:2:4, index 18, character '9' (U+0039), tokenizer state unsigned-integer)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := evalIncludeProgram(t, fsys, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, err.Error(), qt.Contains, tc.want)
		})
	}
}

// TestIncludedFormErrorNamesTheInclude pins B29 gap 1: a form spliced from an
// included file that fails to compile names the include that brought it in, as
// a macro expansion's error names the macro. The splice leaves no include form
// for the compiler to wrap, so the frame rides on the included forms' origin.
func TestIncludedFormErrorNamesTheInclude(t *testing.T) {
	fsys := fstest.MapFS{
		"badcompile.scm": &fstest.MapFile{Data: []byte("(define ok 1)\n(define bad (lambda))\n")},
		"outer.scm":      &fstest.MapFile{Data: []byte("(define o 1)\n(include \"badcompile.scm\")\n")},
		"usesmac.scm":    &fstest.MapFile{Data: []byte("(define-syntax badif (syntax-rules () ((_ x) (if x))))\n(badif 1)\n")},
		"badset.scm":     &fstest.MapFile{Data: []byte("(define ok 1)\n(set! nope 2)\n")},
	}
	tcs := []struct {
		name string
		code string
		want []string
	}{
		{name: "compile error in an included form", code: "(define a 1)\n(include \"badcompile.scm\")",
			want: []string{"badcompile.scm:2:13: lambda requires", "expanded from 'include' at includer.scm:2:1\n"}},
		{name: "compiler-raised error in an included form", code: "(define a 1)\n(include \"badset.scm\")",
			want: []string{"badset.scm:2:6: no such binding", "expanded from 'include' at includer.scm:2:1\n"}},
		{name: "include-ci names itself", code: "(define a 1)\n(include-ci \"badcompile.scm\")",
			want: []string{"expanded from 'include-ci' at includer.scm:2:1\n"}},
		{name: "nested include names both includes", code: "(include \"outer.scm\")",
			want: []string{"expanded from 'include' at outer.scm:2:1\n", "expanded from 'include' at includer.scm:1:1\n"}},
		{name: "macro used in an included file chains to the include", code: "(define a 1)\n(include \"usesmac.scm\")",
			want: []string{"expanded from 'badif' at usesmac.scm:2:1\n", "expanded from 'include' at includer.scm:2:1\n"}},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := evalIncludeProgram(t, fsys, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			for _, want := range tc.want {
				// A chain line ends at a newline or the end of the text, so
				// includer.scm:2:1 cannot match includer.scm:2:10.
				qt.Check(t, strings.Count(err.Error()+"\n", want), qt.Equals, 1, qt.Commentf("%s", err.Error()))
			}
			// The frame is carried, not only rendered: the failing node's
			// SourcedError holds the include in its origin chain.
			var deepest, se *compilation.SourcedError
			for e := err; errors.As(e, &se); e = se.Cause {
				deepest = se
			}
			qt.Assert(t, deepest, qt.IsNotNil)
			found := false
			for o := deepest.Source.Origin; o != nil; o = o.Parent {
				found = found || strings.HasPrefix(o.Identifier, "include")
			}
			qt.Check(t, found, qt.IsTrue, qt.Commentf("no include in the origin chain of %v", deepest))
		})
	}
}

// TestRelativeIncludeResolvesAgainstTheFormsOwnFile pins the lexical rule: a
// relative include resolves against the directory of the file holding the
// include FORM, not against whatever the load stack holds when the form is
// compiled. An eval-when body (and, before P5.8, a cond-expand body) is compiled
// after the splice has popped its file, so the stack rule alone read the decoy at
// the root (or found nothing). Racket's include is lexical in the same way.
func TestRelativeIncludeResolvesAgainstTheFormsOwnFile(t *testing.T) {
	fsys := fstest.MapFS{
		"sub/b.scm":  &fstest.MapFile{Data: []byte(`(cond-expand (else (include "c.scm")))`)},
		"sub/e.scm":  &fstest.MapFile{Data: []byte(`(eval-when (load eval) (include "c.scm"))`)},
		"sub/c.scm":  &fstest.MapFile{Data: []byte(`(define cv 9)`)},
		"c.scm":      &fstest.MapFile{Data: []byte(`(define cv "WRONG-FILE")`)},
		"chain.scm":  &fstest.MapFile{Data: []byte(`(include "sub/b3.scm") (define chainv (+ bv 1))`)},
		"sub/b3.scm": &fstest.MapFile{Data: []byte(`(include "c3.scm") (define bv (+ cv3 1))`)},
		"sub/c3.scm": &fstest.MapFile{Data: []byte(`(define cv3 9)`)},
	}
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "include under cond-expand, decoy at the root", code: `(include "sub/b.scm") cv`, want: "9"},
		{name: "include under eval-when, decoy at the root", code: `(include "sub/e.scm") cv`, want: "9"},
		{name: "chained include still resolves", code: `(include "chain.scm") chainv`, want: "11"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fsys, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

// TestIncludeWithoutASourceFileFallsBack pins the fallback: a form read from a
// string with no file name has no directory of its own, so a relative include
// resolves as it always has (against the search paths / the FS root).
func TestIncludeWithoutASourceFileFallsBack(t *testing.T) {
	fsys := fstest.MapFS{
		"x.scm": &fstest.MapFile{Data: []byte(`(define xv 4)`)},
	}
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithAuthorizer(nil),
		wile.WithSourceFS(fsys),
	)
	qt.Assert(t, err, qt.IsNil)
	result, err := engine.EvalMultiple(ctx, `(include "x.scm") xv`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "4")
}

// TestIncludeLabelDoesNotOverrideTheLoadStack pins the guard in
// lexicalIncludeContext. A relative SourceContext.File is a caller's diagnostic
// label (EvalProgram's name argument, which the CLI sets to the path as typed),
// not a location on disk. With a live load stack (Engine.ContextWithLoadPath,
// which the CLI installs, and whose entry the CLI computes AFTER a prior file's
// set-current-directory!), the stack names the real file and the label must not
// override it: here the label points at a decoy directory.
func TestIncludeLabelDoesNotOverrideTheLoadStack(t *testing.T) {
	tmp := t.TempDir()
	write := func(rel, data string) {
		full := filepath.Join(tmp, rel)
		qt.Assert(t, os.MkdirAll(filepath.Dir(full), 0o755), qt.IsNil)
		qt.Assert(t, os.WriteFile(full, []byte(data), 0o644), qt.IsNil)
	}
	write("real/inc.scm", `(define v 'real)`)
	write("lbl/inc.scm", `(define v 'decoy)`)
	t.Chdir(tmp)

	ctx := context.Background()
	engine, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithAuthorizer(nil),
		wile.WithSourceOS(),
	)
	qt.Assert(t, err, qt.IsNil)
	loadCtx, err := engine.ContextWithLoadPath(ctx, filepath.Join(tmp, "real", "main.scm"))
	qt.Assert(t, err, qt.IsNil)

	result, err := engine.EvalProgram(loadCtx, `(include "inc.scm") v`, "lbl/x.scm")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "real")
}

// deferredIncludeEngine returns an engine over fsys with the load path set to
// main.scm, the shape the two tests below share: a virtual FS (where a relative
// SourceContext.File IS a real path) and a live load stack.
func deferredIncludeEngine(t *testing.T, fsys fstest.MapFS) (*wile.Engine, context.Context) {
	t.Helper()
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithAuthorizer(nil),
		wile.WithSourceFS(fsys),
	)
	qt.Assert(t, err, qt.IsNil)
	loadCtx, err := engine.ContextWithLoadPath(ctx, "main.scm")
	qt.Assert(t, err, qt.IsNil)
	return engine, loadCtx
}

// Pin (P5.8): a relative include under cond-expand in an included file resolves
// against that file's directory even with a virtual FS and a live load stack.
// cond-expand splices at expand time, so the include expands while the file is
// still on the stack; before the splice the body compiled after the file was
// popped and read the includer's decoy.
func TestCondExpandIncludeUnderALiveStackWithRelativePaths(t *testing.T) {
	fsys := fstest.MapFS{
		"sub/b.scm": &fstest.MapFile{Data: []byte(`(cond-expand (else (include "c.scm")))`)},
		"sub/c.scm": &fstest.MapFile{Data: []byte(`(define cv 9)`)},
		"c.scm":     &fstest.MapFile{Data: []byte(`(define cv "decoy")`)},
	}
	engine, loadCtx := deferredIncludeEngine(t, fsys)

	result, err := engine.EvalProgram(loadCtx, `(include "sub/b.scm") cv`, "includer.scm")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "9")
}

// TestKnownLimitationDeferredIncludeUnderALiveStackWithRelativePaths documents,
// as current behaviour, what lexicalIncludeContext cannot yet do: with a virtual
// FS and a live load stack, a relative include nested under a form compiled
// after its file was popped (eval-when; cond-expand no longer, it splices at
// expand time) resolves against the stack's top, as it did before the lexical
// rule. The guard cannot tell a real relative path from a caller's label.
// Filed in todo/tier1-security-correctness.md ("Filed by the P5.7 crosscheck").
// When that row is fixed this test should flip to want "9".
func TestKnownLimitationDeferredIncludeUnderALiveStackWithRelativePaths(t *testing.T) {
	fsys := fstest.MapFS{
		"sub/b.scm": &fstest.MapFile{Data: []byte(`(eval-when (load eval) (include "c.scm"))`)},
		"sub/c.scm": &fstest.MapFile{Data: []byte(`(define cv 9)`)},
		"c.scm":     &fstest.MapFile{Data: []byte(`(define cv "decoy")`)},
	}
	engine, loadCtx := deferredIncludeEngine(t, fsys)

	result, err := engine.EvalProgram(loadCtx, `(include "sub/b.scm") cv`, "includer.scm")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, `"decoy"`)
}
