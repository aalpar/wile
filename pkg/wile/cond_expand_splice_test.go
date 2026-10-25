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
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
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
		// A define in the clause shadows for the forms after it (X25), in body
		// position now that a define in expression position is refused. It does
		// not tell body expansion from per-form expansion; nothing does (see
		// expandCondExpandForm).
		{name: "a define in a body clause shadows for the forms after it", code: `(let () (cond-expand (else (define include (lambda a 'mine)) (include "v.scm"))))`, want: "mine"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fstest.MapFS{"v.scm": &fstest.MapFile{Data: []byte(`5`)}}, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

// TestCondExpandInExpressionPositionRefusesADefine: the selected clause's forms
// are expressions there, as an expression-position begin's are, so a define in
// one is refused rather than defining a variable. Both rows answered (mine)
// before, the define shadowing include for the form after it (X25). Neither
// oracle reads a cond-expand in expression position; both refuse the begin row
// ("define: not allowed in an expression context", "invalid context for
// definition").
func TestCondExpandInExpressionPositionRefusesADefine(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{name: "a define in the clause", code: `(list (cond-expand (else (define include (lambda a 'mine)) (include "v.scm"))))`},
		{name: "control: the same body in an expression-position begin", code: `(list (begin (define include (lambda a 'mine)) (include "v.scm")))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := evalIncludeProgram(t, fstest.MapFS{"v.scm": &fstest.MapFile{Data: []byte(`5`)}}, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Check(t, errors.Is(err, werr.ErrInvalidSyntax), qt.IsTrue, qt.Commentf("%v", err))
			qt.Check(t, err.Error(), qt.Contains, "includer.scm:1:")
			qt.Check(t, err.Error(), qt.Contains, "definition not allowed in an expression context")
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
// against the file's directory on the OS filesystem, with a decoy of the same
// name in the includer's directory. This guards the splice, not the fix: the
// pre-splice compiler also pushed the load stack around the nested include.
func TestCondExpandIncludeChainsRelativeToTheIncludedFileOnDisk(t *testing.T) {
	tmp := t.TempDir()
	write := func(rel, body string) {
		full := filepath.Join(tmp, rel)
		qt.Assert(t, os.MkdirAll(filepath.Dir(full), 0o755), qt.IsNil)
		qt.Assert(t, os.WriteFile(full, []byte(body), 0o644), qt.IsNil)
	}
	write("sub/b.scm", `(cond-expand (else (include "c.scm")))`)
	write("sub/c.scm", `(define cv 9)`)
	write("c.scm", `(define cv 1)`)

	ctx := context.Background()
	engine, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithAuthorizer(nil),
		wile.WithSourceOS(),
	)
	qt.Assert(t, err, qt.IsNil)
	loadCtx, err := engine.ContextWithLoadPath(ctx, filepath.Join(tmp, "main.scm"))
	qt.Assert(t, err, qt.IsNil)
	result, err := engine.EvalProgram(loadCtx, `(include "sub/b.scm") cv`, filepath.Join(tmp, "main.scm"))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "9")
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
// expander, each at the offending clause or the form, line AND column, on a
// multi-line program (0-based columns).
func TestCondExpandMalformedForms(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "no clauses", code: "(define a 1)\n(define b 2)\n(cond-expand)", want: `includer.scm:3:1: .*cond-expand: no clauses`},
		{name: "a clause that is not a list", code: "(define a 1)\n(define b 2)\n(cond-expand 42)", want: `includer.scm:3:13: .*cond-expand: clause 1 must be a list`},
		{name: "no clause matches", code: "(define a 1)\n(cond-expand\n  (nosuchfeature 1))", want: `includer.scm:2:1: .*cond-expand: no matching clause`},
		{name: "else is not last", code: "(define a 1)\n(cond-expand\n  (else 1)\n  (r7rs 2))", want: `includer.scm:3:2: .*else must be the last clause`},
		{name: "an unselected clause is an improper list", code: "(define a 1)\n(cond-expand\n  (r7rs 1)\n  (foo . 5))", want: `includer.scm:4:2: .*cond-expand: clause 2 must be a proper list`},
		{name: "a non-matching clause is an improper list", code: "(define a 1)\n(cond-expand\n  (nonfeat . 5)\n  (else 1))", want: `includer.scm:3:2: .*cond-expand: clause 1 must be a proper list`},
		{name: "not takes exactly one requirement", code: "(define a 1)\n(cond-expand\n  ((not a b) 1))", want: `includer.scm:3:3: .*not: expected exactly one requirement`},
		{name: "library takes exactly one name", code: "(define a 1)\n(cond-expand\n  ((library (scheme base) (scheme write)) 1))", want: `includer.scm:3:3: .*library: expected exactly one library name`},
		{name: "an improper requirement list", code: "(define a 1)\n(cond-expand\n  ((and r7rs . 5) 1))", want: `includer.scm:3:4: .*requirement list must be a proper list`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := evalIncludeProgram(t, fstest.MapFS{}, tc.code)
			qt.Assert(t, err, qt.ErrorMatches, `(?s).*`+tc.want+`.*`)
		})
	}
}

// Pin (P5.8): an improper clause list or an improper clause is refused in every
// position the form can appear in, selected or not.
func TestCondExpandRefusesImproperClausesInEveryPosition(t *testing.T) {
	forms := []string{
		`(cond-expand (r7rs 1) (foo . 5))`,
		`(cond-expand (nonfeat . 5) (else 1))`,
	}
	positions := []struct {
		name string
		wrap func(form string) string
	}{
		{name: "top level", wrap: func(form string) string { return form }},
		{name: "body", wrap: func(form string) string { return "(define (f) " + form + ") (f)" }},
		{name: "eval", wrap: func(form string) string { return "(eval '" + form + " (interaction-environment))" }},
	}
	for _, form := range forms {
		for _, pos := range positions {
			t.Run(pos.name+" "+form, func(t *testing.T) {
				_, err := evalIncludeProgram(t, fstest.MapFS{}, pos.wrap(form))
				qt.Assert(t, err, qt.ErrorMatches, `(?s).*cond-expand: clause [12] must be a proper list.*`)
			})
		}
	}

	for _, form := range forms {
		t.Run("library begin "+form, func(t *testing.T) {
			fsys := fstest.MapFS{
				"probe/clauses.sld": &fstest.MapFile{Data: []byte(`(define-library (probe clauses)
  (import (scheme base))
  (export a)
  (begin ` + form + ` (define a 1)))`)},
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
			_, err = engine.EvalMultiple(ctx, `(import (probe clauses)) a`)
			qt.Assert(t, err, qt.ErrorMatches, `(?s).*cond-expand: clause [12] must be a proper list.*`)
		})
	}
}

// Pin (P5.8, code review): an empty splice is one void operand, not none, and
// does not disturb a body that continues with a define.
func TestEmptySpliceIsOneVoidValue(t *testing.T) {
	fsys := fstest.MapFS{
		"empty.scm": &fstest.MapFile{Data: []byte(``)},
	}
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "empty cond-expand clause as an operand", code: `(length (list (cond-expand (r7rs))))`, want: "1"},
		{name: "empty include as an operand", code: `(length (list (include "empty.scm")))`, want: "1"},
		{name: "empty clause before a body define", code: `(define (f) (cond-expand (else)) (define x 1) x) (f)`, want: "1"},
		{name: "empty include before a body define", code: `(define (f) (include "empty.scm") (define x 2) x) (f)`, want: "2"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fsys, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

// Pin (P5.8, tests review): a macro that expands to a cond-expand can define a
// name the caller passed (the use-site scope is pruned off the clause forms),
// and a name the macro introduces stays hidden from the caller.
func TestMacroExpandingToCondExpand(t *testing.T) {
	const macro = `(define-syntax m (syntax-rules () ((_ n) (cond-expand (else (define n 11))))))
(define-syntax hide (syntax-rules () ((_) (cond-expand (else (define hid 5))))))
`
	t.Run("top level defines the passed name", func(t *testing.T) {
		got, err := evalIncludeProgram(t, fstest.MapFS{}, macro+`(m q) q`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, got, qt.Equals, "11")
	})
	t.Run("body defines the passed name", func(t *testing.T) {
		got, err := evalIncludeProgram(t, fstest.MapFS{}, macro+`(define (g) (m r) r) (g)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, got, qt.Equals, "11")
	})
	t.Run("an introduced name stays hidden", func(t *testing.T) {
		_, err := evalIncludeProgram(t, fstest.MapFS{}, macro+`(hide) hid`)
		qt.Assert(t, err, qt.ErrorMatches, `(?s).*no such.*hid.*`)
	})
}

// Pin (P5.8, tests review): every form of a multi-form clause body is spliced.
func TestCondExpandMultiFormClauseBody(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{name: "top level", code: `(cond-expand (else (define a 1) (define b 2))) (+ a b)`},
		{name: "body", code: `(define (f) (cond-expand (else (define a 1) (define b 2))) (+ a b)) (f)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fstest.MapFS{}, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, "3")
		})
	}
}

// Pin (P5.8 re-review): use-site pruning happens at the transformer boundary, so
// a cond-expand a macro merely RECEIVES as an argument keeps its use-site scopes
// and its define cannot capture the macro's own binder. Same program with
// (begin ...) is the control; Chez answers top.
func TestCondExpandPassedToAMacroDoesNotCaptureTheMacrosBinder(t *testing.T) {
	const prelude = `(define a 'top)
(define-syntax m (syntax-rules () ((_ f) (let () f a))))
`
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "cond-expand argument", code: prelude + `(m (cond-expand (else (define a 'user))))`, want: "top"},
		{name: "begin argument (control)", code: prelude + `(m (begin (define a 'user)))`, want: "top"},
		{name: "macro-local binder", code: `(define-syntax m2 (syntax-rules () ((_ f) (let ((a 'macro)) (let () f a)))))
(m2 (cond-expand (else (define a 'user))))`, want: "macro"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, fstest.MapFS{}, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

// TestImportShadowedBySpliceInALibrary pins what a library sees when an
// included file or a cond-expand clause defines a name the library imported.
// A splice inside one (begin ...) body joins that body's letrec* pre-scan, so a
// reference compiled before the define already reads it. At declaration level
// each declaration compiles on its own, so the earlier reference keeps the
// import, exactly as three plain begin declarations do (the control row): the
// splices add no rule of their own. R7RS 5.6.1 makes redefining an import an
// error and Petite Chez refuses it ("multiple definitions"); Wile accepting it
// is filed separately.
func TestImportShadowedBySpliceInALibrary(t *testing.T) {
	sq := `(define (square x) 'mine)`
	tcs := []struct {
		name string
		decl string
		want string
	}{
		{name: "include in a begin body", decl: `(begin (define (before) (square 3)) (include "sq.scm") (define (after) (square 3)))`, want: "(mine mine)"},
		{name: "cond-expand in a begin body", decl: `(begin (define (before) (square 3)) (cond-expand (else ` + sq + `)) (define (after) (square 3)))`, want: "(mine mine)"},
		{name: "include declaration", decl: `(begin (define (before) (square 3))) (include "sq.scm") (begin (define (after) (square 3)))`, want: "(9 mine)"},
		{name: "cond-expand declaration", decl: `(begin (define (before) (square 3))) (cond-expand (else (begin ` + sq + `))) (begin (define (after) (square 3)))`, want: "(9 mine)"},
		{name: "control: three begin declarations", decl: `(begin (define (before) (square 3))) (begin ` + sq + `) (begin (define (after) (square 3)))`, want: "(9 mine)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			fsys := fstest.MapFS{
				"sh.sld": &fstest.MapFile{Data: []byte(`(define-library (sh) (export before after) (import (scheme base)) ` + tc.decl + `)`)},
				"sq.scm": &fstest.MapFile{Data: []byte(sq)},
			}
			got, err := evalIncludeProgram(t, fsys, `(import (sh)) (list (before) (after))`,
				wile.WithSourceFS(stdlib.FS), wile.WithLibraryPaths("."))
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}
