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
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// shadowFS is the file the include rows read: a form that would answer 5 if the
// include were the special form.
var shadowFS = fstest.MapFS{"v.scm": &fstest.MapFile{Data: []byte(`5`)}}

// TestTopLevelVariableShadowsExpanderForm pins X25: a top-level define of a
// special-form name shadows the form at head position for every form the
// expander consumes, as it already did for the forms the compiler dispatches
// (if, begin). Each program is one EvalProgram unit, so the define and the use
// sit in one begin: the expander predeclares both before either compiles, and
// the use still reads the variable because its define has already expanded.
//
// Racket and Petite Chez answer mine for every row. The changed rows are red on
// master: include and include-ci answered 5, the rest an expansion error. The
// controls were already mine on master: their expanders hand the form on to the
// compiler (unchanged, or with only its operands expanded, or bailing on the
// malformed shape), and the validator already read the top-level variable as a
// call.
func TestTopLevelVariableShadowsExpanderForm(t *testing.T) {
	changed := []string{
		"include", "include-ci", "cond-expand", "define-for-syntax", "eval-when",
		"let-syntax", "letrec-syntax", "import", "syntax-error", "with-binding-scope",
	}
	controls := []string{
		"define-syntax", "begin-for-syntax", "with-syntax", "syntax-case", "quasisyntax",
		"begin", "if", "quasiquote",
	}
	for _, table := range []struct {
		label string
		names []string
	}{{"changed", changed}, {"control", controls}} {
		for _, name := range table.names {
			t.Run(table.label+"/"+name, func(t *testing.T) {
				got, err := evalIncludeProgram(t, shadowFS, `(define `+name+` (lambda args 'mine)) (`+name+` "v.scm")`)
				qt.Assert(t, err, qt.IsNil)
				qt.Assert(t, got, qt.Equals, "mine")
			})
		}
	}
}

// TestLibraryVariableShadowsExpanderForm pins the library half: a library-body
// define of cond-expand shadows it at the library's top level and in a lambda
// body too. A library identifier carries the library's scope, which
// referenceReachesBinderDirectly follows to the binder, so the body-position
// exception of TestHeadShadowStillDivergentThroughScopes is top-level only. The
// if row is the control: the compiler-dispatched form already answered lib on
// master. Racket (a module) answers (lib lib); Petite Chez refuses redefining an
// import, and Wile accepting that is filed separately.
func TestLibraryVariableShadowsExpanderForm(t *testing.T) {
	for _, name := range []string{"cond-expand", "if"} {
		t.Run(name, func(t *testing.T) {
			fsys := fstest.MapFS{
				"lsh.sld": &fstest.MapFile{Data: []byte(`(define-library (lsh) (export top body) (import (scheme base))
  (begin (define (` + name + ` . a) 'lib)
         (define (top) (` + name + `))
         (define (body) ((lambda () (` + name + `))))))`)},
			}
			got, err := evalIncludeProgram(t, fsys, `(import (lsh)) (list (top) (body))`,
				wile.WithSourceFS(stdlib.FS), wile.WithLibraryPaths("."))
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, "(lib lib)")
		})
	}
}

// TestHeadShadowFollowsDefinitionOrder pins the partition the expander and the
// validator share: a use after the define reads the variable, a use before it
// (or the define's own head) reads the form.
func TestHeadShadowFollowsDefinitionOrder(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		// The block's gate, verbatim.
		{name: "cond-expand in an explicit begin", code: `(begin (define cond-expand (lambda args 'mine)) (cond-expand))`, want: "mine"},
		{name: "include in an explicit begin", code: `(begin (define include (lambda args 'mine)) (include "v.scm"))`, want: "mine"},
		{name: "define in a nested begin is still earlier", code: `(begin (begin (define include (lambda args 'mine))) (include "v.scm"))`, want: "mine"},
		// A define no pre-scan saw, a macro's output, binds for the forms after
		// it too: it is predeclared when it expands. Racket and Petite: mine.
		{name: "define-values", code: `(define-values (include) (values (lambda a 'mine))) (include "v.scm")`, want: "mine"},
		{name: "a user macro that emits define", code: `(define-syntax def (syntax-rules () ((_ n v) (define n v)))) (def include (lambda a 'mine)) (include "v.scm")`, want: "mine"},
		{name: "a user macro that emits define, in a body", code: `(define-syntax def (syntax-rules () ((_ n v) (define n v)))) (define (f) (def include (lambda a 'mine)) (include "v.scm")) (f)`, want: "mine"},
		{name: "define-values in a body", code: `(define (g) (define-values (include) (values (lambda a 'mine))) (include "v.scm")) (g)`, want: "mine"},
		// Use before define: the define is later, so both readers see the form.
		// Petite Chez answers 5 too; Racket refuses ("include: undefined").
		{name: "use before define", code: `(define r (include "v.scm")) (define include 3) r`, want: "5"},
		// The define's own head is the form: the first definition of a keyword
		// name binds (R7RS §5.3.1).
		{name: "first define of include binds", code: `(define include 3) include`, want: "3"},
		{name: "first define of define-syntax binds", code: `(define define-syntax 3) define-syntax`, want: "3"},
		{name: "first define of cond-expand binds", code: `(define cond-expand 3) cond-expand`, want: "3"},
		// Controls.
		{name: "a let-bound include", code: `(let ((include (lambda args 'mine))) (include "v.scm"))`, want: "mine"},
		{name: "a shadowed with-binding-scope leaves named let alone", code: `(define with-binding-scope 9) (let loop ((i 0)) (if (< i 3) (loop (+ i 1)) i))`, want: "3"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, shadowFS, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

// TestHeadShadowStillDivergentThroughScopes pins the half X25 leaves to P6.6's
// arc: a reference that carries a scope its top-level binder lacks (a lambda
// body, a macro argument) still reads the form, because
// validate.referenceReachesBinderDirectly cannot tell that widening from a
// template identifier's. Racket and Petite Chez answer mine for both; these rows
// flip when that guard is relaxed. Top level only: inside a library the same
// shapes read the shadow (TestLibraryVariableShadowsExpanderForm).
func TestHeadShadowStillDivergentThroughScopes(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{name: "lambda body", code: `(define include (lambda args 'mine)) (define (f) (include "v.scm")) (f)`},
		{name: "macro argument", code: `(define include (lambda args 'mine)) (define-syntax pass (syntax-rules () ((_ e) e))) (pass (include "v.scm"))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalIncludeProgram(t, shadowFS, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, "5")
		})
	}
}
