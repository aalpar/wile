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
	"testing"
	"testing/fstest"

	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// A template's free identifier is pinned at the phase its expansion lands in,
// not at whatever phase first binds the name (X26). A mutable variable bound
// only above that phase refuses at the use, with the direct reference's
// "(bound at phase N)" clause, as Racket (define-syntax-rule over a
// begin-for-syntax define: "unbound identifier") and Chez (meta define:
// "attempt to reference out-of-context identifier") both refuse.
//
// The pin used to alias LIVE compile-time state: the set! row displayed 8, a
// phase-1 assignment made after the macro was defined. All three template
// producers share the walk: syntax-rules, a syntax-case (syntax ...) with an
// ellipsis, and quote-syntax. A syntax-case template without an ellipsis builds
// its result without the walk and refused already; it is the control. The
// phase-3 row inverts TestFreeTemplateIdentifierAtPhaseZeroResolvesPhaseThreeBinding,
// which pinned the reach as the guard for PresentPhases's lifted ceiling.
//
// tail pins the full text where it is known to differ: the template's
// identifier carries the macro's scopes, so the scoped arm renders "with
// compatible scopes" before the same "(bound at phase 1)" clause the direct,
// scopeless reference gets. That prefix is X22's rule (scoped and a same-named
// binding exists), kept here; changing it must flip this row deliberately.
func TestTemplatePinRefusesAHigherPhaseVariable(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name  string
		code  string
		ident string
		phase string
		from  string
		tail  string
	}{
		{
			name:  "direct reference control",
			code:  `(begin-for-syntax (define helper3 7)) helper3`,
			ident: "helper3", phase: "(bound at phase 1)",
			tail: `no such binding "helper3" (bound at phase 1): no such binding`,
		},
		{
			name: "syntax-rules template",
			code: `(begin-for-syntax (define helper2 7))
(define-syntax useh2 (syntax-rules () ((_) helper2)))
(useh2)`,
			ident: "helper2", phase: "(bound at phase 1)", from: "useh2",
			tail: `no such binding "helper2" with compatible scopes (bound at phase 1): no such binding`,
		},
		{
			name: "a later phase-1 set! is not visible",
			code: `(begin-for-syntax (define helper2 7))
(define-syntax useh2 (syntax-rules () ((_) helper2)))
(begin-for-syntax (set! helper2 8))
(display (useh2))`,
			ident: "helper2", phase: "(bound at phase 1)", from: "useh2",
		},
		{
			name: "phase 3",
			code: `(begin-for-syntax (begin-for-syntax (begin-for-syntax (define deep 42))))
(define-syntax get-deep (syntax-rules () ((_) deep)))
(get-deep)`,
			ident: "deep", phase: "(bound at phase 3)", from: "get-deep",
		},
		{
			name: "quote-syntax",
			code: `(begin-for-syntax (define h5 7))
(define-syntax m (lambda (stx) (quote-syntax h5)))
(m)`,
			ident: "h5", phase: "(bound at phase 1)", from: "m",
		},
		{
			name: "syntax-case template with an ellipsis",
			code: `(begin-for-syntax (define h6 7))
(define-syntax m (lambda (stx) (syntax-case stx () ((_ x ...) (syntax (list h6 x ...))))))
(m 1)`,
			ident: "h6", phase: "(bound at phase 1)", from: "m",
		},
		{
			name: "syntax-case template without an ellipsis control",
			code: `(begin-for-syntax (define h7 7))
(define-syntax m (lambda (stx) (syntax-case stx () ((_) (syntax h7)))))
(m)`,
			ident: "h7", phase: "(bound at phase 1)", from: "m",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			_, err := evalTopLevel(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Check(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue, qt.Commentf("%v", err))
			qt.Check(t, err.Error(), qt.Contains, `"`+tc.ident+`"`)
			qt.Check(t, err.Error(), qt.Contains, tc.phase)
			var se *compilation.SourcedError
			qt.Check(t, errors.As(err, &se), qt.IsTrue, qt.Commentf("unlocated: %v", err))
			if tc.tail != "" {
				qt.Check(t, err.Error(), qt.Contains, tc.tail)
			}
			if tc.from != "" {
				qt.Check(t, err.Error(), qt.Contains, "expanded from '"+tc.from+"'")
			}
		})
	}
}

// The filed row's own form: a library macro whose template names a
// library-private phase-1 define. The importer's (usehelp) gave 1.
func TestLibraryTemplatePinRefusesAHigherPhaseVariable(t *testing.T) {
	t.Parallel()

	ctx := context.Background()
	eng := phaseIsolationEngine(t, fstest.MapFS{
		"ehlib.sld": &fstest.MapFile{Data: []byte(`(define-library (ehlib)
  (export usehelp)
  (import (scheme base) (for-syntax (scheme base)))
  (begin
    (begin-for-syntax (define helper 1))
    (define-syntax usehelp (syntax-rules () ((_) helper)))))
`)},
	})
	_, err := eng.EvalMultiple(ctx, `(import (ehlib))`)
	qt.Assert(t, err, qt.IsNil)
	_, err = eng.EvalMultiple(ctx, `(usehelp)`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Check(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue, qt.Commentf("%v", err))
	// The importer's probes see only its own store, not the library's phase 1,
	// so the clause reports no binding at any phase rather than the phase.
	qt.Check(t, err.Error(), qt.Contains,
		`no such local or global binding "helper" (no global binding of that name at any phase): no such binding`)
	var se *compilation.SourcedError
	qt.Check(t, errors.As(err, &se), qt.IsTrue, qt.Commentf("unlocated: %v", err))
}

// What the use-phase pin must keep: a keyword (stored one phase above the phase
// it is bound at), a variable at the use phase whether defined before or after
// the macro, a phase-0 define winning over a same-named phase-1 one, a phase-1
// macro naming a phase-1 helper (Racket: 5), and the jabberwocky case the walk
// exists for (R7RS §4.3).
func TestTemplatePinKeepsUsePhaseBindings(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			name: "recursive macro keyword",
			code: `(define-syntax my-or (syntax-rules () ((_) #f) ((_ e) e) ((_ e r ...) (let ((t e)) (if t t (my-or r ...))))))
(my-or #f 3)`,
			want: "3",
		},
		{
			name: "forward phase-0 reference",
			code: `(define-syntax usefwd (syntax-rules () ((_) fwd)))
(define fwd 3)
(usefwd)`,
			want: "3",
		},
		{
			name: "phase-0 define beside a phase-1 one",
			code: `(begin-for-syntax (define fwd 9))
(define-syntax usefwd (syntax-rules () ((_) fwd)))
(define fwd 1)
(usefwd)`,
			want: "1",
		},
		{
			name: "phase-1 macro over a phase-1 helper",
			code: `(begin-for-syntax (define helper 5) (define-syntax ph1 (syntax-rules () ((_) helper))))
(define-syntax m (lambda (stx) (datum->syntax stx (ph1))))
(m)`,
			want: "5",
		},
		{
			name: "jabberwocky",
			code: `(define-syntax jabberwocky
  (syntax-rules ()
    ((_ hatter)
     (begin (define march-hare 42)
            (define-syntax hatter (syntax-rules () ((_) march-hare)))))))
(jabberwocky mad-hatter)
(mad-hatter)`,
			want: "42",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got, err := evalTopLevel(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Check(t, got, qt.Equals, tc.want)
		})
	}
}
