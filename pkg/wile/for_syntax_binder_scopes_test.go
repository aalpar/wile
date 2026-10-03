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

// Macro-introduced compile-time binders: the scope argument and the use-site
// pruner, together or not at all (I140, I170).
//
// A `define-for-syntax` binder used to be created under the EMPTY scope set, and
// that single fact had two opposite consequences. It LEAKED a name a macro
// template introduced to any later transformer that spelled it the same way,
// because a binder at {} is reachable from every scope set. It also, by accident,
// kept a macro-SUPPLIED name — one arriving through a pattern variable, so the
// user wrote it — reachable from outside the expansion. Binding under the
// symbol's own scopes closes the leak and breaks the supplied case; adding
// `define-for-syntax` to the use-site pruner's form list restores the supplied
// case alone. r1 and r2 below move in OPPOSITE directions under the first half
// alone, which is the measured reason the two are one change.
//
// THE OBSERVATION CHANNEL IS LOAD-BEARING, and getting it wrong is what blocked
// this work for two days. Every row reads the phase-1 binding from a PROCEDURAL
// transformer — `(lambda (stx) (datum->syntax stx NAME))` — whose body is phase-1
// code, so the reference is ordinary and `datum->syntax` carries the value out to
// phase 0. The tempting alternative, a `syntax-rules` template that simply names
// the binding, emits a phase-0 reference to a phase-1 name; Wile answers it (42),
// and both oracles refuse it — racket with `helper: unbound identifier`, chez with
// `attempt to reference out-of-context identifier helper`. That is a separate
// defect of Wile's with its own ledger row, and this change does NOT fix it. A
// row written in that shape would assert that Wile keeps the leak.
//
// Oracles, run on this same channel: racket 9.3 with `(require (for-syntax
// racket/base))` and a `(define-syntax (use stx) ...)` transformer agrees with
// every row below. petite 10.4.1 covers r1-r3 and agrees (`meta define` for
// `define-for-syntax`, and `syntax-case` + `datum->syntax #'k` because Chez's
// `datum->syntax` demands an identifier template); it has no `begin-for-syntax`
// analogue, so the r4 family is racket-only rather than unmeasured.
//
// EvalProgram, not EvalMultiple: the measurement was taken through the CLI, whose
// unit is one `(begin …)`, and the rows should be pinned in the shape they were
// measured in.

import (
	"context"
	"errors"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// forSyntaxBinderEngine builds the engine every row uses. KitchenSink because the
// rows need the numeric and list vocabulary at both phases; no source FS, because
// nothing here imports.
func forSyntaxBinderEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

// TestForSyntaxBinderScopes is the gate. Two rows are RED on master (r1, r4b),
// and the rest are non-vacuity guards that pass on both sides — r2 is the one
// that matters most, because it is GREEN on master, GREEN after, and RED under
// the scopes argument alone. Without it a half-change ships looking correct.
func TestForSyntaxBinderScopes(t *testing.T) {
	cases := []struct {
		name string
		code string
		// want is the printed value when the program is expected to answer;
		// refusedName is the identifier a refusal must name. Exactly one is set.
		want        string
		refusedName string
		role        string
	}{
		{
			name: "r1 a macro-introduced binder does not leak to a later transformer",
			role: "RED on master, which answered 42",
			code: `(define-syntax genhidden (syntax-rules () ((_) (define-for-syntax hidden 42))))
			       (genhidden)
			       (define-syntax leak (lambda (stx) (datum->syntax stx hidden)))
			       (leak)`,
			refusedName: "hidden",
		},
		{
			name: "r2 a macro-supplied binder stays reachable",
			role: "GREEN both sides; RED under the scopes argument alone. The regression guard",
			code: `(define-syntax genfs (syntax-rules () ((_ n v) (define-for-syntax n v))))
			       (genfs helper 42)
			       (define-syntax usehelper (lambda (stx) (datum->syntax stx helper)))
			       (usehelper)`,
			want: "42",
		},
		{
			name: "r3 a plain define-for-syntax is unaffected",
			role: "GREEN both sides",
			code: `(define-for-syntax plain 7)
			       (define-syntax useplain (lambda (stx) (datum->syntax stx plain)))
			       (useplain)`,
			want: "7",
		},
		{
			name: "r4 a template-introduced binder under begin-for-syntax is refused",
			role: "GREEN both sides, and racket refuses the same shape. NOT a defect",
			code: `(define-syntax genbfs (syntax-rules () ((_) (begin-for-syntax (define (dbl x) (* 2 x))))))
			       (genbfs)
			       (define-syntax usedbl (lambda (stx) (datum->syntax stx (dbl 21))))
			       (usedbl)`,
			refusedName: "dbl",
		},
		{
			name: "r4b a pattern-supplied binder under begin-for-syntax is reachable",
			role: "RED on master, which refused",
			code: `(define-syntax genbfs2 (syntax-rules () ((_ f) (begin-for-syntax (define (f x) (* 2 x))))))
			       (genbfs2 trp)
			       (define-syntax usetrp (lambda (stx) (datum->syntax stx (trp 21))))
			       (usetrp)`,
			want: "42",
		},
		{
			name: "r4c a direct begin-for-syntax is unaffected",
			role: "GREEN both sides. Bounds the claim to the macro-supplied route",
			code: `(begin-for-syntax (define (ddd x) (* 2 x)))
			       (define-syntax useddd (lambda (stx) (datum->syntax stx (ddd 21))))
			       (useddd)`,
			want: "42",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			eng := forSyntaxBinderEngine(t)
			v, err := eng.EvalProgram(context.Background(), tc.code, "<test>")

			if tc.refusedName != "" {
				qt.Assert(t, err, qt.IsNotNil, qt.Commentf("role: %s", tc.role))
				qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
					qt.Commentf("got: %s", err))
				// The identifier and the phase both ride on the message, so a
				// refusal for an unrelated reason cannot satisfy this row.
				want := `no such binding "` + tc.refusedName +
					`" with compatible scopes at phase 1 of this unit's macro tower`
				qt.Assert(t, strings.Contains(err.Error(), want), qt.IsTrue,
					qt.Commentf("want %q in: %s", want, err))
				return
			}

			qt.Assert(t, err, qt.IsNil, qt.Commentf("role: %s", tc.role))
			qt.Assert(t, v.SchemeString(), qt.Equals, tc.want, qt.Commentf("role: %s", tc.role))
		})
	}
}

// TestForSyntaxSuppliedDefineValuesStillRefused records the residual this change
// does NOT close, so that closing it later is a visible edit here rather than a
// silent one.
//
// `define-values` and `define-record-type` are not on pruneDefinitionBinder's
// form list, and adding `begin-for-syntax` as a route to phase-1 definitions
// widens where that gap is reachable from. Refused before AND after this change,
// where racket answers 3 — so it is a real open item, not a shared refusal, and
// it has its own ledger row. Flip the assertion when that row closes.
func TestForSyntaxSuppliedDefineValuesStillRefused(t *testing.T) {
	eng := forSyntaxBinderEngine(t)
	_, err := eng.EvalProgram(context.Background(),
		`(define-syntax genvals
		   (syntax-rules () ((_ a b) (begin-for-syntax (define-values (a b) (values 1 2))))))
		 (genvals pp qq)
		 (define-syntax usepq (lambda (stx) (datum->syntax stx (+ pp qq))))
		 (usepq)`, "<test>")

	qt.Assert(t, err, qt.IsNotNil,
		qt.Commentf("racket answers 3 here; when Wile does too, this row closes"))
	qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue, qt.Commentf("got: %s", err))
}
