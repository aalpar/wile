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

// evalSyntaxForms evaluates src on a KitchenSink engine and returns the last
// value's SchemeString. KitchenSink because Task 4's expand-once pin needs the
// eval extension.
func evalSyntaxForms(t *testing.T, src string, opts ...wile.EngineOption) string {
	t.Helper()
	ctx := context.Background()
	opts = append([]wile.EngineOption{wile.WithProfile(wile.KitchenSink)}, opts...)
	eng, err := wile.NewEngine(ctx, opts...)
	if err != nil {
		t.Fatalf("NewEngine: %v", err)
	}
	defer func() {
		_ = eng.Close()
	}()
	v, err := eng.EvalMultiple(ctx, src)
	if err != nil {
		t.Fatalf("EvalMultiple(%s): %v", src, err)
	}
	return v.SchemeString()
}

// evalSyntaxFormsErr is evalSyntaxForms for a source expected to fail.
func evalSyntaxFormsErr(t *testing.T, src string, opts ...wile.EngineOption) error {
	t.Helper()
	ctx := context.Background()
	opts = append([]wile.EngineOption{wile.WithProfile(wile.KitchenSink)}, opts...)
	eng, err := wile.NewEngine(ctx, opts...)
	if err != nil {
		t.Fatalf("NewEngine: %v", err)
	}
	defer func() {
		_ = eng.Close()
	}()
	_, err = eng.EvalMultiple(ctx, src)
	return err
}

// recLibFS exports rec-len2, whose template names the library-private RECURSIVE
// macro rec-len, itself produced by a macro (mk-rec). Observing 'hijacked means
// the use site's own (define-syntax rec-len …) captured rec-len's self-reference:
// the predeclared slot is what the self-reference must pin to (design §2.2).
var recLibFS = fstest.MapFS{
	"reclib.sld": &fstest.MapFile{
		Data: []byte(`(define-library (reclib)
  (import (scheme base))
  (export rec-len2)
  (begin
    (define-syntax mk-rec
      (syntax-rules ()
        ((_ name) (syntax-rules () ((_) 0) ((_ x . r) (+ 1 (name . r)))))))
    (define-syntax rec-len (mk-rec rec-len))
    (define-syntax rec-len2 (syntax-rules () ((_ . a) (rec-len . a))))))`),
	},
}

// TestP01_TransformerIsAnExpression pins design §7 P0.1. Rows 1-3 fail on master,
// each with its own message: row 1 "unsupported transformer type", row 2 "form
// syntax-rules has no compiler", row 3 "transformer must be a list" (the non-pair
// guard at compile_transformer.go:56 fires before the head switch); rows
// 4-6 are GUARDS for F2: the Go producer's definition-site local arm stops firing
// from a define-syntax / let-syntax / letrec-syntax right-hand side in P0.1, and
// these are the three shapes it used to serve.
func TestP01_TransformerIsAnExpression(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name string
		src  string
		want string
	}{
		{
			// §5.2 repro: a macro that expands to a transformer.
			name: "macro-produced transformer",
			src: `(define-syntax my-er (syntax-rules () ((_ p) (lambda (stx) (p stx)))))
(define-syntax m (my-er (lambda (stx) #'1)))
(m)`,
			want: "1",
		},
		{
			name: "syntax-rules is an expression",
			src:  `(define t (syntax-rules () ((_ x) x))) (procedure? t)`,
			want: "#t",
		},
		{
			// A variable reference is a legal right-hand side (R6RS §11.2.2).
			// EvalMultiple compiles form by form, so t2 exists by the time m2
			// compiles. After P0.1 removes the non-pair guard, the same three
			// forms under the CLI's (begin …) wrap fail instead with `no such
			// binding "t2"`: a define-syntax inside a begin body
			// compiles during expansion (expander_body.go:113-118), before any
			// define-for-syntax / begin-for-syntax in that body has run. That is
			// design §5.5, which P0.1 does not close.
			name: "define-for-syntax value as transformer",
			src: `(define-for-syntax t2 (lambda (stx) (datum->syntax #f 7)))
(define-syntax m2 t2)
(m2)`,
			want: "7",
		},
		{
			// GUARD (F2 a): definition-site local under a same-named use-site let.
			name: "guard: def-site local beats use-site let",
			src: `(let ((x 'def))
  (define-syntax m (syntax-rules () ((_) x)))
  (let ((x 'use)) (m)))`,
			want: "def",
		},
		{
			// GUARD (F2 b): the producer now pins the GLOBAL x (no locals at
			// NextPhase); the compiler's local-before-pin order must still answer local.
			name: "guard: def-site local shadows a same-named global",
			src: `(define x 'global)
(let ((x 'local))
  (define-syntax m (syntax-rules () ((_) x)))
  (m))`,
			want: "local",
		},
		{
			// GUARD (F2 c): letrec-syntax sibling keyword shadowing a same-named
			// global macro; arm 1 (scoped, co-introduced) must win over the pin.
			name: "guard: letrec-syntax sibling shadows a global macro",
			src: `(define-syntax g (syntax-rules () ((_) 'global)))
(letrec-syntax ((g (syntax-rules () ((_) 'sibling)))
                (m (syntax-rules () ((_) (g)))))
  (m))`,
			want: "sibling",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(evalSyntaxForms(t, tc.src), qt.Equals, tc.want)
		})
	}
}

// TestP01_SelfReferencePinsThroughMacroProducedTransformer: rec-len's template
// names rec-len before its slot exists; the predeclare (design §2.2) makes the
// pin real. Fails on master at library load ("unsupported transformer type").
func TestP01_SelfReferencePinsThroughMacroProducedTransformer(t *testing.T) {
	c := qt.New(t)
	got := evalSyntaxForms(t, `(import (reclib))
(define-syntax rec-len (syntax-rules () ((_ . a) 'hijacked)))
(rec-len2 1 2 3)`,
		wile.WithSourceFS(stdlib.FS), wile.WithSourceFS(recLibFS), wile.WithLibraryPaths())
	c.Assert(got, qt.Equals, "3")
}

// TestP01_FailedRightHandSideLeavesNoSlot: the predeclared slot is removed when
// the right-hand side fails, so the keyword is not left bound to nothing. One
// engine for both evaluations — the slot would be invisible across engines. GUARD.
func TestP01_FailedRightHandSideLeavesNoSlot(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()
	_, err = eng.EvalMultiple(ctx, `(define-syntax bad (er-macro-transformer (lambda (a b) 1)))`)
	c.Assert(err, qt.IsNotNil)
	v, err := eng.EvalMultiple(ctx, `(define (bad) 'ok) (bad)`)
	c.Assert(err, qt.IsNil)
	c.Assert(v.SchemeString(), qt.Equals, "ok")
}

// TestP01_NonProcedureRefusedUntilP04: P0.1 admits a closure or an ER
// transformer and refuses anything else at the definition site (design §2.2
// admission ladder). Task 4 flips this test's expectation.
func TestP01_NonProcedureRefusedUntilP04(t *testing.T) {
	c := qt.New(t)
	err := evalSyntaxFormsErr(t, `(define-syntax k 42)`)
	c.Assert(err, qt.ErrorMatches, `(?s).*transformer must evaluate to a procedure.*`)
}
