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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/syntax"
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

// TestP02_LambdaTransformerHygiene: on master a lambda transformer's output gets
// no intro scope at all (only the two Go transform sites and the ER rename
// closure mint one), so a datum->syntax #f binder captures a same-named use-site
// reference: the expected 5 comes back as #f. The kernel flip (design §2.2) is
// what fixes it.
func TestP02_LambdaTransformerHygiene(t *testing.T) {
	c := qt.New(t)
	got := evalSyntaxForms(t, `(define-syntax my-or2
  (lambda (stx)
    (let ((f (syntax->list stx)))
      (datum->syntax #f
        (list 'let (list (list 'tmp (cadr f)))
              (list 'if 'tmp 'tmp (caddr f)))))))
(define tmp 5)
(my-or2 #f tmp)`)
	c.Assert(got, qt.Equals, "5")
}

// TestP02_FreshnessAcrossInvocations (design §6 Freshness): a lambda transformer
// expanding to an internal define of a datum->syntax-made name, used twice in
// one body, must not collide — each invocation's flip gives the binder its own
// scope, so the two defines land in two distinct body slots. Each expansion also
// defines a user-named getter over the binder,
// which is what makes the collision observable: a bare `'ok` assertion passes on
// master, because a duplicate internal define is accepted silently. On master
// both binders are ∅-scoped, the second define wins both getters, and the answer
// is (2 2).
func TestP02_FreshnessAcrossInvocations(t *testing.T) {
	c := qt.New(t)
	src := `(define-syntax mk-def
  (lambda (stx)
    (let ((f (syntax->list stx)))
      (datum->syntax #f
        (list 'begin
              (list 'define 'tmp (caddr f))
              (list 'define (list (cadr f)) 'tmp))))))
(define (body) (mk-def g1 1) (mk-def g2 2) (list (g1) (g2)))
(body)`
	c.Assert(evalSyntaxForms(t, src), qt.Equals, "(1 2)")
}

// TestP02_NestedMacroThroughLambdaTransformer — GUARD: a lambda transformer that
// expands to a syntax-rules macro use; swap!'s tmp and the user's tmp stay apart
// with the Go matcher's intro scope gone (Q9) and the kernel's in its place.
func TestP02_NestedMacroThroughLambdaTransformer(t *testing.T) {
	c := qt.New(t)
	got := evalSyntaxForms(t, `(define-syntax swap!
  (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))
(define-syntax swap-via
  (lambda (stx)
    (let ((f (syntax->list stx)))
      (datum->syntax #f (list 'swap! (cadr f) (caddr f))))))
(let ((tmp 1) (y 2)) (swap-via tmp y) (list tmp y))`)
	c.Assert(got, qt.Equals, "(2 1)")
}

// TestP02_ExpandOnceMirrorsTheLoop — GUARD on the shape; the context-passing
// half of ExpandOnce is pinned in Task 4 where it becomes observable.
// `expand-once` rejects a non-syntax argument before any transformer runs
// (`PrimExpandOnce`, `extensions/eval/prim_eval.go`), so the input is a
// syntax object, not a quoted datum.
func TestP02_ExpandOnceMirrorsTheLoop(t *testing.T) {
	c := qt.New(t)
	got := evalSyntaxForms(t, `(define-syntax my-or2
  (lambda (stx)
    (let ((f (syntax->list stx)))
      (datum->syntax #f
        (list 'let (list (list 'tmp (cadr f)))
              (list 'if 'tmp 'tmp (caddr f)))))))
(call-with-values (lambda () (expand-once (datum->syntax #f '(my-or2 #f 1))))
  (lambda (stx ok) (list (syntax->datum stx) ok)))`)
	c.Assert(got, qt.Equals, "((let ((tmp #f)) (if tmp tmp 1)) #t)")
}

// TestP02_SyntaxLocalIntroduceIsWired: the expander context now carries the
// invocation's intro scope, so syntax-local-introduce flips it instead of
// raising not-implemented. Fails on master with ErrNotImplemented.
func TestP02_SyntaxLocalIntroduceIsWired(t *testing.T) {
	c := qt.New(t)
	got := evalSyntaxForms(t, `(define-syntax anaphoric
  (lambda (stx)
    (let ((f (syntax->list stx)))
      (datum->syntax #f
        (list 'let (list (list (syntax-local-introduce (datum->syntax #f 'it)) (cadr f)))
              (caddr f))))))
(anaphoric 42 it)`)
	c.Assert(got, qt.Equals, "42")
}

// TestP02_AifSurvivesTheFlip — GUARD (§0 F6): datum->syntax already copies the
// template's scopes on master, so a `(car f)` template puts the invocation's
// intro scope on `it` and the flip takes it off again; the anaphoric capture
// must come through P0.2 unchanged. Uses master primitives only, so it compiles
// on this branch. Passes on master and must keep passing here — P0.2 is the one
// phase that could break it, so the guard lives here rather than in Task 5.
func TestP02_AifSurvivesTheFlip(t *testing.T) {
	c := qt.New(t)
	got := evalSyntaxForms(t, `(define-syntax aif
  (lambda (stx)
    (let ((f (syntax->list stx)))
      (datum->syntax (car f)
        (list (datum->syntax #'aif 'let) (list (list 'it (cadr f)))
              (list (datum->syntax #'aif 'if) 'it (caddr f) (cadddr f)))))))
(list (aif 42 it 'no) (aif #f it 'no))`)
	c.Assert(got, qt.Equals, "(42 no)")
}

// TestP03_QuoteSyntaxPinIsTheTemplatePin (F7): the binding quote-syntax's pin
// dereferences to is the one the Go syntax template producer stamps on the same
// free identifier at the same frame (applyHygieneToSymbol, the ellipsis path,
// reached here through a top-level syntax-case). GlobalIndex values are minted
// per query, so the comparison is on the *environment.Binding both reach.
func TestP03_QuoteSyntaxPinIsTheTemplatePin(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()

	quoted, err := eng.EvalMultiple(ctx, `(quote-syntax car)`)
	c.Assert(err, qt.IsNil)
	qsym, ok := quoted.Internal().(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue, qt.Commentf("%T", quoted.Internal()))
	qgi, ok := qsym.ResolvedBinding.(*environment.GlobalIndex)
	c.Assert(ok, qt.IsTrue, qt.Commentf("quote-syntax stamped %T", qsym.ResolvedBinding))

	expanded, err := eng.EvalMultiple(ctx, `(syntax-case #'(1 2) () ((x ...) (syntax (car x ...))))`)
	c.Assert(err, qt.IsNil)
	pair, ok := expanded.Internal().(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue, qt.Commentf("%T", expanded.Internal()))
	tsym, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	tgi, ok := tsym.ResolvedBinding.(*environment.GlobalIndex)
	c.Assert(ok, qt.IsTrue, qt.Commentf("template producer stamped %T", tsym.ResolvedBinding))

	c.Assert(qgi.Env.GetOwnGlobalBinding(qgi), qt.Equals, tgi.Env.GetOwnGlobalBinding(tgi))
}

// TestP03_QuoteSyntaxSkipsThePinUnderALocal: no global pin where a
// definition-site local of the name resolves.
func TestP03_QuoteSyntaxSkipsThePinUnderALocal(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()
	v, err := eng.EvalMultiple(ctx, `(let ((car 1)) (quote-syntax car))`)
	c.Assert(err, qt.IsNil)
	sym, ok := v.Internal().(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue, qt.Commentf("%T", v.Internal()))
	c.Assert(sym.ResolvedBinding, qt.IsNil)
}

// qLibFS: mac's output names the library-private helper, defined AFTER mac, through
// quote-syntax. helper is predeclared before mac compiles, so the pin is non-nil,
// and the library body already carries the library scope (design §2.1 item 2); a
// use-site helper must not capture either. This pins that quote-syntax reaches a
// library-private binding at all — it does not discriminate the
// AddScope(p.libraryScope) line, which is a no-op for a template read in the body.
var qLibFS = fstest.MapFS{
	"qlib.sld": &fstest.MapFile{
		Data: []byte(`(define-library (qlib)
  (import (scheme base))
  (export mac)
  (begin
    (define-syntax mac
      (lambda (stx) (datum->syntax #f (list (quote-syntax helper) 1))))
    (define (helper x) (list 'lib x))))`),
	},
}

func TestP03_QuoteSyntaxCarriesTheLibraryScope(t *testing.T) {
	c := qt.New(t)
	got := evalSyntaxForms(t, `(import (qlib))
(define (helper x) (list 'user x))
(mac)`, wile.WithSourceFS(stdlib.FS), wile.WithSourceFS(qLibFS), wile.WithLibraryPaths())
	c.Assert(got, qt.Equals, "(lib 1)")
}

// TestP03_QuoteSyntaxDefSiteLocalUnderUseSiteLet (design §2.1 item 3): the
// identifier keeps its read scopes, so the definition-site x resolves by subset
// through the whole macro and the use-site x cannot capture it.
func TestP03_QuoteSyntaxDefSiteLocalUnderUseSiteLet(t *testing.T) {
	c := qt.New(t)
	got := evalSyntaxForms(t, `(let ((x 'def))
  (define-syntax m (lambda (stx) (quote-syntax x)))
  (let ((x 'use)) (m)))`)
	c.Assert(got, qt.Equals, "def")
}
