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

// TestP04_NonProcedureIsACompileTimeValue (Q4): a non-procedure right-hand side
// is stored bare and read back by syntax-local-value; in operator position it is
// the existing ErrNotAClosure. Fails on master at the definition site.
//
// This replaces TestP01_NonProcedureRefusedUntilP04, which pinned P0.1's
// admission ladder — the refusal this phase lifts.
func TestP04_NonProcedureIsACompileTimeValue(t *testing.T) {
	c := qt.New(t)
	got := evalSyntaxForms(t, `(define-syntax k 42)
(define-syntax probe (lambda (stx) (datum->syntax #f (syntax-local-value #'k))))
(probe)`)
	c.Assert(got, qt.Equals, "42")
	err := evalSyntaxFormsErr(t, `(define-syntax k 42) (k)`)
	c.Assert(err, qt.ErrorMatches, `(?s).*not a closure.*`)
}

// TestP04_LetSyntaxProbeAtPhase1And2 (design §2.3, measured 2026-09-05 to fail
// "no binding for pv" at both phases): syntax-local-value resolves through the
// let-syntax frames the way macro dispatch does.
func TestP04_LetSyntaxProbeAtPhase1And2(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name, src, want string
	}{
		{
			// The body's value is the syntax LITERAL (probe) expands to, which
			// evaluates to its datum; datum->syntax turns it back into what a
			// transformer must return.
			name: "phase 1: let-syntax keyword inside a transformer body",
			src: `(define-syntax m
  (lambda (stx)
    (datum->syntax #f
      (let-syntax ((pv (lambda (z) z)))
        (let-syntax ((probe (lambda (w) (if (procedure? (syntax-local-value #'pv)) #'1 #'2))))
          (probe))))))
(m)`,
			want: "1",
		},
		{
			// inner's body is wrapped the way m's is above: the probe's
			// expansion evaluates to the integer 7, and expandMacroInvocation
			// refuses a non-syntax transformer result, which Task 2 keeps.
			name: "phase 2: the same probe inside a transformer inside a transformer",
			src: `(define-syntax outer
  (lambda (stx)
    (define-syntax inner
      (lambda (s)
        (datum->syntax #f
          (let-syntax ((pv 7))
            (let-syntax ((probe (lambda (w) (datum->syntax #f (syntax-local-value #'pv)))))
              (probe))))))
    (datum->syntax #f (inner))))
(outer)`,
			want: "7",
		},
		{
			name: "phase 0: top-level let-syntax value",
			src: `(let-syntax ((k 42))
  (let-syntax ((probe (lambda (w) (datum->syntax #f (syntax-local-value #'k)))))
    (probe)))`,
			want: "42",
		},
		{
			// The thunk's value is a symbol, so the transformer has to quote it:
			// a bare `none` in the expansion is a variable reference and the
			// compiler refuses it as an unbound binding.
			name: "failure thunk",
			src: `(define-syntax probe3
  (lambda (w)
    (datum->syntax #f (list 'quote (syntax-local-value #'nope (lambda () 'none))))))
(probe3)`,
			want: "none",
		},
		{
			// expand-once takes a syntax object, not a datum: PrimExpandOnce
			// refuses anything else before a transformer runs. Through a syntax
			// argument the nil context ExpandOnce passed on master made this
			// raise ErrNoCaptureContext (design §2.2); Task 2 handed it the
			// context.
			name: "expand-once passes the expander context",
			src: `(define-syntax k2 42)
(define-syntax probe2 (lambda (w) (datum->syntax #f (syntax-local-value #'k2))))
(call-with-values (lambda () (expand-once #'(probe2)))
  (lambda (stx ok) (syntax->datum stx)))`,
			want: "42",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(evalSyntaxForms(t, tc.src), qt.Equals, tc.want)
		})
	}
}

// TestP04_CompileTimeValueWrapperIsGone: make-compile-time-value and its wrapper
// are retired (Q4); a define-syntax value reads back bare.
func TestP04_CompileTimeValueWrapperIsGone(t *testing.T) {
	c := qt.New(t)
	err := evalSyntaxFormsErr(t, `(make-compile-time-value 1)`)
	c.Assert(err, qt.ErrorMatches, `(?s).*make-compile-time-value.*`)
	got := evalSyntaxForms(t, `(define-syntax cv 9)
(define-syntax probe (lambda (w) (datum->syntax #f (syntax-local-value #'cv))))
(probe)`)
	c.Assert(got, qt.Equals, "9")
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

// TestP05_Accessors: the one-level accessors (design §2.4) and the ER spine (Q6).
func TestP05_Accessors(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name, src, want string
	}{
		{"syntax-pair?", `(list (syntax-pair? #'(a b)) (syntax-pair? #'()) (syntax-pair? #'a) (syntax-pair? '(a)))`, "(#t #f #f #f)"},
		{"syntax-null?", `(list (syntax-null? #'()) (syntax-null? (syntax-cdr (syntax-cdr #'(a b)))) (syntax-null? '()) (syntax-null? #'(a)))`, "(#t #t #t #f)"},
		{"syntax-car keeps syntax", `(list (identifier? (syntax-car #'(a b))) (syntax->datum (syntax-car #'(a b))))`, "(#t a)"},
		{"syntax-cdr keeps syntax", `(list (syntax-pair? (syntax-cdr #'(a b))) (syntax->datum (syntax-cdr #'(a . b))))`, "(#t b)"},
		{"syntax-vector?", `(list (syntax-vector? #'#(1 2)) (syntax-vector? #'(1 2)))`, "(#t #f)"},
		{"syntax-vector->list is a syntax list", `(let ((l (syntax-vector->list #'#(a 2)))) (list (syntax-pair? l) (identifier? (syntax-car l)) (syntax->datum l)))`, "(#t #t (a 2))"},
		{"%syntax-spine: pairs and vectors unwrapped, identifiers kept", `(let ((s (%syntax-spine #'(a (b . 1) #(c) 2))))
  (list (pair? s) (identifier? (car s)) (pair? (cadr s)) (identifier? (car (cadr s))) (cdr (cadr s))
        (vector? (caddr s)) (identifier? (vector-ref (caddr s) 0)) (cadddr s)))`, "(#t #t #t #t 1 #t #t 2)"},
		{"%syntax-spine: empty and improper tails", `(let ((s (%syntax-spine #'(a . b)))) (list (identifier? (car s)) (identifier? (cdr s)) (null? (%syntax-spine #'()))))`, "(#t #t #t)"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(evalSyntaxForms(t, tc.src), qt.Equals, tc.want)
		})
	}
	c.Assert(evalSyntaxFormsErr(t, `(syntax-car #'())`), qt.ErrorMatches, `(?s).*syntax-car: expected a syntax pair.*`)
}

// TestP05_SyntaxViolationCarriesSource (design §6 Diagnostics): the raise helper
// the Scheme layer uses is a catchable error object whose message and irritant
// are readable (R7RS §6.11) and whose cause carries the form's location as a
// real chain member.
func TestP05_SyntaxViolationCarriesSource(t *testing.T) {
	c := qt.New(t)
	got := evalSyntaxForms(t, `(guard (e (#t (list (error-object-message e) (error-object-irritants e))))
  (%syntax-violation 'who "bad form" #'(x y)))`)
	c.Assert(got, qt.Equals, `("who: bad form" ((x y)))`)

	err := evalSyntaxFormsErr(t, "\n\n(%syntax-violation 'who \"bad form\" #'(x y))")
	c.Assert(err, qt.IsNotNil)
	var located interface {
		SourceContext() *syntax.SourceContext
	}
	c.Assert(errors.As(err, &located), qt.IsTrue, qt.Commentf("%v", err))
	c.Assert(located.SourceContext().Start.Line(), qt.Equals, 3)
}

// TestP05_FreeIdentifierEqualShadowProbe (design §3.1, measured 2026-09-05 as
// ("else" "else") through the primitive): a use-site local shadows a literal.
func TestP05_FreeIdentifierEqualShadowProbe(t *testing.T) {
	c := qt.New(t)
	got := evalSyntaxForms(t, `(define-syntax m
  (lambda (stx)
    (let ((f (syntax->list stx)))
      (if (free-identifier=? (cadr f) (quote-syntax else)) #''else #''not))))
(list (m else) (let ((else 1)) (m else)))`)
	c.Assert(got, qt.Equals, "(else not)")
}

// TestP05_DatumToSyntax pins what Q5 asked for. F6: scopes were ALREADY copied on
// master (three probes, 2026-09-05), and no path can copy a pin either
// (NewSyntaxSymbolForSymbol never sets ResolvedBinding), so P0.5 changes no
// datum->syntax behaviour and every row here is a GUARD. The no-pin-copy row and
// its Go consequence fail on master only because quote-syntax is unbound there:
// Task 3 makes the answer observable, it does not change it.
func TestP05_DatumToSyntax(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name, src, want string
	}{
		{"guard: template scopes reach a use-site local", `(let ((x 'outer))
  (define-syntax m (lambda (stx) (datum->syntax (car (syntax->list stx)) 'x)))
  (let ((x 'inner)) (m)))`, "inner"},
		{"guard: #f template has no scopes", `(define zz4 'global)
(define-syntax m4 (lambda (stx) (datum->syntax #f 'zz4)))
(let ((zz4 'local)) (m4))`, "global"},
		{"guard: rename of a definition-site local (referential transparency)", `(let ((helper (lambda () 'def-helper)))
  (define-syntax m (lambda (stx) (datum->syntax #'helper (list 'helper))))
  (let ((helper (lambda () 'use-helper))) (m)))`, "def-helper"},
		{"guard: the pin is never copied", `(free-identifier=? (datum->syntax (quote-syntax car) 'foo) (quote-syntax car))`, "#f"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(evalSyntaxForms(t, tc.src), qt.Equals, tc.want)
		})
	}

	// guard: datum->syntax mints a fresh symbol, so no pin reaches the result
	// here either, on master and after P0.5 alike.
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()
	v, err := eng.EvalMultiple(ctx, `(datum->syntax (quote-syntax car) 'car)`)
	c.Assert(err, qt.IsNil)
	sym, ok := v.Internal().(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(sym.ResolvedBinding, qt.IsNil)
}
