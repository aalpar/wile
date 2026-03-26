package wile_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/stdlib"
)

func TestEngine_EmbeddedStdlib_SchemeBase(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (scheme base))
		(+ 1 2)
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")
}

func TestEngine_EmbeddedStdlib_ChibiTest(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	_, err = eng.EvalMultiple(ctx, `
		(import (chibi test))
		(test-begin "embedded")
		(test-assert (= 1 1))
		(test-end)
	`)
	c.Assert(err, qt.IsNil)
}

func TestEngine_EmbeddedStdlib_SRFI1(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (srfi 1))
		(fold + 0 '(1 2 3 4 5))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "15")
}

func TestEngine_EmbeddedStdlib_WileAlgebra(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra group))
		(group? (make-group + - 0))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#t")
}

func TestAlgebraRewrite_IdentityAxiom(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	// Term protocol for simple (op left right) lists
	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))

		(define proto
		  (make-term-protocol
		    car                             ;; get-operator
		    cdr                             ;; get-operands
		    (lambda (term new-args)         ;; make-term: term × new-operands → term
		      (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b))))) ;; compare

		(define theory (list (identity-axiom '+ (lambda (x) (eq? x 'zero)))))
		(define normalize (make-normalizer theory proto))

		;; (+ x zero) → x
		(normalize '(+ x zero))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "x")
}

func TestAlgebraRewrite_IdentityAxiomLeftZero(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))

		(define proto
		  (make-term-protocol car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))

		(define normalize
		  (make-normalizer (list (identity-axiom '+ (lambda (x) (eq? x 'zero)))) proto))

		;; (+ zero x) → x
		(normalize '(+ zero x))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "x")
}

func TestAlgebraRewrite_NoMatchReturnsFalse(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))

		(define proto
		  (make-term-protocol car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))

		(define normalize
		  (make-normalizer (list (identity-axiom '+ (lambda (x) (eq? x 'zero)))) proto))

		;; (+ a b) — no zero operand, returns #f
		(normalize '(+ a b))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#f")
}

func TestAlgebraRewrite_Commutativity(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))
		(define proto
		  (make-term-protocol car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
		(define normalize (make-normalizer (list (commutativity-axiom '+)) proto))
		;; (+ y a) → (+ a y) because a < y
		(normalize '(+ y a))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "(+ a y)")
}

func TestAlgebraRewrite_CommutativityAlreadyOrdered(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))
		(define proto
		  (make-term-protocol car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
		(define normalize (make-normalizer (list (commutativity-axiom '+)) proto))
		;; (+ a y) — already ordered, returns #f
		(normalize '(+ a y))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#f")
}

func TestAlgebraRewrite_Absorbing(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))
		(define proto
		  (make-term-protocol car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
		(define normalize
		  (make-normalizer (list (absorbing-axiom '* (lambda (x) (eq? x 'zero)))) proto))
		;; (* x zero) → zero
		(normalize '(* x zero))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "zero")
}

func TestAlgebraRewrite_Idempotence(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))
		(define proto
		  (make-term-protocol car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
		(define normalize (make-normalizer (list (idempotence-axiom '&)) proto))
		;; (& x x) → x
		(normalize '(& x x))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "x")
}

func TestAlgebraRewrite_Involution(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))
		(define proto
		  (make-term-protocol car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
		(define normalize (make-normalizer (list (involution-axiom '!)) proto))
		;; (! (! x)) → x
		(normalize '(! (! x)))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "x")
}

func TestAlgebraRewrite_ComposedTheory(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))
		(define proto
		  (make-term-protocol car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
		(define zero? (lambda (x) (eq? x 'zero)))
		(define theory
		  (list (identity-axiom '+ zero?)
		        (commutativity-axiom '+)
		        (absorbing-axiom '* zero?)))
		(define normalize (make-normalizer theory proto))
		;; Identity: (+ x zero) → x
		;; Absorbing: (* y zero) → zero
		;; Commutativity: (+ y a) → (+ a y)
		(list (normalize '(+ x zero))
		      (normalize '(* y zero))
		      (normalize '(+ y a)))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "(x zero (+ a y))")
}

// TestEngine_EmbeddedStdlib_NoOSFallback verifies that the embedded FS
// resolves standard libraries without OS filesystem fallback. This catches
// path mismatches between the embed structure and DefaultLibraryPaths.
func TestEngine_EmbeddedStdlib_NoOSFallback(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (scheme base))
		(+ 1 2)
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")
}
