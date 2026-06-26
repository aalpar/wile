package wile_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// dottedTailEngine builds an engine that can import the sealed stdlib
// libraries (chibi optional/diff live there).
func dottedTailEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths())
	qt.Assert(t, err, qt.IsNil)
	return eng
}

// TestSyntaxRulesNestedSublistDottedTail is the end-to-end guard for C12/C13:
// a syntax-rules clause whose pattern has a nested sub-list in NON-FINAL
// position AND an improper `. body` tail must match a multi-arg call.
// Per R7RS §4.3.2, `(P1 ... Pn . Px)` matches an input of n-or-more elements
// where the first n match P1..Pn and the nth cdr matches Px.
//
// Before the fix the matcher dropped the improper tail of a pair/vector
// sub-pattern, so `(m (1) 9)` raised "no matching clause for input".
func TestSyntaxRulesNestedSublistDottedTail(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	src := `(define-syntax m (syntax-rules () ((m (x) . body) (list x (quote ->) (quote body)))))
	        (m (1) 9)`
	result, err := eng.EvalMultiple(ctx, src)
	c.Assert(err, qt.IsNil, qt.Commentf("(m (1) 9) must expand and not error"))
	c.Assert(result.SchemeString(), qt.Equals, "(1 -> (9))")
}

// TestSyntaxRulesNestedSublistDottedTailMultiArg covers the same shape with a
// longer improper tail: the captured `body` collects every element after the
// nested sub-list.
func TestSyntaxRulesNestedSublistDottedTailMultiArg(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	src := `(define-syntax m (syntax-rules () ((m (x) . body) (list x (quote ->) (quote body)))))
	        (m (5) 7 8 9)`
	result, err := eng.EvalMultiple(ctx, src)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "(5 -> (7 8 9))")
}

// TestSyntaxRulesEllipsisDottedTail covers the companion matcher fix: an
// ellipsis followed by an improper `. rest` tail (`(a ... . rest)`). This shape
// is what (chibi optional)'s let-optionals macro depends on. A proper-list
// input drives the ellipsis to exhaustion, after which `rest` binds to ().
func TestSyntaxRulesEllipsisDottedTail(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	src := `(define-syntax m (syntax-rules () ((m (a ... . rest)) (list (list a ...) rest))))
	        (m (1 2 3))`
	result, err := eng.EvalMultiple(ctx, src)
	c.Assert(err, qt.IsNil, qt.Commentf("(a ... . rest) must match a proper list, rest=()"))
	c.Assert(result.SchemeString(), qt.Equals, "((1 2 3) ())")
}

// TestChibiOptionalLoads verifies (chibi optional) — previously dead because of
// the C12/C13 matcher bug in its let-optionals/let*-to-let macros — now loads
// and its opt-lambda works for both the defaulted and the supplied-argument
// cases.
func TestChibiOptionalLoads(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := dottedTailEngine(t)
	defer eng.Close()

	src := `(import (chibi optional))
	        (define f (opt-lambda ((x 1)) x))
	        (list (f) (f 42))`
	result, err := eng.EvalMultiple(ctx, src)
	c.Assert(err, qt.IsNil, qt.Commentf("(chibi optional) must load and opt-lambda must work"))
	c.Assert(result.SchemeString(), qt.Equals, "(1 42)")
}

// TestChibiDiffLoads verifies (chibi diff) — which imports (chibi optional) —
// now loads, and its lcs procedure computes the longest common subsequence.
func TestChibiDiffLoads(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := dottedTailEngine(t)
	defer eng.Close()

	src := `(import (chibi diff))
	        (lcs '(1 2 3) '(1 9 3) =)`
	result, err := eng.EvalMultiple(ctx, src)
	c.Assert(err, qt.IsNil, qt.Commentf("(chibi diff) must load and lcs must work"))
	c.Assert(result.SchemeString(), qt.Equals, "(1 3)")
}
