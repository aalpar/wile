package validate

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/values"
)

// engineWithoutSetBang returns a runtime env whose engine's FormRegistry is the
// default clone with "set!" removed. Under a per-engine registry, "set!" must
// then validate as an ordinary call in that engine only.
func engineWithoutSetBang() *environment.EnvironmentFrame {
	fr := forms.DefaultRegistry().Clone()
	fr.Remove("set!")
	ns := environment.NewNamespace()
	ns.SetFormRegistry(fr)
	return ns.Runtime()
}

// TestFormRegistry_PerEngineRemovesSetBang is the isolation oracle: with "set!"
// removed from this engine's registry, (set! x 1) validates as a *ValidatedCall,
// not the set! special form.
func TestFormRegistry_PerEngineRemovesSetBang(t *testing.T) {
	c := qt.New(t)
	env := engineWithoutSetBang()

	expr := makeSyntax(values.List(
		values.NewSymbol("set!"),
		values.NewSymbol("x"),
		values.NewInteger(1),
	))
	result := ValidateExpression(context.TODO(), env, expr)

	c.Assert(result.Ok(), qt.IsTrue)
	_, isCall := result.Expr.(*ValidatedCall)
	c.Assert(isCall, qt.IsTrue,
		qt.Commentf("set! removed from this engine's registry must validate as a call, got %T", result.Expr))
}

// TestFormRegistry_DefaultEngineKeepsSetBang guards that the change is scoped:
// a default engine (registry unchanged) still validates set! as the special form.
func TestFormRegistry_DefaultEngineKeepsSetBang(t *testing.T) {
	c := qt.New(t)
	ns := environment.NewNamespace()
	ns.SetFormRegistry(forms.DefaultRegistry().Clone()) // full default clone
	env := ns.Runtime()

	expr := makeSyntax(values.List(
		values.NewSymbol("set!"),
		values.NewSymbol("x"),
		values.NewInteger(1),
	))
	result := ValidateExpression(context.TODO(), env, expr)

	c.Assert(result.Ok(), qt.IsTrue)
	_, isSetBang := result.Expr.(*ValidatedSetBang)
	c.Assert(isSetBang, qt.IsTrue,
		qt.Commentf("default engine must keep set!, got %T", result.Expr))
}

// TestFormRegistry_InheritedIntoLambdaBody locks the seam before dialect Phase 2:
// the per-engine registry is honored inside nested validation envs (a lambda
// body), not just at top level.
func TestFormRegistry_InheritedIntoLambdaBody(t *testing.T) {
	c := qt.New(t)
	env := engineWithoutSetBang()

	// (lambda () (set! x 1))
	expr := makeSyntax(values.List(
		values.NewSymbol("lambda"),
		values.List(), // empty params
		values.List(values.NewSymbol("set!"), values.NewSymbol("x"), values.NewInteger(1)),
	))
	result := ValidateExpression(context.TODO(), env, expr)

	c.Assert(result.Ok(), qt.IsTrue)
	lam, isLambda := result.Expr.(*ValidatedLambda)
	c.Assert(isLambda, qt.IsTrue, qt.Commentf("expected lambda, got %T", result.Expr))
	c.Assert(len(lam.Body()) >= 1, qt.IsTrue)
	_, bodyIsCall := lam.Body()[0].(*ValidatedCall)
	c.Assert(bodyIsCall, qt.IsTrue,
		qt.Commentf("set! inside lambda body must inherit the per-engine registry and validate as a call, got %T", lam.Body()[0]))
}
