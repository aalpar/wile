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

package machine

import (
	"context"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/internal/validate"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// newDynamicWindEnv creates a test environment with dynamic-wind binding
// and helper primitives for tracking call order via foreign closures.
func newDynamicWindEnv() *environment.EnvironmentFrame {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())

	// Register dynamic-wind as a primitive binding so the expander/validator recognize it
	dwSym := env.InternSymbol(values.NewSymbol("dynamic-wind"))
	env.MaybeCreateOwnGlobalBinding(dwSym, environment.BindingTypePrimitive)

	// Register case-lambda as a primitive binding
	clSym := env.InternSymbol(values.NewSymbol("case-lambda"))
	env.MaybeCreateOwnGlobalBinding(clSym, environment.BindingTypePrimitive)

	return env
}

// registerForeignFn registers a zero-argument foreign closure in the environment.
func registerForeignFn(env *environment.EnvironmentFrame, name string, fn func(context.Context, *MachineContext) error) {
	sym := env.InternSymbol(values.NewSymbol(name))
	env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
	closure := NewForeignClosure(env, 0, false, fn)
	env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure) //nolint:errcheck
}

// compileAndRun parses, expands, compiles, and runs a Scheme expression.
func compileAndRun(t *testing.T, env *environment.EnvironmentFrame, code string) *MachineContext {
	t.Helper()
	sv := parseSchemeExpr(t, env, code)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	return mc
}

// compileAndRunExpectError parses, expands, compiles, and runs expecting an error.
func compileAndRunExpectError(t *testing.T, env *environment.EnvironmentFrame, code string) error {
	t.Helper()
	sv := parseSchemeExpr(t, env, code)
	cont, err := newTopLevelThunk(sv, env)
	if err != nil {
		return err
	}
	mc := NewMachineContext(context.Background(), cont)
	return mc.Run()
}

// TestCompileValidatedDynamicWind_ReturnValue verifies that dynamic-wind returns
// the thunk's value.
func TestCompileValidatedDynamicWind_ReturnValue(t *testing.T) {
	env := newDynamicWindEnv()

	// Register no-op thunks for before/after
	registerForeignFn(env, "noop", func(_ context.Context, mc *MachineContext) error {
		mc.SetValue(values.Void)
		return nil
	})

	// Register a thunk that returns 42
	registerForeignFn(env, "ret42", func(_ context.Context, mc *MachineContext) error {
		mc.SetValue(values.NewInteger(42))
		return nil
	})

	mc := compileAndRun(t, env, "(dynamic-wind noop ret42 noop)")
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestCompileValidatedDynamicWind_CallOrder verifies that before, thunk, and after
// are called in the correct order.
func TestCompileValidatedDynamicWind_CallOrder(t *testing.T) {
	env := newDynamicWindEnv()

	var log []string

	registerForeignFn(env, "before-fn", func(_ context.Context, mc *MachineContext) error {
		log = append(log, "before")
		mc.SetValue(values.Void)
		return nil
	})

	registerForeignFn(env, "thunk-fn", func(_ context.Context, mc *MachineContext) error {
		log = append(log, "thunk")
		mc.SetValue(values.NewString("result"))
		return nil
	})

	registerForeignFn(env, "after-fn", func(_ context.Context, mc *MachineContext) error {
		log = append(log, "after")
		mc.SetValue(values.Void)
		return nil
	})

	mc := compileAndRun(t, env, "(dynamic-wind before-fn thunk-fn after-fn)")

	qt.Assert(t, log, qt.DeepEquals, []string{"before", "thunk", "after"})
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewString("result"))
}

// TestCompileValidatedDynamicWind_WithLambdas verifies dynamic-wind works with
// inline lambda expressions rather than named thunks.
func TestCompileValidatedDynamicWind_WithLambdas(t *testing.T) {
	env := newDynamicWindEnv()

	var log []string

	registerForeignFn(env, "log-before", func(_ context.Context, mc *MachineContext) error {
		log = append(log, "before")
		mc.SetValue(values.Void)
		return nil
	})

	registerForeignFn(env, "log-after", func(_ context.Context, mc *MachineContext) error {
		log = append(log, "after")
		mc.SetValue(values.Void)
		return nil
	})

	mc := compileAndRun(t, env,
		`(dynamic-wind
			(lambda () (log-before))
			(lambda () 99)
			(lambda () (log-after)))`)

	qt.Assert(t, log, qt.DeepEquals, []string{"before", "after"})
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(99))
}

// TestCompileValidatedDynamicWind_Nested verifies correct ordering when
// dynamic-wind forms are nested.
func TestCompileValidatedDynamicWind_Nested(t *testing.T) {
	env := newDynamicWindEnv()

	var log []string

	makeFn := func(label string) func(context.Context, *MachineContext) error {
		return func(_ context.Context, mc *MachineContext) error {
			log = append(log, label)
			mc.SetValue(values.Void)
			return nil
		}
	}

	registerForeignFn(env, "outer-before", makeFn("outer-before"))
	registerForeignFn(env, "outer-after", makeFn("outer-after"))
	registerForeignFn(env, "inner-before", makeFn("inner-before"))
	registerForeignFn(env, "inner-after", makeFn("inner-after"))
	registerForeignFn(env, "body", func(_ context.Context, mc *MachineContext) error {
		log = append(log, "body")
		mc.SetValue(values.NewInteger(7))
		return nil
	})

	mc := compileAndRun(t, env,
		`(dynamic-wind
			outer-before
			(lambda ()
				(dynamic-wind
					inner-before
					body
					inner-after))
			outer-after)`)

	qt.Assert(t, log, qt.DeepEquals, []string{
		"outer-before",
		"inner-before",
		"body",
		"inner-after",
		"outer-after",
	})
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(7))
}

// TestCompileValidated_UnknownExprType verifies the default branch in
// compileValidated returns an error for unknown ValidatedExpr types.
func TestCompileValidated_UnknownExprType(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	ctc := NewCompiletimeContinuation(tpl, env)
	ctctx := NewCompileTimeCallContext(context.Background(), false, true)

	// Create a mock ValidatedExpr that compileValidated doesn't know about
	mock := &mockValidatedExpr{}
	err := ctc.compileValidated(ctctx, mock)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err, qt.ErrorMatches, `unknown validated expression type: \*machine\.mockValidatedExpr`)
}

// mockValidatedExpr implements validate.ValidatedExpr for testing the
// exhaustiveness check in compileValidated's default branch.
type mockValidatedExpr struct{}

func (p *mockValidatedExpr) SetFormName(_ string) {}
func (p *mockValidatedExpr) FormName() string     { return "" }
func (p *mockValidatedExpr) Source() *syntax.SourceContext {
	return syntax.NewZeroValueSourceContext()
}

// Verify interface satisfaction at compile time.
var _ validate.ValidatedExpr = (*mockValidatedExpr)(nil)

// TestSetScopesOnLastBinding_EmptyBindings verifies the guard when
// setScopesOnLastBinding is called on a LocalEnvironmentFrame with no bindings.
func TestSetScopesOnLastBinding_EmptyBindings(t *testing.T) {
	c := qt.New(t)

	lenv := environment.NewLocalEnvironment(0)
	scopes := []*syntax.Scope{syntax.NewScope()}

	// Should not panic - the empty bindings guard handles this
	setScopesOnLastBinding(scopes, lenv)
	c.Assert(lenv.Bindings(), qt.HasLen, 0)
}

// TestSetScopesOnLastBinding_WithBinding verifies that scopes are applied
// to the last binding in the environment.
func TestSetScopesOnLastBinding_WithBinding(t *testing.T) {
	c := qt.New(t)

	lenv := environment.NewLocalEnvironment(0)
	sym := values.NewSymbol("x")
	lenv.EnsureLocalBinding(sym, environment.BindingTypeVariable)

	scope := syntax.NewScope()
	setScopesOnLastBinding([]*syntax.Scope{scope}, lenv)

	bindings := lenv.Bindings()
	c.Assert(bindings, qt.HasLen, 1)
	c.Assert(bindings[0].Scopes(), qt.HasLen, 1)
}

// TestSetScopesOnLastBinding_NilScopes verifies that nil scopes is a no-op.
func TestSetScopesOnLastBinding_NilScopes(t *testing.T) {
	lenv := environment.NewLocalEnvironment(0)
	sym := values.NewSymbol("x")
	lenv.EnsureLocalBinding(sym, environment.BindingTypeVariable)

	// Should not modify anything
	setScopesOnLastBinding(nil, lenv)

	bindings := lenv.Bindings()
	qt.Assert(t, bindings, qt.HasLen, 1)
	qt.Assert(t, bindings[0].Scopes(), qt.HasLen, 0)
}

// TestBindRestParameter_DuplicateRestParam verifies that a rest parameter
// with the same name as a required parameter produces an error.
func TestBindRestParameter_DuplicateRestParam(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())

	// Try (lambda (x . x) x) - rest param 'x' duplicates required param 'x'
	err := compileAndRunExpectError(t, env, "(lambda (x . x) x)")
	c.Assert(err, qt.IsNotNil)
}

// TestCompileValidatedCaseLambda_ZeroArgClause verifies case-lambda with a
// zero-argument clause (nil params).
func TestCompileValidatedCaseLambda_ZeroArgClause(t *testing.T) {
	env := newDynamicWindEnv()

	// Register + primitive for the test
	addSym := env.InternSymbol(values.NewSymbol("+"))
	env.MaybeCreateOwnGlobalBinding(addSym, environment.BindingTypeVariable)
	addFn := func(_ context.Context, mc *MachineContext) error {
		a := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value().(*values.Integer).Value
		b := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value().(*values.Integer).Value
		mc.SetValue(values.NewInteger(a + b))
		return nil
	}
	addClosure := NewForeignClosure(env, 2, false, addFn)
	env.SetOwnGlobalValue(environment.NewGlobalIndex(addSym), addClosure) //nolint:errcheck

	// Define a case-lambda with zero-arg and one-arg clauses
	compileAndRun(t, env, `(define f (case-lambda (() 0) ((x) (+ x 1))))`)

	// Call with zero args
	mc := compileAndRun(t, env, "(f)")
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(0))

	// Call with one arg
	mc = compileAndRun(t, env, "(f 10)")
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(11))
}

// TestCompileValidatedCaseLambda_VariadicClause verifies case-lambda with
// a variadic clause using rest parameter.
func TestCompileValidatedCaseLambda_VariadicClause(t *testing.T) {
	env := newDynamicWindEnv()

	// Define a case-lambda with fixed and variadic clauses
	// The variadic clause returns the first rest argument
	compileAndRun(t, env, `(define g (case-lambda ((x) x) ((x y . rest) y)))`)

	// Call with one arg (matches first clause)
	mc := compileAndRun(t, env, "(g 42)")
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))

	// Call with two args (matches second clause, returns y)
	mc = compileAndRun(t, env, "(g 1 2)")
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(2))

	// Call with three args (matches second clause, returns y)
	mc = compileAndRun(t, env, "(g 1 2 3)")
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(2))
}
