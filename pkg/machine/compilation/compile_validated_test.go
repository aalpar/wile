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

package compilation

import (
	"context"
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// newDynamicWindEnv creates a test environment with dynamic-wind binding
// and helper primitives for tracking call order via foreign closures.
func newDynamicWindEnv() *environment.EnvironmentFrame {
	env := newNamespace(environment.NewNamespace().Runtime())

	// Register dynamic-wind as a primitive binding so the expander/validator recognize it
	dwSym := values.NewSymbol("dynamic-wind")
	env.MaybeCreateOwnGlobalBinding(dwSym, environment.BindingTypePrimitive)

	// Register case-lambda as a primitive binding
	clSym := values.NewSymbol("case-lambda")
	env.MaybeCreateOwnGlobalBinding(clSym, environment.BindingTypePrimitive)

	return env
}

// registerForeignFn registers a zero-argument foreign closure in the environment.
func registerForeignFn(env *environment.EnvironmentFrame, name string, fn machine.ForeignFunction) {
	sym := values.NewSymbol(name)
	env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
	closure := machine.NewForeignClosure(env, 0, false, fn)
	env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure) //nolint:errcheck
}

// compileAndRun parses, expands, compiles, and runs a Scheme expression.
func compileAndRun(t *testing.T, env *environment.EnvironmentFrame, code string) *machine.MachineContext {
	t.Helper()
	sv := parseSchemeExpr(t, env, code)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
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
	mc := machine.NewMachineContext(context.Background(), cont)
	return mc.Run()
}

// TestCompileValidatedDynamicWind_ReturnValue verifies that dynamic-wind returns
// the thunk's value.
func TestCompileValidatedDynamicWind_ReturnValue(t *testing.T) {
	env := newDynamicWindEnv()

	// Register no-op thunks for before/after
	registerForeignFn(env, "noop", func(mc machine.CallContext) error {
		mc.SetValue(values.Void)
		return nil
	})

	// Register a thunk that returns 42
	registerForeignFn(env, "ret42", func(mc machine.CallContext) error {
		mc.SetValue(values.NewInteger(42))
		return nil
	})

	mc := compileAndRun(t, env, "(dynamic-wind noop ret42 noop)")
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

// TestCompileValidatedDynamicWind_CallOrder verifies that before, thunk, and after
// are called in the correct order.
func TestCompileValidatedDynamicWind_CallOrder(t *testing.T) {
	env := newDynamicWindEnv()

	var log []string

	registerForeignFn(env, "before-fn", func(mc machine.CallContext) error {
		log = append(log, "before")
		mc.SetValue(values.Void)
		return nil
	})

	registerForeignFn(env, "thunk-fn", func(mc machine.CallContext) error {
		log = append(log, "thunk")
		mc.SetValue(values.NewString("result"))
		return nil
	})

	registerForeignFn(env, "after-fn", func(mc machine.CallContext) error {
		log = append(log, "after")
		mc.SetValue(values.Void)
		return nil
	})

	mc := compileAndRun(t, env, "(dynamic-wind before-fn thunk-fn after-fn)")

	qt.Assert(t, log, qt.DeepEquals, []string{"before", "thunk", "after"})
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewString("result"))
}

// TestCompileValidatedDynamicWind_WithLambdas verifies dynamic-wind works with
// inline lambda expressions rather than named thunks.
func TestCompileValidatedDynamicWind_WithLambdas(t *testing.T) {
	env := newDynamicWindEnv()

	var log []string

	registerForeignFn(env, "log-before", func(mc machine.CallContext) error {
		log = append(log, "before")
		mc.SetValue(values.Void)
		return nil
	})

	registerForeignFn(env, "log-after", func(mc machine.CallContext) error {
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
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(99))
}

// TestCompileValidatedDynamicWind_Nested verifies correct ordering when
// dynamic-wind forms are nested.
func TestCompileValidatedDynamicWind_Nested(t *testing.T) {
	env := newDynamicWindEnv()

	var log []string

	makeFn := func(label string) machine.ForeignFunction {
		return func(mc machine.CallContext) error {
			log = append(log, label)
			mc.SetValue(values.Void)
			return nil
		}
	}

	registerForeignFn(env, "outer-before", makeFn("outer-before"))
	registerForeignFn(env, "outer-after", makeFn("outer-after"))
	registerForeignFn(env, "inner-before", makeFn("inner-before"))
	registerForeignFn(env, "inner-after", makeFn("inner-after"))
	registerForeignFn(env, "body", func(mc machine.CallContext) error {
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
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(7))
}

// newContMarkEnv creates a test environment with with-continuation-mark binding
// and a + primitive for arithmetic tests.
func newContMarkEnv() *environment.EnvironmentFrame {
	env := newNamespace(environment.NewNamespace().Runtime())

	wcmSym := values.NewSymbol("with-continuation-mark")
	env.MaybeCreateOwnGlobalBinding(wcmSym, environment.BindingTypePrimitive)

	// Register + primitive for body-is-call and nested tests
	addSym := values.NewSymbol("+")
	env.MaybeCreateOwnGlobalBinding(addSym, environment.BindingTypeVariable)
	addFn := func(mc machine.CallContext) error {
		a := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value().(*values.Integer).Value
		b := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value().(*values.Integer).Value
		mc.SetValue(values.NewInteger(a + b))
		return nil
	}
	addClosure := machine.NewForeignClosure(env, 2, false, addFn)
	env.SetOwnGlobalValue(environment.NewGlobalIndex(addSym), addClosure) //nolint:errcheck

	return env
}

// TestCompileWithContinuationMark_TailPosition verifies that with-continuation-mark
// in tail position returns the body's value.
func TestCompileWithContinuationMark_TailPosition(t *testing.T) {
	env := newContMarkEnv()

	// (with-continuation-mark 'k 1 'result) → result
	mc := compileAndRun(t, env, "(with-continuation-mark 'k 1 'result)")
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewSymbol("result"))
}

// TestCompileWithContinuationMark_NonTailPosition verifies that with-continuation-mark
// in non-tail position returns the body's value when followed by another expression.
func TestCompileWithContinuationMark_NonTailPosition(t *testing.T) {
	env := newContMarkEnv()

	// In (begin X Y), X is not in tail position. The with-continuation-mark
	// is X, so it compiles with SaveContMark/RestoreContMark. The begin returns
	// the value of Y ('after), but the wcm body must still evaluate correctly.
	// We verify the overall begin returns 'after (the last expression).
	mc := compileAndRun(t, env, "(begin (with-continuation-mark 'k 1 42) 'after)")
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewSymbol("after"))
}

// TestCompileWithContinuationMark_BodyIsCall verifies that with-continuation-mark
// works when the body is a procedure call.
func TestCompileWithContinuationMark_BodyIsCall(t *testing.T) {
	env := newContMarkEnv()

	// (with-continuation-mark 'k 1 (+ 2 3)) → 5
	mc := compileAndRun(t, env, "(with-continuation-mark 'k 1 (+ 2 3))")
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(5))
}

// TestCompileWithContinuationMark_Nested verifies nested with-continuation-mark
// forms compile and run correctly in both tail and non-tail positions.
func TestCompileWithContinuationMark_Nested(t *testing.T) {
	env := newContMarkEnv()

	// Nested in tail position: inner wcm inherits tail from outer wcm
	// (with-continuation-mark 'a 1 (with-continuation-mark 'b 2 (+ 10 20))) → 30
	mc := compileAndRun(t, env, "(with-continuation-mark 'a 1 (with-continuation-mark 'b 2 (+ 10 20)))")
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(30))

	// Nested in non-tail position: both marks saved/restored,
	// begin discards the wcm result and returns the final expression.
	mc = compileAndRun(t, env,
		"(begin (with-continuation-mark 'a 1 (with-continuation-mark 'b 2 (+ 10 20))) 'done)")
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewSymbol("done"))
}

// TestCompileValidated_UnknownExprType verifies the default branch in
// compileValidated returns an error for unknown ValidatedExpr types.
func TestCompileValidated_UnknownExprType(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := NewCompileTimeCallContext(context.Background(), false)

	// Create a mock ValidatedExpr that compileValidated doesn't know about
	mock := &mockValidatedExpr{}
	err := ctc.compileValidated(ctctx, mock)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err, qt.ErrorMatches, `unknown validated expression type: \*compilation\.mockValidatedExpr: invalid argument`)
	c.Assert(errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue)
}

// A registered form with no compiler (an expand-only macro such as let-syntax)
// that reaches codegen as a ValidatedLiteral is an expander bug: compileValidated
// must diagnose it with ErrInvalidSyntax ("should be handled during expansion"),
// NOT silently self-evaluate the raw form into a wrong value. Reproduce the bug
// state by validating a literal and stamping it with a registered-but-no-compiler
// FormName. Under the pre-fix ValidatedLiteral branch this self-evaluated with no
// error; the diagnostic below guards the restored behavior-identity.
func TestCompileValidated_RegisteredFormWithoutCompiler_Diagnoses(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	prog := parseSchemeExpr(t, env, "5")
	result := validate.ValidateExpression(context.Background(), env, prog)
	c.Assert(result.Ok(), qt.IsTrue)
	lit, ok := result.Expr.(*validate.ValidatedLiteral)
	c.Assert(ok, qt.IsTrue)

	// let-syntax is registered as a passthrough (expand-only) validator but
	// carries no codegen compiler; stamping it reproduces the expander-bug state.
	lit.SetFormName("let-syntax")

	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := NewCompileTimeCallContext(context.Background(), false)
	err := ctc.compileValidated(ctctx, lit)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrInvalidSyntax), qt.IsTrue,
		qt.Commentf("expected ErrInvalidSyntax for registered-but-no-compiler form, got %v", err))
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

// TestBindRestParameter_DuplicateRestParam verifies that a rest parameter
// with the same name as a required parameter produces an error.
func TestBindRestParameter_DuplicateRestParam(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())

	// Try (lambda (x . x) x) - rest param 'x' duplicates required param 'x'
	err := compileAndRunExpectError(t, env, "(lambda (x . x) x)")
	c.Assert(err, qt.IsNotNil)
}

// TestCompileValidatedCaseLambda_ZeroArgClause verifies case-lambda with a
// zero-argument clause (nil params).
func TestCompileValidatedCaseLambda_ZeroArgClause(t *testing.T) {
	env := newDynamicWindEnv()

	// Register + primitive for the test
	addSym := values.NewSymbol("+")
	env.MaybeCreateOwnGlobalBinding(addSym, environment.BindingTypeVariable)
	addFn := func(mc machine.CallContext) error {
		a := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value().(*values.Integer).Value
		b := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value().(*values.Integer).Value
		mc.SetValue(values.NewInteger(a + b))
		return nil
	}
	addClosure := machine.NewForeignClosure(env, 2, false, addFn)
	env.SetOwnGlobalValue(environment.NewGlobalIndex(addSym), addClosure) //nolint:errcheck

	// Define a case-lambda with zero-arg and one-arg clauses
	compileAndRun(t, env, `(define f (case-lambda (() 0) ((x) (+ x 1))))`)

	// Call with zero args
	mc := compileAndRun(t, env, "(f)")
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(0))

	// Call with one arg
	mc = compileAndRun(t, env, "(f 10)")
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(11))
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
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))

	// Call with two args (matches second clause, returns y)
	mc = compileAndRun(t, env, "(g 1 2)")
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(2))

	// Call with three args (matches second clause, returns y)
	mc = compileAndRun(t, env, "(g 1 2 3)")
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(2))
}

// TestMarkLiteralImmutable_RecursiveAndCyclic verifies the quote-hook marker
// records every reachable pair/vector by pointer identity, descends into nested
// structure and vectors, and terminates on cyclic literals.
func TestMarkLiteralImmutable_RecursiveAndCyclic(t *testing.T) {
	set := &environment.ImmutableLiterals{}

	// (1 #(2 3)) — a top pair whose cadr is a vector.
	vec := values.NewVector(values.NewInteger(2), values.NewInteger(3))
	inner := values.NewCons(vec, values.EmptyList)
	top := values.NewCons(values.NewInteger(1), inner)

	markLiteralImmutable(top, set, make(map[values.Value]struct{}))

	for _, v := range []values.Value{top, inner, vec} {
		if !set.Contains(v) {
			t.Fatalf("expected %T to be marked immutable", v)
		}
	}
	// Atoms are not marked (membership is for pairs/vectors only).
	if set.Contains(values.NewInteger(1)) {
		t.Fatalf("atoms must not be marked")
	}

	// Cyclic literal: #0=(1 . #0#). Must terminate.
	cyc := values.NewCons(values.NewInteger(1), values.EmptyList)
	cyc.SetCdr(cyc)
	cycSet := &environment.ImmutableLiterals{}
	markLiteralImmutable(cyc, cycSet, make(map[values.Value]struct{}))
	if !cycSet.Contains(cyc) {
		t.Fatalf("cyclic pair must be marked")
	}

	// nil set is a no-op (no panic).
	markLiteralImmutable(top, nil, make(map[values.Value]struct{}))
}
