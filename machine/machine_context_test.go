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
	"errors"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestNewMachineContext(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(3, 0, false)

	// Create a parent continuation to verify parent chain works
	parentCont := NewMachineContinuation(nil, nil, env)

	// Create a continuation with specific state
	cont := &MachineContinuation{
		vmState: vmState{
			env:         env,
			template:    tpl,
			singleValue: values.NewInteger(42),
			evals:       NewStack(),
			pc:          5,
		},
		parent: parentCont,
	}
	cont.evals.Push(values.NewInteger(1))
	cont.evals.Push(values.NewInteger(2))

	// Create MachineContext from continuation
	mc := NewMachineContext(context.Background(), cont)

	// Verify all fields are correctly transferred
	qt.Assert(t, mc.env, qt.Equals, env)
	qt.Assert(t, mc.template, qt.Equals, tpl)
	qt.Assert(t, mc.cont, qt.Equals, parentCont) // cont field should be cont.parent
	qt.Assert(t, mc.pc, qt.Equals, 5)
	qt.Assert(t, mc.GetValues().Len(), qt.Equals, 1)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
	qt.Assert(t, mc.evals.Len(), qt.Equals, 2)
}

func TestNewMachineContext_NilParent(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()

	// Create a continuation with nil parent
	cont := NewMachineContinuation(nil, nil, env)

	mc := NewMachineContext(context.Background(), cont)

	qt.Assert(t, mc.env, qt.Equals, env)
	qt.Assert(t, mc.cont, qt.IsNil) // nil parent means mc.cont should be nil
	qt.Assert(t, mc.pc, qt.Equals, 0)
}

func TestNewMachineContext_RoundTrip(t *testing.T) {
	// Test that saving and restoring a continuation preserves state
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(2, 0, false)

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	// Set some state on the context
	mc.SetValue(values.NewInteger(100))
	mc.evals.Push(values.NewInteger(10))
	mc.evals.Push(values.NewInteger(20))

	// Save continuation with offset
	mc.SaveContinuation(7)

	// The saved continuation should have the previous state
	savedCont := mc.cont
	qt.Assert(t, savedCont.pc, qt.Equals, 7)
	qt.Assert(t, savedCont.env, qt.Equals, env)
	qt.Assert(t, savedCont.template, qt.Equals, tpl)

	// Create a new context from the saved continuation
	mc2 := NewMachineContext(context.Background(), savedCont)

	// Verify the round-trip preserved state
	qt.Assert(t, mc2.env, qt.Equals, env)
	qt.Assert(t, mc2.template, qt.Equals, tpl)
	qt.Assert(t, mc2.pc, qt.Equals, 7)
}

func TestMachineContext_PushContinuation_0(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	cont := NewMachineContinuation(nil, nil, env)
	mc := NewMachineContext(context.Background(), cont)
	qt.Assert(t, mc.cont, qt.IsNil)
	qt.Assert(t, mc.cont.CallDepth(), qt.Equals, 0)
	qt.Assert(t, mc.cont, qt.IsNil)
	qt.Assert(t, mc.PC(), qt.Equals, 0)
	qt.Assert(t, mc.EnvironmentFrame(), valuestest.SchemeEquals, mc.env)
	qt.Assert(t, mc.Template(), qt.IsNil)
	qt.Assert(t, mc.GetValues().Len(), qt.Equals, 0)
	qt.Assert(t, mc.evals.Len(), qt.Equals, 0)
}

func TestMachineContext_PushContinuation_1(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	mc.SaveContinuation(10)
	qt.Assert(t, mc.CallDepth(), qt.Equals, 1)
	qt.Assert(t, mc.Parent(), qt.IsNotNil)
	qt.Assert(t, mc.Parent().PC(), qt.Equals, 10)
	qt.Assert(t, mc.PC(), qt.Equals, 0)
	qt.Assert(t, mc.EnvironmentFrame(), valuestest.SchemeEquals, mc.env)
	qt.Assert(t, mc.Template(), qt.IsNil)
	qt.Assert(t, mc.GetValues().Len(), qt.Equals, 0)
	qt.Assert(t, mc.evals.Len(), qt.Equals, 0)
}

func TestMachineContext_PushContinuation_2(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	bottom0 := mc.cont
	mc.SaveContinuation(10)
	bottom1 := mc.cont
	mc.SaveContinuation(20)
	bottom2 := mc.cont
	qt.Assert(t, mc.cont, qt.IsNotNil)
	qt.Assert(t, mc.CallDepth(), qt.Equals, 2)
	qt.Assert(t, mc.Parent(), valuestest.SchemeEquals, bottom2)
	qt.Assert(t, mc.PC(), qt.Equals, 0)
	mc.PopContinuation()
	qt.Assert(t, mc.cont, qt.Equals, bottom1)
	qt.Assert(t, mc.Parent(), qt.Equals, bottom1)
	qt.Assert(t, mc.CallDepth(), qt.Equals, 1)
	qt.Assert(t, mc.PC(), qt.Equals, 20)
	mc.PopContinuation()
	qt.Assert(t, mc.cont, qt.Equals, bottom0)
	qt.Assert(t, mc.Parent(), qt.IsNil)
	qt.Assert(t, mc.CallDepth(), qt.Equals, 0)
	qt.Assert(t, mc.PC(), qt.Equals, 10)
}

func TestMachineContext_SetValues_GetValues(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	// Test SetValues and GetValues
	mc.SetValues(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	vs := mc.GetValues()
	qt.Assert(t, vs.Len(), qt.Equals, 3)
	qt.Assert(t, vs[0], valuestest.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, vs[1], valuestest.SchemeEquals, values.NewInteger(2))
	qt.Assert(t, vs[2], valuestest.SchemeEquals, values.NewInteger(3))

	// GetValue returns first value
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(1))

	// Test empty values
	mc.SetValues()
	qt.Assert(t, mc.GetValues().Len(), qt.Equals, 0)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.Void)
}

func TestMachineContext_CurrentContinuation(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(2, 0, false)

	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))
	mc.SaveContinuation(5)
	mc.evals.Push(values.NewInteger(42))
	mc.SetValue(values.NewInteger(100))

	curr := mc.CurrentContinuation()

	// Returns the same pointer (no DeepCopy), but chain is marked shared.
	qt.Assert(t, curr, qt.IsNotNil)
	qt.Assert(t, curr == mc.cont, qt.IsTrue)
	qt.Assert(t, curr.shared, qt.IsTrue)
}

func TestMachineContext_NewSubContext(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(2, 0, false)

	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))
	mc.SetValue(values.NewInteger(42))
	mc.evals.Push(values.NewInteger(100))
	mc.SaveContinuation(5)

	subCtx := mc.NewSubContext()

	// Sub-context should have fresh state
	qt.Assert(t, subCtx.template, qt.IsNil)
	qt.Assert(t, subCtx.pc, qt.Equals, 0)
	qt.Assert(t, subCtx.GetValues(), qt.IsNil)
	qt.Assert(t, subCtx.evals.Len(), qt.Equals, 0)
	qt.Assert(t, subCtx.cont, qt.IsNil)
	// But shares top-level environment
	qt.Assert(t, subCtx.env, qt.Equals, env.TopLevel())
}

func TestMachineContext_Restore(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	env1 := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	env2 := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	tpl1 := NewNativeTemplate(1, 0, false)
	tpl2 := NewNativeTemplate(2, 0, false)

	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl1, env1))
	mc.pc = 5
	mc.evals.Push(values.NewInteger(1))

	// Create a continuation with different state
	parent := NewMachineContinuation(nil, nil, env2)
	cont2 := &MachineContinuation{
		vmState: vmState{
			env:      env2,
			template: tpl2,
			evals:    NewStack(),
			pc:       10,
		},
		parent: parent,
	}
	cont2.evals.Push(values.NewInteger(42))

	mc.Restore(cont2)

	qt.Assert(t, mc.env, qt.Equals, env2)
	qt.Assert(t, mc.template, qt.Equals, tpl2)
	qt.Assert(t, mc.pc, qt.Equals, 10)
	qt.Assert(t, mc.cont, qt.Equals, parent)
	qt.Assert(t, mc.evals.Len(), qt.Equals, 1)
}

func TestMachineContext_Apply_FixedArity(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(2, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	// Apply with correct argument count
	_, err := mc.Apply(cls, values.NewInteger(10), values.NewInteger(20))
	qt.Assert(t, err, qt.IsNil)

	// Check bindings were set in the NEW call environment (not the closure's env)
	// Apply now creates a fresh environment for each call to support recursion
	bnds := mc.env.LocalEnvironment().Bindings()
	qt.Assert(t, bnds[0].Value(), valuestest.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, bnds[1].Value(), valuestest.SchemeEquals, values.NewInteger(20))

	// Check context was updated
	qt.Assert(t, mc.template, qt.Equals, tpl)
	// mc.env is now a fresh environment with copied local bindings
	qt.Assert(t, mc.env.Parent(), qt.Equals, env.Parent())
	qt.Assert(t, mc.pc, qt.Equals, 0)
}

func TestMachineContext_Apply_WrongArgCount(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(2, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	// Apply with wrong argument count
	_, err := mc.Apply(cls, values.NewInteger(10))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "expected 2 arguments")
}

func TestMachineContext_Apply_Variadic(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(3)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	// Variadic with 2 required parameters plus rest
	tpl := NewNativeTemplate(3, 0, true)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	// Apply with extra args for rest parameter
	_, err := mc.Apply(cls, values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4))
	qt.Assert(t, err, qt.IsNil)

	// Check bindings in the NEW call environment (not the closure's env)
	bnds := mc.env.LocalEnvironment().Bindings()
	qt.Assert(t, bnds[0].Value(), valuestest.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, bnds[1].Value(), valuestest.SchemeEquals, values.NewInteger(2))
	// Rest parameter should be a list
	rest := bnds[2].Value()
	qt.Assert(t, rest, valuestest.SchemeEquals, values.List(values.NewInteger(3), values.NewInteger(4)))
}

func TestMachineContext_Apply_VariadicTooFewArgs(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(3)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(3, 0, true)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	// Apply with too few args
	_, err := mc.Apply(cls, values.NewInteger(1))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "expected at least 2 arguments")
}

func TestMachineContext_ApplyCaseLambda(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()

	// Create two clauses with different arities
	lenv1 := environment.NewLocalEnvironment(1)
	env1 := environment.NewEnvironmentFrameWithParent(lenv1, topEnv)
	tpl1 := NewNativeTemplate(1, 0, false)
	cls1 := NewClosureWithTemplate(tpl1, env1)

	lenv2 := environment.NewLocalEnvironment(2)
	env2 := environment.NewEnvironmentFrameWithParent(lenv2, topEnv)
	tpl2 := NewNativeTemplate(2, 0, false)
	cls2 := NewClosureWithTemplate(tpl2, env2)

	caseLambda := NewCaseLambdaClosure([]*MachineClosure{cls1, cls2})

	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	// Apply with 1 arg - should use cls1
	_, err := mc.ApplyCaseLambda(caseLambda, values.NewInteger(42))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.template, qt.Equals, tpl1)

	// Reset context
	mc = NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	// Apply with 2 args - should use cls2
	_, err = mc.ApplyCaseLambda(caseLambda, values.NewInteger(1), values.NewInteger(2))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.template, qt.Equals, tpl2)
}

func TestMachineContext_ApplyCaseLambda_NoMatch(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()

	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(2, 0, false)
	cls := NewClosureWithTemplate(tpl, env)

	caseLambda := NewCaseLambdaClosure([]*MachineClosure{cls})

	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	// Apply with wrong number of args
	_, err := mc.ApplyCaseLambda(caseLambda, values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "no matching clause")
}

func TestMachineContext_Apply_FixedArityTooManyArgs(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(2, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	_, err := mc.Apply(cls, values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, values.ErrWrongNumberOfArguments), qt.IsTrue)
	qt.Assert(t, err.Error(), qt.Contains, "expected 2 arguments, got 3")
}

func TestMachineContext_Apply_ZeroArity(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(0, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	// Thunk: zero parameters, zero args
	result, err := mc.Apply(cls)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, qt.Equals, mc)
	qt.Assert(t, mc.template, qt.Equals, tpl)
	qt.Assert(t, mc.pc, qt.Equals, 0)
}

func TestMachineContext_Apply_VariadicExactlyRequiredArgs(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(3)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	// 2 required + rest: (lambda (a b . rest) ...)
	tpl := NewNativeTemplate(3, 0, true)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	// Provide exactly the required args — rest should be empty list
	_, err := mc.Apply(cls, values.NewInteger(10), values.NewInteger(20))
	qt.Assert(t, err, qt.IsNil)

	bnds := mc.env.LocalEnvironment().Bindings()
	qt.Assert(t, bnds[0].Value(), valuestest.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, bnds[1].Value(), valuestest.SchemeEquals, values.NewInteger(20))
	qt.Assert(t, bnds[2].Value(), valuestest.SchemeEquals, values.EmptyList)
}

func TestMachineContext_Apply_VariadicRestOnly(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(1)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	// (lambda args ...) — paramCount=1, variadic, 0 required
	tpl := NewNativeTemplate(1, 0, true)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	// All args go into rest
	_, err := mc.Apply(cls, values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	qt.Assert(t, err, qt.IsNil)

	bnds := mc.env.LocalEnvironment().Bindings()
	qt.Assert(t, bnds[0].Value(), valuestest.SchemeEquals, values.List(
		values.NewInteger(1), values.NewInteger(2), values.NewInteger(3),
	))
}

func TestMachineContext_Apply_VariadicRestOnlyNoArgs(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(1)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	// (lambda args ...) called with zero args
	tpl := NewNativeTemplate(1, 0, true)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	_, err := mc.Apply(cls)
	qt.Assert(t, err, qt.IsNil)

	bnds := mc.env.LocalEnvironment().Bindings()
	qt.Assert(t, bnds[0].Value(), valuestest.SchemeEquals, values.EmptyList)
}

func TestMachineContext_Apply_EnvironmentIsolation(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(1)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(1, 0, false)

	cls := NewClosureWithTemplate(tpl, env)

	// First call
	mc1 := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	_, err := mc1.Apply(cls, values.NewInteger(10))
	qt.Assert(t, err, qt.IsNil)
	env1 := mc1.env

	// Second call on a fresh context — must get an independent environment
	mc2 := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	_, err = mc2.Apply(cls, values.NewInteger(20))
	qt.Assert(t, err, qt.IsNil)
	env2 := mc2.env

	// Environments must be distinct objects
	qt.Assert(t, env1 != env2, qt.IsTrue)
	// Neither should be the original closure env
	qt.Assert(t, env1 != env, qt.IsTrue)
	qt.Assert(t, env2 != env, qt.IsTrue)

	// Modifying one must not affect the other
	bnds1 := env1.LocalEnvironment().Bindings()
	bnds2 := env2.LocalEnvironment().Bindings()
	qt.Assert(t, bnds1[0].Value(), valuestest.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, bnds2[0].Value(), valuestest.SchemeEquals, values.NewInteger(20))
}

func TestMachineContext_Apply_PCResetFromNonZero(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(1)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(1, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	mc.pc = 42 // simulate mid-execution

	_, err := mc.Apply(cls, values.NewInteger(1))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.pc, qt.Equals, 0)
}

func TestMachineContext_Apply_Counters(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(3)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(3, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	before := mc.Counters()
	qt.Assert(t, before.ClosuresApplied, qt.Equals, uint64(0))
	qt.Assert(t, before.EnvsCopied, qt.Equals, uint64(0))
	qt.Assert(t, before.BindingsCopied, qt.Equals, uint64(0))

	_, err := mc.Apply(cls, values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	qt.Assert(t, err, qt.IsNil)

	after := mc.Counters()
	qt.Assert(t, after.ClosuresApplied, qt.Equals, uint64(1))
	qt.Assert(t, after.EnvsCopied, qt.Equals, uint64(1))
	qt.Assert(t, after.BindingsCopied, qt.Equals, uint64(3))
}

func TestMachineContext_Apply_ReturnsSameContext(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(1)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(1, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	result, err := mc.Apply(cls, values.NewInteger(1))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result == mc, qt.IsTrue)
}

func TestMachineContext_Apply_ErrorsWrapSentinel(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()

	tcs := []struct {
		name       string
		paramCount int
		variadic   bool
		args       []values.Value
	}{
		{
			"fixed arity too few",
			2, false,
			[]values.Value{values.NewInteger(1)},
		},
		{
			"fixed arity too many",
			2, false,
			[]values.Value{values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)},
		},
		{
			"variadic too few",
			3, true,
			[]values.Value{values.NewInteger(1)},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			lenv := environment.NewLocalEnvironment(tc.paramCount)
			env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
			tpl := NewNativeTemplate(tc.paramCount, 0, tc.variadic)
			cls := NewClosureWithTemplate(tpl, env)
			mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

			_, err := mc.Apply(cls, tc.args...)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, values.ErrWrongNumberOfArguments), qt.IsTrue)
		})
	}
}

func TestMachineContext_ApplyCaseLambda_VariadicClause(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()

	// Fixed 1-arity clause
	lenv1 := environment.NewLocalEnvironment(1)
	env1 := environment.NewEnvironmentFrameWithParent(lenv1, topEnv)
	tpl1 := NewNativeTemplate(1, 0, false)
	cls1 := NewClosureWithTemplate(tpl1, env1)

	// Variadic clause: (a . rest) — catches 2+ args
	lenv2 := environment.NewLocalEnvironment(2)
	env2 := environment.NewEnvironmentFrameWithParent(lenv2, topEnv)
	tpl2 := NewNativeTemplate(2, 0, true)
	cls2 := NewClosureWithTemplate(tpl2, env2)

	caseLambda := NewCaseLambdaClosure([]*MachineClosure{cls1, cls2})
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)

	// 1 arg → fixed clause
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	_, err := mc.ApplyCaseLambda(caseLambda, values.NewInteger(1))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.template, qt.Equals, tpl1)

	// 3 args → variadic clause, rest = (20 30)
	mc = NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	_, err = mc.ApplyCaseLambda(caseLambda, values.NewInteger(10), values.NewInteger(20), values.NewInteger(30))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.template, qt.Equals, tpl2)
	bnds := mc.env.LocalEnvironment().Bindings()
	qt.Assert(t, bnds[0].Value(), valuestest.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, bnds[1].Value(), valuestest.SchemeEquals, values.List(values.NewInteger(20), values.NewInteger(30)))
}

func TestMachineContext_ApplyCaseLambda_NoMatchErrorSentinel(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()

	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(2, 0, false)
	cls := NewClosureWithTemplate(tpl, env)

	caseLambda := NewCaseLambdaClosure([]*MachineClosure{cls})
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	_, err := mc.ApplyCaseLambda(caseLambda, values.NewInteger(1))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, values.ErrWrongNumberOfArguments), qt.IsTrue)
}

func TestNewMachineContextFromMachineClosure(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(2, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContextFromMachineClosure(context.Background(), cls)

	qt.Assert(t, mc.template, qt.Equals, tpl)
	qt.Assert(t, mc.env, qt.Equals, env)
	qt.Assert(t, mc.pc, qt.Equals, 0)
	qt.Assert(t, mc.cont, qt.IsNil)
}

func TestMachineContext_Error(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	tpl.SetName("test-func")

	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))

	err := mc.Error("test error message")

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Message, qt.Equals, "test error message")
	// Error should be a SchemeError
	_, ok := any(err).(*SchemeError)
	qt.Assert(t, ok, qt.IsTrue)
}

func TestMachineContext_Error_NoSource(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()

	// No template means no source
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	err := mc.Error("no source error")

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Message, qt.Equals, "no source error")
	qt.Assert(t, err.Source, qt.IsNil)
}

func TestMachineContext_WrapError(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)

	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))

	cause := values.NewForeignErrorf("original error")
	err := mc.WrapError(cause, "wrapped message")

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Message, qt.Equals, "wrapped message")
	qt.Assert(t, err.Cause, qt.Equals, cause)
}

func TestMachineContext_WrapError_EmptyMessage(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)

	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))

	cause := values.NewForeignErrorf("original error")
	err := mc.WrapError(cause, "")

	qt.Assert(t, err, qt.IsNotNil)
	// Empty message should use cause's message
	qt.Assert(t, err.Message, qt.Equals, "original error")
	qt.Assert(t, err.Cause, qt.Equals, cause)
}

// Tests moved from coverage_additional_test.go
// TestExecuteSimpleProcedureCall tests actually running a procedure call
func TestExecuteSimpleProcedureCall(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())

	prog := "((lambda (x) x) 42)"
	sv := parseSchemeExpr(t, env, prog)

	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)

	mc := NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValues(), qt.IsNotNil)
	qt.Assert(t, mc.GetValues().Len() > 0, qt.IsTrue)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

// TestExecuteVariadicProcedure tests running a variadic procedure
func TestExecuteVariadicProcedure(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())

	// (lambda args args) called with (1 2 3) should return (1 2 3)
	prog := "((lambda args args) 1 2 3)"
	sv := parseSchemeExpr(t, env, prog)

	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)

	mc := NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValues(), qt.IsNotNil)
}

// TestMachineContextNewSubContext tests creating a sub-context
func TestMachineContextNewSubContext(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `42`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(context.Background(), cont)

	// Create a sub-context
	sub := mc.NewSubContext()
	qt.Assert(t, sub, qt.IsNotNil)
}

// TestMachineContextSetValues tests SetValues and GetValues
func TestMachineContextSetValues(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(
		NewOperationLoadVoid(),
		NewOperationRestoreContinuation())

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	// SetValues
	mc.SetValues(values.NewInteger(1), values.NewInteger(2))
	vs := mc.GetValues()
	qt.Assert(t, len(vs), qt.Equals, 2)
	qt.Assert(t, vs[0], valuestest.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, vs[1], valuestest.SchemeEquals, values.NewInteger(2))
}

// TestMachineContextSetValue tests SetValue and GetValue
func TestMachineContextSetValue(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(
		NewOperationLoadVoid(),
		NewOperationRestoreContinuation())

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	// SetValue
	mc.SetValue(values.NewInteger(42))
	v := mc.GetValue()
	qt.Assert(t, v, valuestest.SchemeEquals, values.NewInteger(42))
}

// TestMachineContextApplySimple tests mc.Apply with a simple closure
func TestMachineContextApplySimple(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Create a lambda and apply it
	sv := parseSchemeExpr(t, env, `((lambda (x) x) 100)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(100))
}

// TestMachineContextValueMethods tests MachineContext value get/set
func TestMachineContextValueMethods(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(NewOperationRestoreContinuation())
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	// Test SetValue and GetValue
	mc.SetValue(values.NewInteger(42))
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))

	// Test SetValues and GetValues
	mc.SetValues(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	vals := mc.GetValues()
	qt.Assert(t, len(vals), qt.Equals, 3)
}

// TestMachineContextNewSubContextAdditional tests additional sub-context paths
func TestMachineContextNewSubContextAdditional(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(NewOperationRestoreContinuation())
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	sub := mc.NewSubContext()
	qt.Assert(t, sub, qt.IsNotNil)

	// Sub context should have its own environment
	qt.Assert(t, sub.EnvironmentFrame(), qt.IsNotNil)
}

func TestApplyCallable_MachineClosure(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(2, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	result, err := mc.ApplyCallable(cls, values.NewInteger(10), values.NewInteger(20))
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, mc)
	c.Assert(mc.template, qt.Equals, tpl)
	c.Assert(mc.pc, qt.Equals, 0)

	bnds := mc.env.LocalEnvironment().Bindings()
	c.Assert(bnds[0].Value(), valuestest.SchemeEquals, values.NewInteger(10))
	c.Assert(bnds[1].Value(), valuestest.SchemeEquals, values.NewInteger(20))
}

func TestApplyCallable_CaseLambdaClosure(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewTopLevelEnvironment().Runtime()

	lenv1 := environment.NewLocalEnvironment(1)
	env1 := environment.NewEnvironmentFrameWithParent(lenv1, topEnv)
	tpl1 := NewNativeTemplate(1, 0, false)
	cls1 := NewClosureWithTemplate(tpl1, env1)

	lenv2 := environment.NewLocalEnvironment(2)
	env2 := environment.NewEnvironmentFrameWithParent(lenv2, topEnv)
	tpl2 := NewNativeTemplate(2, 0, false)
	cls2 := NewClosureWithTemplate(tpl2, env2)

	caseLambda := NewCaseLambdaClosure([]*MachineClosure{cls1, cls2})
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)

	// 1 arg → first clause
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	_, err := mc.ApplyCallable(caseLambda, values.NewInteger(42))
	c.Assert(err, qt.IsNil)
	c.Assert(mc.template, qt.Equals, tpl1)

	// 2 args → second clause
	mc = NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	_, err = mc.ApplyCallable(caseLambda, values.NewInteger(1), values.NewInteger(2))
	c.Assert(err, qt.IsNil)
	c.Assert(mc.template, qt.Equals, tpl2)
}

func TestApplyCallable_Parameter(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)

	tcs := []struct {
		name      string
		initVal   values.Value
		args      []values.Value
		wantValue values.Value
		wantParam values.Value // expected parameter value after call
	}{
		{
			"get value with 0 args",
			values.NewInteger(42),
			nil,
			values.NewInteger(42),
			values.NewInteger(42),
		},
		{
			"set value with 1 arg",
			values.NewInteger(0),
			[]values.Value{values.NewInteger(99)},
			values.Void,
			values.NewInteger(99),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			param := NewParameter(tc.initVal, nil)
			mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

			// Sub-context: cont == nil
			sub := mc.NewSubContext()
			_, err := sub.ApplyCallable(param, tc.args...)
			c.Assert(err, qt.IsNil)

			// Run should return nil (immediateReturnTemplate)
			err = sub.Run()
			c.Assert(err, qt.IsNil)

			c.Assert(sub.GetValue(), valuestest.SchemeEquals, tc.wantValue)
			c.Assert(param.Value(), valuestest.SchemeEquals, tc.wantParam)
		})
	}
}

func TestApplyCallable_Parameter_WrongArgCount(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)

	param := NewParameter(values.NewInteger(0), nil)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	sub := mc.NewSubContext()
	_, err := sub.ApplyCallable(param, values.NewInteger(1), values.NewInteger(2))
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "expected 0 or 1 arguments")
}

func TestApplyCallable_Parameter_WithContinuation(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(NewOperationRestoreContinuation())

	param := NewParameter(values.NewInteger(42), nil)

	// With a continuation (bytecode path): should restore continuation
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	mc.SaveContinuation(0)
	mc.template = tpl
	mc.pc = 0

	_, err := mc.ApplyCallable(param)
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

func TestApplyCallable_ComposableContinuation(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(NewOperationRestoreContinuation())

	// Create a simple continuation to compose
	cont := NewMachineContinuation(nil, tpl, env)
	cc := NewComposableContinuation(cont, nil, 0, nil)

	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	_, err := mc.ApplyCallable(cc, values.NewInteger(7))
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(7))
}

func TestApplyCallable_ComposableContinuation_WrongArgCount(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)

	cc := NewComposableContinuation(nil, nil, 0, nil)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	_, err := mc.ApplyCallable(cc, values.NewInteger(1), values.NewInteger(2))
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "expected 1 argument")
}

func TestApplyCallable_NonCallable(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)

	tcs := []struct {
		name  string
		value values.Value
	}{
		{"integer", values.NewInteger(42)},
		{"string", values.NewString("hello")},
		{"boolean", values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
			_, err := mc.ApplyCallable(tc.value)
			c.Assert(err, qt.IsNotNil)
			c.Assert(err.Error(), qt.Contains, "expected a procedure")
			c.Assert(errors.Is(err, values.ErrNotAProcedure), qt.IsTrue)
		})
	}
}

func TestApplyCallable_Nil(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	_, err := mc.ApplyCallable(nil)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "cannot apply nil")
	c.Assert(errors.Is(err, values.ErrNotAProcedure), qt.IsTrue)
}

// TestNewSubContext_InheritsExceptionHandler verifies that NewSubContext
// automatically inherits the parent's exception handler chain (M3 fix).
func TestNewSubContext_InheritsExceptionHandler(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	parent := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	handler := values.NewInteger(42)
	parent.PushExceptionHandler(handler)

	sub := parent.NewSubContext()

	c.Assert(sub.ExceptionHandler(), qt.Not(qt.IsNil))
	c.Assert(sub.ExceptionHandler().Handler().EqualTo(handler), qt.IsTrue)
}

// TestNewSubContext_InheritsNestedHandlers verifies that nested exception
// handlers form a chain that is correctly inherited by sub-contexts.
func TestNewSubContext_InheritsNestedHandlers(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	parent := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	handler1 := values.NewSymbol("outer")
	handler2 := values.NewSymbol("inner")

	parent.PushExceptionHandler(handler1)
	parent.PushExceptionHandler(handler2)

	sub := parent.NewSubContext()

	c.Assert(sub.ExceptionHandler(), qt.Not(qt.IsNil))
	c.Assert(sub.ExceptionHandler().Handler().EqualTo(handler2), qt.IsTrue)
	c.Assert(sub.ExceptionHandler().Parent(), qt.Not(qt.IsNil))
	c.Assert(sub.ExceptionHandler().Parent().Handler().EqualTo(handler1), qt.IsTrue)
}

// TestNewSubContext_NoExceptionHandler verifies that sub-contexts work
// correctly when the parent has no exception handler installed.
func TestNewSubContext_NoExceptionHandler(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	parent := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	sub := parent.NewSubContext()

	c.Assert(sub.ExceptionHandler(), qt.IsNil)
}

// TestNewThreadSubContext_InheritsExceptionHandler verifies that thread
// sub-contexts correctly inherit exception handlers via SubContextParams.
func TestNewThreadSubContext_InheritsExceptionHandler(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	parent := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	handler := values.NewSymbol("thread-handler")
	parent.PushExceptionHandler(handler)

	params := parent.CaptureSubContextParams()
	thunk := values.NewSymbol("thunk-placeholder")
	thread := values.NewThread(thunk, "test-thread")
	sub := NewThreadSubContext(params, thread)

	c.Assert(sub.ExceptionHandler(), qt.Not(qt.IsNil))
	c.Assert(sub.ExceptionHandler().Handler().EqualTo(handler), qt.IsTrue)
}

func TestSaveContinuation_CallDepthTracking(t *testing.T) {
	tests := []struct {
		name         string
		maxCallDepth uint64
		saveCalls    int
		wantErr      bool
	}{
		{
			name:         "increments on save",
			maxCallDepth: 10,
			saveCalls:    5,
			wantErr:      false,
		},
		{
			name:         "exceeds limit",
			maxCallDepth: 3,
			saveCalls:    4,
			wantErr:      true,
		},
		{
			name:         "exactly at limit",
			maxCallDepth: 3,
			saveCalls:    3,
			wantErr:      false,
		},
		{
			name:         "unlimited when zero",
			maxCallDepth: 0,
			saveCalls:    100,
			wantErr:      false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Build a template with enough operations for all save offsets
			ops := make([]Operation, 200)
			for i := range ops {
				ops[i] = NewOperationLoadVoid()
			}
			tpl := NewNativeTemplate(0, 0, false, ops...)
			topEnv := environment.NewTopLevelEnvironment().Runtime()
			env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
			cont := NewMachineContinuation(nil, tpl, env)
			mc := NewMachineContext(context.Background(), cont)
			mc.SetMaxCallDepth(tt.maxCallDepth)

			var lastErr error
			for i := 0; i < tt.saveCalls; i++ {
				lastErr = mc.SaveContinuation(1)
				if lastErr != nil {
					break
				}
			}

			if tt.wantErr {
				if lastErr == nil {
					t.Fatal("expected error, got nil")
				}
				if !errors.Is(lastErr, values.ErrCallDepthExceeded) {
					t.Fatalf("expected ErrCallDepthExceeded, got: %v", lastErr)
				}
			} else if lastErr != nil {
				t.Fatalf("unexpected error: %v", lastErr)
			}
		})
	}
}

func TestPopContinuation_DecrementsCallDepth(t *testing.T) {
	ops := make([]Operation, 20)
	for i := range ops {
		ops[i] = NewOperationLoadVoid()
	}
	tpl := NewNativeTemplate(0, 0, false, ops...)
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)
	mc.SetMaxCallDepth(10)

	// Save 5 continuations
	for i := range 5 {
		err := mc.SaveContinuation(1)
		if err != nil {
			t.Fatalf("save %d: %v", i, err)
		}
	}

	// Pop 3 of them
	for range 3 {
		mc.PopContinuation()
	}

	// Should be able to save 8 more (was at depth 2 after pops, limit 10)
	for i := range 8 {
		err := mc.SaveContinuation(1)
		if err != nil {
			t.Fatalf("second save %d: %v", i, err)
		}
	}

	// Now at depth 10, one more should fail
	err := mc.SaveContinuation(1)
	if err == nil {
		t.Fatal("expected error at depth limit, got nil")
	}
	if !errors.Is(err, values.ErrCallDepthExceeded) {
		t.Fatalf("expected ErrCallDepthExceeded, got: %v", err)
	}
}

func TestNewSubContext_InheritsMaxCallDepth(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	mc.SetMaxCallDepth(42)

	sub := mc.NewSubContext()
	if sub.MaxCallDepth() != 42 {
		t.Fatalf("sub-context maxCallDepth = %d, want 42", sub.MaxCallDepth())
	}
}

func TestNewThreadSubContext_InheritsMaxCallDepth(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	mc.SetMaxCallDepth(99)

	params := mc.CaptureSubContextParams()
	thunk := values.NewSymbol("thunk-placeholder")
	thread := values.NewThread(thunk, "test-thread")
	sub := NewThreadSubContext(params, thread)
	if sub.MaxCallDepth() != 99 {
		t.Fatalf("thread sub-context maxCallDepth = %d, want 99", sub.MaxCallDepth())
	}
}

// --- Dispatch tests (Phase 6) ---

func TestRunDispatch_InitialOperations(t *testing.T) {
	c := qt.New(t)
	// Template created with initial operations converts them to bytecode.
	tpl := NewNativeTemplate(0, 0, false, NewOperationLoadVoid())
	env := environment.NewTopLevelEnvironment().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), qt.Equals, values.Void)
}

func TestRunDispatch_IntegerPathOpComplex(t *testing.T) {
	c := qt.New(t)
	// Template with code + sideTable uses runIntegerDispatch.
	tpl := NewNativeTemplate(0, 0, false)
	op := NewOperationForeignFunctionCall(func(_ context.Context, mc *MachineContext) error {
		mc.SetValue(values.Void)
		return nil
	})
	instr := tpl.AppendSideTableOp(op)
	tpl.AppendInstruction(instr)

	env := environment.NewTopLevelEnvironment().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), qt.Equals, values.Void)
}

func TestRunDispatch_IntegerPathErrHalt(t *testing.T) {
	c := qt.New(t)
	// OpComplex dispatching to an InlinedOperation that returns errHalt
	// should trigger errHalt, which Run translates to nil.
	tpl := NewNativeTemplate(0, 0, false)
	op := NewOperationForeignFunctionCall(func(_ context.Context, mc *MachineContext) error {
		return errHalt
	})
	instr := tpl.AppendSideTableOp(op)
	tpl.AppendInstruction(instr)

	env := environment.NewTopLevelEnvironment().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
}

func TestRunDispatch_EmptyTemplate(t *testing.T) {
	c := qt.New(t)
	// Empty template (neither operations nor code) returns nil immediately.
	tpl := NewNativeTemplate(0, 0, false)
	env := environment.NewTopLevelEnvironment().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
}

func TestRunDispatch_UnimplementedOpcode(t *testing.T) {
	c := qt.New(t)
	// An opcode with no switch case returns ErrUnknownOpCode.
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpInvalid, Arg: 0})

	env := environment.NewTopLevelEnvironment().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, values.ErrUnknownOpCode), qt.IsTrue)
}

func TestRunDispatch_IntegerPathMultipleOps(t *testing.T) {
	c := qt.New(t)
	// Multiple OpComplex instructions in sequence.
	tpl := NewNativeTemplate(0, 0, false)

	makeLoadVoidFF := func() *OperationForeignFunctionCall {
		return NewOperationForeignFunctionCall(func(_ context.Context, mc *MachineContext) error {
			mc.SetValue(values.Void)
			return nil
		})
	}

	// First: sets value to Void, advances pc
	instr0 := tpl.AppendSideTableOp(makeLoadVoidFF())
	tpl.AppendInstruction(instr0)

	// Second: sets value to Void again, advances pc
	instr1 := tpl.AppendSideTableOp(makeLoadVoidFF())
	tpl.AppendInstruction(instr1)

	env := environment.NewTopLevelEnvironment().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), qt.Equals, values.Void)
}
