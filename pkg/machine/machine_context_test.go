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
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

func TestNewMachineContext(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(3, 0, false)

	// Create a parent continuation to verify parent chain works
	parentCont := NewMachineContinuation(nil, nil, env)

	// Create a continuation with specific state
	cont := &MachineContinuation{
		vmState: vmState{
			env:      env,
			template: tpl,
			evals:    NewStack(),
			pc:       5,
		},
		parent: parentCont,
	}
	cont.SetValue(values.NewInteger(42))
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
	env := environment.NewNamespace().Runtime()

	// Create a continuation with nil parent
	cont := NewMachineContinuation(nil, nil, env)

	mc := NewMachineContext(context.Background(), cont)

	qt.Assert(t, mc.env, qt.Equals, env)
	qt.Assert(t, mc.cont, qt.IsNil) // nil parent means mc.cont should be nil
	qt.Assert(t, mc.pc, qt.Equals, 0)
}

func TestNewMachineContext_RoundTrip(t *testing.T) {
	// Test that saving and restoring a continuation preserves state
	env := environment.NewNamespace().Runtime()
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
	env := environment.NewNamespace().Runtime()
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
	env := environment.NewNamespace().Runtime()
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
	env := environment.NewNamespace().Runtime()
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
	mc.RestoreAndRelease(mc.cont)
	qt.Assert(t, mc.cont, qt.Equals, bottom1)
	qt.Assert(t, mc.Parent(), qt.Equals, bottom1)
	qt.Assert(t, mc.CallDepth(), qt.Equals, 1)
	qt.Assert(t, mc.PC(), qt.Equals, 20)
	mc.RestoreAndRelease(mc.cont)
	qt.Assert(t, mc.cont, qt.Equals, bottom0)
	qt.Assert(t, mc.Parent(), qt.IsNil)
	qt.Assert(t, mc.CallDepth(), qt.Equals, 0)
	qt.Assert(t, mc.PC(), qt.Equals, 10)
}

func TestMachineContext_SetValues_GetValues(t *testing.T) {
	env := environment.NewNamespace().Runtime()
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
	env := environment.NewNamespace().Runtime()
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
	topEnv := environment.NewNamespace().Runtime()
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
	// But shares the mutable runtime top-level environment (post sealed-base carve,
	// NewSubContext captures Namespace().Runtime(), not the sealed base TopLevel()).
	qt.Assert(t, subCtx.env, qt.Equals, env.Namespace().Runtime())
}

func TestMachineContext_Restore(t *testing.T) {
	topEnv := environment.NewNamespace().Runtime()
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
	topEnv := environment.NewNamespace().Runtime()
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
	topEnv := environment.NewNamespace().Runtime()
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
	topEnv := environment.NewNamespace().Runtime()
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
	topEnv := environment.NewNamespace().Runtime()
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
	topEnv := environment.NewNamespace().Runtime()

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
	topEnv := environment.NewNamespace().Runtime()

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
	topEnv := environment.NewNamespace().Runtime()
	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(2, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	_, err := mc.Apply(cls, values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrWrongNumberOfArguments), qt.IsTrue)
	qt.Assert(t, err.Error(), qt.Contains, "expected 2 arguments, got 3")
}

func TestMachineContext_Apply_ZeroArity(t *testing.T) {
	topEnv := environment.NewNamespace().Runtime()
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
	topEnv := environment.NewNamespace().Runtime()
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
	topEnv := environment.NewNamespace().Runtime()
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
	topEnv := environment.NewNamespace().Runtime()
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
	topEnv := environment.NewNamespace().Runtime()
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
	topEnv := environment.NewNamespace().Runtime()
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
	topEnv := environment.NewNamespace().Runtime()
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
	topEnv := environment.NewNamespace().Runtime()
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
	topEnv := environment.NewNamespace().Runtime()

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
			qt.Assert(t, errors.Is(err, werr.ErrWrongNumberOfArguments), qt.IsTrue)
		})
	}
}

func TestMachineContext_ApplyCaseLambda_VariadicClause(t *testing.T) {
	topEnv := environment.NewNamespace().Runtime()

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
	topEnv := environment.NewNamespace().Runtime()

	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(2, 0, false)
	cls := NewClosureWithTemplate(tpl, env)

	caseLambda := NewCaseLambdaClosure([]*MachineClosure{cls})
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	_, err := mc.ApplyCaseLambda(caseLambda, values.NewInteger(1))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrWrongNumberOfArguments), qt.IsTrue)
}

func TestMachineContext_Error(t *testing.T) {
	env := environment.NewNamespace().Runtime()
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
	env := environment.NewNamespace().Runtime()

	// No template means no source
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	err := mc.Error("no source error")

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Message, qt.Equals, "no source error")
	qt.Assert(t, err.Source, qt.IsNil)
}

func TestMachineContext_WrapError(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)

	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))

	cause := werr.NewForeignErrorf("original error")
	err := mc.WrapError(cause, "wrapped message")

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Message, qt.Equals, "wrapped message")
	qt.Assert(t, err.Cause, qt.Equals, cause)
}

func TestMachineContext_WrapError_EmptyMessage(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)

	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))

	cause := werr.NewForeignErrorf("original error")
	err := mc.WrapError(cause, "")

	qt.Assert(t, err, qt.IsNotNil)
	// Empty message should use cause's message
	qt.Assert(t, err.Message, qt.Equals, "original error")
	qt.Assert(t, err.Cause, qt.Equals, cause)
}

// Tests moved from coverage_additional_test.go
// TestExecuteSimpleProcedureCall tests actually running a procedure call
// TestExecuteSimpleProcedureCall, TestExecuteVariadicProcedure,
// TestMachineContextNewSubContext, TestMachineContextApplySimple
// moved to machine_context_pipeline_test.go (external test, needs compilation pipeline).

// TestMachineContextSetValues tests SetValues and GetValues
func TestMachineContextSetValues(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
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
	env := newNamespace(environment.NewNamespace().Runtime())
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

// TestMachineContextValueMethods tests MachineContext value get/set
func TestMachineContextValueMethods(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
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
	env := newNamespace(environment.NewNamespace().Runtime())
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
	topEnv := environment.NewNamespace().Runtime()
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

func TestApply_RecordsNamedMachineClosureCall(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()
	lenv := environment.NewLocalEnvironment(1)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(1, 0, false)
	tpl.SetName("my-scheme-proc")

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	// Enable call tracking.
	mc.counters.callCounts = make(map[string]uint64)

	_, err := mc.Apply(cls, values.NewInteger(42))
	c.Assert(err, qt.IsNil)
	c.Assert(mc.counters.callCounts["my-scheme-proc"], qt.Equals, uint64(1))
}

func TestApply_SkipsAnonymousClosure(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()
	lenv := environment.NewLocalEnvironment(1)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(1, 0, false)
	// No SetName — anonymous lambda.

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	mc.counters.callCounts = make(map[string]uint64)

	_, err := mc.Apply(cls, values.NewInteger(42))
	c.Assert(err, qt.IsNil)
	c.Assert(len(mc.counters.callCounts), qt.Equals, 0)
}

func TestApplyCallable_CaseLambdaClosure(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()

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
	topEnv := environment.NewNamespace().Runtime()
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
			param := NewParameter(tc.initVal, nil, MutableBase)
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
	topEnv := environment.NewNamespace().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)

	param := NewParameter(values.NewInteger(0), nil, MutableBase)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	sub := mc.NewSubContext()
	_, err := sub.ApplyCallable(param, values.NewInteger(1), values.NewInteger(2))
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "expected 0 or 1 arguments")
}

func TestApplyCallable_Parameter_WithContinuation(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(NewOperationRestoreContinuation())

	param := NewParameter(values.NewInteger(42), nil, MutableBase)

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
	topEnv := environment.NewNamespace().Runtime()
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

// A continuation invoked with multiple values resumes with all of them
// (R7RS §6.10). An empty-segment composable continuation invoked with two
// values delivers both to the value register as multiple values.
func TestApplyCallable_ComposableContinuation_MultipleValues(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)

	cc := NewComposableContinuation(nil, nil, 0, nil)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	_, err := mc.ApplyCallable(cc, values.NewInteger(1), values.NewInteger(2))
	c.Assert(err, qt.IsNil)
	got := mc.GetValues()
	c.Assert(len(got), qt.Equals, 2)
	c.Assert(got[0], valuestest.SchemeEquals, values.NewInteger(1))
	c.Assert(got[1], valuestest.SchemeEquals, values.NewInteger(2))
}

// A continuation invoked with zero values resumes with NO values — the register
// is canonical-empty (GetValues len 0), not a fabricated single Void. Pins the
// register-level distinction the Scheme-level nullary-consumer test cannot
// observe directly.
func TestApplyCallable_ComposableContinuation_ZeroValues(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)

	cc := NewComposableContinuation(nil, nil, 0, nil)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	_, err := mc.ApplyCallable(cc)
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValues(), qt.HasLen, 0)
}

func TestApplyCallable_NonCallable(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()
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
			c.Assert(errors.Is(err, werr.ErrNotAProcedure), qt.IsTrue)
		})
	}
}

func TestApplyCallable_Nil(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	_, err := mc.ApplyCallable(nil)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "cannot apply nil")
	c.Assert(errors.Is(err, werr.ErrNotAProcedure), qt.IsTrue)
}

// (Tests for field-based exception-handler inheritance across sub-contexts were
// removed in piece E: the handler now rides the %exception-handlers parameter,
// inherited via parentMC marks and captured by call/cc. That behavior is covered by
// the exception-capture tests in pkg/registry/core.)

func TestSaveContinuation_CallDepthTracking(t *testing.T) {
	tests := []struct {
		name         string
		maxCallDepth int
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
		{
			name:         "negative clamped to zero (unlimited)",
			maxCallDepth: -1,
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
			topEnv := environment.NewNamespace().Runtime()
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
				if !errors.Is(lastErr, werr.ErrCallDepthExceeded) {
					t.Fatalf("expected ErrCallDepthExceeded, got: %v", lastErr)
				}
			} else if lastErr != nil {
				t.Fatalf("unexpected error: %v", lastErr)
			}
		})
	}
}

// TestRestoreAndRelease_ReleasesCallDepthBudget pins that a normal return gives
// the call-depth budget back: five saves then three returns must leave room for
// eight more before the limit bites.
func TestRestoreAndRelease_ReleasesCallDepthBudget(t *testing.T) {
	ops := make([]Operation, 20)
	for i := range ops {
		ops[i] = NewOperationLoadVoid()
	}
	tpl := NewNativeTemplate(0, 0, false, ops...)
	topEnv := environment.NewNamespace().Runtime()
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

	// Return from 3 of them
	for range 3 {
		mc.RestoreAndRelease(mc.cont)
	}

	// Should be able to save 8 more (was at depth 2 after returns, limit 10)
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
	if !errors.Is(err, werr.ErrCallDepthExceeded) {
		t.Fatalf("expected ErrCallDepthExceeded, got: %v", err)
	}
}

func TestNewSubContext_InheritsMaxCallDepth(t *testing.T) {
	topEnv := environment.NewNamespace().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	mc.SetMaxCallDepth(42)

	sub := mc.NewSubContext()
	if sub.MaxCallDepth() != 42 {
		t.Fatalf("sub-context maxCallDepth = %d, want 42", sub.MaxCallDepth())
	}
}

func TestNewThreadSubContext_InheritsMaxCallDepth(t *testing.T) {
	topEnv := environment.NewNamespace().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	mc.SetMaxCallDepth(99)

	params := mc.CaptureSubContextParams()
	thunk := NewParameter(nil, nil, MutableBase)
	thread := values.NewThread(thunk, "test-thread")
	sub := NewThreadSubContext(params, thread)
	if sub.MaxCallDepth() != 99 {
		t.Fatalf("thread sub-context maxCallDepth = %d, want 99", sub.MaxCallDepth())
	}
}

func TestNewSubContext_InheritsMaxStackSize(t *testing.T) {
	topEnv := environment.NewNamespace().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	mc.SetMaxStackSize(500)

	sub := mc.NewSubContext()
	if sub.MaxStackSize() != 500 {
		t.Fatalf("sub-context maxStackSize = %d, want 500", sub.MaxStackSize())
	}
}

func TestNewThreadSubContext_InheritsMaxStackSize(t *testing.T) {
	topEnv := environment.NewNamespace().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))
	mc.SetMaxStackSize(750)

	params := mc.CaptureSubContextParams()
	thunk := NewParameter(nil, nil, MutableBase)
	thread := values.NewThread(thunk, "test-thread")
	sub := NewThreadSubContext(params, thread)
	if sub.MaxStackSize() != 750 {
		t.Fatalf("thread sub-context maxStackSize = %d, want 750", sub.MaxStackSize())
	}
}

func TestNewSubContextWithTemplate(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	parent := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	// Set up parent state that should propagate
	parent.SetMaxCallDepth(100)
	parent.SetMaxStackSize(200)
	parent.windingStack = WindingStack{{}}

	// Target template and env for sub-context
	tpl := NewNativeTemplate(0, 0, false)
	targetEnv := environment.NewEnvironmentFrameWithParent(nil, topEnv)

	sub := parent.NewSubContextWithTemplate(tpl, targetEnv)
	defer ReleaseSubContext(sub)

	// Template and env come from arguments, not parent
	c.Assert(sub.template, qt.Equals, tpl)
	c.Assert(sub.env, qt.Equals, targetEnv)
	c.Assert(sub.pc, qt.Equals, 0)

	// All NewSubContext fields propagate from parent
	c.Assert(sub.parentMC, qt.Equals, parent)
	c.Assert(sub.maxCallDepth, qt.Equals, 100)
	c.Assert(sub.maxStackSize, qt.Equals, uint64(200))
	c.Assert(len(sub.windingStack), qt.Equals, 1)

	// Fresh state
	c.Assert(sub.evals.Len(), qt.Equals, 0)
	c.Assert(sub.cont, qt.IsNil)
}

// --- Dispatch tests (Phase 6) ---

func TestRunDispatch_InitialOperations(t *testing.T) {
	c := qt.New(t)
	// Template created with initial operations converts them to bytecode.
	tpl := NewNativeTemplate(0, 0, false, NewOperationLoadVoid())
	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), qt.Equals, values.Void)
}

// testInlinedOp is a test-only InlinedOperation for exercising the
// OpComplex side-table dispatch path without depending on any specific
// production operation type.
type testInlinedOp struct {
	OperationBase
	fn func(mc *MachineContext) (*MachineContext, error)
}

func newTestInlinedOp(fn func(mc *MachineContext) (*MachineContext, error)) *testInlinedOp {
	return &testInlinedOp{
		OperationBase: NewOperationBase("test-inlined-op"),
		fn:            fn,
	}
}

func (p *testInlinedOp) Apply(mc *MachineContext) (*MachineContext, error) {
	return p.fn(mc)
}

func (*testInlinedOp) OpKind() OpCode {
	return OpComplex
}

func (p *testInlinedOp) EqualTo(o values.Value) bool {
	_, ok := o.(*testInlinedOp)
	return ok && p == o
}

func TestRunDispatch_IntegerPathOpComplex(t *testing.T) {
	c := qt.New(t)
	// Template with code + sideTable uses runIntegerDispatch.
	tpl := NewNativeTemplate(0, 0, false)
	op := newTestInlinedOp(func(mc *MachineContext) (*MachineContext, error) {
		mc.SetValue(values.Void)
		mc.pc++
		return mc, nil
	})
	instr := tpl.AppendSideTableOp(op)
	tpl.AppendInstruction(instr)

	env := environment.NewNamespace().Runtime()
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
	op := newTestInlinedOp(func(mc *MachineContext) (*MachineContext, error) {
		return mc, errHalt
	})
	instr := tpl.AppendSideTableOp(op)
	tpl.AppendInstruction(instr)

	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
}

func TestRunDispatch_EmptyTemplate(t *testing.T) {
	c := qt.New(t)
	// Empty template (neither operations nor code) returns nil immediately.
	tpl := NewNativeTemplate(0, 0, false)
	env := environment.NewNamespace().Runtime()
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

	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrUnknownOpCode), qt.IsTrue)
	// Routed through WrapError (not bare werr): the VM-internal failure now
	// reaches the caller as a *SchemeError carrying a backtrace, while the
	// sentinel stays matchable via the Unwrap chain.
	var se *SchemeError
	c.Assert(errors.As(err, &se), qt.IsTrue)
	c.Assert(se.StackTrace, qt.Not(qt.Equals), "")
}

func TestRunDispatch_IntegerPathMultipleOps(t *testing.T) {
	c := qt.New(t)
	// Multiple OpComplex instructions in sequence.
	tpl := NewNativeTemplate(0, 0, false)

	makeLoadVoidOp := func() *testInlinedOp {
		return newTestInlinedOp(func(mc *MachineContext) (*MachineContext, error) {
			mc.SetValue(values.Void)
			mc.pc++
			return mc, nil
		})
	}

	// First: sets value to Void, advances pc
	instr0 := tpl.AppendSideTableOp(makeLoadVoidOp())
	tpl.AppendInstruction(instr0)

	// Second: sets value to Void again, advances pc
	instr1 := tpl.AppendSideTableOp(makeLoadVoidOp())
	tpl.AppendInstruction(instr1)

	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), qt.Equals, values.Void)
}

// --- Wave 1: zero-operand ops ---

func TestRunDispatch_OpPush(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	litIdx := tpl.MaybeAppendLiteral(values.NewInteger(42))

	// Load 42 → Push → Pop → verify round-trip
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(litIdx)})
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpPop})

	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

func TestRunDispatch_OpPush_Nil(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	// OpPush with nil value register should be a no-op (line 631 guard)
	tpl.AppendInstruction(Instruction{Op: OpPush})

	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.evals.Len(), qt.Equals, 0)
}

func TestRunDispatch_OpPop(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	lit0 := tpl.MaybeAppendLiteral(values.NewInteger(10))
	lit1 := tpl.MaybeAppendLiteral(values.NewInteger(20))

	// Push 10, push 20 → Pop → verify top (20) in value register
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(lit0)})
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(lit1)})
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpPop})

	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(20))
	c.Assert(mc.evals.Len(), qt.Equals, 1) // 10 still on stack
}

func TestRunDispatch_OpPull(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	lit0 := tpl.MaybeAppendLiteral(values.NewInteger(10))
	lit1 := tpl.MaybeAppendLiteral(values.NewInteger(20))

	// Push A(10), Push B(20) → Pull → verify A (bottom) in value register
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(lit0)})
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(lit1)})
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpPull})

	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(10))
	c.Assert(mc.evals.Len(), qt.Equals, 1) // 20 still on stack
}

func TestRunDispatch_OpDrop(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	lit0 := tpl.MaybeAppendLiteral(values.NewInteger(10))
	lit1 := tpl.MaybeAppendLiteral(values.NewInteger(20))

	// Push A(10), Push B(20) → Drop (discard 20) → Pop → verify A(10)
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(lit0)})
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(lit1)})
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpDrop})
	tpl.AppendInstruction(Instruction{Op: OpPop})

	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(10))
}

func TestRunDispatch_OpPopEnv(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpPopEnv})

	topEnv := environment.NewNamespace().Runtime()
	childLenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(childLenv, topEnv)

	cont := NewMachineContinuation(nil, tpl, childEnv)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.EnvironmentFrame(), qt.Equals, topEnv)
}

func TestRunDispatch_OpPopEnv_TopLevel(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpPopEnv})

	// Every owner VIEW is parent-nil post-fold, so popping the root view exercises
	// the nil-parent error path directly.
	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNilParentEnvironment), qt.IsTrue)
	// Routed through WrapError: enriched *SchemeError, sentinel still matchable.
	var se *SchemeError
	c.Assert(errors.As(err, &se), qt.IsTrue)
}

func TestRunDispatch_OpRestoreContinuation_NilCont(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpRestoreContinuation})

	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)
	// mc.cont is nil (cont had nil parent)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
}

// --- Wave 2: single-operand ops ---

func TestRunDispatch_OpBranch(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)

	// Branch +2 → skip OpInvalid → OpLoadVoid
	tpl.AppendInstruction(Instruction{Op: OpBranch, Arg: 2})
	tpl.AppendInstruction(Instruction{Op: OpInvalid})
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})

	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), qt.Equals, values.Void)
}

func TestRunDispatch_OpBranchOnFalseValue_False(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	litFalse := tpl.MaybeAppendLiteral(values.FalseValue)

	// Load #f → BranchOnFalseValue +2 → skip OpInvalid → OpLoadVoid
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(litFalse)})
	tpl.AppendInstruction(Instruction{Op: OpBranchOnFalseValue, Arg: 2})
	tpl.AppendInstruction(Instruction{Op: OpInvalid})
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})

	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), qt.Equals, values.Void)
}

func TestRunDispatch_OpBranchOnFalseValue_True(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	litTrue := tpl.MaybeAppendLiteral(values.TrueValue)
	lit42 := tpl.MaybeAppendLiteral(values.NewInteger(42))

	// Load #t → BranchOnFalseValue +2 (not taken) → load 42
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(litTrue)})
	tpl.AppendInstruction(Instruction{Op: OpBranchOnFalseValue, Arg: 2})
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(lit42)})

	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

func TestRunDispatch_OpSaveContinuation(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	lit42 := tpl.MaybeAppendLiteral(values.NewInteger(42))
	lit99 := tpl.MaybeAppendLiteral(values.NewInteger(99))

	// 0: SaveContinuation +3 → saves pc=0+3=3, advances to 1
	// 1: LoadLiteral 42
	// 2: RestoreContinuation → restores pc=3
	// 3: LoadLiteral 99 → this proves we landed at the target
	tpl.AppendInstruction(Instruction{Op: OpSaveContinuation, Arg: 3})
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(lit42)})
	tpl.AppendInstruction(Instruction{Op: OpRestoreContinuation})
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(lit99)})

	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	// Value 42 was set at instruction 1; RestoreContinuation preserves it,
	// then instruction 3 overwrites with 99.
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(99))
}

func TestRunDispatch_OpLoadLiteral(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	litIdx := tpl.MaybeAppendLiteral(values.NewInteger(42))
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(litIdx)})

	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

func TestRunDispatch_OpPeekK(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	lit0 := tpl.MaybeAppendLiteral(values.NewInteger(10))
	lit1 := tpl.MaybeAppendLiteral(values.NewInteger(20))

	// Push A(10), Push B(20) → PeekK 0 → verify B (top) in value register, stack unchanged
	// PeekK(0) = top, PeekK(1) = second from top, etc.
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(lit0)})
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(lit1)})
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpPeekK, Arg: 0})

	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(20)) // top of stack
	c.Assert(mc.evals.Len(), qt.Equals, 2)                                  // stack unchanged
}

func TestRunDispatch_OpLoadGlobal(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	sym := values.NewSymbol("test-var")
	gi, _ := env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable, nil)

	// Set the global binding value
	bd := env.GetGlobalBinding(gi)
	bd.SetValue(values.NewInteger(99))

	tpl := NewNativeTemplate(0, 0, false)
	litIdx := tpl.MaybeAppendLiteral(gi)
	tpl.AppendInstruction(Instruction{Op: OpLoadGlobal, Arg: int32(litIdx)})

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(99))
}

func TestRunDispatch_OpLoadGlobal_NoBinding(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	// Create a GlobalIndex for a symbol that doesn't exist as a binding
	sym := values.NewSymbol("nonexistent")
	gi := environment.NewGlobalIndex(sym)

	tpl := NewNativeTemplate(0, 0, false)
	litIdx := tpl.MaybeAppendLiteral(gi)
	tpl.AppendInstruction(Instruction{Op: OpLoadGlobal, Arg: int32(litIdx)})

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "no such global binding")
}

func TestRunDispatch_OpLoadGlobal_Sentinels(t *testing.T) {
	t.Run("nil literal returns ErrInvalidLiteralIndex", func(t *testing.T) {
		c := qt.New(t)
		env := environment.NewNamespace().Runtime()
		tpl := NewNativeTemplate(0, 0, false)
		// Append a nil literal manually to simulate a corrupt literal slot.
		tpl.literals = append(tpl.literals, nil)
		tpl.AppendInstruction(Instruction{Op: OpLoadGlobal, Arg: 0})

		cont := NewMachineContinuation(nil, tpl, env)
		mc := NewMachineContext(context.Background(), cont)

		err := mc.Run()
		c.Assert(errors.Is(err, ErrInvalidLiteralIndex), qt.IsTrue)
	})

	t.Run("wrong type returns ErrInvalidGlobalIndex", func(t *testing.T) {
		c := qt.New(t)
		env := environment.NewNamespace().Runtime()
		tpl := NewNativeTemplate(0, 0, false)
		// Put a non-GlobalIndex value in the literal slot.
		litIdx := tpl.MaybeAppendLiteral(values.NewInteger(1))
		tpl.AppendInstruction(Instruction{Op: OpLoadGlobal, Arg: int32(litIdx)})

		cont := NewMachineContinuation(nil, tpl, env)
		mc := NewMachineContext(context.Background(), cont)

		err := mc.Run()
		c.Assert(errors.Is(err, ErrInvalidGlobalIndex), qt.IsTrue)
	})

	t.Run("missing binding returns ErrBindingNotFound", func(t *testing.T) {
		c := qt.New(t)
		env := environment.NewNamespace().Runtime()
		gi := environment.NewGlobalIndex(values.NewSymbol("nonexistent"))

		tpl := NewNativeTemplate(0, 0, false)
		litIdx := tpl.MaybeAppendLiteral(gi)
		tpl.AppendInstruction(Instruction{Op: OpLoadGlobal, Arg: int32(litIdx)})

		cont := NewMachineContinuation(nil, tpl, env)
		mc := NewMachineContext(context.Background(), cont)

		err := mc.Run()
		c.Assert(errors.Is(err, ErrBindingNotFound), qt.IsTrue)
	})
}

func TestRunDispatch_OpStoreGlobal_Sentinels(t *testing.T) {
	t.Run("missing binding returns ErrBindingNotFound", func(t *testing.T) {
		c := qt.New(t)
		env := environment.NewNamespace().Runtime()
		gi := environment.NewGlobalIndex(values.NewSymbol("nonexistent"))

		tpl := NewNativeTemplate(0, 0, false)
		litVal := tpl.MaybeAppendLiteral(values.NewInteger(1))
		litGI := tpl.MaybeAppendLiteral(gi)
		tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(litVal)})
		tpl.AppendInstruction(Instruction{Op: OpPush})
		tpl.AppendInstruction(Instruction{Op: OpStoreGlobal, Arg: int32(litGI)})

		cont := NewMachineContinuation(nil, tpl, env)
		mc := NewMachineContext(context.Background(), cont)

		err := mc.Run()
		c.Assert(errors.Is(err, ErrBindingNotFound), qt.IsTrue)
	})
}

func TestRunDispatch_OpLoadLocal_Sentinels(t *testing.T) {
	t.Run("missing binding returns ErrBindingNotFound", func(t *testing.T) {
		c := qt.New(t)
		env := environment.NewNamespace().Runtime()
		// depth=1 with no parent local frame — resolveLocalBinding returns nil.

		tpl := NewNativeTemplate(0, 0, false)
		li := environment.NewLocalIndex(0, 1)
		tpl.AppendInstruction(Instruction{Op: OpLoadLocal, Arg: EncodeLocalIndex(li)})

		cont := NewMachineContinuation(nil, tpl, env)
		mc := NewMachineContext(context.Background(), cont)

		err := mc.Run()
		c.Assert(errors.Is(err, ErrBindingNotFound), qt.IsTrue)
	})
}

func TestRunDispatch_OpStoreGlobal(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	sym := values.NewSymbol("store-var")
	gi, _ := env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable, nil)

	tpl := NewNativeTemplate(0, 0, false)
	litVal := tpl.MaybeAppendLiteral(values.NewInteger(77))
	litGI := tpl.MaybeAppendLiteral(gi)

	// Load 77 → Push → StoreGlobal
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(litVal)})
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpStoreGlobal, Arg: int32(litGI)})

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)

	// Verify the binding was updated
	bd := env.GetGlobalBinding(gi)
	c.Assert(bd.Value(), valuestest.SchemeEquals, values.NewInteger(77))
}

// --- Wave 3: two-operand ops (bit-packed local index) ---

func TestRunDispatch_OpLoadLocal(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()
	lenv := environment.NewLocalEnvironment(1)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)

	// Set slot 0 to 42
	err := env.SetLocalValueBySlotDepth(0, 0, values.NewInteger(42))
	c.Assert(err, qt.IsNil)

	tpl := NewNativeTemplate(0, 0, false)
	li := environment.NewLocalIndex(0, 0) // slot=0, depth=0
	tpl.AppendInstruction(Instruction{Op: OpLoadLocal, Arg: EncodeLocalIndex(li)})

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err = mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

func TestRunDispatch_OpLoadLocal_Depth(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()

	// Parent frame with slot 0 = 99
	parentLenv := environment.NewLocalEnvironment(1)
	parentEnv := environment.NewEnvironmentFrameWithParent(parentLenv, topEnv)
	err := parentEnv.SetLocalValueBySlotDepth(0, 0, values.NewInteger(99))
	c.Assert(err, qt.IsNil)

	// Child frame
	childLenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(childLenv, parentEnv)

	tpl := NewNativeTemplate(0, 0, false)
	li := environment.NewLocalIndex(0, 1) // slot=0, depth=1
	tpl.AppendInstruction(Instruction{Op: OpLoadLocal, Arg: EncodeLocalIndex(li)})

	cont := NewMachineContinuation(nil, tpl, childEnv)
	mc := NewMachineContext(context.Background(), cont)

	err = mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(99))
}

func TestRunDispatch_OpLoadLocal_NoBinding(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	tpl := NewNativeTemplate(0, 0, false)
	// No local environment → slot 0 doesn't exist
	li := environment.NewLocalIndex(0, 0)
	tpl.AppendInstruction(Instruction{Op: OpLoadLocal, Arg: EncodeLocalIndex(li)})

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "no such local binding")
}

func TestRunDispatch_OpStoreLocal(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()
	lenv := environment.NewLocalEnvironment(1)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)

	tpl := NewNativeTemplate(0, 0, false)
	litVal := tpl.MaybeAppendLiteral(values.NewInteger(55))
	li := environment.NewLocalIndex(0, 0)

	// Load 55 → Push → StoreLocal
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(litVal)})
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpStoreLocal, Arg: EncodeLocalIndex(li)})

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)

	// Verify the local binding was updated
	bd := env.GetLocalBindingBySlotDepth(0, 0)
	c.Assert(bd, qt.IsNotNil)
	c.Assert(bd.Value(), valuestest.SchemeEquals, values.NewInteger(55))
}

// --- Wave 4: fused push ops ---

func TestRunDispatch_OpPushLiteral(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	litIdx := tpl.MaybeAppendLiteral(values.NewInteger(42))

	// PushLiteral → Pop → verify 42
	tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx)})
	tpl.AppendInstruction(Instruction{Op: OpPop})

	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

func TestRunDispatch_OpPushGlobal(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	sym := values.NewSymbol("push-global-var")
	gi, _ := env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable, nil)
	bd := env.GetGlobalBinding(gi)
	bd.SetValue(values.NewInteger(99))

	tpl := NewNativeTemplate(0, 0, false)
	litIdx := tpl.MaybeAppendLiteral(gi)

	// PushGlobal → Pop → verify 99
	tpl.AppendInstruction(Instruction{Op: OpPushGlobal, Arg: int32(litIdx)})
	tpl.AppendInstruction(Instruction{Op: OpPop})

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(99))
}

func TestRunDispatch_OpPushLocal(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()
	lenv := environment.NewLocalEnvironment(1)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	err := env.SetLocalValueBySlotDepth(0, 0, values.NewInteger(42))
	c.Assert(err, qt.IsNil)

	tpl := NewNativeTemplate(0, 0, false)
	li := environment.NewLocalIndex(0, 0)

	// PushLocal → Pop → verify 42
	tpl.AppendInstruction(Instruction{Op: OpPushLocal, Arg: EncodeLocalIndex(li)})
	tpl.AppendInstruction(Instruction{Op: OpPop})

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err = mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

// --- Wave 5: fused call + promoted complex ---

func TestRunDispatch_OpPullApply(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()

	// Create closure template: 1 param, loads local slot 0 (the arg) then ends
	clsTpl := NewNativeTemplate(1, 0, false)
	li := environment.NewLocalIndex(0, 0)
	clsTpl.AppendInstruction(Instruction{Op: OpLoadLocal, Arg: EncodeLocalIndex(li)})

	// Create closure with its own environment (1 slot for the parameter)
	clsLenv := environment.NewLocalEnvironment(1)
	clsEnv := environment.NewEnvironmentFrameWithParent(clsLenv, topEnv)
	cls := NewClosureWithTemplate(clsTpl, clsEnv)

	// Outer template: push closure, push arg(42), PullApply
	tpl := NewNativeTemplate(0, 0, false)
	litCls := tpl.MaybeAppendLiteral(cls)
	litArg := tpl.MaybeAppendLiteral(values.NewInteger(42))

	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(litCls)})
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(litArg)})
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpPullApply})

	env := topEnv
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

func TestRunDispatch_OpMakeClosure(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()

	// Template and env to push on stack
	innerTpl := NewNativeTemplate(1, 0, false)
	innerTpl.AppendInstruction(Instruction{Op: OpLoadVoid})
	innerLenv := environment.NewLocalEnvironment(1)
	innerEnv := environment.NewEnvironmentFrameWithParent(innerLenv, topEnv)

	tpl := NewNativeTemplate(0, 0, false)
	litTpl := tpl.MaybeAppendLiteral(innerTpl)
	litEnv := tpl.MaybeAppendLiteral(innerEnv)

	// Push order: template first, then env (Pop order: env first, then template)
	tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litTpl)})
	tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litEnv)})
	tpl.AppendInstruction(Instruction{Op: OpMakeClosure})

	cont := NewMachineContinuation(nil, tpl, topEnv)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)

	result := mc.GetValue()
	_, ok := result.(*MachineClosure)
	c.Assert(ok, qt.IsTrue)
}

func TestRunDispatch_OpMakeClosure_BadTemplate(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()
	innerLenv := environment.NewLocalEnvironment(1)
	innerEnv := environment.NewEnvironmentFrameWithParent(innerLenv, topEnv)

	tpl := NewNativeTemplate(0, 0, false)
	// Push a non-template (integer) in the template position
	litBad := tpl.MaybeAppendLiteral(values.NewInteger(999))
	litEnv := tpl.MaybeAppendLiteral(innerEnv)

	tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litBad)})
	tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litEnv)})
	tpl.AppendInstruction(Instruction{Op: OpMakeClosure})

	cont := NewMachineContinuation(nil, tpl, topEnv)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNotAMachineTemplate), qt.IsTrue)
}

func TestRunDispatch_OpMakeClosure_BadEnv(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()

	innerTpl := NewNativeTemplate(1, 0, false)
	innerTpl.AppendInstruction(Instruction{Op: OpLoadVoid})

	tpl := NewNativeTemplate(0, 0, false)
	litTpl := tpl.MaybeAppendLiteral(innerTpl)
	// Push a non-env (integer) in the env position
	litBad := tpl.MaybeAppendLiteral(values.NewInteger(999))

	tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litTpl)})
	tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litBad)})
	tpl.AppendInstruction(Instruction{Op: OpMakeClosure})

	cont := NewMachineContinuation(nil, tpl, topEnv)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNotALocalEnvironmentFrame), qt.IsTrue)
}

// --- Wave 6: cached binding ops ---

func TestRunDispatch_OpLoadCachedBinding(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	sym := values.NewSymbol("cached-var")
	gi, _ := env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable, nil)
	bd := env.GetGlobalBinding(gi)
	bd.SetValue(values.NewInteger(42))

	tpl := NewNativeTemplate(0, 0, false)
	cbIdx := tpl.AppendCachedBinding(bd)
	tpl.AppendInstruction(Instruction{Op: OpLoadCachedBinding, Arg: cbIdx})

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

func TestRunDispatch_OpPushCachedBinding(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	sym := values.NewSymbol("push-cached-var")
	gi, _ := env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable, nil)
	bd := env.GetGlobalBinding(gi)
	bd.SetValue(values.NewInteger(42))

	tpl := NewNativeTemplate(0, 0, false)
	cbIdx := tpl.AppendCachedBinding(bd)

	// PushCachedBinding → Pop → verify 42
	tpl.AppendInstruction(Instruction{Op: OpPushCachedBinding, Arg: cbIdx})
	tpl.AppendInstruction(Instruction{Op: OpPop})

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

// markGet finds the value for key in a marks slice using eq? semantics, or nil.
// Test helper only.
func markGet(marks []markEntry, key values.Value) values.Value {
	for _, e := range marks {
		if values.EqIdentity(e.key, key) {
			return e.val
		}
	}
	return nil
}

// markSet sets the value for key in a marks slice using eq? semantics.
// Panics if key is not present. Test helper only.
func markSet(marks []markEntry, key values.Value, val values.Value) {
	for i := range marks {
		if values.EqIdentity(marks[i].key, key) {
			marks[i].val = val
			return
		}
	}
	panic("markSet: key not found")
}

// newContMarkTestContext creates a minimal MachineContext for continuation mark tests.
func newContMarkTestContext() *MachineContext {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	return NewMachineContext(context.Background(), cont)
}

func TestContMark_SetGetDelete(t *testing.T) {
	c := qt.New(t)
	mc := newContMarkTestContext()

	key := values.NewSymbol("k")

	// Initially no marks
	val := mc.GetMark(key)
	c.Assert(val, qt.IsNil)

	// Set and get
	mc.SetMark(key, values.NewInteger(42))
	val = mc.GetMark(key)
	c.Assert(val, qt.Equals, values.NewInteger(42))

	// Delete
	mc.DeleteMark(key)
	val = mc.GetMark(key)
	c.Assert(val, qt.IsNil)
}

func TestContMark_SaveContinuation_NilsMarks(t *testing.T) {
	c := qt.New(t)
	mc := newContMarkTestContext()

	key := values.NewSymbol("k")
	mc.SetMark(key, values.NewInteger(1))

	err := mc.SaveContinuation(1)
	c.Assert(err, qt.IsNil)

	// After save, mc.marks should be nil (callee starts clean)
	c.Assert(mc.GetMark(key), qt.IsNil)

	// Saved continuation should have the mark
	c.Assert(len(mc.cont.marks) > 0, qt.IsTrue)
	c.Assert(markGet(mc.cont.marks, key), qt.Equals, values.NewInteger(1))
}

func TestContMark_RestoreAndRelease_RestoresMarks(t *testing.T) {
	c := qt.New(t)
	mc := newContMarkTestContext()

	key := values.NewSymbol("k")
	otherKey := values.NewSymbol("other")
	mc.SetMark(key, values.NewInteger(1))

	err := mc.SaveContinuation(1)
	c.Assert(err, qt.IsNil)

	// Callee sets different mark
	mc.SetMark(otherKey, values.NewInteger(99))

	// Returning restores saved marks
	mc.RestoreAndRelease(mc.cont)
	c.Assert(mc.GetMark(key), qt.Equals, values.NewInteger(1))
	c.Assert(mc.GetMark(otherKey), qt.IsNil)
}

func TestContMark_Copy_Independent(t *testing.T) {
	c := qt.New(t)
	mc := newContMarkTestContext()

	key := values.NewSymbol("k")
	mc.SetMark(key, values.NewInteger(1))
	err := mc.SaveContinuation(1)
	c.Assert(err, qt.IsNil)

	original := mc.cont
	copied := original.Copy()

	// Mutating copy doesn't affect original
	markSet(copied.marks, key, values.NewInteger(999))
	c.Assert(markGet(original.marks, key), qt.Equals, values.NewInteger(1))
}

func TestMachineContext_Run_NegativePC(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	mc.SetPC(-1)
	err := mc.Run()
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, ErrInvalidProgramCounter), qt.IsTrue)
	// Routed through WrapError even with a corrupt pc: SourceAt bounds-checks,
	// so provenance degrades to nil source rather than panicking, and the
	// result is still an enriched *SchemeError.
	var se *SchemeError
	c.Assert(errors.As(err, &se), qt.IsTrue)
}

func TestMachineContext_CurrentLocation_Nil(t *testing.T) {
	// An empty MachineContext with no template has no source location.
	c := qt.New(t)
	mc := &MachineContext{}
	c.Assert(mc.CurrentLocation(), qt.IsNil)
}

func TestMachineContext_FormatStackTrace_Empty(t *testing.T) {
	c := qt.New(t)
	mc := &MachineContext{}
	c.Assert(mc.FormatStackTrace(10), qt.Equals, "")
}

func TestRun_TimerInterruptFromBytecodeLoop(t *testing.T) {
	// When a timerHandler is installed and the timer context has expired,
	// Run() must return *ErrTimerInterrupt (not the raw context error).
	// Uses WithTimeoutCause so context.Cause returns ErrTimerExpired.
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	ctx, cancel := context.WithTimeoutCause(context.Background(), 0, ErrTimerExpired)
	defer cancel()
	mc.SetContext(ctx)

	handler := NewClosureWithTemplate(NewEmptyNativeTemplate(), env)
	// Install a timer the way RunBodyUnderTimer does (the bare SetTimer API
	// was retired); only the handler is read on the interrupt path.
	mc.timer = &timerState{handler: handler, cancel: func() {}}

	tpl := NewNativeTemplate(0, 0, false, NewOperationLoadVoid())
	mc.template = tpl
	mc.pc = 0

	err := mc.Run()

	var timerErr *ErrTimerInterrupt
	c.Assert(errors.As(err, &timerErr), qt.IsTrue)
	c.Assert(timerErr.Handler, qt.Equals, handler)
}

func TestRun_ContextCancelWithoutTimerHandler(t *testing.T) {
	// When no timerHandler is installed and the context is already cancelled,
	// Run() must return context.Canceled (existing behavior preserved).
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	mc.SetContext(ctx)

	tpl := NewNativeTemplate(0, 0, false, NewOperationLoadVoid())
	mc.template = tpl
	mc.pc = 0

	err := mc.Run()

	c.Assert(errors.Is(err, context.Canceled), qt.IsTrue)
}

// NOTE: the former white-box TestRunWithEscapeHandling_TimerInterrupt was removed
// when with-timeout was reified (RunBodyUnderTimer + the tag-routed resolveTimerInterrupt
// arm). It installed a timer via the bare SetTimer API with no boundary frame on the
// chain — a configuration the reified design no longer produces (a fired timer is now
// routed by FindPrompt(tag) to its with-timeout finalizer frame). The timer-arm behavior
// it checked (handler invoked with a composable continuation, timer torn down, handler
// under a live ctx) is covered end-to-end by TestWithTimeout / TestWithTimeoutResumption /
// TestWithTimeoutNesting / TestWithTimeoutDynamicWind in registry/core, and by the
// eval-nested (sub-context) regression there.

func TestApplyCallableError_PassesThroughTimerInterrupt(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	handler := &ForeignClosure{name: "handler"}
	err := &ErrTimerInterrupt{Handler: handler}
	result := applyCallableError(mc, err)

	var timerErr *ErrTimerInterrupt
	c.Assert(errors.As(result, &timerErr), qt.IsTrue)
	c.Assert(timerErr.Handler, qt.Equals, handler)
}

// TestRunWithEscapeHandling_RecoversStackUnderflow proves the embedding-contract
// guarantee added at the VM boundary: a VM invariant guard that panics — here,
// OpPop on an empty eval stack, which trips Stack.Pop's ErrStackUnderflow guard —
// is recovered in RunWithEscapeHandling and surfaces as a RETURNED error wrapping
// werr.ErrStackUnderflow, not as a raw Go panic escaping the public Run path.
// Well-formed bytecode never underflows, so this deliberately drives the
// malformed-bytecode / compiler-bug path.
//
// The recover wraps the guard's *werr.ForeignError in a *SchemeError (via
// WrapError) so the embedder gets the VM's source location and stack trace, not
// a bare "stack underflow". This test pins both halves of that contract:
//   - the sentinel stays errors.Is-matchable (SchemeError.Unwrap chains to it), and
//   - the returned error IS a *SchemeError — distinguishing the wrap from the
//     earlier `rerr = err`, which returned the raw *ForeignError and for which the
//     errors.As below would fail.
//
// It deliberately does NOT assert *ErrExceptionEscape: a VM-invariant violation
// is a compiler/VM bug, so it must remain uncatchable by guard.
func TestRunWithEscapeHandling_RecoversStackUnderflow(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	// Single OpPop with an empty eval stack.
	tpl := NewNativeTemplate(0, 0, false, NewOperationPop())
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))

	// If the recover regressed, this call would panic and fail the test outright.
	err := mc.RunWithEscapeHandling()
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrStackUnderflow), qt.IsTrue)

	// The guard error is wrapped with VM context as a *SchemeError, not returned raw.
	var schemeErr *SchemeError
	c.Assert(errors.As(err, &schemeErr), qt.IsTrue)

	// It must NOT have been converted into a catchable Scheme exception.
	var escapeErr *ErrExceptionEscape
	c.Assert(errors.As(err, &escapeErr), qt.IsFalse)
}

// panickingComplexOp is a test-only side-table operation whose Apply panics with
// a caller-supplied value that is NOT a *werr.ForeignError. It exists to pin the
// CONTAINMENT of the boundary recover: every panic — VM-invariant guards
// (*werr.ForeignError) and any other Go-level panic (runtime.Error, non-error
// values) alike — is converted into a returned error so it stays within the VM
// boundary instead of unwinding into the embedder / REPL.
type panickingComplexOp struct {
	OperationBase
	panicValue any
}

func (p *panickingComplexOp) EqualTo(o values.Value) bool {
	v, ok := o.(*panickingComplexOp)
	return SameType(p, v, ok)
}

func (*panickingComplexOp) OpKind() OpCode {
	return OpComplex
}

func (p *panickingComplexOp) Apply(*MachineContext) (*MachineContext, error) {
	panic(p.panicValue)
}

// TestRunWithEscapeHandling_ContainsNonForeignError proves the recover contains
// a panic value that is an error but not a *werr.ForeignError: it is converted
// into a returned *SchemeError (carrying source + stack trace, errors.Is-chained
// to the original) rather than unwinding past the VM boundary into the embedder.
func TestRunWithEscapeHandling_ContainsNonForeignError(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	sentinel := errors.New("not a foreign error")
	op := &panickingComplexOp{
		OperationBase: NewOperationBase("test-panicking-complex-op"),
		panicValue:    sentinel,
	}
	tpl := NewNativeTemplate(0, 0, false, op)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))

	// Must not panic: the boundary contains the panic and returns it as an
	// error. A regression here re-panics, which fails the test directly.
	rerr := mc.RunWithEscapeHandling()

	c.Assert(rerr, qt.IsNotNil)
	// Contained as a *SchemeError (a VM/compiler bug, not a catchable condition).
	var schemeErr *SchemeError
	c.Assert(errors.As(rerr, &schemeErr), qt.IsTrue)
	// Not converted into a catchable Scheme exception.
	var escapeErr *ErrExceptionEscape
	c.Assert(errors.As(rerr, &escapeErr), qt.IsFalse)
	// The original panic value stays reachable for matching/diagnostics.
	c.Assert(errors.Is(rerr, sentinel), qt.IsTrue)
	// The rendered message surfaces the panic detail, not just a generic
	// "recovered panic" (SchemeError.Error() does not render Cause, so the
	// detail must be folded into the message at wrap time).
	c.Assert(strings.Contains(rerr.Error(), "not a foreign error"), qt.IsTrue,
		qt.Commentf("Error()=%q", rerr.Error()))
}

// TestMachineContext_ImmutableLiterals verifies the CallContext accessor
// returns the engine-scoped immutable-literal set when a namespace exists.
func TestMachineContext_ImmutableLiterals(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, nil, env)
	mc := NewMachineContext(context.Background(), cont)

	set := mc.ImmutableLiterals()
	if set == nil {
		t.Fatalf("MachineContext.ImmutableLiterals must be non-nil when a namespace exists")
	}
	if set != env.Namespace().ImmutableLiterals() {
		t.Fatalf("MachineContext.ImmutableLiterals must equal the namespace's set")
	}
}
