// Copyright 2025 Aaron Alpar
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
	"wile/environment"
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestNewMachineContext(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(3, 0, false)

	// Create a parent continuation to verify parent chain works
	parentCont := NewMachineContinuation(nil, nil, env)

	// Create a continuation with specific state
	cont := &MachineContinuation{
		parent:   parentCont,
		env:      env,
		template: tpl,
		value:    NewMultipleValues(values.NewInteger(42)),
		evals:    NewStack(),
		pc:       5,
	}
	cont.evals.Push(values.NewInteger(1))
	cont.evals.Push(values.NewInteger(2))

	// Create MachineContext from continuation
	mc := NewMachineContext(cont)

	// Verify all fields are correctly transferred
	qt.Assert(t, mc.env, qt.Equals, env)
	qt.Assert(t, mc.template, qt.Equals, tpl)
	qt.Assert(t, mc.cont, qt.Equals, parentCont) // cont field should be cont.parent
	qt.Assert(t, mc.pc, qt.Equals, 5)
	qt.Assert(t, mc.value.Length(), qt.Equals, 1)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
	qt.Assert(t, mc.evals.Length(), qt.Equals, 2)
}

func TestNewMachineContext_NilParent(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)

	// Create a continuation with nil parent
	cont := NewMachineContinuation(nil, nil, env)

	mc := NewMachineContext(cont)

	qt.Assert(t, mc.env, qt.Equals, env)
	qt.Assert(t, mc.cont, qt.IsNil) // nil parent means mc.cont should be nil
	qt.Assert(t, mc.pc, qt.Equals, 0)
}

func TestNewMachineContext_RoundTrip(t *testing.T) {
	// Test that saving and restoring a continuation preserves state
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(2, 0, false)

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(cont)

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
	mc2 := NewMachineContext(savedCont)

	// Verify the round-trip preserved state
	qt.Assert(t, mc2.env, qt.Equals, env)
	qt.Assert(t, mc2.template, qt.Equals, tpl)
	qt.Assert(t, mc2.pc, qt.Equals, 7)
}

func TestMachineContext_PushContinuation_0(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	cont := NewMachineContinuation(nil, nil, env)
	mc := NewMachineContext(cont)
	qt.Assert(t, mc.cont, qt.IsNil)
	qt.Assert(t, mc.cont.CallDepth(), qt.Equals, 0)
	qt.Assert(t, mc.cont, qt.IsNil)
	qt.Assert(t, mc.PC(), qt.Equals, 0)
	qt.Assert(t, mc.EnvironmentFrame(), values.SchemeEquals, mc.env)
	qt.Assert(t, mc.Template(), qt.IsNil)
	qt.Assert(t, mc.value.Length(), qt.Equals, 0)
	qt.Assert(t, mc.evals.Length(), qt.Equals, 0)
}

func TestMachineContext_PushContinuation_1(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	mc := NewMachineContext(NewMachineContinuation(nil, nil, env))
	mc.SaveContinuation(10)
	qt.Assert(t, mc.CallDepth(), qt.Equals, 1)
	qt.Assert(t, mc.Parent(), qt.IsNotNil)
	qt.Assert(t, mc.Parent().PC(), qt.Equals, 10)
	qt.Assert(t, mc.PC(), qt.Equals, 0)
	qt.Assert(t, mc.EnvironmentFrame(), values.SchemeEquals, mc.env)
	qt.Assert(t, mc.Template(), qt.IsNil)
	qt.Assert(t, mc.value.Length(), qt.Equals, 0)
	qt.Assert(t, mc.evals.Length(), qt.Equals, 0)
}

func TestMachineContext_PushContinuation_2(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	mc := NewMachineContext(NewMachineContinuation(nil, nil, env))
	bottom0 := mc.cont
	mc.SaveContinuation(10)
	bottom1 := mc.cont
	mc.SaveContinuation(20)
	bottom2 := mc.cont
	qt.Assert(t, mc.cont, qt.IsNotNil)
	qt.Assert(t, mc.CallDepth(), qt.Equals, 2)
	qt.Assert(t, mc.Parent(), values.SchemeEquals, bottom2)
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
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	mc := NewMachineContext(NewMachineContinuation(nil, nil, env))

	// Test SetValues and GetValues
	mc.SetValues(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	vs := mc.GetValues()
	qt.Assert(t, vs.Length(), qt.Equals, 3)
	qt.Assert(t, vs[0], values.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, vs[1], values.SchemeEquals, values.NewInteger(2))
	qt.Assert(t, vs[2], values.SchemeEquals, values.NewInteger(3))

	// GetValue returns first value
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(1))

	// Test empty values
	mc.SetValues()
	qt.Assert(t, mc.GetValues().Length(), qt.Equals, 0)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.Void)
}

func TestMachineContext_CurrentContinuation(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(2, 0, false)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))
	mc.SaveContinuation(5)
	mc.evals.Push(values.NewInteger(42))
	mc.SetValue(values.NewInteger(100))

	curr := mc.CurrentContinuation()

	// Should be a copy of the parent continuation
	qt.Assert(t, curr, qt.IsNotNil)
	qt.Assert(t, curr != mc.cont, qt.IsTrue) // Different object
}

func TestMachineContext_NewSubContext(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrame(lenv, genv)
	tpl := NewNativeTemplate(2, 0, false)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))
	mc.SetValue(values.NewInteger(42))
	mc.evals.Push(values.NewInteger(100))
	mc.SaveContinuation(5)

	subCtx := mc.NewSubContext()

	// Sub-context should have fresh state
	qt.Assert(t, subCtx.template, qt.IsNil)
	qt.Assert(t, subCtx.pc, qt.Equals, 0)
	qt.Assert(t, subCtx.value, qt.IsNil)
	qt.Assert(t, subCtx.evals.Length(), qt.Equals, 0)
	qt.Assert(t, subCtx.cont, qt.IsNil)
	// But shares top-level environment
	qt.Assert(t, subCtx.env, qt.Equals, env.TopLevel())
}

func TestMachineContext_Restore(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env1 := environment.NewEnvironmentFrame(nil, genv)
	env2 := environment.NewEnvironmentFrame(nil, genv)
	tpl1 := NewNativeTemplate(1, 0, false)
	tpl2 := NewNativeTemplate(2, 0, false)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl1, env1))
	mc.pc = 5
	mc.evals.Push(values.NewInteger(1))

	// Create a continuation with different state
	parent := NewMachineContinuation(nil, nil, env2)
	cont2 := &MachineContinuation{
		parent:   parent,
		env:      env2,
		template: tpl2,
		evals:    NewStack(),
		pc:       10,
	}
	cont2.evals.Push(values.NewInteger(42))

	mc.Restore(cont2)

	qt.Assert(t, mc.env, qt.Equals, env2)
	qt.Assert(t, mc.template, qt.Equals, tpl2)
	qt.Assert(t, mc.pc, qt.Equals, 10)
	qt.Assert(t, mc.cont, qt.Equals, parent)
	qt.Assert(t, mc.evals.Length(), qt.Equals, 1)
}

func TestMachineContext_Apply_FixedArity(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrame(lenv, genv)
	tpl := NewNativeTemplate(2, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(NewMachineContinuation(nil, nil, env))

	// Apply with correct argument count
	_, err := mc.Apply(cls, values.NewInteger(10), values.NewInteger(20))
	qt.Assert(t, err, qt.IsNil)

	// Check bindings were set in the NEW call environment (not the closure's env)
	// Apply now creates a fresh environment for each call to support recursion
	bnds := mc.env.LocalEnvironment().Bindings()
	qt.Assert(t, bnds[0].Value(), values.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, bnds[1].Value(), values.SchemeEquals, values.NewInteger(20))

	// Check context was updated
	qt.Assert(t, mc.template, qt.Equals, tpl)
	// mc.env is now a fresh environment with copied local bindings
	qt.Assert(t, mc.env.Parent(), qt.Equals, env.Parent())
	qt.Assert(t, mc.pc, qt.Equals, 0)
}

func TestMachineContext_Apply_WrongArgCount(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrame(lenv, genv)
	tpl := NewNativeTemplate(2, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(NewMachineContinuation(nil, nil, env))

	// Apply with wrong argument count
	_, err := mc.Apply(cls, values.NewInteger(10))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "expected 2 arguments")
}

func TestMachineContext_Apply_Variadic(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	lenv := environment.NewLocalEnvironment(3)
	env := environment.NewEnvironmentFrame(lenv, genv)
	// Variadic with 2 required parameters plus rest
	tpl := NewNativeTemplate(3, 0, true)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(NewMachineContinuation(nil, nil, env))

	// Apply with extra args for rest parameter
	_, err := mc.Apply(cls, values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4))
	qt.Assert(t, err, qt.IsNil)

	// Check bindings in the NEW call environment (not the closure's env)
	bnds := mc.env.LocalEnvironment().Bindings()
	qt.Assert(t, bnds[0].Value(), values.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, bnds[1].Value(), values.SchemeEquals, values.NewInteger(2))
	// Rest parameter should be a list
	rest := bnds[2].Value()
	qt.Assert(t, rest, values.SchemeEquals, values.List(values.NewInteger(3), values.NewInteger(4)))
}

func TestMachineContext_Apply_VariadicTooFewArgs(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	lenv := environment.NewLocalEnvironment(3)
	env := environment.NewEnvironmentFrame(lenv, genv)
	tpl := NewNativeTemplate(3, 0, true)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContext(NewMachineContinuation(nil, nil, env))

	// Apply with too few args
	_, err := mc.Apply(cls, values.NewInteger(1))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "expected at least 2 arguments")
}

func TestMachineContext_ApplyCaseLambda(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()

	// Create two clauses with different arities
	lenv1 := environment.NewLocalEnvironment(1)
	env1 := environment.NewEnvironmentFrame(lenv1, genv)
	tpl1 := NewNativeTemplate(1, 0, false)
	cls1 := NewClosureWithTemplate(tpl1, env1)

	lenv2 := environment.NewLocalEnvironment(2)
	env2 := environment.NewEnvironmentFrame(lenv2, genv)
	tpl2 := NewNativeTemplate(2, 0, false)
	cls2 := NewClosureWithTemplate(tpl2, env2)

	caseLambda := NewCaseLambdaClosure([]*MachineClosure{cls1, cls2})

	env := environment.NewEnvironmentFrame(nil, genv)
	mc := NewMachineContext(NewMachineContinuation(nil, nil, env))

	// Apply with 1 arg - should use cls1
	_, err := mc.ApplyCaseLambda(caseLambda, values.NewInteger(42))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.template, qt.Equals, tpl1)

	// Reset context
	mc = NewMachineContext(NewMachineContinuation(nil, nil, env))

	// Apply with 2 args - should use cls2
	_, err = mc.ApplyCaseLambda(caseLambda, values.NewInteger(1), values.NewInteger(2))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.template, qt.Equals, tpl2)
}

func TestMachineContext_ApplyCaseLambda_NoMatch(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()

	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrame(lenv, genv)
	tpl := NewNativeTemplate(2, 0, false)
	cls := NewClosureWithTemplate(tpl, env)

	caseLambda := NewCaseLambdaClosure([]*MachineClosure{cls})

	mc := NewMachineContext(NewMachineContinuation(nil, nil, env))

	// Apply with wrong number of args
	_, err := mc.ApplyCaseLambda(caseLambda, values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "no matching clause")
}

func TestErrContinuationEscape_Error(t *testing.T) {
	err := &ErrContinuationEscape{
		Continuation: nil,
		Value:        values.NewInteger(42),
		Handled:      false,
	}
	qt.Assert(t, err.Error(), qt.Equals, "continuation escape")
}

func TestNewMachineContextFromMachineClosure(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	lenv := environment.NewLocalEnvironment(2)
	env := environment.NewEnvironmentFrame(lenv, genv)
	tpl := NewNativeTemplate(2, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	mc := NewMachineContextFromMachineClosure(cls)

	qt.Assert(t, mc.template, qt.Equals, tpl)
	qt.Assert(t, mc.env, qt.Equals, env)
	qt.Assert(t, mc.pc, qt.Equals, 0)
	qt.Assert(t, mc.cont, qt.IsNil)
}

func TestMachineContext_Error(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.SetName("test-func")

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	err := mc.Error("test error message")

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Message, qt.Equals, "test error message")
	// Error should be a SchemeError
	_, ok := interface{}(err).(*SchemeError)
	qt.Assert(t, ok, qt.IsTrue)
}

func TestMachineContext_Error_NoSource(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)

	// No template means no source
	mc := NewMachineContext(NewMachineContinuation(nil, nil, env))

	err := mc.Error("no source error")

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Message, qt.Equals, "no source error")
	qt.Assert(t, err.Source, qt.IsNil)
}

func TestMachineContext_WrapError(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	cause := values.NewForeignError("original error")
	err := mc.WrapError(cause, "wrapped message")

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Message, qt.Equals, "wrapped message")
	qt.Assert(t, err.Cause, qt.Equals, cause)
}

func TestMachineContext_WrapError_EmptyMessage(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	cause := values.NewForeignError("original error")
	err := mc.WrapError(cause, "")

	qt.Assert(t, err, qt.IsNotNil)
	// Empty message should use cause's message
	qt.Assert(t, err.Message, qt.Equals, "original error")
	qt.Assert(t, err.Cause, qt.Equals, cause)
}
