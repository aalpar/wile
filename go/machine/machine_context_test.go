package machine

import (
	"wile/environment"
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestNewMachineContext(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironment()
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
	genv := environment.NewTopLevelGlobalEnvironment()
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
	genv := environment.NewTopLevelGlobalEnvironment()
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
	genv := environment.NewTopLevelGlobalEnvironment()
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
	genv := environment.NewTopLevelGlobalEnvironment()
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
	genv := environment.NewTopLevelGlobalEnvironment()
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
