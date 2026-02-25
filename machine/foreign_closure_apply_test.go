package machine

import (
	"context"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestApplyForeign_FixedArity(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	fn := func(mc *MachineContext) error {
		bnds := mc.EnvironmentFrame().LocalEnvironment().Bindings()
		a := bnds[0].Value().(*values.Integer).Value
		b := bnds[1].Value().(*values.Integer).Value
		mc.SetValue(values.NewInteger(a + b))
		return nil
	}
	cls := newTestForeignClosure(env, 2, false, fn)

	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	result, err := mc.applyForeign(cls, values.NewInteger(10), values.NewInteger(32))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.GetValue(), qt.DeepEquals, values.NewInteger(42))
}

func TestApplyForeign_Variadic(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	// (lambda (x . rest) rest) — returns the rest arg list
	fn := func(mc *MachineContext) error {
		bnds := mc.EnvironmentFrame().LocalEnvironment().Bindings()
		mc.SetValue(bnds[1].Value())
		return nil
	}
	cls := newTestForeignClosure(env, 2, true, fn)

	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	result, err := mc.applyForeign(cls, values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	qt.Assert(t, err, qt.IsNil)
	// rest = (2 3)
	qt.Assert(t, result.GetValue().(values.Tuple).SchemeString(), qt.Equals, "(2 3)")
}

func TestApplyForeign_ArityError(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	fn := func(mc *MachineContext) error {
		return nil
	}
	cls := newTestForeignClosure(env, 2, false, fn)

	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	_, err := mc.applyForeign(cls, values.NewInteger(1))
	qt.Assert(t, err, qt.IsNotNil)
}

func TestApplyForeign_PanicRecovery(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	fn := func(mc *MachineContext) error {
		panic(values.WrapForeignErrorf(values.ErrDivisionByZero, "test panic"))
	}
	cls := newTestForeignClosure(env, 0, false, fn)

	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	_, err := mc.applyForeign(cls)
	qt.Assert(t, err, qt.IsNotNil)
	// Should be converted to ErrExceptionEscape, not a raw panic
	_, ok := err.(*ErrExceptionEscape)
	qt.Assert(t, ok, qt.IsTrue)
}
