package machine

import (
	"context"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"

	qt "github.com/frankban/quicktest"
)

func TestApplyForeign_FixedArity(t *testing.T) {
	env := environment.NewNamespace().Runtime()
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
	env := environment.NewNamespace().Runtime()
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
	env := environment.NewNamespace().Runtime()
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

func TestApplyForeign_ValidatorCalled(t *testing.T) {
	c := qt.New(t)

	validatorCalls := 0
	fnCalls := 0

	env := environment.NewNamespace().Runtime()
	fn := func(mc *MachineContext) error {
		fnCalls++
		mc.SetValue(values.TrueValue)
		return nil
	}
	cls := newTestForeignClosure(env, 0, false, fn)
	cls.SetValidator(func(mc *MachineContext) error {
		validatorCalls++
		return nil
	})

	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	_, err := mc.applyForeign(cls)
	c.Assert(err, qt.IsNil)
	c.Assert(validatorCalls, qt.Equals, 1)
	c.Assert(fnCalls, qt.Equals, 1)
}

func TestApplyForeign_ValidatorRejectsCall(t *testing.T) {
	c := qt.New(t)

	fnCalls := 0

	env := environment.NewNamespace().Runtime()
	fn := func(mc *MachineContext) error {
		fnCalls++
		mc.SetValue(values.TrueValue)
		return nil
	}
	cls := newTestForeignClosure(env, 0, false, fn)
	cls.SetValidator(func(mc *MachineContext) error {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure, "validator rejected")
	})

	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	_, err := mc.applyForeign(cls)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "validator rejected")
	c.Assert(fnCalls, qt.Equals, 0, qt.Commentf("fn must not be called when validator rejects"))
}

// TestApplyForeign_PanicRecovery was removed: applyForeign no longer
// recovers panics. Division-by-zero and exactness-conversion errors are
// returned through normal error paths (Number.Divide and Number.ToExact
// return errors). Foreign functions must not panic.
