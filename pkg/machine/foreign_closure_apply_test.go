package machine

import (
	"context"
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

func TestApplyForeign_FixedArity(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	fn := func(mc CallContext) error {
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
	fn := func(mc CallContext) error {
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
	fn := func(mc CallContext) error {
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
	validatorCalls := 0
	fnCalls := 0

	env := environment.NewNamespace().Runtime()
	fn := func(mc CallContext) error {
		fnCalls++
		mc.SetValue(values.TrueValue)
		return nil
	}
	cls := newTestForeignClosure(env, 0, false, fn)
	cls.SetValidator(func(mc CallContext) error {
		validatorCalls++
		return nil
	})

	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	_, err := mc.applyForeign(cls)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, validatorCalls, qt.Equals, 1)
	qt.Assert(t, fnCalls, qt.Equals, 1)
}

func TestApplyForeign_ValidatorRejectsCall(t *testing.T) {
	fnCalls := 0

	env := environment.NewNamespace().Runtime()
	fn := func(mc CallContext) error {
		fnCalls++
		mc.SetValue(values.TrueValue)
		return nil
	}
	cls := newTestForeignClosure(env, 0, false, fn)
	cls.SetValidator(func(mc CallContext) error {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure, "validator rejected")
	})

	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	_, err := mc.applyForeign(cls)
	qt.Assert(t, err, qt.IsNotNil)

	var excErr *ErrExceptionEscape
	qt.Assert(t, errors.As(err, &excErr), qt.IsTrue,
		qt.Commentf("validator error must be wrapped as ErrExceptionEscape"))
	qt.Assert(t, fnCalls, qt.Equals, 0,
		qt.Commentf("fn must not be called when validator rejects"))
}

// TestApplyForeign_PanicRecovery was removed: applyForeign no longer
// recovers panics. Division-by-zero and exactness-conversion errors are
// returned through normal error paths (Number.Divide and Number.ToExact
// return errors). Foreign functions must not panic.
