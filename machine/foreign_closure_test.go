package machine

import (
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestForeignClosure_Callable(t *testing.T) {
	// Verify ForeignClosure implements values.Callable.
	var _ values.Callable = (*ForeignClosure)(nil)
}

func TestForeignClosure_ClosureInterface(t *testing.T) {
	// Verify both closure types implement Closure.
	var _ Closure = (*ForeignClosure)(nil)
	var _ Closure = (*MachineClosure)(nil)
}

func TestForeignClosure_SchemeString(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	fn := func(mc *MachineContext) error {
		return nil
	}
	cls := newTestForeignClosure(env, 2, false, fn)
	qt.Assert(t, cls.SchemeString(), qt.Equals, "#<foreign-closure>")
}

func TestForeignClosure_IsVoid(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	fn := func(mc *MachineContext) error {
		return nil
	}
	cls := newTestForeignClosure(env, 1, false, fn)
	qt.Assert(t, cls.IsVoid(), qt.IsFalse)

	var nilCls *ForeignClosure
	qt.Assert(t, nilCls.IsVoid(), qt.IsTrue)
}

func TestForeignClosure_AcceptsArity(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	fn := func(mc *MachineContext) error {
		return nil
	}

	tests := []struct {
		name     string
		params   int
		variadic bool
		n        int
		want     bool
	}{
		{"fixed-2 accepts 2", 2, false, 2, true},
		{"fixed-2 rejects 1", 2, false, 1, false},
		{"fixed-2 rejects 3", 2, false, 3, false},
		{"fixed-0 accepts 0", 0, false, 0, true},
		{"fixed-0 rejects 1", 0, false, 1, false},
		{"variadic-1 accepts 0", 1, true, 0, true},
		{"variadic-1 accepts 1", 1, true, 1, true},
		{"variadic-1 accepts 5", 1, true, 5, true},
		{"variadic-3 rejects 1", 3, true, 1, false},
		{"variadic-3 accepts 2", 3, true, 2, true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			cls := newTestForeignClosure(env, tc.params, tc.variadic, fn)
			qt.Assert(t, cls.AcceptsArity(tc.n), qt.Equals, tc.want)
		})
	}
}

func TestForeignClosure_EqualTo(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	fn := func(mc *MachineContext) error {
		return nil
	}

	cls1 := newTestForeignClosure(env, 1, false, fn)
	cls2 := newTestForeignClosure(env, 1, false, fn)
	cls3 := cls1

	// Same pointer
	qt.Assert(t, cls1.EqualTo(cls3), qt.IsTrue)
	// Different objects — identity semantics
	qt.Assert(t, cls1.EqualTo(cls2), qt.IsFalse)
	// Wrong type
	qt.Assert(t, cls1.EqualTo(values.NewInteger(42)), qt.IsFalse)
	// Nil
	var nilCls *ForeignClosure
	qt.Assert(t, nilCls.EqualTo(nilCls), qt.IsTrue)
	qt.Assert(t, cls1.EqualTo(nilCls), qt.IsFalse)
}

// newTestForeignClosure is a test helper that directly constructs a ForeignClosure
// without going through the public NewForeignClosure (which will be changed
// in a later task). This isolates the struct/method tests from the constructor.
func newTestForeignClosure(env *environment.EnvironmentFrame, pcnt int, variadic bool, fn ForeignFunction) *ForeignClosure {
	lenv := environment.NewLocalEnvironment(pcnt)
	env = environment.NewEnvironmentFrameWithParent(lenv, env)
	return &ForeignClosure{
		fn:         fn,
		env:        env,
		paramCount: pcnt,
		isVariadic: variadic,
	}
}
