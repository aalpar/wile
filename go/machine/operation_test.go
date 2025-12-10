package machine

import (
	"context"
	"skeme/environment"
	"skeme/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestOperation(t *testing.T) {
	tcs := []struct {
		evals   *Stack
		value   []values.Value
		op      Operation
		setupFn func(t *testing.T, mc *MachineContext)
		checkFn func(t *testing.T, mc *MachineContext)
	}{
		{
			value: []values.Value{values.NewInteger(10)},
			op:    NewOperationPush(),
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, (*mc.evals), qt.HasLen, 1)
				qt.Assert(t, (*mc.evals)[0], values.SchemeEquals, values.NewInteger(10))
			},
		},
		{
			evals: &Stack{values.NewInteger(10)},
			op:    NewOperationPop(),
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, *mc.evals, qt.HasLen, 0)
				qt.Assert(t, mc.value, qt.HasLen, 1)
				qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(10))
			},
		},
		{
			evals: &Stack{
				values.NewInteger(10),
				values.NewInteger(20),
				values.NewInteger(30),
				values.NewInteger(40),
				values.NewInteger(50),
			},
			op: NewOperationPopAll(),
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, *mc.evals, qt.HasLen, 0)
				qt.Assert(t, mc.value, qt.HasLen, 5)
				qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(10))
				qt.Assert(t, mc.value[1], values.SchemeEquals, values.NewInteger(20))
				qt.Assert(t, mc.value[2], values.SchemeEquals, values.NewInteger(30))
				qt.Assert(t, mc.value[3], values.SchemeEquals, values.NewInteger(40))
				qt.Assert(t, mc.value[4], values.SchemeEquals, values.NewInteger(50))
			},
		},
		{
			op: NewOperationBranchOffsetImmediate(2),
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 2)
			},
		},
		{
			evals: NewStack(
				values.NewBoolean(false),
			),
			op: NewOperationBranchOnFalseOffsetImmediate(2),
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 2)
			},
		},
		{
			evals: NewStack(
				values.NewBoolean(true),
			),
			op: NewOperationBranchOnFalseOffsetImmediate(2),
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
			},
		},
		{
			evals: NewStack(
				values.NewBoolean(true),
			),
			op: NewOperationBranchOnNotFalseOffsetImmediate(2),
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 2)
			},
		},
		{
			evals: NewStack(
				values.NewBoolean(false),
			),
			op: NewOperationBranchOnNotFalseOffsetImmediate(2),
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
			},
		},
		{
			op: NewOperationLoadLiteralByLiteralIndexImmediate(0),
			setupFn: func(t *testing.T, mc *MachineContext) {
				mc.template.MaybeAppendLiteral(values.NewInteger(10))
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.value, qt.HasLen, 1)
				qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(10))
			},
		},
		{
			op: NewOperationLoadLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
			setupFn: func(t *testing.T, mc *MachineContext) {
				li, _ := mc.env.LocalEnvironment().CreateLocalBinding(mc.env.InternSymbol(values.NewSymbol("foo")), environment.BindingTypeVariable)
				mc.env.LocalEnvironment().SetLocalValue(li, values.NewInteger(10))
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.value, qt.HasLen, 1)
				qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(10))
			},
		},
		{
			op:    NewOperationStoreLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
			evals: NewStack(values.NewInteger(10)),
			setupFn: func(t *testing.T, mc *MachineContext) {
				sym := mc.env.InternSymbol(values.NewSymbol("foo"))
				mc.env.LocalEnvironment().CreateLocalBinding(sym, environment.BindingTypeVariable)
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.value, qt.HasLen, 0)
				qt.Assert(t, *mc.evals, qt.HasLen, 0)
				sym := mc.env.InternSymbol(values.NewSymbol("foo"))
				li := mc.env.LocalEnvironment().GetLocalIndex(sym)
				bd := mc.env.LocalEnvironment().GetLocalBinding(li)
				qt.Assert(t, bd.Value(), values.SchemeEquals, values.NewInteger(10))
			},
		},
		{
			op: NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0),
			setupFn: func(t *testing.T, mc *MachineContext) {
				sym := mc.env.InternSymbol(values.NewSymbol("foo"))
				gi, ok := mc.env.GlobalEnvironment().CreateGlobalBinding(sym, environment.BindingTypeVariable)
				qt.Assert(t, ok, qt.IsTrue)
				mc.template.MaybeAppendLiteral(gi)
				err := mc.env.GlobalEnvironment().SetGlobalValue(gi, values.NewInteger(10))
				qt.Assert(t, err, qt.IsNil)
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.value, qt.HasLen, 1)
				qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(10))
				qt.Assert(t, *mc.evals, qt.HasLen, 0)
			},
		},
		{
			op:    NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(0),
			evals: NewStack(values.NewInteger(10)),
			setupFn: func(t *testing.T, mc *MachineContext) {
				sym := mc.env.InternSymbol(values.NewSymbol("foo"))
				gi, ok := mc.env.GlobalEnvironment().CreateGlobalBinding(sym, environment.BindingTypeVariable)
				qt.Assert(t, ok, qt.IsTrue)
				mc.template.MaybeAppendLiteral(gi)
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.value, qt.HasLen, 0)
				sym := mc.env.InternSymbol(values.NewSymbol("foo"))
				gi := mc.env.GlobalEnvironment().GetGlobalIndex(sym)
				v := mc.env.GlobalEnvironment().GetGlobalBinding(gi).Value()
				qt.Assert(t, v, values.SchemeEquals, values.NewInteger(10))
			},
		},
		{
			op: NewOperationPeekK(1),
			evals: NewStack(
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
			),
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.value, qt.HasLen, 1)
				qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(2))
			},
		},
		{
			op: NewOperationMakeClosure(),
			setupFn: func(t *testing.T, mc *MachineContext) {
				genv := environment.NewTopLevelGlobalEnvironment()
				lenv := environment.NewLocalEnvironment(0)
				env := environment.NewEnvironmentFrame(lenv, genv)
				tpl := NewNativeTemplate(0, 0, false)
				mc.evals.PushAll([]values.Value{tpl, env})
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				// FIXME: this is 0 because we are not actually calling a function here.
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.value, qt.HasLen, 1)
				mcls, ok := mc.value[0].(*MachineClosure)
				qt.Assert(t, ok, qt.IsTrue)
				qt.Assert(t, mcls, qt.IsNotNil)
				qt.Assert(t, *mc.evals, qt.HasLen, 0)
				lenv := mcls.env.LocalEnvironment()
				genv := mcls.env.GlobalEnvironment()
				tpl := mcls.Template()
				qt.Assert(t, tpl, qt.IsNotNil)
				qt.Assert(t, lenv, qt.IsNotNil)
				qt.Assert(t, genv, qt.IsNotNil)
			},
		},
		{
			op: NewOperationSaveContinuationOffsetImmediate(1),
			setupFn: func(t *testing.T, mc *MachineContext) {
				genv := environment.NewTopLevelGlobalEnvironment()
				lenv := environment.NewLocalEnvironment(0)
				tpl := NewNativeTemplate(0, 0, false)
				mc.env = environment.NewEnvironmentFrame(lenv, genv)
				mc.template = tpl
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 0)
				qt.Assert(t, mc.value, qt.HasLen, 0)
				qt.Assert(t, *mc.evals, qt.HasLen, 0)
				// FIXME: this is -1 because we are not actually calling a function here.
				qt.Assert(t, mc.cont.CallDepth(), qt.Equals, 0)
			},
		},
		{
			op: NewOperationRestoreContinuation(),
			setupFn: func(t *testing.T, mc *MachineContext) {
				genv := environment.NewTopLevelGlobalEnvironment()
				lenv := environment.NewLocalEnvironment(0)
				mc.env = environment.NewEnvironmentFrame(lenv, genv)
				mc.template = NewNativeTemplate(0, 0, false)
				mc.SaveContinuation(1)
				genv = environment.NewTopLevelGlobalEnvironment()
				lenv = environment.NewLocalEnvironment(0)
				mc.env = environment.NewEnvironmentFrameWithParent(lenv, mc.env)
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 0)
				qt.Assert(t, mc.value, qt.HasLen, 0)
				qt.Assert(t, *mc.evals, qt.HasLen, 0)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.op.SchemeString(), func(t *testing.T) {
			if tc.evals == nil {
				tc.evals = NewStack()
			}
			genv := environment.NewTopLevelGlobalEnvironment()
			lenv := environment.NewLocalEnvironment(0)
			env := environment.NewEnvironmentFrame(lenv, genv)
			tpl := NewNativeTemplate(0, 0, false, tc.op)
			mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))
			mc.evals = tc.evals
			mc.value = tc.value
			if tc.setupFn != nil {
				tc.setupFn(t, mc)
			}
			ctx := context.Background()
			err := mc.Run(ctx)
			qt.Assert(t, err, qt.IsNil)
			tc.checkFn(t, mc)
		})
	}
}
