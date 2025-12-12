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
	"context"
	"wile/environment"
	"wile/values"
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
				mc.env.LocalEnvironment().SetLocalValue(li, values.NewInteger(10)) //nolint:errcheck
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
				v := mc.env.GlobalEnvironment().GetOwnGlobalBinding(gi).Value()
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
				genv := environment.NewTopLevelGlobalEnvironmentFrame()
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
				genv := environment.NewTopLevelGlobalEnvironmentFrame()
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
				genv := environment.NewTopLevelGlobalEnvironmentFrame()
				lenv := environment.NewLocalEnvironment(0)
				mc.env = environment.NewEnvironmentFrame(lenv, genv)
				mc.template = NewNativeTemplate(0, 0, false)
				mc.SaveContinuation(1)
				genv = environment.NewTopLevelGlobalEnvironmentFrame()
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
			genv := environment.NewTopLevelGlobalEnvironmentFrame()
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

// TestOperationValueMethods tests the IsVoid and EqualTo methods of various operations
func TestOperationValueMethods(t *testing.T) {
	// Test Push
	t.Run("Push", func(t *testing.T) {
		op1 := NewOperationPush()
		op2 := NewOperationPush()
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
	})

	// Test Pop
	t.Run("Pop", func(t *testing.T) {
		op1 := NewOperationPop()
		op2 := NewOperationPop()
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
	})

	// Test PopAll
	t.Run("PopAll", func(t *testing.T) {
		op1 := NewOperationPopAll()
		op2 := NewOperationPopAll()
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
	})

	// Test Pull
	t.Run("Pull", func(t *testing.T) {
		op1 := NewOperationPull()
		op2 := NewOperationPull()
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
	})

	// Test BranchOffsetImmediate
	t.Run("BranchOffsetImmediate", func(t *testing.T) {
		op1 := NewOperationBranchOffsetImmediate(5)
		op2 := NewOperationBranchOffsetImmediate(5)
		op3 := NewOperationBranchOffsetImmediate(10)
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
	})

	// Test BranchOnFalseOffsetImmediate
	t.Run("BranchOnFalseOffsetImmediate", func(t *testing.T) {
		op1 := NewOperationBranchOnFalseOffsetImmediate(5)
		op2 := NewOperationBranchOnFalseOffsetImmediate(5)
		op3 := NewOperationBranchOnFalseOffsetImmediate(10)
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
	})

	// Test BranchOnNotFalseOffsetImmediate
	t.Run("BranchOnNotFalseOffsetImmediate", func(t *testing.T) {
		op1 := NewOperationBranchOnNotFalseOffsetImmediate(5)
		op2 := NewOperationBranchOnNotFalseOffsetImmediate(5)
		op3 := NewOperationBranchOnNotFalseOffsetImmediate(10)
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
	})

	// Test LoadLiteralByLiteralIndexImmediate
	t.Run("LoadLiteralByLiteralIndexImmediate", func(t *testing.T) {
		op1 := NewOperationLoadLiteralByLiteralIndexImmediate(0)
		op2 := NewOperationLoadLiteralByLiteralIndexImmediate(0)
		op3 := NewOperationLoadLiteralByLiteralIndexImmediate(1)
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	})

	// Test LoadLocalByLocalIndexImmediate
	t.Run("LoadLocalByLocalIndexImmediate", func(t *testing.T) {
		idx1 := environment.NewLocalIndex(0, 0)
		idx2 := environment.NewLocalIndex(0, 1)
		op1 := NewOperationLoadLocalByLocalIndexImmediate(idx1)
		op2 := NewOperationLoadLocalByLocalIndexImmediate(idx1)
		op3 := NewOperationLoadLocalByLocalIndexImmediate(idx2)
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	})

	// Test StoreLocalByLocalIndexImmediate
	t.Run("StoreLocalByLocalIndexImmediate", func(t *testing.T) {
		idx1 := environment.NewLocalIndex(0, 0)
		idx2 := environment.NewLocalIndex(0, 1)
		op1 := NewOperationStoreLocalByLocalIndexImmediate(idx1)
		op2 := NewOperationStoreLocalByLocalIndexImmediate(idx1)
		op3 := NewOperationStoreLocalByLocalIndexImmediate(idx2)
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	})

	// Test LoadGlobalByGlobalIndexLiteralIndexImmediate
	t.Run("LoadGlobalByGlobalIndexLiteralIndexImmediate", func(t *testing.T) {
		op1 := NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0)
		op2 := NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0)
		op3 := NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(1)
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	})

	// Test StoreGlobalByGlobalIndexLiteralIndexImmediate
	t.Run("StoreGlobalByGlobalIndexLiteralIndexImmediate", func(t *testing.T) {
		op1 := NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(0)
		op2 := NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(0)
		op3 := NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(1)
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	})

	// Test PeekK
	t.Run("PeekK", func(t *testing.T) {
		op1 := NewOperationPeekK(0)
		op2 := NewOperationPeekK(0)
		op3 := NewOperationPeekK(1)
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	})

	// Test MakeClosure
	t.Run("MakeClosure", func(t *testing.T) {
		op1 := NewOperationMakeClosure()
		op2 := NewOperationMakeClosure()
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
	})

	// Test SaveContinuationOffsetImmediate
	t.Run("SaveContinuationOffsetImmediate", func(t *testing.T) {
		op1 := NewOperationSaveContinuationOffsetImmediate(5)
		op2 := NewOperationSaveContinuationOffsetImmediate(5)
		op3 := NewOperationSaveContinuationOffsetImmediate(10)
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	})

	// Test RestoreContinuation
	t.Run("RestoreContinuation", func(t *testing.T) {
		op1 := NewOperationRestoreContinuation()
		op2 := NewOperationRestoreContinuation()
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		// RestoreContinuation uses pointer equality in EqualTo, so different instances are not equal
		qt.Assert(t, op1.EqualTo(op1), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(op2), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
	})

	// Test Apply
	t.Run("Apply", func(t *testing.T) {
		op1 := NewOperationApply()
		op2 := NewOperationApply()
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
	})

	// Test LoadVoid
	t.Run("LoadVoid", func(t *testing.T) {
		op1 := NewOperationLoadVoid()
		op2 := NewOperationLoadVoid()
		qt.Assert(t, op1.IsVoid(), qt.IsFalse)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
		qt.Assert(t, op1.SchemeString(), qt.Equals, "#<machine-operation-load-void>")
		qt.Assert(t, op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
	})
}

func TestOperations_SchemeString(t *testing.T) {
	ops := NewOperations(
		NewOperationPush(),
		NewOperationPop(),
		NewOperationLoadVoid())

	str := ops.SchemeString()
	qt.Assert(t, str, qt.Equals, "#<machine-operations>")
}

func TestOperations_Copy(t *testing.T) {
	ops := NewOperations(
		NewOperationPush(),
		NewOperationPop(),
		NewOperationLoadVoid())

	copied := ops.Copy()
	qt.Assert(t, copied, qt.IsNotNil)
	qt.Assert(t, copied.Length(), qt.Equals, 3)
	qt.Assert(t, copied[0], values.SchemeEquals, ops[0])
	qt.Assert(t, copied[1], values.SchemeEquals, ops[1])
	qt.Assert(t, copied[2], values.SchemeEquals, ops[2])
}

func TestOperations_EqualTo(t *testing.T) {
	ops1 := NewOperations(NewOperationPush(), NewOperationPop())
	ops2 := NewOperations(NewOperationPush(), NewOperationPop())
	ops3 := NewOperations(NewOperationPush())
	ops4 := NewOperations(NewOperationPop(), NewOperationPush())

	// Same operations in same order
	qt.Assert(t, ops1.EqualTo(ops2), qt.IsTrue)

	// Different length
	qt.Assert(t, ops1.EqualTo(ops3), qt.IsFalse)

	// Same operations in different order
	qt.Assert(t, ops1.EqualTo(ops4), qt.IsFalse)

	// Different type
	qt.Assert(t, ops1.EqualTo(values.NewInteger(42)), qt.IsFalse)

	// Nil operations
	var nilOps1 Operations
	var nilOps2 Operations
	qt.Assert(t, nilOps1.EqualTo(nilOps2), qt.IsTrue)
	qt.Assert(t, ops1.EqualTo(nilOps1), qt.IsFalse)
}

func TestOperations_IsVoid(t *testing.T) {
	ops := NewOperations(NewOperationPush())
	qt.Assert(t, ops.IsVoid(), qt.IsFalse)

	var nilOps Operations
	qt.Assert(t, nilOps.IsVoid(), qt.IsTrue)
}

func TestOperations_Length(t *testing.T) {
	ops := NewOperations(NewOperationPush(), NewOperationPop(), NewOperationLoadVoid())
	qt.Assert(t, ops.Length(), qt.Equals, 3)

	emptyOps := NewOperations()
	qt.Assert(t, emptyOps.Length(), qt.Equals, 0)
}
