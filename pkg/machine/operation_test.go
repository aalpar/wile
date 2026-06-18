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
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestOperation(t *testing.T) {
	tcs := []struct {
		evals       *Stack
		singleValue values.Value
		multiValues MultipleValues
		op          Operation
		setupFn     func(t *testing.T, mc *MachineContext)
		checkFn     func(t *testing.T, mc *MachineContext)
	}{
		{
			singleValue: values.NewInteger(10),
			op:          NewOperationPush(),
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, (*mc.evals), qt.HasLen, 1)
				qt.Assert(t, (*mc.evals)[0], valuestest.SchemeEquals, values.NewInteger(10))
			},
		},
		{
			evals: &Stack{values.NewInteger(10)},
			op:    NewOperationPop(),
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, *mc.evals, qt.HasLen, 0)
				qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
				qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(10))
			},
		},
		{
			op: NewOperationBranchOffsetImmediate(2),
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 2)
			},
		},
		{
			op: NewOperationLoadLiteralByLiteralIndexImmediate(0),
			setupFn: func(t *testing.T, mc *MachineContext) {
				mc.template.MaybeAppendLiteral(values.NewInteger(10))
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
				qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(10))
			},
		},
		{
			op: NewOperationLoadLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
			setupFn: func(t *testing.T, mc *MachineContext) {
				li, _ := mc.env.LocalEnvironment().EnsureLocalBinding(values.NewSymbol("bindSymbolWithScopes"), environment.BindingTypeVariable)
				mc.env.LocalEnvironment().SetLocalValue(li, values.NewInteger(10)) //nolint:errcheck
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
				qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(10))
			},
		},
		{
			op:    NewOperationStoreLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
			evals: NewStack(values.NewInteger(10)),
			setupFn: func(t *testing.T, mc *MachineContext) {
				sym := values.NewSymbol("bindSymbolWithScopes")
				mc.env.LocalEnvironment().EnsureLocalBinding(sym, environment.BindingTypeVariable)
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.GetValues(), qt.HasLen, 0)
				qt.Assert(t, *mc.evals, qt.HasLen, 0)
				sym := values.NewSymbol("bindSymbolWithScopes")
				li := mc.env.LocalEnvironment().GetLocalIndex(sym)
				bd := mc.env.LocalEnvironment().GetLocalBinding(li)
				qt.Assert(t, bd.Value(), valuestest.SchemeEquals, values.NewInteger(10))
			},
		},
		{
			op: NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0),
			setupFn: func(t *testing.T, mc *MachineContext) {
				sym := values.NewSymbol("bindSymbolWithScopes")
				gi, ok := mc.env.GlobalEnvironment().CreateGlobalBinding(sym, environment.BindingTypeVariable)
				qt.Assert(t, ok, qt.IsTrue)
				mc.template.MaybeAppendLiteral(gi)
				err := mc.env.GlobalEnvironment().SetOwnGlobalValue(gi, values.NewInteger(10))
				qt.Assert(t, err, qt.IsNil)
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
				qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(10))
				qt.Assert(t, *mc.evals, qt.HasLen, 0)
			},
		},
		{
			op:    NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(0),
			evals: NewStack(values.NewInteger(10)),
			setupFn: func(t *testing.T, mc *MachineContext) {
				sym := values.NewSymbol("bindSymbolWithScopes")
				gi, ok := mc.env.GlobalEnvironment().CreateGlobalBinding(sym, environment.BindingTypeVariable)
				qt.Assert(t, ok, qt.IsTrue)
				mc.template.MaybeAppendLiteral(gi)
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.GetValues(), qt.HasLen, 0)
				sym := values.NewSymbol("bindSymbolWithScopes")
				gi := mc.env.GlobalEnvironment().GetGlobalIndex(sym)
				v := mc.env.GlobalEnvironment().GetOwnGlobalBinding(gi).Value()
				qt.Assert(t, v, valuestest.SchemeEquals, values.NewInteger(10))
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
				qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
				qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(2))
			},
		},
		{
			op: NewOperationMakeClosure(),
			setupFn: func(t *testing.T, mc *MachineContext) {
				topEnv := environment.NewNamespace().Runtime()
				lenv := environment.NewLocalEnvironment(0)
				env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
				tpl := NewNativeTemplate(0, 0, false)
				mc.evals.PushAll([]values.Value{tpl, env})
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
				mcls, ok := mc.GetValue().(*MachineClosure)
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
			op: NewOperationApply(),
			setupFn: func(t *testing.T, mc *MachineContext) {
				// Create a zero-arity closure that loads literal 42 and returns.
				topEnv := environment.NewNamespace().Runtime()
				lenv := environment.NewLocalEnvironment(0)
				closureEnv := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
				closureTpl := NewNativeTemplate(0, 0, false,
					NewOperationLoadLiteralByLiteralIndexImmediate(0),
					NewOperationRestoreContinuation(),
				)
				closureTpl.MaybeAppendLiteral(values.NewInteger(42))
				mcls := NewClosureWithTemplate(closureTpl, closureEnv)
				// Save continuation so the closure can return to caller.
				mc.SaveContinuation(1)
				// Set callee in value register.
				mc.SetValue(mcls)
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				// Closure executed: value = 42; returned via RestoreContinuation.
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
				qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
				qt.Assert(t, *mc.evals, qt.HasLen, 0)
			},
		},
		{
			op: NewOperationSaveContinuationOffsetImmediate(1),
			setupFn: func(t *testing.T, mc *MachineContext) {
				topEnv := environment.NewNamespace().Runtime()
				lenv := environment.NewLocalEnvironment(0)
				mc.env = environment.NewEnvironmentFrameWithParent(lenv, topEnv)
				// Pre-save a continuation to build chain depth.
				mc.SaveContinuation(0)
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				// SaveCont pushed a second frame; chain depth is now 1.
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.GetValues(), qt.HasLen, 0)
				qt.Assert(t, *mc.evals, qt.HasLen, 0)
				qt.Assert(t, mc.cont.CallDepth(), qt.Equals, 1)
			},
		},
		{
			op: NewOperationRestoreContinuation(),
			setupFn: func(t *testing.T, mc *MachineContext) {
				topEnv := environment.NewNamespace().Runtime()
				lenv := environment.NewLocalEnvironment(0)
				mc.env = environment.NewEnvironmentFrameWithParent(lenv, topEnv)
				// Build a 3-level chain; RestoreContinuation will pop one level.
				mc.SaveContinuation(1)
				mc.SaveContinuation(1)
				mc.SaveContinuation(1)
			},
			checkFn: func(t *testing.T, mc *MachineContext) {
				// Restored one level; two frames remain (CallDepth = 1).
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.GetValues(), qt.HasLen, 0)
				qt.Assert(t, *mc.evals, qt.HasLen, 0)
				qt.Assert(t, mc.cont.CallDepth(), qt.Equals, 1)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.op.SchemeString(), func(t *testing.T) {
			if tc.evals == nil {
				tc.evals = NewStack()
			}
			topEnv := environment.NewNamespace().Runtime()
			lenv := environment.NewLocalEnvironment(0)
			env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
			tpl := NewNativeTemplate(0, 0, false, tc.op)
			mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))
			mc.evals = tc.evals
			switch {
			case tc.multiValues != nil:
				mc.SetValues(tc.multiValues...)
			case tc.singleValue != nil:
				mc.SetValue(tc.singleValue)
			}
			if tc.setupFn != nil {
				tc.setupFn(t, mc)
			}
			err := mc.Run()
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
		// RestoreContinuation is a zero-field operation: all non-nil instances are structurally equal
		qt.Assert(t, op1.EqualTo(op1), qt.IsTrue)
		qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
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
	qt.Assert(t, copied.Len(), qt.Equals, 3)
	qt.Assert(t, copied[0], valuestest.SchemeEquals, ops[0])
	qt.Assert(t, copied[1], valuestest.SchemeEquals, ops[1])
	qt.Assert(t, copied[2], valuestest.SchemeEquals, ops[2])
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
	qt.Assert(t, ops.Len(), qt.Equals, 3)

	emptyOps := NewOperations()
	qt.Assert(t, emptyOps.Len(), qt.Equals, 0)
}

// TestOperationUnpackListToStack_EqualTo tests that two instances are equal
// and that a different operation type is not equal.
func TestOperationUnpackListToStack_EqualTo(t *testing.T) {
	op1 := NewOperationUnpackListToStack()
	op2 := NewOperationUnpackListToStack()

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(NewOperationPush()), qt.IsFalse)
}

// TestOperationEqualToNilCases tests EqualTo methods with nil cases
func TestOperationEqualToNilCases(t *testing.T) {
	// Test LoadLocalByLocalIndexImmediate nil cases
	li := environment.NewLocalIndex(0, 0)
	op1 := NewOperationLoadLocalByLocalIndexImmediate(li)
	var nilOp *OperationLoadLocalByLocalIndexImmediate

	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
	qt.Assert(t, nilOp.EqualTo(op1), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(op1), qt.IsTrue)

	// Same local index should be equal
	op2 := NewOperationLoadLocalByLocalIndexImmediate(li)
	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)

	// Different local index
	li2 := environment.NewLocalIndex(1, 0)
	op3 := NewOperationLoadLocalByLocalIndexImmediate(li2)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
}

// TestOperationLoadGlobalEqualTo tests LoadGlobalByGlobalIndexLiteralIndexImmediate EqualTo
func TestOperationLoadGlobalEqualTo(t *testing.T) {
	op1 := NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(0))
	op2 := NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(0))
	op3 := NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(1))

	var nilOp *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
}

// TestOperationLoadLiteralEqualTo tests LoadLiteralByLiteralIndexImmediate EqualTo
func TestOperationLoadLiteralEqualTo(t *testing.T) {
	op1 := NewOperationLoadLiteralByLiteralIndexImmediate(LiteralIndex(0))
	op2 := NewOperationLoadLiteralByLiteralIndexImmediate(LiteralIndex(0))
	op3 := NewOperationLoadLiteralByLiteralIndexImmediate(LiteralIndex(1))

	var nilOp *OperationLoadLiteralByLiteralIndexImmediate

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
}

// TestOperationStoreGlobalEqualTo tests StoreGlobalByGlobalIndexLiteralIndexImmediate EqualTo
func TestOperationStoreGlobalEqualTo(t *testing.T) {
	op1 := NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(0))
	op2 := NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(0))
	op3 := NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(1))

	var nilOp *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
}

// TestOperationStoreLocalEqualTo tests StoreLocalByLocalIndexImmediate EqualTo
func TestOperationStoreLocalEqualTo(t *testing.T) {
	li := environment.NewLocalIndex(0, 0)
	op1 := NewOperationStoreLocalByLocalIndexImmediate(li)
	op2 := NewOperationStoreLocalByLocalIndexImmediate(li)

	li2 := environment.NewLocalIndex(1, 0)
	op3 := NewOperationStoreLocalByLocalIndexImmediate(li2)

	var nilOp *OperationStoreLocalByLocalIndexImmediate

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
}

// TestOperationSaveContinuationEqualTo tests SaveContinuationOffsetImmediate EqualTo
func TestOperationSaveContinuationEqualTo(t *testing.T) {
	op1 := NewOperationSaveContinuationOffsetImmediate(10)
	op2 := NewOperationSaveContinuationOffsetImmediate(10)
	op3 := NewOperationSaveContinuationOffsetImmediate(20)

	var nilOp *OperationSaveContinuationOffsetImmediate

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
}

// TestOperationPeekKEqualTo tests PeekK EqualTo
func TestOperationPeekKEqualTo(t *testing.T) {
	op1 := NewOperationPeekK(0)
	op2 := NewOperationPeekK(0)
	op3 := NewOperationPeekK(1)

	var nilOp *OperationPeekK

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
}

// TestOperationPushEnvEqualTo tests PushEnv EqualTo
func TestOperationPushEnvEqualTo(t *testing.T) {
	op1 := NewOperationPushEnv(3)
	op2 := NewOperationPushEnv(3)
	op3 := NewOperationPushEnv(5)

	var nilOp *OperationPushEnv

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(NewOperationPopEnv()), qt.IsFalse)
}

// TestOperationBranchMethods tests Branch operation methods
func TestOperationBranchMethods(t *testing.T) {
	op := NewOperationBranchOffsetImmediate(10)
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationBranchOffsetImmediate(10)
	op3 := NewOperationBranchOffsetImmediate(20)
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op.EqualTo(op3), qt.IsFalse)

	var nilOp *OperationBranchOffsetImmediate
	qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
}

// TestOperationLoadVoidMethods tests LoadVoid operation methods
func TestOperationLoadVoidMethods(t *testing.T) {
	op := NewOperationLoadVoid()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationLoadVoid()
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
}

// TestOperationMakeClosureMethods tests MakeClosure operation methods
func TestOperationMakeClosureMethods(t *testing.T) {
	op := NewOperationMakeClosure()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationMakeClosure()
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
}

// TestOperationApplyMethods tests Apply operation methods
func TestOperationApplyMethods(t *testing.T) {
	op := NewOperationApply()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationApply()
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
}

// TestOperationRestoreContinuationMethods tests RestoreContinuation operation methods
func TestOperationRestoreContinuationMethods(t *testing.T) {
	op := NewOperationRestoreContinuation()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo - same object should be equal to itself
	qt.Assert(t, op.EqualTo(op), qt.IsTrue)
}

// TestOperationPushMethods tests Push operation methods
func TestOperationPushMethods(t *testing.T) {
	op := NewOperationPush()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationPush()
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
}

// TestOperationPullMethods tests Pull operation methods
func TestOperationPullMethods(t *testing.T) {
	op := NewOperationPull()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationPull()
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
}

// TestOperationPopMethods tests Pop operation methods
func TestOperationPopMethods(t *testing.T) {
	op := NewOperationPop()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationPop()
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)
}

// TestOperationMakeCaseLambdaClosureMethods tests MakeCaseLambdaClosure operation methods
func TestOperationMakeCaseLambdaClosureMethods(t *testing.T) {
	op := NewOperationMakeCaseLambdaClosure(2)
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// Test EqualTo
	op2 := NewOperationMakeCaseLambdaClosure(2)
	qt.Assert(t, op.EqualTo(op2), qt.IsTrue)

	op3 := NewOperationMakeCaseLambdaClosure(3)
	qt.Assert(t, op.EqualTo(op3), qt.IsFalse)
}

// TestNativeTemplateOperationsArray tests NativeTemplate operations
func TestNativeTemplateOperationsArray(t *testing.T) {
	tpl := NewNativeTemplate(2, 1, false)

	// Add operations
	tpl.AppendOperations(
		NewOperationLoadVoid(),
		NewOperationPush(),
		NewOperationRestoreContinuation())

	qt.Assert(t, tpl.CodeLen(), qt.Equals, 3)
}

// TestMultipleValuesOperationMethods tests MultipleValues methods
func TestMultipleValuesOperationMethods(t *testing.T) {
	mv := MultipleValues{values.NewInteger(1), values.NewInteger(2)}

	// SchemeString - returns concatenation of values, not #<multiple-values
	str := mv.SchemeString()
	qt.Assert(t, str, qt.Contains, "1")

	// IsVoid
	qt.Assert(t, mv.IsVoid(), qt.IsFalse)

	var nilMv MultipleValues
	qt.Assert(t, nilMv.IsVoid(), qt.IsTrue)
}

// TestOperationRestoreContinuationMethodsExtra tests OperationRestoreContinuation extra methods
func TestOperationRestoreContinuationMethodsExtra(t *testing.T) {
	op := NewOperationRestoreContinuation()

	// Test nil case
	var nilOp *OperationRestoreContinuation
	qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
}

// TestOperationPullMethodsExtra tests OperationPull extra methods
func TestOperationPullMethodsExtra(t *testing.T) {
	op := NewOperationPull()

	// Test nil case
	var nilOp *OperationPull
	qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
}

// TestOperationsLength tests Operations Length method
func TestOperationsLength(t *testing.T) {
	ops := Operations{
		NewOperationLoadVoid(),
		NewOperationPush(),
	}
	qt.Assert(t, ops.Len(), qt.Equals, 2)

	var nilOps Operations
	qt.Assert(t, nilOps.Len(), qt.Equals, 0)
}

// TestOperationsCopy tests Operations Copy method
func TestOperationsCopy(t *testing.T) {
	ops := Operations{
		NewOperationLoadVoid(),
		NewOperationPush(),
	}
	cpy := ops.Copy()
	qt.Assert(t, cpy.Len(), qt.Equals, 2)

	// Modifying copy shouldn't affect original
	cpy = append(cpy, NewOperationPop())
	qt.Assert(t, ops.Len(), qt.Equals, 2)
	qt.Assert(t, cpy.Len(), qt.Equals, 3)
}

// TestOperationMakeClosureError tests MakeClosure with wrong stack
func TestOperationMakeClosureError(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(
		// Push something that's not an environment frame
		NewOperationLoadLiteralByLiteralIndexImmediate(tpl.MaybeAppendLiteral(values.NewInteger(42))),
		NewOperationPush(),
		// Push something that's not a template
		NewOperationLoadLiteralByLiteralIndexImmediate(tpl.MaybeAppendLiteral(values.NewInteger(99))),
		NewOperationPush(),
		NewOperationMakeClosure(),
	)

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)
	err := mc.Run()
	qt.Assert(t, err, qt.IsNotNil) // Should error because stack has wrong types
}

// TestOperationBranchMethodsExtra tests OperationBranch extra methods
func TestOperationBranchMethodsExtra(t *testing.T) {
	op := NewOperationBranchOffsetImmediate(10)

	// Nil check
	var nilOp *OperationBranchOffsetImmediate
	qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
}

// TestOperationSaveContinuationMethodsExtra tests OperationSaveContinuation extra methods
func TestOperationSaveContinuationMethodsExtra(t *testing.T) {
	op := NewOperationSaveContinuationOffsetImmediate(3)

	// Nil check
	var nilOp *OperationSaveContinuationOffsetImmediate
	qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
}

// TestOperationApplyMethodsExtra tests OperationApply extra methods
func TestOperationApplyMethodsExtra(t *testing.T) {
	op := NewOperationApply()

	// EqualTo - nil case
	var nilOp *OperationApply
	qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
}

// TestOperationLoadGlobalMethodsExtra tests OperationLoadGlobal extra methods
func TestOperationLoadGlobalMethodsExtra(t *testing.T) {
	op := NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0)
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
}

// TestOperationStoreGlobalMethodsExtra tests OperationStoreGlobal extra methods
func TestOperationStoreGlobalMethodsExtra(t *testing.T) {
	op := NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(0)
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
}

// TestOperationPopMethodsExtra tests OperationPop extra methods
func TestOperationPopMethodsExtra(t *testing.T) {
	op := NewOperationPop()
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	// EqualTo - nil case
	var nilOp *OperationPop
	qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
}

// TestOperationLoadLocalMethods tests OperationLoadLocal methods
func TestOperationLoadLocalMethods(t *testing.T) {
	idx := &environment.LocalIndex{0, 0}
	op := NewOperationLoadLocalByLocalIndexImmediate(idx)
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
}

// TestOperationStoreLocalMethods tests OperationStoreLocal methods
func TestOperationStoreLocalMethods(t *testing.T) {
	idx := &environment.LocalIndex{0, 0}
	op := NewOperationStoreLocalByLocalIndexImmediate(idx)
	qt.Assert(t, op.SchemeString(), qt.IsNotNil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
}

// TestOperationsSchemeString tests Operations SchemeString method
func TestOperationsSchemeString(t *testing.T) {
	ops := Operations{
		NewOperationLoadVoid(),
		NewOperationPush(),
	}
	str := ops.SchemeString()
	qt.Assert(t, str, qt.IsNotNil)
}

// TestOperationsIsVoid tests Operations IsVoid method
func TestOperationsIsVoid(t *testing.T) {
	var nilOps Operations
	qt.Assert(t, nilOps.IsVoid(), qt.IsTrue)

	ops := Operations{NewOperationLoadVoid()}
	qt.Assert(t, ops.IsVoid(), qt.IsFalse)
}

// TestOperationsEqualTo tests Operations EqualTo method
func TestOperationsEqualTo(t *testing.T) {
	ops1 := Operations{NewOperationLoadVoid()}
	ops2 := Operations{NewOperationLoadVoid()}
	qt.Assert(t, ops1.EqualTo(ops2), qt.IsTrue)

	ops3 := Operations{NewOperationPush()}
	qt.Assert(t, ops1.EqualTo(ops3), qt.IsFalse)

	qt.Assert(t, ops1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// TestOperationSaveContinuationEqualToAdditional tests additional save continuation paths
func TestOperationSaveContinuationEqualToAdditional(t *testing.T) {
	op1 := NewOperationSaveContinuationOffsetImmediate(5)

	// Test nil comparison
	var nilOp *OperationSaveContinuationOffsetImmediate
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
}

// TestOperationApplyAdditional tests Apply operation additional methods
func TestOperationApplyAdditional(t *testing.T) {
	op := NewOperationApply()
	qt.Assert(t, op.SchemeString(), qt.Contains, "apply")
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
	qt.Assert(t, op.EqualTo(NewOperationApply()), qt.IsTrue)
	qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// TestOperationPop tests Pop operation
func TestOperationPop(t *testing.T) {
	op := NewOperationPop()
	qt.Assert(t, op.SchemeString(), qt.Contains, "pop")
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
	qt.Assert(t, op.EqualTo(NewOperationPop()), qt.IsTrue)
}

// TestOperationMakeCaseLambdaClosureAdditional tests MakeCaseLambdaClosure additional methods
func TestOperationMakeCaseLambdaClosureAdditional(t *testing.T) {
	op := NewOperationMakeCaseLambdaClosure(2)
	qt.Assert(t, op.SchemeString(), qt.Contains, "case-lambda")
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
}
