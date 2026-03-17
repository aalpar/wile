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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// --- Type construction tests ---

func TestFlatClosure_CaptureEntry(t *testing.T) {
	entry := CaptureEntry{
		SourceSlot:   2,
		SourceDepth:  1,
		ClosureSlot:  0,
		Boxed:        true,
		FromFreeVars: false,
	}
	qt.Assert(t, entry.SourceSlot, qt.Equals, 2)
	qt.Assert(t, entry.SourceDepth, qt.Equals, 1)
	qt.Assert(t, entry.ClosureSlot, qt.Equals, 0)
	qt.Assert(t, entry.Boxed, qt.IsTrue)
	qt.Assert(t, entry.FromFreeVars, qt.IsFalse)
}

func TestFlatClosure_FreeVarInfo(t *testing.T) {
	info := &FreeVarInfo{
		Captures: []CaptureEntry{
			{SourceSlot: 0, SourceDepth: 1, ClosureSlot: 0, Boxed: false, FromFreeVars: false},
			{SourceSlot: 1, SourceDepth: 2, ClosureSlot: 1, Boxed: true, FromFreeVars: true},
		},
		Mutated: map[[2]int]bool{
			{1, 2}: true,
		},
	}
	qt.Assert(t, info.Captures, qt.HasLen, 2)
	qt.Assert(t, info.Captures[0].SourceSlot, qt.Equals, 0)
	qt.Assert(t, info.Captures[1].FromFreeVars, qt.IsTrue)
	qt.Assert(t, info.Mutated[[2]int{1, 2}], qt.IsTrue)
	qt.Assert(t, info.Mutated[[2]int{0, 0}], qt.IsFalse)
}

func TestFlatClosure_NativeTemplateFreeVarInfo(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)
	qt.Assert(t, tpl.FreeVarInfo(), qt.IsNil)

	info := &FreeVarInfo{
		Captures: []CaptureEntry{
			{SourceSlot: 0, SourceDepth: 1, ClosureSlot: 0},
		},
	}
	tpl.SetFreeVarInfo(info)
	qt.Assert(t, tpl.FreeVarInfo(), qt.Equals, info)
}

func TestFlatClosure_NewClosureWithFreeVars(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)
	freeVars := []values.Value{
		values.NewInteger(10),
		values.NewInteger(20),
	}
	cls := NewClosureWithFreeVars(tpl, freeVars)

	qt.Assert(t, cls.Template(), qt.Equals, tpl)
	qt.Assert(t, cls.FreeVars(), qt.HasLen, 2)
	qt.Assert(t, cls.FreeVars()[0], valuestest.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, cls.FreeVars()[1], valuestest.SchemeEquals, values.NewInteger(20))
	qt.Assert(t, cls.Env(), qt.IsNil)
}

func TestFlatClosure_LinkedClosureFreeVarsNil(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	cls := NewClosureWithTemplate(tpl, topEnv)
	qt.Assert(t, cls.FreeVars(), qt.IsNil)
}

func TestFlatClosure_MachineContextFreeVars(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))

	qt.Assert(t, mc.FreeVars(), qt.IsNil)

	fv := []values.Value{values.NewInteger(42)}
	mc.SetFreeVars(fv)
	qt.Assert(t, mc.FreeVars(), qt.HasLen, 1)
	qt.Assert(t, mc.FreeVars()[0], valuestest.SchemeEquals, values.NewInteger(42))
}

// --- Opcode dispatch tests ---

func TestFlatClosure_Opcodes(t *testing.T) {
	tcs := []struct {
		name    string
		setupFn func(t *testing.T, mc *MachineContext)
		op      Operation
		checkFn func(t *testing.T, mc *MachineContext)
	}{
		{
			name: "OpBox wraps value in box",
			setupFn: func(t *testing.T, mc *MachineContext) {
				mc.SetValue(values.NewInteger(42))
			},
			op: NewOperationBox(),
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				box, ok := mc.GetValue().(*values.Box)
				qt.Assert(t, ok, qt.IsTrue)
				qt.Assert(t, box.Value, valuestest.SchemeEquals, values.NewInteger(42))
			},
		},
		{
			name: "OpUnbox unwraps box",
			setupFn: func(t *testing.T, mc *MachineContext) {
				mc.SetValue(values.NewBox(values.NewInteger(99)))
			},
			op: NewOperationUnbox(),
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(99))
			},
		},
		{
			name: "OpSetBox updates box value",
			setupFn: func(t *testing.T, mc *MachineContext) {
				mc.evals.Push(values.NewInteger(77))
				mc.SetValue(values.NewBox(values.NewInteger(0)))
			},
			op: NewOperationSetBox(),
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				box, ok := mc.GetValue().(*values.Box)
				qt.Assert(t, ok, qt.IsTrue)
				qt.Assert(t, box.Value, valuestest.SchemeEquals, values.NewInteger(77))
			},
		},
		{
			name: "OpLoadFreeVar loads from freeVars",
			setupFn: func(t *testing.T, mc *MachineContext) {
				mc.freeVars = []values.Value{
					values.NewInteger(10),
					values.NewInteger(20),
					values.NewInteger(30),
				}
			},
			op: NewOperationLoadFreeVar(1),
			checkFn: func(t *testing.T, mc *MachineContext) {
				qt.Assert(t, mc.pc, qt.Equals, 1)
				qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(20))
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			topEnv := environment.NewTopLevelEnvironment().Runtime()
			lenv := environment.NewLocalEnvironment(0)
			env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
			tpl := NewNativeTemplate(0, 0, false, tc.op)
			mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))
			if tc.setupFn != nil {
				tc.setupFn(t, mc)
			}
			err := mc.Run()
			qt.Assert(t, err, qt.IsNil)
			tc.checkFn(t, mc)
		})
	}
}

func TestFlatClosure_OpUnboxError(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(0, 0, false, NewOperationUnbox())
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))
	mc.SetValue(values.NewInteger(42)) // not a box
	err := mc.Run()
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "unbox")
}

func TestFlatClosure_OpSetBoxError(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(0, 0, false, NewOperationSetBox())
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))
	mc.evals.Push(values.NewInteger(1))
	mc.SetValue(values.NewInteger(42)) // not a box
	err := mc.Run()
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "set-box")
}

func TestFlatClosure_OpMakeFlatClosure(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(0)

	// Set up a local binding at slot 0 BEFORE creating the env frame,
	// because NewEnvironmentFrameWithParent value-copies the LocalEnvironmentFrame.
	sym := values.NewSymbol("x")
	li, _ := lenv.EnsureLocalBinding(sym, environment.BindingTypeVariable)
	lenv.SetLocalValue(li, values.NewInteger(42)) //nolint:errcheck
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)

	// Create the inner template with FreeVarInfo
	innerTpl := NewNativeTemplate(0, 0, false)
	innerTpl.SetFreeVarInfo(&FreeVarInfo{
		Captures: []CaptureEntry{
			{SourceSlot: 0, SourceDepth: 1, ClosureSlot: 0, Boxed: false, FromFreeVars: false},
		},
	})

	// Create a compile-time env for the inner closure's parameter bindings.
	innerLenv := environment.NewLocalEnvironment(0)
	innerEnv := environment.NewEnvironmentFrameWithParent(innerLenv, topEnv)

	// The outer template: push innerTpl, push innerEnv, then MakeFlatClosure
	outerTpl := NewNativeTemplate(0, 0, false)
	tplIdx := outerTpl.MaybeAppendLiteral(innerTpl)
	envIdx := outerTpl.MaybeAppendLiteral(innerEnv)
	outerTpl.AppendOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(tplIdx),
		NewOperationPush(),
		NewOperationLoadLiteralByLiteralIndexImmediate(envIdx),
		NewOperationPush(),
		NewOperationMakeFlatClosure(),
	)

	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, outerTpl, env))
	err := mc.Run()
	qt.Assert(t, err, qt.IsNil)

	cls, ok := mc.GetValue().(*MachineClosure)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, cls.FreeVars(), qt.HasLen, 1)
	qt.Assert(t, cls.FreeVars()[0], valuestest.SchemeEquals, values.NewInteger(42))
	qt.Assert(t, cls.Env(), qt.IsNotNil)
}

func TestFlatClosure_OpMakeFlatClosureFromFreeVars(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)

	// Create the inner template with FreeVarInfo that reads from enclosing freeVars
	innerTpl := NewNativeTemplate(0, 0, false)
	innerTpl.SetFreeVarInfo(&FreeVarInfo{
		Captures: []CaptureEntry{
			{SourceSlot: 0, SourceDepth: 1, ClosureSlot: 0, Boxed: false, FromFreeVars: true},
		},
	})

	// Create a compile-time env for the inner closure's parameter bindings.
	innerLenv := environment.NewLocalEnvironment(0)
	innerEnv := environment.NewEnvironmentFrameWithParent(innerLenv, topEnv)

	// The outer template: push innerTpl, push innerEnv, then MakeFlatClosure
	outerTpl := NewNativeTemplate(0, 0, false)
	tplIdx := outerTpl.MaybeAppendLiteral(innerTpl)
	envIdx := outerTpl.MaybeAppendLiteral(innerEnv)
	outerTpl.AppendOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(tplIdx),
		NewOperationPush(),
		NewOperationLoadLiteralByLiteralIndexImmediate(envIdx),
		NewOperationPush(),
		NewOperationMakeFlatClosure(),
	)

	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, outerTpl, env))
	mc.freeVars = []values.Value{values.NewInteger(99)}
	err := mc.Run()
	qt.Assert(t, err, qt.IsNil)

	cls, ok := mc.GetValue().(*MachineClosure)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, cls.FreeVars(), qt.HasLen, 1)
	qt.Assert(t, cls.FreeVars()[0], valuestest.SchemeEquals, values.NewInteger(99))
}

func TestFlatClosure_OpMakeFlatClosureNoInfo(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)

	innerTpl := NewNativeTemplate(0, 0, false) // no FreeVarInfo set

	// Create a compile-time env for the inner closure.
	innerLenv := environment.NewLocalEnvironment(0)
	innerEnv := environment.NewEnvironmentFrameWithParent(innerLenv, topEnv)

	outerTpl := NewNativeTemplate(0, 0, false)
	tplIdx := outerTpl.MaybeAppendLiteral(innerTpl)
	envIdx := outerTpl.MaybeAppendLiteral(innerEnv)
	outerTpl.AppendOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(tplIdx),
		NewOperationPush(),
		NewOperationLoadLiteralByLiteralIndexImmediate(envIdx),
		NewOperationPush(),
		NewOperationMakeFlatClosure(),
	)

	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, outerTpl, env))
	err := mc.Run()
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "FreeVarInfo")
}

// --- Operation value method tests ---

func TestFlatClosure_OperationLoadFreeVarMethods(t *testing.T) {
	op1 := NewOperationLoadFreeVar(0)
	op2 := NewOperationLoadFreeVar(0)
	op3 := NewOperationLoadFreeVar(1)

	qt.Assert(t, op1.IsVoid(), qt.IsFalse)
	qt.Assert(t, op1.SchemeString(), qt.Contains, "load-free-var")
	qt.Assert(t, op1.SchemeString(), qt.Contains, "0")
	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsFalse)
	qt.Assert(t, op1.EqualTo(values.NewInteger(1)), qt.IsFalse)

	var nilOp *OperationLoadFreeVar
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
	qt.Assert(t, nilOp.EqualTo(nilOp), qt.IsTrue)
}

func TestFlatClosure_OperationBoxMethods(t *testing.T) {
	op1 := NewOperationBox()
	op2 := NewOperationBox()

	qt.Assert(t, op1.IsVoid(), qt.IsFalse)
	qt.Assert(t, op1.SchemeString(), qt.Contains, "box")
	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(values.NewInteger(1)), qt.IsFalse)

	var nilOp *OperationBox
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
}

func TestFlatClosure_OperationUnboxMethods(t *testing.T) {
	op1 := NewOperationUnbox()
	op2 := NewOperationUnbox()

	qt.Assert(t, op1.IsVoid(), qt.IsFalse)
	qt.Assert(t, op1.SchemeString(), qt.Contains, "unbox")
	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(values.NewInteger(1)), qt.IsFalse)

	var nilOp *OperationUnbox
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
}

func TestFlatClosure_OperationSetBoxMethods(t *testing.T) {
	op1 := NewOperationSetBox()
	op2 := NewOperationSetBox()

	qt.Assert(t, op1.IsVoid(), qt.IsFalse)
	qt.Assert(t, op1.SchemeString(), qt.Contains, "set-box")
	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(values.NewInteger(1)), qt.IsFalse)

	var nilOp *OperationSetBox
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
}

func TestFlatClosure_OperationMakeFlatClosureMethods(t *testing.T) {
	op1 := NewOperationMakeFlatClosure()
	op2 := NewOperationMakeFlatClosure()

	qt.Assert(t, op1.IsVoid(), qt.IsFalse)
	qt.Assert(t, op1.SchemeString(), qt.Contains, "make-flat-closure")
	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(values.NewInteger(1)), qt.IsFalse)

	var nilOp *OperationMakeFlatClosure
	qt.Assert(t, op1.EqualTo(nilOp), qt.IsFalse)
}

// --- Instruction round-trip tests ---

func TestFlatClosure_InstructionRoundTrip(t *testing.T) {
	tcs := []struct {
		name string
		op   Operation
	}{
		{"LoadFreeVar", NewOperationLoadFreeVar(5)},
		{"Box", NewOperationBox()},
		{"Unbox", NewOperationUnbox()},
		{"SetBox", NewOperationSetBox()},
		{"MakeFlatClosure", NewOperationMakeFlatClosure()},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			instr, ok := operationToInstruction(tc.op)
			qt.Assert(t, ok, qt.IsTrue)
			roundTripped := instructionToOperation(instr)
			qt.Assert(t, roundTripped, qt.IsNotNil)
			qt.Assert(t, tc.op.EqualTo(roundTripped), qt.IsTrue)
		})
	}
}

// --- Continuation save/restore preserves freeVars ---

func TestFlatClosure_ContinuationSavesRestoresFreeVars(t *testing.T) {
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(0, 0, false)

	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))
	originalFV := []values.Value{values.NewInteger(1), values.NewInteger(2)}
	mc.freeVars = originalFV

	// SaveContinuation captures freeVars into the continuation
	err := mc.SaveContinuation(0)
	qt.Assert(t, err, qt.IsNil)

	// Overwrite freeVars on mc
	mc.freeVars = []values.Value{values.NewInteger(99)}

	// RestoreAndRelease should bring back the original freeVars
	mc.RestoreAndRelease(mc.cont)
	qt.Assert(t, mc.freeVars, qt.HasLen, 2)
	qt.Assert(t, mc.freeVars[0], valuestest.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, mc.freeVars[1], valuestest.SchemeEquals, values.NewInteger(2))
}

func TestFlatClosure_ContinuationCopyPreservesFreeVars(t *testing.T) {
	fv := []values.Value{values.NewInteger(10)}
	topEnv := environment.NewTopLevelEnvironment().Runtime()
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	tpl := NewNativeTemplate(0, 0, false)

	cont := NewMachineContinuation(nil, tpl, env)
	cont.freeVars = fv

	copied := cont.Copy()
	qt.Assert(t, copied.freeVars, qt.HasLen, 1)
	qt.Assert(t, copied.freeVars[0], valuestest.SchemeEquals, values.NewInteger(10))
}

// --- Apply fast path tests ---

func TestFlatClosure_ApplyCopyPath(t *testing.T) {
	// A flat closure with noCopyApply=false (contains SaveContinuation)
	// should use InitApplyFrame (no binding value copy).
	topEnv := environment.NewTopLevelEnvironment().Runtime()

	// Create a flat closure: template with SaveContinuation (forces copy path),
	// 1 parameter, and freeVars set.
	lenv := environment.NewLocalEnvironment(1)
	sym := values.NewSymbol("n")
	lenv.EnsureLocalBinding(sym, environment.BindingTypeVariable)
	closureEnv := environment.NewEnvironmentFrameWithParent(lenv, topEnv)

	// Template: SaveContinuation (forces noCopyApply=false) + RestoreContinuation
	innerTpl := NewNativeTemplate(1, 0, false,
		NewOperationSaveContinuationOffsetImmediate(1),
		NewOperationRestoreContinuation(),
	)
	innerTpl.Optimize()
	innerTpl.computeNoCopyApply()
	innerTpl.SetName("flat-copy-test")

	cls := NewClosureWithFreeVars(innerTpl, []values.Value{values.NewInteger(99)})
	cls.env = closureEnv

	// Verify noCopyApply is false (SaveContinuation present)
	qt.Assert(t, innerTpl.NoCopyApply(), qt.IsFalse)

	// Set up a parent MachineContext to call Apply on
	outerTpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, outerTpl, topEnv))

	// Apply the flat closure with one arg
	_, err := mc.Apply(cls, values.NewInteger(42))
	qt.Assert(t, err, qt.IsNil)

	// Verify freeVars were set
	qt.Assert(t, mc.freeVars, qt.HasLen, 1)
	qt.Assert(t, mc.freeVars[0], valuestest.SchemeEquals, values.NewInteger(99))

	// Verify the counter tracked it as a copy apply
	qt.Assert(t, mc.counters.EnvsCopied, qt.Equals, uint64(1))

	// Verify envPooled is true (frame from pool, will be recycled)
	qt.Assert(t, mc.envPooled, qt.IsTrue)
}

func TestFlatClosure_ApplyNoCopyPath(t *testing.T) {
	// A flat closure with noCopyApply=true should reuse the closure's env
	// and still set freeVars.
	topEnv := environment.NewTopLevelEnvironment().Runtime()

	lenv := environment.NewLocalEnvironment(1)
	sym := values.NewSymbol("n")
	lenv.EnsureLocalBinding(sym, environment.BindingTypeVariable)
	closureEnv := environment.NewEnvironmentFrameWithParent(lenv, topEnv)

	// Template: no SaveContinuation → noCopyApply=true
	innerTpl := NewNativeTemplate(1, 0, false,
		NewOperationRestoreContinuation(),
	)
	innerTpl.Optimize()
	innerTpl.computeNoCopyApply()

	cls := NewClosureWithFreeVars(innerTpl, []values.Value{values.NewInteger(7)})
	cls.env = closureEnv

	qt.Assert(t, innerTpl.NoCopyApply(), qt.IsTrue)

	outerTpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, outerTpl, topEnv))

	_, err := mc.Apply(cls, values.NewInteger(1))
	qt.Assert(t, err, qt.IsNil)

	// freeVars should be set even on the noCopy path
	qt.Assert(t, mc.freeVars, qt.HasLen, 1)
	qt.Assert(t, mc.freeVars[0], valuestest.SchemeEquals, values.NewInteger(7))

	// noCopy path should not increment EnvsCopied
	qt.Assert(t, mc.counters.EnvsCopied, qt.Equals, uint64(0))
	qt.Assert(t, mc.counters.NoCopyApplies, qt.Equals, uint64(1))

	// env should be the closure's own env (not pooled)
	qt.Assert(t, mc.env, qt.Equals, closureEnv)
	qt.Assert(t, mc.envPooled, qt.IsFalse)
}

// --- Opcode metadata tests ---

func TestFlatClosure_OpcodeString(t *testing.T) {
	qt.Assert(t, OpLoadFreeVar.String(), qt.Equals, "LoadFreeVar")
	qt.Assert(t, OpBox.String(), qt.Equals, "Box")
	qt.Assert(t, OpUnbox.String(), qt.Equals, "Unbox")
	qt.Assert(t, OpSetBox.String(), qt.Equals, "SetBox")
	qt.Assert(t, OpMakeFlatClosure.String(), qt.Equals, "MakeFlatClosure")
}
