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

func TestOperationBrk(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()
	called := false
	var capturedMc *MachineContext

	fn := func(ctx context.Context, mc *MachineContext) error {
		called = true
		capturedMc = mc
		return nil
	}

	mc := &MachineContext{
		env: env,
		pc:  0,
	}

	op := NewOperationBrk(fn)
	newMc, err := op.Apply(context.Background(), mc)

	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, called, qt.IsTrue)
	qt.Assert(t, capturedMc, qt.Equals, mc)
	qt.Assert(t, newMc.pc, qt.Equals, 1)
}

func TestOperationBrk_WithError(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()
	expectedErr := values.NewForeignError("test error")

	fn := func(ctx context.Context, mc *MachineContext) error {
		return expectedErr
	}

	mc := &MachineContext{
		env: env,
		pc:  0,
	}

	op := NewOperationBrk(fn)
	newMc, err := op.Apply(context.Background(), mc)

	qt.Assert(t, err, qt.Equals, expectedErr)
	qt.Assert(t, newMc.pc, qt.Equals, 1)
}

func TestOperationBrk_SchemeString(t *testing.T) {
	op := NewOperationBrk(nil)
	qt.Assert(t, op.SchemeString(), qt.Equals, "#<machine-operation-brk>")
}

func TestOperationBrk_IsVoid(t *testing.T) {
	op := NewOperationBrk(nil)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	var nilOp *OperationBrk
	qt.Assert(t, nilOp.IsVoid(), qt.IsTrue)
}

func TestOperationBrk_EqualTo(t *testing.T) {
	fn1 := func(ctx context.Context, mc *MachineContext) error { return nil }
	fn2 := func(ctx context.Context, mc *MachineContext) error { return nil }

	op1 := NewOperationBrk(fn1)
	op2 := NewOperationBrk(fn2)

	// Functions are not comparable, but both operations should be equal if non-nil
	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(values.NewInteger(42)), qt.IsFalse)

	var nilOp1 *OperationBrk
	var nilOp2 *OperationBrk
	qt.Assert(t, nilOp1.EqualTo(nilOp2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(nilOp1), qt.IsFalse)
}

func TestOperationLoadLiteralInteger(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()

	mc := &MachineContext{
		env:   env,
		pc:    0,
		value: MultipleValues{values.NewInteger(999)},
	}

	op := NewOperationLoadLiteralInteger(42)
	newMc, err := op.Apply(context.Background(), mc)

	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, newMc.pc, qt.Equals, 1)
	qt.Assert(t, newMc.value, qt.IsNil)
}

func TestOperationLoadLiteralInteger_SchemeString(t *testing.T) {
	op := NewOperationLoadLiteralInteger(42)
	qt.Assert(t, op.SchemeString(), qt.Equals, "#<machine-operation-load-literal-integer>")
}

func TestOperationLoadLiteralInteger_IsVoid(t *testing.T) {
	op := NewOperationLoadLiteralInteger(42)
	qt.Assert(t, op.IsVoid(), qt.IsFalse)

	var nilOp *OperationLoadLiteralInteger
	qt.Assert(t, nilOp.IsVoid(), qt.IsTrue)
}

func TestOperationLoadLiteralInteger_EqualTo(t *testing.T) {
	op1 := NewOperationLoadLiteralInteger(42)
	op2 := NewOperationLoadLiteralInteger(42)
	op3 := NewOperationLoadLiteralInteger(99)

	qt.Assert(t, op1.EqualTo(op2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(op3), qt.IsTrue) // Implementation returns true for all non-nil
	qt.Assert(t, op1.EqualTo(values.NewInteger(42)), qt.IsFalse)

	var nilOp1 *OperationLoadLiteralInteger
	var nilOp2 *OperationLoadLiteralInteger
	qt.Assert(t, nilOp1.EqualTo(nilOp2), qt.IsTrue)
	qt.Assert(t, op1.EqualTo(nilOp1), qt.IsFalse)
}

func TestOperationApply_SchemeString(t *testing.T) {
	op := NewOperationApply()
	qt.Assert(t, op.SchemeString(), qt.Equals, "#<operation-apply>")
}

func TestOperationPull_SchemeString(t *testing.T) {
	op := NewOperationPull()
	qt.Assert(t, op.SchemeString(), qt.Equals, "#<machine-operation-pull>")
}
