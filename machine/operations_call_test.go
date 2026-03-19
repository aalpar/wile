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
	"testing"

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestOperationsCall(t *testing.T) {
	tcs := []struct {
		name    string
		op      Operation
		checkFn func(t *testing.T, op Operation)
	}{
		// --- Apply ---
		{
			name: "Apply/constructor",
			op:   NewOperationApply(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "Apply/SchemeString",
			op:   NewOperationApply(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "apply")
			},
		},
		{
			name: "Apply/IsVoid",
			op:   NewOperationApply(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "Apply/EqualTo_self",
			op:   NewOperationApply(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationApply()), qt.IsTrue)
			},
		},
		{
			name: "Apply/EqualTo_different_type",
			op:   NewOperationApply(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "Apply/EqualTo_nil",
			op:   NewOperationApply(),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationApply
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		// --- ForeignFunctionCall ---
		{
			name: "ForeignFunctionCall/constructor",
			op: NewOperationForeignFunctionCall(func(mc *MachineContext) error {
				return nil
			}),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "ForeignFunctionCall/SchemeString",
			op: NewOperationForeignFunctionCall(func(mc *MachineContext) error {
				return nil
			}),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "foreign-function-call")
			},
		},
		{
			name: "ForeignFunctionCall/IsVoid",
			op: NewOperationForeignFunctionCall(func(mc *MachineContext) error {
				return nil
			}),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "ForeignFunctionCall/EqualTo_self",
			op: NewOperationForeignFunctionCall(func(mc *MachineContext) error {
				return nil
			}),
			checkFn: func(t *testing.T, op Operation) {
				// ForeignFunctionCall uses sameType, so two distinct instances are equal
				qt.Assert(t, op.EqualTo(NewOperationForeignFunctionCall(func(mc *MachineContext) error {
					return nil
				})), qt.IsTrue)
			},
		},
		{
			name: "ForeignFunctionCall/EqualTo_different_type",
			op: NewOperationForeignFunctionCall(func(mc *MachineContext) error {
				return nil
			}),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "ForeignFunctionCall/EqualTo_nil",
			op: NewOperationForeignFunctionCall(func(mc *MachineContext) error {
				return nil
			}),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationForeignFunctionCall
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		// --- UnpackListToStack ---
		{
			name: "UnpackListToStack/constructor",
			op:   NewOperationUnpackListToStack(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "UnpackListToStack/SchemeString",
			op:   NewOperationUnpackListToStack(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "unpack-list-to-stack")
			},
		},
		{
			name: "UnpackListToStack/IsVoid",
			op:   NewOperationUnpackListToStack(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "UnpackListToStack/EqualTo_self",
			op:   NewOperationUnpackListToStack(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationUnpackListToStack()), qt.IsTrue)
			},
		},
		{
			name: "UnpackListToStack/EqualTo_different_type",
			op:   NewOperationUnpackListToStack(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "UnpackListToStack/EqualTo_nil",
			op:   NewOperationUnpackListToStack(),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationUnpackListToStack
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tc.checkFn(t, tc.op)
		})
	}
}
