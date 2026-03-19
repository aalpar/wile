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

func TestOperationsControl(t *testing.T) {
	tcs := []struct {
		name    string
		op      Operation
		checkFn func(t *testing.T, op Operation)
	}{
		{
			name: "BranchOffsetImmediate/constructor",
			op:   NewOperationBranchOffsetImmediate(5),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "BranchOffsetImmediate/SchemeString",
			op:   NewOperationBranchOffsetImmediate(5),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "branch-offset-immediate")
				qt.Assert(t, op.SchemeString(), qt.Contains, "5")
			},
		},
		{
			name: "BranchOffsetImmediate/IsVoid",
			op:   NewOperationBranchOffsetImmediate(5),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "BranchOffsetImmediate/EqualTo_same_offset",
			op:   NewOperationBranchOffsetImmediate(5),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationBranchOffsetImmediate(5)), qt.IsTrue)
			},
		},
		{
			name: "BranchOffsetImmediate/EqualTo_different_offset",
			op:   NewOperationBranchOffsetImmediate(5),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationBranchOffsetImmediate(10)), qt.IsFalse)
			},
		},
		{
			name: "BranchOffsetImmediate/EqualTo_different_type",
			op:   NewOperationBranchOffsetImmediate(5),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(5)), qt.IsFalse)
			},
		},
		{
			name: "BranchOffsetImmediate/EqualTo_nil",
			op:   NewOperationBranchOffsetImmediate(5),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationBranchOffsetImmediate
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		{
			name: "BranchOnFalseValueOffsetImmediate/constructor",
			op:   NewOperationBranchOnFalseValueOffsetImmediate(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "BranchOnFalseValueOffsetImmediate/SchemeString",
			op:   NewOperationBranchOnFalseValueOffsetImmediate(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "branch-on-false")
				qt.Assert(t, op.SchemeString(), qt.Contains, "3")
			},
		},
		{
			name: "BranchOnFalseValueOffsetImmediate/IsVoid",
			op:   NewOperationBranchOnFalseValueOffsetImmediate(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "BranchOnFalseValueOffsetImmediate/EqualTo_same_offset",
			op:   NewOperationBranchOnFalseValueOffsetImmediate(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationBranchOnFalseValueOffsetImmediate(3)), qt.IsTrue)
			},
		},
		{
			name: "BranchOnFalseValueOffsetImmediate/EqualTo_different_offset",
			op:   NewOperationBranchOnFalseValueOffsetImmediate(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationBranchOnFalseValueOffsetImmediate(7)), qt.IsFalse)
			},
		},
		{
			name: "BranchOnFalseValueOffsetImmediate/EqualTo_different_type",
			op:   NewOperationBranchOnFalseValueOffsetImmediate(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(3)), qt.IsFalse)
			},
		},
		{
			name: "BranchOnFalseValueOffsetImmediate/EqualTo_nil",
			op:   NewOperationBranchOnFalseValueOffsetImmediate(3),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationBranchOnFalseValueOffsetImmediate
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		{
			name: "SaveContinuationOffsetImmediate/constructor",
			op:   NewOperationSaveContinuationOffsetImmediate(10),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "SaveContinuationOffsetImmediate/SchemeString",
			op:   NewOperationSaveContinuationOffsetImmediate(10),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "save-continuation")
				qt.Assert(t, op.SchemeString(), qt.Contains, "10")
			},
		},
		{
			name: "SaveContinuationOffsetImmediate/IsVoid",
			op:   NewOperationSaveContinuationOffsetImmediate(10),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "SaveContinuationOffsetImmediate/EqualTo_same_offset",
			op:   NewOperationSaveContinuationOffsetImmediate(10),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationSaveContinuationOffsetImmediate(10)), qt.IsTrue)
			},
		},
		{
			name: "SaveContinuationOffsetImmediate/EqualTo_different_offset",
			op:   NewOperationSaveContinuationOffsetImmediate(10),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationSaveContinuationOffsetImmediate(20)), qt.IsFalse)
			},
		},
		{
			name: "SaveContinuationOffsetImmediate/EqualTo_different_type",
			op:   NewOperationSaveContinuationOffsetImmediate(10),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(10)), qt.IsFalse)
			},
		},
		{
			name: "SaveContinuationOffsetImmediate/EqualTo_nil",
			op:   NewOperationSaveContinuationOffsetImmediate(10),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationSaveContinuationOffsetImmediate
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		{
			name: "RestoreContinuation/constructor",
			op:   NewOperationRestoreContinuation(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "RestoreContinuation/SchemeString",
			op:   NewOperationRestoreContinuation(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "restore-continuation")
			},
		},
		{
			name: "RestoreContinuation/IsVoid",
			op:   NewOperationRestoreContinuation(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "RestoreContinuation/EqualTo_self",
			op:   NewOperationRestoreContinuation(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationRestoreContinuation()), qt.IsTrue)
			},
		},
		{
			name: "RestoreContinuation/EqualTo_different_type",
			op:   NewOperationRestoreContinuation(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "RestoreContinuation/EqualTo_nil",
			op:   NewOperationRestoreContinuation(),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationRestoreContinuation
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
