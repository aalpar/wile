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

func TestOperationsStack(t *testing.T) {
	tcs := []struct {
		name    string
		op      Operation
		checkFn func(t *testing.T, op Operation)
	}{
		{
			name: "Push/constructor",
			op:   NewOperationPush(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "Push/SchemeString",
			op:   NewOperationPush(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "push")
			},
		},
		{
			name: "Push/IsVoid",
			op:   NewOperationPush(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "Push/EqualTo_self",
			op:   NewOperationPush(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationPush()), qt.IsTrue)
			},
		},
		{
			name: "Push/EqualTo_different_type",
			op:   NewOperationPush(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "Push/EqualTo_nil",
			op:   NewOperationPush(),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationPush
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		{
			name: "Pop/constructor",
			op:   NewOperationPop(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "Pop/SchemeString",
			op:   NewOperationPop(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "pop")
			},
		},
		{
			name: "Pop/IsVoid",
			op:   NewOperationPop(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "Pop/EqualTo_self",
			op:   NewOperationPop(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationPop()), qt.IsTrue)
			},
		},
		{
			name: "Pop/EqualTo_different_type",
			op:   NewOperationPop(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "Pop/EqualTo_nil",
			op:   NewOperationPop(),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationPop
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		{
			name: "Pull/constructor",
			op:   NewOperationPull(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "Pull/SchemeString",
			op:   NewOperationPull(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "pull")
			},
		},
		{
			name: "Pull/IsVoid",
			op:   NewOperationPull(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "Pull/EqualTo_self",
			op:   NewOperationPull(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationPull()), qt.IsTrue)
			},
		},
		{
			name: "Pull/EqualTo_different_type",
			op:   NewOperationPull(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "Pull/EqualTo_nil",
			op:   NewOperationPull(),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationPull
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		{
			name: "Drop/constructor",
			op:   NewOperationDrop(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "Drop/SchemeString",
			op:   NewOperationDrop(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "drop")
			},
		},
		{
			name: "Drop/IsVoid",
			op:   NewOperationDrop(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "Drop/EqualTo_self",
			op:   NewOperationDrop(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationDrop()), qt.IsTrue)
			},
		},
		{
			name: "Drop/EqualTo_different_type",
			op:   NewOperationDrop(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "Drop/EqualTo_nil",
			op:   NewOperationDrop(),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationDrop
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		{
			name: "PeekK/constructor",
			op:   NewOperationPeekK(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "PeekK/SchemeString",
			op:   NewOperationPeekK(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "peek-k")
				qt.Assert(t, op.SchemeString(), qt.Contains, "3")
			},
		},
		{
			name: "PeekK/IsVoid",
			op:   NewOperationPeekK(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "PeekK/EqualTo_same_depth",
			op:   NewOperationPeekK(5),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationPeekK(5)), qt.IsTrue)
			},
		},
		{
			name: "PeekK/EqualTo_different_depth",
			op:   NewOperationPeekK(5),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationPeekK(10)), qt.IsFalse)
			},
		},
		{
			name: "PeekK/EqualTo_different_type",
			op:   NewOperationPeekK(5),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(5)), qt.IsFalse)
			},
		},
		{
			name: "PeekK/EqualTo_nil",
			op:   NewOperationPeekK(5),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationPeekK
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
