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

	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

func TestOperationsClosure(t *testing.T) {
	tcs := []struct {
		name    string
		op      Operation
		checkFn func(t *testing.T, op Operation)
	}{
		// --- MakeClosure ---
		{
			name: "MakeClosure/constructor",
			op:   NewOperationMakeClosure(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "MakeClosure/SchemeString",
			op:   NewOperationMakeClosure(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "make-closure")
			},
		},
		{
			name: "MakeClosure/IsVoid",
			op:   NewOperationMakeClosure(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "MakeClosure/EqualTo_self",
			op:   NewOperationMakeClosure(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationMakeClosure()), qt.IsTrue)
			},
		},
		{
			name: "MakeClosure/EqualTo_different_type",
			op:   NewOperationMakeClosure(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "MakeClosure/EqualTo_nil",
			op:   NewOperationMakeClosure(),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationMakeClosure
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		// --- MakeCaseLambdaClosure ---
		{
			name: "MakeCaseLambdaClosure/constructor",
			op:   NewOperationMakeCaseLambdaClosure(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "MakeCaseLambdaClosure/SchemeString",
			op:   NewOperationMakeCaseLambdaClosure(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "case-lambda")
			},
		},
		{
			name: "MakeCaseLambdaClosure/IsVoid",
			op:   NewOperationMakeCaseLambdaClosure(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "MakeCaseLambdaClosure/EqualTo_same_count",
			op:   NewOperationMakeCaseLambdaClosure(2),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationMakeCaseLambdaClosure(2)), qt.IsTrue)
			},
		},
		{
			name: "MakeCaseLambdaClosure/EqualTo_different_count",
			op:   NewOperationMakeCaseLambdaClosure(2),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationMakeCaseLambdaClosure(5)), qt.IsFalse)
			},
		},
		{
			name: "MakeCaseLambdaClosure/EqualTo_different_type",
			op:   NewOperationMakeCaseLambdaClosure(2),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(2)), qt.IsFalse)
			},
		},
		{
			name: "MakeCaseLambdaClosure/EqualTo_nil",
			op:   NewOperationMakeCaseLambdaClosure(2),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationMakeCaseLambdaClosure
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
