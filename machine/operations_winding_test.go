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

func TestOperationsWinding(t *testing.T) {
	tcs := []struct {
		name    string
		op      Operation
		checkFn func(t *testing.T, op Operation)
	}{
		// --- PushWind ---
		{
			name: "PushWind/constructor",
			op:   NewOperationPushWind(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "PushWind/SchemeString",
			op:   NewOperationPushWind(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "push-wind")
			},
		},
		{
			name: "PushWind/IsVoid",
			op:   NewOperationPushWind(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "PushWind/EqualTo_self",
			op:   NewOperationPushWind(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationPushWind()), qt.IsTrue)
			},
		},
		{
			name: "PushWind/EqualTo_different_type",
			op:   NewOperationPushWind(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "PushWind/EqualTo_nil",
			op:   NewOperationPushWind(),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationPushWind
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		// --- PopWind ---
		{
			name: "PopWind/constructor",
			op:   NewOperationPopWind(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "PopWind/SchemeString",
			op:   NewOperationPopWind(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "pop-wind")
			},
		},
		{
			name: "PopWind/IsVoid",
			op:   NewOperationPopWind(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "PopWind/EqualTo_self",
			op:   NewOperationPopWind(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationPopWind()), qt.IsTrue)
			},
		},
		{
			name: "PopWind/EqualTo_different_type",
			op:   NewOperationPopWind(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "PopWind/EqualTo_nil",
			op:   NewOperationPopWind(),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationPopWind
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		// --- PopEnv ---
		{
			name: "PopEnv/constructor",
			op:   NewOperationPopEnv(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "PopEnv/SchemeString",
			op:   NewOperationPopEnv(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "pop-env")
			},
		},
		{
			name: "PopEnv/IsVoid",
			op:   NewOperationPopEnv(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "PopEnv/EqualTo_self",
			op:   NewOperationPopEnv(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationPopEnv()), qt.IsTrue)
			},
		},
		{
			name: "PopEnv/EqualTo_different_type",
			op:   NewOperationPopEnv(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "PopEnv/EqualTo_nil",
			op:   NewOperationPopEnv(),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationPopEnv
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
