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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

func TestOperationsLoadStore(t *testing.T) {
	tcs := []struct {
		name    string
		op      Operation
		checkFn func(t *testing.T, op Operation)
	}{
		// --- LoadLiteral ---
		{
			name: "LoadLiteral/constructor",
			op:   NewOperationLoadLiteralByLiteralIndexImmediate(0),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "LoadLiteral/SchemeString",
			op:   NewOperationLoadLiteralByLiteralIndexImmediate(7),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "load-literal")
				qt.Assert(t, op.SchemeString(), qt.Contains, "7")
			},
		},
		{
			name: "LoadLiteral/IsVoid",
			op:   NewOperationLoadLiteralByLiteralIndexImmediate(0),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "LoadLiteral/EqualTo_same_index",
			op:   NewOperationLoadLiteralByLiteralIndexImmediate(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationLoadLiteralByLiteralIndexImmediate(3)), qt.IsTrue)
			},
		},
		{
			name: "LoadLiteral/EqualTo_different_index",
			op:   NewOperationLoadLiteralByLiteralIndexImmediate(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationLoadLiteralByLiteralIndexImmediate(9)), qt.IsFalse)
			},
		},
		{
			name: "LoadLiteral/EqualTo_different_type",
			op:   NewOperationLoadLiteralByLiteralIndexImmediate(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(3)), qt.IsFalse)
			},
		},
		{
			name: "LoadLiteral/EqualTo_nil",
			op:   NewOperationLoadLiteralByLiteralIndexImmediate(3),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationLoadLiteralByLiteralIndexImmediate
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		// --- LoadLocal ---
		{
			name: "LoadLocal/constructor",
			op:   NewOperationLoadLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "LoadLocal/SchemeString",
			op:   NewOperationLoadLocalByLocalIndexImmediate(environment.NewLocalIndex(1, 2)),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "load-local")
			},
		},
		{
			name: "LoadLocal/IsVoid",
			op:   NewOperationLoadLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "LoadLocal/EqualTo_same_index",
			op:   NewOperationLoadLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationLoadLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0))), qt.IsTrue)
			},
		},
		{
			name: "LoadLocal/EqualTo_different_index",
			op:   NewOperationLoadLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationLoadLocalByLocalIndexImmediate(environment.NewLocalIndex(1, 0))), qt.IsFalse)
			},
		},
		{
			name: "LoadLocal/EqualTo_different_type",
			op:   NewOperationLoadLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "LoadLocal/EqualTo_nil",
			op:   NewOperationLoadLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationLoadLocalByLocalIndexImmediate
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		// --- LoadGlobal ---
		{
			name: "LoadGlobal/constructor",
			op:   NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "LoadGlobal/SchemeString",
			op:   NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(4),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "load-global")
				qt.Assert(t, op.SchemeString(), qt.Contains, "4")
			},
		},
		{
			name: "LoadGlobal/IsVoid",
			op:   NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "LoadGlobal/EqualTo_same_index",
			op:   NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(2),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(2)), qt.IsTrue)
			},
		},
		{
			name: "LoadGlobal/EqualTo_different_index",
			op:   NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(2),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(5)), qt.IsFalse)
			},
		},
		{
			name: "LoadGlobal/EqualTo_different_type",
			op:   NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(2),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(2)), qt.IsFalse)
			},
		},
		{
			name: "LoadGlobal/EqualTo_nil",
			op:   NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(2),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		// --- StoreLocal ---
		{
			name: "StoreLocal/constructor",
			op:   NewOperationStoreLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "StoreLocal/SchemeString",
			op:   NewOperationStoreLocalByLocalIndexImmediate(environment.NewLocalIndex(1, 2)),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "store-local")
			},
		},
		{
			name: "StoreLocal/IsVoid",
			op:   NewOperationStoreLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "StoreLocal/EqualTo_same_index",
			op:   NewOperationStoreLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationStoreLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0))), qt.IsTrue)
			},
		},
		{
			name: "StoreLocal/EqualTo_different_index",
			op:   NewOperationStoreLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationStoreLocalByLocalIndexImmediate(environment.NewLocalIndex(1, 0))), qt.IsFalse)
			},
		},
		{
			name: "StoreLocal/EqualTo_different_type",
			op:   NewOperationStoreLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "StoreLocal/EqualTo_nil",
			op:   NewOperationStoreLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationStoreLocalByLocalIndexImmediate
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		// --- StoreGlobal ---
		{
			name: "StoreGlobal/constructor",
			op:   NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(0),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "StoreGlobal/SchemeString",
			op:   NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(6),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "store-global")
				qt.Assert(t, op.SchemeString(), qt.Contains, "6")
			},
		},
		{
			name: "StoreGlobal/IsVoid",
			op:   NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(0),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "StoreGlobal/EqualTo_same_index",
			op:   NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(4),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(4)), qt.IsTrue)
			},
		},
		{
			name: "StoreGlobal/EqualTo_different_index",
			op:   NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(4),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(8)), qt.IsFalse)
			},
		},
		{
			name: "StoreGlobal/EqualTo_different_type",
			op:   NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(4),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(4)), qt.IsFalse)
			},
		},
		{
			name: "StoreGlobal/EqualTo_nil",
			op:   NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(4),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		// --- LoadVoid ---
		{
			name: "LoadVoid/constructor",
			op:   NewOperationLoadVoid(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "LoadVoid/SchemeString",
			op:   NewOperationLoadVoid(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Equals, "#<machine-operation-load-void>")
			},
		},
		{
			name: "LoadVoid/IsVoid",
			op:   NewOperationLoadVoid(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "LoadVoid/EqualTo_self",
			op:   NewOperationLoadVoid(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationLoadVoid()), qt.IsTrue)
			},
		},
		{
			name: "LoadVoid/EqualTo_different_type",
			op:   NewOperationLoadVoid(),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "LoadVoid/EqualTo_nil",
			op:   NewOperationLoadVoid(),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationLoadVoid
				qt.Assert(t, op.EqualTo(nilOp), qt.IsFalse)
			},
		},
		// --- LoadCachedBinding ---
		{
			name: "LoadCachedBinding/constructor",
			op:   NewOperationLoadCachedBinding(0),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op, qt.IsNotNil)
			},
		},
		{
			name: "LoadCachedBinding/SchemeString",
			op:   NewOperationLoadCachedBinding(5),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.SchemeString(), qt.Contains, "load-cached-binding")
				qt.Assert(t, op.SchemeString(), qt.Contains, "5")
			},
		},
		{
			name: "LoadCachedBinding/IsVoid",
			op:   NewOperationLoadCachedBinding(0),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "LoadCachedBinding/EqualTo_same_index",
			op:   NewOperationLoadCachedBinding(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationLoadCachedBinding(3)), qt.IsTrue)
			},
		},
		{
			name: "LoadCachedBinding/EqualTo_different_index",
			op:   NewOperationLoadCachedBinding(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(NewOperationLoadCachedBinding(7)), qt.IsFalse)
			},
		},
		{
			name: "LoadCachedBinding/EqualTo_different_type",
			op:   NewOperationLoadCachedBinding(3),
			checkFn: func(t *testing.T, op Operation) {
				qt.Assert(t, op.EqualTo(values.NewInteger(3)), qt.IsFalse)
			},
		},
		{
			name: "LoadCachedBinding/EqualTo_nil",
			op:   NewOperationLoadCachedBinding(3),
			checkFn: func(t *testing.T, op Operation) {
				var nilOp *OperationLoadCachedBinding
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
