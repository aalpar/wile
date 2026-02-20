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

func TestNewOperationDrop(t *testing.T) {
	c := qt.New(t)

	op := NewOperationDrop()
	c.Assert(op, qt.IsNotNil)
}

func TestOperationDrop_SchemeString(t *testing.T) {
	c := qt.New(t)

	op := NewOperationDrop()
	c.Assert(op.SchemeString(), qt.Equals, "#<machine-operation-drop>")
}

func TestOperationDrop_IsVoid(t *testing.T) {
	c := qt.New(t)

	op := NewOperationDrop()
	c.Assert(op.IsVoid(), qt.IsFalse)
}

func TestOperationDrop_EqualTo(t *testing.T) {
	c := qt.New(t)

	op1 := NewOperationDrop()
	op2 := NewOperationDrop()

	c.Assert(op1.EqualTo(op2), qt.IsTrue)
	c.Assert(op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestOperationDrop_EqualTo_NilCases(t *testing.T) {
	c := qt.New(t)

	op := NewOperationDrop()
	var nilOp *OperationDrop

	c.Assert(op.EqualTo(nilOp), qt.IsFalse)
	c.Assert(nilOp.EqualTo(op), qt.IsFalse)
	c.Assert(nilOp.EqualTo(nilOp), qt.IsTrue)
}
