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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

func TestOperationSetContMark_Apply(t *testing.T) {
	c := qt.New(t)
	mc := newContMarkTestContext()

	key := values.NewSymbol("k")

	// Stack: [key], value register: val
	mc.evals.Push(key)
	mc.SetValue(values.NewInteger(42))

	op := NewOperationSetContMark()
	result, err := op.Apply(mc)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, mc)

	// Mark set on frame
	c.Assert(mc.GetMark(key), qt.Equals, values.NewInteger(42))
	// Key popped from stack
	c.Assert(mc.evals.Len(), qt.Equals, 0)
	// PC advanced
	c.Assert(mc.pc, qt.Equals, 1)
}

func TestOperationContMark_EqualTo(t *testing.T) {
	c := qt.New(t)
	c.Assert(NewOperationSetContMark().EqualTo(NewOperationSetContMark()), qt.IsTrue)
	c.Assert(NewOperationSetContMark().EqualTo(NewOperationPopWind()), qt.IsFalse)
}
