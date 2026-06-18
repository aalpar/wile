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

func TestOperationSaveContMark_Apply(t *testing.T) {
	c := qt.New(t)
	mc := newContMarkTestContext()

	key := values.NewSymbol("k")

	// Pre-existing mark
	mc.SetMark(key, values.NewInteger(1))

	// Stack: [key], value register: new val
	mc.evals.Push(key)
	mc.SetValue(values.NewInteger(2))

	op := NewOperationSaveContMark()
	result, err := op.Apply(mc)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, mc)

	// Mark updated
	c.Assert(mc.GetMark(key), qt.Equals, values.NewInteger(2))
	// Stack: [key, old_value] — 2 entries saved
	c.Assert(mc.evals.Len(), qt.Equals, 2)
	// PC advanced
	c.Assert(mc.pc, qt.Equals, 1)
}

func TestOperationSaveContMark_NoExisting(t *testing.T) {
	c := qt.New(t)
	mc := newContMarkTestContext()

	key := values.NewSymbol("k")

	// No pre-existing mark
	mc.evals.Push(key)
	mc.SetValue(values.NewInteger(2))

	op := NewOperationSaveContMark()
	_, err := op.Apply(mc)
	c.Assert(err, qt.IsNil)

	// Stack: [key, sentinel]
	c.Assert(mc.evals.Len(), qt.Equals, 2)
	// Top of stack should be sentinel
	c.Assert(mc.evals.Pop(), qt.Equals, noMarkSentinel)
}

func TestOperationRestoreContMark_RestoresOld(t *testing.T) {
	c := qt.New(t)
	mc := newContMarkTestContext()

	key := values.NewSymbol("k")

	// Simulate post-body state: stack has [key, old_value]
	mc.evals.Push(key)
	mc.evals.Push(values.NewInteger(1))
	// Current mark was changed by body
	mc.SetMark(key, values.NewInteger(99))

	op := NewOperationRestoreContMark()
	result, err := op.Apply(mc)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, mc)

	// Mark restored to old value
	c.Assert(mc.GetMark(key), qt.Equals, values.NewInteger(1))
	// Stack cleaned
	c.Assert(mc.evals.Len(), qt.Equals, 0)
	// PC advanced
	c.Assert(mc.pc, qt.Equals, 1)
}

func TestOperationRestoreContMark_DeletesWhenSentinel(t *testing.T) {
	c := qt.New(t)
	mc := newContMarkTestContext()

	key := values.NewSymbol("k")

	// Stack has [key, sentinel] — no previous mark existed
	mc.evals.Push(key)
	mc.evals.Push(noMarkSentinel)
	mc.SetMark(key, values.NewInteger(99))

	op := NewOperationRestoreContMark()
	_, err := op.Apply(mc)
	c.Assert(err, qt.IsNil)

	// Mark removed
	c.Assert(mc.GetMark(key), qt.IsNil)
}

func TestOperationContMark_EqualTo(t *testing.T) {
	c := qt.New(t)
	c.Assert(NewOperationSetContMark().EqualTo(NewOperationSetContMark()), qt.IsTrue)
	c.Assert(NewOperationSaveContMark().EqualTo(NewOperationSaveContMark()), qt.IsTrue)
	c.Assert(NewOperationRestoreContMark().EqualTo(NewOperationRestoreContMark()), qt.IsTrue)
	c.Assert(NewOperationSetContMark().EqualTo(NewOperationSaveContMark()), qt.IsFalse)
}
