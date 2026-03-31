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
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestNewStack(t *testing.T) {
	q := NewStack()
	qt.Assert(t, q, qt.IsNotNil)
	qt.Assert(t, *q, qt.HasLen, 0)
}

func TestStack_Push(t *testing.T) {
	q := NewStack()
	qt.Assert(t, q.Len(), qt.Equals, 0)
	qt.Assert(t, func() { q.Pop() }, qt.PanicMatches, ".*stack underflow.*")
	q.Push(values.NewInteger(10))
	qt.Assert(t, q.Len(), qt.Equals, 1)
	qt.Assert(t, q.Pop(), valuestest.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, q.Len(), qt.Equals, 0)
}

func TestStack_Clear(t *testing.T) {
	q := NewStack()
	q.Push(values.NewInteger(10))
	qt.Assert(t, q.Len(), qt.Equals, 1)
	q.Clear()
	qt.Assert(t, q.Len(), qt.Equals, 0)
}

func TestStack_Pop(t *testing.T) {
	q := NewStack()
	q.Push(values.NewInteger(10))
	qt.Assert(t, q.Pop(), valuestest.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, q.Len(), qt.Equals, 0)
}

func TestStack_Copy(t *testing.T) {
	q := NewStack()
	q.Push(values.NewInteger(10))
	r := q.Copy()
	r.Push(values.NewInteger(20))
	qt.Assert(t, q.Len(), qt.Equals, 1)
	qt.Assert(t, r.Len(), qt.Equals, 2)
	qt.Assert(t, q.Pop(), valuestest.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, q.Len(), qt.Equals, 0)
	qt.Assert(t, r.Pop(), valuestest.SchemeEquals, values.NewInteger(20))
	qt.Assert(t, r.Pop(), valuestest.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, q.Len(), qt.Equals, 0)
}

func TestStack_Length(t *testing.T) {
	q := NewStack()
	qt.Assert(t, q.Len(), qt.Equals, 0)
	q.Push(values.NewInteger(10))
	qt.Assert(t, q.Len(), qt.Equals, 1)
	q.Push(values.NewInteger(20))
	qt.Assert(t, q.Len(), qt.Equals, 2)
}

func TestStack_PopAll(t *testing.T) {
	q := NewStack()
	q.Push(values.NewInteger(10))
	q.Push(values.NewInteger(20))
	qs := q.PopAll()
	qt.Assert(t, qs, qt.HasLen, 2)
	qt.Assert(t, qs[0], valuestest.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, qs[1], valuestest.SchemeEquals, values.NewInteger(20))
}

func TestStack_Drain(t *testing.T) {
	q := NewStack()
	q.Push(values.NewInteger(10))
	q.Push(values.NewInteger(20))
	vs := q.Drain()
	qt.Assert(t, vs, qt.HasLen, 2)
	qt.Assert(t, vs[0], valuestest.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, vs[1], valuestest.SchemeEquals, values.NewInteger(20))
	qt.Assert(t, q.Len(), qt.Equals, 0)
}

func TestStack_DrainEmpty(t *testing.T) {
	q := NewStack()
	vs := q.Drain()
	qt.Assert(t, vs == nil, qt.IsTrue)
	qt.Assert(t, q.Len(), qt.Equals, 0)
}

func TestStack_DrainThenPush(t *testing.T) {
	q := NewStack()
	q.Push(values.NewInteger(1))
	q.Push(values.NewInteger(2))

	vs := q.Drain()
	qt.Assert(t, vs, qt.HasLen, 2)

	// Read the drained values BEFORE pushing (this is the contract).
	qt.Assert(t, vs[0], valuestest.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, vs[1], valuestest.SchemeEquals, values.NewInteger(2))

	// Push overwrites the backing array — view is now invalid.
	q.Push(values.NewInteger(99))
	qt.Assert(t, q.Len(), qt.Equals, 1)
	qt.Assert(t, q.Pop(), valuestest.SchemeEquals, values.NewInteger(99))
}

func TestStack_DrainCapIsSealed(t *testing.T) {
	// The returned view's cap equals its len, preventing accidental
	// append from growing into the stack's backing array.
	q := NewStack()
	q.Push(values.NewInteger(1))
	q.Push(values.NewInteger(2))
	vs := q.Drain()
	qt.Assert(t, cap(vs), qt.Equals, len(vs))
}

func TestStack_Pull(t *testing.T) {
	q := NewStack()
	q.Push(values.NewInteger(10))
	q.Push(values.NewInteger(20))
	r := q.Pull()
	qt.Assert(t, q.Len(), qt.Equals, 1)
	qt.Assert(t, (*q)[0], valuestest.SchemeEquals, values.NewInteger(20))
	qt.Assert(t, r, valuestest.SchemeEquals, values.NewInteger(10))
}

func TestStack_AsList(t *testing.T) {
	q := NewStack()
	q.Push(values.NewInteger(10))

	list := q.AsList()
	// AsList is called in the code, so we're testing for coverage
	qt.Assert(t, list, qt.IsNotNil)
}

func TestStack_SchemeString(t *testing.T) {
	q := NewStack()
	q.Push(values.NewInteger(10))
	q.Push(values.NewInteger(20))
	str := q.SchemeString()
	qt.Assert(t, str, qt.Equals, "#<stack (10 20)>")
}

func TestStack_String(t *testing.T) {
	q := NewStack()
	q.Push(values.NewInteger(10))
	q.Push(values.NewInteger(20))
	str := q.String()
	qt.Assert(t, str, qt.Equals, "[10 20]")
}

// Tests moved from coverage_additional_test.go
// TestStackOperations tests Stack AsList and Clear
func TestStackOperations(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))
	s.Push(values.NewInteger(3))

	// Test AsList
	list := s.AsList()
	qt.Assert(t, list, qt.IsNotNil)

	// Verify list structure - AsList returns in push order (last pushed is first)
	pr, ok := list.(*values.Pair)
	qt.Assert(t, ok, qt.IsTrue)
	// Stack order: push 1, push 2, push 3 - AsList builds from top
	qt.Assert(t, pr.Car(), valuestest.SchemeEquals, values.NewInteger(1))

	// Test Clear
	s.Clear()
	qt.Assert(t, s.Len(), qt.Equals, 0)

	// AsList on empty stack
	emptyList := s.AsList()
	qt.Assert(t, emptyList, valuestest.SchemeEquals, values.EmptyList)

	// Single element stack
	s.Push(values.NewInteger(42))
	singleList := s.AsList()
	qt.Assert(t, singleList, qt.IsNotNil)
}

// TestStackMoreOperations tests more stack operations
func TestStackMoreOperations(t *testing.T) {
	s := NewStack()

	// Push multiple values
	for i := range 10 {
		s.Push(values.NewInteger(int64(i)))
	}
	qt.Assert(t, s.Len(), qt.Equals, 10)

	// Pop all
	for i := 9; i >= 0; i-- {
		v := s.Pop()
		qt.Assert(t, v, valuestest.SchemeEquals, values.NewInteger(int64(i)))
	}
	qt.Assert(t, s.Len(), qt.Equals, 0)

	// PeekK to peek at stack
	s.Push(values.NewInteger(42))
	v := s.PeekK(0) // PeekK(0) gets top of stack
	qt.Assert(t, v, valuestest.SchemeEquals, values.NewInteger(42))
	qt.Assert(t, s.Len(), qt.Equals, 1) // Still there
}

// TestCopyStack tests stack copy
func TestCopyStack(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))

	// Copy
	cpy := s.Copy()
	qt.Assert(t, cpy.Len(), qt.Equals, 2)

	// Modify original doesn't affect copy
	s.Push(values.NewInteger(3))
	qt.Assert(t, s.Len(), qt.Equals, 3)
	qt.Assert(t, cpy.Len(), qt.Equals, 2)
}

// TestStackPull tests stack Pull operation
func TestStackPull(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))
	s.Push(values.NewInteger(3))

	// Pull removes from front (FIFO)
	v := s.Pull()
	qt.Assert(t, v, valuestest.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, s.Len(), qt.Equals, 2)
}

func TestStack_PullDrain(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))
	s.Push(values.NewInteger(3))

	proc, args := s.PullDrain()
	qt.Assert(t, proc, valuestest.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, len(args), qt.Equals, 2)
	qt.Assert(t, args[0], valuestest.SchemeEquals, values.NewInteger(2))
	qt.Assert(t, args[1], valuestest.SchemeEquals, values.NewInteger(3))
	qt.Assert(t, s.Len(), qt.Equals, 0)
}

func TestStack_PullDrainSingle(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(42))

	proc, args := s.PullDrain()
	qt.Assert(t, proc, valuestest.SchemeEquals, values.NewInteger(42))
	qt.Assert(t, args == nil, qt.IsTrue)
	qt.Assert(t, s.Len(), qt.Equals, 0)
}

func TestStack_PullDrainEmpty(t *testing.T) {
	s := NewStack()
	qt.Assert(t, func() { s.PullDrain() }, qt.PanicMatches, `.*stack is empty.*`)
}

// TestStackPopN tests stack PopN operation
func TestStackPopN(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))
	s.Push(values.NewInteger(3))
	s.Push(values.NewInteger(4))
	s.Push(values.NewInteger(5))

	// Pop 3 elements from top
	vs := s.PopN(3)
	qt.Assert(t, len(vs), qt.Equals, 3)
	qt.Assert(t, s.Len(), qt.Equals, 2) // 2 elements remain

	// PopN returns elements in stack order (bottom to top of the popped range)
	qt.Assert(t, vs[0], valuestest.SchemeEquals, values.NewInteger(3)) // Bottom of popped range
	qt.Assert(t, vs[1], valuestest.SchemeEquals, values.NewInteger(4))
	qt.Assert(t, vs[2], valuestest.SchemeEquals, values.NewInteger(5)) // Top of stack (last popped)

	// Remaining elements
	qt.Assert(t, s.Pop(), valuestest.SchemeEquals, values.NewInteger(2))
	qt.Assert(t, s.Pop(), valuestest.SchemeEquals, values.NewInteger(1))
}

// TestStackPopN_EdgeCases tests PopN edge cases
func TestStackPopN_EdgeCases(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))

	// PopN(0) should return nil
	vs := s.PopN(0)
	qt.Assert(t, vs, qt.IsNil)
	qt.Assert(t, s.Len(), qt.Equals, 2)

	// PopN with negative count should panic
	qt.Assert(t, func() { s.PopN(-1) }, qt.PanicMatches, ".*negative count.*")

	// PopN more than available should panic
	qt.Assert(t, func() { s.PopN(3) }, qt.PanicMatches, ".*requested 3 elements from stack of length 2.*")
}

// TestStackPopAll tests stack PopAll operation
func TestStackPopAll(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))
	s.Push(values.NewInteger(3))

	vs := s.PopAll()
	qt.Assert(t, len(vs), qt.Equals, 3)
	qt.Assert(t, s.Len(), qt.Equals, 0) // Stack is empty after PopAll
}

// TestStackClear tests stack Clear operation
func TestStackClear(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))
	qt.Assert(t, s.Len(), qt.Equals, 2)

	s.Clear()
	qt.Assert(t, s.Len(), qt.Equals, 0)

	// Clear on nil should not panic
	var nilStack *Stack
	nilStack.Clear() // Should not panic
}

// TestStackAsList tests stack AsList conversion
func TestStackAsList(t *testing.T) {
	// Create stack with single element
	s := NewStack(values.NewInteger(1))

	list := s.AsList()
	qt.Assert(t, list, qt.IsNotNil)
	// AsList returns a list containing the stack element
	qt.Assert(t, list.SchemeString(), qt.Contains, "1")
}

// TestStackAsListEmpty tests stack AsList with empty/nil stack
func TestStackAsListEmpty(t *testing.T) {
	// NewStack() creates a nil slice, which returns nil pair
	s := NewStack()
	list := s.AsList()
	qt.Assert(t, list, qt.IsNil) // nil stack returns nil pair

	// Test with actual empty slice (after Push/Pop)
	s2 := NewStack(values.NewInteger(1))
	s2.Pop()
	list2 := s2.AsList()
	qt.Assert(t, list2, valuestest.SchemeEquals, values.EmptyList) // empty (non-nil) slice returns EmptyList
}

// TestStackSchemeString tests stack SchemeString
func TestStackSchemeString(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))

	str := s.SchemeString()
	qt.Assert(t, str, qt.Contains, "#<stack")
	qt.Assert(t, str, qt.Contains, "1")
	qt.Assert(t, str, qt.Contains, "2")
}

// TestStackNilSchemeString tests stack SchemeString with nil stack
func TestStackNilSchemeString(t *testing.T) {
	var s Stack
	str := s.SchemeString()
	qt.Assert(t, str, qt.Equals, "#<stack ()>")
}

// TestStackIsVoid tests stack IsVoid
func TestStackIsVoid(t *testing.T) {
	var s Stack // nil slice
	qt.Assert(t, s.IsVoid(), qt.IsTrue)

	// NewStack() with no args creates a nil slice (variadic with no args)
	s2 := NewStack()
	qt.Assert(t, s2.IsVoid(), qt.IsTrue) // nil slice is void

	// NewStack with values creates non-nil slice
	s3 := NewStack(values.NewInteger(1))
	qt.Assert(t, s3.IsVoid(), qt.IsFalse)
}

// TestStackString tests stack String method
func TestStackString(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))

	str := s.String()
	qt.Assert(t, str, qt.Contains, "[")
	qt.Assert(t, str, qt.Contains, "1")
	qt.Assert(t, str, qt.Contains, "2")
	qt.Assert(t, str, qt.Contains, "]")
}

// TestStackNilString tests stack String with nil
func TestStackNilString(t *testing.T) {
	var s Stack
	str := s.String()
	qt.Assert(t, str, qt.Equals, "[]")
}

// TestStackPushAll tests stack PushAll
func TestStackPushAll(t *testing.T) {
	s := NewStack()
	s.PushAll([]values.Value{
		values.NewInteger(1),
		values.NewInteger(2),
		values.NewInteger(3),
	})
	qt.Assert(t, s.Len(), qt.Equals, 3)
}

// TestStackPeekKMultiple tests PeekK at different positions
func TestStackPeekKMultiple(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))
	s.Push(values.NewInteger(3))

	// PeekK(0) is top of stack (last pushed)
	qt.Assert(t, s.PeekK(0), valuestest.SchemeEquals, values.NewInteger(3))
	// PeekK(1) is one below top
	qt.Assert(t, s.PeekK(1), valuestest.SchemeEquals, values.NewInteger(2))
	// PeekK(2) is bottom
	qt.Assert(t, s.PeekK(2), valuestest.SchemeEquals, values.NewInteger(1))
}

// TestStackNilLength tests Length on nil stack
func TestStackNilLength(t *testing.T) {
	var s Stack
	qt.Assert(t, s.Len(), qt.Equals, 0)
}

// TestStackCopyMethod tests Stack.Copy more thoroughly
func TestStackCopyMethod(t *testing.T) {
	s := NewStack(values.NewInteger(1), values.NewInteger(2))
	copied := s.Copy()

	qt.Assert(t, s.Len(), qt.Equals, copied.Len())

	// Modify original, copied should be unchanged
	s.Push(values.NewInteger(3))
	qt.Assert(t, s.Len(), qt.Equals, 3)
	qt.Assert(t, copied.Len(), qt.Equals, 2)
}

// TestStackPeekKBoundary tests Stack.PeekK edge cases
func TestStackPeekKBoundary(t *testing.T) {
	s := NewStack(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))

	// Peek at different positions
	qt.Assert(t, s.PeekK(0), valuestest.SchemeEquals, values.NewInteger(3)) // top
	qt.Assert(t, s.PeekK(1), valuestest.SchemeEquals, values.NewInteger(2)) // second
	qt.Assert(t, s.PeekK(2), valuestest.SchemeEquals, values.NewInteger(1)) // third/bottom
}

// TestStackPopAllThenPush verifies that Push works after PopAll
// (PopAll copies data out and retains the backing array for reuse).
func TestStackPopAllThenPush(t *testing.T) {
	s := NewStack()
	s.Push(values.NewInteger(1))
	s.Push(values.NewInteger(2))

	got := s.PopAll()
	qt.Assert(t, len(got), qt.Equals, 2)
	qt.Assert(t, s.Len(), qt.Equals, 0)

	// Push after PopAll must work and must not corrupt the returned slice.
	s.Push(values.NewInteger(99))
	qt.Assert(t, s.Len(), qt.Equals, 1)
	qt.Assert(t, s.Pop(), valuestest.SchemeEquals, values.NewInteger(99))

	// Original slice unchanged.
	qt.Assert(t, got[0], valuestest.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, got[1], valuestest.SchemeEquals, values.NewInteger(2))
}

// TestStackPeekKPanics tests that Stack.PeekK panics on invalid indices
func TestStackPeekKPanics(t *testing.T) {
	s := NewStack(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))

	// Negative index should panic
	qt.Assert(t, func() {
		s.PeekK(-1)
	}, qt.PanicMatches, ".*stack underflow.*")

	// Index equal to length should panic
	qt.Assert(t, func() {
		s.PeekK(3)
	}, qt.PanicMatches, ".*stack underflow.*")

	// Index greater than length should panic
	qt.Assert(t, func() {
		s.PeekK(100)
	}, qt.PanicMatches, ".*stack underflow.*")

	// Empty stack should panic for any index
	empty := NewStack()
	qt.Assert(t, func() {
		empty.PeekK(0)
	}, qt.PanicMatches, ".*stack underflow.*")
}
