// Copyright 2025 Aaron Alpar
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
	qt "github.com/frankban/quicktest"
	"wile/values"
	"testing"
)

func TestNewStack(t *testing.T) {
	q := NewStack()
	qt.Assert(t, q, qt.IsNotNil)
	qt.Assert(t, *q, qt.HasLen, 0)
}

func TestStack_Push(t *testing.T) {
	q := NewStack()
	qt.Assert(t, q.Length(), qt.Equals, 0)
	qt.Assert(t, func() { q.Pop() }, qt.PanicMatches, "stack underflow.*")
	q.Push(values.NewInteger(10))
	qt.Assert(t, q.Length(), qt.Equals, 1)
	qt.Assert(t, q.Pop(), values.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, q.Length(), qt.Equals, 0)
}

func TestStack_Clear(t *testing.T) {
	q := NewStack()
	q.Push(values.NewInteger(10))
	qt.Assert(t, q.Length(), qt.Equals, 1)
	q.Clear()
	qt.Assert(t, q.Length(), qt.Equals, 0)
}

func TestStack_Pop(t *testing.T) {
	q := NewStack()
	q.Push(values.NewInteger(10))
	qt.Assert(t, q.Pop(), values.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, q.Length(), qt.Equals, 0)
}

func TestStack_Copy(t *testing.T) {
	q := NewStack()
	q.Push(values.NewInteger(10))
	r := q.Copy()
	r.Push(values.NewInteger(20))
	qt.Assert(t, q.Length(), qt.Equals, 1)
	qt.Assert(t, r.Length(), qt.Equals, 2)
	qt.Assert(t, q.Pop(), values.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, q.Length(), qt.Equals, 0)
	qt.Assert(t, r.Pop(), values.SchemeEquals, values.NewInteger(20))
	qt.Assert(t, r.Pop(), values.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, q.Length(), qt.Equals, 0)
}

func TestStack_Length(t *testing.T) {
	q := NewStack()
	qt.Assert(t, q.Length(), qt.Equals, 0)
	q.Push(values.NewInteger(10))
	qt.Assert(t, q.Length(), qt.Equals, 1)
	q.Push(values.NewInteger(20))
	qt.Assert(t, q.Length(), qt.Equals, 2)
}

func TestStack_PopAll(t *testing.T) {
	q := NewStack()
	q.Push(values.NewInteger(10))
	q.Push(values.NewInteger(20))
	qs := q.PopAll()
	qt.Assert(t, qs, qt.HasLen, 2)
	qt.Assert(t, qs[0], values.SchemeEquals, values.NewInteger(10))
	qt.Assert(t, qs[1], values.SchemeEquals, values.NewInteger(20))
}

func TestStack_Pull(t *testing.T) {
	q := NewStack()
	q.Push(values.NewInteger(10))
	q.Push(values.NewInteger(20))
	r := q.Pull()
	qt.Assert(t, q.Length(), qt.Equals, 1)
	qt.Assert(t, (*q)[0], values.SchemeEquals, values.NewInteger(20))
	qt.Assert(t, r, values.SchemeEquals, values.NewInteger(10))
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
