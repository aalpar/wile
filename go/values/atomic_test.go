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

package values

import (
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// --- AtomicBox ---

func TestAtomicBox_NewAtomicBox(t *testing.T) {
	a := NewAtomicBox(NewInteger(42))
	qt.Assert(t, a, qt.Not(qt.IsNil))
	qt.Assert(t, a.ID() > 0, qt.IsTrue)
}

func TestAtomicBox_LoadStore(t *testing.T) {
	a := NewAtomicBox(NewInteger(1))
	qt.Assert(t, a.Load(), SchemeEquals, NewInteger(1))

	a.Store(NewInteger(2))
	qt.Assert(t, a.Load(), SchemeEquals, NewInteger(2))
}

func TestAtomicBox_NilInitial(t *testing.T) {
	a := NewAtomicBox(nil)
	qt.Assert(t, a.Load() == nil, qt.IsTrue)
}

func TestAtomicBox_Swap(t *testing.T) {
	a := NewAtomicBox(NewInteger(1))
	old := a.Swap(NewInteger(2))
	qt.Assert(t, old, SchemeEquals, NewInteger(1))
	qt.Assert(t, a.Load(), SchemeEquals, NewInteger(2))
}

func TestAtomicBox_SwapFromNil(t *testing.T) {
	a := NewAtomicBox(nil)
	old := a.Swap(NewInteger(1))
	qt.Assert(t, old == nil, qt.IsTrue)
	qt.Assert(t, a.Load(), SchemeEquals, NewInteger(1))
}

func TestAtomicBox_CompareAndSwap(t *testing.T) {
	a := NewAtomicBox(NewInteger(1))

	// Successful CAS
	ok := a.CompareAndSwap(NewInteger(1), NewInteger(2))
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, a.Load(), SchemeEquals, NewInteger(2))

	// Failed CAS (old value doesn't match)
	ok = a.CompareAndSwap(NewInteger(1), NewInteger(3))
	qt.Assert(t, ok, qt.IsFalse)
	qt.Assert(t, a.Load(), SchemeEquals, NewInteger(2))
}

func TestAtomicBox_IsVoid(t *testing.T) {
	a := NewAtomicBox(NewInteger(1))
	qt.Assert(t, a.IsVoid(), qt.IsFalse)

	var nilA *AtomicBox
	qt.Assert(t, nilA.IsVoid(), qt.IsTrue)
}

func TestAtomicBox_EqualTo(t *testing.T) {
	a1 := NewAtomicBox(NewInteger(1))
	a2 := NewAtomicBox(NewInteger(1))
	qt.Assert(t, a1.EqualTo(a1), qt.IsTrue)
	qt.Assert(t, a1.EqualTo(a2), qt.IsFalse)
	qt.Assert(t, a1.EqualTo(NewInteger(1)), qt.IsFalse)
}

func TestAtomicBox_SchemeString(t *testing.T) {
	a := NewAtomicBox(NewInteger(42))
	s := a.SchemeString()
	qt.Assert(t, strings.Contains(s, "atomic"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "42"), qt.IsTrue)

	a2 := NewAtomicBox(nil)
	s2 := a2.SchemeString()
	qt.Assert(t, strings.Contains(s2, "void"), qt.IsTrue)

	var nilA *AtomicBox
	qt.Assert(t, nilA.SchemeString(), qt.Equals, "#<atomic:void>")
}

// --- AtomicInt64 ---

func TestAtomicInt64_NewAtomicInt64(t *testing.T) {
	a := NewAtomicInt64(42)
	qt.Assert(t, a, qt.Not(qt.IsNil))
	qt.Assert(t, a.ID() > 0, qt.IsTrue)
}

func TestAtomicInt64_LoadStore(t *testing.T) {
	a := NewAtomicInt64(1)
	qt.Assert(t, a.Load(), qt.Equals, int64(1))

	a.Store(2)
	qt.Assert(t, a.Load(), qt.Equals, int64(2))
}

func TestAtomicInt64_Add(t *testing.T) {
	a := NewAtomicInt64(10)
	result := a.Add(5)
	qt.Assert(t, result, qt.Equals, int64(15))
	qt.Assert(t, a.Load(), qt.Equals, int64(15))

	result = a.Add(-3)
	qt.Assert(t, result, qt.Equals, int64(12))
}

func TestAtomicInt64_Swap(t *testing.T) {
	a := NewAtomicInt64(1)
	old := a.Swap(2)
	qt.Assert(t, old, qt.Equals, int64(1))
	qt.Assert(t, a.Load(), qt.Equals, int64(2))
}

func TestAtomicInt64_CompareAndSwap(t *testing.T) {
	a := NewAtomicInt64(1)

	ok := a.CompareAndSwap(1, 2)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, a.Load(), qt.Equals, int64(2))

	ok = a.CompareAndSwap(1, 3)
	qt.Assert(t, ok, qt.IsFalse)
	qt.Assert(t, a.Load(), qt.Equals, int64(2))
}

func TestAtomicInt64_IsVoid(t *testing.T) {
	a := NewAtomicInt64(0)
	qt.Assert(t, a.IsVoid(), qt.IsFalse)

	var nilA *AtomicInt64
	qt.Assert(t, nilA.IsVoid(), qt.IsTrue)
}

func TestAtomicInt64_EqualTo(t *testing.T) {
	a1 := NewAtomicInt64(1)
	a2 := NewAtomicInt64(1)
	qt.Assert(t, a1.EqualTo(a1), qt.IsTrue)
	qt.Assert(t, a1.EqualTo(a2), qt.IsFalse)
	qt.Assert(t, a1.EqualTo(NewInteger(1)), qt.IsFalse)
}

func TestAtomicInt64_SchemeString(t *testing.T) {
	a := NewAtomicInt64(42)
	s := a.SchemeString()
	qt.Assert(t, strings.Contains(s, "atomic-int64"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "42"), qt.IsTrue)

	var nilA *AtomicInt64
	qt.Assert(t, nilA.SchemeString(), qt.Equals, "#<atomic-int64:void>")
}
