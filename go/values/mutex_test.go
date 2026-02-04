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

func TestMutex_NewMutex(t *testing.T) {
	m := NewMutex("test-mutex")
	qt.Assert(t, m, qt.Not(qt.IsNil))
	qt.Assert(t, m.Name(), qt.Equals, "test-mutex")
	qt.Assert(t, m.ID() > 0, qt.IsTrue)
	qt.Assert(t, m.State(), qt.Equals, MutexUnlocked)
}

func TestMutex_DefaultName(t *testing.T) {
	m := NewMutex("")
	qt.Assert(t, strings.HasPrefix(m.Name(), "mutex-"), qt.IsTrue)
}

func TestMutex_Specific(t *testing.T) {
	m := NewMutex("test")
	qt.Assert(t, m.Specific() == nil, qt.IsTrue)

	m.SetSpecific(NewInteger(42))
	qt.Assert(t, m.Specific(), SchemeEquals, NewInteger(42))
}

func TestMutexState_String(t *testing.T) {
	tcs := []struct {
		state MutexState
		str   string
	}{
		{MutexUnlocked, "not-owned"},
		{MutexLockedOwned, "owned"},
		{MutexLockedNotOwned, "not-owned"},
		{MutexAbandoned, "abandoned"},
		{MutexState(99), "unknown"},
	}
	for _, tc := range tcs {
		t.Run(tc.str, func(t *testing.T) {
			qt.Assert(t, tc.state.String(), qt.Equals, tc.str)
		})
	}
}

func TestMutex_LockUnlock_NoOwner(t *testing.T) {
	m := NewMutex("test")

	ok, err := m.Lock(nil, nil)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, m.State(), qt.Equals, MutexLockedNotOwned)
	qt.Assert(t, m.Owner() == nil, qt.IsTrue)

	ok = m.Unlock(nil, nil)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, m.State(), qt.Equals, MutexUnlocked)
}

func TestMutex_LockUnlock_WithOwner(t *testing.T) {
	m := NewMutex("test")
	th := NewThread(NewSymbol("thunk"), "owner")

	ok, err := m.Lock(nil, th)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, m.State(), qt.Equals, MutexLockedOwned)
	qt.Assert(t, m.Owner(), qt.Equals, th)

	m.Unlock(nil, nil)
	qt.Assert(t, m.Owner() == nil, qt.IsTrue)
}

func TestMutex_StateValue(t *testing.T) {
	m := NewMutex("test")

	// Unlocked
	sv := m.StateValue()
	qt.Assert(t, sv, SchemeEquals, NewSymbol("not-owned"))

	// Locked with owner
	th := NewThread(NewSymbol("thunk"), "owner")
	m.Lock(nil, th)
	sv = m.StateValue()
	qt.Assert(t, sv.EqualTo(th), qt.IsTrue)

	// Unlock and mark abandoned
	m.Unlock(nil, nil)
	m.Lock(nil, th)
	m.MarkAbandoned()
	sv = m.StateValue()
	qt.Assert(t, sv, SchemeEquals, NewSymbol("abandoned"))
}

func TestMutex_MarkAbandoned(t *testing.T) {
	m := NewMutex("test")
	th := NewThread(NewSymbol("thunk"), "owner")
	m.Lock(nil, th)

	m.MarkAbandoned()
	qt.Assert(t, m.State(), qt.Equals, MutexAbandoned)
}

func TestMutex_LockAbandoned(t *testing.T) {
	m := NewMutex("test")
	th := NewThread(NewSymbol("thunk"), "owner")
	m.Lock(nil, th)
	m.MarkAbandoned()

	// Locking an abandoned mutex should succeed but return AbandonedMutexException
	ok, err := m.Lock(nil, nil)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, err, qt.Not(qt.IsNil))

	_, isAbandoned := err.(*AbandonedMutexException)
	qt.Assert(t, isAbandoned, qt.IsTrue)
}

func TestMutex_IsVoid(t *testing.T) {
	m := NewMutex("test")
	qt.Assert(t, m.IsVoid(), qt.IsFalse)

	var nilM *Mutex
	qt.Assert(t, nilM.IsVoid(), qt.IsTrue)
}

func TestMutex_EqualTo(t *testing.T) {
	m1 := NewMutex("a")
	m2 := NewMutex("b")
	qt.Assert(t, m1.EqualTo(m1), qt.IsTrue)
	qt.Assert(t, m1.EqualTo(m2), qt.IsFalse)
	qt.Assert(t, m1.EqualTo(NewInteger(1)), qt.IsFalse)
}

func TestMutex_SchemeString(t *testing.T) {
	m := NewMutex("my-mutex")
	s := m.SchemeString()
	qt.Assert(t, strings.Contains(s, "mutex"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "my-mutex"), qt.IsTrue)

	var nilM *Mutex
	qt.Assert(t, nilM.SchemeString(), qt.Equals, "#<mutex:void>")
}
