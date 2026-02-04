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

// --- RWMutex ---

func TestRWMutex_NewRWMutex(t *testing.T) {
	m := NewRWMutex("test-rw")
	qt.Assert(t, m, qt.Not(qt.IsNil))
	qt.Assert(t, m.Name(), qt.Equals, "test-rw")
	qt.Assert(t, m.ID() > 0, qt.IsTrue)
}

func TestRWMutex_DefaultName(t *testing.T) {
	m := NewRWMutex("")
	qt.Assert(t, strings.HasPrefix(m.Name(), "rwmutex-"), qt.IsTrue)
}

func TestRWMutex_LockUnlock(t *testing.T) {
	m := NewRWMutex("test")
	m.Lock()
	qt.Assert(t, m, qt.Not(qt.IsNil)) // verify lock held
	m.Unlock()
}

func TestRWMutex_RLockRUnlock(t *testing.T) {
	m := NewRWMutex("test")
	m.RLock()
	qt.Assert(t, m, qt.Not(qt.IsNil)) // verify rlock held
	m.RLock()
	qt.Assert(t, m.Name(), qt.Equals, "test") // verify second rlock held
	m.RUnlock()
	m.RUnlock()
}

func TestRWMutex_TryLock(t *testing.T) {
	m := NewRWMutex("test")
	qt.Assert(t, m.TryLock(), qt.IsTrue)
	qt.Assert(t, m.TryLock(), qt.IsFalse) // already locked
	m.Unlock()
}

func TestRWMutex_TryRLock(t *testing.T) {
	m := NewRWMutex("test")
	qt.Assert(t, m.TryRLock(), qt.IsTrue)
	qt.Assert(t, m.TryRLock(), qt.IsTrue) // multiple readers
	m.RUnlock()
	m.RUnlock()
}

func TestRWMutex_IsVoid(t *testing.T) {
	m := NewRWMutex("test")
	qt.Assert(t, m.IsVoid(), qt.IsFalse)

	var nilM *RWMutex
	qt.Assert(t, nilM.IsVoid(), qt.IsTrue)
}

func TestRWMutex_EqualTo(t *testing.T) {
	m1 := NewRWMutex("test")
	m2 := NewRWMutex("test")
	qt.Assert(t, m1.EqualTo(m1), qt.IsTrue)
	qt.Assert(t, m1.EqualTo(m2), qt.IsFalse)
	qt.Assert(t, m1.EqualTo(NewInteger(1)), qt.IsFalse)
}

func TestRWMutex_SchemeString(t *testing.T) {
	m := NewRWMutex("test-rw")
	s := m.SchemeString()
	qt.Assert(t, strings.Contains(s, "rw-mutex"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "test-rw"), qt.IsTrue)

	var nilM *RWMutex
	qt.Assert(t, nilM.SchemeString(), qt.Equals, "#<rw-mutex:void>")
}

// --- Once ---

func TestOnce_NewOnce(t *testing.T) {
	o := NewOnce()
	qt.Assert(t, o, qt.Not(qt.IsNil))
	qt.Assert(t, o.ID() > 0, qt.IsTrue)
}

func TestOnce_Do(t *testing.T) {
	o := NewOnce()
	qt.Assert(t, o.Done(), qt.IsFalse)

	count := 0
	called := o.Do(func() { count++ })
	qt.Assert(t, called, qt.IsTrue)
	qt.Assert(t, count, qt.Equals, 1)
	qt.Assert(t, o.Done(), qt.IsTrue)

	// Second call should not execute
	called = o.Do(func() { count++ })
	qt.Assert(t, called, qt.IsFalse)
	qt.Assert(t, count, qt.Equals, 1)
}

func TestOnce_IsVoid(t *testing.T) {
	o := NewOnce()
	qt.Assert(t, o.IsVoid(), qt.IsFalse)

	var nilO *Once
	qt.Assert(t, nilO.IsVoid(), qt.IsTrue)
}

func TestOnce_EqualTo(t *testing.T) {
	o1 := NewOnce()
	o2 := NewOnce()
	qt.Assert(t, o1.EqualTo(o1), qt.IsTrue)
	qt.Assert(t, o1.EqualTo(o2), qt.IsFalse)
	qt.Assert(t, o1.EqualTo(NewInteger(1)), qt.IsFalse)
}

func TestOnce_SchemeString(t *testing.T) {
	o := NewOnce()
	s := o.SchemeString()
	qt.Assert(t, strings.Contains(s, "once"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "pending"), qt.IsTrue)

	o.Do(func() {})
	s2 := o.SchemeString()
	qt.Assert(t, strings.Contains(s2, "done"), qt.IsTrue)

	var nilO *Once
	qt.Assert(t, nilO.SchemeString(), qt.Equals, "#<once:void>")
}

// --- WaitGroup ---

func TestWaitGroup_NewWaitGroup(t *testing.T) {
	wg := NewWaitGroup()
	qt.Assert(t, wg, qt.Not(qt.IsNil))
	qt.Assert(t, wg.ID() > 0, qt.IsTrue)
}

func TestWaitGroup_AddDoneWait(t *testing.T) {
	wg := NewWaitGroup()
	wg.Add(1)

	done := make(chan struct{})
	go func() {
		wg.Wait()
		close(done)
	}()

	wg.Done()
	<-done // should not hang
}

func TestWaitGroup_IsVoid(t *testing.T) {
	wg := NewWaitGroup()
	qt.Assert(t, wg.IsVoid(), qt.IsFalse)

	var nilWG *WaitGroup
	qt.Assert(t, nilWG.IsVoid(), qt.IsTrue)
}

func TestWaitGroup_EqualTo(t *testing.T) {
	wg1 := NewWaitGroup()
	wg2 := NewWaitGroup()
	qt.Assert(t, wg1.EqualTo(wg1), qt.IsTrue)
	qt.Assert(t, wg1.EqualTo(wg2), qt.IsFalse)
	qt.Assert(t, wg1.EqualTo(NewInteger(1)), qt.IsFalse)
}

func TestWaitGroup_SchemeString(t *testing.T) {
	wg := NewWaitGroup()
	s := wg.SchemeString()
	qt.Assert(t, strings.Contains(s, "wait-group"), qt.IsTrue)

	var nilWG *WaitGroup
	qt.Assert(t, nilWG.SchemeString(), qt.Equals, "#<wait-group:void>")
}
