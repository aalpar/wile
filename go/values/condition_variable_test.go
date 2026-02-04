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

func TestConditionVariable_NewConditionVariable(t *testing.T) {
	cv := NewConditionVariable("test-cv")
	qt.Assert(t, cv, qt.Not(qt.IsNil))
	qt.Assert(t, cv.Name(), qt.Equals, "test-cv")
	qt.Assert(t, cv.ID() > 0, qt.IsTrue)
}

func TestConditionVariable_DefaultName(t *testing.T) {
	cv := NewConditionVariable("")
	qt.Assert(t, strings.HasPrefix(cv.Name(), "condvar-"), qt.IsTrue)
}

func TestConditionVariable_Specific(t *testing.T) {
	cv := NewConditionVariable("test")
	qt.Assert(t, cv.Specific() == nil, qt.IsTrue)

	cv.SetSpecific(NewInteger(42))
	qt.Assert(t, cv.Specific(), SchemeEquals, NewInteger(42))
}

func TestConditionVariable_SignalBroadcast_NoWaiters(t *testing.T) {
	cv := NewConditionVariable("test")
	qt.Assert(t, cv.WaiterCount(), qt.Equals, 0)

	// Signal and Broadcast with no waiters should not panic
	cv.Signal()
	cv.Broadcast()
}

func TestConditionVariable_WaiterCount(t *testing.T) {
	cv := NewConditionVariable("test")
	qt.Assert(t, cv.WaiterCount(), qt.Equals, 0)
}

func TestConditionVariable_IsVoid(t *testing.T) {
	cv := NewConditionVariable("test")
	qt.Assert(t, cv.IsVoid(), qt.IsFalse)

	var nilCV *ConditionVariable
	qt.Assert(t, nilCV.IsVoid(), qt.IsTrue)
}

func TestConditionVariable_EqualTo(t *testing.T) {
	cv1 := NewConditionVariable("a")
	cv2 := NewConditionVariable("b")
	qt.Assert(t, cv1.EqualTo(cv1), qt.IsTrue)
	qt.Assert(t, cv1.EqualTo(cv2), qt.IsFalse)
	qt.Assert(t, cv1.EqualTo(NewInteger(1)), qt.IsFalse)
}

func TestConditionVariable_SchemeString(t *testing.T) {
	cv := NewConditionVariable("my-cv")
	s := cv.SchemeString()
	qt.Assert(t, strings.Contains(s, "condition-variable"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "my-cv"), qt.IsTrue)

	var nilCV *ConditionVariable
	qt.Assert(t, nilCV.SchemeString(), qt.Equals, "#<condition-variable:void>")
}
