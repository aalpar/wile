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

package values_test

import (
	"runtime"
	"strings"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

// stableGoroutineCount polls runtime.NumGoroutine() until two consecutive
// reads return the same value, or the deadline elapses.
func stableGoroutineCount(deadline time.Duration) int {
	end := time.Now().Add(deadline)
	prev := runtime.NumGoroutine()
	for time.Now().Before(end) {
		runtime.Gosched()
		runtime.GC()
		time.Sleep(1 * time.Millisecond)
		curr := runtime.NumGoroutine()
		if curr == prev {
			return curr
		}
		prev = curr
	}
	return prev
}

func TestConditionVariable_NewConditionVariable(t *testing.T) {
	cv := values.NewConditionVariable("test-cv")
	qt.Assert(t, cv, qt.Not(qt.IsNil))
	qt.Assert(t, cv.Name(), qt.Equals, "test-cv")
	qt.Assert(t, cv.ID() > 0, qt.IsTrue)
}

func TestConditionVariable_DefaultName(t *testing.T) {
	cv := values.NewConditionVariable("")
	qt.Assert(t, strings.HasPrefix(cv.Name(), "condvar-"), qt.IsTrue)
}

func TestConditionVariable_Specific(t *testing.T) {
	cv := values.NewConditionVariable("test")
	qt.Assert(t, cv.Specific() == nil, qt.IsTrue)

	cv.SetSpecific(values.NewInteger(42))
	qt.Assert(t, cv.Specific(), valuestest.SchemeEquals, values.NewInteger(42))
}

func TestConditionVariable_SignalBroadcast_NoWaiters(t *testing.T) {
	cv := values.NewConditionVariable("test")
	qt.Assert(t, cv.WaiterCount(), qt.Equals, 0)

	// Signal and Broadcast with no waiters should not panic
	cv.Signal()
	cv.Broadcast()
}

func TestConditionVariable_WaiterCount(t *testing.T) {
	cv := values.NewConditionVariable("test")
	qt.Assert(t, cv.WaiterCount(), qt.Equals, 0)
}

func TestConditionVariable_IsVoid(t *testing.T) {
	cv := values.NewConditionVariable("test")
	qt.Assert(t, cv.IsVoid(), qt.IsFalse)

	var nilCV *values.ConditionVariable
	qt.Assert(t, nilCV.IsVoid(), qt.IsTrue)
}

func TestConditionVariable_EqualTo(t *testing.T) {
	cv1 := values.NewConditionVariable("a")
	cv2 := values.NewConditionVariable("b")
	qt.Assert(t, cv1.EqualTo(cv1), qt.IsTrue)
	qt.Assert(t, cv1.EqualTo(cv2), qt.IsFalse)
	qt.Assert(t, cv1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestConditionVariable_SchemeString(t *testing.T) {
	cv := values.NewConditionVariable("my-cv")
	s := cv.SchemeString()
	qt.Assert(t, strings.Contains(s, "condition-variable"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "my-cv"), qt.IsTrue)

	var nilCV *values.ConditionVariable
	qt.Assert(t, nilCV.SchemeString(), qt.Equals, "#<condition-variable:void>")
}

func TestConditionVariable_Wait_NoGoroutineLeak(t *testing.T) {
	c := qt.New(t)

	// Measure baseline goroutine count
	baseline := stableGoroutineCount(2 * time.Second)

	cv := values.NewConditionVariable("leak-test")
	timeout := 10 * time.Millisecond

	// Run 100 timeouts (old code would leak 100 goroutines)
	for range 100 {
		signaled := cv.Wait(nil, &timeout)
		c.Assert(signaled, qt.IsFalse)
	}

	// Give goroutines time to exit
	final := stableGoroutineCount(2 * time.Second)
	// Allow small variance (±2) for test framework overhead
	c.Assert(final <= baseline+2, qt.IsTrue,
		qt.Commentf("goroutine leak detected: baseline=%d final=%d", baseline, final))
}

func TestConditionVariable_Wait_SignalBeforeTimeout(t *testing.T) {
	c := qt.New(t)
	cv := values.NewConditionVariable("signal-test")
	timeout := 1 * time.Second

	// Signal after 50ms
	go func() {
		time.Sleep(50 * time.Millisecond)
		cv.Signal()
	}()

	start := time.Now()
	signaled := cv.Wait(nil, &timeout)
	elapsed := time.Since(start)

	c.Assert(signaled, qt.IsTrue)
	c.Assert(elapsed < 500*time.Millisecond, qt.IsTrue,
		qt.Commentf("should wake quickly, took %v", elapsed))
}

func TestConditionVariable_Wait_Timeout(t *testing.T) {
	c := qt.New(t)
	cv := values.NewConditionVariable("timeout-test")
	timeout := 50 * time.Millisecond

	start := time.Now()
	signaled := cv.Wait(nil, &timeout)
	elapsed := time.Since(start)

	c.Assert(signaled, qt.IsFalse)
	c.Assert(elapsed >= timeout, qt.IsTrue)
	c.Assert(elapsed < timeout*2, qt.IsTrue,
		qt.Commentf("timeout should be accurate, elapsed=%v", elapsed))
}

func TestConditionVariable_Wait_BroadcastBeforeTimeout(t *testing.T) {
	c := qt.New(t)
	cv := values.NewConditionVariable("broadcast-test")
	timeout := 1 * time.Second

	go func() {
		time.Sleep(50 * time.Millisecond)
		cv.Broadcast()
	}()

	start := time.Now()
	signaled := cv.Wait(nil, &timeout)
	elapsed := time.Since(start)

	c.Assert(signaled, qt.IsTrue)
	c.Assert(elapsed < 500*time.Millisecond, qt.IsTrue)
}

func TestConditionVariable_Wait_NilTimeout(t *testing.T) {
	c := qt.New(t)
	cv := values.NewConditionVariable("nil-timeout-test")

	go func() {
		time.Sleep(50 * time.Millisecond)
		cv.Signal()
	}()

	signaled := cv.Wait(nil, nil)
	c.Assert(signaled, qt.IsTrue)
}

func TestConditionVariable_Wait_RaceCondition(t *testing.T) {
	cv := values.NewConditionVariable("race-test")
	timeout := 50 * time.Millisecond

	// Signal at ~timeout boundary (creates race)
	go func() {
		time.Sleep(45 * time.Millisecond)
		cv.Signal()
	}()

	// Either true or false is acceptable (depends on scheduler)
	signaled := cv.Wait(nil, &timeout)
	// No assertion on value — just verify no panic/leak
	_ = signaled
}

func TestConditionVariable_Wait_ConcurrentWaiters(t *testing.T) {
	c := qt.New(t)
	cv := values.NewConditionVariable("concurrent-test")
	timeout := 100 * time.Millisecond

	const numWaiters = 50
	results := make(chan bool, numWaiters)

	// Start 50 waiters
	for range numWaiters {
		go func() {
			signaled := cv.Wait(nil, &timeout)
			results <- signaled
		}()
	}

	// Broadcast after 50ms (should wake all)
	time.Sleep(50 * time.Millisecond)
	cv.Broadcast()

	// Collect results
	signaled := 0
	timedout := 0
	for range numWaiters {
		if <-results {
			signaled++
		} else {
			timedout++
		}
	}

	// Most should be signaled (allow some variance for scheduler)
	c.Assert(signaled > numWaiters/2, qt.IsTrue,
		qt.Commentf("signaled=%d timedout=%d", signaled, timedout))
}
