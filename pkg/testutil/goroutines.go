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

package testutil

import (
	"runtime"
	"testing"
	"time"
)

// StableGoroutineCount polls runtime.NumGoroutine() until two consecutive
// reads return the same value, or the deadline elapses. Logs a warning if
// stability is not achieved.
//
// The count is process-wide, so it corroborates a leak claim rather than
// proving one: an unrelated goroutine starting or exiting during the poll
// moves it too. Assert on the resource's own state (a thread's ThreadState, a
// process's ProcessState) and use this only as a second signal.
func StableGoroutineCount(t testing.TB, deadline time.Duration) int {
	t.Helper()
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
	t.Logf("StableGoroutineCount: count did not stabilize within %v (last=%d)", deadline, prev)
	return prev
}
