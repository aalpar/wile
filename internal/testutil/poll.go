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

// PollUntil calls check() repeatedly until it returns true or deadline
// elapses. Fails the test immediately if the deadline expires without
// check() returning true.
func PollUntil(t testing.TB, check func() bool, deadline time.Duration) {
	t.Helper()
	end := time.Now().Add(deadline)
	for time.Now().Before(end) {
		if check() {
			return
		}
		runtime.Gosched()
		time.Sleep(1 * time.Millisecond)
	}
	t.Fatalf("PollUntil: condition not met within %v", deadline)
}
