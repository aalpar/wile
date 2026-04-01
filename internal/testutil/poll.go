package testutil

import (
	"runtime"
	"time"
)

// PollUntil calls check() repeatedly until it returns true or deadline elapses.
// Returns true if check() succeeded, false on timeout.
func PollUntil(check func() bool, deadline time.Duration) bool {
	end := time.Now().Add(deadline)
	for time.Now().Before(end) {
		if check() {
			return true
		}
		runtime.Gosched()
		time.Sleep(1 * time.Millisecond)
	}
	return false
}
