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
	"context"
	"sync"
)

// waitOnCondCtx parks on cond until it is woken by a Broadcast/Signal OR ctx is
// cancelled, and reports whether waiting should continue: true means woken for a
// reason other than cancellation (recheck the predicate), false means ctx was
// cancelled (stop waiting). The caller MUST hold cond.L and call this inside a
// predicate loop, exactly like sync.Cond.Wait.
//
// This is the single ctx-to-cond bridge for the synchronization value types
// (the SRFI-18 Mutex). It is what lets a thread blocked ACQUIRING one be
// unparked by thread-terminate!'s ctx cancellation instead of stalling on a Go
// sync primitive with no cancellable form. A thread that HOLDS a lock is
// unaffected: this governs the wait side only, so a terminated holder's lock
// stays held (releasing it would expose the resource mid-transition, out of
// serialization order).
//
// sync.Cond has no ctx-aware wait, so cancellation is bridged by a side goroutine
// that Broadcasts when ctx fires. The goroutine takes cond.L before it
// Broadcasts. Because the caller holds cond.L continuously until cond.Wait
// atomically releases it and parks, that Broadcast cannot land before the caller
// is parked — the lost-wakeup that would otherwise hang the waiter forever. The
// goroutine is bounded: it always terminates, at the latest once the caller
// releases cond.L. It may briefly outlive this call while blocked on that lock
// (the caller holds cond.L from Wait's return through its own deferred unlock),
// but it cannot leak.
func waitOnCondCtx(ctx context.Context, cond *sync.Cond) bool {
	if ctx.Err() != nil {
		return false
	}
	done := make(chan struct{})
	go func() {
		select {
		case <-ctx.Done():
			cond.L.Lock()
			cond.Broadcast()
			cond.L.Unlock()
		case <-done:
		}
	}()
	cond.Wait()
	close(done)
	return ctx.Err() == nil
}
