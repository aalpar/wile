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

package machine

import (
	"context"
	"errors"
	"sync"
	"sync/atomic"
	"time"
)

// SpawnContext returns the ctx for work that a computation under ctx starts and
// that runs on after it: an SRFI-18 thread, a spawned child process. A
// with-timeout bounds its thunk's computation, not the work the thunk starts:
// that work outlives the thunk's normal return and any escape from it. It ends
// when one of the timers armed around the start expires, when the ctx below them
// all ends (the engine's, the host's, a starting thread's own), or when a ctx
// layered above a timer ends on its own; the ending ctx's Err() and cause are
// passed on unchanged, so a deadline stays context.DeadlineExceeded. The values
// of ctx are kept. With no timer armed, ctx itself is returned.
//
// release ends the returned ctx and drops what it registered on the ctxs it
// watches. Call it when the work never started, or once nothing can derive from
// the ctx any more (a reaped child); not when a thread ends, since what the
// thread started derives from its ctx and outlives it. It may be called more
// than once.
//
// Deriving the work from ctx itself let the finalizer's cancel of a normal
// return end it with a plain context.Canceled.
func SpawnContext(ctx context.Context) (context.Context, func()) {
	var timers []context.Context
	base := ctx
	for {
		t, ok := base.Value(timerCtxKey{}).(*timerState)
		if !ok {
			break
		}
		timers = append(timers, t.ctx)
		base = t.savedCtx
	}
	if len(timers) == 0 {
		return ctx, func() {}
	}
	causeCtx, setCause := context.WithCancelCause(context.Background())
	q := &spawnCtx{values: ctx, base: base, causeCtx: causeCtx, setCause: setCause, done: make(chan struct{})}
	liveSpawnContexts.Add(1)
	// The starter's ctx ends with whatever ended it, Err and cause copied down
	// from the ctx that ended (an expiry, the base's ending, a layer above a
	// timer that a host cancelled), except when it ended because a timer was left.
	// Once it has ended, the base and the outer timers' expiries still reach the
	// work through their own watches.
	q.watch(ctx, func() bool {
		return !errors.Is(context.Cause(ctx), errTimerLeft)
	})
	q.watch(base, func() bool {
		return true
	})
	for _, tc := range timers {
		// An outer expiry reaches an inner timer's ctx with its own cause, so
		// each check sees whichever expiry ended that timer.
		q.watch(tc, func() bool {
			return expiredTimer(tc) != nil
		})
	}
	return q, func() {
		q.end(context.Canceled, context.Canceled)
	}
}

// liveSpawnContexts counts the spawn contexts not yet ended, the witness that
// release is called (export_spawn_context_test.go).
var liveSpawnContexts atomic.Int64

// spawnCtx is SpawnContext's ctx. Its Err is the ending ctx's Err, which no
// context.WithCancelCause child can report (its Err is always Canceled), so it
// is a Context of its own. causeCtx is a plain cancel ctx that Value hands to
// context.Cause, and whose cancellation runs AfterFunc callbacks, including the
// one a context.WithCancel child of q registers: that child then copies q's Err
// and cause. done is q's own channel, not causeCtx's, so such a child is not
// linked to causeCtx directly, which would give it causeCtx's Canceled.
type spawnCtx struct {
	values   context.Context
	base     context.Context
	causeCtx context.Context
	setCause context.CancelCauseFunc
	done     chan struct{}

	mu    sync.Mutex
	err   error
	stops []func() bool
}

// watch ends q with c's ending when c ends and forward says to.
func (p *spawnCtx) watch(c context.Context, forward func() bool) {
	stop := context.AfterFunc(c, func() {
		if forward() {
			p.end(c.Err(), context.Cause(c))
		}
	})
	p.mu.Lock()
	defer p.mu.Unlock()
	if p.err != nil {
		stop()
		return
	}
	p.stops = append(p.stops, stop)
}

// end records err and cause as q's ending, the first time it is called.
func (p *spawnCtx) end(err, cause error) {
	p.mu.Lock()
	if p.err != nil {
		p.mu.Unlock()
		return
	}
	p.err = err
	stops := p.stops
	p.stops = nil
	p.mu.Unlock()
	// Counted down before anything can observe the ending.
	liveSpawnContexts.Add(-1)
	p.setCause(cause)
	close(p.done)
	for _, stop := range stops {
		stop()
	}
}

func (p *spawnCtx) Deadline() (time.Time, bool) {
	return p.base.Deadline()
}

func (p *spawnCtx) Done() <-chan struct{} {
	return p.done
}

func (p *spawnCtx) Err() error {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.err
}

// Value shadows the timer marker, so work q's work starts in turn does not walk
// past q to the starter's timers.
func (p *spawnCtx) Value(key any) any {
	_, isMarker := key.(timerCtxKey)
	if isMarker {
		return nil
	}
	v := p.causeCtx.Value(key)
	if v != nil {
		return v
	}
	return p.values.Value(key)
}

// AfterFunc lets a context.WithCancel child of q register on q without a
// goroutine of its own.
func (p *spawnCtx) AfterFunc(f func()) func() bool {
	return context.AfterFunc(p.causeCtx, f)
}
