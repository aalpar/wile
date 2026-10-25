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
	"testing"
	"time"

	qt "github.com/frankban/quicktest"
)

// armTestTimer builds a timer ctx over parent as RunBodyUnderTimer does on the
// wall clock (newTimerContext, which reads no context state when parent carries
// no test clock): a deadline-backed ctx whose expiry names the timer, carrying
// the marker SpawnContext walks.
func armTestTimer(parent context.Context, d time.Duration) (context.Context, context.CancelFunc) {
	t := &timerState{savedCtx: parent}
	var p *MachineContext
	raw, cancel := p.newTimerContext(parent, d, &timerExpiry{timer: t})
	t.ctx = context.WithValue(raw, timerCtxKey{}, t)
	return t.ctx, cancel
}

// TestSpawnContextKeepsTheEndingsClass pins that work started under a timer ends
// with the ctx.Err() of whatever ended it, as when it derived from the timer's
// ctx: a wall-clock expiry and a host deadline are context.DeadlineExceeded, a
// host cancel is context.Canceled, never both, so a host can still tell them
// apart (werr.ContextEnding). The work's own ctx is a context.WithCancel child,
// as Thread.Start makes, and it must copy the class and the cause. Each row
// waits for the ending; nothing races a timer.
func TestSpawnContextKeepsTheEndingsClass(t *testing.T) {
	tcs := []struct {
		name      string
		arm       func() (context.Context, func())
		wantErr   error
		wantOther error
		expiry    bool
	}{
		{
			name: "the timer expires",
			arm: func() (context.Context, func()) {
				tc, cancel := armTestTimer(context.Background(), time.Millisecond)
				return tc, cancel
			},
			wantErr:   context.DeadlineExceeded,
			wantOther: context.Canceled,
			expiry:    true,
		},
		{
			name: "the host's deadline passes",
			arm: func() (context.Context, func()) {
				host, cancelHost := context.WithTimeout(context.Background(), time.Millisecond)
				tc, cancel := armTestTimer(host, time.Hour)
				return tc, func() {
					cancel()
					cancelHost()
				}
			},
			wantErr:   context.DeadlineExceeded,
			wantOther: context.Canceled,
		},
		{
			name: "the host cancels",
			arm: func() (context.Context, func()) {
				host, cancelHost := context.WithCancel(context.Background())
				tc, cancel := armTestTimer(host, time.Hour)
				cancelHost()
				return tc, cancel
			},
			wantErr:   context.Canceled,
			wantOther: context.DeadlineExceeded,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			timerCtx, cleanup := tc.arm()
			defer cleanup()
			q, release := SpawnContext(timerCtx)
			defer release()
			work, cancelWork := context.WithCancel(q)
			defer cancelWork()
			<-work.Done()
			for _, got := range []context.Context{q, work} {
				c.Assert(got.Err(), qt.ErrorIs, tc.wantErr)
				c.Assert(errors.Is(got.Err(), tc.wantOther), qt.IsFalse)
				c.Assert(expiredTimer(got) != nil, qt.Equals, tc.expiry, qt.Commentf("cause %v", context.Cause(got)))
			}
		})
	}
}

// TestSpawnContextHidesTheTimerMarker pins that the work's ctx does not carry
// the starter's timer marker, so work it starts in turn finds no timer and
// derives from the work's ctx directly: terminating a thread then ends the
// threads it started. With the marker visible, the walk skipped the thread's
// own ctx and went to the base below the starter's timer.
func TestSpawnContextHidesTheTimerMarker(t *testing.T) {
	c := qt.New(t)
	timerCtx, cancelTimer := armTestTimer(context.Background(), time.Hour)
	defer cancelTimer()
	q, release := SpawnContext(timerCtx)
	defer release()
	c.Assert(q.Value(timerCtxKey{}), qt.IsNil)
	work, cancelWork := context.WithCancel(q)
	defer cancelWork()
	inner, releaseInner := SpawnContext(work)
	defer releaseInner()
	c.Assert(inner, qt.Equals, work)
}
