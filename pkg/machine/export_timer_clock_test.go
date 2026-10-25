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
	"time"
)

// WithTimerFiringAfter returns ctx with a timerClock under which a with-timeout
// fires k instructions after it is armed, whatever its duration: the masked poll
// is aligned to land on the k-th instruction, and the timer's ctx reports Done
// from that instruction on. An instruction that is a foreign call is executing
// when the count is reached, so its post-call recheck fires first; that sweeps
// both fire points. Meant for one with-timeout per run on one context: each
// arming realigns the context's instruction count.
func WithTimerFiringAfter(ctx context.Context, k uint64) context.Context {
	clock := timerClock(func(p *MachineContext, parent context.Context, _ time.Duration, cause error) (context.Context, context.CancelFunc) {
		at := ((p.counters.OpsExecuted + k) | contextCheckMask) + 1
		p.counters.OpsExecuted = at - k
		inner, cancel := context.WithCancelCause(parent)
		q := &opCountContext{Context: inner, cancel: cancel, mc: p, at: at, cause: cause}
		return q, func() {
			cancel(context.Canceled)
		}
	})
	return context.WithValue(ctx, timerClockKey{}, clock)
}

// opCountContext is a timer ctx that expires once mc has counted to at.
type opCountContext struct {
	context.Context
	cancel context.CancelCauseFunc
	mc     *MachineContext
	at     uint64
	cause  error
}

func (p *opCountContext) Done() <-chan struct{} {
	if p.mc.counters.OpsExecuted >= p.at {
		p.cancel(p.cause)
	}
	return p.Context.Done()
}
