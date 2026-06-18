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
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// ErrTimerExpired is the context cause set by with-timeout via
// context.WithTimeoutCause. Interrupt check sites compare against this
// to distinguish timer expiry from external cancellation (e.g. Ctrl+C).
var ErrTimerExpired = werr.NewStaticError("timer expired")

// ErrTimerInterrupt signals that a wall-clock timer has expired.
// It propagates through the Go error return path and is handled by
// the with-timeout primitive or (as a safety net) by RunWithEscapeHandling.
//
// This is a signal, not an exception. It is NOT caught by Scheme exception
// handlers — only by the VM infrastructure that installed the timer.
type ErrTimerInterrupt struct {
	Handler values.Callable
}

func (p *ErrTimerInterrupt) Error() string {
	return "timer interrupt"
}
