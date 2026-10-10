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
	"testing"
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
)

// deadlineCounter counts the Deadline questions that reach it.
type deadlineCounter struct {
	context.Context
	calls int
}

func (p *deadlineCounter) Deadline() (time.Time, bool) {
	p.calls++
	return p.Context.Deadline()
}

// TestTimerContextAnswersItsOwnDeadline pins that the context inside a
// with-timeout's extent answers Deadline without asking the timer's context. A
// with-timeout nested under an earlier deadline asks its parent for it, and a
// context that passed the question on made it walk the whole stack of timers below:
// each nesting doubled the walk, and a loop leaving stale timers grew quadratic.
// The timer's context is asked once, when the extent begins.
func TestTimerContextAnswersItsOwnDeadline(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	p := NewMachineContext(context.Background(), NewMachineContinuation(nil, namedTemplate("timer"), env))
	timed, cancel := context.WithTimeout(context.Background(), time.Hour)
	defer cancel()
	counted := &deadlineCounter{Context: timed}
	thunk := NewForeignClosure(env, 0, false, func(CallContext) error {
		return nil
	})
	_, err := p.RunBodyUnderTimer(counted, cancel, thunk, thunk)
	qt.Assert(t, err, qt.IsNil)
	asked := counted.calls
	for range 3 {
		_, ok := p.Context().Deadline()
		qt.Assert(t, ok, qt.IsTrue)
	}
	qt.Assert(t, counted.calls, qt.Equals, asked)
}
