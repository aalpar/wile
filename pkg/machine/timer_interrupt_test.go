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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/werr"
)

// TestWindingThunkFailureMarksOnlyUnderAFiredTimer pins that windingThunkFailure
// leaves a failure alone on a context no timer cancelled: the failure a winding
// thunk returns is the very value the program sees, as before the timer rule.
// Marked whenever the thunk's own context was not cancelled, every winding thunk's
// failure carried the mark, which only the timer rule reads.
func TestWindingThunkFailureMarksOnlyUnderAFiredTimer(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	p := NewMachineContext(context.Background(), NewMachineContinuation(nil, namedTemplate("winding"), env))
	sub := p.NewSubContext()
	defer ReleaseSubContext(sub)
	err := werr.WrapForeignErrorf(werr.ErrNotANumber, "thunk: not a number")
	qt.Assert(t, p.windingThunkFailure(sub, err), qt.Equals, err)
}

// TestWindingThunkFailureMarksForTheInnermostTimer pins which timer a mark records:
// p's innermost at the time. Once that timer has ended, the mark no longer answers
// for p, and a fired timer outside it claims the failure if it reaches p again.
// Recording the outermost kept the mark honoured for as long as any of p's timers
// stayed armed.
func TestWindingThunkFailureMarksForTheInnermostTimer(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	fired, cancel := context.WithCancelCause(context.Background())
	cancel(ErrTimerExpired)
	p := NewMachineContext(fired, NewMachineContinuation(nil, namedTemplate("winding"), env))
	outer := &timerState{}
	inner := &timerState{parent: outer}
	p.timer = inner
	sub := p.NewSubContext()
	defer ReleaseSubContext(sub)
	sub.ctx = context.Background()
	marked := p.windingThunkFailure(sub, werr.WrapForeignErrorf(werr.ErrNotANumber, "thunk: not a number"))
	qt.Assert(t, p.timerClaims(marked), qt.IsFalse)
	p.timer = outer
	qt.Assert(t, p.timerClaims(marked), qt.IsTrue)
}

// TestJudgedUnclaimedReadsEveryMark pins that a failure carrying several marks
// counts as judged by p when any one of them is p's. A failure p marked and then
// passed through a context that marked it again (an Engine.Call root it ran
// outside the fired timer of) carries both, the foreign one outside or inside p's.
// Reading only the outermost mark, or only the innermost, let p's timer claim a
// failure p had judged its own: the handler ran, the escape was lost, and the after
// thunks ran again.
func TestJudgedUnclaimedReadsEveryMark(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	p := NewMachineContext(context.Background(), NewMachineContinuation(nil, namedTemplate("winding"), env))
	own := &timerState{}
	foreign := &timerState{}
	p.timer = own
	err := werr.WrapForeignErrorf(werr.ErrNotANumber, "thunk: not a number")
	tcs := []struct {
		name   string
		marked error
		want   bool
	}{
		{"p's mark", &unclaimedFailure{err: err, timer: own}, true},
		{"p's mark inside another's", &unclaimedFailure{err: &unclaimedFailure{err: err, timer: own}, timer: foreign}, true},
		{"another's mark inside p's", &unclaimedFailure{err: &unclaimedFailure{err: err, timer: foreign}, timer: own}, true},
		{"another's mark", &unclaimedFailure{err: err, timer: foreign}, false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, p.judgedUnclaimed(tc.marked), qt.Equals, tc.want)
		})
	}
}

func TestErrTimerInterrupt_Error(t *testing.T) {
	err := &ErrTimerInterrupt{
		Handler: &ForeignClosure{},
	}
	qt.Assert(t, err.Error(), qt.Equals, "timer interrupt")
}

func TestErrTimerInterrupt_ErrorsAs(t *testing.T) {
	var timerErr *ErrTimerInterrupt
	err := error(&ErrTimerInterrupt{
		Handler: &ForeignClosure{},
	})
	qt.Assert(t, errors.As(err, &timerErr), qt.IsTrue)
	qt.Assert(t, timerErr.Handler, qt.IsNotNil)
}

func TestErrTimerInterrupt_NotMatchOtherErrors(t *testing.T) {
	var timerErr *ErrTimerInterrupt
	err := errors.New("something else")
	qt.Assert(t, errors.As(err, &timerErr), qt.IsFalse)
}

func TestErrTimerInterrupt_NilHandler(t *testing.T) {
	err := &ErrTimerInterrupt{}
	qt.Assert(t, err.Error(), qt.Equals, "timer interrupt")
}
