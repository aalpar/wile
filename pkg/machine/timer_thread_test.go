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

package machine_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// TestThreadStartedInATimedThunkEndsOnlyByExpiry pins which ends of a
// with-timeout reach a thread its thunk started. The thread meets the main thread
// at a rendezvous (so it is parked at its wait before the thunk goes on), then
// waits on m, which the main thread holds. A thunk that returns, or is escaped
// from, leaves the thread running: it acquires m once the main thread releases it.
// An expiry of the timer around the start cancels it with the expiry as the
// cause, and so does an outer timer's expiry after an inner thunk returned. The
// expiry rows never release m, so the thread can end only by its cancellation; a
// wait cancelled by an expiry names no cause, one cancelled by a normal return
// ends ": context canceled". Until the thread's ctx was taken off the timer's,
// every row ended the thread, the returning ones with that plain cancellation.
func TestThreadStartedInATimedThunkEndsOnlyByExpiry(t *testing.T) {
	const start = `(mutex-lock! rm) (thread-start! t) (mutex-unlock! rm rcv)`
	tcs := []struct {
		name    string
		h       string
		release string
		want    string
	}{
		{
			name:    "the thunk returns",
			h:       `(with-timeout 1000 (lambda (k) 'timed-out) (lambda () ` + start + ` 'thunk-returned))`,
			release: `(mutex-unlock! m)`,
			want:    `(thunk-returned thread-value)`,
		},
		{
			name: "the thunk is escaped from",
			h: `(call/cc (lambda (out)
  (with-timeout 1000 (lambda (k) 'timed-out) (lambda () ` + start + ` (out 'escaped)))))`,
			release: `(mutex-unlock! m)`,
			want:    `(escaped thread-value)`,
		},
		{
			name:    "the thunk returns, the thread started inside eval",
			h:       `(with-timeout 1000 (lambda (k) 'timed-out) (lambda () (eval '(begin ` + start + `) (interaction-environment)) 'thunk-returned))`,
			release: `(mutex-unlock! m)`,
			want:    `(thunk-returned thread-value)`,
		},
		{
			name: "the timer expires",
			h:    `(with-timeout 1000 (lambda (k) 'timed-out) (lambda () ` + start + ` (fire! 0) 'unreached))`,
			want: `(timed-out (uncaught "mutex-lock!: cancelled before the wait completed"))`,
		},
		{
			name: "an inner thunk returns, then the outer timer expires",
			h: `(with-timeout 1000 (lambda (k) 'outer-timed-out)
  (lambda ()
    (with-timeout 1000 (lambda (k) 'inner-timed-out) (lambda () ` + start + ` 'inner-returned))
    (fire! 0)
    'unreached))`,
			want: `(outer-timed-out (uncaught "mutex-lock!: cancelled before the wait completed"))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := testhelpers.NewFullRuntimeEnv(t)
			ctx, fire := machine.WithTimersFiringOnCue(context.Background())
			defineFire(t, env, fire)
			result, err := testhelpers.RunSchemeCodeWithEnvAndContext(ctx, t, env, `(begin
  (define m (make-mutex))
  (mutex-lock! m)
  (define rm (make-mutex))
  (define rcv (make-condition-variable))
  (define t (make-thread (lambda ()
    (mutex-lock! rm) (condition-variable-signal! rcv) (mutex-unlock! rm)
    (mutex-lock! m)
    'thread-value)))
  (define h `+tc.h+`)
  `+tc.release+`
  (list h (guard (e ((uncaught-exception? e) (list 'uncaught (error-object-message (uncaught-exception-reason e)))))
            (thread-join! t))))`)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestJoinReportsJoineeCancellationAsUncaught pins that a JOINEE dying of a
// cancelled wait is the joinee's outcome, not the joiner's cancellation. Join wraps
// every joinee error in UncaughtThreadException, whose Unwrap lets errors.Is see
// the ErrOperationCancelled inside; reading that as the joiner's own cancellation
// raised "thread-join!: cancelled before the wait completed" from a joiner nobody
// cancelled. The thread-sleep! row is the one that failed for that reason: there a
// cancelled sleep already raised, while a cancelled mutex-lock! answered #f. The
// thread is started inside a with-timeout thunk whose timer then expires, which
// cancels the thread's ctx; the thunk fires the timer only once the thread has
// signalled that it is at its wait, so the cancellation reaches the wait rather
// than the thread's first VM poll. The joiner joins outside the with-timeout.
func TestJoinReportsJoineeCancellationAsUncaught(t *testing.T) {
	waits := []struct {
		site string
		wait string
	}{
		{"mutex-lock!", `(mutex-lock! m)`},
		{"thread-sleep!", `(thread-sleep! 10)`},
	}
	for _, w := range waits {
		t.Run(w.site, func(t *testing.T) {
			env := testhelpers.NewFullRuntimeEnv(t)
			ctx, fire := machine.WithTimersFiringOnCue(context.Background())
			defineFire(t, env, fire)
			result, err := testhelpers.RunSchemeCodeWithEnvAndContext(ctx, t, env, `(begin
  (define m (make-mutex))
  (mutex-lock! m)
  (define rm (make-mutex))
  (define rcv (make-condition-variable))
  (define t (make-thread (lambda ()
    (mutex-lock! rm) (condition-variable-signal! rcv) (mutex-unlock! rm)
    `+w.wait+`)))
  (define h (with-timeout 1000 (lambda (k) 'timed-out)
    (lambda () (mutex-lock! rm) (thread-start! t) (mutex-unlock! rm rcv) (fire! 0) 'unreached)))
  (list h (guard (e ((uncaught-exception? e) (error-object-message (uncaught-exception-reason e)))
                    ((error-object? e) (list 'error-object (error-object-message e))))
            (thread-join! t))))`)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, `(timed-out "`+w.site+`: cancelled before the wait completed")`)
		})
	}
}

// TestLeavingATimerEndsItsContext pins that an escape ends the timer's ctx as a
// normal return does. An escape that only restored the ctx outside the timer,
// without cancelling it, left the timer's ctx live until its deadline, and
// whatever waited on it waiting (a started thread did, until it was taken off
// the timer's ctx). The thunk hands its ctx to the host through note-ctx!.
func TestLeavingATimerEndsItsContext(t *testing.T) {
	tcs := []struct {
		name  string
		leave string
		want  string
	}{
		{"escape", `(raise 'x)`, "caught"},
		{"normal return", `'ret`, "ret"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := testhelpers.NewFullRuntimeEnv(t)
			var timerCtx context.Context
			_, err := env.DefineOwnGlobal(values.NewSymbol("note-ctx!"), environment.BindingTypeVariable, syntax.Scopes{},
				machine.NewForeignClosure(env, 0, false, func(cc machine.CallContext) error {
					timerCtx = cc.Context()
					cc.SetValue(values.Void)
					return nil
				}))
			qt.Assert(t, err, qt.IsNil)
			result, err := testhelpers.RunSchemeCodeWithEnvAndContext(context.Background(), t, env,
				`(guard (e (#t 'caught)) (with-timeout 60000 (lambda (k) 'h) (lambda () (note-ctx!) `+tc.leave+`)))`)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.want)
			qt.Assert(t, timerCtx, qt.IsNotNil)
			qt.Assert(t, timerCtx.Err(), qt.ErrorIs, context.Canceled)
		})
	}
}

// defineFire binds (fire! i) in env to fire(i).
func defineFire(t *testing.T, env *environment.EnvironmentFrame, fire func(i int)) {
	t.Helper()
	_, err := env.DefineOwnGlobal(values.NewSymbol("fire!"), environment.BindingTypeVariable, syntax.Scopes{},
		machine.NewForeignClosure(env, 1, false, func(cc machine.CallContext) error {
			i, ok := cc.Arg(0).(*values.Integer)
			if !ok {
				return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "fire!: the timer index is not an integer")
			}
			fire(int(i.Value))
			cc.SetValue(values.Void)
			return nil
		}))
	qt.Assert(t, err, qt.IsNil)
}
