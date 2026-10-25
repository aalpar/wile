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
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// TestResumedTimerThunkAnswersAsUninterrupted sweeps a with-timeout's firing
// point over the first instructions of its thunk (WithTimerFiringAfter) and pins
// that resuming the handler's continuation answers exactly what the thunk answers
// uninterrupted, at every point. Each loop's handler records that it ran, so a
// sweep that outran the thunk would fail rather than pass vacuously.
//
// Two fire points were wrong. At the poll, the resume replaced the value register
// (the operand the next instruction consumes) with the values k was applied to:
// none for (k), which the next push refused ("expected 1 value, got 0"). After a
// foreign call, the interrupt was taken on the call instruction, so a resume re-ran
// the call on its drained operands ("expected 2 arguments, got 0"); the resume
// must instead make the call's pending return.
func TestResumedTimerThunkAnswersAsUninterrupted(t *testing.T) {
	const sweep = 160
	tcs := []struct {
		name     string
		code     string
		expected values.Value
		// points, when non-zero, bounds the sweep to the points inside the thunk.
		points uint64
	}{
		{
			name: "set! loop resumed with no values",
			code: `(let ((n 0) (fired #f))
  (list (with-timeout 1000 (lambda (k) (set! fired #t) (k))
          (lambda () (let loop () (set! n (+ n 1)) (if (< n 100) (loop) (list 'done n)))))
        fired))`,
			expected: values.List(values.List(values.NewSymbol("done"), values.NewInteger(100)), values.TrueValue),
		},
		{
			name: "set! loop resumed with a value, which is discarded",
			code: `(let ((n 0) (fired #f))
  (list (with-timeout 1000 (lambda (k) (set! fired #t) (k 'stray))
          (lambda () (let loop () (set! n (+ n 1)) (if (< n 100) (loop) (list 'done n)))))
        fired))`,
			expected: values.List(values.List(values.NewSymbol("done"), values.NewInteger(100)), values.TrueValue),
		},
		{
			name: "parameterized loop",
			code: `(let ((p (make-parameter 'outer)))
  (with-timeout 1000 (lambda (k) (list (p) (k)))
    (lambda () (parameterize ((p 'inner)) (let loop ((i 0)) (if (< i 100) (loop (+ i 1)) (p)))))))`,
			expected: values.List(values.NewSymbol("outer"), values.NewSymbol("inner")),
		},
		{
			name: "foreign calls in the loop",
			code: `(let ((fired #f))
  (list (with-timeout 1000 (lambda (k) (set! fired #t) (k))
          (lambda () (let loop ((i 0) (s 0)) (if (< i 100) (loop (+ i 1) (+ s (string-length (number->string i)))) s))))
        fired))`,
			expected: values.List(values.NewInteger(190), values.TrueValue),
		},
		{
			name: "a foreign call in tail position to the thunk",
			code: `(let ((fired #f))
  (list (with-timeout 1000 (lambda (k) (set! fired #t) (k))
          (lambda () (let loop ((i 0)) (if (< i 100) (loop (+ i 1)) (string->symbol "end")))))
        fired))`,
			expected: values.List(values.NewSymbol("end"), values.TrueValue),
		},
		{
			// Only the first point is inside the thunk, which returns at once. A
			// later one lands between the boundary frame's restore and the
			// finalizer's teardown, which raises an internal error on master too
			// (filed separately).
			name:     "a primitive as the thunk",
			code:     `(with-timeout 1000 (lambda (k) (k)) list)`,
			expected: values.EmptyList,
			points:   1,
		},
		{
			name: "multiple values in the register",
			code: `(let ((fired #f))
  (list (with-timeout 1000 (lambda (k) (set! fired #t) (k))
          (lambda () (let loop ((i 0) (acc '())) (if (< i 50) (call-with-values (lambda () (values i 'v)) (lambda (a b) (loop (+ a 1) (cons b acc)))) (length acc)))))
        fired))`,
			expected: values.List(values.NewInteger(50), values.TrueValue),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			env := testhelpers.NewFullRuntimeEnv(t)
			points := uint64(sweep)
			if tc.points > 0 {
				points = tc.points
			}
			for k := range points {
				ctx := machine.WithTimerFiringAfter(context.Background(), k)
				result, err := testhelpers.RunSchemeCodeWithEnvAndContext(ctx, t, env, tc.code)
				qt.Assert(t, err, qt.IsNil, qt.Commentf("fired after %d instructions", k))
				qt.Assert(t, result, valuestest.SchemeEquals, tc.expected, qt.Commentf("fired after %d instructions", k))
			}
		})
	}
}

// TestResumedTimerThunkReentersItsWindingExtent pins the one way a resumed thunk
// does not answer as if uninterrupted: the timer's hard suspension leaves a
// dynamic-wind extent without running its after thunk, and the resume re-enters
// it, running its before thunk again. So a timer that fires inside the extent
// answers (x 2 1 #t), and one that fires before the before thunk ran answers
// (x 1 1 #t). This is the current design, filed for review, not an endorsement:
// the sweep asserts that each point answers one of the two, and that every point
// after the first inside the extent answers the second.
func TestResumedTimerThunkReentersItsWindingExtent(t *testing.T) {
	const sweep = 160
	code := `(let ((before 0) (after 0) (fired #f))
  (let ((r (with-timeout 1000 (lambda (k) (set! fired #t) (k))
             (lambda ()
               (dynamic-wind
                 (lambda () (set! before (+ before 1)))
                 (lambda () (let loop ((i 0)) (if (< i 100) (loop (+ i 1)) 'x)))
                 (lambda () (set! after (+ after 1))))))))
    (list r before after fired)))`
	outside := values.List(values.NewSymbol("x"), values.NewInteger(1), values.NewInteger(1), values.TrueValue)
	inside := values.List(values.NewSymbol("x"), values.NewInteger(2), values.NewInteger(1), values.TrueValue)
	env := testhelpers.NewFullRuntimeEnv(t)
	entered := false
	for k := range uint64(sweep) {
		ctx := machine.WithTimerFiringAfter(context.Background(), k)
		result, err := testhelpers.RunSchemeCodeWithEnvAndContext(ctx, t, env, code)
		qt.Assert(t, err, qt.IsNil, qt.Commentf("fired after %d instructions", k))
		if values.Equal(result, inside) {
			entered = true
			continue
		}
		qt.Assert(t, entered, qt.IsFalse, qt.Commentf("fired after %d instructions, after the extent was entered", k))
		qt.Assert(t, result, valuestest.SchemeEquals, outside, qt.Commentf("fired after %d instructions", k))
	}
	qt.Assert(t, entered, qt.IsTrue)
}

// TestResumedTimerInsideNonContinuableHandlerStillEscalates sweeps a
// with-timeout's firing point over a thunk whose non-continuable raise's handler
// loops and then returns, and pins that resuming the timeout handler's
// continuation answers what the thunk answers uninterrupted: the R7RS 6.11
// secondary exception, caught by the outer guard. The timer's hard suspension
// drops the handler's finalizer frame without leaving its extent, so a resume
// that judged the frame's absence from the live chain as a re-entry from outside
// forwarded the handler's 42 into the raise, (a 42 b), from the first point
// inside the handler on. The second row runs the same program inside eval, whose
// sub-context arms the timer.
func TestResumedTimerInsideNonContinuableHandlerStillEscalates(t *testing.T) {
	const sweep = 160
	const program = `(let ((fired #f))
  (list (guard (e (#t (list 'secondary (error-object? e))))
          (with-timeout 1000 (lambda (k) (set! fired #t) (k))
            (lambda ()
              (with-exception-handler
                (lambda (e) (let loop ((i 0)) (if (< i 100) (loop (+ i 1)))) 42)
                (lambda () (list 'a (raise 'boom) 'b))))))
        fired))`
	tcs := []struct {
		name string
		code string
	}{
		{name: "on the arming context", code: program},
		{name: "inside eval", code: "(eval '" + program + " (interaction-environment))"},
	}
	expected := values.List(values.List(values.NewSymbol("secondary"), values.TrueValue), values.TrueValue)
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			env := testhelpers.NewFullRuntimeEnv(t)
			for k := range uint64(sweep) {
				ctx := machine.WithTimerFiringAfter(context.Background(), k)
				result, err := testhelpers.RunSchemeCodeWithEnvAndContext(ctx, t, env, tc.code)
				qt.Assert(t, err, qt.IsNil, qt.Commentf("fired after %d instructions", k))
				qt.Assert(t, result, valuestest.SchemeEquals, expected, qt.Commentf("fired after %d instructions", k))
			}
		})
	}
}

// TestTimerClaimsErrorsOwnArgumentFailureBelowIt pins that error's own argument
// failure, a non-string message, is claimed by a with-timeout that fired above
// it, as car's failure is: inside eval, after a primitive that outlived the timer
// (fire!), the guard inside eval does not see it and the timeout handler answers.
// error raised that condition in place, so the guard caught it. A well-formed
// error is the program's own raise and still reaches the guard (the timeout
// answers once eval returns); the row without fire! shows the guard's flag is a
// live witness.
func TestTimerClaimsErrorsOwnArgumentFailureBelowIt(t *testing.T) {
	tcs := []struct {
		name   string
		body   string
		want   string
		caught bool
	}{
		{name: "error with a non-string message", body: `(fire!) (error 5)`, want: "timed-out"},
		{name: "car of a non-pair, the control", body: `(fire!) (car 5)`, want: "timed-out"},
		{name: "a well-formed error is the program's raise", body: `(fire!) (error "boom" 5)`, want: "timed-out", caught: true},
		{name: "no timer fired", body: `(error 5)`, want: "caught", caught: true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := testhelpers.NewFullRuntimeEnv(t)
			ctx, fire := machine.WithTimerFiringOnCue(context.Background())
			_, err := env.DefineOwnGlobal(values.NewSymbol("fire!"), environment.BindingTypeVariable, syntax.Scopes{},
				machine.NewForeignClosure(env, 0, false, func(cc machine.CallContext) error {
					fire()
					cc.SetValue(values.Void)
					return nil
				}))
			qt.Assert(t, err, qt.IsNil)
			result, err := testhelpers.RunSchemeCodeWithEnvAndContext(ctx, t, env, `(begin
  (define caught (vector #f))
  (list (with-timeout 1000 (lambda (k) 'timed-out)
          (lambda ()
            (eval '(guard (e (#t (vector-set! caught 0 #t) 'caught)) `+tc.body+`)
                  (interaction-environment))))
        (vector-ref caught 0)))`)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals,
				values.List(values.NewSymbol(tc.want), values.BoolToBoolean(tc.caught)))
		})
	}

	t.Run("uncaught, it keeps its sentinel", func(t *testing.T) {
		_, err := testhelpers.RunSchemeCode(t, `(error 5)`)
		qt.Assert(t, errors.Is(err, werr.ErrNotAString), qt.IsTrue)
	})
}

// TestTimerHandlerAfterAnEscapeSeesTheOuterParameterize pins that a with-timeout
// handler run because a wait in an escape's after thunk was cancelled sees the
// parameterization outside the escaping continuation, not the one around its
// capture: the escape reinstates k0, whose capture-time snapshot held the
// parameterize around the call/cc, and the handler, below the reinstated frames,
// read it ((handler esc)).
func TestTimerHandlerAfterAnEscapeSeesTheOuterParameterize(t *testing.T) {
	env := testhelpers.NewFullRuntimeEnv(t)
	ctx, fire := machine.WithTimerFiringOnCue(context.Background())
	_, err := env.DefineOwnGlobal(values.NewSymbol("fire!"), environment.BindingTypeVariable, syntax.Scopes{},
		machine.NewForeignClosure(env, 0, false, func(cc machine.CallContext) error {
			fire()
			cc.SetValue(values.Void)
			return nil
		}))
	qt.Assert(t, err, qt.IsNil)
	result, err := testhelpers.RunSchemeCodeWithEnvAndContext(ctx, t, env, `(begin
  (define p (make-parameter 'outer))
  (define m (make-mutex))
  (mutex-lock! m)
  (with-timeout 1000 (lambda (k) (list 'handler (p)))
    (lambda ()
      (parameterize ((p 'esc))
        (call/cc (lambda (k0)
          (dynamic-wind (lambda () #f)
                        (lambda () (k0 'escaped))
                        (lambda () (fire!) (mutex-lock! m)))))))))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.List(values.NewSymbol("handler"), values.NewSymbol("outer")))
}
