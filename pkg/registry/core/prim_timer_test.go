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

package core_test

import (
	"context"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

func TestWithTimeout(t *testing.T) {
	t.Parallel()

	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "normal completion returns thunk result",
			Code:     `(with-timeout 5000 (lambda (k) 'timeout) (lambda () 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name:     "handler return value when timeout fires",
			Code:     `(with-timeout 1 (lambda (k) 'expired) (lambda () (let loop () (loop))))`,
			Expected: values.NewSymbol("expired"),
		},
		{
			Name: "handler receives composable continuation",
			Code: `(with-timeout 1
                     (lambda (k) (procedure? k))
                     (lambda () (let loop () (loop))))`,
			Expected: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 5*time.Second)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestWithTimeoutErrors(t *testing.T) {
	t.Parallel()

	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "ms not integer", Code: `(with-timeout "bad" (lambda (k) k) (lambda () 1))`},
		{Name: "handler not procedure", Code: `(with-timeout 100 42 (lambda () 1))`},
		{Name: "thunk not procedure", Code: `(with-timeout 100 (lambda (k) k) 42)`},
		{Name: "negative ms", Code: `(with-timeout -1 (lambda (k) k) (lambda () 1))`},
		{Name: "wrong arity", Code: `(with-timeout 100 (lambda (k) k))`},
		{Name: "ms overflow", Code: `(with-timeout 9999999999999999 (lambda (k) k) (lambda () 1))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestDriverAppliedHandlerFailureIsCatchable pins the two handlers the driver
// applies itself, not through an OpApply: the with-timeout handler and the
// prompt handler an abort runs. Their application's failure (here an arity
// mismatch) is a Scheme condition a guard catches, at top level and inside eval,
// as any other procedure application's is. Racket agrees for the abort twin
// (with-handlers catches the arity error). The correct-arity rows are the
// controls.
func TestDriverAppliedHandlerFailureIsCatchable(t *testing.T) {
	t.Parallel()

	const (
		timerArity = `(with-timeout 20 (lambda () 'x) (lambda () (let loop () (loop))))`
		abortArity = `(let ((t (make-continuation-prompt-tag 't)))
  (call-with-continuation-prompt (lambda () (abort-current-continuation t 1 2)) t (lambda (x) x)))`
	)
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "timer handler arity, guard at top level",
			Code:     `(guard (e (#t (error-object-message e))) ` + timerArity + `)`,
			Expected: values.NewString("expected 0 arguments, got 1"),
		},
		{
			Name:     "abort handler arity, guard at top level",
			Code:     `(guard (e (#t (error-object-message e))) ` + abortArity + `)`,
			Expected: values.NewString("expected 1 arguments, got 2"),
		},
		{
			Name:     "timer handler arity, guard inside eval",
			Code:     `(eval '(guard (e (#t 'caught)) ` + timerArity + `) (interaction-environment))`,
			Expected: values.NewSymbol("caught"),
		},
		{
			Name:     "abort handler arity, guard inside eval",
			Code:     `(eval '(guard (e (#t 'caught)) ` + abortArity + `) (interaction-environment))`,
			Expected: values.NewSymbol("caught"),
		},
		{
			Name:     "timer handler arity, guard around eval",
			Code:     `(guard (e (#t 'caught)) (eval '` + timerArity + ` (interaction-environment)))`,
			Expected: values.NewSymbol("caught"),
		},
		{
			Name:     "abort handler arity, guard around eval",
			Code:     `(guard (e (#t 'caught)) (eval '` + abortArity + ` (interaction-environment)))`,
			Expected: values.NewSymbol("caught"),
		},
		{
			Name:     "control: timer handler of the right arity",
			Code:     `(guard (e (#t 'caught)) (with-timeout 20 (lambda (k) 'x) (lambda () (let loop () (loop)))))`,
			Expected: values.NewSymbol("x"),
		},
		{
			Name: "control: abort handler of the right arity",
			Code: `(let ((t (make-continuation-prompt-tag 't)))
  (guard (e (#t 'caught))
    (call-with-continuation-prompt (lambda () (abort-current-continuation t 1 2)) t (lambda (x y) y))))`,
			Expected: values.NewInteger(2),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 5*time.Second)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestTimerHandlerRunsOutsideTheAbandonedThunk pins the marks half of the hard
// suspension: the handler, and a failure of its application, see the handlers
// and parameterizations of the with-timeout's own chain, not those the timed-out
// thunk installed, as an abort's prompt handler does. A resumed thunk still sees
// its own (the marks travel in the resumable). The failing handler is the arity
// mismatch of TestDriverAppliedHandlerFailureIsCatchable.
func TestTimerHandlerRunsOutsideTheAbandonedThunk(t *testing.T) {
	t.Parallel()

	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "handler failure skips a handler the thunk installed",
			Code: `(call/cc (lambda (out)
  (with-exception-handler (lambda (e) (out 'outer))
    (lambda ()
      (with-timeout 20 (lambda () 'x)
        (lambda ()
          (call/cc (lambda (in)
            (with-exception-handler (lambda (e) (in 'inner))
              (lambda () (let loop () (loop))))))))))))`,
			Expected: values.NewSymbol("outer"),
		},
		{
			Name: "handler failure does not pass through an escalating handler of the thunk",
			Code: `(guard (e (#t (list 'outer (error-object? e))))
  (with-timeout 20 (lambda () 'x)
    (lambda ()
      (with-exception-handler (lambda (e) (raise 'inner))
        (lambda () (let loop () (loop)))))))`,
			Expected: values.List(values.NewSymbol("outer"), values.TrueValue),
		},
		{
			Name: "handler sees the parameterization outside the thunk",
			Code: `(let ((p (make-parameter 'outer)))
  (with-timeout 20 (lambda (k) (p))
    (lambda () (parameterize ((p 'inner)) (let loop () (loop))))))`,
			Expected: values.NewSymbol("outer"),
		},
		{
			// The thunk spins until the handler has run, so neither the timer's
			// firing nor the 5s budget depends on how fast a loaded machine counts.
			Name: "resumed thunk sees its own parameterization",
			Code: `(let ((p (make-parameter 'outer)) (fired #f))
  (with-timeout 1 (lambda (k) (set! fired #t) (list (p) (k)))
    (lambda () (parameterize ((p 'inner)) (let loop () (if fired (p) (loop)))))))`,
			Expected: values.List(values.NewSymbol("outer"), values.NewSymbol("inner")),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 5*time.Second)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestFiredTimerIsTornDownBeforeItsHandlerRuns pins the order inside the timer
// arm: the fired timer is torn down before its handler is applied, so a failure
// of that application is not claimed by the same timer as a second interrupt.
// Claimed, the driver resolves the interrupt again and applies the handler a
// second time; the Go handler counts its applications.
func TestFiredTimerIsTornDownBeforeItsHandlerRuns(t *testing.T) {
	t.Parallel()
	c := qt.New(t)

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	engine, err := wile.NewEngine(ctx)
	c.Assert(err, qt.IsNil)
	defer func() {
		_ = engine.Close()
	}()
	calls := 0
	c.Assert(engine.RegisterFunc("failing-handler", func(wile.Value) error {
		calls++
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "failing-handler: always fails")
	}), qt.IsNil)

	result, err := engine.Eval(ctx, engine.MustParse(ctx,
		`(guard (e (#t 'caught)) (with-timeout 20 failing-handler (lambda () (let loop () (loop)))))`))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "caught")
	c.Assert(calls, qt.Equals, 1)
}

func TestWithTimeoutNesting(t *testing.T) {
	t.Parallel()

	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "inner fires before outer",
			Code: `(with-timeout 5000 (lambda (k) 'outer-fired)
                     (lambda ()
                       (with-timeout 1 (lambda (k) 'inner-fired)
                         (lambda () (let loop () (loop))))))`,
			Expected: values.NewSymbol("inner-fired"),
		},
		{
			Name: "inner completes normally within outer",
			Code: `(with-timeout 5000 (lambda (k) 'outer-fired)
                     (lambda ()
                       (with-timeout 5000 (lambda (k) 'inner-fired)
                         (lambda () 42))))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "nested normal completion propagates",
			Code: `(with-timeout 5000 (lambda (k) 'outer-fired)
                     (lambda ()
                       (+ 1 (with-timeout 5000 (lambda (k) 0)
                               (lambda () 41)))))`,
			Expected: values.NewInteger(42),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 10*time.Second)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestWithTimeoutResumption(t *testing.T) {
	t.Parallel()

	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "handler discards continuation",
			Code:     `(with-timeout 1 (lambda (k) 'discarded) (lambda () (let loop () (loop))))`,
			Expected: values.NewSymbol("discarded"),
		},
		{
			Name: "handler can inspect continuation type",
			Code: `(with-timeout 1
                     (lambda (k) (and (procedure? k) 'ok))
                     (lambda () (let loop () (loop))))`,
			Expected: values.NewSymbol("ok"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 10*time.Second)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestWithTimeoutDynamicWind(t *testing.T) {
	t.Parallel()

	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "normal completion fires dynamic-wind after thunk",
			Code: `(let ((log '()))
                     (with-timeout 5000
                       (lambda (k) 'timeout)
                       (lambda ()
                         (dynamic-wind
                           (lambda () (set! log (cons 'before log)))
                           (lambda () 42)
                           (lambda () (set! log (cons 'after log))))))
                     log)`,
			Expected: values.List(
				values.NewSymbol("after"),
				values.NewSymbol("before"),
			),
		},
		{
			Name: "handler discards continuation — after thunks NOT called",
			Code: `(let ((after-called #f))
                     (with-timeout 1
                       (lambda (k) after-called)
                       (lambda ()
                         (dynamic-wind
                           (lambda () #f)
                           (lambda () (let loop () (loop)))
                           (lambda () (set! after-called #t))))))`,
			Expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 10*time.Second)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestTimerHandlerContinuationCarriesNoTimerFrame pins that the continuation a
// with-timeout handler receives carries the extents inside the timer's, not the
// timer's own frame: the segment stops below the finalizer, the only code that pops
// that frame. Carried, the frame was re-entered by a resume and left on the stack,
// where an enclosing dynamic-wind's PopWind took it in place of its own, and the end
// of the run (a thread's exit, here) ran that after thunk a second time. The spin
// outlasts the 20 ms timer. The 0 k is resumed with is discarded: the resume
// restores the value register the interrupt froze.
func TestTimerHandlerContinuationCarriesNoTimerFrame(t *testing.T) {
	t.Parallel()

	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "resumed inside a dynamic-wind in the handler",
			Code: `
				(let ((log '()))
					(define (note x) (set! log (cons x log)))
					(define (spin) (let loop ((i 0)) (if (< i 3000000) (loop (+ i 1)) 'done)))
					(let ((r (thread-join! (thread-start! (make-thread (lambda ()
							(with-timeout 20
								(lambda (k) (dynamic-wind (lambda () (note 'hin)) (lambda () (k 0)) (lambda () (note 'hout))))
								(lambda () (list 'got (spin))))))))))
						(list r (reverse log))))
			`,
			Expected: values.List(values.List(values.NewSymbol("got"), values.NewSymbol("done")),
				values.List(values.NewSymbol("hin"), values.NewSymbol("hout"))),
		},
		{
			Name: "saved, then resumed inside a later dynamic-wind",
			Code: `
				(let ((log '()) (saved #f))
					(define (note x) (set! log (cons x log)))
					(define (spin) (let loop ((i 0)) (if (< i 3000000) (loop (+ i 1)) 'done)))
					(let ((r (thread-join! (thread-start! (make-thread (lambda ()
							(with-timeout 20 (lambda (k) (set! saved k) 'h) (lambda () (list 'got (spin))))
							(dynamic-wind (lambda () (note 'in)) (lambda () (note (saved 0)) 'body) (lambda () (note 'out)))))))))
						(list r (reverse log))))
			`,
			Expected: values.List(values.NewSymbol("body"),
				values.List(values.NewSymbol("in"), values.List(values.NewSymbol("got"), values.NewSymbol("done")), values.NewSymbol("out"))),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestFiredTimerInsideAFrameLeavesTheFrame pins that the finalizer pops only its own
// timer's frame. After the timer arm's hard suspension the top frame is the
// enclosing dynamic-wind's, which must stay for its own PopWind; popped, the
// dynamic-wind's exit found its frame gone.
func TestFiredTimerInsideAFrameLeavesTheFrame(t *testing.T) {
	t.Parallel()

	result, err := testhelpers.RunSchemeCode(t, `
		(let ((log '()))
			(define (note x) (set! log (cons x log)))
			(let ((r (dynamic-wind (lambda () (note 'in))
					(lambda () (with-timeout 20 (lambda (k) 'timed-out) (lambda () (let loop () (loop)))))
					(lambda () (note 'out)))))
				(list r (reverse log))))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.List(values.NewSymbol("timed-out"),
		values.List(values.NewSymbol("in"), values.NewSymbol("out"))))
}

// TestWithTimeoutReturnLeavesItsExtent pins that a with-timeout returning normally
// pops its own winding frame: left on the stack, the enclosing dynamic-wind's
// PopWind popped it instead, and the dynamic-wind's frame stayed behind, so the
// end of the run ran its after thunk a second time. The dynamic-wind runs in a
// thread, whose exit is that end, so the log read after the join shows it.
func TestWithTimeoutReturnLeavesItsExtent(t *testing.T) {
	t.Parallel()

	result, err := testhelpers.RunSchemeCode(t, `
		(let ((log '()))
			(define (note x) (set! log (cons x log)))
			(thread-join! (thread-start! (make-thread (lambda ()
				(dynamic-wind
					(lambda () (note 'in))
					(lambda () (with-timeout 1000 (lambda (k) 'h) (lambda () 'x)))
					(lambda () (note 'out)))))))
			(reverse log))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.List(values.NewSymbol("in"), values.NewSymbol("out")))
}

// TestWithTimeoutInSubContext is the regression guard for the timer-arm symmetry: a
// with-timeout whose timer FIRES while it runs inside a SURVIVING sub-context (here an
// eval sub-context, driven by RunWithinBoundary) must be resolved on that sub's OWN chain
// by RunWithinBoundary's timer arm — not re-raised to the top driver, whose FindPrompt
// would walk the wrong chain and find no boundary. The reified design moved timer routing
// from a primitive-local Go catch to FindPrompt(tag), so without the symmetric
// resolveTimerInterrupt arm in RunWithinBoundary this truncates or crashes ("no boundary
// frame found"). If the loop is not interrupted, the 10s test timeout fails it loudly.
func TestWithTimeoutInSubContext(t *testing.T) {
	t.Parallel()

	code := `(eval '(with-timeout 1 (lambda (k) 'expired) (lambda () (let loop () (loop))))
	               (interaction-environment))`
	result, err := testhelpers.RunSchemeCodeWithTimeout(t, code, 10*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("expired"))
}

func TestWithTimeoutThreadIsolation(t *testing.T) {
	t.Parallel()

	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "timeout in one thread does not affect another",
			Code: `(let ((result #f)
                          (m (make-mutex)))
                      (mutex-lock! m)
                      (thread-start!
                        (make-thread
                          (lambda ()
                            (set! result
                              (with-timeout 5000
                                (lambda (k) 'timeout)
                                (lambda () 'ok)))
                            (mutex-unlock! m))))
                      (mutex-lock! m)
                      result)`,
			Expected: values.NewSymbol("ok"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 10*time.Second)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
