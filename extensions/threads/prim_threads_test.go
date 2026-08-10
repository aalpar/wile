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

package threads_test

import (
	"context"
	"errors"
	"testing"
	"time"

	extthreads "github.com/aalpar/wile/extensions/threads"
	"github.com/aalpar/wile/pkg/testutil"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// newEngine creates a Wile engine with the threads extension loaded.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extthreads.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return engine
}

// newEngineWithExceptions creates a Wile engine with threads.
// Exception primitives are now in core, so only threads need explicit loading.
func newEngineWithExceptions(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extthreads.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return engine
}

// eval runs Scheme code and returns the result.
func eval(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.EvalMultiple(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNil)
	return result
}

// evalWithWatchdog runs code on its own goroutine and fails the test if Eval has
// not returned within d.
//
// The failure mode every cancellation gate in this file guards against is a HANG,
// and a hanging test does not report: it is killed by the package timeout with no
// assertion attached and no indication of which case stalled. Making the watchdog
// part of the assertion is what turns "the suite timed out" into a named failure.
func evalWithWatchdog(t *testing.T, engine *wile.Engine, code string, d time.Duration) (wile.Value, error) {
	t.Helper()
	type outcome struct {
		value wile.Value
		err   error
	}
	done := make(chan outcome, 1)
	go func() {
		v, err := engine.EvalMultiple(context.Background(), code)
		done <- outcome{value: v, err: err}
	}()

	timer := time.NewTimer(d)
	defer timer.Stop()
	select {
	case o := <-done:
		return o.value, o.err
	case <-timer.C:
		t.Fatalf("Eval did not return within %v — the parked primitive has no cancellation edge", d)
		return nil, nil
	}
}

// evalExpectError runs Scheme code and asserts that it produces an error.
func evalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	expr, err := engine.Parse(context.Background(), code)
	if err != nil {
		return // parse error counts as expected error
	}
	_, err = engine.Eval(context.Background(), expr)
	qt.New(t).Assert(err, qt.IsNotNil)
}

// TestThreadDoesNotInheritCreatorMarks pins SRFI-18-aligned behavior: a thread does
// NOT inherit its creator's continuation marks — neither parameterize bindings nor
// the exception handler (which now rides the %exception-handlers parameter, i.e. the
// same mark mechanism). NewThreadSubContext severs parentMC and copies no marks, so a
// thread gets a fresh dynamic environment.
//
// This is the documented post-rework semantics. Before the handler moved onto a
// parameter it was the one piece of dynamic state field-inherited by threads; it now
// behaves like every other parameter (uninherited). That also removes the old
// unsoundness — a guard handler inherited into the child would escape via a guard-k
// captured in the PARENT thread, tripping ErrCrossThreadContinuation.
func TestThreadDoesNotInheritCreatorMarks(t *testing.T) {
	c := qt.New(t)
	engine := newEngineWithExceptions(t)
	result, err := engine.EvalMultiple(context.Background(), `
		(let ((p (make-parameter 'base)) (seen #f))
		  (parameterize ((p 'outer))
		    (let ((th (make-thread (lambda () (set! seen (p))))))
		      (thread-start! th)
		      (thread-join! th)))
		  (eq? seen 'base))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestThreadBasics(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// current-thread returns 'primordial in main goroutine
		// Note: eq? fails because PrimCurrentThread creates a fresh symbol
		// via NewSymbol, which is not pointer-equal to reader-interned symbols.
		{"current-thread primordial",
			`(and (symbol? (current-thread))
			      (equal? (symbol->string (current-thread)) "primordial"))`,
			values.TrueValue},

		// thread?
		{"thread? on thread",
			`(thread? (make-thread (lambda () #t)))`,
			values.TrueValue},
		{"thread? false integer",
			`(thread? 42)`,
			values.FalseValue},
		{"thread? false string",
			`(thread? "hello")`,
			values.FalseValue},

		// thread-name with explicit name
		{"thread-name explicit",
			`(let ((t (make-thread (lambda () #t) "my-thread")))
			   (equal? (thread-name t) "my-thread"))`,
			values.TrueValue},

		// thread-name auto-generated
		{"thread-name auto",
			`(let ((t (make-thread (lambda () #t))))
			   (> (string-length (thread-name t)) 0))`,
			values.TrueValue},

		// thread-specific defaults to void (nil)
		{"thread-specific default",
			`(let ((t (make-thread (lambda () #t))))
			   (eq? (thread-specific t) (if #f #f)))`,
			values.TrueValue},

		// thread-specific-set! and thread-specific round-trip
		{"thread-specific set and get",
			`(let ((t (make-thread (lambda () #t))))
			   (thread-specific-set! t 42)
			   (= (thread-specific t) 42))`,
			values.TrueValue},
		{"thread-specific set string",
			`(let ((t (make-thread (lambda () #t))))
			   (thread-specific-set! t "data")
			   (equal? (thread-specific t) "data"))`,
			values.TrueValue},

		// thread-yield! returns without error
		{"thread-yield!",
			`(begin (thread-yield!) #t)`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestThreadLifecycle(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// thread-start! returns the thread
		{"start returns thread",
			`(let ((t (make-thread (lambda () #t))))
			   (thread? (thread-start! t)))`,
			values.TrueValue},

		// thread-join! returns thunk result
		{"join returns result",
			`(let ((t (make-thread (lambda () (+ 1 2)))))
			   (thread-start! t)
			   (= (thread-join! t) 3))`,
			values.TrueValue},

		// thread-join! with string result
		{"join returns string",
			`(let ((t (make-thread (lambda () "hello"))))
			   (thread-start! t)
			   (equal? (thread-join! t) "hello"))`,
			values.TrueValue},

		// thread-join! with list result
		{"join returns list",
			`(let ((t (make-thread (lambda () '(1 2 3)))))
			   (thread-start! t)
			   (equal? (thread-join! t) '(1 2 3)))`,
			values.TrueValue},

		// thread-terminate!
		{"terminate thread",
			`(let ((t (make-thread (lambda () #t))))
			   (thread-terminate! t)
			   #t)`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestThreadSleep(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// thread-sleep! with zero seconds (immediate return)
		{"sleep zero integer",
			`(begin (thread-sleep! 0) #t)`,
			values.TrueValue},
		{"sleep zero float",
			`(begin (thread-sleep! 0.0) #t)`,
			values.TrueValue},

		// thread-sleep! with time object
		{"sleep with time object",
			`(begin (thread-sleep! (current-time)) #t)`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// TestThreadTimeoutParsing pins the timeout-parsing contract shared by the
// optional timeout arguments of mutex-lock!, mutex-unlock!, and thread-join!:
// integer seconds, float seconds, and absolute time objects are all accepted
// (the #t and string reject branches are covered in TestThreadsErrors). Each
// case acquires a free resource (or joins a finished thread), so the timeout
// never actually elapses except the deliberate mutex-unlock! case, which times
// out immediately.
func TestThreadTimeoutParsing(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"mutex-lock! integer timeout",
			`(let ((m (make-mutex))) (mutex-lock! m 5))`,
			values.TrueValue},
		{"mutex-lock! float timeout",
			`(let ((m (make-mutex))) (mutex-lock! m 0.5))`,
			values.TrueValue},
		{"mutex-lock! time-object timeout",
			`(let ((m (make-mutex))) (mutex-lock! m (current-time)))`,
			values.TrueValue},
		{"thread-join! integer timeout",
			`(let ((t (thread-start! (make-thread (lambda () 42)))))
			   (= (thread-join! t 5) 42))`,
			values.TrueValue},
		{"mutex-unlock! condvar timeout returns #f on timeout",
			`(let ((m (make-mutex)) (cv (make-condition-variable)))
			   (mutex-lock! m)
			   (not (mutex-unlock! m cv 0)))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestThreadSleepContextCancellation(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		code string
	}{
		{"cancel during integer sleep",
			`(begin (test-ready!) (thread-sleep! 60))`},
		{"cancel during float sleep",
			`(begin (test-ready!) (thread-sleep! 60.0))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			ext, ready := testutil.ReadyExtension()
			engine, err := wile.NewEngine(context.Background(),
				wile.WithExtension(extthreads.Extension),
				wile.WithExtension(ext),
			)
			c.Assert(err, qt.IsNil)

			ctx, cancel := context.WithCancel(context.Background())
			done := make(chan error, 1)
			go func() {
				_, err := engine.Eval(ctx, engine.MustParse(ctx, tc.code))
				done <- err
			}()
			select {
			case <-ready:
			case err := <-done:
				t.Fatalf("Eval returned before ready signal: %v", err)
			}
			cancel()

			select {
			case err := <-done:
				// Against the sentinel, never against nil. A test named for
				// cancellation that only knows an error arrived cannot see
				// WHICH cancellation, which is the whole content of the claim.
				c.Assert(errors.Is(err, werr.ErrOperationCancelled), qt.IsTrue,
					qt.Commentf("thread-sleep! must report a cancelled sleep as ErrOperationCancelled, got %v", err))
			case <-time.After(2 * time.Second):
				t.Fatal("thread-sleep! did not respect context cancellation")
			}
		})
	}
}

func TestMutexBasics(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// make-mutex and mutex?
		{"make-mutex", `(mutex? (make-mutex))`, values.TrueValue},
		{"make-mutex named", `(mutex? (make-mutex "my-mutex"))`, values.TrueValue},
		{"make-mutex symbol name", `(mutex? (make-mutex 'my-mutex))`, values.TrueValue},
		// A name argument that is neither string nor symbol degrades silently to
		// the unnamed case (helpers.OptionalName never errors) — still a mutex.
		{"make-mutex non-string/symbol name degrades to unnamed",
			`(mutex? (make-mutex 42))`, values.TrueValue},
		{"mutex? false integer", `(mutex? 42)`, values.FalseValue},
		{"mutex? false string", `(mutex? "hello")`, values.FalseValue},

		// mutex-name with explicit name
		{"mutex-name explicit",
			`(equal? (mutex-name (make-mutex "test-lock")) "test-lock")`,
			values.TrueValue},

		// mutex-name auto-generated
		{"mutex-name auto",
			`(> (string-length (mutex-name (make-mutex))) 0)`,
			values.TrueValue},

		// A fresh mutex is NOT HELD, so SRFI-18 says 'not-abandoned. This
		// asserted "not-owned" and was pinning the defect: 'not-owned is the
		// answer for a mutex that IS held with no owning thread, and rendering
		// both cases the same made them indistinguishable at the Scheme
		// surface while leaving 'not-abandoned unreachable.
		{"mutex-state new",
			`(and (symbol? (mutex-state (make-mutex)))
			      (equal? (symbol->string (mutex-state (make-mutex))) "not-abandoned"))`,
			values.TrueValue},

		// mutex-specific defaults to void (nil)
		{"mutex-specific default",
			`(eq? (mutex-specific (make-mutex)) (if #f #f))`,
			values.TrueValue},

		// mutex-specific-set! and mutex-specific round-trip
		{"mutex-specific set and get",
			`(let ((m (make-mutex)))
			   (mutex-specific-set! m 42)
			   (= (mutex-specific m) 42))`,
			values.TrueValue},
		{"mutex-specific set string",
			`(let ((m (make-mutex)))
			   (mutex-specific-set! m "data")
			   (equal? (mutex-specific m) "data"))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestMutexLockUnlock(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// mutex-lock! returns #t on success
		{"lock returns true",
			`(let ((m (make-mutex)))
			   (mutex-lock! m))`,
			values.TrueValue},

		// mutex-unlock! after lock
		{"lock then unlock",
			`(let ((m (make-mutex)))
			   (mutex-lock! m)
			   (mutex-unlock! m))`,
			values.TrueValue},

		// lock, unlock, lock again
		{"relock after unlock",
			`(let ((m (make-mutex)))
			   (mutex-lock! m)
			   (mutex-unlock! m)
			   (mutex-lock! m))`,
			values.TrueValue},

		// mutex-state after lock (not-owned because primordial has no Thread)
		{"mutex-state after lock",
			`(let ((m (make-mutex)))
			   (mutex-lock! m)
			   (let ((s (mutex-state m)))
			     (mutex-unlock! m)
			     (and (symbol? s)
			          (equal? (symbol->string s) "not-owned"))))`,
			values.TrueValue},

		// Unlocked again, so not held: 'not-abandoned, for the same reason as
		// the fresh-mutex row.
		{"mutex-state after unlock",
			`(let ((m (make-mutex)))
			   (mutex-lock! m)
			   (mutex-unlock! m)
			   (let ((s (mutex-state m)))
			     (and (symbol? s)
			          (equal? (symbol->string s) "not-abandoned"))))`,
			values.TrueValue},

		// The DISCRIMINATING pair, and the reason the two symbols exist: held
		// and not-held must not render the same. If a future edit collapses
		// them again, exactly one of these two rows goes red.
		{"mutex-state distinguishes held-unowned from not-held",
			`(let ((m (make-mutex)))
			   (let ((held (begin (mutex-lock! m) (mutex-state m))))
			     (mutex-unlock! m)
			     (and (equal? (symbol->string held) "not-owned")
			          (equal? (symbol->string (mutex-state m)) "not-abandoned")
			          (not (eq? held (mutex-state m))))))`,
			values.TrueValue},

		// The fourth answer: held by a thread renders as that thread, not a
		// symbol. Completes the SRFI-18 quadruple alongside the rows above and
		// the abandoned case in TestMutexAbandonment.
		{"mutex-state held by a thread is that thread",
			`(let ((m (make-mutex)) (t (make-thread (lambda () 1))))
			   (mutex-lock! m #f t)
			   (and (not (symbol? (mutex-state m)))
			        (eq? (mutex-state m) t)))`,
			values.TrueValue},

		// mutex-lock! with #f owner (no owner)
		{"lock with no owner",
			`(let ((m (make-mutex)))
			   (mutex-lock! m #f #f))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestConditionVariable(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// make-condition-variable and condition-variable?
		{"make-condition-variable",
			`(condition-variable? (make-condition-variable))`,
			values.TrueValue},
		{"make-condition-variable named",
			`(condition-variable? (make-condition-variable "my-cv"))`,
			values.TrueValue},
		{"make-condition-variable symbol name",
			`(condition-variable? (make-condition-variable 'my-cv))`,
			values.TrueValue},
		{"condition-variable? false integer",
			`(condition-variable? 42)`,
			values.FalseValue},
		{"condition-variable? false string",
			`(condition-variable? "hello")`,
			values.FalseValue},

		// condition-variable-name
		{"condition-variable-name explicit",
			`(equal? (condition-variable-name (make-condition-variable "test-cv")) "test-cv")`,
			values.TrueValue},
		{"condition-variable-name auto",
			`(> (string-length (condition-variable-name (make-condition-variable))) 0)`,
			values.TrueValue},

		// condition-variable-specific defaults to void
		{"condition-variable-specific default",
			`(eq? (condition-variable-specific (make-condition-variable)) (if #f #f))`,
			values.TrueValue},

		// condition-variable-specific-set! and get
		{"condition-variable-specific set and get",
			`(let ((cv (make-condition-variable)))
			   (condition-variable-specific-set! cv 42)
			   (= (condition-variable-specific cv) 42))`,
			values.TrueValue},

		// signal and broadcast on empty CV (no-op, no error)
		{"signal no waiters",
			`(let ((cv (make-condition-variable)))
			   (condition-variable-signal! cv)
			   #t)`,
			values.TrueValue},
		{"broadcast no waiters",
			`(let ((cv (make-condition-variable)))
			   (condition-variable-broadcast! cv)
			   #t)`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestTime(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// current-time returns a time object
		{"current-time is time",
			`(time? (current-time))`,
			values.TrueValue},

		// time?
		{"time? false integer",
			`(time? 42)`,
			values.FalseValue},
		{"time? false string",
			`(time? "hello")`,
			values.FalseValue},

		// time->seconds returns positive number (epoch seconds)
		{"time->seconds positive",
			`(> (time->seconds (current-time)) 0)`,
			values.TrueValue},

		// seconds->time creates a time object
		{"seconds->time integer",
			`(time? (seconds->time 1000))`,
			values.TrueValue},
		{"seconds->time float",
			`(time? (seconds->time 1000.5))`,
			values.TrueValue},

		// round-trip: seconds->time->seconds preserves value
		{"seconds round-trip integer",
			`(let ((s 1234567890))
			   (< (abs (- (time->seconds (seconds->time s)) s)) 1.0))`,
			values.TrueValue},
		{"seconds round-trip float",
			`(let ((s 1234567890.5))
			   (< (abs (- (time->seconds (seconds->time s)) s)) 0.001))`,
			values.TrueValue},

		// two calls to current-time produce non-decreasing seconds
		{"time monotonic",
			`(let ((t1 (time->seconds (current-time)))
			       (t2 (time->seconds (current-time))))
			   (>= t2 t1))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestThreadsErrors(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		// thread type errors
		{"thread-name not thread", `(thread-name 42)`},
		{"thread-specific not thread", `(thread-specific 42)`},
		{"thread-specific-set! not thread", `(thread-specific-set! 42 "val")`},
		{"thread-start! not thread", `(thread-start! 42)`},
		{"thread-terminate! not thread", `(thread-terminate! 42)`},
		{"thread-join! not thread", `(thread-join! 42)`},

		// thread-sleep! type error
		{"thread-sleep! string", `(thread-sleep! "hello")`},
		{"thread-sleep! boolean", `(thread-sleep! #t)`},

		// thread-start! already started
		{"thread-start! already started",
			`(let ((t (make-thread (lambda () #t))))
			   (thread-start! t)
			   (thread-join! t)
			   (thread-start! t))`},

		// mutex type errors
		{"mutex-name not mutex", `(mutex-name 42)`},
		{"mutex-specific not mutex", `(mutex-specific 42)`},
		{"mutex-specific-set! not mutex", `(mutex-specific-set! 42 "val")`},
		{"mutex-state not mutex", `(mutex-state 42)`},
		{"mutex-lock! not mutex", `(mutex-lock! 42)`},
		{"mutex-unlock! not mutex", `(mutex-unlock! 42)`},

		// mutex-lock! timeout parse errors (parseTimeout #t and default branches)
		{"mutex-lock! timeout #t", `(let ((m (make-mutex))) (mutex-lock! m #t))`},
		{"mutex-lock! timeout string", `(let ((m (make-mutex))) (mutex-lock! m "x"))`},

		// condition-variable type errors
		{"condition-variable-name not cv", `(condition-variable-name 42)`},
		{"condition-variable-specific not cv", `(condition-variable-specific 42)`},
		{"condition-variable-specific-set! not cv", `(condition-variable-specific-set! 42 "val")`},
		{"condition-variable-signal! not cv", `(condition-variable-signal! 42)`},
		{"condition-variable-broadcast! not cv", `(condition-variable-broadcast! 42)`},

		// time type errors
		{"time->seconds not time", `(time->seconds 42)`},
		{"seconds->time string", `(seconds->time "hello")`},
		{"seconds->time boolean", `(seconds->time #t)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// =============================================================================
// Thread Identity Tests
// =============================================================================

func TestCurrentThreadIdentity(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// current-thread in primordial thread returns 'primordial
		{"primordial thread",
			`(and (symbol? (current-thread))
			      (equal? (symbol->string (current-thread)) "primordial"))`,
			values.TrueValue},

		// current-thread inside a thread returns the thread object itself
		{"thread identity",
			`(let ((t (make-thread
			            (lambda ()
			              (thread? (current-thread))))))
			   (thread-start! t)
			   (thread-join! t))`,
			values.TrueValue},

		// current-thread inside a thread returns the same object as make-thread created
		{"thread self identity",
			`(let* ((result #f)
			        (t (make-thread
			             (lambda ()
			               (set! result (current-thread))))))
			   (thread-start! t)
			   (thread-join! t)
			   (eq? result t))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// =============================================================================
// Cross-Thread Continuation Rejection Tests
// =============================================================================

func TestCrossThreadContinuationRejection(t *testing.T) {
	engine := newEngineWithExceptions(t)
	tcs := []struct {
		name string
		code string
	}{
		// Capture continuation in one thread, invoke from another -> error
		{"cross-thread call/cc",
			`(let* ((k #f)
			        (t1 (make-thread
			              (lambda ()
			                (call/cc (lambda (cont) (set! k cont)))))))
			   (thread-start! t1)
			   (thread-join! t1)
			   ;; k now holds a continuation captured in t1
			   ;; invoking from primordial thread should fail
			   (k 42))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestSameThreadContinuationAllowed(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// Capture and invoke continuation in the same thread -> works
		{"same-thread call/cc",
			`(= (call/cc (lambda (k) (k 42))) 42)`,
			values.TrueValue},

		// Primordial thread continuation invoked from primordial -> works
		{"primordial call/cc",
			`(let ((result (call/cc (lambda (k) (k 99)))))
			   (= result 99))`,
			values.TrueValue},

		// call/cc inside a thread works within that thread
		{"thread-internal call/cc",
			`(let ((t (make-thread
			            (lambda ()
			              (call/cc (lambda (k) (k 77)))))))
			   (thread-start! t)
			   (= (thread-join! t) 77))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// =============================================================================
// Dynamic-Wind Cleanup on Thread Termination Tests
// =============================================================================

func TestDynamicWindCleanupOnThreadExit(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// dynamic-wind after-thunk runs on normal thread exit
		{"after-thunk on normal exit",
			`(let* ((box (cons #f '()))
			        (t (make-thread
			             (lambda ()
			               (dynamic-wind
			                 (lambda () #f)
			                 (lambda () 42)
			                 (lambda () (set-car! box #t)))))))
			   (thread-start! t)
			   (thread-join! t)
			   (car box))`,
			values.TrueValue},

		// nested dynamic-wind after-thunks run in correct order (innermost first)
		{"nested after-thunks order",
			`(let* ((log '())
			        (t (make-thread
			             (lambda ()
			               (dynamic-wind
			                 (lambda () #f)
			                 (lambda ()
			                   (dynamic-wind
			                     (lambda () #f)
			                     (lambda () 42)
			                     (lambda () (set! log (cons 'inner log)))))
			                 (lambda () (set! log (cons 'outer log))))))))
			   (thread-start! t)
			   (thread-join! t)
			   ;; log should be (outer inner) - inner runs first, then outer
			   (and (equal? (car log) 'outer)
			        (equal? (car (cdr log)) 'inner)))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// =============================================================================
// Mutex Abandonment on Thread Termination Tests
// =============================================================================

func TestMutexAbandonedOnTermination(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// Mutex state becomes 'abandoned when owning thread is terminated
		{"mutex abandoned on terminate",
			`(let* ((m (make-mutex))
			        (t (make-thread
			             (lambda ()
			               (mutex-lock! m)
			               ;; sleep to keep thread alive while we terminate it
			               (thread-sleep! 10)))))
			   (thread-start! t)
			   ;; Wait until the child thread actually acquires the mutex.
			   ;; mutex-state returns the owner thread (not a symbol) when locked.
			   (let wait ()
			     (when (symbol? (mutex-state m))
			       (thread-yield!)
			       (wait)))
			   (thread-terminate! t)
			   ;; The mutex should be abandoned
			   (let ((state (mutex-state m)))
			     (and (symbol? state)
			          (equal? (symbol->string state) "abandoned"))))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// =============================================================================
// Context Cancellation Integration Tests
// =============================================================================

// TestThreadParentContextCancellation verifies that cancelling the parent
// context propagates to a running thread's derived context, causing the
// thread's VM loop to terminate via context.Canceled.
func TestThreadParentContextCancellation(t *testing.T) {
	c := qt.New(t)
	ext, ready := testutil.ReadyExtension()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extthreads.Extension),
		wile.WithExtension(ext),
	)
	c.Assert(err, qt.IsNil)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	errCh := make(chan error, 1)
	go func() {
		_, err := engine.Eval(ctx, engine.MustParse(ctx,
			`(let ((th (make-thread (lambda () (begin (test-ready!) (let loop () (loop)))))))
			   (thread-start! th)
			   (thread-join! th))`))
		errCh <- err
	}()

	select {
	case <-ready:
	case err := <-errCh:
		t.Fatalf("Eval returned before ready signal: %v", err)
	}
	cancel()

	select {
	case err := <-errCh:
		c.Assert(err, qt.IsNotNil)
	case <-time.After(5 * time.Second):
		t.Fatal("scheme evaluation did not return after context cancellation")
	}
}

// TestThreadRespectsParentTimeout verifies that a thread running an infinite
// loop terminates when the parent context's deadline expires.
func TestThreadRespectsParentTimeout(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	ctx, cancel := context.WithTimeout(context.Background(), 500*time.Millisecond)
	defer cancel()

	start := time.Now()
	// Note: engine.Eval is the Scheme interpreter's eval, not JavaScript eval.
	// It compiles and runs Scheme source code on the Wile VM.
	_, err := engine.Eval(ctx, engine.MustParse(ctx,
		`(let ((th (make-thread (lambda () (let loop () (loop))))))
		   (thread-start! th)
		   (thread-join! th))`))
	elapsed := time.Since(start)

	c.Assert(err, qt.IsNotNil)
	c.Assert(elapsed < 2*time.Second, qt.IsTrue,
		qt.Commentf("should terminate within timeout window, took %v", elapsed))
}

// TestThreadJoinWrapsUncaughtException pins the SRFI-18 contract: a thread that
// terminates via an uncaught exception has an uncaught-exception object raised in
// the JOINING thread's dynamic environment (whose uncaught-exception-reason is the
// original condition), so a guard / with-exception-handler around the thread-join!
// call catches it and can recover the original condition. Before the wrapper, the
// bare condition was re-raised (and earlier still, the carrier bubbled past the
// guard to the top level). Each case self-checks in Scheme and returns #t.
func TestThreadJoinWrapsUncaughtException(t *testing.T) {
	c := qt.New(t)
	engine := newEngineWithExceptions(t)
	tcs := []struct {
		name string
		code string
	}{
		// A guard around thread-join! catches an uncaught-exception wrapper whose
		// reason is the thread's bare (raise obj).
		{"guard catches wrapper; reason is the raised symbol",
			`(let ((t (make-thread (lambda () (raise 'boom)))))
			   (thread-start! t)
			   (guard (e ((uncaught-exception? e)
			              (eq? 'boom (uncaught-exception-reason e))))
			     (thread-join! t)))`},

		// The reason is the ORIGINAL object: an error-object keeps its message and
		// irritants (dispatchable after unwrapping), not a flattened string.
		{"reason is the original error-object with irritants",
			`(let ((t (make-thread (lambda () (error "boom" 42)))))
			   (thread-start! t)
			   (guard (e ((uncaught-exception? e)
			              (let ((r (uncaught-exception-reason e)))
			                (and (error-object? r)
			                     (equal? (error-object-message r) "boom")
			                     (equal? (error-object-irritants r) '(42))))))
			     (thread-join! t)))`},

		// The wrapper is a DISTINCT object from its reason.
		{"wrapper is not its own reason",
			`(let ((t (make-thread (lambda () (raise 'boom)))))
			   (thread-start! t)
			   (guard (e (#t (not (eq? e (uncaught-exception-reason e)))))
			     (thread-join! t)))`},

		// Identity is preserved: uncaught-exception-reason returns the SAME object
		// the thread raised, not a reconstruction.
		{"reason is eq? to the raised object",
			`(let ((obj (cons 1 2)))
			   (let ((t (make-thread (lambda () (raise obj)))))
			     (thread-start! t)
			     (guard (e (#t (eq? obj (uncaught-exception-reason e))))
			       (thread-join! t))))`},

		// uncaught-exception-reason on a non-uncaught-exception raises the
		// MakeUnaryAccessor sentinel path — a real error-object, not just any
		// catchable condition (the guard clause is keyed on error-object? so an
		// unrelated failure would fall through to #f).
		{"uncaught-exception-reason rejects a non-wrapper",
			`(guard (e ((error-object? e) #t) (#t #f))
			   (uncaught-exception-reason 'not-a-wrapper)
			   #f)`},

		// The predicate is #f for a non-wrapper (its #t path is covered above).
		{"uncaught-exception? is #f for a non-wrapper",
			`(not (uncaught-exception? 'not-a-wrapper))`},

		// No regression: a thread that returns normally still yields its value through
		// a surrounding guard rather than tripping the handler.
		{"normal result still returns through guard",
			`(let ((t (make-thread (lambda () 99))))
			   (thread-start! t)
			   (= 99 (guard (e (#t -1))
			           (thread-join! t))))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, values.TrueValue)
		})
	}
}

// TestSRFI18ExceptionPredicatesDiscriminate pins the property the three
// predicates exist for: each of SRFI-18's abnormal outcomes reaches a guard
// around the call as a DISTINCT object, so a handler can tell them apart without
// matching on an error message.
//
// The discrimination is what is under test, not merely catchability. Before the
// predicates, all three arrived as generic error-objects carrying only a string,
// so every case below would have fallen through to the catch-all clause. Each
// case therefore keys its success on the ONE predicate that should fire and
// additionally asserts the other three do not — a condition raised as the wrong
// type would otherwise still satisfy a single positive check.
//
// Each case self-checks in Scheme and returns #t.
func TestSRFI18ExceptionPredicatesDiscriminate(t *testing.T) {
	c := qt.New(t)
	engine := newEngineWithExceptions(t)

	// classify returns the symbol naming whichever SRFI-18 predicate matched, or
	// 'other for anything else — so a case asserting (eq? 'join-timeout ...)
	// fails, rather than silently passing, if the condition arrives as the wrong
	// type or as a bare error-object.
	const classify = `
	  (define (classify thunk)
	    (guard (e ((join-timeout-exception? e) 'join-timeout)
	              ((terminated-thread-exception? e) 'terminated)
	              ((abandoned-mutex-exception? e) 'abandoned)
	              ((uncaught-exception? e) 'uncaught)
	              (#t 'other))
	      (thunk)
	      'no-raise))
	  ;; Spin until the child owns the mutex. mutex-state returns the owner
	  ;; THREAD once locked and a symbol while unlocked, so symbol? is the
	  ;; "not yet acquired" test.
	  (define (wait-for-lock m)
	    (when (symbol? (mutex-state m))
	      (thread-yield!)
	      (wait-for-lock m)))`

	tcs := []struct {
		name string
		code string
	}{
		// thread-join! reaching its timeout with no timeout-val raises a
		// join-timeout-exception, per SRFI-18.
		{"join timeout raises join-timeout-exception",
			`(let ((t (make-thread (lambda () (thread-sleep! 30)))))
			   (thread-start! t)
			   (let ((got (classify (lambda () (thread-join! t 0.05)))))
			     (thread-terminate! t)
			     (eq? 'join-timeout got)))`},

		// A supplied timeout-val suppresses the raise entirely: SRFI-18 returns
		// the value instead. Guards the branch that must NOT become a raise.
		{"join timeout with timeout-val returns the value, raises nothing",
			`(let ((t (make-thread (lambda () (thread-sleep! 30)))))
			   (thread-start! t)
			   (let ((got (thread-join! t 0.05 'fallback)))
			     (thread-terminate! t)
			     (eq? 'fallback got)))`},

		// Joining a thread killed by thread-terminate! raises a
		// terminated-thread-exception — NOT an uncaught-exception. The two are
		// separate SRFI-18 conditions and the joiner must be able to tell which
		// happened; internally both arrive wrapped in the same Go carrier, so
		// this pins the ordering in joinConditionFor.
		{"joining a terminated thread raises terminated-thread-exception",
			`(let ((t (make-thread (lambda () (thread-sleep! 30)))))
			   (thread-start! t)
			   (thread-terminate! t)
			   (eq? 'terminated (classify (lambda () (thread-join! t)))))`},

		// A thread that dies on an uncaught exception still yields
		// uncaught-exception, not one of the two new conditions.
		{"uncaught exception still raises uncaught-exception",
			`(let ((t (make-thread (lambda () (raise 'boom)))))
			   (thread-start! t)
			   (eq? 'uncaught (classify (lambda () (thread-join! t)))))`},

		// mutex-lock! on a mutex whose owner was terminated raises an
		// abandoned-mutex-exception.
		{"locking an abandoned mutex raises abandoned-mutex-exception",
			`(let* ((m (make-mutex))
			        (t (make-thread (lambda () (mutex-lock! m) (thread-sleep! 30)))))
			   (thread-start! t)
			   (wait-for-lock m)
			   (thread-terminate! t)
			   (eq? 'abandoned (classify (lambda () (mutex-lock! m)))))`},

		// SRFI-18 changes the mutex state BEFORE raising: the lock is genuinely
		// held afterwards, so a handler that unlocks is correct and one that
		// re-locks would deadlock. Distinguishes "raised and acquired" from
		// "raised instead of acquiring".
		{"abandoned mutex is acquired before the condition is raised",
			`(let* ((m (make-mutex))
			        (t (make-thread (lambda () (mutex-lock! m) (thread-sleep! 30)))))
			   (thread-start! t)
			   (wait-for-lock m)
			   (thread-terminate! t)
			   (guard (e ((abandoned-mutex-exception? e)
			              ;; Held means neither still-abandoned nor released. The
			              ;; owning answer varies ('not-owned when the locker has
			              ;; no thread object, else the thread itself), so the
			              ;; assertion excludes the two not-held answers rather
			              ;; than naming one held answer.
			              (let ((state (mutex-state m)))
			                (and (not (eq? 'abandoned state))
			                     (not (eq? 'unlocked state))))))
			     (mutex-lock! m)
			     #f))`},

		// Each predicate rejects the other conditions' objects, so a handler
		// chain cannot match the wrong clause. Built by catching one real
		// condition and testing all four predicates against it.
		{"predicates are mutually exclusive on a real condition",
			`(let ((t (make-thread (lambda () (thread-sleep! 30)))))
			   (thread-start! t)
			   (let ((r (guard (e (#t (list (join-timeout-exception? e)
			                                (terminated-thread-exception? e)
			                                (abandoned-mutex-exception? e)
			                                (uncaught-exception? e))))
			              (thread-join! t 0.05))))
			     (thread-terminate! t)
			     (equal? r '(#t #f #f #f))))`},

		// Non-conditions are rejected by all three new predicates.
		{"predicates are #f for ordinary objects",
			`(and (not (join-timeout-exception? 'x))
			      (not (join-timeout-exception? 5))
			      (not (terminated-thread-exception? "s"))
			      (not (terminated-thread-exception? '()))
			      (not (abandoned-mutex-exception? #f))
			      (not (abandoned-mutex-exception? (make-mutex))))`},
	}
	// Defined once: the engine is shared across subtests and its top level is
	// immutable, so re-evaluating the prelude per case would fail on redefinition.
	eval(t, engine, classify)

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, values.TrueValue)
		})
	}
}

// TestThreadState pins thread-state, which reports a thread's lifecycle position
// as a symbol. It is NOT SRFI-18 (that spec has mutex-state and no thread-state);
// the name and vocabulary follow Gambit.
func TestThreadState(t *testing.T) {
	c := qt.New(t)
	engine := newEngineWithExceptions(t)
	tcs := []struct {
		name string
		code string
	}{
		// A created-but-unstarted thread is 'new.
		{"unstarted thread is new",
			`(eq? 'new (thread-state (make-thread (lambda () 1))))`},

		// After a completed join the thread is 'terminated. Joining first is what
		// makes this deterministic — polling the state of a running thread races
		// its own transitions.
		{"joined thread is terminated",
			`(let ((t (make-thread (lambda () 1))))
			   (thread-start! t)
			   (thread-join! t)
			   (eq? 'terminated (thread-state t)))`},

		// thread-terminate! moves the thread to 'terminated even though it never
		// ran to completion.
		{"terminated thread is terminated",
			`(let ((t (make-thread (lambda () (thread-sleep! 30)))))
			   (thread-start! t)
			   (thread-terminate! t)
			   (eq? 'terminated (thread-state t)))`},

		// A thread observing itself is running, so it must see 'runnable — the
		// state is not merely a terminal marker.
		{"a running thread sees itself as runnable",
			`(let ((t (make-thread (lambda () (thread-state (current-thread))))))
			   (thread-start! t)
			   (eq? 'runnable (thread-join! t)))`},

		// Non-thread arguments hit the MakeUnaryAccessor sentinel path and raise
		// a real error-object. The guard keys on error-object? so an unrelated
		// failure falls through to #f rather than passing.
		{"thread-state rejects a non-thread",
			`(guard (e ((error-object? e) #t) (#t #f))
			   (thread-state 'not-a-thread)
			   #f)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, values.TrueValue)
		})
	}
}

// TestTerminateUnparksUntimedThreadJoin covers the cancellation edge on an
// untimed thread-join!, and it has two arms that must not be confused with each
// other. Which thread gets terminated is the whole content of the test.
//
//   - "joining-thread" is the GATE. It terminates the thread that is PARKED IN
//     the join, and asserts that its own untimed thread-join! unparks. Terminate
//     on a started thread cancels its ctx but does NOT close its done channel
//     (ownsDone is true only for a never-started thread), so before the fix the
//     joiner stayed parked in a bare <-victim.done, which has no ctx arm, and its
//     termination never took effect. Measured before the fix: 'join-timed-out,
//     because the joiner's done never closed and the outer join's own 5 s bound
//     expired. That bound is why this arm reports instead of hanging; drop it and
//     the failure becomes a hang, which is what the watchdog is for.
//
//   - "joinee" is a CONTROL, and it CANNOT FAIL. It terminates the thread being
//     joined, which already worked: Terminate cancels the joinee's ctx, its
//     goroutine unwinds and closes done, and since 2026-08-02 the joiner unparks
//     by raising terminated-thread-exception. Measured before the fix: green, in
//     well under a second. It is here because it is the natural misreading of
//     "park a thread in an untimed thread-join!, terminate it, assert the join
//     returns" — the gate as originally filed, which passed without the fix. Keep
//     it labelled, or the weaker form gets restored as the gate.
//
// Both arms run under a watchdog because the gate's failure mode is a hang, and a
// hanging test does not report.
func TestTerminateUnparksUntimedThreadJoin(t *testing.T) {
	// The rendezvous is the mutex itself, not a shared cell: main holds m until
	// the joiner hands it back immediately before parking, and the mutex is what
	// supplies the happens-before edge between the two threads. A plain pair cell
	// polled with thread-yield! would be a data race.
	const joiningThread = `
(define m (make-mutex))
(mutex-lock! m)
(define victim (make-thread (lambda () (thread-sleep! 3600))))
(define joiner (make-thread (lambda ()
  (mutex-unlock! m)
  (thread-join! victim))))
(thread-start! victim)
(thread-start! joiner)
(mutex-lock! m)
(thread-terminate! joiner)
(define outcome
  (guard (e ((terminated-thread-exception? e) 'terminated)
            ((join-timeout-exception? e) 'join-timed-out)
            (#t 'other))
    (thread-join! joiner 5)
    'returned-normally))
(thread-terminate! victim)
outcome`

	const joinee = `
(define victim (make-thread (lambda () (thread-sleep! 3600))))
(thread-start! victim)
(thread-terminate! victim)
(guard (e ((terminated-thread-exception? e) 'terminated)
          ((join-timeout-exception? e) 'join-timed-out)
          (#t 'other))
  (thread-join! victim)
  'returned-normally)`

	tcs := []struct {
		name string
		code string
	}{
		{"joining-thread", joiningThread},
		{"joinee", joinee},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newEngine(t)
			result, err := evalWithWatchdog(t, engine, tc.code, 12*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.Internal().SchemeString(), qt.Equals, "terminated")
		})
	}
}

// TestWithTimeoutInterruptsParkedThreadJoin is defect 35's second shape and the
// composition analogue of TestWithTimeoutInterruptsParkedMutexLock: an untimed
// thread-join! parked inside a with-timeout must run the handler. victim never
// terminates, so the only thing that can end the join is the timer.
//
// Measured before the fix: Eval never returns, and the watchdog fires.
func TestWithTimeoutInterruptsParkedThreadJoin(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	result, err := evalWithWatchdog(t, engine, `
(define victim (make-thread (lambda () (thread-sleep! 3600))))
(thread-start! victim)
(define outcome
  (with-timeout 500
    (lambda (k) 'timed-out)
    (lambda () (thread-join! victim))))
(thread-terminate! victim)
outcome`, 12*time.Second)

	c.Assert(err, qt.IsNil)
	c.Assert(result.Internal().SchemeString(), qt.Equals, "timed-out")
}

// TestWithTimeoutInterruptsParkedThreadSleep is defect 38's COMPOSITION arm: the
// Scheme-level proof that a thread-sleep! parked inside a with-timeout runs the
// handler rather than escaping an error to the host.
//
// It is TestWithTimeoutInterruptsParkedMutexLock (extensions/gointerop) ported to
// thread-sleep!, and the port is not cosmetic: mutex-lock! reaches the handler by
// reporting a cancelled acquire as an error-free #f, which is what lets
// callForeignCached's eager ErrTimerExpired recheck fire. thread-sleep! returns
// Void and so has no such value channel, and before the fix it returned the raw
// ctx.Err() instead: the sleep aborted the whole evaluation with "context deadline
// exceeded" and the handler never ran.
//
// Both shapes matter because the escaping error IS a catchable Scheme condition,
// so a guard around the sleep changes which mechanism resolves the timeout. The
// guarded arm additionally records whether its clause body ran, because
// with-timeout's handler value alone cannot distinguish "nothing was raised" from
// "something was raised and the guard clause was skipped mid-dispatch".
func TestWithTimeoutInterruptsParkedThreadSleep(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{"unguarded", `
(with-timeout 300
  (lambda (k) 'timed-out)
  (lambda () (thread-sleep! 60)))`, "timed-out"},

		// The flag is a pair cell, not a top-level set!: top-level bindings are
		// immutable by default, so the mutation needs a mutable cell.
		{"guarded", `
(define guard-flag (list 'not-run))
(define outcome
  (with-timeout 300
    (lambda (k) 'timed-out)
    (lambda ()
      (guard (e (#t (set-car! guard-flag 'guard-ran) 'guard-value))
        (thread-sleep! 60)))))
(list outcome (car guard-flag))`, "(timed-out not-run)"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newEngine(t)
			result, err := evalWithWatchdog(t, engine, tc.code, 10*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.Internal().SchemeString(), qt.Equals, tc.want)
		})
	}
}
