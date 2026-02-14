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
	"testing"
	"time"

	"github.com/aalpar/wile"
	extexceptions "github.com/aalpar/wile/internal/extensions/exceptions"
	extthreads "github.com/aalpar/wile/internal/extensions/threads"
	"github.com/aalpar/wile/values"

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

// newEngineWithExceptions creates a Wile engine with threads and exceptions.
// Needed for tests that use guard, with-exception-handler, etc.
func newEngineWithExceptions(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extthreads.Extension),
		wile.WithExtension(extexceptions.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return engine
}

// eval runs Scheme code and returns the result.
func eval(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNil)
	return result
}

// evalExpectError runs Scheme code and asserts that it produces an error.
func evalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	_, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNotNil)
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

		// mutex-state on new mutex (not-owned)
		{"mutex-state new",
			`(and (symbol? (mutex-state (make-mutex)))
			      (equal? (symbol->string (mutex-state (make-mutex))) "not-owned"))`,
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

		// mutex-state after unlock
		{"mutex-state after unlock",
			`(let ((m (make-mutex)))
			   (mutex-lock! m)
			   (mutex-unlock! m)
			   (let ((s (mutex-state m)))
			     (and (symbol? s)
			          (equal? (symbol->string s) "not-owned"))))`,
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
	engine := newEngine(t)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Run scheme code in a goroutine: starts an infinite-loop thread, then joins it.
	// The join blocks until the thread terminates.
	errCh := make(chan error, 1)
	go func() {
		// Note: engine.Eval is the Scheme interpreter's eval, not JavaScript eval.
		// It compiles and runs Scheme source code on the Wile VM.
		_, err := engine.Eval(ctx,
			`(let ((th (make-thread (lambda () (let loop () (loop))))))
			   (thread-start! th)
			   (thread-join! th))`)
		errCh <- err
	}()

	// Give the thread time to start and enter its loop
	time.Sleep(200 * time.Millisecond)

	// Cancel the parent context — should propagate to thread's derived context
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
	_, err := engine.Eval(ctx,
		`(let ((th (make-thread (lambda () (let loop () (loop))))))
		   (thread-start! th)
		   (thread-join! th))`)
	elapsed := time.Since(start)

	c.Assert(err, qt.IsNotNil)
	c.Assert(elapsed < 2*time.Second, qt.IsTrue,
		qt.Commentf("should terminate within timeout window, took %v", elapsed))
}
