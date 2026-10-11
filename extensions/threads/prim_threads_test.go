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
	"os"
	"path/filepath"
	"testing"
	"time"

	extthreads "github.com/aalpar/wile/extensions/threads"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/registry/core"
	"github.com/aalpar/wile/pkg/security"
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
	return evalWithWatchdogCtx(context.Background(), t, engine, code, d)
}

// evalWithWatchdogCtx is evalWithWatchdog under ctx.
func evalWithWatchdogCtx(ctx context.Context, t *testing.T, engine *wile.Engine, code string, d time.Duration) (wile.Value, error) {
	t.Helper()
	type outcome struct {
		value wile.Value
		err   error
	}
	done := make(chan outcome, 1)
	go func() {
		v, err := engine.EvalMultiple(ctx, code)
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

// evalCancelledAfterReady evaluates code, which calls (test-ready!) just before
// the wait under test, cancels the engine context with cause once it has, and
// returns what the evaluation returned. The cause is what the primitives read, so
// a cancel carrying context.DeadlineExceeded stands in for a deadline without
// racing compilation against a wall clock.
func evalCancelledAfterReady(t *testing.T, code string, cause error) (wile.Value, error) {
	t.Helper()
	ext, ready := testutil.ReadyExtension()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extthreads.Extension),
		wile.WithExtension(ext),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = engine.Close()
	})

	ctx, cancel := context.WithCancelCause(context.Background())
	defer cancel(nil)
	type outcome struct {
		value wile.Value
		err   error
	}
	done := make(chan outcome, 1)
	go func() {
		v, err := engine.Eval(ctx, engine.MustParse(ctx, code))
		done <- outcome{v, err}
	}()
	select {
	case <-ready:
	case o := <-done:
		t.Fatalf("Eval returned before ready signal: %v", o.err)
	}
	cancel(cause)

	select {
	case o := <-done:
		return o.value, o.err
	case <-time.After(5 * time.Second):
		t.Fatal("the wait did not respect context cancellation")
		return nil, nil
	}
}

// TestCancelledLockWaitRaisesOperationCancelled pins the rule the four blocking
// waits now share: a wait cancelled by anything other than a with-timeout timer
// armed in the waiting thread raises ErrOperationCancelled, carrying the raw ctx
// cause. mutex-lock! and (mutex-unlock! m cv) used to report it as an error-free
// #f, so when the wait was the program's last form the embedder got #f and a nil
// error after its own cancellation. The with-timeout half, where the wait must run
// the handler instead, is TestWithTimeoutInterruptsParkedMutexLock
// (extensions/gointerop).
//
// The main thread holds m with no owner, and m is not recursive, so the second
// acquire parks. Three rows reach branches the plain rows cannot:
//   - The timed acquire parks on a path that does not watch ctx at all, so only
//     PrimMutexLock's own check after the timeout turns the cancellation into a
//     raise; without it the program would end on #f with a nil error. Its 1 s
//     timeout is the margin the cancel has to land in after (test-ready!).
//   - The timed cv wait does watch ctx, and must raise rather than answer #f.
//   - The acquire inside with-timeout has a timer ARMED but not expired. The
//     embedder's cancel reaches the timer's ctx with the embedder's cause, which
//     must raise, not run the handler: only an ErrTimerExpired cause belongs to a
//     timer (the cause check in expiredTimer, read by firedTimer and timerAbove, pkg/machine).
func TestCancelledLockWaitRaisesOperationCancelled(t *testing.T) {
	waits := []struct {
		name string
		code string
	}{
		{"mutex-lock!",
			`(begin (define m (make-mutex)) (mutex-lock! m) (test-ready!) (mutex-lock! m))`},
		{"mutex-lock! with a timeout",
			`(begin (define m (make-mutex)) (mutex-lock! m) (test-ready!) (mutex-lock! m 1))`},
		{"mutex-lock! inside with-timeout",
			`(begin (define m (make-mutex)) (mutex-lock! m) (test-ready!)
			        (with-timeout 5000 (lambda (k) 'timed-out) (lambda () (mutex-lock! m))))`},
		{"mutex-unlock! with cv",
			`(begin (define m (make-mutex)) (define cv (make-condition-variable))
			        (mutex-lock! m) (test-ready!) (mutex-unlock! m cv))`},
		{"mutex-unlock! with cv and a timeout",
			`(begin (define m (make-mutex)) (define cv (make-condition-variable))
			        (mutex-lock! m) (test-ready!) (mutex-unlock! m cv 5))`},
	}
	causes := []struct {
		name  string
		cause error
	}{
		{"embedder cancel", context.Canceled},
		{"embedder deadline", context.DeadlineExceeded},
	}
	for _, w := range waits {
		for _, cs := range causes {
			t.Run(w.name+"/"+cs.name, func(t *testing.T) {
				c := qt.New(t)
				_, err := evalCancelledAfterReady(t, w.code, cs.cause)
				c.Assert(errors.Is(err, werr.ErrOperationCancelled), qt.IsTrue,
					qt.Commentf("a cancelled %s must raise ErrOperationCancelled, got %v", w.name, err))
				c.Assert(errors.Is(err, cs.cause), qt.IsTrue,
					qt.Commentf("the raw ctx cause must survive, got %v", err))
			})
		}
	}
}

// TestCaughtCancellationCleanupUnlockReturns pins that only a wait that did not
// complete reports the cancellation. Under a cancelled context a plain
// mutex-unlock! releases and answers #t, and an uncontended mutex-lock! acquires
// and answers #t, so cleanup that catches a cancelled wait and then releases or
// takes a lock does not raise again. A lock acquired under a cancelled ctx is held;
// raising there would leave it held with nobody told.
func TestCaughtCancellationCleanupUnlockReturns(t *testing.T) {
	c := qt.New(t)
	v, err := evalCancelledAfterReady(t, `(begin (define m (make-mutex)) (define m2 (make-mutex)) (define m3 (make-mutex))
	  (mutex-lock! m) (mutex-lock! m2) (test-ready!)
	  (guard (e (#t (list 'cleaned (mutex-unlock! m2) (mutex-lock! m3) (mutex-state m2))))
	    (mutex-lock! m)))`, context.Canceled)
	c.Assert(err, qt.IsNil)
	c.Assert(v.SchemeString(), qt.Equals, "(cleaned #t #t not-abandoned)")
}

// timerCancelledWaits are the four blocking waits, each spliced into a program
// that begins with waitPrelude, so every wait parks until a timer cancels it. site
// is the name the wait reports a cancellation under.
var timerCancelledWaits = []struct {
	name string
	site string
	wait string
}{
	{"mutex-lock!", "mutex-lock!", `(mutex-lock! m)`},
	{"mutex-unlock! with cv", "mutex-unlock!", `(let ((m2 (make-mutex))) (mutex-lock! m2) (mutex-unlock! m2 cv))`},
	{"thread-sleep!", "thread-sleep!", `(thread-sleep! 10)`},
	{"thread-join!", "thread-join!", `(thread-join! blocker)`},
}

// waitPrelude defines what the timerCancelledWaits rows refer to: m, held by the
// main thread with no owner, so a wait on it parks; cv; and blocker, a thread
// started before any with-timeout. A timer therefore cannot cancel blocker, and a
// thread-join! on it ends only as the joiner's own cancellation. A blocker started
// in the timed context would die of the same timer, racing the joinee's outcome
// against the joiner's cancellation in Join's select.
const waitPrelude = `
(define m (make-mutex))
(define cv (make-condition-variable))
(mutex-lock! m)
(define blocker (make-thread (lambda () (thread-sleep! 10))))
(thread-start! blocker)
`

// TestTimerCancelledWaitInChildThreadRaises pins a thread started inside a
// with-timeout thunk. Its ctx derives from the timed one (Thread.Start), so the
// timer's expiry cancels its parked wait with ErrTimerExpired as the cause, but no
// timer is armed on the thread's own context chain, so nothing in it can run a
// handler. The wait must raise. It used to return error-free, and the thread
// carried on past it: past a mutex-lock! that had not acquired, into the critical
// section, and then released the main thread's lock.
//
// The child signals on rcv just before its wait, and the thunk waits for that
// signal before it sleeps into the timer, so the child is past its first VM poll
// and at its wait when the timer fires. A child cancelled before its first
// instruction would die at that poll instead, a different path (cancellation.md,
// Boundaries); the 200 ms timer is the margin the child has to start in.
//
// The join reports the wait's own cancellation as the uncaught reason, the code
// after the wait never runs, and the main thread's lock stays held ('not-owned: a
// top-level lock has no owner thread). The child's mutex-unlock! after the wait is
// what would release it if the wait returned.
func TestTimerCancelledWaitInChildThreadRaises(t *testing.T) {
	for _, w := range timerCancelledWaits {
		t.Run(w.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newEngine(t)
			defer func() {
				_ = engine.Close()
			}()
			result, err := evalWithWatchdog(t, engine, waitPrelude+`
(define rm (make-mutex))
(define rcv (make-condition-variable))
(define entered (vector #f))
(define t (make-thread (lambda ()
  (mutex-lock! rm) (condition-variable-signal! rcv) (mutex-unlock! rm)
  `+w.wait+`
  (vector-set! entered 0 #t) (mutex-unlock! m) 'child-done)))
(define h (with-timeout 200 (lambda (k) 'timed-out)
  (lambda () (mutex-lock! rm) (thread-start! t) (mutex-unlock! rm rcv) (thread-sleep! 10))))
(list h
      (guard (e ((uncaught-exception? e) (error-object-message (uncaught-exception-reason e)))
                (#t (list 'other e)))
        (thread-join! t))
      (vector-ref entered 0)
      (mutex-state m))`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals,
				`(timed-out "`+w.site+`: cancelled before the wait completed" #f not-owned)`)
		})
	}
}

// TestTimerCancelledWaitInSubContextRunsHandler pins the same-thread case the eager
// recheck cannot reach: a wait inside eval runs in a synchronous sub-context, which
// shares the timed ctx but not the timer. The wait used to return error-free there,
// so the eval'd code ran past the cancelled wait before the handler ran. The wait
// now raises; the sub-context passes the failure up undispatched, and the context
// that owns the timer turns it into the interrupt when eval returns it, so the
// handler runs and nothing after the wait does. A wait that returned a value would
// fail here, where in the timer's own context the eager recheck still rescues it.
// The 200 ms timer leaves eval room to expand and compile before the wait parks;
// a timer that fired earlier would still run the handler, but without reaching
// the wait.
func TestTimerCancelledWaitInSubContextRunsHandler(t *testing.T) {
	for _, w := range timerCancelledWaits {
		t.Run(w.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
			c.Assert(err, qt.IsNil)
			defer func() {
				_ = engine.Close()
			}()
			result, err := evalWithWatchdog(t, engine, waitPrelude+`
(define after (vector #f))
(define h (with-timeout 200 (lambda (k) 'timed-out)
  (lambda () (eval '(begin `+w.wait+` (vector-set! after 0 #t) (mutex-unlock! m) 'evaluated)
                   (interaction-environment)))))
(list h (vector-ref after 0) (mutex-state m))`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, "(timed-out #f not-owned)")
		})
	}
}

// TestTimerCancelledWaitInRootlessCallCCRunsHandler pins the longest route a
// failure takes back to the timer's owner. An after thunk run by an escape (k0
// here) runs on a winding sub-context, and a tail call/cc in it applies its
// procedure on a further sub-context with its own RunResumable driver. The wait
// there raises; each sub-context passes the failure up undispatched, the after
// thunk's failure returns through the escape's winding reconcile, and the owner's
// driver turns it into the interrupt. HEAD answered (escaped #t not-abandoned): no
// handler, the code after the wait ran, and it released the main thread's lock.
func TestTimerCancelledWaitInRootlessCallCCRunsHandler(t *testing.T) {
	for _, w := range timerCancelledWaits {
		t.Run(w.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newEngine(t)
			defer func() {
				_ = engine.Close()
			}()
			result, err := evalWithWatchdog(t, engine, waitPrelude+`
(define after (vector #f))
(define h (with-timeout 50 (lambda (k) 'timed-out)
  (lambda ()
    (call/cc (lambda (k0)
      (dynamic-wind (lambda () #f)
                    (lambda () (k0 'escaped))
                    (lambda () (call/cc (lambda (k) `+w.wait+` (vector-set! after 0 #t) (mutex-unlock! m))))))))))
(list h (vector-ref after 0) (mutex-state m))`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, "(timed-out #f not-owned)")
		})
	}
}

// TestInnerTimerExpiryRunsTheInnerHandler pins which handler a timer-cancelled wait
// runs when two timers are armed on its chain and the inner one fires: the inner
// one's. On one context the inner with-timeout's timer is the context's timer, its
// outer one is that timer's parent. Across contexts the inner with-timeout runs
// inside eval, so eval's sub-context owns the inner timer and resolves it. The outer
// timer's expiry is TestTimerExpiryNamesTheTimerThatFired's.
func TestInnerTimerExpiryRunsTheInnerHandler(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"two timers on one context", `
(define m (make-mutex))
(mutex-lock! m)
(with-timeout 5000 (lambda (k) 'outer)
  (lambda () (with-timeout 50 (lambda (k) 'inner) (lambda () (mutex-lock! m)))))`},
		{"timers on two contexts", `
(define m (make-mutex))
(mutex-lock! m)
(with-timeout 5000 (lambda (k) 'outer)
  (lambda () (eval '(with-timeout 50 (lambda (k) 'inner) (lambda () (mutex-lock! m)))
                   (interaction-environment))))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
			c.Assert(err, qt.IsNil)
			defer func() {
				_ = engine.Close()
			}()
			result, err := evalWithWatchdog(t, engine, tc.code, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, "inner")
		})
	}
}

// TestTimerCancelledWaitIsNeverCaught pins that no exception handler sees a wait's
// timer cancellation, wherever the handler sits. In the timer's own context the
// failure becomes the interrupt before it is raised, so a with-exception-handler
// around the wait never runs; its flag is written before any foreign call, which
// the eager recheck would otherwise get to first. A guard around the wait inside
// eval, one inside a nested eval, and a guard outside the with-timeout altogether
// are all reachable from the sub-context the wait runs in. If that sub-context
// raised the failure in place, the guards inside would catch it (the caught flag),
// and the one outside would escape past the with-timeout, losing the timeout.
// The sub-context passes the failure up undispatched instead, through every
// sub-context between it and the timer's owner, so the handler runs. The eval
// shapes use a 200 ms timer so that eval has expanded and compiled before it fires.
func TestTimerCancelledWaitIsNeverCaught(t *testing.T) {
	shapes := []struct {
		name string
		code func(wait string) string
	}{
		{"handler around the wait in the timer's context", func(wait string) string {
			return `(list (with-timeout 50 (lambda (k) 'timed-out)
  (lambda () (with-exception-handler (lambda (e) (vector-set! caught 0 #t) 'handled)
               (lambda () ` + wait + ` (vector-set! after 0 #t)))))
  (vector-ref caught 0) (vector-ref after 0))`
		}},
		{"guard around the wait inside eval", func(wait string) string {
			return `(list (with-timeout 200 (lambda (k) 'timed-out)
  (lambda () (eval '(guard (e (#t (vector-set! caught 0 #t) 'caught)) ` + wait + ` (vector-set! after 0 #t))
                   (interaction-environment))))
  (vector-ref caught 0) (vector-ref after 0))`
		}},
		{"guard around the wait two sub-contexts below", func(wait string) string {
			return `(list (with-timeout 200 (lambda (k) 'timed-out)
  (lambda () (eval '(eval '(guard (e (#t (vector-set! caught 0 #t) 'caught)) ` + wait + ` (vector-set! after 0 #t))
                          (interaction-environment))
                   (interaction-environment))))
  (vector-ref caught 0) (vector-ref after 0))`
		}},
		{"guard outside the with-timeout", func(wait string) string {
			return `(list (guard (e (#t (vector-set! caught 0 #t) 'caught))
  (with-timeout 200 (lambda (k) 'timed-out)
    (lambda () (eval '(begin ` + wait + ` (vector-set! after 0 #t)) (interaction-environment)))))
  (vector-ref caught 0) (vector-ref after 0))`
		}},
	}
	for _, sh := range shapes {
		for _, w := range timerCancelledWaits {
			t.Run(sh.name+"/"+w.name, func(t *testing.T) {
				c := qt.New(t)
				engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
				c.Assert(err, qt.IsNil)
				defer func() {
					_ = engine.Close()
				}()
				result, err := evalWithWatchdog(t, engine, waitPrelude+`
(define caught (vector #f))
(define after (vector #f))
`+sh.code(w.wait), 5*time.Second)
				c.Assert(err, qt.IsNil)
				c.Assert(result.SchemeString(), qt.Equals, "(timed-out #f #f)")
			})
		}
	}
}

// TestTimerExpiryDuringEvalComputeRunsHandler pins that the owner-side rule is not
// only for waits. A timer that fires while eval's or load's sub-context computes, or
// while a macro transformer loops at expansion time inside eval, stops it at a VM
// poll or the expander's context check, neither of which has a timer to turn the
// expiry into and so returns the raw context error. That error returns through eval
// or load to the context that owns the timer, which turns it into the timeout. HEAD
// reported "context deadline exceeded" to the embedder instead, and the handler
// never ran.
func TestTimerExpiryDuringEvalComputeRunsHandler(t *testing.T) {
	looping := filepath.Join(t.TempDir(), "loop.scm")
	qt.Assert(t, os.WriteFile(looping, []byte("(let loop () (loop))\n"), 0o600), qt.IsNil)
	tcs := []struct {
		name string
		code string
	}{
		{"compute inside eval", `(eval '(let loop () (loop)) (interaction-environment))`},
		{"compute inside load", `(load "` + looping + `")`},
		{"a transformer looping at expansion inside eval",
			`(eval '(let-syntax ((m (er-macro-transformer (lambda (f r c) (let loop () (loop)))))) (m)) (interaction-environment))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
			c.Assert(err, qt.IsNil)
			defer func() {
				_ = engine.Close()
			}()
			result, err := evalWithWatchdog(t, engine,
				`(with-timeout 50 (lambda (k) 'timed-out) (lambda () `+tc.code+`))`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, "timed-out")
		})
	}
}

// TestTimerDoesNotClaimAControlTransfer pins the half of the rule that leaves
// control flow alone. After the timer fired, an escape and an abort leave eval for
// a continuation and a prompt outside the with-timeout; each is a control signal on
// its way through the owner's bridge, and must arrive, not be swallowed into the
// timeout handler. test-slow-ok returns only once the timer has fired, and the
// timer is 500 ms so that eval has reached test-slow-ok first: fired earlier, the
// cancellation itself reaches the owner, which claims it.
func TestTimerDoesNotClaimAControlTransfer(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{"call/cc escape", `(call/cc (lambda (k) (vector-set! kv 0 k)
  (with-timeout 500 (lambda (h) 'timed-out)
    (lambda () (eval '(begin (test-slow-ok) ((vector-ref kv 0) 'escaped)) (interaction-environment))))))`, "escaped"},
		{"abort to a prompt", `(call-with-continuation-prompt
  (lambda () (with-timeout 500 (lambda (h) 'timed-out)
               (lambda () (eval '(begin (test-slow-ok) (abort-current-continuation tag 'aborted))
                                (interaction-environment)))))
  tag (lambda (v) v))`, "aborted"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newSlowFailEngine(t, werr.WrapForeignErrorf(werr.ErrNotANumber, "unused"))
			result, err := evalWithWatchdog(t, engine, `
(define kv (vector #f))
(define tag (make-continuation-prompt-tag))
`+tc.code, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// hiddenHostError is a host error that wraps nothing, the shape of a host that
// reports a callback's failure in its own words.
type hiddenHostError struct {
	msg string
}

func (p hiddenHostError) Error() string {
	return p.msg
}

// TestTimerCancelledWaitInHostCallbackRunsHandler pins a wait that runs inside a
// host function: as a func() error callback the host invokes, and as a procedure
// the host re-enters the engine with through Engine.Call, whose root is not linked
// to the caller at all. Two hosts replace the failure with an error that wraps
// nothing; the timer's owner does not need to recognise what comes back, since any
// failure returning through its foreign call after its timer fired is the timeout.
// The forwarding Engine.Call host returns the failure unchanged, and there it is an
// uncaught-condition escape: the unlinked root has no timer above it, so the wait's
// failure was raised in place there and escaped. An escape is a control signal the
// owner still claims; it is the one it does.
func TestTimerCancelledWaitInHostCallbackRunsHandler(t *testing.T) {
	for _, via := range []string{"call-and-hide", "call-via-engine", "call-via-engine-forward"} {
		for _, w := range timerCancelledWaits {
			t.Run(via+"/"+w.name, func(t *testing.T) {
				c := qt.New(t)
				engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
				c.Assert(err, qt.IsNil)
				defer func() {
					_ = engine.Close()
				}()
				c.Assert(engine.RegisterFunc("call-and-hide", func(f func() error) error {
					ferr := f()
					if ferr != nil {
						return hiddenHostError{"host: step failed"}
					}
					return nil
				}), qt.IsNil)
				c.Assert(engine.RegisterFunc("call-via-engine", func(ctx context.Context, proc wile.Value) error {
					_, cerr := engine.Call(ctx, proc)
					if cerr != nil {
						return hiddenHostError{"host: call failed"}
					}
					return nil
				}), qt.IsNil)
				c.Assert(engine.RegisterFunc("call-via-engine-forward", func(ctx context.Context, proc wile.Value) error {
					_, cerr := engine.Call(ctx, proc)
					return cerr
				}), qt.IsNil)
				result, err := evalWithWatchdog(t, engine, waitPrelude+`
(define after (vector #f))
(list (with-timeout 50 (lambda (k) 'timed-out)
        (lambda () (`+via+` (lambda () `+w.wait+` (vector-set! after 0 #t)))))
      (vector-ref after 0))`, 5*time.Second)
				c.Assert(err, qt.IsNil)
				c.Assert(result.SchemeString(), qt.Equals, "(timed-out #f)")
			})
		}
	}
}

// newSlowFailEngine builds a KitchenSink engine with three primitives that wait
// until their context is cancelled and then ignore the cancellation:
// test-slow-fail and test-slow-fail-1 (one ignored argument, for use as a handler)
// return fail, and test-slow-ok returns #t 100 ms after. Each returns only once a
// timer has fired, with no recheck or poll in between, so a row that needs the
// timer fired never runs before it, and a row that pins what a fired timer must not
// claim always reaches the fired-timer branch. Every use is under a timer that
// fires.
func newSlowFailEngine(t *testing.T, fail error) *wile.Engine {
	t.Helper()
	slowFail := func(mc machine.CallContext) error {
		<-mc.Context().Done()
		return fail
	}
	ext := registry.NewExtension("test-slow", func(r *registry.PrimitiveRegistry) error {
		r.AddPrimitive(registry.PrimitiveSpec{Name: "test-slow-fail", ParamCount: 0, Impl: slowFail}, registry.PhaseSetRuntime)
		r.AddPrimitive(registry.PrimitiveSpec{Name: "test-slow-fail-1", ParamCount: 1, Impl: slowFail}, registry.PhaseSetRuntime)
		r.AddPrimitive(registry.PrimitiveSpec{Name: "test-slow-ok", ParamCount: 0, Impl: slowOK}, registry.PhaseSetRuntime)
		return nil
	})
	engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink), wile.WithExtension(ext))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = engine.Close()
	})
	return engine
}

// slowOK is test-slow-ok: it waits until its context is cancelled and returns #t
// 100 ms after.
func slowOK(mc machine.CallContext) error {
	<-mc.Context().Done()
	// Linger past the cancellation, so a thread the same timer cancelled has
	// died (and abandoned what it owned) before the caller's next step.
	time.Sleep(100 * time.Millisecond)
	mc.SetValue(values.TrueValue)
	return nil
}

// newCancelFailEngine builds a KitchenSink engine with test-slow-ok and
// test-cancel-and-fail, which cancels the returned context and then returns
// ErrNotANumber. Run under that context, a program can put the embedder's
// cancellation after a timer fires and before a failure, with no clock between
// them.
func newCancelFailEngine(t *testing.T) (*wile.Engine, context.Context) {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	ext := registry.NewExtension("test-cancel", func(r *registry.PrimitiveRegistry) error {
		r.AddPrimitive(registry.PrimitiveSpec{Name: "test-slow-ok", ParamCount: 0, Impl: slowOK}, registry.PhaseSetRuntime)
		r.AddPrimitive(registry.PrimitiveSpec{
			Name:       "test-cancel-and-fail",
			ParamCount: 0,
			Impl: func(machine.CallContext) error {
				cancel()
				return werr.WrapForeignErrorf(werr.ErrNotANumber, "test-cancel-and-fail: not a number")
			},
		}, registry.PhaseSetRuntime)
		return nil
	})
	engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink), wile.WithExtension(ext))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = engine.Close()
	})
	return engine, ctx
}

// TestTimerClaimsAHandlerFailure pins the handler applications the drivers make
// themselves, where a failure comes back from the application rather than from Run.
// A call-with-continuation-prompt handler that fails after the timer fired, applied
// by an abort the sub-context resolves; and a with-timeout handler that fails after
// an outer timer also fired, applied by the inner timer's resolution. Each failure
// becomes a timer's interrupt (or, for a handler that fails without a foreign
// call, is claimed by the fired timer), which the driver must dispatch on its own
// chain. Returned instead, it reached the embedder as a raw interrupt or as the
// handler's failure (top level), or reached the top driver, which could not find
// the boundary and reported an internal error (inside eval). The arity rows' inner
// handler takes no argument. Its timer fires first, during test-slow-ok, which
// outlasts the outer timer too, so the outer timer has fired when the inner
// handler's application fails and the failure is the outer timer's to claim.
func TestTimerClaimsAHandlerFailure(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{"prompt handler, timer inside eval", `(eval '(with-timeout 50 (lambda (k) 'timed-out)
  (lambda () (let ((tag (make-continuation-prompt-tag)))
               (call-with-continuation-prompt (lambda () (abort-current-continuation tag)) tag test-slow-fail))))
  (interaction-environment))`, "timed-out"},
		{"inner with-timeout handler, top level", `(with-timeout 30 (lambda (k) 'outer)
  (lambda () (with-timeout 10 test-slow-fail-1 (lambda () (thread-sleep! 10)))))`, "outer"},
		{"inner with-timeout handler, inside eval", `(eval '(with-timeout 30 (lambda (k) 'outer)
  (lambda () (with-timeout 10 test-slow-fail-1 (lambda () (thread-sleep! 10)))))
  (interaction-environment))`, "outer"},
		{"inner with-timeout handler of the wrong arity", `(with-timeout 50 (lambda (k) 'outer)
  (lambda () (with-timeout 10 (lambda () 'no-k) (lambda () (test-slow-ok)))))`, "outer"},
		{"inner with-timeout handler of the wrong arity, inside eval", `(eval '(with-timeout 50 (lambda (k) 'outer)
  (lambda () (with-timeout 10 (lambda () 'no-k) (lambda () (test-slow-ok)))))
  (interaction-environment))`, "outer"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newSlowFailEngine(t, werr.WrapForeignErrorf(werr.ErrNotANumber, "test-slow-fail: not a number"))
			result, err := evalWithWatchdog(t, engine, tc.code, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestEscapeFromATimerEndsIt pins that leaving a with-timeout's extent by an escape
// ends its timer, as a normal return does: the escape exits the timer's winding
// frame, which cancels the timer and restores the context outside it. Left armed,
// the timer fired during the later sleep, which then raised its cancellation; and a
// poll or recheck after it named a boundary that was gone (an internal error). Each
// row is one form: EvalMultiple runs each top-level form on a fresh context.
func TestEscapeFromATimerEndsIt(t *testing.T) {
	// then is the escape followed by work that outlives its 50 ms timer: a sleep, then
	// enough instructions for a poll.
	then := func(escape string) string {
		return `(list ` + escape + `
      (begin (thread-sleep! 0.15) 'slept)
      (let loop ((i 0)) (if (< i 100000) (loop (+ i 1)) 'looped)))`
	}
	const guarded = `(guard (e (#t 'caught)) (with-timeout 50 (lambda (k) 'h) (lambda () (raise 'x))))`
	tcs := []struct {
		name string
		code string
		want string
	}{
		{"guard", then(guarded), "(caught slept looped)"},
		{"call/cc", then(`(call/cc (lambda (out) (with-timeout 50 (lambda (k) 'h) (lambda () (out 'escaped)))))`), "(escaped slept looped)"},
		{"prompt abort", then(`(call-with-continuation-prompt
  (lambda () (with-timeout 50 (lambda (k) 'h)
               (lambda () (abort-current-continuation (default-continuation-prompt-tag) 'x))))
  (default-continuation-prompt-tag) (lambda args 'aborted))`), "(aborted slept looped)"},
		// The work stays inside eval: a timer eval's sub-context arms cancels only that
		// sub-context's ctx, which work after eval returns never runs under.
		{"guard, inside eval", `(eval '` + then(guarded) + ` (interaction-environment))`, "(caught slept looped)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
			c.Assert(err, qt.IsNil)
			defer func() {
				_ = engine.Close()
			}()
			result, err := evalWithWatchdog(t, engine, tc.code, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestEscapeFromATimerCancelsWhatDerivesFromIt pins that an escape ends the timer's
// context as a normal return does: a thread started inside the with-timeout, whose
// context derives from the timer's, is cancelled either way, so its wait ends
// before the join's timeout. An escape that only restored the context outside the
// timer, without cancelling it, left the thread waiting.
func TestEscapeFromATimerCancelsWhatDerivesFromIt(t *testing.T) {
	tcs := []struct {
		name  string
		leave string
		want  string
	}{
		{"escape", `(raise 'x)`, "(caught thread-ended)"},
		{"normal return", `'ret`, "(ret thread-ended)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newEngine(t)
			defer func() {
				_ = engine.Close()
			}()
			result, err := evalWithWatchdog(t, engine, `
(define m (make-mutex))
(mutex-lock! m)
(define th (vector #f))
(list (guard (e (#t 'caught))
        (with-timeout 5000 (lambda (k) 'h)
          (lambda () (vector-set! th 0 (thread-start! (make-thread (lambda () (mutex-lock! m) 'got)))) `+tc.leave+`)))
      (guard (e (#t 'thread-ended))
        (thread-join! (vector-ref th 0) 1 'still-waiting)))`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestTimerExpiryNamesTheTimerThatFired pins that an expiry is answered by the
// with-timeout whose timer fired, not by the innermost one armed. An outer timer's
// expiry ran an armed inner with-timeout's handler first, early (the outer handler
// then ran when the inner one returned under the fired outer timer, so the result
// alone hid it); a starting thread's ran
// the started thread's own with-timeout handler, whose timer had not fired. The
// started thread owns no timer that fired, so its wait raises, and the starter's
// handler, which joins it, finds its own handler never ran.
func TestTimerExpiryNamesTheTimerThatFired(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{"outer timer, inner one armed", `(with-timeout 50 (lambda (k) (list 'outer (vector-ref ran 0)))
  (lambda () (with-timeout 5000 (lambda (k) (vector-set! ran 0 'inner-ran) 'inner) (lambda () (mutex-lock! m)))))`, "(outer #f)"},
		{"outer timer, inner one armed inside eval", `(with-timeout 50 (lambda (k) (list 'outer (vector-ref ran 0)))
  (lambda () (eval '(with-timeout 5000 (lambda (k) (vector-set! ran 0 'inner-ran) 'inner) (lambda () (mutex-lock! m)))
                   (interaction-environment))))`, "(outer #f)"},
		// The inner handler is a continuation: a closure handler would be cut off at
		// its first instruction by the poll after the interrupt (filed in TODO.md),
		// which hid which handler the poll named.
		{"outer timer, inner one armed, at the poll", `(call/cc (lambda (out)
  (with-timeout 50 (lambda (k) 'outer)
    (lambda () (with-timeout 5000 out (lambda () (let loop () (loop))))))))`, "outer"},
		{"outer timer, inner one armed, at the recheck after a foreign call", `(with-timeout 50 (lambda (k) (list 'outer (vector-ref ran 0)))
  (lambda () (with-timeout 5000 (lambda (k) (vector-set! ran 0 'inner-ran) 'inner) (lambda () (test-slow-ok)))))`, "(outer #f)"},
		{"outer timer, inner one armed, at the recheck after applying a primitive", `(with-timeout 50 (lambda (k) (list 'outer (vector-ref ran 0)))
  (lambda () (with-timeout 5000 (lambda (k) (vector-set! ran 0 'inner-ran) 'inner) test-slow-ok)))`, "(outer #f)"},
		{"a starter's timer, the started thread's own armed", `(with-timeout 50
  (lambda (k) (guard (e (#t #f)) (thread-join! (vector-ref th 0))) (vector-ref ran 0))
  (lambda ()
    (vector-set! th 0 (thread-start! (make-thread (lambda ()
      (with-timeout 5000 (lambda (k) (vector-set! ran 0 'own-handler)) (lambda () (mutex-lock! m)))))))
    (thread-join! (vector-ref th 0))))`, "#f"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newSlowFailEngine(t, werr.WrapForeignErrorf(werr.ErrNotANumber, "test-slow-fail: not a number"))
			_, err := evalWithWatchdog(t, engine, `
(define m (make-mutex))
(mutex-lock! m)
(define ran (vector #f))
(define th (vector #f))`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			result, err := evalWithWatchdog(t, engine, tc.code, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestThreadExitLeavesTheStartersExtents pins that a thread leaving its root,
// by finishing or by an abort to its own boundary, unwinds only the extents it
// entered. It inherits its starter's winding stack, and both used to unwind all of
// it: the starter's after thunk ran in the thread and again in the starter, and
// once a with-timeout's extent was a winding frame the thread ended the starter's
// timer, so the starter's next wait raised a plain cancellation instead of
// becoming the timeout.
func TestThreadExitLeavesTheStartersExtents(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{"dynamic-wind", `(dynamic-wind (lambda () (note 'in))
  (lambda () (thread-join! (thread-start! (make-thread (lambda () (note 'thread-done))))))
  (lambda () (note 'out)))
(reverse (vector-ref log 0))`, "(in thread-done out)"},
		{"with-timeout", `(with-timeout 300 (lambda (k) 'timed-out)
  (lambda () (thread-join! (thread-start! (make-thread (lambda () 'done)))) (mutex-lock! m) 'past-lock))`, "timed-out"},
		{"dynamic-wind, the thread aborting to its boundary", `(dynamic-wind (lambda () (note 'in))
  (lambda () (note (list 'joined (thread-join! (thread-start! (make-thread (lambda ()
    (abort-current-continuation (default-continuation-prompt-tag) 'x))))))))
  (lambda () (note 'out)))
(reverse (vector-ref log 0))`, "(in (joined x) out)"},
		{"with-timeout, the thread aborting to its boundary", `(with-timeout 300 (lambda (k) 'timed-out)
  (lambda () (thread-join! (thread-start! (make-thread (lambda ()
    (abort-current-continuation (default-continuation-prompt-tag) 'x)))))
    (mutex-lock! m) 'past-lock))`, "timed-out"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newEngine(t)
			defer func() {
				_ = engine.Close()
			}()
			result, err := evalWithWatchdog(t, engine, `
(define log (vector '()))
(define (note x) (vector-set! log 0 (cons x (vector-ref log 0))))
(define m (make-mutex))
(mutex-lock! m)
`+tc.code, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestEscapeOutOfASubContextTimerKeepsTheOwners pins that leaving a with-timeout
// armed inside eval, by an escape the owner reconciles, ends that timer without
// touching the owner's own: the owner exits a frame for a timer it does not arm,
// so it cancels the timer and restores nothing. Restoring regardless set the
// owner's timer to the inner one's parent, nil, so the owner's later expiry found
// no armed timer and its wait raised a plain cancellation.
func TestEscapeOutOfASubContextTimerKeepsTheOwners(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer func() {
		_ = engine.Close()
	}()
	result, err := evalWithWatchdog(t, engine, `
(define m (make-mutex))
(mutex-lock! m)
(define kv (vector #f))
(with-timeout 300 (lambda (k) 'outer)
  (lambda ()
    (call/cc (lambda (k) (vector-set! kv 0 k)
      (eval '(with-timeout 5000 (lambda (k) 'inner) (lambda () ((vector-ref kv 0) 'escaped))) (interaction-environment))))
    (mutex-lock! m)
    'past-lock))`, 5*time.Second)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "outer")
}

// TestReturnThroughAReenteredTimerKeepsTheLiveOne pins that returning through a
// with-timeout whose extent was re-entered, after it ended, leaves the timers
// armed now alone. A composable continuation captured inside an inner with-timeout
// is applied inside another one; its segment returns through the inner one's
// finalizer, which restored the timer and context saved when the inner one began,
// so the live with-timeout's timer was no longer armed and its expiry was lost.
func TestReturnThroughAReenteredTimerKeepsTheLiveOne(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	defer func() {
		_ = engine.Close()
	}()
	result, err := evalWithWatchdog(t, engine, `
(define m (make-mutex))
(mutex-lock! m)
(define dtag (default-continuation-prompt-tag))
(define kv (vector #f))
(call-with-continuation-prompt
  (lambda () (with-timeout 5000 (lambda (k) 'A)
               (lambda () (call-with-composable-continuation (lambda (k) (vector-set! kv 0 k) 0) dtag))))
  dtag (lambda args args))
(with-timeout 300 (lambda (k) 'timed-out-B) (lambda () ((vector-ref kv 0) 7) (mutex-lock! m) 'past-lock))`, 5*time.Second)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "timed-out-B")
}

// TestCallCCInEvalKeepsTheExtentsAroundIt pins the delimiting depth of a call/cc
// captured inside eval: eval's sub-context has no default prompt of its own and
// hands the resume to the driver above it, so the continuation carries the extents
// the driver's prompt does not. Delimited at eval's own boundary instead, it left
// out the extents between the driver and the eval call, and its resume unwound
// them: the enclosing dynamic-wind's after thunk ran before the rest of eval, and
// an enclosing with-timeout's timer ended. The log is read by a second evaluation,
// because the resume also discards the rest of the top-level form (filed in
// TODO.md).
func TestCallCCInEvalKeepsTheExtentsAroundIt(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer func() {
		_ = engine.Close()
	}()
	_, err = evalWithWatchdog(t, engine, `
(define log (vector '()))
(define (note x) (vector-set! log 0 (cons x (vector-ref log 0))))`, 5*time.Second)
	c.Assert(err, qt.IsNil)
	_, err = evalWithWatchdog(t, engine, `(dynamic-wind (lambda () (note 'in))
  (lambda () (eval '(begin (note 'e1) (call/cc (lambda (r) (r 1))) (note 'e2) 7) (interaction-environment)))
  (lambda () (note 'out)))`, 5*time.Second)
	c.Assert(err, qt.IsNil)
	logged, err := evalWithWatchdog(t, engine, `(reverse (vector-ref log 0))`, 5*time.Second)
	c.Assert(err, qt.IsNil)
	c.Assert(logged.SchemeString(), qt.Equals, "(in e1 e2 out)")
}

// TestUnfiredTimerDrawsNothingPastAGuard pins that a with-timeout whose timer has
// not fired claims no failure below it, even on a context the embedder cancelled:
// the failure inside eval is eval's own, and its guard sees it. The pass-up asks
// whether a parent arms the timer whose expiry cancelled the failing context
// (timerAbove); an embedder's cancellation names no timer.
func TestUnfiredTimerDrawsNothingPastAGuard(t *testing.T) {
	c := qt.New(t)
	engine, ctx := newCancelFailEngine(t)
	_, _ = evalWithWatchdogCtx(ctx, t, engine, `
(define seen (vector #f))
(with-timeout 20000 (lambda (k) 'outer)
  (lambda ()
    (eval '(guard (e (#t (vector-set! seen 0 'nested-guard))) (test-cancel-and-fail))
          (interaction-environment))))`, 5*time.Second)
	result, err := evalWithWatchdog(t, engine, `(vector-ref seen 0)`, 5*time.Second)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "nested-guard")
}

// TestOuterExpiryTearsDownTheInnerTimer pins that the outer timer's interrupt, with
// an inner one armed, tears both down before the handler runs
// (resolveTimerInterrupt's walk to the timer the interrupt names). Tearing down
// only the innermost left the fired outer timer armed: the handler ran under its
// cancelled context, was interrupted, and ran again, and the work after the
// with-timeout was cancelled too.
func TestOuterExpiryTearsDownTheInnerTimer(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	defer func() {
		_ = engine.Close()
	}()
	result, err := evalWithWatchdog(t, engine, `
(define m (make-mutex))
(mutex-lock! m)
(list (call/cc (lambda (out)
        (with-timeout 50 (lambda (k) (out 'outer))
          (lambda () (with-timeout 5000 (lambda (k) 'inner) (lambda () (mutex-lock! m)))))))
      (begin (thread-sleep! 0.01) 'slept))`, 5*time.Second)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "(outer slept)")
}

// TestEscapePastATimerLeavesItsExtent pins the dynamic environment an enclosing
// dynamic-wind's after thunk runs in when an escape leaves a with-timeout through
// it. The after thunk belongs to the dynamic-wind's call site, outside the
// with-timeout (R7RS §6.10), so the timer is no part of it: its 150 ms wait must
// run to completion although the 50 ms timer fires meanwhile, the escape must
// arrive, and the after thunk must run once. Run under the timer's context, the
// wait was cancelled; the owner-side rule then turned that into the timeout, which
// lost the escape and re-entered the extent so the after thunk ran again. HEAD
// arrived, but cut the wait short. The log is read by a second evaluation.
func TestEscapePastATimerLeavesItsExtent(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{"call/cc escape", `(call/cc (lambda (return)
  (dynamic-wind (lambda () (note 'before))
                (lambda () (with-timeout 50 (lambda (k) 'timed-out) (lambda () (return 'escaped))))
                (lambda () (thread-sleep! 0.15) (note 'after)))))`, "escaped"},
		{"call/cc escape, after thunk joins a thread", `(begin (thread-start! worker) (call/cc (lambda (return)
  (dynamic-wind (lambda () (note 'before))
                (lambda () (with-timeout 50 (lambda (k) 'timed-out) (lambda () (return 'escaped))))
                (lambda () (thread-join! worker) (note 'after))))))`, "escaped"},
		{"call/cc escape past two timers", `(call/cc (lambda (return)
  (dynamic-wind (lambda () (note 'before))
                (lambda () (with-timeout 50 (lambda (k) 'outer)
                             (lambda () (with-timeout 5000 (lambda (k) 'inner) (lambda () (return 'escaped))))))
                (lambda () (thread-sleep! 0.15) (note 'after)))))`, "escaped"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newEngine(t)
			defer func() {
				_ = engine.Close()
			}()
			_, err := evalWithWatchdog(t, engine, `
(define log (vector '()))
(define (note x) (vector-set! log 0 (cons x (vector-ref log 0))))
(define worker (make-thread (lambda () (thread-sleep! 0.15) 'worker-done)))`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			start := time.Now()
			result, err := evalWithWatchdog(t, engine, tc.code, 5*time.Second)
			elapsed := time.Since(start)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
			c.Assert(elapsed >= 140*time.Millisecond, qt.IsTrue,
				qt.Commentf("the after thunk's wait was cut short: %v", elapsed))
			logged, err := evalWithWatchdog(t, engine, `(reverse (vector-ref log 0))`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(logged.SchemeString(), qt.Equals, "(before after)")
		})
	}
}

// TestAfterThunkEnteredBelowTheTimerStaysInsideIt pins the other side of
// TestEscapePastATimerLeavesItsExtent. A dynamic-wind entered inside eval or load,
// under a with-timeout in force at top level, is inside that timer, though the
// sub-context it runs on has no timer of its own: its ctx is the timer's, and
// load's is the timer's wrapped with load's own value. When a call/cc escape there
// runs the after thunk (reconciled by the top driver, which owns the timer), the
// thunk must run under the timer, so its wait is cancelled and the handler runs.
// The reconcile runs a thunk in the dynamic environment its frame's position on
// the winding stack gives it: above the top timer's frame, under that timer. Under
// a with-timeout of eval's own, the frame is above that timer's frame too, which
// the reconciling top driver does not arm, and the thunk stays under the top
// timer. The last entry is at top level, left from inside a later with-timeout:
// the escape exits that one first, and the thunk runs under the outer one.
func TestAfterThunkEnteredBelowTheTimerStaysInsideIt(t *testing.T) {
	const escape = `(k0 'escaped)`
	form := func(body, wait string) string {
		return `(call/cc (lambda (k0)
  (dynamic-wind (lambda () #f)
                (lambda () ` + body + `)
                (lambda () ` + wait + ` (vector-set! after 0 #t) (mutex-unlock! m)))))`
	}
	entries := []struct {
		name  string
		below func(t *testing.T, wait string) string
	}{
		{"eval", func(_ *testing.T, wait string) string {
			return `(eval '` + form(escape, wait) + ` (interaction-environment))`
		}},
		{"eval, under a with-timeout of its own", func(_ *testing.T, wait string) string {
			return `(eval '(with-timeout 5000 (lambda (k) 'inner) (lambda () ` + form(escape, wait) + `)) (interaction-environment))`
		}},
		{"load", func(t *testing.T, wait string) string {
			path := filepath.Join(t.TempDir(), "after-thunk.scm")
			qt.Assert(t, os.WriteFile(path, []byte(form(escape, wait)), 0o600), qt.IsNil)
			return `(load "` + path + `")`
		}},
		{"top level, left from inside a later with-timeout", func(_ *testing.T, wait string) string {
			return form(`(with-timeout 5000 (lambda (k) 'inner) (lambda () `+escape+`))`, wait)
		}},
	}
	for _, e := range entries {
		for _, w := range timerCancelledWaits {
			t.Run(e.name+"/"+w.name, func(t *testing.T) {
				c := qt.New(t)
				engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
				c.Assert(err, qt.IsNil)
				defer func() {
					_ = engine.Close()
				}()
				result, err := evalWithWatchdog(t, engine, waitPrelude+`
(define after (vector #f))
(define h (with-timeout 50 (lambda (k) 'timed-out)
  (lambda () `+e.below(t, w.wait)+`)))
(list h (vector-ref after 0) (mutex-state m))`, 5*time.Second)
				c.Assert(err, qt.IsNil)
				c.Assert(result.SchemeString(), qt.Equals, "(timed-out #f not-owned)")
			})
		}
	}
}

// TestAfterThunkOutsideAFiredTimerKeepsItsGuard pins what the pass-up claims. An
// escape leaves a with-timeout through an enclosing dynamic-wind whose after thunk
// runs outside the timer; the timer fires while the thunk sleeps, and the thunk
// then fails. Its context was not cancelled by the timer, so the failure is
// ordinary: the thunk's own guard catches it although a fired timer is above it,
// and the handler never runs. Passing up every failure under a fired timer,
// whatever cancelled the failing context, took that guard away and lost the
// escape. The embedder row cancels the thunk's context just before the failure:
// a context the embedder cancelled was not cancelled by the timer either.
func TestAfterThunkOutsideAFiredTimerKeepsItsGuard(t *testing.T) {
	tcs := []struct {
		name string
		fail string
	}{
		{"ordinary failure", `(car '())`},
		{"failure after the embedder cancels", `(test-cancel-and-fail)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine, ctx := newCancelFailEngine(t)
			_, err := evalWithWatchdog(t, engine, `
(define log (vector '()))
(define (note x) (vector-set! log 0 (cons x (vector-ref log 0))))`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			// The embedder row ends on its own cancellation, so only the log is asserted.
			_, _ = evalWithWatchdogCtx(ctx, t, engine, `(call/cc (lambda (return)
  (dynamic-wind (lambda () #f)
                (lambda () (with-timeout 50 (lambda (k) (note 'handler) 'timed-out) (lambda () (return 'escaped))))
                (lambda () (thread-sleep! 0.15) (guard (e (#t (note 'thunk-guard))) `+tc.fail+`)))))`, 5*time.Second)
			logged, err := evalWithWatchdog(t, engine, `(vector-ref log 0)`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(logged.SchemeString(), qt.Equals, "(thunk-guard)")
		})
	}
}

// TestWindingThunkFailureOutsideAFiredTimerIsItsOwn pins the uncaught half of
// TestAfterThunkOutsideAFiredTimerKeepsItsGuard. A winding thunk runs outside the
// timer and fails with no handler after the timer fired. The failure is the
// thunk's, so it ends the evaluation as itself, and the handler never runs. The
// driver reconciling the transfer had not yet left the timer's extent, so judged by
// its state the failure was claimed: the handler ran, the transfer was lost, and an
// escape's after thunk ran a second time without a before. The rows fail each way a
// reconcile can: an after thunk on an escape and on an abort, failing as it runs or
// as it is applied (the outer frame's after thunk is car, applied to no argument,
// once the inner one has outlived the timer); and a before thunk on a re-entry from
// inside a later with-timeout, failing as it runs or as it is applied (newline, to
// a port the outer before thunk closed after outliving the timer).
func TestWindingThunkFailureOutsideAFiredTimerIsItsOwn(t *testing.T) {
	escape := func(form string) string {
		return `(call/cc (lambda (k0)
  (dynamic-wind (lambda () (note 'before))
                (lambda () (with-timeout 50 (lambda (k) (note 'handler) 'timed-out) (lambda () ` + form + `)))
                (lambda () (note 'after) (thread-sleep! 0.15) (error "boom")))))`
	}
	// reenter re-enters inner from inside a 50 ms with-timeout; outer's before thunk
	// notes before-again and outlives the timer on the re-entry, then runs again.
	reenter := func(again, inner string) string {
		return `(let ((r (dynamic-wind
           (lambda () (vector-set! n 0 (+ 1 (vector-ref n 0)))
                      (if (> (vector-ref n 0) 1)
                          (begin (note 'before-again) (thread-sleep! 0.15) ` + again + `)
                          (note 'before)))
           (lambda () ` + inner + `)
           (lambda () (note 'after)))))
  (if (eq? r 'first)
      (with-timeout 50 (lambda (h) (note 'handler) 'timed-out) (lambda () ((vector-ref kk 0) 'reentered)))
      r))`
	}
	const capture = `(call/cc (lambda (k) (vector-set! kk 0 k) 'first))`
	tcs := []struct {
		name string
		code string
		err  string
		log  string
	}{
		{"after thunk, call/cc escape", escape(`(k0 'escaped)`), `boom`, "(before after)"},
		{"after thunk, abort", `(let ((tag (make-continuation-prompt-tag)))
  (call-with-continuation-prompt
    (lambda () ` + escape(`(abort-current-continuation tag 'escaped)`) + `)
    tag
    (lambda (v) v)))`, `boom`, "(before after)"},
		{"after thunk applied", `(call/cc (lambda (k0)
  (dynamic-wind (lambda () (note 'before))
                (lambda () (dynamic-wind (lambda () #f)
                                         (lambda () (with-timeout 50 (lambda (k) (note 'handler) 'timed-out)
                                                      (lambda () (k0 'escaped))))
                                         (lambda () (note 'after) (thread-sleep! 0.15))))
                car)))`, `expected 1 argument`, "(before after)"},
		{"before thunk, re-entry", reenter(`(car '())`, capture), `car`, "(before after before-again)"},
		{"before thunk applied, re-entry", reenter(`(close-port p)`,
			`(parameterize ((current-output-port p)) (dynamic-wind newline (lambda () `+capture+`) (lambda () #f)))`),
			`newline`, "(before after before-again)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
			c.Assert(err, qt.IsNil)
			defer func() {
				_ = engine.Close()
			}()
			_, err = evalWithWatchdog(t, engine, `
(define log (vector '()))
(define (note x) (vector-set! log 0 (cons x (vector-ref log 0))))
(define kk (vector #f))
(define n (vector 0))
(define p (open-output-string))`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			result, err := evalWithWatchdog(t, engine, tc.code, 5*time.Second)
			c.Assert(err, qt.ErrorMatches, `(?s).*`+tc.err+`.*`, qt.Commentf("result %v", result))
			logged, err := evalWithWatchdog(t, engine, `(reverse (vector-ref log 0))`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(logged.SchemeString(), qt.Equals, tc.log)
		})
	}
}

// TestUnclaimedFailureIsClaimedByAnotherOwner pins that a failure its own context
// did not claim, because it ran outside that context's fired timer
// (TestWindingThunkFailureOutsideAFiredTimerIsItsOwn), is still the timeout for
// another context whose timer fired when the failure reaches it, as any failure
// is: a thread joining the failed one, below its timer's owner, and the caller of
// a host function that ran the failing code through Engine.Call. A mark on the
// failure, honoured everywhere, once let the joiner's guard catch the joinee's end
// and the host's caller report the failure as itself, when the failing code's own
// timer had happened to fire.
func TestUnclaimedFailureIsClaimedByAnotherOwner(t *testing.T) {
	// failing escapes a 20 ms with-timeout through a dynamic-wind whose after
	// thunk outlives that timer and then raises, uncaught.
	const failing = `(lambda ()
  (call/cc (lambda (k0)
    (dynamic-wind (lambda () #f)
                  (lambda () (with-timeout 20 (lambda (k) 'inner) (lambda () (k0 'escaped))))
                  (lambda () (thread-sleep! 0.25) (raise 'boom))))))`
	t.Run("joiner below its timer's owner", func(t *testing.T) {
		c := qt.New(t)
		engine := newSlowFailEngine(t, werr.WrapForeignErrorf(werr.ErrNotANumber, "unused"))
		_, err := evalWithWatchdog(t, engine, `
(define t (make-thread `+failing+`))
(thread-start! t)
(guard (e (#t 'joined)) (thread-join! t))`, 5*time.Second)
		c.Assert(err, qt.IsNil)
		result, err := evalWithWatchdog(t, engine, `
(define caught (vector #f))
(list (with-timeout 50 (lambda (k) 'timed-out)
        (lambda () (eval '(guard (e (#t (vector-set! caught 0 #t) 'caught)) (test-slow-ok) (thread-join! t))
                         (interaction-environment))))
      (vector-ref caught 0))`, 5*time.Second)
		c.Assert(err, qt.IsNil)
		c.Assert(result.SchemeString(), qt.Equals, "(timed-out #f)")
	})
	t.Run("caller of an Engine.Call host", func(t *testing.T) {
		// The host returns the failure only once its caller's timer has fired.
		c := qt.New(t)
		engine := newEngine(t)
		defer func() {
			_ = engine.Close()
		}()
		c.Assert(engine.RegisterFunc("call-then-wait", func(ctx context.Context, proc wile.Value) error {
			_, cerr := engine.Call(context.Background(), proc)
			<-ctx.Done()
			return cerr
		}), qt.IsNil)
		result, err := evalWithWatchdog(t, engine, `
(with-timeout 50 (lambda (k) 'timed-out) (lambda () (call-then-wait `+failing+`)))`, 5*time.Second)
		c.Assert(err, qt.IsNil)
		c.Assert(result.SchemeString(), qt.Equals, "timed-out")
	})
}

// TestReentryUnderATimerRewindsOutsideIt pins the rewind half of
// TestEscapePastATimerLeavesItsExtent. A dynamic-wind entered with no timer is
// re-entered by a continuation invoked inside a later with-timeout; the before
// thunk runs again, in the dynamic environment of the dynamic-wind call, so outside
// that timer, and its 150 ms sleep completes although the 50 ms timer fires
// meanwhile. Run under the timer's context, the sleep was cut short and the handler
// ran in place of the re-entry. The jump leaves the with-timeout, which ends its
// timer, so the form returns the re-entry's value; the log is read by a further
// evaluation.
func TestReentryUnderATimerRewindsOutsideIt(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	defer func() {
		_ = engine.Close()
	}()
	_, err := evalWithWatchdog(t, engine, `
(define log (vector '()))
(define (note x) (vector-set! log 0 (cons x (vector-ref log 0))))
(define kk (vector #f))
(define n (vector 0))`, 5*time.Second)
	c.Assert(err, qt.IsNil)
	start := time.Now()
	result, err := evalWithWatchdog(t, engine, `(let ((r (dynamic-wind
           (lambda () (vector-set! n 0 (+ 1 (vector-ref n 0)))
                      (if (> (vector-ref n 0) 1) (begin (thread-sleep! 0.15) (note 'slept)))
                      (note 'before))
           (lambda () (call/cc (lambda (k) (vector-set! kk 0 k) 'first)))
           (lambda () (note 'after)))))
  (if (eq? r 'first)
      (with-timeout 50 (lambda (h) (note 'handler) 'timed-out) (lambda () ((vector-ref kk 0) 'reentered)))
      r))`, 5*time.Second)
	elapsed := time.Since(start)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "reentered")
	c.Assert(elapsed >= 140*time.Millisecond, qt.IsTrue,
		qt.Commentf("the before thunk's sleep was cut short: %v", elapsed))
	logged, err := evalWithWatchdog(t, engine, `(reverse (vector-ref log 0))`, 5*time.Second)
	c.Assert(err, qt.IsNil)
	c.Assert(logged.SchemeString(), qt.Equals, "(before after slept before after)")
}

// TestReentryInsideATimerStaysUnderIt pins that a continuation applied inside a
// with-timeout, whose frames come to sit above the timer's, keeps the timer: a
// composable continuation composes onto the invoker's winding stack, and a call/cc
// continuation reinstated at a prompt inside the with-timeout keeps the frames
// outside that prompt. Either used to reconcile to the captured stack as an
// absolute target, which unwound the timer's frame and ended the timer, so the
// wait after the re-entry was never cancelled.
func TestReentryInsideATimerStaysUnderIt(t *testing.T) {
	tcs := []struct {
		name    string
		capture string
		reenter string
	}{
		{"composable continuation",
			`(call-with-composable-continuation (lambda (k) (vector-set! kv 0 k) 0) dtag)`,
			`((vector-ref kv 0) 7)`},
		{"call/cc continuation at an inner prompt",
			`(call/cc (lambda (k) (vector-set! kv 0 k) 0))`,
			`(call-with-continuation-prompt (lambda () ((vector-ref kv 0) 7)) dtag (lambda args args))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newEngine(t)
			defer func() {
				_ = engine.Close()
			}()
			result, err := evalWithWatchdog(t, engine, `
(define m (make-mutex))
(mutex-lock! m)
(define dtag (default-continuation-prompt-tag))
(define kv (vector #f))
(call-with-continuation-prompt (lambda () (+ 1 `+tc.capture+`)) dtag (lambda args args))
(with-timeout 300 (lambda (k) 'timed-out) (lambda () `+tc.reenter+` (mutex-lock! m) 'past-lock))`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, "timed-out")
		})
	}
}

// TestReentryIntoAnEndedTimerKeepsTheEnclosingOne pins the step out of a timer
// that has ended. A dynamic-wind entered inside an inner with-timeout, itself
// inside an outer one, is re-entered after the inner one returned, from inside the
// outer one. The inner timer is over (its finalizer ended it), and re-entering its
// frame arms nothing, so the before thunk runs under the outer timer, the one
// still armed. Run under the ended timer's context, the before thunk was cancelled
// at its first instruction and the re-entry failed.
func TestReentryIntoAnEndedTimerKeepsTheEnclosingOne(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	defer func() {
		_ = engine.Close()
	}()
	_, err := evalWithWatchdog(t, engine, `
(define log (vector '()))
(define (note x) (vector-set! log 0 (cons x (vector-ref log 0))))
(define kk (vector #f))`, 5*time.Second)
	c.Assert(err, qt.IsNil)
	result, err := evalWithWatchdog(t, engine, `(with-timeout 5000 (lambda (h) 'outer)
  (lambda ()
    (let ((r (with-timeout 4000 (lambda (h) 'inner)
               (lambda () (dynamic-wind (lambda () (note 'before))
                                        (lambda () (call/cc (lambda (k) (vector-set! kk 0 k) 'first)))
                                        (lambda () (note 'after)))))))
      (if (eq? r 'first) ((vector-ref kk 0) 'reentered) r))))`, 5*time.Second)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "reentered")
	logged, err := evalWithWatchdog(t, engine, `(reverse (vector-ref log 0))`, 5*time.Second)
	c.Assert(err, qt.IsNil)
	c.Assert(logged.SchemeString(), qt.Equals, "(before after before after)")
}

// TestEscapedTimerDoesNotHideALiveOne pins that an inner with-timeout left by an
// escape leaves the outer one to answer its own expiry. The escape ends the inner
// timer; when the outer one fires, a wait's cancellation is the outer timer's to
// claim: the outer handler runs, and no handler below it sees the failure. While
// an escape left the inner timer armed, the context's innermost timer was that
// stale one, and deciding by it claimed nothing, so the cancellation reached the
// with-exception-handler, which escaped, and the with-timeout returned as if no
// timer had fired; with no handler, the raw cancellation reached the embedder.
// That the handler then runs outside both timers is
// TestEscapedTimerLeavesTheHandlersContinuation's: a second application of it,
// under the fired outer timer, is interrupted before it can count here. The last
// row raises in place instead of returning its failure (FailureClaimedByTimer): t,
// a thread the outer timer kills, abandons m2, and a timed mutex-lock!, which does
// not watch the context, acquires it. The outer timer is 500 ms so that every row
// is at its wait when it fires.
func TestEscapedTimerDoesNotHideALiveOne(t *testing.T) {
	tcs := []struct {
		name string
		then string
	}{
		{"handler around the wait", `(call/cc (lambda (k) (with-exception-handler (lambda (e) (k 'caught)) (lambda () (mutex-lock! m)))))`},
		{"no handler", `(mutex-lock! m)`},
		{"abandoned mutex raised in place", `(mutex-lock! rm) (thread-start! t) (mutex-unlock! rm rcv)
          (guard (e (#t 'caught)) (mutex-lock! m2 5))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newEngine(t)
			defer func() {
				_ = engine.Close()
			}()
			result, err := evalWithWatchdog(t, engine, `
(define m (make-mutex))
(mutex-lock! m)
(define m2 (make-mutex))
(define rm (make-mutex))
(define rcv (make-condition-variable))
(define t (make-thread (lambda ()
  (mutex-lock! m2)
  (mutex-lock! rm) (condition-variable-signal! rcv) (mutex-unlock! rm)
  (mutex-lock! m))))
(define hits (vector 0))
(list (with-timeout 500 (lambda (k) (vector-set! hits 0 (+ 1 (vector-ref hits 0))) (thread-sleep! 0.01) 'timed-out)
        (lambda ()
          (guard (e (#t 'inner-caught)) (with-timeout 100000 (lambda (k) 'inner) (lambda () (raise 'x))))
          `+tc.then+`))
      (vector-ref hits 0))`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, "(timed-out 1)")
		})
	}
}

// TestEscapedTimerLeavesTheHandlersContinuation pins what the outer timer's
// handler receives when an inner with-timeout inside it was left by an escape: the
// same continuation as without one, so resuming it behaves the same in both shapes.
// Resuming k after a wait's interrupt is itself defective (filed in TODO.md), so
// the two shapes are compared rather than either asserted. While the escape left
// the inner timer armed, tearing down only that one applied the handler under the
// fired outer timer; the recheck after its first foreign call interrupted it and
// applied it again, with a k that captured that first application, and resuming it
// underflowed the eval stack (an internal error). The outer timer is 500 ms so that
// both shapes are at the wait when it fires.
func TestEscapedTimerLeavesTheHandlersContinuation(t *testing.T) {
	run := func(stale string) error {
		engine := newEngine(t)
		defer func() {
			_ = engine.Close()
		}()
		_, err := evalWithWatchdog(t, engine, `
(define m (make-mutex))
(mutex-lock! m)
(define ran (vector 0))
(with-timeout 500 (lambda (k) (vector-set! ran 0 (+ 1 (vector-ref ran 0))) (k 'resumed))
  (lambda () `+stale+` (list 'thunk (mutex-lock! m))))`, 5*time.Second)
		return err
	}
	c := qt.New(t)
	plain := run(`#f`)
	stale := run(`(guard (e (#t 'inner-caught)) (with-timeout 100000 (lambda (k) 'inner) (lambda () (raise 'x))))`)
	c.Assert(stale == nil, qt.Equals, plain == nil, qt.Commentf("plain: %v; stale: %v", plain, stale))
	c.Assert(errors.Is(stale, werr.ErrInternal), qt.Equals, errors.Is(plain, werr.ErrInternal),
		qt.Commentf("plain: %v; stale: %v", plain, stale))
}

// TestTimerClaimsAnApplySpreadFailureBelowIt pins the two failures apply raises
// while spreading its final argument onto the stack (OpUnpackListToStack): not a
// list, and an improper list. Raised inside eval after the timer fired, each is a
// failure under a fired timer above it, so it passes up to the owner like a
// primitive's, and the guard inside eval never sees it. Raised in place, the guard
// caught it.
func TestTimerClaimsAnApplySpreadFailureBelowIt(t *testing.T) {
	tcs := []struct {
		name  string
		apply string
	}{
		{"final argument not a list", `(apply + 1 2)`},
		{"final argument an improper list", `(apply + 1 '(2 . 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newSlowFailEngine(t, werr.WrapForeignErrorf(werr.ErrNotANumber, "unused"))
			result, err := evalWithWatchdog(t, engine, `
(define caught (vector #f))
(list (with-timeout 200 (lambda (k) 'timed-out)
        (lambda () (eval '(guard (e (#t (vector-set! caught 0 #t) 'caught)) (test-slow-ok) `+tc.apply+`)
                         (interaction-environment))))
      (vector-ref caught 0))`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, "(timed-out #f)")
		})
	}
}

// TestTimerClaimsAnSRFI18ConditionBelowIt pins the two places a wait primitive
// raises a condition in place instead of returning a failure: thread-join!'s
// joinee outcome and mutex-lock!'s abandoned mutex. A thread started in the timed
// thunk dies of the timer (its wait raises), abandoning a mutex it owned; code in
// eval, below the timer's owner, then joins it, or acquires that mutex, after a
// primitive that ignores the timer. Raised in place, the condition reached the
// guard inside eval (the caught flag). It is a failure after the timer fired, so
// the primitive hands it back to the bridge, which passes it up to the owner. The
// thread signals once it owns m2, and the thunk waits for that before eval runs,
// so the abandonment is certain.
func TestTimerClaimsAnSRFI18ConditionBelowIt(t *testing.T) {
	tcs := []struct {
		name string
		then string
	}{
		{"thread-join!", `(thread-join! t)`},
		{"mutex-lock! of an abandoned mutex", `(mutex-lock! m2)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newSlowFailEngine(t, werr.WrapForeignErrorf(werr.ErrNotANumber, "unused"))
			result, err := evalWithWatchdog(t, engine, `
(define m (make-mutex))
(define m2 (make-mutex))
(mutex-lock! m)
(define caught (vector #f))
(define after (vector #f))
(define rm (make-mutex))
(define rcv (make-condition-variable))
(define t (make-thread (lambda ()
  (mutex-lock! m2)
  (mutex-lock! rm) (condition-variable-signal! rcv) (mutex-unlock! rm)
  (mutex-lock! m))))
(list (with-timeout 200 (lambda (k) 'timed-out)
        (lambda () (mutex-lock! rm) (thread-start! t) (mutex-unlock! rm rcv)
                   (eval '(guard (e (#t (vector-set! caught 0 #t) 'caught))
                            (test-slow-ok) `+tc.then+` (vector-set! after 0 #t))
                         (interaction-environment))))
      (vector-ref caught 0) (vector-ref after 0))`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, "(timed-out #f #f)")
		})
	}
}

// TestTimerSupersedesFailureButNotDenial pins both sides of what a fired timer
// claims. A primitive that ignores its context outlives a short with-timeout and
// then fails. An ordinary failure belongs to the abandoned thunk and becomes the
// timeout. A security denial does not: it must reach the embedder however the
// program is written, or a with-timeout around a refused operation would turn the
// refusal into the handler's value.
func TestTimerSupersedesFailureButNotDenial(t *testing.T) {
	failures := []struct {
		name   string
		fail   error
		denial bool
	}{
		{"ordinary failure", werr.WrapForeignErrorf(werr.ErrNotANumber, "test-slow-fail: not a number"), false},
		{"security denial", werr.WrapForeignErrorf(security.ErrAccessDenied, "test-slow-fail: denied"), true},
	}
	for _, f := range failures {
		t.Run(f.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newSlowFailEngine(t, f.fail)
			result, err := evalWithWatchdog(t, engine,
				`(with-timeout 20 (lambda (k) 'timed-out) (lambda () (test-slow-fail)))`, 5*time.Second)
			if f.denial {
				c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue,
					qt.Commentf("a denial after the timer fired must reach the embedder, got %v / %v", result, err))
				return
			}
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, "timed-out")
		})
	}
}

// TestJoinReportsJoineeCancellationAsUncaught pins that a JOINEE dying of a
// cancelled wait is the joinee's outcome, not the joiner's cancellation. Join wraps
// every joinee error in UncaughtThreadException, whose Unwrap lets errors.Is see
// the ErrOperationCancelled inside; reading that as the joiner's own cancellation
// raised "thread-join!: cancelled before the wait completed" from a joiner nobody
// cancelled. The thread-sleep! row is the one that fails on HEAD for that reason:
// there a cancelled sleep already raised, while a cancelled mutex-lock! answered #f.
// The thread here is started inside a with-timeout thunk that returns normally,
// which cancels the thread's ctx (with plain context.Canceled) on the way out. The
// thunk returns only once the thread has signalled that it is at its wait, so the
// cancellation reaches the wait rather than the thread's first VM poll.
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
			c := qt.New(t)
			engine := newEngine(t)
			defer func() {
				_ = engine.Close()
			}()
			result, err := evalWithWatchdog(t, engine, `
(define m (make-mutex))
(mutex-lock! m)
(define rm (make-mutex))
(define rcv (make-condition-variable))
(define t (make-thread (lambda ()
  (mutex-lock! rm) (condition-variable-signal! rcv) (mutex-unlock! rm)
  `+w.wait+`)))
(define h (with-timeout 5000 (lambda (k) 'timed-out)
  (lambda () (mutex-lock! rm) (thread-start! t) (mutex-unlock! rm rcv) 'thunk-returned)))
(list h (guard (e ((uncaught-exception? e) (error-object-message (uncaught-exception-reason e)))
                  ((error-object? e) (list 'error-object (error-object-message e))))
          (thread-join! t)))`, 5*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals,
				`(thunk-returned "`+w.site+`: cancelled before the wait completed: context canceled")`)
		})
	}
}

// TestCancelledWaitInChildThreadIsCatchableThere pins the other side of the child
// rule: a thread started inside a timed thunk owns no timer and has none above it,
// so its cancelled wait is an ordinary condition IN THAT THREAD, and a guard there
// catches it. Only a timer on the waiting chain makes a cancellation uncatchable.
func TestCancelledWaitInChildThreadIsCatchableThere(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	defer func() {
		_ = engine.Close()
	}()
	result, err := evalWithWatchdog(t, engine, `
(define m (make-mutex))
(mutex-lock! m)
(define rm (make-mutex))
(define rcv (make-condition-variable))
(define t (make-thread (lambda ()
  (mutex-lock! rm) (condition-variable-signal! rcv) (mutex-unlock! rm)
  (guard (e (#t (list 'child-caught (error-object-message e))))
    (mutex-lock! m)))))
(define h (with-timeout 200 (lambda (k) 'timed-out)
  (lambda () (mutex-lock! rm) (thread-start! t) (mutex-unlock! rm rcv) (thread-sleep! 10))))
(list h (thread-join! t) (mutex-state m))`, 5*time.Second)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals,
		`(timed-out (child-caught "mutex-lock!: cancelled before the wait completed") not-owned)`)
}

// TestTimerCancelledWaitInAbortAfterThunkRunsHandler pins the abort route back to
// the timer's owner. A guard inside the timed thunk escapes by an abort, which runs
// the dynamic-wind after thunk on a winding sub-context; the wait there is cancelled
// by the timer, its failure passes up from the winding sub-context, and the driver
// resolving the abort hands it back to its loop instead of returning it. With the
// with-timeout at top level that driver is RunResumable; inside eval it is the eval
// sub-context's RunWithinBoundary, which owns the timer. The second shape lost the
// handler until RunWithinBoundary was given the same route.
func TestTimerCancelledWaitInAbortAfterThunkRunsHandler(t *testing.T) {
	owners := []struct {
		name string
		wrap func(body string) string
	}{
		{"top level", func(body string) string {
			return body
		}},
		{"inside eval", func(body string) string {
			return `(eval '` + body + ` (interaction-environment))`
		}},
	}
	for _, o := range owners {
		for _, w := range timerCancelledWaits {
			t.Run(o.name+"/"+w.name, func(t *testing.T) {
				c := qt.New(t)
				engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
				c.Assert(err, qt.IsNil)
				defer func() {
					_ = engine.Close()
				}()
				body := `(with-timeout 200 (lambda (k) 'timed-out)
  (lambda () (guard (e (#t 'inner-guard))
               (dynamic-wind (lambda () #f)
                             (lambda () (raise 'x))
                             (lambda () ` + w.wait + ` (vector-set! after 0 #t))))))`
				result, err := evalWithWatchdog(t, engine, waitPrelude+`
(define after (vector #f))
(list `+o.wrap(body)+` (vector-ref after 0))`, 5*time.Second)
				c.Assert(err, qt.IsNil)
				c.Assert(result.SchemeString(), qt.Equals, "(timed-out #f)")
			})
		}
	}
}

// TestOrdinaryErrorUnderArmedTimerIsCaught pins that the timer rule claims only a
// timer's cancellation. Under a with-timeout that has not fired, an ordinary error
// is caught by a guard in the timer's own context and by one inside eval below it;
// passing every failure up from a sub-context with a timer above it would take the
// second away.
func TestOrdinaryErrorUnderArmedTimerIsCaught(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer func() {
		_ = engine.Close()
	}()
	result, err := evalWithWatchdog(t, engine, `
(list (with-timeout 5000 (lambda (k) 'timed-out)
        (lambda () (guard (e (#t 'caught-here)) (car 1))))
      (with-timeout 5000 (lambda (k) 'timed-out)
        (lambda () (eval '(guard (e (#t 'caught-in-eval)) (car 1)) (interaction-environment)))))`, 5*time.Second)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "(caught-here caught-in-eval)")
}

// TestEmbedderDeadlineInsideArmedTimerRaises pins a real embedder deadline, not a
// cancel that names DeadlineExceeded as its cause, inside a with-timeout that has
// not fired. The timer's ctx then reports the same Err() as an expired timer
// (DeadlineExceeded) but a different cause, and only the cause belongs to the
// timer: the wait raises, carrying the embedder's deadline, and the handler does not
// run.
func TestEmbedderDeadlineInsideArmedTimerRaises(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	defer func() {
		_ = engine.Close()
	}()
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	type outcome struct {
		value wile.Value
		err   error
	}
	done := make(chan outcome, 1)
	go func() {
		v, err := engine.Eval(ctx, engine.MustParse(ctx, `(begin (define m (make-mutex)) (mutex-lock! m)
	  (with-timeout 5000 (lambda (k) 'timed-out) (lambda () (mutex-lock! m))))`))
		done <- outcome{v, err}
	}()
	var result wile.Value
	var err error
	select {
	case o := <-done:
		result, err = o.value, o.err
	case <-time.After(5 * time.Second):
		t.Fatal("the wait did not respect the embedder's deadline")
	}
	c.Assert(errors.Is(err, werr.ErrOperationCancelled), qt.IsTrue,
		qt.Commentf("an embedder deadline inside an armed with-timeout must raise, got %v / %v", result, err))
	c.Assert(errors.Is(err, context.DeadlineExceeded), qt.IsTrue)
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
// thread-sleep!. It proves the composition end to end, not which VM path does it:
// the owner-side rule turns the sleep's failure into the timer's interrupt, and in
// the timer's own context the eager recheck after the guard's own foreign calls
// would reach the handler too (TestTimerCancelledWaitIsNeverCaught pins the rule).
// Before defect 38's fix thread-sleep! returned the raw ctx.Err(): the sleep
// aborted the whole evaluation with "context deadline exceeded" and the handler
// never ran.
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

// TestEngineCloseTerminatesLiveThreads pins the closer registered by addThreads:
// closing an engine must terminate the SRFI-18 threads that engine started, not
// leave them spinning for the life of the host process.
//
// The thread-state assertion is the gate. It is valid immediately after Close
// returns because the closer calls Terminate (which records the terminal outcome
// synchronously) and only then waits on Thread.Done(). Without the closer the
// thread is still ThreadRunnable and its goroutine is still looping.
//
// The goroutine count is corroboration only, and it is asserted TIGHT
// (final <= baseline). The ±2 slack that
// TestConditionVariable_Wait_NoGoroutineLeak allows would make this arm VACUOUS
// here: exactly one goroutine leaks, so baseline+2 passes with the fix reverted.
func TestEngineCloseTerminatesLiveThreads(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extthreads.Extension),
	)
	c.Assert(err, qt.IsNil)

	baseline := testutil.StableGoroutineCount(t, 2*time.Second)

	result := eval(t, engine, `
		(define spinner
		  (make-thread (lambda () (let loop () (thread-sleep! 0.05) (loop)))))
		(thread-start! spinner)`)
	th, ok := result.Internal().(*values.Thread)
	c.Assert(ok, qt.IsTrue, qt.Commentf("thread-start! returned %T", result.Internal()))
	testutil.PollUntil(t, func() bool {
		return th.State() != values.ThreadNew
	}, 2*time.Second)

	err = engine.Close()
	c.Assert(err, qt.IsNil)

	c.Assert(th.State(), qt.Equals, values.ThreadTerminated,
		qt.Commentf("Engine.Close must terminate the threads the engine started"))

	final := testutil.StableGoroutineCount(t, 2*time.Second)
	c.Assert(final <= baseline, qt.IsTrue,
		qt.Commentf("thread goroutine outlived Engine.Close: baseline=%d final=%d", baseline, final))
}

// TestEngineCloseReapsOwnThreadsOnSharedRegistry is the cross-engine arm of the
// same gate, and the one that discriminates ownership from registration order.
//
// With a registry shared through WithRegistry, registry.Apply binds the FIRST
// registration of a name in every engine (pkg/registry/apply.go, the `bound`
// StringSet), so engine2 runs engine1's thread-start!. A tracker captured in
// that closure therefore collects engine2's threads under engine1 —
// engine2.Close() finds nothing (observed before the fix: State()=="runnable",
// still spinning) and engine1.Close() terminates a thread belonging to a
// still-running engine. Keying the tracker on the calling namespace
// (extensions/threads/close.go) is what makes the closing engine, rather than
// the registering one, the owner.
func TestEngineCloseReapsOwnThreadsOnSharedRegistry(t *testing.T) {
	c := qt.New(t)

	shared := registry.NewRegistry()
	c.Assert(core.AddToRegistry(shared), qt.IsNil)

	newShared := func() *wile.Engine {
		engine, err := wile.NewEngine(context.Background(),
			wile.WithRegistry(shared),
			wile.WithExtension(extthreads.Extension),
		)
		c.Assert(err, qt.IsNil)
		return engine
	}
	engine1 := newShared()
	engine2 := newShared()

	const spin = `
		(define spinner
		  (make-thread (lambda () (let loop () (thread-sleep! 0.05) (loop)))))
		(thread-start! spinner)`
	started := func(engine *wile.Engine) *values.Thread {
		result, err := engine.EvalMultiple(context.Background(), spin)
		c.Assert(err, qt.IsNil)
		th, ok := result.Internal().(*values.Thread)
		c.Assert(ok, qt.IsTrue, qt.Commentf("thread-start! returned %T", result.Internal()))
		testutil.PollUntil(t, func() bool {
			return th.State() != values.ThreadNew
		}, 2*time.Second)
		return th
	}
	th1 := started(engine1)
	th2 := started(engine2)

	// Close the SECOND engine first: its thread is the one a closure-captured
	// tracker would have filed under engine1.
	c.Assert(engine2.Close(), qt.IsNil)
	c.Assert(th2.State(), qt.Equals, values.ThreadTerminated,
		qt.Commentf("an engine must terminate the thread IT started, even on a shared registry"))
	c.Assert(th1.State(), qt.Not(qt.Equals), values.ThreadTerminated,
		qt.Commentf("closing engine2 must not reach into a still-running engine1"))

	c.Assert(engine1.Close(), qt.IsNil)
	c.Assert(th1.State(), qt.Equals, values.ThreadTerminated)
}
