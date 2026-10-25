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

package wile_test

import (
	"context"
	"errors"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"testing/fstest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// newCallWithExitEngine is newHostCallEngine with esc.scm loadable: it calls the
// exit procedure stored in gc with 2; dw.scm is d2Wind (TestCallWithExitBeforeThunkOfASubContextCapture).
func newCallWithExitEngine(t *testing.T) *wile.Engine {
	t.Helper()
	return newHostCallEngine(t,
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceFS(fstest.MapFS{
			"esc.scm": &fstest.MapFile{Data: []byte("((vector-ref gc 0) 2)")},
			"dw.scm":  &fstest.MapFile{Data: []byte(d2Wind)},
		}),
		wile.WithLibraryPaths("."),
	)
}

// TestCallWithExitValidWhileExtentLive pins that an exit procedure is valid
// exactly while its call-with-exit frame is on the chain its escape travels, as
// Racket's call/ec: a continuation re-entering the extent makes it valid again
// (it expired for good before 2026-10-25), and an after thunk its own escape runs
// may escape to it again. The controls are escapes from the sub-contexts and
// roots that chain passes through (eval, load, a transformer body at expansion
// time, a func-typed host callback, dynamic-wind thunks), green before the
// change. gc and cell are vectors because the top level is immutable.
func TestCallWithExitValidWhileExtentLive(t *testing.T) {
	t.Parallel()

	reentered := `(let ((k #f) (n 0) (log '()))
	  (let ((r (call-with-exit (lambda (esc)
	             (vector-set! gc 0 esc)
	             (call/cc (lambda (c) (set! k c)))
	             (set! n (+ n 1))
	             (if (= n 2) ESCAPE 'normal)))))
	    (set! log (cons r log))
	    (if (< n 2) (k #f) (reverse log))))`
	tcs := []struct {
		name string
		src  string
		want string
	}{
		{
			// The row's gate; racket call/ec: normal escaped.
			name: "re-entered extent escapes directly",
			src:  strings.Replace(reentered, "ESCAPE", "(esc 'escaped)", 1),
			want: "(normal escaped)",
		},
		{
			// racket (eval (list esc 9) ns): (normal 9).
			name: "re-entered extent escapes from eval",
			src:  strings.Replace(reentered, "ESCAPE", "(eval '((vector-ref gc 0) 9) (interaction-environment))", 1),
			want: "(normal 9)",
		},
		{
			name: "re-entered extent escapes from a func-typed host callback",
			src:  strings.Replace(reentered, "ESCAPE", "(host-call-func (lambda (_) (esc 9)))", 1),
			want: "(normal 9)",
		},
		{
			// The after thunk runs once; racket call/ec: (7 1). It was the
			// expired-escape error.
			name: "an after thunk the escape runs escapes to it again",
			src: `(let ((n 0))
			        (list (call-with-exit (lambda (e)
			                (dynamic-wind (lambda () #f)
			                              (lambda () (e 1))
			                              (lambda () (set! n (+ n 1)) (e 7)))))
			              n))`,
			want: "(7 1)",
		},
		{
			// racket call/ec: 2.
			name: "control: an after thunk an outer jump runs escapes to the frame it is leaving",
			src: `(call/cc (lambda (out)
			        (call-with-exit (lambda (e)
			          (dynamic-wind (lambda () #f) (lambda () (out 1)) (lambda () (e 2)))))))`,
			want: "2",
		},
		{
			// Validity needs the frame on the escaping chain, so the escape is
			// still upward-only and a barrier still lets it through.
			name: "re-entered extent escapes through a continuation barrier",
			src:  strings.Replace(reentered, "ESCAPE", "(call-with-continuation-barrier (lambda () (esc 'x)))", 1),
			want: "(normal x)",
		},
		{
			name: "control: escape from eval",
			src:  `(call-with-exit (lambda (esc) (vector-set! gc 0 esc) (eval '((vector-ref gc 0) 1) (interaction-environment)) 'no))`,
			want: "1",
		},
		{
			name: "control: escape from a loaded file",
			src:  `(call-with-exit (lambda (esc) (vector-set! gc 0 esc) (load "esc.scm") 'no))`,
			want: "2",
		},
		{
			// The transformer body runs on an expansion-time root, which has no
			// link to eval's context: its escape is passed up by error return.
			name: "control: escape from a transformer body at expansion time",
			src: `(import (for-syntax (scheme base)))
			      (define-syntax m (lambda (stx) (let ((k (cadr (syntax->datum stx)))) (k 3)) #''no))
			      (call-with-exit (lambda (esc) (eval (list 'm esc) (interaction-environment)) 'no))`,
			want: "3",
		},
		{
			name: "control: escape from a func-typed host callback",
			src:  `(call-with-exit (lambda (esc) (host-call-func (lambda (_) (esc 4))) 'no))`,
			want: "4",
		},
		{
			name: "control: escape from a dynamic-wind before thunk",
			src:  `(call-with-exit (lambda (e) (dynamic-wind (lambda () (e 5)) (lambda () 'no) (lambda () #f))))`,
			want: "5",
		},
		{
			name: "control: escape from an after thunk on normal exit",
			src:  `(call-with-exit (lambda (e) (dynamic-wind (lambda () #f) (lambda () 'no) (lambda () (e 6)))))`,
			want: "6",
		},
		{
			// It was an uncatchable "no prompt found for tag": the escape was not
			// marked expired when an outer jump left its extent. racket:
			// a catchable exn:fail.
			name: "an escape whose extent an outer jump left is caught as an error",
			src: `(let ((saved #f))
			        (call/cc (lambda (out) (call-with-exit (lambda (e) (set! saved e) (out 1)))))
			        (guard (x (#t (list 'caught (error-object? x)))) (saved 2)))`,
			want: "(caught #t)",
		},
		{
			// The resume is taken inside the extent, so the before thunk its
			// rewind runs continues it on both chains. racket call/ec: x.
			name: "control: a before thunk a re-entry inside the extent runs escapes to it",
			src: `(let ((k #f) (n 0))
			        (call-with-exit (lambda (e)
			          (dynamic-wind (lambda () (set! n (+ n 1)) (if (= n 2) (e 'x) #f))
			                        (lambda () (call/cc (lambda (c) (set! k c))))
			                        (lambda () #f))
			          (if (= n 1) (k #f) 'done))))`,
			want: "x",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			engine := newCallWithExitEngine(t)
			got, err := engine.EvalMultiple(context.Background(), "(define gc (vector #f))\n"+tc.src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestCallWithExitExpiredOffTheLiveChain pins the other half: an exit procedure
// whose frame is not on the chain its escape would travel raises the catchable
// expired-escape error, wherever it is called from. A dynamic-wind thunk a jump
// runs continues only the dynamic-wind call's continuation: an after thunk the
// chain being left, outside its dynamic-wind; a before thunk the chain being
// entered. The thunk rows escaped silently into an extent being left before
// (the jump was dropped); racket: "attempt to jump into an escape continuation"
// on each. A thread cannot read the capturing thread's chain, so another
// thread's call is refused as cross-thread, live extent or not.
func TestCallWithExitExpiredOffTheLiveChain(t *testing.T) {
	t.Parallel()

	const stale = `(call-with-exit (lambda (e) (vector-set! gc 0 e) 1))
	               (define (saved x) ((vector-ref gc 0) x))`
	tcs := []struct {
		name string
		src  string
		want error
	}{
		{
			name: "after a normal return",
			src:  stale + `(saved 2)`,
			want: werr.ErrExpiredEscape,
		},
		{
			name: "after its own escape",
			src:  `(call-with-exit (lambda (e) (vector-set! gc 0 e) (e 1))) ((vector-ref gc 0) 2)`,
			want: werr.ErrExpiredEscape,
		},
		{
			// It was "no prompt found for tag".
			name: "after an outer jump left the extent",
			src:  `(call/cc (lambda (out) (call-with-exit (lambda (e) (vector-set! gc 0 e) (out 1))))) ((vector-ref gc 0) 2)`,
			want: werr.ErrExpiredEscape,
		},
		{
			// racket call/ec: an exn:fail on the second escape.
			name: "after a re-entered extent was left again",
			src: `(let ((k #f) (n 0))
			        (call-with-exit (lambda (esc) (vector-set! gc 0 esc) (call/cc (lambda (c) (set! k c))) (set! n (+ n 1))))
			        (if (< n 2) (k #f) ((vector-ref gc 0) 2)))`,
			want: werr.ErrExpiredEscape,
		},
		{
			name: "from eval",
			src:  stale + `(eval '(saved 2) (interaction-environment))`,
			want: werr.ErrExpiredEscape,
		},
		{
			name: "from a func-typed host callback",
			src:  stale + `(host-call-func (lambda (_) (saved 2)))`,
			want: werr.ErrExpiredEscape,
		},
		{
			name: "from an Engine.Call callback",
			src:  stale + `(host-call (lambda (_) (saved 2)))`,
			want: werr.ErrExpiredEscape,
		},
		{
			name: "from another thread during the extent",
			src: `(call-with-exit (lambda (e)
			        (let ((th (make-thread (lambda () (e 99))))) (thread-start! th) (thread-join! th))))`,
			want: werr.ErrCrossThreadContinuation,
		},
		{
			// It was ErrExpiredEscape: another thread is refused as cross-thread.
			name: "from another thread after the extent",
			src:  stale + `(let ((th (make-thread (lambda () (saved 99))))) (thread-start! th) (thread-join! th))`,
			want: werr.ErrCrossThreadContinuation,
		},
		{
			// The inner extent is inside the dynamic-wind the outer escape leaves.
			// It escaped into the inner extent (master too).
			name: "an after thunk an outer escape runs, into an extent being left",
			src: `(let ()
			        (define saved #f)
			        (define n 0)
			        (call-with-exit (lambda (outer)
			          (dynamic-wind (lambda () #f)
			            (lambda () (call-with-exit (lambda (inner) (set! saved inner) (outer 1) 'x)) 'body-done)
			            (lambda () (set! n (+ n 1)) (if (= n 1) (saved 2) #f))))))`,
			want: werr.ErrExpiredEscape,
		},
		{
			// The same after re-entering the inner extent (master: expired).
			name: "an after thunk an outer escape runs, into a re-entered extent being left",
			src: `(let ((saved #f) (k #f) (pass 0) (fired 0))
			        (call-with-exit (lambda (outer)
			          (dynamic-wind (lambda () #f)
			            (lambda ()
			              (call-with-exit (lambda (inner)
			                (set! saved inner)
			                (call/cc (lambda (c) (set! k c)))
			                (set! pass (+ pass 1))
			                (if (= pass 2) (outer 'outer-escaped) 'first)))
			              (if (= pass 1) (k #f) 'body-done))
			            (lambda ()
			              (if (= pass 2) (begin (set! fired (+ fired 1)) (if (= fired 1) (saved 'into-inner) #f)) #f))))))`,
			want: werr.ErrExpiredEscape,
		},
		{
			// A resume leaves the exit's extent for a dynamic-wind outside it; the
			// rewind's before thunk escaped into the extent being left (master too).
			name: "a before thunk a resume runs, into the extent it leaves",
			src: `(let ()
			        (define k #f)
			        (define n 0)
			        (define saved #f)
			        (dynamic-wind
			          (lambda () (set! n (+ n 1)) (if (and (= n 2) saved) (saved 'from-before) #f))
			          (lambda () (call/cc (lambda (c) (set! k c))) 'dest)
			          (lambda () #f))
			        (if (= n 1) (call-with-exit (lambda (e) (set! saved e) (k #f) 'no)) #f))`,
			want: werr.ErrExpiredEscape,
		},
		{
			// The same with the extent re-entered first (it printed
			// (result escaped-into-left-frame); master: expired).
			name: "a before thunk a resume runs, into a re-entered extent it leaves",
			src: `(let ((cell (vector #f #f 0 #f)) (n 0))
			        (dynamic-wind
			          (lambda () (set! n (+ n 1)) (if (= n 2) ((vector-ref cell 0) 'escaped-into-left-frame) #f))
			          (lambda () (call/cc (lambda (k) (vector-set! cell 3 k))) 'body)
			          (lambda () #f))
			        (if (= n 1)
			          (begin
			            (call-with-exit (lambda (esc)
			              (vector-set! cell 0 esc)
			              (call/cc (lambda (c) (vector-set! cell 1 c)))
			              (vector-set! cell 2 (+ 1 (vector-ref cell 2)))
			              (if (= (vector-ref cell 2) 2) ((vector-ref cell 3) #f) 'first-normal)))
			            (if (= (vector-ref cell 2) 1) ((vector-ref cell 1) #f) #f))
			          #f))`,
			want: werr.ErrExpiredEscape,
		},
		{
			// RESIDUAL: racket escapes ((r escaped-from-before 2)). The before
			// thunk continues the re-entered extent, but the abort is routed on
			// the driver's chain, which does not hold it until the rewind ends;
			// the walk reports it expired rather than let the abort fail
			// uncatchably with "no prompt found".
			name: "a before thunk a re-entry runs, into the extent it re-enters",
			src: `(let ((k #f) (n 0) (log '()))
			        (let ((r (call-with-exit (lambda (e)
			                   (dynamic-wind
			                     (lambda () (set! n (+ n 1)) (if (= n 2) (e 'escaped-from-before) #f))
			                     (lambda () (call/cc (lambda (c) (set! k c))) 'body)
			                     (lambda () #f))))))
			          (set! log (cons r log))
			          (if (= n 1) (k #f) log)))`,
			want: werr.ErrExpiredEscape,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			engine := newCallWithExitEngine(t)
			_, err := engine.EvalMultiple(context.Background(), "(define gc (vector #f))\n"+tc.src)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, tc.want), qt.IsTrue, qt.Commentf("got: %v", err))
		})
	}
}

// TestCallWithExitPastAnUnlinkedRootKeepsTheOldRule pins that the chain walk
// does not decide an escape from code run past a root with no link to its
// caller: an Engine.Call callback, and a host primitive's nested Eval (the
// outermost run's root is not one). A live outer extent is not expired there;
// the escape still fails with "no prompt found", as before, because neither
// root passes an abort up (todo N29, "Engine.Call is still a default prompt for
// aborts", and "Engine.EvalMultiple called from a host is a prompt too"). The
// rows pin the walk's answer, not that failure.
func TestCallWithExitPastAnUnlinkedRootKeepsTheOldRule(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name string
		src  string
	}{
		{
			name: "Engine.Call",
			src:  `(call-with-exit (lambda (esc) (host-call (lambda (_) (esc 4))) 'no))`,
		},
		{
			name: "nested EvalMultiple",
			src:  `(call-with-exit (lambda (esc) (vector-set! gc 0 esc) (host-eval) 'no))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			engine := newCallWithExitEngine(t)
			err := engine.RegisterPrimitive(wile.PrimitiveSpec{
				Name:       "host-eval",
				ParamCount: 0,
				Impl: func(mc wile.CallContext) error {
					v, evalErr := engine.EvalMultiple(mc.Context(), "((vector-ref gc 0) 5)")
					if evalErr != nil {
						return evalErr
					}
					mc.SetValue(v.Internal())
					return nil
				},
			})
			qt.Assert(t, err, qt.IsNil)
			_, err = engine.EvalMultiple(context.Background(), "(define gc (vector #f))\n"+tc.src)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue, qt.Commentf("got: %v", err))
			qt.Assert(t, strings.Contains(err.Error(), "no prompt found"), qt.IsTrue, qt.Commentf("got: %v", err))
		})
	}
}

// d2Wind is a dynamic-wind whose before thunk, on its second run, calls the
// exit procedure in st; its body captures its continuation into st.
const d2Wind = `(dynamic-wind
  (lambda () (vector-set! st 2 (+ (vector-ref st 2) 1)) (if (= (vector-ref st 2) 2) ((vector-ref st 0) 'from-before) #f))
  (lambda () (call/cc (lambda (c) (vector-set! st 1 c) 'first)))
  (lambda () #f))`

// TestCallWithExitBeforeThunkOfASubContextCapture pins that a before thunk a
// resume runs still escapes to a live outer extent when the continuation was
// captured inside a sub-context (eval, load, call-with-output-file) and resumed
// at the root. Such a segment stops at the sub-context's boundary, so it is not
// the whole chain being entered and the walk keeps the driver's chain, which
// holds the exit frame. Green on master; the first version of the rewind rule
// reported it expired. racket and petite: from-before.
func TestCallWithExitBeforeThunkOfASubContextCapture(t *testing.T) {
	t.Parallel()

	out := filepath.Join(t.TempDir(), "out.txt")
	tcs := []struct {
		name    string
		capture string
	}{
		{name: "eval", capture: "(eval '" + d2Wind + " (interaction-environment))"},
		{name: "load", capture: `(load "dw.scm")`},
		{name: "call-with-output-file", capture: "(call-with-output-file " + strconv.Quote(out) + " (lambda (port) " + d2Wind + "))"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			engine := newCallWithExitEngine(t)
			src := `(define st (vector #f #f 0))
			        (call-with-exit (lambda (e)
			          (vector-set! st 0 e)
			          (let ((v CAPTURE))
			            (if (= (vector-ref st 2) 1) ((vector-ref st 1) 'again) (list 'v v)))))`
			got, err := engine.EvalMultiple(context.Background(), strings.Replace(src, "CAPTURE", tc.capture, 1))
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got.SchemeString(), qt.Equals, "from-before")
		})
	}
}

// TestCallWithExitNestedRootIsPerRun pins that a root is nested only when its own
// run descends from another run of the engine: while a SRFI-18 thread is inside a
// host primitive's nested EvalMultiple, the main goroutine's next top-level run
// is not nested, so an exit its guard left, called again, is the catchable
// expired error. An engine-wide count made it opaque, and the call failed with an
// uncatchable "no prompt found". The thread is held inside the nested run by a
// channel, not a timer.
func TestCallWithExitNestedRootIsPerRun(t *testing.T) {
	t.Parallel()

	ctx := context.Background()
	engine, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	entered := make(chan struct{})
	release := make(chan struct{})
	err = engine.RegisterPrimitive(wile.PrimitiveSpec{
		Name:       "wait-prim",
		ParamCount: 0,
		Impl: func(mc wile.CallContext) error {
			close(entered)
			<-release
			mc.SetValue(wile.NewInteger(1).Internal())
			return nil
		},
	})
	qt.Assert(t, err, qt.IsNil)
	err = engine.RegisterPrimitive(wile.PrimitiveSpec{
		Name:       "host-eval",
		ParamCount: 0,
		Impl: func(mc wile.CallContext) error {
			v, evalErr := engine.EvalMultiple(mc.Context(), "(wait-prim)")
			if evalErr != nil {
				return evalErr
			}
			mc.SetValue(v.Internal())
			return nil
		},
	})
	qt.Assert(t, err, qt.IsNil)
	_, err = engine.EvalMultiple(ctx, `(define s (vector #f)) (define th (make-thread (lambda () (host-eval)))) (thread-start! th)`)
	qt.Assert(t, err, qt.IsNil)
	<-entered
	defer close(release)
	got, err := engine.EvalMultiple(ctx, `(guard (e (#t #f)) (call-with-exit (lambda (k) (vector-set! s 0 k) (raise 'x))))
	  (guard (e (#t (list 'caught (error-object? e)))) ((vector-ref s 0) 1))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.SchemeString(), qt.Equals, "(caught #t)")
}
