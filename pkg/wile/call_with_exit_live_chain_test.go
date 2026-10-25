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
	"strings"
	"testing"
	"testing/fstest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// newCallWithExitEngine is newHostCallEngine with esc.scm loadable: it calls the
// exit procedure stored in gc with 2.
func newCallWithExitEngine(t *testing.T) *wile.Engine {
	t.Helper()
	return newHostCallEngine(t,
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceFS(fstest.MapFS{"esc.scm": &fstest.MapFile{Data: []byte("((vector-ref gc 0) 2)")}}),
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
// expired-escape error, wherever it is called from. A thread cannot read the
// capturing thread's chain, so it is refused as before: cross-thread while the
// extent has not been left, expired once it has.
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
			name: "from another thread after the extent",
			src:  stale + `(let ((th (make-thread (lambda () (saved 99))))) (thread-start! th) (thread-join! th))`,
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

// TestCallWithExitPastEngineCallKeepsTheOldRule pins that the chain walk does
// not decide an escape from an Engine.Call callback: the call's root has no link
// to the host's caller, so a live outer extent is not expired there. The escape
// itself still fails, with "no prompt found", as before: Engine.Call does not
// pass an abort up (todo N29, "Engine.Call is still a default prompt for
// aborts"). This row pins the walk's answer, not that failure.
func TestCallWithExitPastEngineCallKeepsTheOldRule(t *testing.T) {
	t.Parallel()

	engine := newCallWithExitEngine(t)
	_, err := engine.EvalMultiple(context.Background(),
		`(call-with-exit (lambda (esc) (host-call (lambda (_) (esc 4))) 'no))`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrExpiredEscape), qt.IsFalse, qt.Commentf("got: %v", err))
}
