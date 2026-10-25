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
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// TestNonContinuableSecondaryAcrossContexts pins that a non-continuable raise's
// handler which takes a jump and then returns raises R7RS 6.11's secondary
// exception wherever the (k v) site runs: on a sub-context (eval, a dynamic-wind
// thunk inside eval), or on a macro transformer's expansion-time root, whose chain
// has no parent. The direct jump (the last row) is the control.
// Chez raises &non-continuable on the eval, dynamic-wind, after-thunk escape and
// direct rows; the composable and transformer rows have no Chez equivalent and
// are pinned by agreement with the direct jump.
func TestNonContinuableSecondaryAcrossContexts(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name string
		src  string
	}{
		{
			name: "call/cc continuation invoked through eval",
			src: `(define cell (vector #f))
			      (with-exception-handler
			        (lambda (e) (call/cc (lambda (k) (vector-set! cell 0 k) (eval '((vector-ref cell 0) 1) (interaction-environment)))) 42)
			        (lambda () (list 'a (error "boom") 'b)))`,
		},
		{
			name: "call/cc continuation invoked through eval in a dynamic-wind body",
			src: `(define cell (vector #f))
			      (with-exception-handler
			        (lambda (e)
			          (call/cc (lambda (k)
			            (vector-set! cell 0 k)
			            (dynamic-wind (lambda () #f) (lambda () (eval '((vector-ref cell 0) 1) (interaction-environment))) (lambda () #f))))
			          42)
			        (lambda () (list 'a (error "boom") 'b)))`,
		},
		{
			name: "call/cc continuation invoked through eval in a dynamic-wind after thunk",
			src: `(define cell (vector #f))
			      (with-exception-handler
			        (lambda (e)
			          (call/cc (lambda (k)
			            (vector-set! cell 0 k)
			            (dynamic-wind (lambda () #f) (lambda () #f) (lambda () (eval '((vector-ref cell 0) 1) (interaction-environment))))))
			          42)
			        (lambda () (list 'a (error "boom") 'b)))`,
		},
		{
			name: "composable continuation invoked through eval",
			src: `(define cell (vector #f))
			      (define tag (default-continuation-prompt-tag))
			      (call-with-continuation-prompt
			        (lambda ()
			          (with-exception-handler
			            (lambda (e) (call-with-composable-continuation (lambda (k) (vector-set! cell 0 k) (eval '((vector-ref cell 0) 1) (interaction-environment))) tag) 42)
			            (lambda () (list 'a (error "boom") 'b))))
			        tag
			        (lambda args 'handler))`,
		},
		{
			name: "call/cc continuation invoked from a macro transformer body",
			src: `(import (for-syntax (scheme base)))
			      (define-syntax m (lambda (stx) (let ((k (cadr (syntax->datum stx)))) (k 1)) #''ok))
			      (with-exception-handler
			        (lambda (e) (call/cc (lambda (k) (eval (list 'm k) (interaction-environment)))) 42)
			        (lambda () (list 'a (error "boom") 'b)))`,
		},
		{
			// The after thunk runs on a sub-context while the handler's escape is in
			// flight and re-enters the handler, which then returns.
			name: "call/cc continuation invoked from an after thunk the handler's escape runs",
			src: `(let ()
			        (define k #f)
			        (call/cc (lambda (esc)
			          (dynamic-wind
			            (lambda () #f)
			            (lambda ()
			              (with-exception-handler
			                (lambda (e) (if (call/cc (lambda (c) (set! k c) #t)) (esc 'out) 42))
			                (lambda () (list 'a (raise 'boom) 'b))))
			            (lambda () (if k (let ((kk k)) (set! k #f) (kk #f))))))))`,
		},
		{
			name: "control: call/cc continuation invoked directly",
			src: `(with-exception-handler
			        (lambda (e) (call/cc (lambda (k) (k 1))) 42)
			        (lambda () (list 'a (error "boom") 'b)))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			eng, err := wile.NewEngine(context.Background(),
				wile.WithProfile(wile.KitchenSink),
				wile.WithSourceFS(stdlib.FS),
				wile.WithLibraryPaths("."),
			)
			qt.Assert(t, err, qt.IsNil)
			v, err := eng.EvalMultiple(context.Background(), tc.src)
			if err == nil {
				t.Fatalf("handler return was forwarded as %s", v.SchemeString())
			}
			qt.Assert(t, strings.Contains(err.Error(), "non-continuable"), qt.IsTrue,
				qt.Commentf("expected the non-continuable secondary exception, got: %v", err))
		})
	}
}

// TestNonContinuableReentryAfterGuardExitRaises: a handler returning from a
// non-continuable raise raises the secondary in its own dynamic environment also
// when its continuation is re-entered after the enclosing guard has exited, or is
// reached by a guard miss's re-raise (decided 2026-10-25: match Chez). Before,
// each row forwarded the handler's value: (2 handler-returned), (outer 99).
//
// The re-entry rows use a call/cc guard (R7RS 7.3's shape, catch-all) because
// Wile's guard escapes with call-with-exit, whose one-shot exit procedure is
// expired when the re-entered handler's secondary reaches it.
// petite agrees on every row.
func TestNonContinuableReentryAfterGuardExitRaises(t *testing.T) {
	t.Parallel()

	// cell holds the handler's continuation, for eval to reach: the top level
	// is immutable under the Engine's default.
	const reentry = `
(define cell (vector #f))
(define (cc-guard on-catch thunk)
  ((call/cc
     (lambda (guard-k)
       (with-exception-handler
         (lambda (c) (guard-k (lambda () (on-catch c))))
         (lambda () (let ((v (thunk))) (lambda () v))))))))
(let ((n 0) (out '()))
  (let ((r (cc-guard (lambda (o) (list 'outer 'non-continuable))
             (lambda ()
               (with-exception-handler
                 (lambda (e) (call/cc (lambda (k) (vector-set! cell 0 k))) 'handler-returned)
                 (lambda () (raise 'boom)))))))
    (set! n (+ n 1))
    (set! out (cons (list n r) out))
    (if (= n 1) REENTER (reverse out))))`
	const caughtTwice = "((1 (outer non-continuable)) (2 (outer non-continuable)))"

	tcs := []struct {
		name string
		src  string
		want string // "" = the program raises the secondary exception
	}{
		{
			name: "handler re-entered directly after the guard exited",
			src:  strings.Replace(reentry, "REENTER", "((vector-ref cell 0) 7)", 1),
			want: caughtTwice,
		},
		{
			name: "handler re-entered through eval after the guard exited",
			src:  strings.Replace(reentry, "REENTER", "(eval '((vector-ref cell 0) 7) (interaction-environment))", 1),
			want: caughtTwice,
		},
		{
			name: "guard miss inside eval re-raises to an outer handler",
			src: `(with-exception-handler
			        (lambda (e) (list 'outer e))
			        (lambda () (eval '(guard (x ((string? x) 'str)) (raise 99)) (interaction-environment))))`,
		},
		{
			name: "guard miss inside eval re-raises to a handler inside eval",
			src: `(eval '(with-exception-handler
			               (lambda (e) (list 'outer e))
			               (lambda () (guard (x ((string? x) 'str)) (raise 99))))
			        (interaction-environment))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			eng, err := wile.NewEngine(context.Background(),
				wile.WithProfile(wile.KitchenSink),
				wile.WithSourceFS(stdlib.FS),
				wile.WithLibraryPaths("."),
			)
			qt.Assert(t, err, qt.IsNil)
			v, err := eng.EvalMultiple(context.Background(), tc.src)
			if tc.want != "" {
				qt.Assert(t, err, qt.IsNil)
				qt.Assert(t, v.SchemeString(), qt.Equals, tc.want)
				return
			}
			if err == nil {
				t.Fatalf("handler return was forwarded as %s", v.SchemeString())
			}
			qt.Assert(t, strings.Contains(err.Error(), "non-continuable"), qt.IsTrue,
				qt.Commentf("expected the non-continuable secondary exception, got: %v", err))
		})
	}
}
