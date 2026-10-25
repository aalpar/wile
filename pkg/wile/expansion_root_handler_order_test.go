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
	"testing"

	qt "github.com/frankban/quicktest"
)

// expansionRootTrail is the prelude of the handler-order rows: note records a
// step in trail, and the transformers m (raise) and mc (raise-continuable) run
// the raise inside a dynamic-wind whose after thunk notes 'after. note reaches
// the transformer as a datum in the form, (m note).
const expansionRootTrail = `(import (for-syntax (scheme base)))
  (define trail (vector '()))
  (define (note x) (vector-set! trail 0 (cons x (vector-ref trail 0))))
  (define-syntax m
    (lambda (stx)
      (let ((note (cadr (syntax->datum stx))))
        (dynamic-wind (lambda () #f) (lambda () (raise 'boom)) (lambda () (note 'after))))))
  (define-syntax mc
    (lambda (stx)
      (let ((note (cadr (syntax->datum stx))))
        (datum->syntax stx
          (dynamic-wind (lambda () #f) (lambda () (raise-continuable 'boom)) (lambda () (note 'after)))))))
  (define env (interaction-environment))`

// TestExpansionRootRaiseRunsTheCallersHandlerBeforeTheRootUnwinds: a raise out
// of an expansion-time root is handled in its dynamic extent (R7RS 6.11): the
// caller's handler runs while the root's dynamic-wind is live, and its after
// thunk runs when the handler escapes. A guard escapes first, so its clause runs
// after the after thunk. Before, the caller's handler ran once the root had
// ended, and the after thunk never ran. petite agrees on every row (eval of a
// define-syntax use); racket's call-with-exception-handler on the first two.
func TestExpansionRootRaiseRunsTheCallersHandlerBeforeTheRootUnwinds(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name, src, want string
	}{
		{"guard", `(guard (e (#t (note 'guard) e)) (eval (list 'm note) env))`,
			"(boom (after guard))"},
		{"handler escaping through call/cc",
			`(call/cc (lambda (k) (with-exception-handler (lambda (c) (note 'handler) (k c)) (lambda () (eval (list 'm note) env)))))`,
			"(boom (handler after))"},
		{"handler escaping through call-with-exit",
			`(call-with-exit (lambda (k) (with-exception-handler (lambda (c) (note 'handler) (k c)) (lambda () (eval (list 'm note) env)))))`,
			"(boom (handler after))"},
		// The handler runs once: the secondary is raised in the raise's dynamic
		// environment, to the handlers outside it, and the root's escape is not
		// handed to the caller's handlers a second time.
		{"handler returning from a raise",
			`(guard (e (#t (note 'outer) (error-object-message e)))
			   (with-exception-handler (lambda (c) (note 'handler) 42) (lambda () (eval (list 'm note) env))))`,
			`("exception handler returned from non-continuable exception" (handler after outer))`},
		// The root is live while the handler runs, so a raise-continuable resumes:
		// the transformer returns the handler's value.
		{"handler returning from a raise-continuable",
			`(with-exception-handler (lambda (c) (note 'handler) 42) (lambda () (eval (list 'mc note) env)))`,
			"(42 (handler after))"},
		{"guard around eval in a thread",
			`(import (wile threads))
			 (thread-join! (thread-start! (make-thread (lambda () (guard (e (#t (note 'guard) e)) (eval (list 'm note) env))))))`,
			"(boom (after guard))"},
		// The inner root, run by an eval in the outer transformer, sees the outer
		// root's own handler.
		{"guard in an enclosing transformer",
			`(import (for-syntax (scheme eval) (scheme repl)))
			 (define-syntax outer
			   (lambda (stx)
			     (let ((note (cadr (syntax->datum stx))))
			       (guard (e (#t (note 'guard) (datum->syntax stx (list 'quote e))))
			         (eval (list 'm note) (interaction-environment))))))
			 (eval (list 'outer note) env)`,
			"(boom (after guard))"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			v, err := newExpansionRootProbeEngine(t).EvalMultiple(context.Background(),
				expansionRootTrail+"\n(define result (begin "+tc.src+"))\n(list result (reverse (vector-ref trail 0)))")
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestLibraryBodyRaiseRunsTheImportersHandlerBeforeTheBodyUnwinds is the
// library-body twin: the body's raise is handled while its dynamic-wind is live.
// A later import does not run the body again and sees the same raise.
func TestLibraryBodyRaiseRunsTheImportersHandlerBeforeTheBodyUnwinds(t *testing.T) {
	t.Parallel()

	const body = `(dynamic-wind (lambda () #f) (lambda () (raise 'boom)) (lambda () ((vector-ref hook 0) 'after)))`
	const prelude = `(import (probe box))
	  (define trail (vector '()))
	  (vector-set! hook 0 (lambda (x) (vector-set! trail 0 (cons x (vector-ref trail 0)))))
	  (define (note x) ((vector-ref hook 0) x))`
	for _, tc := range []struct {
		name, src, want string
	}{
		{"guard around environment", `(guard (e (#t (note 'guard) e)) (environment '(probe user)))`,
			"(boom (after guard))"},
		{"guard around an eval of an import",
			`(guard (e (#t (note 'guard) e)) (eval '(begin (import (probe user)) used) (environment '(scheme base))))`,
			"(boom (after guard))"},
		{"handler escaping through call/cc",
			`(call/cc (lambda (k) (with-exception-handler (lambda (c) (note 'handler) (k c)) (lambda () (environment '(probe user))))))`,
			"(boom (handler after))"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			eng := newLibraryBodyProbeEngine(t, body)
			v, err := eng.EvalMultiple(context.Background(),
				prelude+"\n(define result (begin "+tc.src+"))\n(list result (reverse (vector-ref trail 0)))")
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, tc.want)

			v, err = eng.EvalMultiple(context.Background(),
				`(vector-set! trail 0 '()) (list (guard (e (#t (note 'guard) e)) (environment '(probe user))) (reverse (vector-ref trail 0)) (vector-ref cell 0))`)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, "(boom (guard) 1)")
		})
	}
}
