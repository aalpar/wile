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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
)

// expansionRootTrail is the prelude of the handler-order rows: note records a
// step in trail, and the transformers m (raise), mc (raise-continuable, its value
// as syntax) and mb (raise-continuable, its value bare) run the raise inside a
// dynamic-wind whose before and after thunks note 'before and 'after, so a
// re-entry of the root shows as a second 'before. note reaches the transformer
// as a datum in the form, (m note).
const expansionRootTrail = `(import (for-syntax (scheme base)))
  (define trail (vector '()))
  (define (note x) (vector-set! trail 0 (cons x (vector-ref trail 0))))
  (define-syntax m
    (lambda (stx)
      (let ((note (cadr (syntax->datum stx))))
        (dynamic-wind (lambda () (note 'before)) (lambda () (raise 'boom)) (lambda () (note 'after))))))
  (define-syntax mc
    (lambda (stx)
      (let ((note (cadr (syntax->datum stx))))
        (datum->syntax stx
          (dynamic-wind (lambda () (note 'before)) (lambda () (raise-continuable 'boom)) (lambda () (note 'after)))))))
  (define-syntax mb
    (lambda (stx)
      (let ((note (cadr (syntax->datum stx))))
        (dynamic-wind (lambda () (note 'before)) (lambda () (raise-continuable 'boom)) (lambda () (note 'after))))))
  (define env (interaction-environment))`

// TestExpansionRootRaiseRunsTheCallersHandlerBeforeTheRootUnwinds: a raise out
// of an expansion-time root is handled in its dynamic extent (R7RS 6.11): the
// caller's handler runs while the root's dynamic-wind is live, and its after
// thunk runs when the handler escapes. A guard escapes first, so its clause runs
// after the after thunk. Before, the caller's handler ran once the root had
// ended, and the after thunk never ran. petite agrees on every row but the
// bare-datum one (eval of a define-syntax use); racket's
// call-with-exception-handler on the first two.
func TestExpansionRootRaiseRunsTheCallersHandlerBeforeTheRootUnwinds(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name, src, want string
	}{
		// petite: BEFORE AFTER GUARD boom. racket (with-handlers): the same.
		{"guard", `(guard (e (#t (note 'guard) e)) (eval (list 'm note) env))`,
			"(boom (before after guard))"},
		// petite: BEFORE HANDLER AFTER boom. racket (call-with-exception-handler):
		// the same.
		{"handler escaping through call/cc",
			`(call/cc (lambda (k) (with-exception-handler (lambda (c) (note 'handler) (k c)) (lambda () (eval (list 'm note) env)))))`,
			"(boom (before handler after))"},
		{"handler escaping through call-with-exit",
			`(call-with-exit (lambda (k) (with-exception-handler (lambda (c) (note 'handler) (k c)) (lambda () (eval (list 'm note) env)))))`,
			"(boom (before handler after))"},
		// The handler runs once: the secondary is raised in the raise's dynamic
		// environment, to the handlers outside it, and the root's escape is not
		// handed to the caller's handlers a second time. petite: BEFORE HANDLER
		// AFTER OUTER &non-continuable. racket (call-with-exception-handler) hands
		// the handler's value to the next handler instead (HANDLER AFTER OUTER 42),
		// its own rule for a handler that returns.
		{"handler returning from a raise",
			`(guard (e (#t (note 'outer) (error-object-message e)))
			   (with-exception-handler (lambda (c) (note 'handler) 42) (lambda () (eval (list 'm note) env))))`,
			`("exception handler returned from non-continuable exception" (before handler after outer))`},
		// The root is live while the handler runs, so a raise-continuable resumes:
		// the transformer returns the handler's value.
		{"handler returning from a raise-continuable",
			`(with-exception-handler (lambda (c) (note 'handler) 42) (lambda () (eval (list 'mc note) env)))`,
			"(42 (before handler after))"},
		// A transformer must return syntax in Wile, so the resumed root's bare 42 is
		// an error ("syntax transformer returned non-syntax value"), raised once the
		// root has ended: the handler runs a second time, on it, and returns from
		// that too. petite takes the datum as the expansion (HANDLER AFTER 42);
		// racket with srfi/34 runs the handler after the after thunk (AFTER HANDLER
		// 42).
		{"handler returning from a raise-continuable whose transformer then returns a datum",
			`(guard (e (#t (note 'outer) (error-object-message e)))
			   (with-exception-handler (lambda (c) (note 'handler) 42)
			     (lambda () (eval (list 'mb note) env))))`,
			`("exception handler returned from non-continuable exception" (before handler after handler outer))`},
		{"guard around eval in a thread",
			`(import (wile threads))
			 (thread-join! (thread-start! (make-thread (lambda () (guard (e (#t (note 'guard) e)) (eval (list 'm note) env))))))`,
			"(boom (before after guard))"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			v, err := newExpansionRootProbeEngine(t).EvalMultiple(context.Background(),
				expansionRootTrail+"\n(define result (begin "+tc.src+"))\n(list result (reverse (vector-ref trail 0)))")
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, tc.want)
		})
	}

	// The inner root, run by an eval in the outer transformer, sees the outer
	// root's own handler. The definitions are top level: the table's sources are
	// wrapped in an expression (begin), which cannot hold one (R7RS 5.3).
	t.Run("guard in an enclosing transformer", func(t *testing.T) {
		t.Parallel()
		v, err := newExpansionRootProbeEngine(t).EvalMultiple(context.Background(), expansionRootTrail+`
			(import (for-syntax (scheme eval) (scheme repl)))
			(define-syntax outer
			  (lambda (stx)
			    (let ((note (cadr (syntax->datum stx))))
			      (guard (e (#t (note 'guard) (datum->syntax stx (list 'quote e))))
			        (eval (list 'm note) (interaction-environment))))))
			(define result (eval (list 'outer note) env))
			(list result (reverse (vector-ref trail 0)))`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.SchemeString(), qt.Equals, "(boom (before after guard))")
	})
}

// TestGuardMissPastAnEndedExpansionRootReRaisesFromTheGuard: a guard whose
// clauses all miss re-raises with raise-continuable in the dynamic environment of
// the original raise (R7RS 4.2.7), by re-entering its handler's continuation.
// When the raise came out of an expansion-time root, that continuation is in the
// root, which ended when the guard escaped to its clauses; the guard re-raises
// from its own continuation instead, in the same handlers (those outside it), so
// the root's dynamic-wind is not entered again. Before, the re-entry ran the
// before thunk of a root that had ended and lost the enclosing handlers' exits.
// petite re-enters the root (a second before/after pair on every row); racket's
// with-handlers re-raises from its own continuation, as here. Full parity with
// petite waits on re-entering a call-with-exit past an expansion root (TODO.md,
// the call-with-exit live-chain residual).
func TestGuardMissPastAnEndedExpansionRootReRaisesFromTheGuard(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name, src, want string
	}{
		// petite: BEFORE AFTER BEFORE AFTER OUTER boom. racket (nested
		// with-handlers): BEFORE AFTER OUTER boom.
		{"an outer guard", `(guard (e (#t (note 'outer) e)) (guard (e ((string? e) 'no)) (eval (list 'm note) env)))`,
			"(boom (before after outer))"},
		// petite: BEFORE AFTER BEFORE OUTER AFTER boom. racket (with-handlers in
		// call-with-exception-handler): BEFORE AFTER OUTER boom.
		{"an outer handler escaping through call/cc",
			`(call/cc (lambda (k) (with-exception-handler (lambda (c) (note 'outer) (k c))
			   (lambda () (guard (e ((string? e) 'no)) (eval (list 'm note) env))))))`,
			"(boom (before after outer))"},
		// The root has ended, so the re-raise of its raise-continuable cannot resume
		// it, and an outer handler that returns is told so, as an uncaught
		// raise-continuable out of an ended root is. petite re-enters the root and
		// resumes it with the handler's value, then expands the form again and ends
		// non-continuable: BEFORE AFTER BEFORE HANDLER AFTER BEFORE HANDLER HANDLER
		// &non-continuable AFTER.
		{"an outer handler returning from a raise-continuable",
			`(guard (e (#t (note 'outer) (error-object-message e)))
			   (with-exception-handler (lambda (c) (note 'handler) 42)
			     (lambda () (guard (e ((string? e) 'no)) (eval (list 'mc note) env)))))`,
			`("exception handler returned from raise-continuable at :12:63, in a transformer or library body that has ended, so it cannot be resumed" (before after handler outer))`},
		// petite: BEFORE AFTER BEFORE HANDLER HANDLER AFTER OUTER &non-continuable.
		{"an outer handler returning from a raise",
			`(guard (e (#t (note 'outer) (error-object-message e)))
			   (with-exception-handler (lambda (c) (note 'handler) 42)
			     (lambda () (guard (e ((string? e) 'no)) (eval (list 'm note) env)))))`,
			`("exception handler returned from non-continuable exception" (before after handler outer))`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			v, err := newExpansionRootProbeEngine(t).EvalMultiple(context.Background(),
				expansionRootTrail+"\n(define result (begin "+tc.src+"))\n(list result (reverse (vector-ref trail 0)))")
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, tc.want)
		})
	}

	// With no handler outside the guard the re-raise is uncaught, and the body was
	// entered once. petite: BEFORE AFTER BEFORE, the uncaught boom, then AFTER as
	// the REPL resets.
	t.Run("no handler outside the guard", func(t *testing.T) {
		t.Parallel()
		eng := newExpansionRootProbeEngine(t)
		_, err := eng.EvalMultiple(context.Background(),
			expansionRootTrail+"\n(guard (e ((string? e) 'no)) (eval (list 'm note) env))")
		qt.Assert(t, err, qt.ErrorMatches, `(?s).*boom.*`)
		v, err := eng.EvalMultiple(context.Background(), "(reverse (vector-ref trail 0))")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.SchemeString(), qt.Equals, "(before after)")
	})
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
		// A guard whose clauses miss re-raises from its own continuation, not from
		// the body, which has ended
		// (TestGuardMissPastAnEndedExpansionRootReRaisesFromTheGuard).
		{"an outer guard around a guard that misses",
			`(guard (e (#t (note 'outer) e)) (guard (e ((string? e) 'no)) (environment '(probe user))))`,
			"(boom (after outer))"},
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

// TestExpansionRootRunsInTheCallersParameterization: an expansion-time root
// inherits the whole dynamic environment of the runtime call that started it,
// not its handler stack alone, so the caller's handler runs on the root with the
// caller's parameterization (R7RS 6.11: the dynamic environment of the raise),
// and so does the transformer. Before, every parameter but the handler stack
// read its default there. petite and racket agree on every row.
func TestExpansionRootRunsInTheCallersParameterization(t *testing.T) {
	t.Parallel()

	const prelude = `(import (for-syntax (scheme base) (scheme write)))
	  (define-syntax m (lambda (stx) (raise 'boom)))
	  (define-syntax shout (lambda (stx) (display "EXPANDING") #'1))
	  (define env (interaction-environment))
	  (define p (make-parameter 1))
	  (define port (open-output-string))`
	for _, tc := range []struct {
		name, src, want string
	}{
		{"a parameter the handler reads",
			`(parameterize ((p 2))
			   (call/cc (lambda (k) (with-exception-handler (lambda (e) (k (list 'p= (p)))) (lambda () (eval '(m) env))))))`,
			"(p= 2)"},
		{"the port the handler writes to",
			`(parameterize ((current-output-port port))
			   (call/cc (lambda (k) (with-exception-handler (lambda (e) (display "IN-HANDLER") (k 0)) (lambda () (eval '(m) env))))))
			 (get-output-string port)`,
			`"IN-HANDLER"`},
		{"the port the transformer writes to",
			`(parameterize ((current-output-port port)) (eval '(shout) env))
			 (get-output-string port)`,
			`"EXPANDING"`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			v, err := newExpansionRootProbeEngine(t).EvalMultiple(context.Background(), prelude+"\n"+tc.src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestAnExpansionInATransformerInheritsItsLiveHandlers: compile, expand and
// expand-once in a transformer start an expansion whose roots inherit the
// transformer's live handlers, its own guard first, not the handlers of the eval
// that runs the transformer. Before, compile inherited none, and the guard
// around eval took the raise after the root had ended (no after thunk); expand
// and expand-once, run inside an expansion, inherited that eval's caller's and
// skipped the transformer's guard.
func TestAnExpansionInATransformerInheritsItsLiveHandlers(t *testing.T) {
	t.Parallel()

	const prelude = `(import (for-syntax (scheme base) (wile eval)))
	  (define-syntax q (lambda (s) (raise 'inner)))
	  (define-syntax m
	    (lambda (s)
	      (datum->syntax s
	        (list 'quote
	          (guard (e (#t (list 'caught-in-transformer e)))
	            (%s)
	            'notcaught)))))`
	for _, tc := range []struct {
		name, call string
	}{
		{"compile", `compile '(let-syntax ((q (lambda (s) (raise 'inner)))) (q))`},
		{"expand", `expand (datum->syntax s '(q))`},
		{"expand-once", `expand-once (datum->syntax s '(q))`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			src := strings.Replace(prelude, "%s", tc.call, 1) +
				"\n(guard (e (#t (list 'outer e))) (eval '(m) (interaction-environment)))"
			v, err := newExpansionRootProbeEngine(t).EvalMultiple(context.Background(), src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, "(caught-in-transformer inner)")
		})
	}

	// compile at run time takes the order eval does: the guard's clause after
	// the root's after thunk.
	v, err := newExpansionRootProbeEngine(t).EvalMultiple(context.Background(),
		expansionRootTrail+"\n(define result (guard (e (#t (note 'guard) e)) (compile (list 'm note))))"+
			"\n(list result (reverse (vector-ref trail 0)))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "(boom (before after guard))")
}

// TestLibraryBodyResumedRaiseIsNotItsEnd: a library body's raise-continuable
// that the importer's handler resumes is over; when the body then leaves
// through a continuation, a later importer is told it exited that way, not
// given the resumed raise.
func TestLibraryBodyResumedRaiseIsNotItsEnd(t *testing.T) {
	t.Parallel()

	eng := newLibraryBodyProbeEngine(t, `(raise-continuable 'warn) ((vector-ref hook 0) 'escaped)`)
	v, err := eng.EvalMultiple(context.Background(), `(import (probe box))
	  (call/cc (lambda (k) (vector-set! hook 0 k)
	    (with-exception-handler (lambda (e) 42) (lambda () (environment '(probe user))))))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "escaped")
	_, err = eng.EvalMultiple(context.Background(), `(environment '(probe user))`)
	qt.Assert(t, errors.Is(err, werr.ErrLibraryInstantiationEscaped), qt.IsTrue, qt.Commentf("%v", err))
}
