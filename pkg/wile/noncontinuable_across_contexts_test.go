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

// TestNonContinuableSecondaryAcrossContexts pins that a jump taken inside a
// non-continuable raise's handler is judged a within-extent jump wherever the
// (k v) site runs: on a sub-context (eval, a dynamic-wind thunk inside eval),
// or on a macro transformer's expansion-time root, whose chain has no parent.
// Each reads the handler's finalizer frame as absent from its own chain; the
// frame is on the handler context's chain, which the sub-context's parentMC
// reaches and which the first driver above the transformer's root holds. The
// handler then returns, so R7RS 6.11's secondary exception must fire, as it
// does for the same jump taken directly (the last row, a control).
// Chez raises &non-continuable on every row.
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

// TestNonContinuableForwardAcrossContexts is the other side of the rows above:
// a guard whose clauses miss re-enters its handler's extent from outside, which
// stays a revival (the handler's value is forwarded) when the raise, and the
// re-raise's resume, run inside eval. Walking the parent contexts must not find
// the finalizer frame the guard's exit discarded.
func TestNonContinuableForwardAcrossContexts(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name string
		src  string
		want string
	}{
		{
			name: "guard miss inside eval re-raises to an outer handler",
			src: `(with-exception-handler
			        (lambda (e) (list 'outer e))
			        (lambda () (eval '(guard (x ((string? x) 'str)) (raise 99)) (interaction-environment))))`,
			want: "(outer 99)",
		},
		{
			name: "guard miss inside eval re-raises to a handler inside eval",
			src: `(eval '(with-exception-handler
			               (lambda (e) (list 'outer e))
			               (lambda () (guard (x ((string? x) 'str)) (raise 99))))
			        (interaction-environment))`,
			want: "(outer 99)",
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
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, tc.want)
		})
	}
}
