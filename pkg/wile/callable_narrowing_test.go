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
	"path/filepath"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// narrowingEngine builds a KitchenSink engine — the file sites need the files
// extension, and the prompt sites need the delimited-continuation primitives.
func narrowingEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths())
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

// TestCallableNarrowingSitesAcceptEveryProcedure is Wave 4 item 11's behavioural
// gate (design §8.6.3 G3.1 / G3.2).
//
// THE DEFECT. `ParamTypes: TypeProcedure` means "implements values.Callable" —
// values.TypeProcedure's check is makeCheck[Callable]("procedure"). Eleven sites
// declared that and then asserted machine.Closure, which adds closureMarker() and
// has exactly two implementors against ApplyCallable's six. So a case-lambda, a
// parameter object and a continuation — all #t to Scheme's own `procedure?`, all
// applicable everywhere else, including via `apply`, `map` and
// `with-exception-handler` — were refused by call/cc, call-with-values,
// dynamic-wind, make-parameter, call-with-exit, the continuation barrier, the
// prompt family and call-with-{input,output}-file.
//
// THE THREE ARMS ARE NOT REDUNDANT. They are three distinct ApplyCallable
// dispatch cases (*CaseLambdaClosure, *Parameter, *CapturedContinuation /
// *ComposableContinuation). A fix that widened to "any NamedCallable" would pass
// the case-lambda arm and still fail the other two, so each site carries all
// three.
//
// OBSERVED AT 003b3353 (freshly built dist/darwin/arm64/wile): every row below
// failed with "<prim>: expected a procedure but got *machine.CaseLambdaClosure",
// "…*machine.Parameter", "…*machine.CapturedContinuation" or
// "…*machine.ComposableContinuation" — a diagnostic that names a procedure as
// not-a-procedure. The three dynamic-wind rows failed with the uncatchable
// "dynamic-wind: before/after must be a procedure", and the make-parameter rows
// with "make-parameter: converter must be a procedure".
func TestCallableNarrowingSitesAcceptEveryProcedure(t *testing.T) {
	dir := t.TempDir()
	outFile := filepath.Join(dir, "narrowing-out.txt")
	quoted := `"` + outFile + `"`

	tcs := []struct {
		name string // "<site> / <ApplyCallable arm>"
		src  string
		want string
	}{
		// --- site 1: prim_control.go, call/cc + call-with-current-continuation ---
		{
			name: "call/cc/case-lambda",
			src:  `(call/cc (case-lambda ((k) 42)))`,
			want: "42",
		},
		{
			name: "call/cc/parameter",
			src:  `(let ((p (make-parameter 7))) (call/cc p) (procedure? (p)))`,
			want: "#t",
		},
		{
			name: "call/cc/continuation",
			src:  `(procedure? (call/cc (lambda (k) (call/cc k))))`,
			want: "#t",
		},

		// --- site 2: prim_control.go, call-with-values PRODUCER ---
		{
			name: "call-with-values-producer/case-lambda",
			src:  `(call-with-values (case-lambda (() (values 1 2))) +)`,
			want: "3",
		},
		{
			name: "call-with-values-producer/parameter",
			src:  `(call-with-values (make-parameter 5) (lambda (x) x))`,
			want: "5",
		},
		{
			// The producer is applied with zero arguments, so resuming k delivers
			// zero values to the outer call/cc; the assertion is that control got
			// there at all.
			name: "call-with-values-producer/continuation",
			src:  `(begin (call/cc (lambda (k) (call-with-values k (lambda () 'ignored)))) 'producer-k-ok)`,
			want: "producer-k-ok",
		},

		// --- site 3: prim_control.go, call-with-values CONSUMER ---
		{
			name: "call-with-values-consumer/case-lambda",
			src:  `(call-with-values (lambda () (values 1 2)) (case-lambda ((a b) (+ a b))))`,
			want: "3",
		},
		{
			name: "call-with-values-consumer/parameter",
			src:  `(let ((p (make-parameter 0))) (call-with-values (lambda () 9) p) (p))`,
			want: "9",
		},
		{
			// G3.2, the sharpest R7RS case: §6.10 gives call-with-values a
			// procedure argument with no sub-kind, and passing a continuation as
			// the consumer is idiomatic.
			name: "call-with-values-consumer/continuation",
			src:  `(call/cc (lambda (k) (call-with-values (lambda () 1) k)))`,
			want: "1",
		},

		// --- site 4: prim_exit.go, call-with-exit ---
		{
			name: "call-with-exit/case-lambda",
			src:  `(call-with-exit (case-lambda ((k) 42)))`,
			want: "42",
		},
		{
			name: "call-with-exit/parameter",
			src:  `(let ((p (make-parameter 0))) (call-with-exit p) (procedure? (p)))`,
			want: "#t",
		},
		{
			name: "call-with-exit/continuation",
			src:  `(procedure? (call/cc (lambda (k) (call-with-exit k))))`,
			want: "#t",
		},

		// --- site 5: prim_barrier.go, call-with-continuation-barrier ---
		// The continuation arm is not here: a barrier refuses every continuation
		// that crosses it, so there is no accepting witness. It is asserted as an
		// error-KIND change instead — see
		// TestBarrierContinuationReachesTheBarrierNotTheNarrowing.
		{
			name: "call-with-continuation-barrier/case-lambda",
			src:  `(call-with-continuation-barrier (case-lambda (() 42)))`,
			want: "42",
		},
		{
			name: "call-with-continuation-barrier/parameter",
			src:  `(call-with-continuation-barrier (make-parameter 42))`,
			want: "42",
		},

		// --- site 6: prim_prompt.go, call-with-continuation-prompt THUNK ---
		{
			name: "prompt-thunk/case-lambda",
			src:  `(call-with-continuation-prompt (case-lambda (() 42)) (default-continuation-prompt-tag) #f)`,
			want: "42",
		},
		{
			name: "prompt-thunk/parameter",
			src:  `(call-with-continuation-prompt (make-parameter 42) (default-continuation-prompt-tag) #f)`,
			want: "42",
		},
		{
			name: "prompt-thunk/continuation",
			src:  `(begin (call/cc (lambda (k) (call-with-continuation-prompt k (default-continuation-prompt-tag) #f))) 'prompt-k-ok)`,
			want: "prompt-k-ok",
		},

		// --- site 7: prim_prompt.go, call-with-continuation-prompt HANDLER ---
		{
			name: "prompt-handler/case-lambda",
			src: `(call-with-continuation-prompt
			         (lambda () (abort-current-continuation (default-continuation-prompt-tag) 7))
			         (default-continuation-prompt-tag)
			         (case-lambda ((x) (* x 2))))`,
			want: "14",
		},
		{
			name: "prompt-handler/parameter",
			src: `(let ((p (make-parameter 0)))
			         (call-with-continuation-prompt
			           (lambda () (abort-current-continuation (default-continuation-prompt-tag) 7))
			           (default-continuation-prompt-tag)
			           p)
			         (p))`,
			want: "7",
		},
		{
			// A COMPOSABLE continuation, not a captured one: a full continuation
			// invoked from a handler escapes past the prompt that would resolve
			// its resume. The composable arm is the *ComposableContinuation
			// dispatch case, which no other row in this table covers.
			// (+ 1 (handler 7)) = 8, then the outer prompt yields 9 through the
			// composed frame.
			name: "prompt-handler/composable-continuation",
			src: `(call-with-continuation-prompt
			         (lambda ()
			           (+ 1 (call-with-composable-continuation
			                  (lambda (ck)
			                    (call-with-continuation-prompt
			                      (lambda () (abort-current-continuation (default-continuation-prompt-tag) 7))
			                      (default-continuation-prompt-tag)
			                      ck))
			                  (default-continuation-prompt-tag))))
			         (default-continuation-prompt-tag) #f)`,
			want: "9",
		},

		// --- site 8: prim_prompt.go, call-with-composable-continuation ---
		{
			name: "call-with-composable-continuation/case-lambda",
			src: `(call-with-continuation-prompt
			         (lambda () (call-with-composable-continuation (case-lambda ((k) 42))
			                                                       (default-continuation-prompt-tag)))
			         (default-continuation-prompt-tag) #f)`,
			want: "42",
		},
		{
			name: "call-with-composable-continuation/parameter",
			src: `(let ((p (make-parameter 0)))
			         (call-with-continuation-prompt
			           (lambda () (call-with-composable-continuation p (default-continuation-prompt-tag)))
			           (default-continuation-prompt-tag) #f)
			         (procedure? (p)))`,
			want: "#t",
		},
		{
			name: "call-with-composable-continuation/continuation",
			src: `(procedure?
			        (call/cc
			          (lambda (k)
			            (call-with-continuation-prompt
			              (lambda () (call-with-composable-continuation k (default-continuation-prompt-tag)))
			              (default-continuation-prompt-tag) #f))))`,
			want: "#t",
		},

		// --- site 9: prim_parameters.go, make-parameter CONVERTER ---
		{
			name: "make-parameter-converter/case-lambda",
			src:  `((make-parameter 10 (case-lambda ((v) (* v 2)))))`,
			want: "20",
		},
		{
			name: "make-parameter-converter/parameter",
			src:  `(let ((p (make-parameter 0))) (make-parameter 10 p) (p))`,
			want: "10",
		},
		{
			name: "make-parameter-converter/continuation",
			src:  `(call/cc (lambda (k) (make-parameter 10 k)))`,
			want: "10",
		},

		// --- site 10: extensions/files/prim_files.go, callWithFile ---
		// Serves call-with-input-file and call-with-output-file from one guard.
		{
			name: "call-with-file/case-lambda",
			src: `(begin (call-with-output-file ` + quoted + ` (case-lambda ((p) (display 'hi p))))
			            (call-with-input-file ` + quoted + ` (case-lambda ((p) (read p)))))`,
			want: "hi",
		},
		{
			name: "call-with-file/parameter",
			src:  `(let ((q (make-parameter 0))) (call-with-output-file ` + quoted + ` q) (port? (q)))`,
			want: "#t",
		},
		{
			name: "call-with-file/continuation",
			src:  `(port? (call/cc (lambda (k) (call-with-output-file ` + quoted + ` k))))`,
			want: "#t",
		},

		// --- site 11: pkg/machine/operations_winding.go, dynamic-wind ---
		// A compiled form, not a primitive, so there is no ParamTypes to
		// contradict — the same narrowing all the same.
		{
			name: "dynamic-wind-before-after/case-lambda",
			src: `(let ((log '()))
			         (dynamic-wind (case-lambda (() (set! log (cons 'in log))))
			                       (lambda () 'body)
			                       (case-lambda (() (set! log (cons 'out log)))))
			         (reverse log))`,
			want: "(in out)",
		},
		{
			name: "dynamic-wind-before-after/parameter",
			src:  `(dynamic-wind (make-parameter 1) (lambda () 'body) (make-parameter 2))`,
			want: "body",
		},
		{
			// `after` is applied on normal exit, so resuming k delivers zero
			// values to the outer call/cc. `before` needs no continuation row:
			// applying one there escapes before PushWind is ever reached, which
			// is why that check is now an internal invariant — see
			// TestDynamicWindBeforeCheckIsUnreachable.
			name: "dynamic-wind-after/continuation",
			src:  `(begin (call/cc (lambda (k) (dynamic-wind (lambda () 1) (lambda () 2) k))) 'wind-after-k-ok)`,
			want: "wind-after-k-ok",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng := narrowingEngine(t)
			v, err := eng.EvalMultiple(context.Background(), "(begin "+tc.src+")")
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestBarrierContinuationReachesTheBarrierNotTheNarrowing covers the one arm of
// site 5 with no accepting witness.
//
// A continuation barrier refuses every continuation that would cross it, so
// there is no continuation that call-with-continuation-barrier can accept AND
// run. The discriminating observation is therefore the error's KIND: at
// 003b3353 it was ErrNotAProcedure from the machine.Closure assertion ("expected
// a procedure but got *machine.CapturedContinuation") — the value never reached
// the barrier machinery. It must now be the barrier's own refusal, which is what
// R7RS-adjacent barrier semantics call for and what a widening to
// "any NamedCallable" would still not produce.
func TestBarrierContinuationReachesTheBarrierNotTheNarrowing(t *testing.T) {
	eng := narrowingEngine(t)
	_, err := eng.EvalMultiple(context.Background(),
		`(begin (call/cc (lambda (k) (call-with-continuation-barrier k))) 'unreachable)`)
	qt.Assert(t, err, qt.IsNotNil)
	msg := err.Error()
	qt.Assert(t, strings.Contains(msg, "continuation barrier"), qt.IsTrue,
		qt.Commentf("want the barrier's own refusal, got %q", msg))
	qt.Assert(t, strings.Contains(msg, "expected a procedure"), qt.IsFalse,
		qt.Commentf("the narrowing diagnostic is back: %q", msg))
}

// TestNarrowedSitesStillRejectNonProcedures is the other half of the widening:
// a value that is NOT a procedure must still be refused, and the diagnostic must
// name it in Scheme terms rather than by Go type. helpers.RequireCallable renders
// SchemeString, so "42" — not "*values.Integer".
func TestNarrowedSitesStillRejectNonProcedures(t *testing.T) {
	tcs := []struct {
		name string
		src  string
	}{
		{name: "call/cc", src: `(call/cc 42)`},
		{name: "call-with-values-producer", src: `(call-with-values 42 (lambda (x) x))`},
		{name: "call-with-values-consumer", src: `(call-with-values (lambda () 1) 42)`},
		{name: "call-with-exit", src: `(call-with-exit 42)`},
		{name: "call-with-continuation-barrier", src: `(call-with-continuation-barrier 42)`},
		{name: "prompt-thunk", src: `(call-with-continuation-prompt 42 (default-continuation-prompt-tag) #f)`},
		{name: "prompt-handler", src: `(call-with-continuation-prompt (lambda () 1) (default-continuation-prompt-tag) 42)`},
		{name: "make-parameter-converter", src: `(make-parameter 10 42)`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng := narrowingEngine(t)
			_, err := eng.EvalMultiple(context.Background(), "(begin "+tc.src+")")
			qt.Assert(t, err, qt.IsNotNil)
			msg := err.Error()
			qt.Assert(t, strings.Contains(msg, "expected a procedure but got 42"), qt.IsTrue,
				qt.Commentf("want a Scheme-terms diagnostic, got %q", msg))
		})
	}
}

// TestWithExceptionHandlerStillTakesEveryProcedure is the in-tree counter-example
// the widening was modelled on: with-exception-handler takes its handler as a
// values.Value and lets ApplyCallable decide, so it never had the defect. It
// answers 99 at 003b3353 and must keep answering 99.
func TestWithExceptionHandlerStillTakesEveryProcedure(t *testing.T) {
	eng := narrowingEngine(t)
	v, err := eng.EvalMultiple(context.Background(),
		`(begin (with-exception-handler (case-lambda ((e) 99)) (lambda () (raise-continuable 'boom))))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "99")
}
