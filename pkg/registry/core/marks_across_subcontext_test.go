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

package core_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
)

// Regression tests for "B-marks": continuation-mark-backed parameters (and the
// exception handler, which rides one) must survive a call/cc capture across a
// sub-context boundary (a call-with-values producer, etc.). Before B-marks the
// resumed continuation lost marks reachable only via parentMC, because
// applyCapturedContinuation sets isolatedMarks and cuts the parentMC walk.
// See pkg/machine/machine_context_apply.go (SnapshotReachableMarksInto / collectMarksOutsideSegment).

// An OUTER parameterize (above the call-with-values boundary) must be visible to a
// continuation captured inside the producer and re-invoked.
func TestMarks_OuterParamSurvivesCaptureAcrossProducer(t *testing.T) {
	t.Parallel()

	got, err := testhelpers.RunSchemeCode(t, `
(let ((p (make-parameter 'base)))
  (parameterize ((p 'outer))
    (call-with-values
      (lambda () ((call/cc (lambda (gk) (gk (lambda () (p)))))))
      (lambda (x) x))))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.SchemeString(), qt.Equals, "outer")
}

// NOTE: re-invoking a continuation captured inside a call-with-values PRODUCER to
// resume the producer (and thereby observe a producer-level parameterize on resume)
// is blocked by the separate value-facet truncation (claim 1 in the open-problem
// plan: the consumer + rest are dropped on re-invoke). That is independent of
// B-marks and is not retested here; B-marks concerns marks ABOVE the boundary, which
// the two tests around this note cover.

// Regression: a Go primitive error raised INSIDE a Scheme procedure (sq = (* x x)),
// caught by guard, in a begin context. The procedure-body frame can be detached from
// its namespace (nil parent, nil namespace) when reached through guard's
// call-with-values producer; RaiseInPlace dispatches the handler via NewSubContext at
// that error site, which must resolve the namespace via parentMC (mutableRuntimeFor /
// MutableRuntimeOrNil). Before that fix this nil-pointer-panicked.
func TestMarks_GoErrorInProcUnderGuard(t *testing.T) {
	t.Parallel()

	got, err := testhelpers.RunSchemeCode(t, `
(begin
  (define (sq x) (* x x))
  (guard (e (else 'caught)) (sq "hello")))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.SchemeString(), qt.Equals, "caught")
}

// Nested guards: an inner guard runs in the outer guard's call-with-values producer.
// An unmatched inner re-raise must escalate to the outer guard (its handler mark is
// above the producer boundary). Mirrors TestCoverageExceptionReRaise at the registry
// level as a permanent B-marks regression guard.
func TestMarks_NestedGuardReRaiseEscalates(t *testing.T) {
	t.Parallel()

	got, err := testhelpers.RunSchemeCode(t, `
(guard (outer ((number? outer) (+ outer 100)))
  (guard (inner ((and (number? inner) (< inner 5)) (raise (+ inner 10))))
    (raise 1)))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.SchemeString(), qt.Equals, "111")
}

// TestMarks_ResumedParameterizeEndsWithItsExtent pins that a call/cc
// continuation reinstated from outside its extent leaves a parameterize, or a
// with-exception-handler, around the capture once the resumed frames return.
// The capture-time snapshot a resume consults below its frames held the
// segment's own marks too, so they stayed in force for the rest of the run:
// the first row answered ((0 outer) (1 inner)). Petite and Racket agree with
// every row.
func TestMarks_ResumedParameterizeEndsWithItsExtent(t *testing.T) {
	t.Parallel()

	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			name: "call/cc in tail position of the parameterize body",
			code: `(let ((p (make-parameter 'outer)) (k #f) (n 0) (log '()))
			  (let ((v (parameterize ((p 'inner)) (call/cc (lambda (c) (set! k c) 0)))))
			    (set! log (cons (list v (p)) log))
			    (set! n (+ n 1))
			    (if (< n 2) (k 1) #f)
			    (reverse log)))`,
			want: "((0 outer) (1 outer))",
		},
		{
			name: "call/cc in a procedure's parameterize",
			code: `(let* ((p (make-parameter 'outer)) (k #f) (n 0)
			        (f (lambda () (parameterize ((p 'inner)) (call/cc (lambda (c) (set! k c) 0))))))
			  (let ((r (list (f) (p))))
			    (set! n (+ n 1))
			    (if (< n 2) (k 1) #f)
			    (list r (p))))`,
			want: "((1 outer) outer)",
		},
		{
			name: "non-tail call/cc still sees its parameterize while inside it",
			code: `(let ((p (make-parameter 'outer)) (k #f) (n 0) (log '()))
			  (parameterize ((p 'inner))
			    (let ((v (call/cc (lambda (c) (set! k c) 0))))
			      (set! log (cons (list v (p)) log))))
			  (set! log (cons (list 'after (p)) log))
			  (set! n (+ n 1))
			  (if (< n 2) (k 1) #f)
			  (reverse log))`,
			want: "((0 inner) (after outer) (1 inner) (after outer))",
		},
		{
			name: "captured inside eval, an outer parameterize still resolves",
			code: `(let* ((p (make-parameter 'outer)) (k #f) (n 0)
			        (save! (lambda (c) (set! k c)))
			        (bump! (lambda () (set! n (+ n 1)) n))
			        (resume! (lambda (v) (k v))))
			  (list (parameterize ((p 'pz))
			          (eval (list 'let (list (list 'v (list 'parameterize (list (list p ''in-eval))
			                                         (list 'call/cc (list 'lambda '(c) (list save! 'c) 0)))))
			                      (list 'if (list '< (list bump!) 2) (list resume! 1) (list 'list 'v (list p))))
			                (interaction-environment)))
			        (p)))`,
			want: "((1 pz) outer)",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got.SchemeString(), qt.Equals, tc.want)
		})
	}

	t.Run("a handler around the capture does not catch a later raise", func(t *testing.T) {
		t.Parallel()
		_, err := testhelpers.RunSchemeCode(t, `(let ((k #f) (n 0))
		  (with-exception-handler (lambda (e) 'inner-h) (lambda () (call/cc (lambda (c) (set! k c) 0))))
		  (set! n (+ n 1))
		  (if (< n 2) (k 1) (raise-continuable 'after-resume)))`)
		qt.Assert(t, err, qt.ErrorMatches, `(?s).*after-resume.*`)
	})
}
