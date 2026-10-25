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

package machine_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestResumedTimerThunkAnswersAsUninterrupted sweeps a with-timeout's firing
// point over the first instructions of its thunk (WithTimerFiringAfter) and pins
// that resuming the handler's continuation answers exactly what the thunk answers
// uninterrupted, at every point. Each loop's handler records that it ran, so a
// sweep that outran the thunk would fail rather than pass vacuously.
//
// Two fire points were wrong. At the poll, the resume replaced the value register
// (the operand the next instruction consumes) with the values k was applied to:
// none for (k), which the next push refused ("expected 1 value, got 0"). After a
// foreign call, the interrupt was taken on the call instruction, so a resume re-ran
// the call on its drained operands ("expected 2 arguments, got 0"); the resume
// must instead make the call's pending return.
func TestResumedTimerThunkAnswersAsUninterrupted(t *testing.T) {
	const sweep = 160
	tcs := []struct {
		name     string
		code     string
		expected values.Value
		// points, when non-zero, bounds the sweep to the points inside the thunk.
		points uint64
	}{
		{
			name: "set! loop resumed with no values",
			code: `(let ((n 0) (fired #f))
  (list (with-timeout 1000 (lambda (k) (set! fired #t) (k))
          (lambda () (let loop () (set! n (+ n 1)) (if (< n 100) (loop) (list 'done n)))))
        fired))`,
			expected: values.List(values.List(values.NewSymbol("done"), values.NewInteger(100)), values.TrueValue),
		},
		{
			name: "set! loop resumed with a value, which is discarded",
			code: `(let ((n 0) (fired #f))
  (list (with-timeout 1000 (lambda (k) (set! fired #t) (k 'stray))
          (lambda () (let loop () (set! n (+ n 1)) (if (< n 100) (loop) (list 'done n)))))
        fired))`,
			expected: values.List(values.List(values.NewSymbol("done"), values.NewInteger(100)), values.TrueValue),
		},
		{
			name: "parameterized loop",
			code: `(let ((p (make-parameter 'outer)))
  (with-timeout 1000 (lambda (k) (list (p) (k)))
    (lambda () (parameterize ((p 'inner)) (let loop ((i 0)) (if (< i 100) (loop (+ i 1)) (p)))))))`,
			expected: values.List(values.NewSymbol("outer"), values.NewSymbol("inner")),
		},
		{
			name: "foreign calls in the loop",
			code: `(let ((fired #f))
  (list (with-timeout 1000 (lambda (k) (set! fired #t) (k))
          (lambda () (let loop ((i 0) (s 0)) (if (< i 100) (loop (+ i 1) (+ s (string-length (number->string i)))) s))))
        fired))`,
			expected: values.List(values.NewInteger(190), values.TrueValue),
		},
		{
			name: "a foreign call in tail position to the thunk",
			code: `(let ((fired #f))
  (list (with-timeout 1000 (lambda (k) (set! fired #t) (k))
          (lambda () (let loop ((i 0)) (if (< i 100) (loop (+ i 1)) (string->symbol "end")))))
        fired))`,
			expected: values.List(values.NewSymbol("end"), values.TrueValue),
		},
		{
			// Only the first point is inside the thunk, which returns at once. A
			// later one lands between the boundary frame's restore and the
			// finalizer's teardown, which raises an internal error on master too
			// (filed separately).
			name:     "a primitive as the thunk",
			code:     `(with-timeout 1000 (lambda (k) (k)) list)`,
			expected: values.EmptyList,
			points:   1,
		},
		{
			name: "multiple values in the register",
			code: `(let ((fired #f))
  (list (with-timeout 1000 (lambda (k) (set! fired #t) (k))
          (lambda () (let loop ((i 0) (acc '())) (if (< i 50) (call-with-values (lambda () (values i 'v)) (lambda (a b) (loop (+ a 1) (cons b acc)))) (length acc)))))
        fired))`,
			expected: values.List(values.NewInteger(50), values.TrueValue),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			env := testhelpers.NewFullRuntimeEnv(t)
			points := uint64(sweep)
			if tc.points > 0 {
				points = tc.points
			}
			for k := range points {
				ctx := machine.WithTimerFiringAfter(context.Background(), k)
				result, err := testhelpers.RunSchemeCodeWithEnvAndContext(ctx, t, env, tc.code)
				qt.Assert(t, err, qt.IsNil, qt.Commentf("fired after %d instructions", k))
				qt.Assert(t, result, valuestest.SchemeEquals, tc.expected, qt.Commentf("fired after %d instructions", k))
			}
		})
	}
}
