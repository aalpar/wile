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
	"os"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// redContinuationEnv gates the RED characterization below. The underlying fix is
// decision-gated (plan 2026-06-25-libraries-remediation-completion.local.md, B1
// C1): tightening the frame-reclaim classifier is a soundness-critical change
// (false positive = corruption), so it is not applied autonomously. The RED test
// is committed as an executable specification but skipped by default so CI stays
// green; run it with WILE_RUN_RED_CONTINUATION=1 to watch it fail.
const redContinuationEnv = "WILE_RUN_RED_CONTINUATION"

// reEntrantCounter is the canonical C1 shape: a procedure captures a
// continuation, then a later re-invocation resumes inside that procedure, which
// must still reach an enclosing binding it set!s. The %s is the binding form
// that introduces n, k*, and step.
const reEntrantCounterBody = `(step)
   (if (< n 3) (k* #f))
   n`

func evalCounter(t *testing.T, src string) (string, error) {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()
	result, evalErr := eng.EvalMultiple(ctx, src)
	if evalErr != nil {
		return "", evalErr
	}
	return result.SchemeString(), nil
}

// TestContinuationReentryInternalDefine_RED characterizes plan item B1/C1: a
// re-invoked continuation captured inside an INTERNAL define in a let body cannot
// reach the enclosing let frame on re-entry — it has been reclaimed — and the VM
// raises "no such local binding 0:1" instead of completing the loop. The desired
// behavior is that the counter runs to n == 3 (each (k* #f) re-enters step, which
// increments n in the enclosing frame). This is the only frame shape that fails;
// see TestContinuationReentryVariantsConverge for the working boundary.
func TestContinuationReentryInternalDefine_RED(t *testing.T) {
	if os.Getenv(redContinuationEnv) == "" {
		t.Skipf("RED characterization for the open C1 frame-reclaim re-entry bug; set %s=1 to run. Fix is decision-gated (plan B1).", redContinuationEnv)
	}
	// let body with an internal define for step.
	src := `(let ((n 0) (k* #f))
              (define (step)
                (call/cc (lambda (k) (set! k* k)))
                (set! n (+ n 1)))
              ` + reEntrantCounterBody + `)`
	got, err := evalCounter(t, src)
	qt.Assert(t, err, qt.IsNil) // currently FAILS: "no such local binding 0:1"
	qt.Assert(t, got, qt.Equals, "3")
}

// TestContinuationReentryVariantsConverge is the no-false-positive companion:
// the same re-entrant-counter semantics expressed WITHOUT an internal define all
// work today and must keep working. It pins the boundary of the C1 bug (internal
// define in a let body) so a future fix is not over-broad and a regression here
// is caught immediately.
func TestContinuationReentryVariantsConverge(t *testing.T) {
	tcs := []struct {
		name string
		src  string
	}{
		{
			name: "letrec",
			src: `(letrec ((n 0) (k* #f)
                          (step (lambda ()
                                  (call/cc (lambda (k) (set! k* k)))
                                  (set! n (+ n 1)))))
                   ` + reEntrantCounterBody + `)`,
		},
		{
			name: "nested let with lambda",
			src: `(let ((n 0) (k* #f))
                   (let ((step (lambda ()
                                 (call/cc (lambda (k) (set! k* k)))
                                 (set! n (+ n 1)))))
                     ` + reEntrantCounterBody + `))`,
		},
		{
			name: "direct let body, no procedure",
			src: `(let ((n 0) (k* #f))
                   (call/cc (lambda (k) (set! k* k)))
                   (set! n (+ n 1))
                   (if (< n 3) (k* #f))
                   n)`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := evalCounter(t, tc.src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got, qt.Equals, "3")
		})
	}
}
