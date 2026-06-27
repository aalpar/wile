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
// See pkg/machine/exception_raise.go (SnapshotReachableMarksInto / collectReachableMarks).

// An OUTER parameterize (above the call-with-values boundary) must be visible to a
// continuation captured inside the producer and re-invoked.
func TestMarks_OuterParamSurvivesCaptureAcrossProducer(t *testing.T) {
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

// Nested guards: an inner guard runs in the outer guard's call-with-values producer.
// An unmatched inner re-raise must escalate to the outer guard (its handler mark is
// above the producer boundary). Mirrors TestCoverageExceptionReRaise at the registry
// level as a permanent B-marks regression guard.
func TestMarks_NestedGuardReRaiseEscalates(t *testing.T) {
	got, err := testhelpers.RunSchemeCode(t, `
(guard (outer ((number? outer) (+ outer 100)))
  (guard (inner ((and (number? inner) (< inner 5)) (raise (+ inner 10))))
    (raise 1)))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got.SchemeString(), qt.Equals, "111")
}
