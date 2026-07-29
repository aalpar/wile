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

package wile

import (
	"testing"
)

// TestOrLoopEnvFrameAllocations is the dynamic half of TestOrEmitsNoEnvFrame
// (or_lowering_test.go, which is in the external test package and cannot reach
// the allocation harness here). Counting opcodes proves the frame is not
// emitted; this proves nothing allocates one in its place, on the shape that
// pays: an `or` inside a tail loop cost one env frame per operand per iteration,
// and the self call under it was left at depth 1 and never rewritten.
func TestOrLoopEnvFrameAllocations(t *testing.T) {
	const def = "(begin (define (or-loop i n) " +
		"(if (>= i n) i (or #f (or-loop (+ i 1) n))))\n)"

	const smallTrips = 10000
	const bigTrips = 30000
	small := allocsForRun(t, def, "(or-loop 0 10000)")
	big := allocsForRun(t, def, "(or-loop 0 30000)")

	slope := allocSlope(small, big, smallTrips, bigTrips)
	t.Logf("or-loop allocs: %d trips=%.0f, %d trips=%.0f, slope=%.3f frames/iter",
		smallTrips, small, bigTrips, big, slope)

	if slope > 0.1 {
		t.Errorf("or in a tail loop leaks env frames: %.3f frames/iter (want < 0.1); "+
			"%d→%.0f allocs, %d→%.0f allocs", slope, smallTrips, small, bigTrips, big)
	}
}
