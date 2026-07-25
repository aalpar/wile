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
	"fmt"
	"testing"
)

// renamedFoldReclaimSetup mirrors foldReclaimSetup but imports srfi-1 fold under
// a DIFFERENT surface name (my-fold). The measured call uses that alias.
func renamedFoldReclaimSetup(n int) string {
	return fmt.Sprintf(`(begin
(import (rename (only (srfi 1) fold) (fold my-fold)))
(define (cb x acc) acc)
(define (build i acc) (if (= i 0) acc (build (- i 1) (cons i acc))))
(define lst (build %d '())))`, n)
}

// TestInlineHOFRenamedFoldReclaims locks in that the dispatch-by-stamped-identity
// fix preserves the OPTIMIZATION, not just correctness: srfi-1 fold imported under
// a non-canonical surface name (my-fold) must still inline its reclaiming loop.
// A value-only test would stay green under total deoptimization (a real fold call
// returns the same value); only the allocation slope catches a silent regression
// where a renamed HOF stops inlining. Slope ~0 when reclaimed; ~2 frames/element
// if it fell through to the real fold. Mirrors TestInlineHOFFoldReclaims.
func TestInlineHOFRenamedFoldReclaims(t *testing.T) {
	a1 := allocsForRun(t, renamedFoldReclaimSetup(1000), "(my-fold cb 0 lst)")
	a2 := allocsForRun(t, renamedFoldReclaimSetup(2000), "(my-fold cb 0 lst)")
	slope := (a2 - a1) / 1000.0
	if slope > 0.5 {
		t.Errorf("renamed fold does not reclaim: %.3f allocs/element (a1=%.0f@1000, a2=%.0f@2000); "+
			"want ~0 — an import-renamed curated HOF must still inline via its stamped identity", slope, a1, a2)
	}
}
