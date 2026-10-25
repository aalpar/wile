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
	"context"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestGraphCanonicalFormRefinementPrunesTheSearch is the deterministic witness
// that (wile algebra combinatorial-graph)'s 1-WL refinement step refines.
//
// A refinement step that returns its coloring unchanged leaves every answer
// correct: individualization alone still reaches a discrete partition, so the
// lex-smallest leaf is still the canonical form. What it changes is cost. On a
// cycle (vertex-transitive, so the first refinement cannot split it) refinement
// after one individualization leaves cells of size two, about 2n leaves; without
// it the search visits n! leaves. The Petersen canary in
// test/wile/algebra-combinatorial-graph-test.scm runs past 150 s under that
// mutant instead of failing, so the Scheme suite cannot name it.
//
// This test can, without a clock: it compares VM instruction counts
// (LastCounters().OpsExecuted, a pure function of the program) on C5 and C6.
// Measured 2026-10-25: 418224 and 694132 (ratio 1.66; C7/C6 1.78, C9/C8 1.54);
// under the identity-refinement mutant 997655 and 7343986 (ratio 7.36). The bound
// of 4 is a ratio, so it does not move when the library's constant costs do.
func TestGraphCanonicalFormRefinementPrunesTheSearch(t *testing.T) {
	t.Parallel()

	ops := func(c *qt.C, graph string) uint64 {
		eng := newSRFITestEngine(c)
		ctx := context.Background()
		// The import is its own run so that loading the library is not counted.
		_, err := eng.EvalMultiple(ctx, `(import (wile algebra combinatorial-graph))`)
		c.Assert(err, qt.IsNil)
		_, err = eng.EvalMultiple(ctx, `(graph-canonical-form `+graph+`)`)
		c.Assert(err, qt.IsNil)
		return eng.LastCounters().OpsExecuted
	}

	c := qt.New(t)
	c5 := ops(c, "(cycle-graph 5)")
	c6 := ops(c, "(cycle-graph 6)")
	c.Assert(c5 > 0, qt.IsTrue)
	ratio := float64(c6) / float64(c5)
	c.Assert(ratio < 4, qt.IsTrue, qt.Commentf(
		"C6 costs %d instructions, C5 %d (ratio %.2f): the search grows factorially, "+
			"so refinement is not splitting cells after individualization", c6, c5, ratio))
}
