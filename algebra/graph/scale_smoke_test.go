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

package graph_test

import (
	"testing"
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/algebra/graph"
)

// machineShapedGraph constructs a synthetic graph whose scale and shape
// approximate the wile/machine package call graph that produced the
// 3-hour incident documented in
// memory/feedback-counting-semiring-on-cycles.md:
//
//   - 539 nodes
//   - ~623 directed edges
//   - 12 back-edges that induce non-trivial strongly-connected components
//
// Shape: a forward backbone (node i → i+1) gives the DAG-like skeleton of
// most real call graphs; periodic shortcut edges add cross-call structure;
// a handful of well-spaced back-edges induce SCCs that mirror mutual
// recursion clusters. The specific node count, edge count, and back-edge
// count match the incident so the smoke test exercises the same scale
// the original counting query failed on.
func machineShapedGraph() (int, []graph.Edge) {
	const numNodes = 539
	const targetEdges = 623
	const backEdges = 12

	edges := make([]graph.Edge, 0, targetEdges)

	// Forward backbone: 538 edges (i → i+1).
	for i := range numNodes - 1 {
		edges = append(edges, graph.Edge{U: i, V: i + 1})
	}

	// Shortcut forward edges to bring the total to (targetEdges - backEdges).
	// Pattern: every Sth node forks to i+3, deterministic for reproducibility.
	want := targetEdges - backEdges
	stride := max(1, (numNodes-3)/(want-(numNodes-1)))
	for i := 0; i < numNodes-3 && len(edges) < want; i += stride {
		edges = append(edges, graph.Edge{U: i, V: i + 3})
	}

	// 12 back-edges, evenly spaced. Each jumps back 5 nodes, forming a
	// small SCC at that locale. Skip back-edges that would underflow.
	for k := 1; k <= backEdges; k++ {
		i := numNodes * k / (backEdges + 1)
		if i > 5 {
			edges = append(edges, graph.Edge{U: i, V: i - 5})
		}
	}

	return numNodes, edges
}

func TestCountPathsCyclic_MachineScaleSmoke(t *testing.T) {
	c := qt.New(t)
	numNodes, edges := machineShapedGraph()

	// Sanity: the synthetic graph matches the incident's scale and shape.
	c.Assert(numNodes, qt.Equals, 539)
	c.Assert(len(edges) > 600 && len(edges) <= 630, qt.IsTrue,
		qt.Commentf("expected ~623 edges, got %d", len(edges)))

	start := time.Now()
	res, err := graph.CountPathsCyclic(numNodes, edges, 0)
	elapsed := time.Since(start)

	// Correctness:
	c.Assert(err, qt.IsNil)
	c.Assert(res, qt.Not(qt.IsNil), qt.Commentf("kernel must not nil out on the incident shape"))
	c.Assert(len(res.SCC), qt.Equals, numNodes)
	c.Assert(len(res.CountsBySCC), qt.Equals, len(res.NonTrivial),
		qt.Commentf("per-SCC vectors must agree on NumSCCs"))

	// Source's SCC always has count 1 (it IS the start node).
	srcSCC := res.SCC[0]
	c.Assert(res.CountsBySCC[srcSCC].String(), qt.Equals, "1")

	// The back-edges should induce at least one non-trivial SCC.
	nontrivial := 0
	for _, nt := range res.NonTrivial {
		if nt {
			nontrivial++
		}
	}
	// Deterministic graph construction: 12 back-edges → 12 non-trivial
	// SCCs (one per back-edge, since each back-edge induces a small
	// cycle disjoint from the others by construction). Pinning the exact
	// count catches regressions that change SCC detection in ways a
	// loose `> 0` assertion would miss.
	c.Assert(nontrivial, qt.Equals, 12,
		qt.Commentf("expected exactly 12 non-trivial SCCs (one per back-edge); got %d", nontrivial))

	// Scale: the 3-hour incident must reduce to milliseconds. The acceptance
	// gate in plans/2026-05-26-scc-condensation.md asks for "under 1 second";
	// we assert under 250ms here so a regression that's still "fast" but
	// quadratic-ish would be caught.
	c.Assert(elapsed < 250*time.Millisecond, qt.IsTrue,
		qt.Commentf("expected < 250ms; got %v (3-hour incident baseline)", elapsed))

	t.Logf("scale-smoke: %d nodes, %d edges, %d non-trivial SCCs, completed in %v",
		numNodes, len(edges), nontrivial, elapsed)
}

func BenchmarkCountPathsCyclic_MachineScale(b *testing.B) {
	numNodes, edges := machineShapedGraph()
	for b.Loop() {
		_, _ = graph.CountPathsCyclic(numNodes, edges, 0)
	}
}
