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
	"math/big"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/algebra/graph"
)

// Helper: extract the per-node count as a decimal string for readable assertions.
func countStrings(counts []*big.Int) []string {
	out := make([]string, len(counts))
	for i, c := range counts {
		out[i] = c.String()
	}
	return out
}

func TestCountPathsInDAG_SingleNode(t *testing.T) {
	c := qt.New(t)
	// One node, no edges, source = 0.
	// Expected: d[0] = 1 (the source itself).
	counts := graph.CountPathsInDAG(1, nil, 0)
	c.Assert(countStrings(counts), qt.DeepEquals, []string{"1"})
}

func TestCountPathsInDAG_LinearChain(t *testing.T) {
	c := qt.New(t)
	// 0 → 1 → 2 → 3, source = 0.
	// Expected: each reachable node has exactly one path; d[0..3] = 1, 1, 1, 1.
	edges := []graph.Edge{{U: 0, V: 1}, {U: 1, V: 2}, {U: 2, V: 3}}
	counts := graph.CountPathsInDAG(4, edges, 0)
	c.Assert(countStrings(counts), qt.DeepEquals, []string{"1", "1", "1", "1"})
}

func TestCountPathsInDAG_DiamondDAG(t *testing.T) {
	c := qt.New(t)
	// Classic diamond:
	//        0
	//       / \
	//      1   2
	//       \ /
	//        3
	// Expected paths from 0: 0→1→3 and 0→2→3, so d[3] = 2.
	edges := []graph.Edge{
		{U: 0, V: 1}, {U: 0, V: 2},
		{U: 1, V: 3}, {U: 2, V: 3},
	}
	counts := graph.CountPathsInDAG(4, edges, 0)
	c.Assert(countStrings(counts), qt.DeepEquals, []string{"1", "1", "1", "2"})
}

func TestCountPathsInDAG_UnreachableNodes(t *testing.T) {
	c := qt.New(t)
	// 0 → 1, plus isolated 2 and 3.
	// Expected: d[0] = 1, d[1] = 1, d[2] = 0, d[3] = 0.
	edges := []graph.Edge{{U: 0, V: 1}}
	counts := graph.CountPathsInDAG(4, edges, 0)
	c.Assert(countStrings(counts), qt.DeepEquals, []string{"1", "1", "0", "0"})
}

func TestCountPathsInDAG_MotivatingCallGraph(t *testing.T) {
	c := qt.New(t)
	// The DAG from the plan's Example 1, mapped to integer indices:
	//   0 = main
	//   1 = init
	//   2 = run
	//   3 = setup
	//   4 = loop
	//
	// Edges:
	//   main → init    (0 → 1)
	//   main → run     (0 → 2)
	//   init → setup   (1 → 3)
	//   run  → setup   (2 → 3)
	//   run  → loop    (2 → 4)
	//   loop → setup   (4 → 3)
	//
	// Expected counts:
	//   main  = 1
	//   init  = 1   (only via main → init)
	//   run   = 1   (only via main → run)
	//   setup = 3   (main→init→setup, main→run→setup, main→run→loop→setup)
	//   loop  = 1   (only via main → run → loop)
	edges := []graph.Edge{
		{U: 0, V: 1}, // main → init
		{U: 0, V: 2}, // main → run
		{U: 1, V: 3}, // init → setup
		{U: 2, V: 3}, // run → setup
		{U: 2, V: 4}, // run → loop
		{U: 4, V: 3}, // loop → setup
	}
	counts := graph.CountPathsInDAG(5, edges, 0)
	c.Assert(countStrings(counts), qt.DeepEquals, []string{"1", "1", "1", "3", "1"})
}

func TestCountPathsInDAG_LargeCounts(t *testing.T) {
	c := qt.New(t)
	// Construct a "binary-tree-shaped" DAG layered so that the leaves accumulate
	// exponential path counts that exceed int64. Easiest: two parallel chains
	// converging on every layer doubles the count per layer.
	//
	//   0 ─┬→ 1 ─┬→ 3 ─┬→ 5 ─┬→ ... → 2k-1 ─┬→ 2k+1
	//      └→ 2 ─┘    │    │           │   │
	//                 └→ 4 ─┘ ... ─→ 2k    │
	//
	// Simpler: V-layer "wide diamond" — layer i has two nodes, each pointing
	// to both nodes in layer i+1. Path counts double per layer.
	//
	// Build a 32-layer wide-diamond. 2^32 = 4_294_967_296 paths to each
	// layer-32 node — well within int64 but already shows bignum growth.
	const layers = 32
	const numNodes = 2*layers + 1 // node 0 is source; nodes 1..2L are layered

	edges := make([]graph.Edge, 0, 4*layers)
	// source → layer-1 nodes
	edges = append(edges,
		graph.Edge{U: 0, V: 1},
		graph.Edge{U: 0, V: 2},
	)
	// Wire each layer i (nodes 2i-1 and 2i) to layer i+1 (nodes 2i+1 and 2i+2).
	for i := 1; i < layers; i++ {
		u1, u2 := 2*i-1, 2*i
		v1, v2 := 2*i+1, 2*i+2
		edges = append(edges,
			graph.Edge{U: u1, V: v1},
			graph.Edge{U: u1, V: v2},
			graph.Edge{U: u2, V: v1},
			graph.Edge{U: u2, V: v2},
		)
	}

	counts := graph.CountPathsInDAG(numNodes, edges, 0)

	// Source has count 1.
	c.Assert(counts[0].String(), qt.Equals, "1")

	// Layer i (i ≥ 1) nodes each have path count = 2^i (each of two predecessors
	// in layer i-1 contributes its own 2^(i-1)).
	for i := 1; i <= layers; i++ {
		want := new(big.Int).Lsh(big.NewInt(1), uint(i)-1)
		// Wait: layer 1 sees only the source (count 1) from each of its two
		// in-edges? No — both layer-1 nodes have one predecessor (the source,
		// count 1), so each is 1. Layer 2 nodes each see both layer-1 nodes
		// (1 + 1 = 2). Layer i+1 nodes each see both layer-i nodes (2 * layer-i).
		// So layer i count = 2^(i-1). Adjust:
		_ = want
		// Re-derive: layer 1 nodes = 1 each (one predecessor: source).
		// layer 2 nodes = 2 each (two predecessors: layer-1 nodes, count 1 each).
		// layer 3 nodes = 4 each (two predecessors: layer-2 nodes, count 2 each).
		// layer i nodes = 2^(i-1).
		expected := new(big.Int).Lsh(big.NewInt(1), uint(i-1))
		n1, n2 := 2*i-1, 2*i
		c.Assert(counts[n1].Cmp(expected), qt.Equals, 0,
			qt.Commentf("layer %d node %d: got %s, want %s", i, n1, counts[n1], expected))
		c.Assert(counts[n2].Cmp(expected), qt.Equals, 0,
			qt.Commentf("layer %d node %d: got %s, want %s", i, n2, counts[n2], expected))
	}

	// Spot-check: layer 32 should be 2^31 = 2_147_483_648.
	c.Assert(counts[2*layers].String(), qt.Equals, "2147483648")
}

func TestCountPathsInDAG_RejectsSelfLoop(t *testing.T) {
	c := qt.New(t)
	// 0 → 0 (self-loop). Cyclic input — kernel returns nil rather than
	// looping forever or producing nonsense.
	edges := []graph.Edge{{U: 0, V: 0}}
	counts := graph.CountPathsInDAG(1, edges, 0)
	c.Assert(counts, qt.IsNil)
}

func TestCountPathsInDAG_RejectsCycle(t *testing.T) {
	c := qt.New(t)
	// 0 → 1, 1 → 2, 2 → 0  (cycle back to source).
	edges := []graph.Edge{{U: 0, V: 1}, {U: 1, V: 2}, {U: 2, V: 0}}
	counts := graph.CountPathsInDAG(3, edges, 0)
	c.Assert(counts, qt.IsNil)
}

func TestCountPathsInDAG_RejectsCycleNotInvolvingSource(t *testing.T) {
	c := qt.New(t)
	// 0 → 1, 1 → 2, 2 → 1  (cycle between 1 and 2, reachable from 0).
	edges := []graph.Edge{{U: 0, V: 1}, {U: 1, V: 2}, {U: 2, V: 1}}
	counts := graph.CountPathsInDAG(3, edges, 0)
	c.Assert(counts, qt.IsNil)
}

func TestCountPathsInDAG_IgnoresUnreachableCycle(t *testing.T) {
	c := qt.New(t)
	// 0 → 1, plus cycle 2 ↔ 3 unreachable from 0. The reachable subgraph is
	// a DAG, so the kernel should succeed.
	edges := []graph.Edge{{U: 0, V: 1}, {U: 2, V: 3}, {U: 3, V: 2}}
	counts := graph.CountPathsInDAG(4, edges, 0)
	c.Assert(counts, qt.Not(qt.IsNil))
	c.Assert(countStrings(counts), qt.DeepEquals, []string{"1", "1", "0", "0"})
}
