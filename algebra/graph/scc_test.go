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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/algebra/graph"
)

// assertReverseTopological asserts that for every edge (u, v) where the
// endpoints are in different SCCs, SCC[u] < SCC[v]. This is the defining
// property of the reverse-topological numbering the kernel relies on.
func assertReverseTopological(c *qt.C, res *graph.SCCResult, edges []graph.Edge) {
	for _, e := range edges {
		su, sv := res.SCC[e.U], res.SCC[e.V]
		if su == sv {
			continue
		}
		c.Assert(su < sv, qt.IsTrue,
			qt.Commentf("edge %d→%d: SCC[u]=%d should be < SCC[v]=%d", e.U, e.V, su, sv))
	}
}

func TestComputeSCC_NilOnInvalidInput(t *testing.T) {
	c := qt.New(t)

	// Zero or negative numNodes.
	c.Assert(graph.ComputeSCC(0, nil), qt.IsNil)
	c.Assert(graph.ComputeSCC(-1, nil), qt.IsNil)

	// Out-of-range edge endpoints.
	c.Assert(graph.ComputeSCC(2, []graph.Edge{{U: 0, V: 5}}), qt.IsNil)
	c.Assert(graph.ComputeSCC(2, []graph.Edge{{U: -1, V: 0}}), qt.IsNil)
}

func TestComputeSCC_SingleNodeNoEdges(t *testing.T) {
	c := qt.New(t)
	// 1 node, no edges → 1 trivial SCC.
	res := graph.ComputeSCC(1, nil)
	c.Assert(res, qt.Not(qt.IsNil))
	c.Assert(res.NumSCCs, qt.Equals, 1)
	c.Assert(res.SCC, qt.DeepEquals, []int{0})
	c.Assert(res.NonTrivial, qt.DeepEquals, []bool{false})
}

func TestComputeSCC_LinearChain(t *testing.T) {
	c := qt.New(t)
	// 0 → 1 → 2 → 3 — a single deterministic path. Every node is its own
	// trivial SCC, and the reverse-topological numbering is fully forced
	// by the chain structure: SCC[i] = i.
	edges := []graph.Edge{{U: 0, V: 1}, {U: 1, V: 2}, {U: 2, V: 3}}
	res := graph.ComputeSCC(4, edges)
	c.Assert(res.NumSCCs, qt.Equals, 4)
	c.Assert(res.SCC, qt.DeepEquals, []int{0, 1, 2, 3})
	c.Assert(res.NonTrivial, qt.DeepEquals, []bool{false, false, false, false})
	assertReverseTopological(c, res, edges)
}

func TestComputeSCC_SingleCycle(t *testing.T) {
	c := qt.New(t)
	// 0 → 1 → 2 → 0 — all three nodes form one non-trivial SCC.
	edges := []graph.Edge{{U: 0, V: 1}, {U: 1, V: 2}, {U: 2, V: 0}}
	res := graph.ComputeSCC(3, edges)
	c.Assert(res.NumSCCs, qt.Equals, 1)
	c.Assert(res.SCC, qt.DeepEquals, []int{0, 0, 0})
	c.Assert(res.NonTrivial, qt.DeepEquals, []bool{true})
}

func TestComputeSCC_TwoDisconnectedCycles(t *testing.T) {
	c := qt.New(t)
	// 0↔1 and 2↔3 — two disconnected 2-cycles. Two non-trivial SCCs.
	// Without inter-SCC edges, the relative numbering of the two SCCs
	// depends on DFS order and is not asserted; only the partition is.
	edges := []graph.Edge{
		{U: 0, V: 1}, {U: 1, V: 0},
		{U: 2, V: 3}, {U: 3, V: 2},
	}
	res := graph.ComputeSCC(4, edges)
	c.Assert(res.NumSCCs, qt.Equals, 2)
	c.Assert(res.NonTrivial, qt.DeepEquals, []bool{true, true})
	c.Assert(res.SCC[0], qt.Equals, res.SCC[1],
		qt.Commentf("nodes 0 and 1 must share an SCC"))
	c.Assert(res.SCC[2], qt.Equals, res.SCC[3],
		qt.Commentf("nodes 2 and 3 must share an SCC"))
	c.Assert(res.SCC[0], qt.Not(qt.Equals), res.SCC[2],
		qt.Commentf("the two disconnected cycles must be different SCCs"))
}

func TestComputeSCC_SelfLoopSingleNode(t *testing.T) {
	c := qt.New(t)
	// 0 → 0 — single node with self-loop forms one non-trivial SCC of size 1.
	edges := []graph.Edge{{U: 0, V: 0}}
	res := graph.ComputeSCC(1, edges)
	c.Assert(res.NumSCCs, qt.Equals, 1)
	c.Assert(res.SCC, qt.DeepEquals, []int{0})
	c.Assert(res.NonTrivial, qt.DeepEquals, []bool{true},
		qt.Commentf("self-loop makes a single-node SCC non-trivial"))
}

func TestComputeSCC_DiamondDAG(t *testing.T) {
	c := qt.New(t)
	//     0
	//    / \
	//   1   2
	//    \ /
	//     3
	// Four trivial SCCs. Source SCC has ID 0; sink SCC has ID NumSCCs-1.
	edges := []graph.Edge{
		{U: 0, V: 1}, {U: 0, V: 2},
		{U: 1, V: 3}, {U: 2, V: 3},
	}
	res := graph.ComputeSCC(4, edges)
	c.Assert(res.NumSCCs, qt.Equals, 4)
	c.Assert(res.NonTrivial, qt.DeepEquals, []bool{false, false, false, false})
	c.Assert(res.SCC[0], qt.Equals, 0, qt.Commentf("node 0 is the unique source"))
	c.Assert(res.SCC[3], qt.Equals, 3, qt.Commentf("node 3 is the unique sink"))
	assertReverseTopological(c, res, edges)
}

func TestComputeSCC_Bowtie(t *testing.T) {
	c := qt.New(t)
	// Two 3-cycles sharing node 2:
	//   0 → 1 → 2 → 0   (cycle A)
	//   2 → 3 → 4 → 2   (cycle B)
	// All five nodes form one non-trivial SCC.
	edges := []graph.Edge{
		{U: 0, V: 1}, {U: 1, V: 2}, {U: 2, V: 0},
		{U: 2, V: 3}, {U: 3, V: 4}, {U: 4, V: 2},
	}
	res := graph.ComputeSCC(5, edges)
	c.Assert(res.NumSCCs, qt.Equals, 1)
	c.Assert(res.SCC, qt.DeepEquals, []int{0, 0, 0, 0, 0})
	c.Assert(res.NonTrivial, qt.DeepEquals, []bool{true})
}

func TestComputeSCC_CycleWithTail(t *testing.T) {
	c := qt.New(t)
	// 3-cycle {0, 1, 2} plus a tail 0 → 3.
	//   0 → 1 → 2 → 0
	//   0 → 3
	// Two SCCs: {0, 1, 2} (non-trivial), {3} (trivial). The cycle is the
	// source SCC (has the inter-SCC edge to 3); 3 is the sink.
	edges := []graph.Edge{
		{U: 0, V: 1}, {U: 1, V: 2}, {U: 2, V: 0},
		{U: 0, V: 3},
	}
	res := graph.ComputeSCC(4, edges)
	c.Assert(res.NumSCCs, qt.Equals, 2)
	// Cycle members share an SCC.
	c.Assert(res.SCC[0], qt.Equals, res.SCC[1])
	c.Assert(res.SCC[1], qt.Equals, res.SCC[2])
	c.Assert(res.SCC[3], qt.Not(qt.Equals), res.SCC[0])
	// The cycle SCC is non-trivial; the tail SCC is trivial.
	c.Assert(res.NonTrivial[res.SCC[0]], qt.IsTrue)
	c.Assert(res.NonTrivial[res.SCC[3]], qt.IsFalse)
	// Cycle SCC must have lower ID than tail SCC (it is the source).
	c.Assert(res.SCC[0] < res.SCC[3], qt.IsTrue)
	assertReverseTopological(c, res, edges)
}

func TestComputeSCC_DisconnectedComponents(t *testing.T) {
	c := qt.New(t)
	// Two disconnected DAGs: 0 → 1 and 2 → 3.
	edges := []graph.Edge{{U: 0, V: 1}, {U: 2, V: 3}}
	res := graph.ComputeSCC(4, edges)
	c.Assert(res.NumSCCs, qt.Equals, 4)
	c.Assert(res.NonTrivial, qt.DeepEquals, []bool{false, false, false, false})
	// Within-component ordering: source before sink.
	c.Assert(res.SCC[0] < res.SCC[1], qt.IsTrue)
	c.Assert(res.SCC[2] < res.SCC[3], qt.IsTrue)
	assertReverseTopological(c, res, edges)
}

func TestComputeSCC_NestedSCCs(t *testing.T) {
	c := qt.New(t)
	// Two separate cycles connected by a bridge:
	//   0 ↔ 1   (cycle X)
	//   1 → 2   (bridge)
	//   2 ↔ 3   (cycle Y)
	// SCCs: {0, 1} and {2, 3}, both non-trivial. X comes before Y in
	// reverse-topological order.
	edges := []graph.Edge{
		{U: 0, V: 1}, {U: 1, V: 0},
		{U: 1, V: 2},
		{U: 2, V: 3}, {U: 3, V: 2},
	}
	res := graph.ComputeSCC(4, edges)
	c.Assert(res.NumSCCs, qt.Equals, 2)
	c.Assert(res.NonTrivial, qt.DeepEquals, []bool{true, true})
	c.Assert(res.SCC[0], qt.Equals, res.SCC[1])
	c.Assert(res.SCC[2], qt.Equals, res.SCC[3])
	c.Assert(res.SCC[0] < res.SCC[2], qt.IsTrue,
		qt.Commentf("X is reachable to Y via the bridge, so X has the lower ID"))
	assertReverseTopological(c, res, edges)
}

func TestComputeSCC_EmptyEdgeListMultipleNodes(t *testing.T) {
	c := qt.New(t)
	// 5 isolated nodes, no edges → 5 trivial SCCs.
	res := graph.ComputeSCC(5, nil)
	c.Assert(res.NumSCCs, qt.Equals, 5)
	for v := range 5 {
		c.Assert(res.NonTrivial[res.SCC[v]], qt.IsFalse,
			qt.Commentf("isolated node %d should be in a trivial SCC", v))
	}
}
