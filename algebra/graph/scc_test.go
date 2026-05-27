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
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/algebra/graph"
	"github.com/aalpar/wile/werr"
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

// mustComputeSCC is a test helper that fails the test if ComputeSCC
// returns an error. The "happy-path" SCC tests use it so each test stays
// focused on the structural assertion rather than error plumbing.
func mustComputeSCC(c *qt.C, numNodes int, edges []graph.Edge) *graph.SCCResult {
	c.Helper()
	res, err := graph.ComputeSCC(numNodes, edges)
	c.Assert(err, qt.IsNil)
	c.Assert(res, qt.Not(qt.IsNil))
	return res
}

// mustCondenseSCC is the analogous helper for CondenseSCC.
func mustCondenseSCC(c *qt.C, numNodes int, edges []graph.Edge) (*graph.SCCResult, []graph.Edge) {
	c.Helper()
	scc, cond, err := graph.CondenseSCC(numNodes, edges)
	c.Assert(err, qt.IsNil)
	c.Assert(scc, qt.Not(qt.IsNil))
	return scc, cond
}

func TestComputeSCC_ErrorOnInvalidInput(t *testing.T) {
	c := qt.New(t)
	cases := []struct {
		name     string
		numNodes int
		edges    []graph.Edge
	}{
		{"numNodes zero", 0, nil},
		{"numNodes negative", -1, nil},
		{"edge V out of range", 2, []graph.Edge{{U: 0, V: 5}}},
		{"edge U negative", 2, []graph.Edge{{U: -1, V: 0}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			res, err := graph.ComputeSCC(tc.numNodes, tc.edges)
			c.Assert(res, qt.IsNil)
			c.Assert(errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue,
				qt.Commentf("want errors.Is(err, ErrInvalidArgument); got %v", err))
		})
	}
}

func TestComputeSCC_SingleNodeNoEdges(t *testing.T) {
	c := qt.New(t)
	// 1 node, no edges → 1 trivial SCC.
	res := mustComputeSCC(c,1, nil)
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
	res := mustComputeSCC(c,4, edges)
	c.Assert(res.NumSCCs, qt.Equals, 4)
	c.Assert(res.SCC, qt.DeepEquals, []int{0, 1, 2, 3})
	c.Assert(res.NonTrivial, qt.DeepEquals, []bool{false, false, false, false})
	assertReverseTopological(c, res, edges)
}

func TestComputeSCC_SingleCycle(t *testing.T) {
	c := qt.New(t)
	// 0 → 1 → 2 → 0 — all three nodes form one non-trivial SCC.
	edges := []graph.Edge{{U: 0, V: 1}, {U: 1, V: 2}, {U: 2, V: 0}}
	res := mustComputeSCC(c,3, edges)
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
	res := mustComputeSCC(c,4, edges)
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
	res := mustComputeSCC(c,1, edges)
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
	res := mustComputeSCC(c,4, edges)
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
	res := mustComputeSCC(c,5, edges)
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
	res := mustComputeSCC(c,4, edges)
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
	res := mustComputeSCC(c,4, edges)
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
	res := mustComputeSCC(c,4, edges)
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
	res := mustComputeSCC(c,5, nil)
	c.Assert(res.NumSCCs, qt.Equals, 5)
	for v := range 5 {
		c.Assert(res.NonTrivial[res.SCC[v]], qt.IsFalse,
			qt.Commentf("isolated node %d should be in a trivial SCC", v))
	}
}

// --- CondenseSCC ---

// condensedEdgeMultiset collects edges into a multiset keyed by (U, V) with
// a count of occurrences. Multi-edges are preserved.
func condensedEdgeMultiset(edges []graph.Edge) map[graph.Edge]int {
	m := make(map[graph.Edge]int)
	for _, e := range edges {
		m[e]++
	}
	return m
}

func TestCondenseSCC_ErrorOnInvalidInput(t *testing.T) {
	c := qt.New(t)
	scc, cond, err := graph.CondenseSCC(0, nil)
	c.Assert(scc, qt.IsNil)
	c.Assert(cond, qt.IsNil)
	c.Assert(errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue)

	scc, cond, err = graph.CondenseSCC(2, []graph.Edge{{U: 0, V: 5}})
	c.Assert(scc, qt.IsNil)
	c.Assert(cond, qt.IsNil)
	c.Assert(errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue)
}

func TestCondenseSCC_AcyclicPreservesEdges(t *testing.T) {
	c := qt.New(t)
	// Diamond DAG — every edge is inter-SCC, so condensation is just
	// the original edges renumbered through SCC[].
	original := []graph.Edge{
		{U: 0, V: 1}, {U: 0, V: 2},
		{U: 1, V: 3}, {U: 2, V: 3},
	}
	scc, cond := mustCondenseSCC(c,4, original)
	c.Assert(scc.NumSCCs, qt.Equals, 4)
	c.Assert(len(cond), qt.Equals, len(original),
		qt.Commentf("acyclic input: every original edge becomes one condensed edge"))

	// Each condensed edge should map back to its original under SCC[].
	want := condensedEdgeMultiset([]graph.Edge{
		{U: scc.SCC[0], V: scc.SCC[1]},
		{U: scc.SCC[0], V: scc.SCC[2]},
		{U: scc.SCC[1], V: scc.SCC[3]},
		{U: scc.SCC[2], V: scc.SCC[3]},
	})
	got := condensedEdgeMultiset(cond)
	c.Assert(got, qt.DeepEquals, want)
}

func TestCondenseSCC_SingleCycleEmpty(t *testing.T) {
	c := qt.New(t)
	// 0 → 1 → 2 → 0 — all edges within one SCC; condensation is empty.
	scc, cond := mustCondenseSCC(c,3, []graph.Edge{
		{U: 0, V: 1}, {U: 1, V: 2}, {U: 2, V: 0},
	})
	c.Assert(scc.NumSCCs, qt.Equals, 1)
	c.Assert(cond, qt.HasLen, 0)
}

func TestCondenseSCC_BowtieEmpty(t *testing.T) {
	c := qt.New(t)
	// Two cycles sharing node 2 — all five nodes are in one SCC; every
	// original edge is within-SCC and condensation is empty.
	scc, cond := mustCondenseSCC(c,5, []graph.Edge{
		{U: 0, V: 1}, {U: 1, V: 2}, {U: 2, V: 0},
		{U: 2, V: 3}, {U: 3, V: 4}, {U: 4, V: 2},
	})
	c.Assert(scc.NumSCCs, qt.Equals, 1)
	c.Assert(cond, qt.HasLen, 0)
}

func TestCondenseSCC_SelfLoopDroppedFromCondensation(t *testing.T) {
	c := qt.New(t)
	// 0 → 0 (self-loop). The single-node SCC is non-trivial, but the
	// edge from node 0 to itself stays within SCC[0] and is dropped.
	scc, cond := mustCondenseSCC(c,1, []graph.Edge{{U: 0, V: 0}})
	c.Assert(scc.NumSCCs, qt.Equals, 1)
	c.Assert(scc.NonTrivial[0], qt.IsTrue)
	c.Assert(cond, qt.HasLen, 0,
		qt.Commentf("within-SCC self-loop must not appear in the condensation"))
}

func TestCondenseSCC_MultiEdgesPreserved(t *testing.T) {
	c := qt.New(t)
	// Two parallel edges from node 0 to node 1, both inter-SCC.
	// The condensation must keep both — they represent two distinct
	// inter-SCC paths and the downstream count must reflect that.
	scc, cond := mustCondenseSCC(c,2, []graph.Edge{
		{U: 0, V: 1}, {U: 0, V: 1},
	})
	c.Assert(scc.NumSCCs, qt.Equals, 2)
	c.Assert(cond, qt.HasLen, 2,
		qt.Commentf("two original parallel edges must produce two condensed edges"))
	want := graph.Edge{U: scc.SCC[0], V: scc.SCC[1]}
	c.Assert(cond[0], qt.Equals, want)
	c.Assert(cond[1], qt.Equals, want)
}

func TestCondenseSCC_CycleWithTailDropsInternalEdges(t *testing.T) {
	c := qt.New(t)
	// Cycle {0,1,2} plus tail 0 → 3:
	//   intra-SCC edges (within {0,1,2}): (0,1), (1,2), (2,0) — dropped
	//   inter-SCC edge: (0, 3) — kept, becomes (SCC[0], SCC[3])
	original := []graph.Edge{
		{U: 0, V: 1}, {U: 1, V: 2}, {U: 2, V: 0},
		{U: 0, V: 3},
	}
	scc, cond := mustCondenseSCC(c,4, original)
	c.Assert(scc.NumSCCs, qt.Equals, 2)
	c.Assert(cond, qt.HasLen, 1,
		qt.Commentf("only the inter-SCC edge (0,3) survives condensation"))
	c.Assert(cond[0], qt.Equals, graph.Edge{U: scc.SCC[0], V: scc.SCC[3]})
}

func TestCondenseSCC_CondensedGraphIsAcyclic(t *testing.T) {
	c := qt.New(t)
	// On any input — including a graph with multiple cycles bridged by
	// inter-SCC edges — the condensed graph is guaranteed acyclic.
	// Verify by feeding it through CountPathsInDAG, which returns nil
	// on cycles. The kernel must succeed for an arbitrary SCC source.
	edges := []graph.Edge{
		{U: 0, V: 1}, {U: 1, V: 0}, // cycle X
		{U: 1, V: 2},               // bridge X → Y
		{U: 2, V: 3}, {U: 3, V: 2}, // cycle Y
		{U: 3, V: 4}, // bridge Y → singleton {4}
	}
	scc, cond := mustCondenseSCC(c,5, edges)
	c.Assert(scc.NumSCCs, qt.Equals, 3)
	// Run the DAG kernel on the condensed graph starting from the
	// source-most SCC. Non-nil result confirms acyclicity.
	counts := graph.CountPathsInDAG(scc.NumSCCs, cond, scc.SCC[0])
	c.Assert(counts, qt.Not(qt.IsNil),
		qt.Commentf("condensed graph must be acyclic"))
}
