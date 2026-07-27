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

// Package graph provides Go-side fast paths for graph algorithms used by the
// (wile algebra graph) Scheme library.
//
// The kernels here are pure Go and operate on integer-indexed nodes. The
// Scheme library is responsible for translating between Scheme node
// identifiers (any equal?-comparable value) and integer indices.
package graph

import (
	"math/big"

	"github.com/aalpar/wile/pkg/werr"
)

// Edge represents a directed edge from U to V with unit weight (for path
// counting). Multiple edges between the same pair of nodes are allowed and
// each contributes a distinct path.
type Edge struct {
	U, V int
}

// CountPathsInDAG computes the number of distinct paths from `source` to each
// node in a directed acyclic graph. Returns a slice indexed by node, where
// counts[v] is the number of paths from source to v as a *big.Int. Nodes
// unreachable from source have count 0.
//
// Returns nil if the subgraph reachable from `source` contains a cycle.
// Cycles unreachable from `source` are ignored. nil is also returned for
// invalid input: numNodes <= 0, an out-of-range source, or any edge endpoint
// outside [0, numNodes). A nil result therefore does not by itself
// distinguish "cyclic" from "bad input".
//
// Algorithm: DFS from source produces a reverse-postorder, which is a valid
// topological order of the reachable subgraph. Cycle detection via gray-state
// tracking. Counts are then propagated in topological order with a single
// forward pass — each reachable edge relaxed exactly once. The per-edge step
// is the monotone in-place addition (`counts[v].Add(counts[v], counts[u])`),
// which is the kernel that future Σ-semiring DAG variants will inherit.
//
// Why topological order and not worklist Bellman-Ford: the counting semiring's
// + is not idempotent. A worklist that re-propagates a node's full current
// count when the node is re-popped over-counts (the node's count grows
// monotonically as predecessors settle, and each pop sends the full new value
// forward — adding to whatever was already sent). Topological-order
// processing visits each node exactly once after its count has settled.
func CountPathsInDAG(numNodes int, edges []Edge, source int) []*big.Int {
	if numNodes <= 0 || source < 0 || source >= numNodes {
		return nil
	}
	for _, e := range edges {
		if e.U < 0 || e.U >= numNodes || e.V < 0 || e.V >= numNodes {
			return nil
		}
	}

	// Build outgoing adjacency.
	outAdj := make([][]int, numNodes)
	for _, e := range edges {
		outAdj[e.U] = append(outAdj[e.U], e.V)
	}

	// DFS from source. White/gray/black tracking detects back-edges (cycles)
	// in the reachable subgraph. Reverse-postorder = topological order.
	const (
		white = 0
		gray  = 1
		black = 2
	)
	color := make([]int, numNodes)
	postorder := make([]int, 0, numNodes)
	cycle := false

	// Iterative DFS to avoid stack-depth issues on deep DAGs.
	type frame struct {
		node    int
		nextIdx int // index into outAdj[node] of next neighbor to visit
	}
	stack := []frame{{node: source, nextIdx: 0}}
	color[source] = gray

	for len(stack) > 0 && !cycle {
		top := &stack[len(stack)-1]
		if top.nextIdx >= len(outAdj[top.node]) {
			// All neighbors visited; mark black and emit in postorder.
			color[top.node] = black
			postorder = append(postorder, top.node)
			stack = stack[:len(stack)-1]
			continue
		}
		neighbor := outAdj[top.node][top.nextIdx]
		top.nextIdx++

		switch color[neighbor] {
		case white:
			color[neighbor] = gray
			stack = append(stack, frame{node: neighbor, nextIdx: 0})
		case gray:
			// Back-edge: cycle detected in the reachable subgraph.
			cycle = true
		case black:
			// Forward or cross-edge: already finalized, skip.
		}
	}

	if cycle {
		return nil
	}

	// Initialize counts. Source has count 1; everything else 0.
	counts := make([]*big.Int, numNodes)
	for i := range counts {
		counts[i] = new(big.Int)
	}
	counts[source].SetInt64(1)

	// Process in topological order (reverse postorder). For each node u with
	// non-zero count, propagate counts[u] to each successor's count via
	// in-place addition. Pattern 3A's monotone-add inner-loop step, applied
	// in the order that makes it correct.
	for i := len(postorder) - 1; i >= 0; i-- {
		u := postorder[i]
		if counts[u].Sign() == 0 {
			continue
		}
		for _, v := range outAdj[u] {
			counts[v].Add(counts[v], counts[u])
		}
	}

	return counts
}

// CyclicCountResult is the result of CountPathsCyclic. Unlike
// CountPathsInDAG which returns counts indexed by node, this returns
// counts indexed by SCC plus the SCC map (via the embedded *SCCResult)
// so callers can project back to per-node answers.
//
// The embedded *SCCResult gives `result.SCC`, `result.NumSCCs`, and
// `result.NonTrivial` directly (promoted fields). All three exported
// slices (SCC, NonTrivial, CountsBySCC) alias kernel-internal storage —
// callers MUST treat them as read-only. Mutation will silently corrupt
// any downstream computation that re-reads them.
type CyclicCountResult struct {
	*SCCResult

	// CountsBySCC[c] is the number of distinct paths in the condensed
	// DAG from SCC[source] to SCC c. Semantics by SCC kind:
	//
	//   Trivial SCC (NonTrivial[c] == false): single node, no within-SCC
	//     paths. CountsBySCC[c] is the exact number of paths from
	//     source to that node in the original graph.
	//
	//   Non-trivial SCC (NonTrivial[c] == true): contains a cycle.
	//     Within-SCC path counts are infinite, so CountsBySCC[c] is the
	//     "entry count" — the number of distinct paths from SCC[source]
	//     that reach this SCC via some entry point in the condensed
	//     DAG. Callers should propagate the NonTrivial flag so users
	//     understand the semantic shift.
	CountsBySCC []*big.Int
}

// CountPathsCyclic computes path counts on an arbitrary directed graph
// by SCC-condensing it and running the monotone kernel on the resulting
// DAG. Returns counts per SCC, not per node. For acyclic input this is
// equivalent to CountPathsInDAG with one extra SCC pass of overhead;
// callers that know their input is acyclic should prefer
// CountPathsInDAG directly.
//
// Returns ErrInvalidArgument if numNodes <= 0, source is out of range,
// or any edge references an out-of-range node.
func CountPathsCyclic(numNodes int, edges []Edge, source int) (*CyclicCountResult, error) {
	if numNodes <= 0 {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"CountPathsCyclic: numNodes must be > 0, got %d", numNodes)
	}
	if source < 0 || source >= numNodes {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"CountPathsCyclic: source=%d out of range [0, %d)", source, numNodes)
	}

	scc, condensed, err := CondenseSCC(numNodes, edges)
	if err != nil {
		return nil, err
	}

	// The condensed graph is acyclic by construction, and source is in
	// range by the validation above, so CountPathsInDAG cannot return nil
	// here. The defensive guard would only fire on a future kernel
	// invariant change.
	counts := CountPathsInDAG(scc.NumSCCs, condensed, scc.SCC[source])
	if counts == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"CountPathsCyclic: internal invariant violated — CountPathsInDAG returned nil on condensed DAG")
	}

	return &CyclicCountResult{
		SCCResult:   scc,
		CountsBySCC: counts,
	}, nil
}
