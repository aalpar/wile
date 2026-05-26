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

import "math/big"

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
// Cycles unreachable from `source` are ignored.
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
