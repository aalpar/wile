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

package graph

import "github.com/aalpar/wile/pkg/werr"

// Strongly-connected-component computation via Pearce's path-based algorithm
// (Pearce 2005, "An Improved Algorithm for Finding the Strongly Connected
// Components of a Directed Graph"). One DFS pass, O(V+E), single rindex
// field per node plus a small auxiliary path stack.
//
// Iterative DFS — mirrors the implementation in CountPathsInDAG so deep
// call graphs do not exhaust the Go stack.

// SCCResult describes the strongly-connected-component decomposition of a
// directed graph. Components are numbered 0..NumSCCs-1 in reverse
// topological order of the condensation: SCC 0 has no incoming inter-SCC
// edges (a "root" in the condensation); SCC NumSCCs-1 has no outgoing
// inter-SCC edges (a "leaf"). Equivalently, for every condensed edge
// (c, d) with c != d, c < d.
//
// The exported slices (SCC, NonTrivial) alias kernel-internal storage —
// callers MUST treat them as read-only. Mutation will silently corrupt
// any downstream computation that re-reads them (CondenseSCC,
// CountPathsCyclic).
type SCCResult struct {
	// SCC[v] is the component ID of node v. 0 <= SCC[v] < NumSCCs.
	SCC []int

	// NumSCCs is the number of distinct components.
	NumSCCs int

	// NonTrivial[c] is true iff component c contains a cycle: either
	// it has more than one node, or it is a single node with a
	// self-loop. A trivial SCC is a single node with no self-loop.
	NonTrivial []bool
}

// ComputeSCC computes the strongly-connected components of a directed
// graph using Pearce's path-based algorithm. Returns ErrInvalidArgument
// if numNodes <= 0 or any edge references an out-of-range node.
func ComputeSCC(numNodes int, edges []Edge) (*SCCResult, error) {
	if numNodes <= 0 {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"ComputeSCC: numNodes must be > 0, got %d", numNodes)
	}
	for i, e := range edges {
		if e.U < 0 || e.U >= numNodes {
			return nil, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"ComputeSCC: edge[%d].U=%d out of range [0, %d)", i, e.U, numNodes)
		}
		if e.V < 0 || e.V >= numNodes {
			return nil, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"ComputeSCC: edge[%d].V=%d out of range [0, %d)", i, e.V, numNodes)
		}
	}

	outAdj := make([][]int, numNodes)
	selfLoops := make([]bool, numNodes)
	for _, e := range edges {
		outAdj[e.U] = append(outAdj[e.U], e.V)
		if e.U == e.V {
			selfLoops[e.U] = true
		}
	}

	// Pearce state.
	//   rindex[v]   = 0 means unvisited; else DFS depth assigned at first visit.
	//   inComp[v]   = true once v's SCC has been finalized.
	//   root[v]     = true iff v is its own SCC root (initially true on visit;
	//                 cleared by relax steps that pull rindex[v] downward).
	//   pathStack   = Pearce's "S": non-root nodes awaiting their SCC root.
	//   component[v] = final SCC ID (Pearce-order; flipped after the DFS).
	rindex := make([]int, numNodes)
	inComp := make([]bool, numNodes)
	root := make([]bool, numNodes)
	component := make([]int, numNodes)
	var pathStack []int
	index := 1
	c := 0

	// Iterative DFS frame: node plus index of next outgoing edge to process.
	type frame struct {
		v       int
		edgeIdx int
	}
	var dfs []frame

	for v0 := range numNodes {
		if rindex[v0] != 0 {
			continue
		}

		// beginVisit(v0)
		rindex[v0] = index
		index++
		root[v0] = true
		dfs = append(dfs, frame{v: v0})

		for len(dfs) > 0 {
			top := &dfs[len(dfs)-1]
			v := top.v

			if top.edgeIdx < len(outAdj[v]) {
				w := outAdj[v][top.edgeIdx]
				top.edgeIdx++

				if rindex[w] == 0 {
					// Recurse: push child frame. The parent's
					// relax-against-w happens after the child finishes.
					rindex[w] = index
					index++
					root[w] = true
					dfs = append(dfs, frame{v: w})
				} else if !inComp[w] && rindex[w] < rindex[v] {
					// Already-visited successor still in the active path:
					// relax v's rindex toward w's. v can no longer be a root.
					rindex[v] = rindex[w]
					root[v] = false
				}
				continue
			}

			// All edges processed: finishVisit(v).
			if root[v] {
				index--
				for len(pathStack) > 0 && rindex[pathStack[len(pathStack)-1]] >= rindex[v] {
					w := pathStack[len(pathStack)-1]
					pathStack = pathStack[:len(pathStack)-1]
					component[w] = c
					inComp[w] = true
					index--
				}
				component[v] = c
				inComp[v] = true
				c++
			} else {
				pathStack = append(pathStack, v)
			}
			dfs = dfs[:len(dfs)-1]

			// Relax the parent against the just-finished child.
			// Mirrors the post-recursion check in Pearce's recursive form.
			if len(dfs) > 0 {
				parent := dfs[len(dfs)-1].v
				if !inComp[v] && rindex[v] < rindex[parent] {
					rindex[parent] = rindex[v]
					root[parent] = false
				}
			}
		}
	}

	numSCCs := c

	// Pearce assigns SCC IDs in finalization order (sinks/leaves first,
	// roots last). Flip so the result is in reverse-topological order
	// with sources at low IDs and leaves at high IDs.
	for i := range component {
		component[i] = numSCCs - 1 - component[i]
	}

	// Non-trivial flag: multi-node SCCs are always non-trivial; single-node
	// SCCs are non-trivial iff they have a self-loop.
	nonTrivial := make([]bool, numSCCs)
	sizeOfScc := make([]int, numSCCs)
	for v := range numNodes {
		sizeOfScc[component[v]]++
	}
	for sid, sz := range sizeOfScc {
		if sz > 1 {
			nonTrivial[sid] = true
		}
	}
	for v := range numNodes {
		if selfLoops[v] && sizeOfScc[component[v]] == 1 {
			nonTrivial[component[v]] = true
		}
	}

	return &SCCResult{
		SCC:        component,
		NumSCCs:    numSCCs,
		NonTrivial: nonTrivial,
	}, nil
}

// CondenseSCC reduces a directed graph to its DAG of strongly-connected
// components. Returns the SCC decomposition and the edge list of the
// condensed graph.
//
// For each original edge (u, v) where SCC[u] != SCC[v], emits the
// condensed edge (SCC[u], SCC[v]). Within-SCC edges (SCC[u] == SCC[v])
// are dropped — by construction the condensed graph is acyclic.
//
// Multi-edges in the condensation are preserved: if two distinct
// original edges (u1, v1) and (u2, v2) both satisfy SCC[u1]==SCC[u2]==c
// and SCC[v1]==SCC[v2]==d with c != d, both emit a condensed edge
// (c, d). They contribute two distinct inter-SCC paths and downstream
// path counting must reflect that.
//
// Returns (nil, nil, err) if ComputeSCC fails (input validation
// failure).
func CondenseSCC(numNodes int, edges []Edge) (*SCCResult, []Edge, error) {
	scc, err := ComputeSCC(numNodes, edges)
	if err != nil {
		return nil, nil, err
	}

	condensed := make([]Edge, 0, len(edges))
	for _, e := range edges {
		su, sv := scc.SCC[e.U], scc.SCC[e.V]
		if su == sv {
			continue
		}
		condensed = append(condensed, Edge{U: su, V: sv})
	}
	return scc, condensed, nil
}
