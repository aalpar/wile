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

package algebragraph

import (
	"context"

	"github.com/aalpar/wile/extensions/algebra/graph"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// maxNodes bounds num-nodes so a public caller cannot drive the kernel into an
// oversized allocation. CountPathsInDAG allocates several O(numNodes) arrays —
// the dominant being counts []*big.Int, one new(big.Int) per node — so a
// num-nodes of 2^40 from a two-token call OOMs the host. At 1<<22 nodes the
// worst-case allocation is a few hundred MB, far beyond any realistic embedded
// graph yet no longer a host-OOM DoS. Kept symmetric with sat.maxVars.
const maxNodes = 1 << 22

// extractNumNodes pulls a non-negative int from an exact-integer argument.
// Returns ErrInvalidArgument if the value is not an exact integer, is
// negative, exceeds int range, or exceeds maxNodes.
//
// Uses values.ExactInteger (not helpers.RequireArg[*values.Integer])
// because the contract here is "exact integer fitting int64" — which
// includes *BigInteger and integer-valued *Rational, per R7RS §6.1.
// helpers.RequireIndex is closest in spirit but requires a length
// bound; this helper has no upper bound from a sequence length.
func extractNumNodes(v values.Value, primName string) (int, error) {
	n, ok := values.ExactInteger(v)
	if !ok {
		return 0, werr.WrapForeignErrorf(werr.ErrNotAnInteger,
			"%s: num-nodes: expected exact integer in int64 range, got %T", primName, v)
	}
	if n < 0 {
		return 0, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"%s: num-nodes: must be non-negative, got %d", primName, n)
	}
	if n > int64(maxNodes) {
		return 0, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"%s: num-nodes: %d exceeds maximum (too many nodes; limit %d)", primName, n, maxNodes)
	}
	return int(n), nil
}

// extractNodeIndex pulls a node index in [0, numNodes) from an exact-integer
// argument, naming the slot in any error message.
func extractNodeIndex(v values.Value, numNodes int, primName, slot string) (int, error) {
	n, ok := values.ExactInteger(v)
	if !ok {
		return 0, werr.WrapForeignErrorf(werr.ErrNotAnInteger,
			"%s: %s: expected exact integer, got %T", primName, slot, v)
	}
	if n < 0 || n >= int64(numNodes) {
		return 0, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"%s: %s: %d out of range [0, %d)", primName, slot, n, numNodes)
	}
	return int(n), nil
}

// extractEdgeList walks the argument as a list of (u . v) pairs and returns
// the corresponding []graph.Edge. Each element must be a Pair whose car and
// cdr are exact integers in [0, numNodes).
func extractEdgeList(ctx context.Context, v values.Value, numNodes int, primName string) ([]graph.Edge, error) {
	lst, ok := v.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAList,
			"%s: edges: expected list of (u . v) pairs, got %T", primName, v)
	}
	var edges []graph.Edge
	err := helpers.ForEachList(ctx, lst, primName+": edges", func(_ context.Context, i int, _ bool, elem values.Value) error {
		// *values.Pair (not values.Tuple): an edge MUST be a dotted pair
		// like (u . v), not '() or a proper list like (u v). This is
		// the type-specific-predicate exception in the CLAUDE.md
		// "Tuple vs *Pair" rule — Tuple matches '() and proper lists
		// too, which would silently mis-parse user input.
		pair, isPair := elem.(*values.Pair)
		if !isPair {
			return werr.WrapForeignErrorf(werr.ErrNotAPair,
				"%s: edges[%d]: expected (u . v) pair, got %T", primName, i, elem)
		}
		u, uOK := values.ExactInteger(pair.Car())
		if !uOK {
			return werr.WrapForeignErrorf(werr.ErrNotAnInteger,
				"%s: edges[%d]: car: expected exact integer, got %T", primName, i, pair.Car())
		}
		w, wOK := values.ExactInteger(pair.Cdr())
		if !wOK {
			return werr.WrapForeignErrorf(werr.ErrNotAnInteger,
				"%s: edges[%d]: cdr: expected exact integer, got %T", primName, i, pair.Cdr())
		}
		if u < 0 || u >= int64(numNodes) {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"%s: edges[%d]: car: %d out of range [0, %d)", primName, i, u, numNodes)
		}
		if w < 0 || w >= int64(numNodes) {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"%s: edges[%d]: cdr: %d out of range [0, %d)", primName, i, w, numNodes)
		}
		edges = append(edges, graph.Edge{U: int(u), V: int(w)})
		return nil
	})
	if err != nil {
		return nil, err
	}
	return edges, nil
}

// PrimCountPathsInDAG implements (count-paths-in-dag num-nodes edges source).
// Returns a vector of exact-integer counts (length num-nodes), or #f when
// the input graph contains a cycle reachable from source.
func PrimCountPathsInDAG(mc machine.CallContext) error {
	const primName = "count-paths-in-dag"

	numNodes, err := extractNumNodes(mc.Arg(0), primName)
	if err != nil {
		return err
	}
	edges, err := extractEdgeList(mc.Context(), mc.Arg(1), numNodes, primName)
	if err != nil {
		return err
	}
	source, err := extractNodeIndex(mc.Arg(2), numNodes, primName, "source")
	if err != nil {
		return err
	}

	counts := graph.CountPathsInDAG(numNodes, edges, source)
	if counts == nil {
		// Cyclic input — signal to caller via #f.
		mc.SetValue(values.FalseValue)
		return nil
	}

	out := make([]values.Value, len(counts))
	for i, c := range counts {
		out[i] = values.NewBigInteger(c)
	}
	mc.SetValue(values.NewVector(out...))
	return nil
}

// PrimCountPathsCyclic implements (count-paths-cyclic num-nodes edges source).
// Returns three values: (scc-vector, counts-by-scc-vector, nontrivial-vector).
// scc-vector has length num-nodes; the other two have length (num-sccs).
func PrimCountPathsCyclic(mc machine.CallContext) error {
	const primName = "count-paths-cyclic"

	numNodes, err := extractNumNodes(mc.Arg(0), primName)
	if err != nil {
		return err
	}
	edges, err := extractEdgeList(mc.Context(), mc.Arg(1), numNodes, primName)
	if err != nil {
		return err
	}
	source, err := extractNodeIndex(mc.Arg(2), numNodes, primName, "source")
	if err != nil {
		return err
	}

	res, err := graph.CountPathsCyclic(numNodes, edges, source)
	if err != nil {
		// Wrapper validation should have caught everything the kernel
		// can reject; propagate verbatim if a future kernel invariant
		// regresses.
		return err
	}

	sccVec := make([]values.Value, len(res.SCC))
	for i, s := range res.SCC {
		sccVec[i] = values.NewInteger(int64(s))
	}
	countsVec := make([]values.Value, len(res.CountsBySCC))
	for i, c := range res.CountsBySCC {
		countsVec[i] = values.NewBigInteger(c)
	}
	ntVec := make([]values.Value, len(res.NonTrivial))
	for i, nt := range res.NonTrivial {
		ntVec[i] = values.BoolToBoolean(nt)
	}

	mc.SetValues(
		values.NewVector(sccVec...),
		values.NewVector(countsVec...),
		values.NewVector(ntVec...),
	)
	return nil
}
