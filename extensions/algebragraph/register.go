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
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
)

// Extension is the algebra-graph FFI extension.
var Extension = registry.NewDescribedExtension("algebragraph",
	"Graph algorithm kernels (path counting on DAGs and cyclic graphs via SCC condensation) backing (wile algebra graph)'s bigint-carrier fast path.",
	AddToRegistry)

// Builder aggregates all algebragraph registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all algebragraph primitives.
var AddToRegistry = Builder.AddToRegistry

// graphPrimParamTypes is the shared parameter shape for both count-paths-*
// primitives: (exact-integer, list-of-pairs, exact-integer).
var graphPrimParamTypes = []values.TypeConstraint{
	values.TypeInteger,
	values.TypeList,
	values.TypeInteger,
}

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{
			Name:       "count-paths-in-dag",
			ParamCount: 3,
			Impl:       PrimCountPathsInDAG,
			Doc:        "Count paths in a DAG. Returns a vector of length num-nodes where v[i] is the number of distinct paths from source to node i (as an exact integer). Returns #f iff the input graph contains a cycle reachable from source (counting on cycles diverges); raises an error for invalid input (non-integer arg, malformed edges, out-of-range source). Internal FFI for (wile algebra graph).\n\nParameters:\n  num-nodes : positive integer (source must satisfy 0 <= source < num-nodes)\n  edges : list of (u . v) pairs of node indices in [0, num-nodes)\n  source : node index in [0, num-nodes)\nReturns: vector or #f\nCategory: algebra-graph",
			ParamNames: []string{"num-nodes", "edges", "source"},
			ParamTypes: graphPrimParamTypes,
			Category:   "algebra-graph",
			Keywords:   []string{"graph", "path-count", "dag", "monotone"},
		},
		{
			Name:       "count-paths-cyclic",
			ParamCount: 3,
			Impl:       PrimCountPathsCyclic,
			Doc:        "Count paths via SCC condensation. Returns three values: (1) a length-num-nodes vector mapping each node to its SCC ID; (2) a length-num-sccs vector of distinct-path counts (per-SCC); (3) a length-num-sccs vector of #t/#f flags marking SCCs that contain a cycle. For nodes in non-trivial SCCs, the per-SCC count is the entry count — the number of distinct paths from the source SCC that reach this SCC via some entry point in the condensed DAG (within-SCC counts are infinite). Raises an error on invalid input. Internal FFI for (wile algebra graph).\n\nParameters:\n  num-nodes : positive integer (source must satisfy 0 <= source < num-nodes)\n  edges : list of (u . v) pairs of node indices in [0, num-nodes)\n  source : node index in [0, num-nodes)\nReturns: 3 values (vector, vector, vector)\nCategory: algebra-graph",
			ParamNames: []string{"num-nodes", "edges", "source"},
			ParamTypes: graphPrimParamTypes,
			Category:   "algebra-graph",
			Keywords:   []string{"graph", "path-count", "scc", "condensation", "cyclic"},
		},
	}, registry.PhaseSetRuntime)
	return nil
}
