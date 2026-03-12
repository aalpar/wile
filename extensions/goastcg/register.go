package goastcg

import "github.com/aalpar/wile/registry"

// cgExtension wraps Extension to implement LibraryNamer.
type cgExtension struct {
	registry.Extension
}

// LibraryName returns (wile goast callgraph) for R7RS import.
func (p *cgExtension) LibraryName() []string {
	return []string{"wile", "goast", "callgraph"}
}

// Extension is the callgraph extension entry point.
var Extension registry.Extension = &cgExtension{
	Extension: registry.NewExtension("goast-callgraph", AddToRegistry),
}

// Builder aggregates all callgraph registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all callgraph primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{
			Name: "go-callgraph", ParamCount: 2, IsVariadic: false,
			Impl:       PrimGoCallgraph,
			Doc:        "Builds a call graph for a Go package using the specified algorithm.",
			ParamNames: []string{"pattern", "algorithm"},
			Category:   "goast-callgraph",
		},
		{
			Name: "go-callgraph-callers", ParamCount: 2,
			Impl:       PrimGoCallgraphCallers,
			Doc:        "Returns the incoming edges (callers) of a function in the call graph.",
			ParamNames: []string{"graph", "func-name"},
			Category:   "goast-callgraph",
		},
		{
			Name: "go-callgraph-callees", ParamCount: 2,
			Impl:       PrimGoCallgraphCallees,
			Doc:        "Returns the outgoing edges (callees) of a function in the call graph.",
			ParamNames: []string{"graph", "func-name"},
			Category:   "goast-callgraph",
		},
		{
			Name: "go-callgraph-reachable", ParamCount: 2,
			Impl:       PrimGoCallgraphReachable,
			Doc:        "Returns a list of function names transitively reachable from the root.",
			ParamNames: []string{"graph", "root-name"},
			Category:   "goast-callgraph",
		},
	}, registry.PhaseRuntime)
	return nil
}
