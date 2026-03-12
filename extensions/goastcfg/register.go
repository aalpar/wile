package goastcfg

import "github.com/aalpar/wile/registry"

// cfgExtension wraps Extension to implement LibraryNamer.
type cfgExtension struct {
	registry.Extension
}

// LibraryName returns (wile goast cfg) for R7RS import.
func (p *cfgExtension) LibraryName() []string {
	return []string{"wile", "goast", "cfg"}
}

// Extension is the CFG extension entry point.
var Extension registry.Extension = &cfgExtension{
	Extension: registry.NewExtension("goast-cfg", AddToRegistry),
}

// Builder aggregates all CFG registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all CFG primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{
			Name: "go-cfg", ParamCount: 3, IsVariadic: true,
			Impl:       PrimGoCFG,
			Doc:        "Builds the CFG for a named function in a Go package.",
			ParamNames: []string{"pattern", "func-name", "options"},
			Category:   "goast-cfg",
		},
		{
			Name: "go-cfg-dominators", ParamCount: 1,
			Impl:       PrimGoCFGDominators,
			Doc:        "Builds a dominator tree from a cfg-block list returned by go-cfg.",
			ParamNames: []string{"cfg"},
			Category:   "goast-cfg",
		},
		{
			Name: "go-cfg-dominates?", ParamCount: 3,
			Impl:       PrimGoCFGDominates,
			Doc:        "Returns #t if block a dominates block b in the dominator tree.",
			ParamNames: []string{"dom-tree", "a", "b"},
			Category:   "goast-cfg",
		},
		{
			Name: "go-cfg-paths", ParamCount: 3,
			Impl:       PrimGoCFGPaths,
			Doc:        "Enumerates simple paths between two blocks in the CFG. Capped at 1024 paths.",
			ParamNames: []string{"cfg", "from", "to"},
			Category:   "goast-cfg",
		},
	}, registry.PhaseRuntime)
	return nil
}
