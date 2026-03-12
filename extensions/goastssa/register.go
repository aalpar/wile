package goastssa

import "github.com/aalpar/wile/registry"

// ssaExtension wraps ExtensionFunc to implement LibraryNamer.
type ssaExtension struct {
	registry.Extension
}

// LibraryName returns (wile goast ssa) for R7RS import.
func (p *ssaExtension) LibraryName() []string {
	return []string{"wile", "goast", "ssa"}
}

// Extension is the SSA extension entry point.
var Extension registry.Extension = &ssaExtension{
	Extension: registry.NewExtension("goast-ssa", AddToRegistry),
}

// Builder aggregates all SSA registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all SSA primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{
			Name: "go-ssa-build", ParamCount: 2, IsVariadic: true,
			Impl:       PrimGoSSABuild,
			Doc:        "Builds SSA for a Go package and returns a list of ssa-func nodes.",
			ParamNames: []string{"pattern", "options"},
			Category:   "goast-ssa",
		},
	}, registry.PhaseRuntime)
	return nil
}
