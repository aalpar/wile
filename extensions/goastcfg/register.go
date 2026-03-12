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
		{Name: "go-cfg", ParamCount: 3, IsVariadic: true, Impl: PrimGoCFG,
			Doc:        "Builds the CFG for a named function in a Go package.",
			ParamNames: []string{"pattern", "func-name", "options"}, Category: "goast-cfg"},
		{Name: "go-cfg-dominators", ParamCount: 1, Impl: PrimGoCFGDominators,
			Doc:        "Builds a dominator tree from a cfg-block list returned by go-cfg.",
			ParamNames: []string{"cfg"}, Category: "goast-cfg"},
		{Name: "go-cfg-dominates?", ParamCount: 3, Impl: PrimGoCFGDominates,
			Doc:        "Returns #t if block a dominates block b in the dominator tree.",
			ParamNames: []string{"dom-tree", "a", "b"}, Category: "goast-cfg"},
		{Name: "go-cfg-paths", ParamCount: 3, Impl: PrimGoCFGPaths,
			Doc:        "Enumerates simple paths between two blocks in the CFG. Capped at 1024 paths.",
			ParamNames: []string{"cfg", "from", "to"}, Category: "goast-cfg"},
	}, registry.PhaseRuntime)
	return nil
}
