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

package goastlint

import "github.com/aalpar/wile/registry"

// lintExtension wraps Extension to implement LibraryNamer.
type lintExtension struct {
	registry.Extension
}

// LibraryName returns (wile goast lint) for R7RS import.
func (p *lintExtension) LibraryName() []string {
	return []string{"wile", "goast", "lint"}
}

// Extension is the lint extension entry point.
var Extension registry.Extension = &lintExtension{
	Extension: registry.NewExtension("goast-lint", AddToRegistry),
}

// Builder aggregates all lint registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all lint primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{
			Name: "go-analyze", ParamCount: 2, IsVariadic: true,
			Impl:       PrimGoAnalyze,
			Doc:        "Runs named go/analysis passes on a Go package and returns diagnostics.",
			ParamNames: []string{"pattern", "analyzer-names"},
			Category:   "goast-lint",
		},
		{
			Name: "go-analyze-list", ParamCount: 0,
			Impl:       PrimGoAnalyzeList,
			Doc:        "Returns a sorted list of available analyzer names.",
			ParamNames: []string{},
			Category:   "goast-lint",
		},
	}, registry.PhaseRuntime)
	return nil
}
