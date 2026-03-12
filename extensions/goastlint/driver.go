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

import (
	"go/token"
	"go/types"

	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/packages"
)

// diagnostic holds a captured analysis diagnostic.
type diagnostic struct {
	analyzerName string
	diag         analysis.Diagnostic
}

// runAnalyzers runs the requested analyzers on a loaded package, first
// resolving and running all prerequisites in topological order.
// Only diagnostics from the originally requested analyzers are returned;
// prerequisite results are available to dependents via Pass.ResultOf but
// their diagnostics are silently discarded.
func runAnalyzers(pkg *packages.Package, fset *token.FileSet, analyzers []*analysis.Analyzer) []diagnostic {
	// Collect all analyzers (including prerequisites) in topological order.
	ordered := topoSort(analyzers)

	// Track which analyzers were explicitly requested (not just prerequisites).
	requested := make(map[*analysis.Analyzer]bool, len(analyzers))
	for _, a := range analyzers {
		requested[a] = true
	}

	resultOf := make(map[*analysis.Analyzer]any)
	failed := make(map[*analysis.Analyzer]bool)
	var diags []diagnostic

	for _, a := range ordered {
		// Skip if any prerequisite failed — avoids nil-deref in ResultOf lookups.
		skip := false
		for _, req := range a.Requires {
			if failed[req] {
				skip = true
				break
			}
		}
		if skip {
			failed[a] = true
			continue
		}

		var collected []analysis.Diagnostic
		pass := makePass(pkg, fset, a, resultOf, func(d analysis.Diagnostic) {
			if requested[a] {
				collected = append(collected, d)
			}
		})
		result, err := a.Run(pass)
		if err != nil {
			failed[a] = true
			continue
		}
		if result != nil {
			resultOf[a] = result
		}
		for _, d := range collected {
			diags = append(diags, diagnostic{
				analyzerName: a.Name,
				diag:         d,
			})
		}
	}
	return diags
}

// topoSort returns all analyzers (including transitive prerequisites) in
// topological order: prerequisites before their dependents.
func topoSort(analyzers []*analysis.Analyzer) []*analysis.Analyzer {
	var ordered []*analysis.Analyzer
	seen := make(map[*analysis.Analyzer]bool)
	var visit func(a *analysis.Analyzer)
	visit = func(a *analysis.Analyzer) {
		if seen[a] {
			return
		}
		seen[a] = true
		for _, req := range a.Requires {
			visit(req)
		}
		ordered = append(ordered, a)
	}
	for _, a := range analyzers {
		visit(a)
	}
	return ordered
}

// makePass constructs an analysis.Pass for the given analyzer and package.
// Fact functions are no-ops: single-package analysis, no cross-package facts.
// This means ctrlflow's noReturn facts won't propagate across packages,
// so nilness and lostcancel may under-report (false negatives). Acceptable.
func makePass(
	pkg *packages.Package,
	fset *token.FileSet,
	a *analysis.Analyzer,
	resultOf map[*analysis.Analyzer]any,
	report func(analysis.Diagnostic),
) *analysis.Pass {
	return &analysis.Pass{
		Analyzer:          a,
		Fset:              fset,
		Files:             pkg.Syntax,
		Pkg:               pkg.Types,
		TypesInfo:         pkg.TypesInfo,
		TypesSizes:        pkg.TypesSizes,
		ResultOf:          resultOf,
		Report:            report,
		AllObjectFacts:    func() []analysis.ObjectFact { return nil },
		AllPackageFacts:   func() []analysis.PackageFact { return nil },
		ExportObjectFact:  func(types.Object, analysis.Fact) {},
		ExportPackageFact: func(analysis.Fact) {},
		ImportObjectFact:  func(types.Object, analysis.Fact) bool { return false },
		ImportPackageFact: func(*types.Package, analysis.Fact) bool { return false },
	}
}
