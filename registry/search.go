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

package registry

import (
	"sort"
	"strings"

	"github.com/aalpar/wile/docparse"
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine/compilation"
)

// DocSearchResult holds one search hit from SearchDoc.
type DocSearchResult struct {
	Name     string
	Doc      string
	Category string
	Keywords []string
}

// SearchDoc searches all documentation sources for case-insensitive
// substring matches on name, doc text, category, or keywords.
//
// Sources searched in order:
//  1. Registry primitives
//  2. Registry binding specs (parsed via docparse)
//  3. Registry doc entries (parsed via docparse)
//  4. Environment bindings (if env is non-nil)
//  5. Loaded libraries (if libReg is non-nil)
//
// Primitives take precedence over non-primitives with the same name.
// Results are sorted by name. env and libReg may be nil.
func SearchDoc(reg *Registry, env *environment.EnvironmentFrame, libReg *compilation.LibraryRegistry, pattern string) []DocSearchResult {
	lowerPattern := strings.ToLower(pattern)
	var results []DocSearchResult

	// 1. Registry primitives — always take precedence.
	prims := reg.Primitives()
	primNames := make(map[string]bool, len(prims))
	for _, pr := range prims {
		primNames[pr.Spec.Name] = true
		if matchesDoc(pr.Spec.Name, pr.Spec.Doc, pr.Spec.Category, pr.Spec.Keywords, lowerPattern) {
			results = append(results, DocSearchResult{
				Name:     pr.Spec.Name,
				Doc:      pr.Spec.Doc,
				Category: pr.Spec.Category,
				Keywords: pr.Spec.Keywords,
			})
		}
	}

	// 2-3. Binding specs and doc entries (non-primitive docs).
	seen := make(map[string]bool)
	for _, r := range nonPrimitiveDocs(reg) {
		if primNames[r.Name] || seen[r.Name] {
			continue
		}
		if matchesDoc(r.Name, r.Doc, r.Category, r.Keywords, lowerPattern) {
			seen[r.Name] = true
			results = append(results, r)
		}
	}

	// 4. Environment bindings.
	if env != nil {
		for _, r := range searchEnvironmentBindings(env, lowerPattern) {
			if primNames[r.Name] || seen[r.Name] {
				continue
			}
			seen[r.Name] = true
			results = append(results, r)
		}
	}

	// 5. Loaded libraries.
	if libReg != nil {
		for _, r := range searchLibraries(libReg, lowerPattern) {
			if seen[r.Name] {
				continue
			}
			seen[r.Name] = true
			results = append(results, r)
		}
	}

	sort.Slice(results, func(i, j int) bool {
		return results[i].Name < results[j].Name
	})
	return results
}

// nonPrimitiveDocs returns doc search results from binding specs and doc entries.
func nonPrimitiveDocs(reg *Registry) []DocSearchResult {
	var results []DocSearchResult
	for _, bs := range reg.BindingSpecs() {
		if bs.Doc == "" {
			continue
		}
		parsed := docparse.ParseDocstring(bs.Doc)
		results = append(results, DocSearchResult{
			Name:     bs.Name,
			Doc:      parsed.Doc,
			Category: parsed.Category,
			Keywords: parsed.Keywords,
		})
	}
	for _, de := range reg.Docs() {
		parsed := docparse.ParseDocstring(de.Doc)
		results = append(results, DocSearchResult{
			Name:     de.Name,
			Doc:      parsed.Doc,
			Category: parsed.Category,
			Keywords: parsed.Keywords,
		})
	}
	return results
}

// searchEnvironmentBindings walks phase environment bindings for matches.
func searchEnvironmentBindings(env *environment.EnvironmentFrame, lowerPattern string) []DocSearchResult {
	ns := env.Namespace()
	if ns == nil {
		return nil
	}
	phases := ns.Phases()
	phaseIndices := phases.Phases()

	seen := make(map[string]bool)
	var results []DocSearchResult
	for _, phase := range phaseIndices {
		phaseEnv := phases.Get(phase)
		if phaseEnv == nil {
			continue
		}
		global := phaseEnv.GlobalEnvironment()
		if global == nil {
			continue
		}
		keys := global.Keys()
		bindings := global.Bindings()
		for sym, idx := range keys {
			name := sym.Key
			if seen[name] {
				continue
			}
			seen[name] = true

			doc := ""
			if idx < len(bindings) {
				bnd := bindings[idx]
				if bnd == nil {
					continue
				}
				doc = bnd.Doc()
				if doc == "" && bnd.BindingType() == environment.BindingTypeVariable {
					dc, ok := bnd.Value().(interface{ Doc() string })
					if ok {
						doc = dc.Doc()
					}
				}
			}

			category := ""
			var keywords []string
			displayDoc := doc
			if doc != "" {
				parsed := docparse.ParseDocstring(doc)
				if parsed.HasStructuredMetadata() {
					category = parsed.Category
					keywords = parsed.Keywords
					displayDoc = parsed.Doc
				}
			}

			if matchesDoc(name, doc, category, keywords, lowerPattern) {
				results = append(results, DocSearchResult{
					Name:     name,
					Doc:      displayDoc,
					Category: category,
					Keywords: keywords,
				})
			}
		}
	}
	return results
}

// searchLibraries searches loaded libraries for matches.
func searchLibraries(libReg *compilation.LibraryRegistry, lowerPattern string) []DocSearchResult {
	var results []DocSearchResult
	for _, lib := range libReg.All() {
		name := lib.Name.SchemeString()
		if strings.Contains(strings.ToLower(name), lowerPattern) ||
			strings.Contains(strings.ToLower(lib.Description), lowerPattern) {
			results = append(results, DocSearchResult{
				Name:     name,
				Doc:      lib.Description,
				Category: "library",
			})
		}
	}
	return results
}

// matchesDoc reports whether any of name, doc, category, or keywords
// contains the given lowercase pattern.
func matchesDoc(name, doc, category string, keywords []string, pattern string) bool {
	if strings.Contains(strings.ToLower(name), pattern) ||
		strings.Contains(strings.ToLower(doc), pattern) ||
		strings.Contains(strings.ToLower(category), pattern) {
		return true
	}
	for _, kw := range keywords {
		if strings.Contains(strings.ToLower(kw), pattern) {
			return true
		}
	}
	return false
}
