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
	"fmt"
	"sort"
	"strings"

	"github.com/aalpar/wile/docparse"
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine/compilation"
)

// ExtractLibraryRegistry extracts the *compilation.LibraryRegistry from an
// environment frame, returning nil if unavailable or a different concrete type.
func ExtractLibraryRegistry(env *environment.EnvironmentFrame) *compilation.LibraryRegistry {
	if env == nil {
		return nil
	}
	lr, ok := env.LibraryRegistry().(*compilation.LibraryRegistry)
	if !ok {
		return nil
	}
	return lr
}

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
//  6. Unloaded library exports (if exportIndex is non-nil)
//
// Primitives take precedence over non-primitives with the same name.
// Results are sorted by name. env, libReg, and exportIndex may be nil.
func SearchDoc(reg *Registry, env *environment.EnvironmentFrame, libReg *compilation.LibraryRegistry, exportIndex *compilation.LibraryExportIndex, pattern string) []DocSearchResult {
	lowerPattern := strings.ToLower(pattern)
	var q []DocSearchResult

	// 1. Registry primitives — always take precedence.
	prims := reg.Primitives()
	primNames := make(map[string]bool, len(prims))
	for _, pr := range prims {
		primNames[pr.Spec.Name] = true
		if matchesDoc(pr.Spec.Name, pr.Spec.Doc, pr.Spec.Category, pr.Spec.Keywords, lowerPattern) {
			q = append(q, DocSearchResult{
				Name:     pr.Spec.Name,
				Doc:      pr.Spec.Doc,
				Category: pr.Spec.Category,
				Keywords: pr.Spec.Keywords,
			})
		}
	}

	// 2-3. Binding specs and doc entries (non-primitive docs).
	seen := make(map[string]bool)
	for _, r := range NonPrimitiveDocs(reg) {
		if primNames[r.Name] || seen[r.Name] {
			continue
		}
		if matchesDoc(r.Name, r.Doc, r.Category, r.Keywords, lowerPattern) {
			seen[r.Name] = true
			q = append(q, r)
		}
	}

	// 4. Environment bindings.
	if env != nil {
		for _, r := range searchEnvironmentBindings(env, lowerPattern) {
			if primNames[r.Name] || seen[r.Name] {
				continue
			}
			seen[r.Name] = true
			q = append(q, r)
		}
	}

	// 5. Loaded libraries.
	if libReg != nil {
		for _, r := range searchLibraries(libReg, lowerPattern) {
			if seen[r.Name] {
				continue
			}
			seen[r.Name] = true
			q = append(q, r)
		}
	}

	// 6. Unloaded library exports.
	if exportIndex != nil {
		for _, r := range searchUnloadedExports(exportIndex, libReg, lowerPattern) {
			if primNames[r.Name] || seen[r.Name] {
				continue
			}
			seen[r.Name] = true
			q = append(q, r)
		}
	}

	sort.Slice(q, func(i, j int) bool {
		return q[i].Name < q[j].Name
	})
	return q
}

// NonPrimitiveDocs returns doc search results from binding specs and doc entries.
// Each entry's Doc, Category, and Keywords are extracted via docparse.ParseDocstring.
func NonPrimitiveDocs(reg *Registry) []DocSearchResult {
	var q []DocSearchResult
	for _, bs := range reg.BindingSpecs() {
		if bs.Doc == "" {
			continue
		}
		parsed := docparse.ParseDocstring(bs.Doc)
		q = append(q, DocSearchResult{
			Name:     bs.Name,
			Doc:      parsed.Doc,
			Category: parsed.Category,
			Keywords: parsed.Keywords,
		})
	}
	for _, de := range reg.Docs() {
		parsed := docparse.ParseDocstring(de.Doc)
		q = append(q, DocSearchResult{
			Name:     de.Name,
			Doc:      parsed.Doc,
			Category: parsed.Category,
			Keywords: parsed.Keywords,
		})
	}
	return q
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
	var q []DocSearchResult
	for _, phase := range phaseIndices {
		phaseEnv := phases.Get(phase)
		if phaseEnv == nil {
			continue
		}
		global := phaseEnv.GlobalEnvironment()
		if global == nil {
			continue
		}
		// Keys() and Bindings() are separate locked snapshots. A concurrent
		// define could add a key whose index exceeds the bindings snapshot
		// length. The idx < len(bindings) guard below prevents a panic;
		// the skipped entry is acceptable for a best-effort search.
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
				q = append(q, DocSearchResult{
					Name:     name,
					Doc:      displayDoc,
					Category: category,
					Keywords: keywords,
				})
			}
		}
	}
	return q
}

// searchLibraries searches loaded libraries for matches.
func searchLibraries(libReg *compilation.LibraryRegistry, lowerPattern string) []DocSearchResult {
	var q []DocSearchResult
	for _, lib := range libReg.All() {
		name := lib.Name.SchemeString()
		if strings.Contains(strings.ToLower(name), lowerPattern) ||
			strings.Contains(strings.ToLower(lib.Description), lowerPattern) {
			q = append(q, DocSearchResult{
				Name:     name,
				Doc:      lib.Description,
				Category: "library",
			})
		}
	}
	return q
}

// searchUnloadedExports searches the export index for matching library names,
// descriptions, and export names from libraries that are not yet loaded.
// Libraries already present in libReg are skipped (they were imported after
// the index was built).
func searchUnloadedExports(idx *compilation.LibraryExportIndex, libReg *compilation.LibraryRegistry, lowerPattern string) []DocSearchResult {
	if idx == nil {
		return nil
	}
	var q []DocSearchResult
	for _, summary := range idx.Entries() {
		if libReg != nil && libReg.Lookup(summary.Name) != nil {
			continue
		}

		libName := summary.Name.SchemeString()

		// Library-level match: check name and description, mirroring
		// searchLibraries for loaded libraries.
		if strings.Contains(strings.ToLower(libName), lowerPattern) ||
			strings.Contains(strings.ToLower(summary.Description), lowerPattern) {
			q = append(q, DocSearchResult{
				Name:     libName,
				Doc:      summary.Description,
				Category: "library (not imported)",
			})
		}

		// Export-level match: check individual export names.
		for _, export := range summary.Exports {
			if !strings.Contains(strings.ToLower(export), lowerPattern) {
				continue
			}
			doc := libName
			if summary.Description != "" {
				doc = fmt.Sprintf("%s — %s", libName, summary.Description)
			}
			q = append(q, DocSearchResult{
				Name:     export,
				Category: "not imported",
				Doc:      doc,
			})
		}
	}
	return q
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
