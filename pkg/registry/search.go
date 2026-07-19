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

	"github.com/aalpar/wile/pkg/docparse"
	"github.com/aalpar/wile/pkg/environment"
)

// DocSearchResult holds one search hit from SearchDoc.
type DocSearchResult struct {
	Name     string
	Doc      string
	Category string
	Keywords []string
}

// LibraryDoc is a read-only view of a loaded library, sufficient for
// documentation search.
type LibraryDoc struct {
	Name        string // canonical Scheme form, e.g. "(wile math)"
	Description string
}

// LibraryExportDoc is a read-only view of an indexed (possibly unloaded)
// library and the names it exports.
type LibraryExportDoc struct {
	Name        string // canonical Scheme form, e.g. "(srfi 1)"
	Description string
	Exports     []string
}

// LibrarySearcher enumerates loaded libraries for SearchDoc. Defining the
// dependency as a narrow interface keeps the registry package free of any
// import of machine/compilation — the concrete adapter lives in the caller's
// package (see registry/core). Per the Interface Segregation Principle,
// SearchDoc depends only on the enumeration it uses, not on the full
// library-registry surface.
type LibrarySearcher interface {
	AllLibraries() []LibraryDoc
}

// LibraryExportSearcher enumerates indexed library exports for SearchDoc.
type LibraryExportSearcher interface {
	AllLibraryExports() []LibraryExportDoc
}

// SearchDoc searches all documentation sources for case-insensitive
// substring matches on name, doc text, category, or keywords.
//
// Sources searched in order:
//  1. Registry primitives — real primitives AND doc-only primitives
//     (the latter carry full PrimitiveSpec metadata; both take precedence
//     over non-primitive sources below).
//  2. Registry binding specs — includes real bindings AND simple DocOnly
//     entries registered via AddDocumentation (post-Phase-1 unification).
//  3. Environment bindings (if env is non-nil)
//  4. Loaded libraries (if libs is non-nil)
//  5. Unloaded library exports (if exports is non-nil)
//
// Primitives take precedence over non-primitives with the same name.
// Results are sorted by name. env, libs, and exports may be nil.
func SearchDoc(reg *Registry, env *environment.EnvironmentFrame, libs LibrarySearcher, exports LibraryExportSearcher, pattern string) []DocSearchResult {
	lowerPattern := strings.ToLower(pattern)
	var q []DocSearchResult

	// 1. Registry primitives — always take precedence. Walks both real
	// primitives and doc-only primitives; AddDocOnlyPrimitive's
	// skip-if-primitive-exists guarantees no duplicate name across the two.
	prims := reg.Primitives()
	primNames := make(map[string]bool, len(prims)+8)
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
	for _, dp := range reg.DocPrimitives() {
		primNames[dp.Name] = true
		if matchesDoc(dp.Name, dp.Doc, dp.Category, dp.Keywords, lowerPattern) {
			q = append(q, DocSearchResult{
				Name:     dp.Name,
				Doc:      dp.Doc,
				Category: dp.Category,
				Keywords: dp.Keywords,
			})
		}
	}

	// 2. Binding specs (non-primitive docs; includes DocOnly entries).
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

	// 3. Environment bindings.
	if env != nil {
		for _, r := range searchEnvironmentBindings(env, lowerPattern) {
			if primNames[r.Name] || seen[r.Name] {
				continue
			}
			seen[r.Name] = true
			q = append(q, r)
		}
	}

	// 4. Loaded libraries.
	if libs != nil {
		for _, r := range searchLibraries(libs, lowerPattern) {
			if seen[r.Name] {
				continue
			}
			seen[r.Name] = true
			q = append(q, r)
		}
	}

	// 5. Unloaded library exports.
	if exports != nil {
		for _, r := range searchUnloadedExports(exports, libs, lowerPattern) {
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

// NonPrimitiveDocs returns doc search results from binding specs (including
// DocOnly entries registered via AddDocumentation / AddDocOnlyPrimitive).
// Each entry's Doc, Category, and Keywords are extracted via docparse.ParseDocstring.
//
// Post-Phase-1: single walk. Pre-Phase-1 this walked BindingSpecs + Docs
// as two separate sources; they were unified into bindingSpecs (with DocOnly
// distinguishing them).
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

	collect := func(frame *environment.EnvironmentFrame) {
		if frame == nil {
			return
		}
		global := frame.GlobalEnvironment()
		if global == nil {
			return
		}
		// Keys() and Bindings() are separate locked snapshots. A concurrent
		// define could add a key whose index exceeds the bindings snapshot
		// length. The idx < len(bindings) guard below prevents a panic;
		// the skipped entry is acceptable for a best-effort search.
		keys := global.Keys()
		bindings := global.Bindings()
		for sym, slots := range keys {
			name := sym.Key
			if seen[name] || len(slots) == 0 {
				continue
			}
			seen[name] = true

			// Search is name-oriented and already dedupes by name, so the first
			// slot represents the name. A name can now own several slots (bindings
			// distinguished by hygiene scope set), but they share a docstring
			// audience.
			idx := slots[0]

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

	for _, phase := range phaseIndices {
		collect(phases.Get(phase))
	}
	// Post-carve, primitives and bootstrap procedures live in the sealed base — the
	// parent of the runtime phase frame, which is NOT a phase-registry entry, so the
	// phase walk above never reaches it. Collect it so ,apropos still surfaces
	// binding-level docs on sealed entries, mirroring Namespace.BoundSymbolNames.
	collect(ns.SealedBase())

	return q
}

// searchLibraries searches loaded libraries for matches.
func searchLibraries(libs LibrarySearcher, lowerPattern string) []DocSearchResult {
	var q []DocSearchResult
	for _, lib := range libs.AllLibraries() {
		if strings.Contains(strings.ToLower(lib.Name), lowerPattern) ||
			strings.Contains(strings.ToLower(lib.Description), lowerPattern) {
			q = append(q, DocSearchResult{
				Name:     lib.Name,
				Doc:      lib.Description,
				Category: "library",
			})
		}
	}
	return q
}

// searchUnloadedExports searches the export index for matching library names,
// descriptions, and export names from libraries that are not yet loaded.
// Libraries already present in libs are skipped (they were imported after
// the index was built).
func searchUnloadedExports(exports LibraryExportSearcher, libs LibrarySearcher, lowerPattern string) []DocSearchResult {
	if exports == nil {
		return nil
	}

	// Libraries already loaded are excluded — match by canonical name.
	// Name strings are an injective key (library name parts are identifiers
	// with no embedded spaces), so string equality matches the structural
	// LibraryName equality the old *LibraryRegistry.Lookup performed.
	loaded := make(map[string]bool)
	if libs != nil {
		for _, lib := range libs.AllLibraries() {
			loaded[lib.Name] = true
		}
	}

	var q []DocSearchResult
	for _, summary := range exports.AllLibraryExports() {
		if loaded[summary.Name] {
			continue
		}

		// Library-level match: check name and description, mirroring
		// searchLibraries for loaded libraries.
		if strings.Contains(strings.ToLower(summary.Name), lowerPattern) ||
			strings.Contains(strings.ToLower(summary.Description), lowerPattern) {
			q = append(q, DocSearchResult{
				Name:     summary.Name,
				Doc:      summary.Description,
				Category: "library (not imported)",
			})
		}

		// Export-level match: check individual export names.
		for _, export := range summary.Exports {
			if !strings.Contains(strings.ToLower(export), lowerPattern) {
				continue
			}
			doc := summary.Name
			if summary.Description != "" {
				doc = fmt.Sprintf("%s — %s", summary.Name, summary.Description)
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
