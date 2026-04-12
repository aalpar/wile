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

package repl

import (
	"context"
	"sort"
	"strings"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/docparse"
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/registry"
)

// RegistryDocProvider adapts a registry.Registry to the DocProvider interface.
type RegistryDocProvider struct {
	reg *registry.Registry
	eng *wile.Engine
}

// NewRegistryDocProvider creates a DocProvider backed by the given registry.
// eng may be nil; when non-nil, Search includes loaded and unloaded libraries.
func NewRegistryDocProvider(reg *registry.Registry, eng *wile.Engine) *RegistryDocProvider {
	return &RegistryDocProvider{
		reg: reg,
		eng: eng,
	}
}

// env returns the engine's environment, or nil if the engine is nil.
func (p *RegistryDocProvider) env() *environment.EnvironmentFrame {
	if p.eng == nil {
		return nil
	}
	return p.eng.Environment()
}

// LookupDoc returns documentation for the named binding from the registry.
// It checks primitives first, then falls back to binding specs and doc entries.
func (p *RegistryDocProvider) LookupDoc(name string) (DocInfo, bool) {
	pr, found := p.reg.FindPrimitive(name, 0)
	if found {
		return DocInfo{
			Doc:        pr.Spec.Doc,
			TypeLabel:  "primitive",
			ParamNames: pr.Spec.ParamNames,
			Category:   pr.Spec.Category,
			ParamCount: pr.Spec.ParamCount,
			IsVariadic: pr.Spec.IsVariadic,
			ParamTypes: pr.Spec.ParamTypes,
			ReturnType: pr.Spec.ReturnType,
			Keywords:   pr.Spec.Keywords,
		}, true
	}

	// Fall back to binding specs and doc entries.
	info, ok := p.lookupNonPrimitiveDoc(name)
	if ok {
		return info, true
	}
	return DocInfo{}, false
}

// lookupNonPrimitiveDoc searches binding specs and doc entries for the named binding.
func (p *RegistryDocProvider) lookupNonPrimitiveDoc(name string) (DocInfo, bool) {
	for _, bs := range p.reg.BindingSpecs() {
		if bs.Name == name && bs.Doc != "" {
			parsed := docparse.ParseDocstring(bs.Doc)
			return DocInfo{
				Doc:        parsed.Doc,
				Syntax:     parsed.Syntax,
				ParamNames: parsed.ParamNames,
				ParamTypes: parsed.ParamTypes,
				ReturnType: parsed.ReturnType,
				Category:   parsed.Category,
				Keywords:   parsed.Keywords,
			}, true
		}
	}
	for _, de := range p.reg.Docs() {
		if de.Name == name {
			parsed := docparse.ParseDocstring(de.Doc)
			return DocInfo{
				Doc:        parsed.Doc,
				Syntax:     parsed.Syntax,
				ParamNames: parsed.ParamNames,
				ParamTypes: parsed.ParamTypes,
				ReturnType: parsed.ReturnType,
				Category:   parsed.Category,
				Keywords:   parsed.Keywords,
			}, true
		}
	}
	return DocInfo{}, false
}

// Search returns entries whose name, doc, or category contains pattern
// (case-insensitive substring match). Results are sorted by name.
// Delegates to registry.SearchDoc for non-library results, then appends
// library results from the Engine's loaded and unloaded library methods.
func (p *RegistryDocProvider) Search(ctx context.Context, pattern string) []registry.DocSearchResult {
	// Get non-library results from the registry (passing nil for library params).
	q := registry.SearchDoc(p.reg, p.env(), nil, nil, pattern)

	// Append library results from the Engine.
	if p.eng != nil {
		lowerPattern := strings.ToLower(pattern)
		seen := make(map[string]bool, len(q))
		for _, r := range q {
			seen[r.Name] = true
		}

		// Loaded libraries.
		for _, lib := range p.eng.LoadedLibraries() {
			if seen[lib.Name] {
				continue
			}
			if strings.Contains(strings.ToLower(lib.Name), lowerPattern) ||
				strings.Contains(strings.ToLower(lib.Description), lowerPattern) {
				seen[lib.Name] = true
				q = append(q, registry.DocSearchResult{
					Name:     lib.Name,
					Doc:      lib.Description,
					Category: "library",
				})
			}
		}

		// Unloaded libraries.
		for _, lib := range p.eng.UnloadedLibraries(ctx) {
			if seen[lib.Name] {
				continue
			}
			if strings.Contains(strings.ToLower(lib.Name), lowerPattern) ||
				strings.Contains(strings.ToLower(lib.Description), lowerPattern) {
				seen[lib.Name] = true
				q = append(q, registry.DocSearchResult{
					Name:     lib.Name,
					Doc:      lib.Description,
					Category: "library (not imported)",
				})
			}

			// Export-level match: check individual export names.
			for _, export := range lib.Exports {
				if seen[export] {
					continue
				}
				if !strings.Contains(strings.ToLower(export), lowerPattern) {
					continue
				}
				seen[export] = true
				doc := lib.Name
				if lib.Description != "" {
					doc = lib.Name + " — " + lib.Description
				}
				q = append(q, registry.DocSearchResult{
					Name:     export,
					Category: "not imported",
					Doc:      doc,
				})
			}
		}

		// Re-sort after appending library results.
		sort.Slice(q, func(i, j int) bool {
			return q[i].Name < q[j].Name
		})
	}

	return q
}

// UnloadedLibraries returns info for libraries that are discoverable via
// the file resolver but not yet imported. Returns nil if no engine is
// available.
func (p *RegistryDocProvider) UnloadedLibraries(ctx context.Context) []*wile.LibraryInfo {
	if p.eng == nil {
		return nil
	}
	return p.eng.UnloadedLibraries(ctx)
}

// Categories returns sorted category names, excluding the empty-string category.
func (p *RegistryDocProvider) Categories() []string {
	cats := make(map[string]bool)
	for _, pr := range p.reg.Primitives() {
		if pr.Spec.Category != "" {
			cats[pr.Spec.Category] = true
		}
	}
	for _, r := range registry.NonPrimitiveDocs(p.reg) {
		if r.Category != "" {
			cats[r.Category] = true
		}
	}

	q := make([]string, 0, len(cats))
	for cat := range cats {
		q = append(q, cat)
	}
	sort.Strings(q)
	return q
}

// ByCategory returns entries in the named category, sorted by name.
func (p *RegistryDocProvider) ByCategory(category string) []registry.DocSearchResult {
	seen := make(map[string]bool)
	var results []registry.DocSearchResult

	byCategory := p.reg.PrimitivesByCategory()
	for _, pr := range byCategory[category] {
		seen[pr.Spec.Name] = true
		results = append(results, registry.DocSearchResult{
			Name:     pr.Spec.Name,
			Doc:      pr.Spec.Doc,
			Category: pr.Spec.Category,
			Keywords: pr.Spec.Keywords,
		})
	}

	for _, r := range registry.NonPrimitiveDocs(p.reg) {
		if r.Category == category && !seen[r.Name] {
			seen[r.Name] = true
			results = append(results, r)
		}
	}

	if len(results) == 0 {
		return nil
	}
	sort.Slice(results, func(i, j int) bool {
		return results[i].Name < results[j].Name
	})
	return results
}
