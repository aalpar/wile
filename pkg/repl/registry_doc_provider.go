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
	"cmp"
	"context"
	"maps"
	"slices"
	"strings"

	"github.com/aalpar/wile/pkg/docparse"
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"
)

// RegistryDocProvider adapts a registry.PrimitiveRegistry to the DocProvider interface.
type RegistryDocProvider struct {
	reg *registry.PrimitiveRegistry
	eng *wile.Engine
}

// NewRegistryDocProvider creates a DocProvider backed by the given registry.
// eng may be nil; when non-nil, Search includes loaded and unloaded libraries.
func NewRegistryDocProvider(reg *registry.PrimitiveRegistry, eng *wile.Engine) *RegistryDocProvider {
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

// Search returns entries whose name, doc, category, or keywords contain
// pattern (case-insensitive substring match). Results are sorted by name.
// Delegates to registry.SearchDoc for non-library results, then appends
// library results from the Engine's loaded and unloaded library methods.
//
// The library matching logic here mirrors registry.SearchDoc's
// searchLibraries/searchUnloadedExports functions but operates on
// wile.LibraryInfo instead of compilation.* types. This duplication
// is a structural consequence of decoupling repl/ from machine/compilation/.
func (p *RegistryDocProvider) Search(ctx context.Context, pattern string) []registry.DocSearchResult {
	// Get non-library results from the registry (passing nil for library params).
	q := registry.SearchDoc(p.reg, p.env(), nil, nil, pattern)

	// Append library results from the Engine.
	if p.eng != nil {
		lowerPattern := strings.ToLower(pattern)
		seen := values.NewStringSet(len(q))
		for _, r := range q {
			seen.Add(r.Name)
		}

		// Loaded libraries.
		loaded, _ := p.eng.LoadedLibraries()
		for _, lib := range loaded {
			dup := seen.ContainsOne(lib.Name)
			if dup {
				continue
			}
			if strings.Contains(strings.ToLower(lib.Name), lowerPattern) ||
				strings.Contains(strings.ToLower(lib.Description), lowerPattern) {
				seen.Add(lib.Name)
				q = append(q, registry.DocSearchResult{
					Name:     lib.Name,
					Doc:      lib.Description,
					Category: "library",
				})
			}
		}

		// Unloaded libraries.
		for _, lib := range p.eng.UnloadedLibraries(ctx) {
			dup := seen.ContainsOne(lib.Name)
			if dup {
				continue
			}
			if strings.Contains(strings.ToLower(lib.Name), lowerPattern) ||
				strings.Contains(strings.ToLower(lib.Description), lowerPattern) {
				seen.Add(lib.Name)
				q = append(q, registry.DocSearchResult{
					Name:     lib.Name,
					Doc:      lib.Description,
					Category: "library (not imported)",
				})
			}

			// Export-level match: check individual export names.
			for _, export := range lib.Exports {
				dup := seen.ContainsOne(export)
				if dup {
					continue
				}
				if !strings.Contains(strings.ToLower(export), lowerPattern) {
					continue
				}
				seen.Add(export)
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
		slices.SortFunc(q, func(a, b registry.DocSearchResult) int {
			return cmp.Compare(a.Name, b.Name)
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
	cats := values.NewStringSet(0)
	for _, pr := range p.reg.Primitives() {
		if pr.Spec.Category != "" {
			cats.Add(pr.Spec.Category)
		}
	}
	for _, r := range registry.NonPrimitiveDocs(p.reg) {
		if r.Category != "" {
			cats.Add(r.Category)
		}
	}

	return slices.Sorted(maps.Keys(cats))
}

// ByCategory returns entries in the named category, sorted by name.
func (p *RegistryDocProvider) ByCategory(category string) []registry.DocSearchResult {
	seen := values.NewStringSet(0)
	var results []registry.DocSearchResult

	byCategory := p.reg.PrimitivesByCategory()
	for _, pr := range byCategory[category] {
		seen.Add(pr.Spec.Name)
		results = append(results, registry.DocSearchResult{
			Name:     pr.Spec.Name,
			Doc:      pr.Spec.Doc,
			Category: pr.Spec.Category,
			Keywords: pr.Spec.Keywords,
		})
	}

	for _, r := range registry.NonPrimitiveDocs(p.reg) {
		dup := seen.ContainsOne(r.Name)
		if r.Category == category && !dup {
			seen.Add(r.Name)
			results = append(results, r)
		}
	}

	if len(results) == 0 {
		return nil
	}
	slices.SortFunc(results, func(a, b registry.DocSearchResult) int {
		return cmp.Compare(a.Name, b.Name)
	})
	return results
}
