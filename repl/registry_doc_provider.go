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
	"sort"
	"strings"

	"github.com/aalpar/wile/docparse"
	"github.com/aalpar/wile/registry"
)

// RegistryDocProvider adapts a registry.Registry to the DocProvider interface.
type RegistryDocProvider struct {
	reg *registry.Registry
}

// NewRegistryDocProvider creates a DocProvider backed by the given registry.
func NewRegistryDocProvider(reg *registry.Registry) *RegistryDocProvider {
	return &RegistryDocProvider{
		reg: reg,
	}
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
			}, true
		}
	}
	return DocInfo{}, false
}

// Search returns entries whose name, doc, or category contains pattern
// (case-insensitive substring match). Results are sorted by name.
// Primitives always take precedence over binding specs and doc entries:
// if a name exists as a primitive, non-primitive entries with the same
// name are suppressed regardless of whether the primitive matched the pattern.
func (p *RegistryDocProvider) Search(pattern string) []DocSearchResult {
	lowerPattern := strings.ToLower(pattern)
	var results []DocSearchResult

	// Build complete set of primitive names so non-primitives with the same
	// name are always suppressed, even when the primitive doesn't match.
	prims := p.reg.Primitives()
	primNames := make(map[string]bool, len(prims))
	for _, pr := range prims {
		primNames[pr.Spec.Name] = true
		if matchesFields(pr.Spec.Name, pr.Spec.Doc, pr.Spec.Category, lowerPattern) {
			results = append(results, DocSearchResult{
				Name:     pr.Spec.Name,
				Doc:      pr.Spec.Doc,
				Category: pr.Spec.Category,
			})
		}
	}

	seen := make(map[string]bool)
	for _, r := range p.nonPrimitiveDocs() {
		if primNames[r.Name] || seen[r.Name] {
			continue
		}
		if matchesFields(r.Name, r.Doc, r.Category, lowerPattern) {
			seen[r.Name] = true
			results = append(results, r)
		}
	}

	sort.Slice(results, func(i, j int) bool {
		return results[i].Name < results[j].Name
	})
	return results
}

// Categories returns sorted category names, excluding the empty-string category.
func (p *RegistryDocProvider) Categories() []string {
	cats := make(map[string]bool)
	for _, pr := range p.reg.Primitives() {
		if pr.Spec.Category != "" {
			cats[pr.Spec.Category] = true
		}
	}
	for _, r := range p.nonPrimitiveDocs() {
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
func (p *RegistryDocProvider) ByCategory(category string) []DocSearchResult {
	seen := make(map[string]bool)
	var results []DocSearchResult

	byCategory := p.reg.PrimitivesByCategory()
	for _, pr := range byCategory[category] {
		seen[pr.Spec.Name] = true
		results = append(results, DocSearchResult{
			Name:     pr.Spec.Name,
			Doc:      pr.Spec.Doc,
			Category: pr.Spec.Category,
		})
	}

	for _, r := range p.nonPrimitiveDocs() {
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

// nonPrimitiveDocs returns doc search results from binding specs and doc entries.
// These are parsed via docparse to extract structured metadata.
func (p *RegistryDocProvider) nonPrimitiveDocs() []DocSearchResult {
	var results []DocSearchResult
	for _, bs := range p.reg.BindingSpecs() {
		if bs.Doc == "" {
			continue
		}
		parsed := docparse.ParseDocstring(bs.Doc)
		results = append(results, DocSearchResult{
			Name:     bs.Name,
			Doc:      parsed.Doc,
			Category: parsed.Category,
		})
	}
	for _, de := range p.reg.Docs() {
		parsed := docparse.ParseDocstring(de.Doc)
		results = append(results, DocSearchResult{
			Name:     de.Name,
			Doc:      parsed.Doc,
			Category: parsed.Category,
		})
	}
	return results
}

// matchesFields returns true if any of name, doc, or category
// contains the given lowercase pattern.
func matchesFields(name, doc, category, pattern string) bool {
	return strings.Contains(strings.ToLower(name), pattern) ||
		strings.Contains(strings.ToLower(doc), pattern) ||
		strings.Contains(strings.ToLower(category), pattern)
}
