package repl

import (
	"sort"
	"strings"

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

// LookupDoc returns documentation for the named primitive from the registry.
func (p *RegistryDocProvider) LookupDoc(name string) (DocInfo, bool) {
	pr, found := p.reg.FindPrimitive(name, 0)
	if !found {
		return DocInfo{}, false
	}
	return DocInfo{
		Doc:        pr.Spec.Doc,
		ParamNames: pr.Spec.ParamNames,
		Category:   pr.Spec.Category,
		ParamCount: pr.Spec.ParamCount,
		IsVariadic: pr.Spec.IsVariadic,
		ParamTypes: pr.Spec.ParamTypes,
		ReturnType: pr.Spec.ReturnType,
	}, true
}

// Search returns entries whose name, doc, or category contains pattern
// (case-insensitive substring match). Results are sorted by name.
func (p *RegistryDocProvider) Search(pattern string) []DocSearchResult {
	lowerPattern := strings.ToLower(pattern)
	var results []DocSearchResult
	for _, pr := range p.reg.Primitives() {
		if matchesPrimitive(pr.Spec, lowerPattern) {
			results = append(results, DocSearchResult{
				Name:     pr.Spec.Name,
				Doc:      pr.Spec.Doc,
				Category: pr.Spec.Category,
			})
		}
	}
	sort.Slice(results, func(i, j int) bool {
		return results[i].Name < results[j].Name
	})
	return results
}

// Categories returns sorted category names, excluding the empty-string category.
func (p *RegistryDocProvider) Categories() []string {
	byCategory := p.reg.PrimitivesByCategory()
	var categories []string
	for cat := range byCategory {
		if cat != "" {
			categories = append(categories, cat)
		}
	}
	sort.Strings(categories)
	return categories
}

// ByCategory returns entries in the named category, sorted by name.
func (p *RegistryDocProvider) ByCategory(category string) []DocSearchResult {
	byCategory := p.reg.PrimitivesByCategory()
	prims, ok := byCategory[category]
	if !ok {
		return nil
	}
	results := make([]DocSearchResult, len(prims))
	for i, pr := range prims {
		results[i] = DocSearchResult{
			Name:     pr.Spec.Name,
			Doc:      pr.Spec.Doc,
			Category: pr.Spec.Category,
		}
	}
	sort.Slice(results, func(i, j int) bool {
		return results[i].Name < results[j].Name
	})
	return results
}

// matchesPrimitive returns true if the spec's name, doc, or category
// contains the given lowercase pattern.
func matchesPrimitive(spec registry.PrimitiveSpec, pattern string) bool {
	return strings.Contains(strings.ToLower(spec.Name), pattern) ||
		strings.Contains(strings.ToLower(spec.Doc), pattern) ||
		strings.Contains(strings.ToLower(spec.Category), pattern)
}
