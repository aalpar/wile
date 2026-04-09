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
	"sync"

	"github.com/aalpar/wile/docparse"
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine/compilation"
	"github.com/aalpar/wile/registry"
)

// RegistryDocProvider adapts a registry.Registry to the DocProvider interface.
type RegistryDocProvider struct {
	reg *registry.Registry
	env *environment.EnvironmentFrame

	// Lazy export index — built on first Search() or UnloadedLibraries() call.
	// Uses mutex + flag instead of sync.Once so transient failures (context
	// cancellation, slow filesystem) can retry on the next call.
	indexMu     sync.Mutex
	indexBuilt  bool
	exportIndex *compilation.LibraryExportIndex
}

// NewRegistryDocProvider creates a DocProvider backed by the given registry.
// env may be nil; when non-nil, Search includes environment bindings and
// loaded libraries. The library registry is read dynamically from the
// environment on each call to ensure libraries loaded after construction
// are visible.
func NewRegistryDocProvider(reg *registry.Registry, env *environment.EnvironmentFrame) *RegistryDocProvider {
	return &RegistryDocProvider{
		reg: reg,
		env: env,
	}
}

// libraryRegistry returns the live library registry from the environment.
// Returns nil if the environment is nil or the registry is not available.
func (p *RegistryDocProvider) libraryRegistry() *compilation.LibraryRegistry {
	return registry.ExtractLibraryRegistry(p.env)
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

// ensureExportIndex builds the LibraryExportIndex on first successful call.
// Retries on subsequent calls if a previous attempt failed (e.g., context
// cancellation, slow filesystem). Permanent conditions (nil env, nil resolver)
// are marked as built to avoid repeated nil checks. Safe for concurrent use.
func (p *RegistryDocProvider) ensureExportIndex(ctx context.Context) {
	p.indexMu.Lock()
	defer p.indexMu.Unlock()
	if p.indexBuilt {
		return
	}
	if p.env == nil {
		p.indexBuilt = true
		return
	}
	resolver := p.env.FileResolver()
	if resolver == nil {
		p.indexBuilt = true
		return
	}
	idx, err := compilation.BuildExportIndex(ctx, resolver, p.libraryRegistry())
	if err != nil {
		return // transient failure — retry on next call
	}
	p.exportIndex = idx
	p.indexBuilt = true
}

// Search returns entries whose name, doc, or category contains pattern
// (case-insensitive substring match). Results are sorted by name.
// Delegates to registry.SearchDoc for unified search across all sources.
// On first call, lazily builds a LibraryExportIndex so unloaded library
// exports are discoverable via apropos.
func (p *RegistryDocProvider) Search(ctx context.Context, pattern string) []registry.DocSearchResult {
	p.ensureExportIndex(ctx)
	return registry.SearchDoc(p.reg, p.env, p.libraryRegistry(), p.exportIndex, pattern)
}

// UnloadedLibraries returns summaries of libraries that are discoverable via
// the file resolver but not yet imported. Returns nil if no export index is
// available. Libraries already present in the library registry are excluded.
func (p *RegistryDocProvider) UnloadedLibraries(ctx context.Context) []*compilation.LibrarySummary {
	p.ensureExportIndex(ctx)
	if p.exportIndex == nil {
		return nil
	}
	libReg := p.libraryRegistry()
	var q []*compilation.LibrarySummary
	for _, summary := range p.exportIndex.Entries() {
		if libReg != nil && libReg.Lookup(summary.Name) != nil {
			continue
		}
		q = append(q, summary)
	}
	return q
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
