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

package wile

import (
	"context"
	"sort"

	"github.com/aalpar/wile/machine/compilation"
)

// LibraryInfo holds read-only metadata about a Scheme library.
type LibraryInfo struct {
	Name        string // Scheme representation, e.g. "(scheme base)"
	Description string
	SourceFile  string
	Exports     []string // sorted export names
}

// LookupLibrary returns info for a loaded library identified by its
// name parts (e.g., "scheme", "base"). Returns nil if not loaded.
func (p *Engine) LookupLibrary(parts ...string) *LibraryInfo {
	reg := p.libraryRegistry()
	if reg == nil {
		return nil
	}
	name := compilation.NewLibraryName(parts...)
	lib := reg.Lookup(name)
	if lib == nil {
		return nil
	}
	return compiledLibraryToInfo(lib)
}

// LoadedLibraries returns metadata for all currently loaded libraries,
// sorted by name.
func (p *Engine) LoadedLibraries() []*LibraryInfo {
	reg := p.libraryRegistry()
	if reg == nil {
		return nil
	}
	libs := reg.All()
	q := make([]*LibraryInfo, len(libs))
	for i, lib := range libs {
		q[i] = compiledLibraryToInfo(lib)
	}
	sort.Slice(q, func(i, j int) bool {
		return q[i].Name < q[j].Name
	})
	return q
}

// UnloadedLibraries returns metadata for libraries discoverable via the
// file resolver but not yet imported. Returns nil if no resolver is
// available. Thread-safe via lazy initialization with retry on failure.
func (p *Engine) UnloadedLibraries(ctx context.Context) []*LibraryInfo {
	idx := p.ensureExportIndex(ctx)
	if idx == nil {
		return nil
	}
	libReg := p.libraryRegistry()
	var q []*LibraryInfo
	for _, summary := range idx.Entries() {
		if libReg != nil && libReg.Lookup(summary.Name) != nil {
			continue
		}
		q = append(q, &LibraryInfo{
			Name:        summary.Name.SchemeString(),
			Description: summary.Description,
			SourceFile:  summary.SourceFile,
			Exports:     summary.Exports,
		})
	}
	return q
}

// libraryRegistry extracts the concrete LibraryRegistry from the
// environment, returning nil if unavailable.
func (p *Engine) libraryRegistry() *compilation.LibraryRegistry {
	regSearcher := p.env.LibraryRegistry()
	if regSearcher == nil {
		return nil
	}
	reg, ok := regSearcher.(*compilation.LibraryRegistry)
	if !ok {
		return nil
	}
	return reg
}

// ensureExportIndex lazily builds the library export index on first
// successful call. Retries on transient failures (context cancellation,
// slow filesystem). Permanent conditions (nil env, nil resolver) are
// marked as built to avoid repeated nil checks.
func (p *Engine) ensureExportIndex(ctx context.Context) *compilation.LibraryExportIndex {
	p.exportIndexMu.Lock()
	defer p.exportIndexMu.Unlock()
	if p.exportIndexBuilt {
		return p.exportIndex
	}
	resolver := p.env.FileResolver()
	if resolver == nil {
		p.exportIndexBuilt = true
		return nil
	}
	idx, err := compilation.BuildExportIndex(ctx, resolver, p.libraryRegistry())
	if err != nil {
		return nil // transient failure — retry next call
	}
	p.exportIndex = idx
	p.exportIndexBuilt = true
	return idx
}

func compiledLibraryToInfo(lib *compilation.CompiledLibrary) *LibraryInfo {
	exports := make([]string, 0, len(lib.Exports))
	for name := range lib.Exports {
		exports = append(exports, name)
	}
	sort.Strings(exports)
	return &LibraryInfo{
		Name:        lib.Name.SchemeString(),
		Description: lib.Description,
		SourceFile:  lib.SourceFile,
		Exports:     exports,
	}
}
