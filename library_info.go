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
	"errors"
	"sort"

	"github.com/aalpar/wile/machine/compilation"
	"github.com/aalpar/wile/werr"
)

// LibraryInfo holds read-only metadata about a Scheme library.
type LibraryInfo struct {
	Name        string // Scheme representation, e.g. "(scheme base)"
	Description string
	SourceFile  string
	Exports     []string // sorted export names
}

// LookupLibrary returns info for a loaded library identified by its
// name parts (e.g., "scheme", "base"). Returns (nil, nil) if no library
// registry is configured. Returns a non-nil error if the registry has
// an unexpected type.
func (p *Engine) LookupLibrary(parts ...string) (*LibraryInfo, error) {
	reg, err := p.libraryRegistry()
	if err != nil {
		return nil, err
	}
	if reg == nil {
		return nil, nil
	}
	name := compilation.NewLibraryName(parts...)
	lib := reg.Lookup(name)
	if lib == nil {
		return nil, nil
	}
	return compiledLibraryToInfo(lib), nil
}

// LoadedLibraries returns metadata for all currently loaded libraries,
// sorted by name. Returns (nil, nil) if no library registry is
// configured.
func (p *Engine) LoadedLibraries() ([]*LibraryInfo, error) {
	reg, err := p.libraryRegistry()
	if err != nil {
		return nil, err
	}
	if reg == nil {
		return nil, nil
	}
	libs := reg.All()
	q := make([]*LibraryInfo, len(libs))
	for i, lib := range libs {
		q[i] = compiledLibraryToInfo(lib)
	}
	sort.Slice(q, func(i, j int) bool {
		return q[i].Name < q[j].Name
	})
	return q, nil
}

// UnloadedLibraries returns metadata for libraries discoverable via the
// file resolver but not yet imported. Returns nil if no resolver is
// available. Thread-safe via lazy initialization with retry on failure.
func (p *Engine) UnloadedLibraries(ctx context.Context) []*LibraryInfo {
	idx := p.ensureExportIndex(ctx)
	if idx == nil {
		return nil
	}
	reg, _ := p.libraryRegistry()
	var q []*LibraryInfo
	for _, summary := range idx.Entries() {
		if reg != nil && reg.Lookup(summary.Name) != nil {
			continue
		}
		exports := append([]string(nil), summary.Exports...)
		sort.Strings(exports)
		q = append(q, &LibraryInfo{
			Name:        summary.Name.SchemeString(),
			Description: summary.Description,
			SourceFile:  summary.SourceFile,
			Exports:     exports,
		})
	}
	return q
}

// libraryRegistry extracts the concrete LibraryRegistry from the
// environment. Returns (nil, nil) if no registry is configured.
// Returns a non-nil error if the registry has an unexpected concrete
// type, consistent with AvailableLibraries.
func (p *Engine) libraryRegistry() (*compilation.LibraryRegistry, error) {
	regSearcher := p.env.LibraryRegistry()
	if regSearcher == nil {
		return nil, nil
	}
	reg, ok := regSearcher.(*compilation.LibraryRegistry)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryConfiguration,
			"libraryRegistry: unexpected type %T", regSearcher)
	}
	return reg, nil
}

// ensureExportIndex lazily builds the library export index on first
// successful call. Retries on context cancellation/deadline. Permanent
// failures (permissions, corrupt .sld, etc.) are marked as built to
// avoid repeated retries.
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
	reg, _ := p.libraryRegistry()
	idx, err := compilation.BuildExportIndex(ctx, resolver, reg)
	if err != nil {
		if errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded) {
			return nil // transient — retry next call
		}
		// Non-transient errors (malformed .sld, permissions, etc.):
		// use partial results since BuildExportIndex returns valid
		// entries alongside the error.
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
