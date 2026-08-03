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

package compilation

import (
	"cmp"
	"errors"
	"slices"

	"github.com/aalpar/wile/pkg/machine/compilation/resolver"
)

// DiscoverAvailableLibraries returns all importable library names by
// combining filesystem discovery (via the resolver's FileEnumerator)
// with registry-known libraries (synthetic extension libraries).
// Returns a sorted, deduplicated list.
//
// If the resolver does not implement FileEnumerator, only registry
// libraries are returned. If reg is nil, only filesystem libraries are
// returned.
//
// Two error shapes: an EnumerateFiles failure aborts with (nil, err),
// discarding any partial result; per-path FilePathToLibraryName failures
// are joined and returned alongside a complete list.
func DiscoverAvailableLibraries(res FileResolver, reg *LibraryRegistry) ([]LibraryName, error) {
	seen := make(map[string]bool)
	var result []LibraryName

	// Filesystem discovery via resolver chain.
	// A nil resolver is safe: the nil interface assertion returns (nil, false).
	// This happens when the environment has no file resolver configured.
	var pathErrs []error
	fileEnum, ok := res.(resolver.FileEnumerator)
	if ok {
		files, err := fileEnum.EnumerateFiles()
		if err != nil {
			return nil, err
		}
		for _, path := range files {
			name, nameErr := FilePathToLibraryName(path)
			if nameErr != nil {
				pathErrs = append(pathErrs, nameErr)
				continue
			}
			key := name.Key()
			if !seen[key] {
				seen[key] = true
				result = append(result, name)
			}
		}
	}

	// Registry-known libraries (synthetic extensions).
	if reg != nil {
		for _, name := range reg.AllNames() {
			key := name.Key()
			if !seen[key] {
				seen[key] = true
				result = append(result, name)
			}
		}
	}

	slices.SortFunc(result, func(a, b LibraryName) int {
		return cmp.Compare(a.Key(), b.Key())
	})
	return result, errors.Join(pathErrs...)
}
