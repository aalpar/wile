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

import "sort"

// DiscoverAvailableLibraries returns all importable library names by
// combining filesystem discovery (via the resolver's LibraryEnumerator)
// with registry-known libraries (synthetic extension libraries).
// Returns a sorted, deduplicated list.
//
// If the resolver does not implement LibraryEnumerator, only registry
// libraries are returned. If reg is nil, only filesystem libraries are
// returned.
func DiscoverAvailableLibraries(resolver FileResolver, reg *LibraryRegistry) ([]LibraryName, error) {
	seen := make(map[string]bool)
	var result []LibraryName

	// Filesystem discovery via resolver chain.
	// A nil resolver is safe: the nil interface assertion returns (nil, false).
	// This happens when the environment has no file resolver configured.
	enumerator, ok := resolver.(LibraryEnumerator)
	if ok {
		libs, err := enumerator.EnumerateLibraries()
		if err != nil {
			return nil, err
		}
		for _, lib := range libs {
			key := lib.Key()
			if !seen[key] {
				seen[key] = true
				result = append(result, lib)
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

	sort.Slice(result, func(i, j int) bool {
		return result[i].Key() < result[j].Key()
	})
	return result, nil
}
