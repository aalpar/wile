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
	"strconv"
	"sync"
	"testing"
)

// TestLibraryRegistryConcurrentLoad exercises the registry from multiple
// goroutines loading distinct libraries. Without synchronization the
// libraries/loading maps race and the runtime aborts with "concurrent map
// writes" (CC1). Run under -race to surface the data race even when no crash
// occurs. Distinct from TODO.md D2 (a binding.go race).
func TestLibraryRegistryConcurrentLoad(t *testing.T) {
	reg := NewLibraryRegistry()
	var wg sync.WaitGroup
	for i := range 8 {
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			name := NewLibraryName("conc", strconv.Itoa(i))
			reg.StartLoading(name)
			reg.IsLoading(name)
			_ = reg.Register(NewCompiledLibrary(name, nil))
			reg.FinishLoading(name)
		}(i)
	}
	wg.Wait()
}

// TestLibraryRegistryLookupOrClaim verifies the atomic check-and-claim:
// exactly one of N concurrent claimants for the same name wins the loading
// slot; the rest see either the cached library or a circular-dependency
// rejection (option (a): concurrent same-library load is reported as a
// circular dependency rather than blocking — see Task 1C).
func TestLibraryRegistryLookupOrClaim(t *testing.T) {
	reg := NewLibraryRegistry()
	name := NewLibraryName("conc", "shared")

	const n = 8
	var wg sync.WaitGroup
	var mu sync.Mutex
	claims := 0
	for range n {
		wg.Add(1)
		go func() {
			defer wg.Done()
			cached, claimed, err := reg.LookupOrClaim(name)
			if claimed {
				// Winner: register and release the slot.
				_ = reg.Register(NewCompiledLibrary(name, nil))
				reg.FinishLoading(name)
				mu.Lock()
				claims++
				mu.Unlock()
				return
			}
			// Loser: either the lib was already cached, or a circular-dependency
			// rejection while the winner held the slot. Both are acceptable; what
			// must never happen is a second successful claim.
			_ = cached
			_ = err
		}()
	}
	wg.Wait()

	if claims != 1 {
		t.Fatalf("want exactly 1 claim of the loading slot, got %d", claims)
	}
}
