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
			reg.LookupClaimOrWait(name)
			reg.IsLoading(name)
			_ = reg.Register(NewCompiledLibrary(name, nil))
			reg.FinishLoading(name)
		}(i)
	}
	wg.Wait()
}

// TestLibraryRegistryLookupClaimOrWait verifies the atomic check-and-claim:
// exactly one of N concurrent claimants for the same name wins the loading
// slot; the rest either see the cached library (winner already registered) or
// are handed a wait latch (winner still loading). A concurrent same-library
// load is NEVER reported as a circular dependency — that is the whole point of
// the per-name latch: waiters block then read the cache rather than failing.
func TestLibraryRegistryLookupClaimOrWait(t *testing.T) {
	reg := NewLibraryRegistry()
	name := NewLibraryName("conc", "shared")

	const n = 8
	var wg sync.WaitGroup
	var mu sync.Mutex
	claims := 0
	cachedHits := 0
	waits := 0
	for range n {
		wg.Add(1)
		go func() {
			defer wg.Done()
			cached, claimed, wait := reg.LookupClaimOrWait(name)
			// Manual unlock (not defer) so the winner releases mu before the slow
			// Register/FinishLoading. Every branch must unlock exactly once. Keep
			// the in-lock assertion a t.Errorf, NOT t.Fatalf: Fatalf calls
			// runtime.Goexit with mu still held, deadlocking the siblings on Lock
			// and hanging wg.Wait.
			mu.Lock()
			switch {
			case claimed:
				claims++
				mu.Unlock()
				// Winner: register and release the slot (wakes any waiters).
				_ = reg.Register(NewCompiledLibrary(name, nil))
				reg.FinishLoading(name)
				return
			case cached != nil:
				cachedHits++
			default:
				// Loser: a latch to wait on, never an error.
				waits++
				if wait == nil {
					t.Errorf("non-claiming, non-cached caller got a nil wait latch")
				}
			}
			mu.Unlock()
		}()
	}
	wg.Wait()

	if claims != 1 {
		t.Fatalf("want exactly 1 claim of the loading slot, got %d", claims)
	}
	if claims+cachedHits+waits != n {
		t.Fatalf("outcomes do not sum to %d: claims=%d cached=%d waits=%d",
			n, claims, cachedHits, waits)
	}
}

// TestLibraryRegistryFinishLoadingClosesLatch pins the wake contract a waiting
// loader depends on: the latch handed to a waiter is closed by FinishLoading,
// and after the owner Registers, a re-consult returns the cached library
// without re-claiming the slot.
func TestLibraryRegistryFinishLoadingClosesLatch(t *testing.T) {
	reg := NewLibraryRegistry()
	name := NewLibraryName("conc", "latch")

	// Owner claims.
	cached, claimed, wait := reg.LookupClaimOrWait(name)
	if cached != nil || !claimed || wait != nil {
		t.Fatalf("first call: want claim, got cached=%v claimed=%v wait!=nil=%v", cached, claimed, wait != nil)
	}

	// A second caller gets a wait latch, not a claim, not an error.
	cached, claimed, wait = reg.LookupClaimOrWait(name)
	if claimed || cached != nil || wait == nil {
		t.Fatalf("second call: want wait latch, got cached=%v claimed=%v wait==nil=%v", cached, claimed, wait == nil)
	}

	// Latch is open until FinishLoading.
	select {
	case <-wait:
		t.Fatal("latch closed before FinishLoading")
	default:
	}

	lib := NewCompiledLibrary(name, nil)
	err := reg.Register(lib)
	if err != nil {
		t.Fatalf("Register: %v", err)
	}
	reg.FinishLoading(name)

	// Waiter wakes.
	select {
	case <-wait:
	default:
		t.Fatal("latch not closed after FinishLoading")
	}

	// Re-consult returns the cached library, no re-claim.
	cached, claimed, wait = reg.LookupClaimOrWait(name)
	if claimed {
		t.Fatal("re-consult re-claimed an already-registered library")
	}
	if wait != nil {
		t.Fatal("re-consult handed a wait latch for an already-registered library")
	}
	if cached != lib {
		t.Fatalf("want the cached library pointer, got %v", cached)
	}
}

// TestLibraryRegistryFailedLoadIsReclaimable pins the failed-owner path: when a
// loader claims the slot but its load FAILS (FinishLoading without Register —
// what LoadLibrary's unconditional `defer FinishLoading` does on any error), the
// name must not be poisoned. A subsequent claimant re-claims and retries rather
// than reading a closed latch or a phantom cache. Deterministic, no goroutines.
func TestLibraryRegistryFailedLoadIsReclaimable(t *testing.T) {
	reg := NewLibraryRegistry()
	name := NewLibraryName("conc", "failed")

	// Owner claims, then its load fails: FinishLoading WITHOUT Register.
	cached, claimed, wait := reg.LookupClaimOrWait(name)
	if cached != nil || !claimed || wait != nil {
		t.Fatalf("first claim: got cached=%v claimed=%v wait!=nil=%v", cached, claimed, wait != nil)
	}
	reg.FinishLoading(name)

	// The slot is free again: a re-consult re-claims (not cached, not waiting).
	cached, claimed, wait = reg.LookupClaimOrWait(name)
	if !claimed {
		t.Fatal("failed load poisoned the name: re-consult did not re-claim")
	}
	if cached != nil {
		t.Fatalf("failed load left a phantom cached library: %v", cached)
	}
	if wait != nil {
		t.Fatal("failed load left a dangling latch: re-consult got a wait channel")
	}
}
