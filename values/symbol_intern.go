// Copyright 2025 Aaron Alpar
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

package values

import "sync"

// globalSymbolInterns is the process-wide symbol interning table.
// Per R7RS §6.5: "Two symbols are identical (in the sense of eq?) if and only
// if their names are spelled the same way."
// Using a global table ensures this property holds regardless of which
// environment or library creates the symbol.
var globalSymbolInterns = make(map[Symbol]*Symbol)
var globalSymbolInternsMu sync.RWMutex

// InternSymbol returns the canonical interned version of the given symbol.
// If a symbol with the same name has been interned before, that pointer is returned.
// Otherwise, the symbol is added to the global intern table and returned.
// This ensures symbol identity (eq?) works correctly per R7RS §6.5.
//
// Deprecated: Use TopLevelEnvironment.InternSymbol() for per-instance symbol interning.
// This global function is kept for backward compatibility with legacy environments.
// New code should use TopLevelEnvironment to enable multiple isolated Wile VMs.
//
// This function is thread-safe.
func InternSymbol(s *Symbol) *Symbol {
	if s == nil {
		return nil
	}

	// Fast path: check if already interned with read lock
	globalSymbolInternsMu.RLock()
	v, ok := globalSymbolInterns[*s]
	if ok {
		globalSymbolInternsMu.RUnlock()
		return v
	}
	globalSymbolInternsMu.RUnlock()

	// Slow path: acquire write lock and intern
	globalSymbolInternsMu.Lock()
	defer globalSymbolInternsMu.Unlock()

	// Double-check after acquiring write lock (another goroutine may have interned it)
	v, ok = globalSymbolInterns[*s]
	if ok {
		return v
	}

	globalSymbolInterns[*s] = s
	return s
}

// InternSymbolByName creates and interns a symbol with the given name.
// This is a convenience function equivalent to InternSymbol(NewSymbol(name)).
//
// Deprecated: Use TopLevelEnvironment.InternSymbol() for per-instance symbol interning.
func InternSymbolByName(name string) *Symbol {
	return InternSymbol(NewSymbol(name))
}

// ResetSymbolInterns clears the global symbol intern table.
// This is intended for testing purposes only to ensure test isolation.
// Do not use in production code.
func ResetSymbolInterns() {
	globalSymbolInternsMu.Lock()
	defer globalSymbolInternsMu.Unlock()
	globalSymbolInterns = make(map[Symbol]*Symbol)
}

// SymbolInternCount returns the number of interned symbols.
// This is intended for testing and debugging purposes.
func SymbolInternCount() int {
	globalSymbolInternsMu.RLock()
	defer globalSymbolInternsMu.RUnlock()
	return len(globalSymbolInterns)
}
