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

import "context"

// loadChain tracks the set of libraries currently being loaded on one
// synchronous load chain. It is carried in context.Context, which threads as a
// plain Go value down the import-resolution call chain
// (LoadLibrary → compile/expand → resolveImportSet → LoadLibrary) but does NOT
// cross the SRFI-18 goroutine boundary: each thread sub-context starts from its
// own context (see machine.CaptureSubContextParams). That property is what lets
// the chain distinguish the two cases the registry's loading map cannot:
//
//   - re-entry of a name already on THIS chain  ⇒ a genuine import cycle (A→B→A),
//     all on one goroutine — must error.
//   - a name being loaded by ANOTHER goroutine (in the registry's loading map but
//     not on this chain) ⇒ a concurrent duplicate load — must wait, not error.
//
// The chain is an immutable singly-linked list: withLoadChain prepends without
// mutating the parent's node, so sibling imports and concurrent goroutines never
// observe each other's entries, and unwinding is implicit (the parent frame
// keeps its own shorter context).
type loadChain struct {
	name string
	prev *loadChain
}

// loadChainKey is the context key for the load chain. A zero-value of an
// unexported type cannot collide with any other package's key (matching the
// context-key idiom in security/context.go).
type loadChainKey struct{}

// withLoadChain returns a context that records name as being loaded on the
// current chain, in addition to whatever the parent context already carried.
func withLoadChain(ctx context.Context, name LibraryName) context.Context {
	prev, _ := ctx.Value(loadChainKey{}).(*loadChain)
	return context.WithValue(ctx, loadChainKey{}, &loadChain{name: name.Key(), prev: prev})
}

// loadChainContains reports whether name is already being loaded on the current
// chain — i.e. a synchronous re-entry, which is a circular dependency.
func loadChainContains(ctx context.Context, name LibraryName) bool {
	key := name.Key()
	node, _ := ctx.Value(loadChainKey{}).(*loadChain)
	for node != nil {
		if node.name == key {
			return true
		}
		node = node.prev
	}
	return false
}
