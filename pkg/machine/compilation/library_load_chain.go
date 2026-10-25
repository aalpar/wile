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
	"context"
	"slices"
)

// ctxChain is an immutable singly-linked list carried on a context.Context
// under one key, for state that belongs to one synchronous causal chain: the
// libraries being loaded (loadChainKey) and the files being included
// (includeChainKey). Pushing prepends without mutating the parent's node, so
// sibling branches and concurrent goroutines never observe each other's
// entries, and unwinding is implicit (the parent frame keeps its own shorter
// context).
type ctxChain[T any] struct {
	v    T
	prev *ctxChain[T]
}

// pushCtxChain returns a context whose chain under key is ctx's with v added
// innermost.
func pushCtxChain[T any](ctx context.Context, key any, v T) context.Context {
	prev, _ := ctx.Value(key).(*ctxChain[T])
	return context.WithValue(ctx, key, &ctxChain[T]{v: v, prev: prev})
}

// ctxChainValues returns the entries of ctx's chain under key, outermost first.
func ctxChainValues[T any](ctx context.Context, key any) []T {
	var q []T
	node, _ := ctx.Value(key).(*ctxChain[T])
	for ; node != nil; node = node.prev {
		q = append(q, node.v)
	}
	slices.Reverse(q)
	return q
}

// The load chain is the set of libraries currently being loaded on one
// synchronous load chain. context.Context threads as a plain Go value down the
// import-resolution call chain (LoadLibrary → compile/expand → resolveImportSet
// → LoadLibrary) but does NOT cross the SRFI-18 goroutine boundary: each thread
// sub-context starts from its own context (see machine.CaptureSubContextParams).
// That property is what lets the chain distinguish the two cases the registry's
// loading map cannot:
//
//   - re-entry of a name already on THIS chain  ⇒ a genuine import cycle (A→B→A),
//     all on one goroutine — must error.
//   - a name being loaded by ANOTHER goroutine (in the registry's loading map but
//     not on this chain) ⇒ a concurrent duplicate load — must wait, not error.
//
// loadChainKey is its context key. A zero-value of an unexported type cannot
// collide with any other package's key (matching the context-key idiom in
// security/context.go).
type loadChainKey struct{}

// withLoadChain returns a context that records name as being loaded on the
// current chain, in addition to whatever the parent context already carried.
func withLoadChain(ctx context.Context, name LibraryName) context.Context {
	return pushCtxChain(ctx, loadChainKey{}, name.Key())
}

// loadChainContains reports whether name is already being loaded on the current
// chain — i.e. a synchronous re-entry, which is a circular dependency.
func loadChainContains(ctx context.Context, name LibraryName) bool {
	return slices.Contains(ctxChainValues[string](ctx, loadChainKey{}), name.Key())
}
