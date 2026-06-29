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
	"testing"
)

// TestLoadChainContains exercises the ctx-borne load chain directly. Without
// this, a regression in the chain only surfaces transitively through the
// single-thread cycle tests, where it manifests as a multi-minute deadlock
// (the re-entrant load waits on a latch only it can close) rather than a clean
// assertion failure.
func TestLoadChainContains(t *testing.T) {
	a := NewLibraryName("test", "a")
	b := NewLibraryName("test", "b")
	c := NewLibraryName("test", "c")

	// Empty chain contains nothing.
	if loadChainContains(context.Background(), a) {
		t.Fatal("empty chain reported containing a")
	}

	// Two nested entries: both are present, an unrelated name is not.
	ctx := withLoadChain(withLoadChain(context.Background(), a), b)
	if !loadChainContains(ctx, a) {
		t.Fatal("chain {a,b} did not contain a (prev-walk broken)")
	}
	if !loadChainContains(ctx, b) {
		t.Fatal("chain {a,b} did not contain b (head missing)")
	}
	if loadChainContains(ctx, c) {
		t.Fatal("chain {a,b} reported containing unrelated c")
	}
}

// TestLoadChainForkIsolation pins the immutability/structural-sharing property:
// a name added on one fork is not visible on a sibling fork. This is the
// property that lets concurrent goroutines (and sibling imports) carry
// independent chains without observing each other's entries.
func TestLoadChainForkIsolation(t *testing.T) {
	a := NewLibraryName("test", "a")
	b := NewLibraryName("test", "b")

	base := withLoadChain(context.Background(), a)
	fork1 := withLoadChain(base, b)

	// b is on fork1 but must NOT leak back onto base (the sibling/parent view).
	if !loadChainContains(fork1, b) {
		t.Fatal("fork1 did not contain its own entry b")
	}
	if loadChainContains(base, b) {
		t.Fatal("entry b leaked from fork1 onto the parent context (chain is mutated, not prepended)")
	}
	if !loadChainContains(base, a) {
		t.Fatal("parent context lost its own entry a")
	}
}
