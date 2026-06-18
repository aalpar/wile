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

package validate

import "testing"

func TestMayCapture_LeafAndSelfRecursion(t *testing.T) {
	// A leaf with no callees and no structural capture is safe.
	leaf := &reclaimNode{label: "leaf"}
	v := mayCapture([]*reclaimNode{leaf})
	if v[leaf] {
		t.Fatalf("a clean leaf must not mayCapture")
	}
	if !frameReclaimable(leaf, v) {
		t.Fatalf("a clean leaf must be frameReclaimable")
	}

	// Self-recursion over safe callees stays safe: the greatest fixpoint must
	// converge, not diverge to true on the self-edge.
	fib := &reclaimNode{label: "fib"}
	safePrim := &reclaimNode{label: "+"}
	fib.callees = []reclaimEdge{
		{target: safePrim, immutable: true},
		{target: fib, immutable: true}, // self-edge
	}
	v = mayCapture([]*reclaimNode{fib, safePrim})
	if v[fib] {
		t.Fatalf("self-recursive fib over safe callees must not mayCapture")
	}
	if frameReclaimable(fib, v) == v[fib] {
		t.Fatalf("frameReclaimable must be the negation of mayCapture")
	}
}

func TestMayCapture_AdversarialTable(t *testing.T) {
	prim := func(name string) *reclaimNode {
		return &reclaimNode{label: name}
	}

	tests := []struct {
		name     string
		build    func() (subject *reclaimNode, all []*reclaimNode)
		wantSafe bool // true ⇒ FrameReclaimable (¬mayCapture)
	}{
		{
			name: "positive: leaf arithmetic fn",
			build: func() (*reclaimNode, []*reclaimNode) {
				p := prim("*")
				sq := &reclaimNode{label: "sq", callees: []reclaimEdge{{target: p, immutable: true}}}
				return sq, []*reclaimNode{sq, p}
			},
			wantSafe: true,
		},
		{
			name: "positive: tail accumulator (self-recursion + safe prims)",
			build: func() (*reclaimNode, []*reclaimNode) {
				eq, sub, add := prim("="), prim("-"), prim("+")
				loop := &reclaimNode{label: "loop"}
				loop.callees = []reclaimEdge{
					{target: eq, immutable: true},
					{target: sub, immutable: true},
					{target: add, immutable: true},
					{target: loop, immutable: true},
				}
				return loop, []*reclaimNode{loop, eq, sub, add}
			},
			wantSafe: true,
		},
		{
			name: "positive: fib-shaped, all callees non-capturing (immutability stipulated)",
			build: func() (*reclaimNode, []*reclaimNode) {
				lt, sub, add := prim("<"), prim("-"), prim("+")
				fib := &reclaimNode{label: "fib"}
				fib.callees = []reclaimEdge{
					{target: lt, immutable: true},
					{target: sub, immutable: true},
					{target: add, immutable: true},
					{target: fib, immutable: true},
				}
				return fib, []*reclaimNode{fib, lt, sub, add}
			},
			wantSafe: true,
		},
		{
			name: "positive: mutual recursion over safe callees",
			build: func() (*reclaimNode, []*reclaimNode) {
				not := prim("not")
				even := &reclaimNode{label: "even?"}
				odd := &reclaimNode{label: "odd?"}
				even.callees = []reclaimEdge{{target: not, immutable: true}, {target: odd, immutable: true}}
				odd.callees = []reclaimEdge{{target: not, immutable: true}, {target: even, immutable: true}}
				return even, []*reclaimNode{even, odd, not}
			},
			wantSafe: true,
		},
		{
			name: "negative: body references call/cc",
			build: func() (*reclaimNode, []*reclaimNode) {
				f := &reclaimNode{label: "f", referencesCapture: true}
				return f, []*reclaimNode{f}
			},
			wantSafe: false,
		},
		{
			name: "negative: body makes an escaping lambda",
			build: func() (*reclaimNode, []*reclaimNode) {
				f := &reclaimNode{label: "f", createsEscaping: true}
				return f, []*reclaimNode{f}
			},
			wantSafe: false,
		},
		{
			name: "negative: call to a set!-mutated (mutable) binding",
			build: func() (*reclaimNode, []*reclaimNode) {
				g := prim("g")
				f := &reclaimNode{label: "f", callees: []reclaimEdge{{target: g, immutable: false}}}
				return f, []*reclaimNode{f, g}
			},
			wantSafe: false,
		},
		{
			name: "negative: call to an unresolved binding",
			build: func() (*reclaimNode, []*reclaimNode) {
				f := &reclaimNode{label: "f", callees: []reclaimEdge{{target: nil, immutable: true}}}
				return f, []*reclaimNode{f}
			},
			wantSafe: false,
		},
		{
			name: "negative: safe-looking caller of a capturing callee (transitivity)",
			build: func() (*reclaimNode, []*reclaimNode) {
				capturer := &reclaimNode{label: "capturer", referencesCapture: true}
				caller := &reclaimNode{label: "caller", callees: []reclaimEdge{{target: capturer, immutable: true}}}
				return caller, []*reclaimNode{caller, capturer}
			},
			wantSafe: false,
		},
		{
			name: "negative L0: (let loop () (helper) (loop)) with capturing helper",
			build: func() (*reclaimNode, []*reclaimNode) {
				helper := &reclaimNode{label: "helper", referencesCapture: true}
				loop := &reclaimNode{label: "loop"}
				loop.callees = []reclaimEdge{
					{target: helper, immutable: true}, // non-tail (helper) — pins the loop frame
					{target: loop, immutable: true},   // tail (loop)
				}
				return loop, []*reclaimNode{loop, helper}
			},
			wantSafe: false,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			subject, all := tc.build()
			v := mayCapture(all)
			got := frameReclaimable(subject, v)
			if got != tc.wantSafe {
				t.Fatalf("frameReclaimable(%s) = %v, want %v", subject.label, got, tc.wantSafe)
			}
		})
	}
}
