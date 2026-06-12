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

// reclaimNode is one analyzable function in a compilation unit's call graph.
// Its structural capture facts are precomputed (Layer A predicates) and its
// call sites resolved to edges (the builder, frame_reclaim_build.go) before the
// fixpoint runs.
type reclaimNode struct {
	label             string        // diagnostic label (define/lambda name)
	referencesCapture bool          // body invokes a continuation-capturing operator
	createsEscaping   bool          // body creates a non-immediately-applied closure
	callees           []reclaimEdge // call sites that impose a capture constraint
}

// reclaimEdge is one resolved call site that constrains reclaimability. A call
// to a capture-safe primitive imposes no constraint and contributes no edge.
type reclaimEdge struct {
	// target is the resolved callee node, or nil if the operator did not
	// resolve to an analyzable function in this unit (⇒ unsafe).
	target *reclaimNode
	// immutable reports that the callee's binding is provably not set!-able.
	immutable bool
}

// mayCapture computes, for every node, whether it may expose a frame to a
// continuation. It is the negation of a greatest-fixpoint Safe predicate:
//
//	Safe(n) := ¬referencesCapture(n) ∧ ¬createsEscaping(n)
//	         ∧ ∀ edge e in n.callees: e.target≠nil ∧ e.immutable ∧ Safe(e.target)
//
// Start optimistic (all Safe) and knock out violators until stable. The
// co-inductive (greatest-fixpoint) formulation is what makes self- and mutual
// recursion converge: a self-recursive function over safe callees stays safe
// because its self-edge points at a still-Safe node. Monotone decreasing over
// a finite node set ⇒ it terminates.
//
// Every uncertainty resolves toward unsafe — the sound direction for a
// correctness-critical optimization. An unresolved callee (target==nil), a
// mutable callee, or any structural capture marks the node unsafe ⇒
// mayCapture true ⇒ the frame stays heap (status-quo leak). A false negative
// leaks (correct, slow); a false positive would corrupt — so we never default
// to safe.
func mayCapture(nodes []*reclaimNode) map[*reclaimNode]bool {
	safe := make(map[*reclaimNode]bool, len(nodes))
	for _, n := range nodes {
		safe[n] = true
	}

	for {
		changed := false
		for _, n := range nodes {
			if !safe[n] {
				continue // monotone: once unsafe, never reverts
			}
			if !nodeSafe(n, safe) {
				safe[n] = false
				changed = true
			}
		}
		if !changed {
			break
		}
	}

	out := make(map[*reclaimNode]bool, len(nodes))
	for _, n := range nodes {
		out[n] = !safe[n]
	}
	return out
}

// nodeSafe evaluates the Safe clause for n under the current safe assignment.
// An edge whose target is not present in safe (a node not passed to mayCapture)
// reads false, which is the conservative outcome — but the builder guarantees
// every non-nil edge target is a member of the node set.
func nodeSafe(n *reclaimNode, safe map[*reclaimNode]bool) bool {
	if n.referencesCapture || n.createsEscaping {
		return false
	}
	for _, e := range n.callees {
		if e.target == nil || !e.immutable || !safe[e.target] {
			return false
		}
	}
	return true
}

// frameReclaimable reports whether n's tail frames are safe to release. Phase 1
// uses the conservative all-callees form, so it is exactly ¬mayCapture(n). The
// tail-position-precise form (only n's non-tail callees must be non-capturing)
// is a documented later refinement; it can only ever turn MORE frames
// reclaimable, gated by the sibling plan's Phase 5 continuation suite.
func frameReclaimable(n *reclaimNode, verdict map[*reclaimNode]bool) bool {
	return !verdict[n]
}
