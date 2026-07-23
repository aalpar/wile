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

package wile

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestFrameReclaimVerdict_NonFunctionSameNameShadowStaysUnreclaimable is the
// attempted reproduction of the "function-node-only" gap (crosscheck errors lens,
// Finding 2): resolveNodeByScopes ranks only top-level FUNCTION defines, whereas the
// VM's env.GetBinding ranks ALL bindings of a name — value-form defines and imports
// included. If a same-name VALUE binding carries a strictly larger (more specific)
// scope set than the function node, resolveNodeByScopes returns the safe function
// node while GetBinding would dispatch the call to the more-specific value binding —
// in principle a false-positive reclaim (frame_reclaim.go: a false positive corrupts).
//
// The gap in resolveNodeByScopes is real, but the false positive is currently
// UNREACHABLE, masked one layer up: finalizeStability (errors.go) keys defined-once
// by BARE name (definedKeyCount), so the second same-name define — the capturing
// value f — drives definedKeyCount["f"] to 2 and forfeits the function f's
// StableInUnit. The function node's rebindStable is then false, so a caller's edge to
// it is MUTABLE (unsafe) whatever node resolveNodeByScopes picked. This test pins
// that masking and its load-bearing control, and stands as the tripwire: if a future
// change makes definedKeyCount scope-keyed (to sharpen StableInUnit), the bug
// assertion below flips to true and this test goes RED — signalling Finding 2's gap
// has reopened and resolveNodeByScopes must then rank non-function bindings too.
func TestFrameReclaimVerdict_NonFunctionSameNameShadowStaysUnreclaimable(t *testing.T) {
	ctx := context.Background()

	// A safe top-level FUNCTION f at {}, a capturing VALUE f introduced by the same
	// macro at {a}, and a caller referencing f at {a}. caller's f-reference is
	// hygiene-scoped to {a}, which GetBinding would resolve to the more-specific
	// capturing VALUE f — but resolveNodeByScopes only sees the function f.
	bug := `(begin
		(define (f x) (+ x 1))
		(define-syntax m (syntax-rules ()
			((_) (begin
				(define f (lambda (x) (call/cc (lambda (k) (k x)))))
				(define (caller n) (f n))))))
		(m))`
	got, err := classifyCompiled(ctx, bug, true)
	qt.Assert(t, err, qt.IsNil)

	// The capturing VALUE f is not a function, so it never becomes a graph node: only
	// the safe function f carries a verdict for the name. This IS the gap — the node
	// set excludes the very binding the VM would call.
	qt.Assert(t, verdictsForName(got, "f"), qt.DeepEquals, []bool{true})

	// caller is NOT reclaimable — the false positive does not occur. The denial comes
	// from the name-keyed defined-once gate (definedKeyCount["f"] == 2 ⇒ function f
	// not StableInUnit ⇒ caller edge mutable), NOT from resolveNodeByScopes.
	qt.Assert(t, verdictsForName(got, "caller"), qt.DeepEquals, []bool{false})

	// Control: the SAME caller with no same-name value f. Now f is defined once,
	// StableInUnit holds, the caller edge is immutable, and caller IS reclaimable —
	// proving the denial above is the name-keyed gate at work, not caller being
	// structurally unsafe.
	control := `(begin
		(define (f x) (+ x 1))
		(define-syntax m (syntax-rules ()
			((_) (define (caller n) (f n)))))
		(m))`
	got2, err := classifyCompiled(ctx, control, true)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, verdictsForName(got2, "caller"), qt.DeepEquals, []bool{true})
}
