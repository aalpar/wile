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

// TestFrameReclaimVerdict_ScopeCollisionUnderApproximates pins the resolution of
// the C5 blocker: ClassifyFrameReclaim must not report a continuation-capturing
// top-level define as frame-reclaimable because a same-Key sibling does not
// capture.
//
// The verdict map is keyed by Sym.Key, once justified by the claim that a
// GlobalEnvironmentFrame holds one slot per Key, so a Key uniquely names a global
// binding. Commit 8afeb66a made keys a map[values.Symbol][]int, and a60e32e1
// deleted the rename pass that had kept two hygiene-distinct same-name top-level
// binders differently NAMED. Both halves of that justification are now false, so
// the classifier detects the collision instead (reclaimNode.collided).
//
// Mechanism it guards: buildReclaimGraph's byName is last-wins and the fixpoint's
// node set is built from byName's VALUES, so an earlier define's node is dropped
// before mayCapture runs. Publishing the survivor's verdict under the shared Key
// would hand it to every define of that name — frameReuseForDefine
// (compile_define.go) reads it by bare Key, and the verdict alone is sufficient
// to return releaseReuse(); there is no second structural gate on the release
// path.
//
// Direction matters: the pre-fix behavior was NOT the safe over-approximation
// BindingRef's doc argues for (pkg/environment/binding_ref.go). It was the false
// positive frame_reclaim.go rules out by construction — "A false negative leaks
// (correct, slow); a false positive would corrupt — so we never default to safe."
//
// NOT ESTABLISHED: no program was ever known to actually corrupt. The unsoundness
// was proven at the classifier and releaseReuse() proven reachable from it, but
// whether OpReleaseEnvFrame corrupts a frame held live by a captured continuation
// was never demonstrated. Do not cite this test as a crash repro.
func TestFrameReclaimVerdict_ScopeCollisionUnderApproximates(t *testing.T) {
	ctx := context.Background()

	// Control 1: the capturing define alone must be non-reclaimable. Establishes
	// that the classifier does see the call/cc, so a true verdict below is a
	// collision artifact and not a failure to detect capture at all.
	solo, err := classifyCompiled(ctx, `(begin
		(define (f x) (call/cc (lambda (k) (k x)))))`, true)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, solo["f"], qt.IsFalse)

	// Control 2: the structurally safe define alone is reclaimable. Establishes
	// that `true` is the verdict actually being borrowed below.
	safe, err := classifyCompiled(ctx, `(begin
		(define (f x) (+ x 1)))`, true)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, safe["f"], qt.IsTrue)

	// The defect. Two hygiene-distinct top-level function defines of `f`: the
	// user's captures a continuation, the macro-introduced one does not. The
	// macro expansion is LAST, so it wins byName and publishes true under "f".
	//
	// Measured on HEAD: map[f:true], one entry.
	got, err := classifyCompiled(ctx, `(begin
		(define-syntax m (syntax-rules () ((_) (define (f x) (+ x 1)))))
		(define (f x) (call/cc (lambda (k) (k x))))
		(m))`, true)
	qt.Assert(t, err, qt.IsNil)

	// A Key naming two distinct bindings, one of which captures, must not report
	// reclaimable. Any sound fix satisfies this, whether by collision detection
	// or by making the domain scope-aware.
	qt.Assert(t, got["f"], qt.IsFalse)
}
