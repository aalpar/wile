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

// TestFrameReclaimVerdict_ScopeDistinctBindersGetSeparateVerdicts pins the
// scope-aware resolution of the frame-reclaim verdict domain (TODO "Frame-reclaim's
// verdict domain is name-keyed"): two hygiene-distinct top-level defines of one name
// each get their OWN reclamation verdict, so a structurally-safe binder recovers
// reclamation while a capturing binder of the same name stays denied.
//
// History (the C5 blocker): the verdict map was keyed by Sym.Key, once justified by
// the claim that a GlobalEnvironmentFrame holds one slot per Key, so a Key uniquely
// names a global binding. Commit 8afeb66a made keys a map[values.Symbol][]int and
// a60e32e1 deleted the rename pass that had kept two hygiene-distinct same-name
// top-level binders differently NAMED. Both halves of that justification became
// false, so the classifier first detected the collision and forced it unsafe
// (reclaimNode.collided) — sound, but conservative: BOTH binders forfeited
// reclamation. Keying the domain on the scope-discriminated binding identity (the
// *Binding env.GetBinding resolves) separates them, recovering the safe one.
//
// Direction matters: the collision the guard ruled out was the false positive
// frame_reclaim.go rejects by construction — "A false negative leaks (correct,
// slow); a false positive would corrupt — so we never default to safe." Binding
// keying keeps that soundness (the capturing binder is still denied) AND regains the
// precision the name-keyed collapse lost.
//
// NOT ESTABLISHED: no program was ever known to actually corrupt under the pre-guard
// false positive. The unsoundness was proven at the classifier and releaseReuse()
// proven reachable from it, but whether OpReleaseEnvFrame corrupts a frame held live
// by a captured continuation was never demonstrated. Do not cite this test as a
// crash repro.
func TestFrameReclaimVerdict_ScopeDistinctBindersGetSeparateVerdicts(t *testing.T) {
	ctx := context.Background()

	// Control 1: the capturing define alone is non-reclaimable. Establishes that the
	// classifier sees the call/cc, so a true verdict below is a real recovery and not
	// a failure to detect capture at all.
	solo, err := classifyCompiled(ctx, `(begin
		(define (f x) (call/cc (lambda (k) (k x)))))`, true)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, verdictsForName(solo, "f"), qt.DeepEquals, []bool{false})

	// Control 2: the structurally safe define alone is reclaimable. Establishes that
	// `true` is the verdict the safe binder recovers below.
	safe, err := classifyCompiled(ctx, `(begin
		(define (f x) (+ x 1)))`, true)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, verdictsForName(safe, "f"), qt.DeepEquals, []bool{true})

	// The recovery. Two hygiene-distinct top-level function defines of `f`: the user's
	// captures a continuation, the macro-introduced one does not. Under scope-keyed
	// global storage they are DISTINCT bindings, so the verdict domain keyed by
	// binding identity holds two entries for "f".
	got, err := classifyCompiled(ctx, `(begin
		(define-syntax m (syntax-rules () ((_) (define (f x) (+ x 1)))))
		(define (f x) (call/cc (lambda (k) (k x))))
		(m))`, true)
	qt.Assert(t, err, qt.IsNil)

	// Exactly one of the two is reclaimable: the safe macro f recovered (precision),
	// the capturing user f stays denied (soundness). A name-keyed domain collapses
	// these to a single `false` entry (len 1), so len==2 ∧ trues==1 passes only under
	// scope-aware keying — the discrimination this test exists to pin.
	verdicts := verdictsForName(got, "f")
	qt.Assert(t, len(verdicts), qt.Equals, 2)
	trues := 0
	for _, v := range verdicts {
		if v {
			trues++
		}
	}
	qt.Assert(t, trues, qt.Equals, 1)
}
