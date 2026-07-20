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

	"github.com/aalpar/wile/pkg/machine"
)

// letBodyReleasesFrame compiles prog through the real pipeline and reports
// whether the closure bound to name emits OpReleaseEnvFrame.
//
// The assertion is on the instruction stream, not on the program's answer: a
// false-positive reclaim corrupts a frame only once a captured continuation is
// re-entered after the frame is recycled, which no known program observes
// reliably. The subsystem's own invariant (pkg/internal/validate/frame_reclaim.go)
// treats the emitted opcode itself as the defect.
func letBodyReleasesFrame(ctx context.Context, t *testing.T, prog string, name string) bool {
	t.Helper()

	eng, err := newBenchmarkEngine(ctx, true)
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, prog)
	qt.Assert(t, err, qt.IsNil)

	v, ok := eng.Get(name)
	qt.Assert(t, ok, qt.IsTrue)
	closure, ok := v.Internal().(*machine.MachineClosure)
	qt.Assert(t, ok, qt.IsTrue)

	for _, instr := range closure.Template().Code() {
		if instr.Op == machine.OpReleaseEnvFrame {
			return true
		}
	}
	return false
}

// TestFrameReclaimVerdict_DoesNotLeakIntoLetBody pins N1: the interprocedural
// frame-reclaim verdict is scoped to the top-level defines of the compiled unit,
// and nothing below top level may read it.
//
// CompileValidatedLet compiles the let body on the SAME compiler (compile_let.go
// swaps p.env and calls p.compileValidatedSequence with no child compiler), so
// p.frameReclaimVerdict stays live inside the body. An internal define reaches
// frameReuseForDefine, which reads the map by bare Sym.Key — and collects the
// TOP-LEVEL binding's verdict when the names collide.
//
// The classifier cannot defend against this: collectTopLevelDefines
// (frame_reclaim_build.go) never visits internal defines, so the collision is
// invisible to reclaimNode.collided, which closes the same-Key hole for
// top-level binders only.
//
// Direction matters, as in TestFrameReclaimVerdict_ScopeCollisionUnderApproximates:
// this is the false positive frame_reclaim.go rules out by construction, not the
// safe over-approximation. The body captures a continuation, so it is
// unconditionally NOT releasable, whatever a same-named top-level sibling does.
func TestFrameReclaimVerdict_DoesNotLeakIntoLetBody(t *testing.T) {
	ctx := context.Background()

	// Control 1: the leaked value is genuinely `true`. Without this the test
	// could pass on a unit where the top-level f is not reclaimable either, and
	// prove nothing about the boundary.
	verdict, err := classifyCompiled(ctx, `(begin
		(define (f n) (+ n 1)))`, true)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, verdict["f"], qt.IsTrue)

	// All four binding forms share one compile path (CompileValidatedLet switches
	// on v.Kind), and that path holds the only p.env swap on a live compiler in the
	// package — every other body compile builds a child continuation, where the map
	// is nil. Covering the four kinds pins that reading of the code: a later
	// per-form fix that patched `let` alone would fail here.
	// Subtests, not a bare loop: qt.Assert is fatal, so a loop would stop at the
	// first binder and leave the other three unproven — including against the
	// mutation run that establishes the test discriminates at all.
	for _, binder := range []string{"let", "let*", "letrec", "letrec*"} {
		t.Run(binder, func(t *testing.T) {
			// Control 2: a capturing internal define whose name collides with
			// NOTHING at top level is not released. Establishes that the body
			// predicate (BodyIsFrameReleasable) rejects a call/cc body on its own,
			// so a release below is attributable to the leaked verdict, not the body.
			uncollided := letBodyReleasesFrame(ctx, t, `(begin
				(define (f n) (+ n 1))
				(define g (`+binder+` ((x 1)) (define (h n) (call/cc (lambda (k) (k n)))) h)))`, "g")
			qt.Assert(t, uncollided, qt.IsFalse, qt.Commentf("control: uncollided body released"))

			// The defect: identical body, renamed to collide with the reclaimable
			// top-level f. Measured on 4f73936d: ReleaseEnvFrame at pc 5,
			// immediately before the call/cc PullApply.
			collided := letBodyReleasesFrame(ctx, t, `(begin
				(define (f n) (+ n 1))
				(define g (`+binder+` ((x 1)) (define (f n) (call/cc (lambda (k) (k n)))) f)))`, "g")
			qt.Assert(t, collided, qt.IsFalse, qt.Commentf("top-level verdict leaked into body"))
		})
	}
}
