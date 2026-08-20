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

package wile_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// The VALUE half of frame-reclaim Phase C, which widened OpSelfTailCall from
// depth-0 self calls to self calls nested under `let` frames.
//
// The allocation probes in tail_call_alloc_test.go can only see the phase
// working. These see it working CORRECTLY, and that is the distinction the
// plan's kill criterion draws: "in-place rebind corrupting a continuation is the
// classic failure ... any suite failure ⇒ the depth accounting is wrong". A
// wrong pop count does not allocate differently. It rebinds the WRONG FRAME —
// writing the callee's arguments into an inner `let`'s slots and jumping to
// pc=0 — which produces a wrong answer or a wild read, never a slope change.
//
// Every program here is a loop whose answer depends on values read from the
// popped frames, so an off-by-one in either direction is visible in the result.

func nestedDepthEngine(t *testing.T) (*wile.Engine, context.Context) {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
		// Load-bearing: without the Stable stamp on same-unit defines and the
		// ambient primitives, no site arms at all and every row below passes
		// against the unoptimized compiler.
		wile.WithImmutableTopLevel(),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng, ctx
}

// TestNestedSelfTailCallValues drives loops whose recursive arguments are read
// out of the frames the op has to pop.
//
// ORDER IS THE PROPERTY. The arguments are evaluated while the let frames are
// still live and land on the eval stack; only then does the op unwind. An
// implementation that popped first would read `j` out of a frame it had already
// left — so a row like "one let, argument is the let binding" is an ordering
// assertion wearing an arithmetic disguise.
func TestNestedSelfTailCallValues(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			// One let. The argument IS the let binding, so the value must be read
			// before the pop.
			name: "one let, argument is the let binding",
			code: `(define (loop i n) (if (>= i n) i (let ((j (+ i 1))) (loop j n)))) (loop 0 1000)`,
			want: "1000",
		},
		{
			// Two lets. Pop count 2. Popping one would rebind the outer let's
			// single slot as if it were the parameter frame.
			name: "two nested lets",
			code: `(define (loop i n) (if (>= i n) i (let ((j (+ i 1))) (let ((k j)) (loop k n))))) (loop 0 1000)`,
			want: "1000",
		},
		{
			// Mixed depths in ONE procedure: the base branch's self call is at
			// depth 0, the other at depth 1. The pop count is per CALL SITE, not
			// per closure, so a design that armed one count for the whole
			// procedure gets exactly one of these two wrong.
			name: "two call sites at different depths",
			code: `(define (loop i n acc)
			         (if (>= i n)
			             acc
			             (if (even? i)
			                 (loop (+ i 1) n (+ acc 1))
			                 (let ((j (+ i 1))) (loop j n (+ acc 10))))))
			       (loop 0 10 0)`,
			want: "55",
		},
		{
			// The argument mixes a let binding with a PARAMETER, so both the
			// popped frame and the destination frame are read in one expression.
			name: "argument reads a let binding and a parameter",
			code: `(define (loop i n acc) (if (>= i n) acc (let ((step (* 2 i))) (loop (+ i 1) n (+ acc step)))))
			       (loop 0 10 0)`,
			want: "90",
		},
		{
			// A let* — three bindings, still ONE OpPushEnv, so the pop count must
			// be 1 and not 3. compileValidatedLet emits one frame per let form
			// regardless of binding count; a count keyed on bindings rather than
			// on frames fails here.
			name: "let* with three bindings is one frame",
			code: `(define (loop i n) (if (>= i n) i (let* ((a (+ i 1)) (b a) (c b)) (loop c n)))) (loop 0 1000)`,
			want: "1000",
		},
		{
			// letrec pushes a frame the same way. Included because its codegen arm
			// differs (delayed stores) while its frame discipline does not.
			name: "letrec body",
			code: `(define (loop i n) (if (>= i n) i (letrec ((j (+ i 1))) (loop j n)))) (loop 0 1000)`,
			want: "1000",
		},
		{
			// A let INSIDE the argument, with the self call at depth 0. The
			// argument's frame is pushed and popped by the let's own OpPopEnv
			// (non-tail position), so the op must pop NOTHING. The mirror image of
			// the rows above, and the one that fails if the counter is incremented
			// somewhere other than the body descent.
			name: "let in argument position pops nothing",
			code: `(define (loop i n) (if (>= i n) i (loop (let ((j (+ i 1))) j) n))) (loop 0 1000)`,
			want: "1000",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// A FRESH engine per row, not a shared one. Every row defines `loop`,
			// and WithImmutableTopLevel — which the arming needs — refuses the
			// second define of a Stable name. Sharing the engine turns every row
			// after the first into an error assertion about redefinition.
			eng, ctx := nestedDepthEngine(t)
			c := qt.New(t)
			v, err := eng.EvalMultiple(ctx, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(v.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestNestedSelfTailCallAcrossContinuationReentry is the depth>0 twin of
// TestReclaimableLoopAcrossContinuationReentry, and it is the row the kill
// criterion actually names.
//
// The loop runs on every re-entry of a captured continuation. If the pops or the
// in-place rebind disturbed a frame a continuation still reaches, the re-entered
// computation would resume against corrupted bindings and the arithmetic would
// change — an allocation probe would see nothing.
//
// The call/cc lives OUTSIDE the loop deliberately: one inside the body would
// refuse the arming (bodyReferencesCaptureOperator), so the test would pass with
// the optimization disabled. tag walks 0→1→2 across two re-entries, so visits=3,
// each pass sums 1..50 = 1275 through a let-wrapped self call, and the answer is
// 3*1275 = 3825.
func TestNestedSelfTailCallAcrossContinuationReentry(t *testing.T) {
	eng, ctx := nestedDepthEngine(t)
	const program = `
(define (accumulate i n acc)
  (if (>= i n) acc (let ((next (+ i 1))) (accumulate next n (+ acc next)))))
(let ((k #f) (visits 0))
  (let ((tag (call/cc (lambda (c) (set! k c) 0))))
    (set! visits (+ visits 1))
    (let ((s (accumulate 0 50 0)))
      (if (< tag 2)
          (k (+ tag 1))
          (* visits s)))))`
	c := qt.New(t)
	v, err := eng.EvalMultiple(ctx, program)
	c.Assert(err, qt.IsNil)
	c.Assert(v.SchemeString(), qt.Equals, "3825",
		qt.Commentf("a let-wrapped self-tail loop must survive continuation re-entry; "+
			"a corrupted re-entered frame changes this number, not the allocation count"))
}

// TestNestedSelfTailCallIsArmed is the non-vacuity guard for both tests above.
//
// Every value assertion here passes with Phase C reverted — that is what "the
// answer is still right" means. Without this, the whole file would be a suite
// that cannot see what it was written for.
func TestNestedSelfTailCallIsArmed(t *testing.T) {
	tpl := templateOf(t,
		"(define (loop i n) (if (>= i n) i (let ((j (+ i 1))) (loop j n))))", "loop")
	// The pop count is 0, not 1, since let-slot merging: the `let` allocates its
	// slot in the enclosing parameter frame and emits no OpPushEnv, so there is
	// no frame between the call site and the frame OpSelfTailCall rebinds. What
	// this guard is for is unchanged — an EMPTY result still means the value
	// tests above are measuring an unoptimized compiler.
	qt.Assert(t, selfTailPops(tpl), qt.DeepEquals, []int{0},
		qt.Commentf("the let-wrapped self call must compile to one OpSelfTailCall; "+
			"an empty result means the value tests above are measuring the "+
			"unoptimized compiler"))
}
