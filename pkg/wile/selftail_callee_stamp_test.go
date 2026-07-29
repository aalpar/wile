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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/wile"
)

// selfTailSites counts OpSelfTailCall across a template AND every nested template
// reachable through its literals. A named let compiles its loop lambda into a
// child template pushed as a literal (PushLiteral + MakeClosure), so counting only
// the top template would report zero for every loop-shaped procedure and make each
// assertion below pass or fail for the wrong reason.
func selfTailSites(tpl *machine.NativeTemplate) int {
	n := 0
	for _, instr := range tpl.Code() {
		if instr.Op == machine.OpSelfTailCall {
			n++
		}
	}
	for _, lit := range tpl.Literals() {
		sub, ok := lit.(*machine.NativeTemplate)
		if !ok {
			continue
		}
		n += selfTailSites(sub)
	}
	return n
}

// TestSelfTailCall_CalleeStampFlipsAnEnclosingLoop confirms the mechanism behind
// the one benchmark that converted the A-local capture-safe rule into wall-clock
// time: examples/benchmarks/primes.scm, measured at -4.94% over eight interleaved
// A/B rounds (never once positive) while its ClassifyFrameReclaim verdict stayed
// at 0. The frame-reclaim classifier is therefore NOT the consumer that paid.
//
// The claim under test is narrower and is about a DIFFERENT consumer of the same
// stamp: bodyCalleesAllCaptureSafe resolves a non-tail callee through
// env.GetBinding and demands IsCaptureSafe(), so a loop whose body calls a helper
// becomes LetBindingSelfTailReusable exactly when that HELPER is provable — the
// stamp travels from the callee to the caller's loop, one edge up.
//
// primes.scm is that shape: is-prime? wraps a named let, which before A-local made
// is-prime? itself unprovable (its loop operator was lexically bound, and a bound
// operator was refused outright). primes-upto's loop calls is-prime? in non-tail
// position, so it inherited the refusal and allocated a binding frame per
// iteration across ~100k iterations.
//
// The three cases below are a discriminator, not three spellings of one check:
// the loop shape is held constant and only the callee's provability varies, so a
// pass cannot be explained by "named lets get OpSelfTailCall".
func TestSelfTailCall_CalleeStampFlipsAnEnclosingLoop(t *testing.T) {
	loopSitesOf := func(t *testing.T, src, name string) int {
		t.Helper()
		ctx := context.Background()
		eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
		qt.Assert(t, err, qt.IsNil)
		t.Cleanup(func() {
			_ = eng.Close()
		})
		_, err = eng.EvalMultiple(ctx, src)
		qt.Assert(t, err, qt.IsNil)

		v, ok := eng.Get(name)
		qt.Assert(t, ok, qt.IsTrue)
		closure, ok := v.Internal().(*machine.MachineClosure)
		qt.Assert(t, ok, qt.IsTrue)
		return selfTailSites(closure.Template())
	}

	// The benchmark's own two procedures, verbatim in shape.
	const primes = `
		(begin
		  (define (is-prime? n)
		    (if (< n 2)
		        #f
		        (let loop ((i 2))
		          (cond ((> (* i i) n) #t)
		                ((= (modulo n i) 0) #f)
		                (else (loop (+ i 1)))))))
		  (define (primes-upto n)
		    (let loop ((i 2) (result '()))
		      (if (> i n)
		          (reverse result)
		          (loop (+ i 1)
		                (if (is-prime? i)
		                    (cons i result)
		                    result))))))
	`

	// Control: is-prime?'s OWN inner loop was reusable in both arms. Its self-call
	// operator is not shadowed by the shadow set (which is seeded with the lambda's
	// parameters only), so it always took the name==selfName path, never the
	// locally-bound path A-local changed. If this were 0, the A-local decomposition
	// in the plan would be wrong about which loop moved.
	inner := loopSitesOf(t, primes, "is-prime?")
	qt.Assert(t, inner, qt.Equals, 1,
		qt.Commentf("is-prime?'s inner named let must emit OpSelfTailCall — it did so "+
			"before A-local too, and is the control that keeps the case below from "+
			"being explained by the loop shape alone"))

	// The finding: primes-upto's loop reuses its frame, and can only do so because
	// is-prime? resolves as capture-safe. This is the site that pays the -4.94%.
	outer := loopSitesOf(t, primes, "primes-upto")
	qt.Assert(t, outer, qt.Equals, 1,
		qt.Commentf("primes-upto's loop calls is-prime? in non-tail position; it is "+
			"self-tail-reusable only if that callee is stamped capture-safe. Before "+
			"A-local is-prime? was unprovable (its own loop operator was lexically "+
			"bound and refused), so this was 0 and the loop allocated a binding frame "+
			"per iteration"))

	// Negative: hold the loop shape fixed and make the callee unprovable. slow?
	// calls a procedure-invoking callee (map applies an unknown callback, so it can
	// capture), which denies slow? the stamp and must deny the enclosing loop the
	// reuse. Without this, the assertion above is satisfied by any change that arms
	// OpSelfTailCall unconditionally for named lets.
	const unprovable = `
		(begin
		  (define (slow? n)
		    (pair? (map (lambda (x) (* x n)) (list 1 2 3))))
		  (define (count-upto n)
		    (let loop ((i 2) (result '()))
		      (if (> i n)
		          (reverse result)
		          (loop (+ i 1)
		                (if (slow? i)
		                    (cons i result)
		                    result))))))
	`
	denied := loopSitesOf(t, unprovable, "count-upto")
	qt.Assert(t, denied, qt.Equals, 0,
		qt.Commentf("a loop whose non-tail callee is NOT provably capture-safe must "+
			"not reuse its frame: the callee could capture the continuation that pins "+
			"it. A-local widened which callees are provable, it did not remove the "+
			"requirement"))
}
