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
	"fmt"
	"testing"

	"github.com/aalpar/wile/pkg/stdlib"
)

// vectorReclaimSetup builds an n-element vector filled with a cached small
// integer (so element reads add no boxed-Integer noise) plus a capture-safe
// global callback. The measured run is only the (vector-map cb v) / (vector-
// for-each cb v) call; the one-time make-vector for the result cancels in the
// allocation slope across two sizes.
func vectorReclaimSetup(n int) string {
	return fmt.Sprintf(`(begin
(define (cb x) x)
(define v (make-vector %d 7)))`, n)
}

// TestInlineHOFVectorMapReclaims is the P6 reclaim gate for vector-map.
// (vector-map <capture-safe-callback> v) must inline vector-map's single-vector
// index loop with the callback substituted so the loop self-tail-reclaims its
// env frame instead of leaking ~2 frames/element. Measured as an allocation
// slope across two vector sizes: pre-inline ~2 allocs/element; reclaimed ~0.
// An absolute slope suffices here (unlike the string tests' differential): the
// vector backing mutates in place, so vector-ref/vector-set! allocate nothing per
// element and the only per-element allocation is the env frame the inline removes.
// The stamp/mechanism itself is pinned separately by TestInlineHOFStamp.
func TestInlineHOFVectorMapReclaims(t *testing.T) {
	a1 := allocsForRun(t, vectorReclaimSetup(1000), "(vector-map cb v)")
	a2 := allocsForRun(t, vectorReclaimSetup(2000), "(vector-map cb v)")
	slope := (a2 - a1) / 1000.0
	if slope > 0.5 {
		t.Errorf("vector-map does not reclaim: %.3f allocs/element (a1=%.0f@1000, a2=%.0f@2000); "+
			"want ~0 — the inlined single-vector loop should self-tail-reclaim", slope, a1, a2)
	}
}

// TestInlineHOFVectorMapCorrect pins that inlining preserves vector-map
// semantics: the inlined index loop must build the same result vector as the real
// vector-map, in order, including the boundary inputs (empty, single) where a
// hand-transcribed loop's off-by-one would show, and the multi-vector arity that
// must NOT inline (the template is single-vector; a 3-arg call falls through to
// the real zipping clause).
func TestInlineHOFVectorMapCorrect(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	tcs := []struct {
		name string
		code string
		want string
	}{
		{"squares", `(vector-map (lambda (x) (* x x)) #(1 2 3 4 5))`, "#(1 4 9 16 25)"},
		{"empty", `(vector-map (lambda (x) (* x x)) #())`, "#()"},
		{"single", `(vector-map (lambda (x) (* x x)) #(7))`, "#(49)"},
		{"multi-vector fall-through", `(vector-map + #(1 2 3) #(10 20 30))`, "#(11 22 33)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := eng.EvalMultiple(ctx, tc.code)
			if err != nil {
				t.Fatalf("eval: %v", err)
			}
			got := result.SchemeString()
			if got != tc.want {
				t.Errorf("vector-map = %s, want %s", got, tc.want)
			}
		})
	}
}

// TestInlineHOFVectorMapHygiene is the cross-env soundness gate for the index-loop
// shape: the inlined vector-map loop calls vector-ref/vector-set!/make-vector/etc.
// as free identifiers; a call site that locally rebinds one of them MUST NOT
// capture the inlined loop's — it must use the sealed-base global. Shadows
// vector-ref with a function that would corrupt the result if it leaked in.
func TestInlineHOFVectorMapHygiene(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	const program = `(let ((vector-ref (lambda (v i) 999)))
  (vector-map (lambda (x) x) #(1 2 3)))`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != "#(1 2 3)" {
		t.Errorf("hygiene leak: inlined vector-map = %s, want #(1 2 3) "+
			"(a call-site local vector-ref must not capture the inlined loop's vector-ref)", got)
	}
}

// TestInlineHOFVectorMapCapturingCallbackReentrant is the soundness boundary for
// the result-building index-loop shape, with teeth: the callback captures a
// continuation mid-map and the test RE-ENTERS it. A passive call/cc test (capture
// k but never resume it) passes even if the gate is broken, because a wrongly-
// reclaimed frame is never used-after-release when the continuation is never
// resumed. Re-entry is the shape that exposes the bug: under the correct gate the
// call/cc callback is not capture-safe, so the dispatch falls through to the real
// (capturable) vector-map and resuming the saved continuation rebuilds the result
// (#(1 2 99)); a wrongly-inlined reclaiming loop would release the frame the
// captured continuation needs and corrupt it.
func TestInlineHOFVectorMapCapturingCallbackReentrant(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	// saved holds the continuation captured at the last element; the one-shot
	// re-entry resumes it with 99, which re-completes vector-map with 99 written
	// at the final index.
	const program = `(begin
(define saved #f)
(define done #f)
(define r (vector-map (lambda (x) (call/cc (lambda (k) (set! saved k) x))) #(1 2 3)))
(if (not done) (begin (set! done #t) (saved 99)))
r)`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != "#(1 2 99)" {
		t.Errorf("re-entrant call/cc through vector-map = %s, want #(1 2 99) "+
			"(the callback captures a continuation at the last element and is re-entered with 99; "+
			"the real capturable vector-map must rebuild the result — a wrongly-inlined reclaiming "+
			"loop would corrupt the released frame)", got)
	}
}

// TestInlineHOFVectorForEachReclaims is the P6 reclaim gate for vector-for-each
// (the side-effecting index loop): the inlined single-vector loop must
// self-tail-reclaim, slope ~0 allocs/element.
func TestInlineHOFVectorForEachReclaims(t *testing.T) {
	a1 := allocsForRun(t, vectorReclaimSetup(1000), "(vector-for-each cb v)")
	a2 := allocsForRun(t, vectorReclaimSetup(2000), "(vector-for-each cb v)")
	slope := (a2 - a1) / 1000.0
	if slope > 0.5 {
		t.Errorf("vector-for-each does not reclaim: %.3f allocs/element (a1=%.0f@1000, a2=%.0f@2000); "+
			"want ~0 — the inlined single-vector loop should self-tail-reclaim", slope, a1, a2)
	}
}

// TestInlineHOFVectorForEachCorrect pins that inlining preserves
// vector-for-each semantics: identical side effects (and order) to the real
// vector-for-each. Accumulates each visited element; the result must match a
// left-to-right traversal.
func TestInlineHOFVectorForEachCorrect(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	const program = `(begin
(define acc '())
(define (cb x) (set! acc (cons x acc)))
(vector-for-each cb #(1 2 3 4 5))
acc)`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != "(5 4 3 2 1)" {
		t.Errorf("inlined vector-for-each result = %s, want (5 4 3 2 1)", got)
	}
}
