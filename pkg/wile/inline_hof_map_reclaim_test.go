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

// mapReclaimSetup builds a list of n cached small integers (no per-element boxed-
// Integer noise) and a capture-safe identity callback. The measured run is only
// the (map cb lst) call.
func mapReclaimSetup(n int) string {
	return fmt.Sprintf(`(begin
(define (cb x) x)
(define (build i acc) (if (= i 0) acc (build (- i 1) (cons i acc))))
(define lst (build %d '())))`, n)
}

// TestInlineHOFMapReclaims is the reclaim gate for map, the dominant uncovered
// HOF. map's real single-list clause conses in non-tail position
// ((cons (f (car lst)) (loop (cdr lst)))); the inline template is a TAIL rewrite
// (accumulate + reverse) that self-tail-reclaims its env frame instead of leaking
// ~2 frames/element. map returns an n-element list, so it conses ~1 result
// pair/element regardless of inlining — an absolute slope cannot isolate the
// env-frame reclaim from the inherent result cost. Measured differentially
// (reclaimVsLeakSlope): the inline arm minus a forced-leaky (car (list cb)) arm
// cancels the result-cons cost, leaving the ~2 env frames/element reclaimed.
func TestInlineHOFMapReclaims(t *testing.T) {
	delta := reclaimVsLeakSlope(t, mapReclaimSetup, "(map cb lst)", "(map (car (list cb)) lst)")
	if delta < 1.5 {
		t.Errorf("map does not reclaim: inlining frees only %.3f allocs/element vs the leaking "+
			"(car (list cb)) callback; want ~2 (the tail-rewrite loop should reclaim its env frame)", delta)
	}
}

// TestInlineHOFMapCorrect pins that inlining preserves map semantics: the inlined
// tail-rewrite loop must build the same result list as the real map, in order,
// including the boundary inputs (empty, single) and the multi-list arity that must
// NOT inline (the template is single-list; a 3-arg call falls through to the real
// zipping clause).
func TestInlineHOFMapCorrect(t *testing.T) {
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
		{"square", `(map (lambda (x) (* x x)) '(1 2 3 4))`, "(1 4 9 16)"},
		{"empty", `(map (lambda (x) (* x x)) '())`, "()"},
		{"single", `(map (lambda (x) (* x x)) '(5))`, "(25)"},
		{"multi-list fall-through", `(map + '(1 2 3) '(10 20 30))`, "(11 22 33)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := eng.EvalMultiple(ctx, tc.code)
			if err != nil {
				t.Fatalf("eval: %v", err)
			}
			got := result.SchemeString()
			if got != tc.want {
				t.Errorf("inlined map = %s, want %s", got, tc.want)
			}
		})
	}
}

// TestInlineHOFMapOrder pins that the tail rewrite preserves map's left-to-right
// application order — Wile requires L→R (stricter than R7RS). The accumulate +
// reverse rewrite applies f front-to-back and reverses once, so f sees elements
// in list order even though the spine is built back-to-front.
func TestInlineHOFMapOrder(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	const program = `(begin
(define seen '())
(define (cb x) (set! seen (cons x seen)) x)
(define r (map cb '(1 2 3 4)))
(list r (reverse seen)))`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != "((1 2 3 4) (1 2 3 4))" {
		t.Errorf("inlined map application order = %s, want ((1 2 3 4) (1 2 3 4)) "+
			"(result and L→R application order must both be (1 2 3 4))", got)
	}
}

// TestInlineHOFMapHygiene is the cross-env soundness gate for the tail-rewrite
// shape: the inlined map loop calls car/cdr/null?/cons/reverse as free
// identifiers; a call site that locally rebinds one MUST NOT capture the inlined
// loop's — it must use the sealed-base global. Shadows car with a function that
// would corrupt the result if it leaked in.
func TestInlineHOFMapHygiene(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	const program = `(begin
(let ((car (lambda (p) 999)))
  (map (lambda (x) x) '(1 2 3))))`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != "(1 2 3)" {
		t.Errorf("hygiene leak: inlined map = %s, want (1 2 3) "+
			"(a call-site local car must not capture the inlined map loop's car)", got)
	}
}

// TestInlineHOFMapLambdaReclaims pins the unify path: a capture-safe LAMBDA
// callback (not just a symbol) also inlines and reclaims. Measured differentially
// because map's result list is allocated either way.
func TestInlineHOFMapLambdaReclaims(t *testing.T) {
	setup := func(n int) string {
		return fmt.Sprintf(`(begin
(define (build i acc) (if (= i 0) acc (build (- i 1) (cons i acc))))
(define lst (build %d '())))`, n)
	}
	delta := reclaimVsLeakSlope(t, setup, "(map (lambda (x) x) lst)", "(map (car (list (lambda (x) x))) lst)")
	if delta < 1.5 {
		t.Errorf("map with a capture-safe lambda callback does not reclaim: inlining frees only "+
			"%.3f allocs/element — the unify path should inline it", delta)
	}
}

// TestInlineHOFMapCapturingCallbackReentrant is the soundness boundary for map,
// with teeth: the callback captures a continuation mid-map and the test RE-ENTERS
// it. A passive call/cc test passes even if the gate is broken (a wrongly-
// reclaimed frame is never used-after-release when the continuation is never
// resumed); re-entry exposes the bug. The call/cc callback is not capture-safe, so
// the dispatch falls through to the real (capturable, non-tail) map. The first
// pass returns (1 2 3); resuming the continuation captured at the first element
// with 99 re-runs the map tail from that point, yielding (99 2 3).
func TestInlineHOFMapCapturingCallbackReentrant(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	const program = `(begin
(define saved #f)
(define done #f)
(define r (map (lambda (x) (call/cc (lambda (k) (if (= x 1) (set! saved k)) x))) '(1 2 3)))
(if (not done) (begin (set! done #t) (saved 99)))
r)`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != "(99 2 3)" {
		t.Errorf("re-entrant call/cc through map = %s, want (99 2 3) "+
			"(the callback captures a continuation at the first element and is re-entered with 99; "+
			"the real capturable map must support the re-entry — a wrongly-inlined reclaiming loop "+
			"would corrupt the released frame)", got)
	}
}
