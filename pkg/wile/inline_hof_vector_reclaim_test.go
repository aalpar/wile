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
// semantics: the inlined index loop must build the same result vector as the
// real vector-map, in order.
func TestInlineHOFVectorMapCorrect(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	const program = `(vector-map (lambda (x) (* x x)) #(1 2 3 4 5))`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != "#(1 4 9 16 25)" {
		t.Errorf("inlined vector-map result = %s, want #(1 4 9 16 25)", got)
	}
}

// TestInlineHOFVectorMapCapturingCallbackCorrect is the soundness boundary for
// the result-building index-loop shape: a call/cc callback is NOT capture-safe,
// so the dispatch must refuse to inline (no frame release across the capture
// while writing into the result buffer) and fall through to the real
// vector-map. The result must remain correct.
func TestInlineHOFVectorMapCapturingCallbackCorrect(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	const program = `(vector-map (lambda (x) (call/cc (lambda (k) (* x x)))) #(1 2 3 4))`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != "#(1 4 9 16)" {
		t.Errorf("vector-map with a call/cc callback = %s, want #(1 4 9 16) "+
			"(must fall through to the real vector-map, result intact)", got)
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
