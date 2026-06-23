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

// forEachReclaimSetup builds a list of n integers (all within the cached-Integer
// window so the list itself adds no per-element boxed-Integer noise) and a
// capture-safe global callback. allocsForRun measures only the (for-each cb lst)
// run, so the recursive list builder's own cost is irrelevant.
func forEachReclaimSetup(n int) string {
	return fmt.Sprintf(`(begin
(define (cb x) x)
(define (build i acc) (if (= i 0) acc (build (- i 1) (cons i acc))))
(define lst (build %d '())))`, n)
}

// TestInlineHOFForEachReclaims is the P3 reclaim gate (callback specialization
// Strategy A). (for-each <capture-safe-callback> lst) must inline for-each's
// single-list loop with the callback substituted so the loop self-tail-reclaims
// its env frame instead of leaking ~2 frames/element (the P0 baseline of 40,026
// allocs/op over 20000 elements). Measured as an allocation slope across two list
// sizes: pre-inline the slope is ~2 allocs/element; reclaimed it is ~0.
func TestInlineHOFForEachReclaims(t *testing.T) {
	a1 := allocsForRun(t, forEachReclaimSetup(1000), "(for-each cb lst)")
	a2 := allocsForRun(t, forEachReclaimSetup(2000), "(for-each cb lst)")
	slope := (a2 - a1) / 1000.0
	if slope > 0.5 {
		t.Errorf("for-each does not reclaim: %.3f allocs/element (a1=%.0f@1000, a2=%.0f@2000); "+
			"want ~0 — the inlined single-list loop should self-tail-reclaim", slope, a1, a2)
	}
}

// TestInlineHOFForEachCorrect pins that inlining preserves for-each semantics:
// the inlined loop must produce identical side effects (and order) to the real
// for-each. Accumulates each visited element into a list; the result must match a
// plain left-to-right traversal.
func TestInlineHOFForEachCorrect(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	const program = `(begin
(define acc '())
(define (cb x) (set! acc (cons x acc)))
(for-each cb '(1 2 3 4 5))
acc)`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != "(5 4 3 2 1)" {
		t.Errorf("inlined for-each result = %s, want (5 4 3 2 1)", got)
	}
}

// TestInlineHOFForEachHygiene is the cross-env soundness gate. The inlined
// for-each loop calls car/cdr/null? internally; a call site that locally rebinds
// car MUST NOT capture the loop's car — the inlined loop must use the GLOBAL car.
// If hygiene leaks, the loop would walk the list with the user's car and corrupt
// the traversal (or error).
func TestInlineHOFForEachHygiene(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	// Shadow car locally with a function that would corrupt traversal if it
	// captured the inlined loop's car. The global car must still drive for-each.
	const program = `(begin
(define acc '())
(define (cb x) (set! acc (cons x acc)))
(let ((car (lambda (p) 'WRONG)))
  (for-each cb '(1 2 3)))
acc)`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != "(3 2 1)" {
		t.Errorf("hygiene leak: inlined for-each result = %s, want (3 2 1) "+
			"(a call-site local car must not capture the inlined loop's car)", got)
	}
}

// TestInlineHOFForEachLambdaReclaims pins the unify path: a capture-safe LAMBDA
// callback (not just a symbol) also reclaims. The lambda body (set! on a global +
// no capture operator) is proven capture-safe, so the callback is synthetic-let-
// bound and stamped, and the inlined loop self-tail-reclaims just as for a symbol.
func TestInlineHOFForEachLambdaReclaims(t *testing.T) {
	setup := func(n int) string {
		return fmt.Sprintf(`(begin
(define (build i acc) (if (= i 0) acc (build (- i 1) (cons i acc))))
(define lst (build %d '())))`, n)
	}
	// A pure capture-safe lambda literal (no top-level mutation, which the
	// immutable default forbids; no per-element allocation of its own).
	a1 := allocsForRun(t, setup(1000), "(for-each (lambda (x) x) lst)")
	a2 := allocsForRun(t, setup(2000), "(for-each (lambda (x) x) lst)")
	slope := (a2 - a1) / 1000.0
	if slope > 0.5 {
		t.Errorf("for-each with a capture-safe lambda callback does not reclaim: "+
			"%.3f allocs/element (a1=%.0f, a2=%.0f) — the unify path should inline it", slope, a1, a2)
	}
}

// TestInlineHOFForEachCapturingCallbackCorrect is the soundness boundary: a
// callback that runs call/cc is NOT capture-safe, so the dispatch must refuse to
// inline (no frame release across the capture) and fall through to the real
// for-each. The traversal must remain correct — a wrongly-inlined reclaiming loop
// would risk corrupting the captured continuation rather than merely changing
// allocations.
func TestInlineHOFForEachCapturingCallbackCorrect(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	const program = `(begin
(define acc '())
(for-each (lambda (x) (call/cc (lambda (k) (set! acc (cons x acc))))) '(1 2 3 4))
acc)`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != "(4 3 2 1)" {
		t.Errorf("for-each with a call/cc callback = %s, want (4 3 2 1) "+
			"(must fall through to the real for-each, traversal intact)", got)
	}
}
