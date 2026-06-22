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

// stringReclaimSetup builds an n-character string of a cached ASCII character
// plus a capture-safe identity callback (returns the char it receives, valid for
// string-map's char-result requirement). The measured run is only the HOF call.
func stringReclaimSetup(n int) string {
	return fmt.Sprintf(`(begin
(define (cb x) x)
(define s (make-string %d #\a)))`, n)
}

// reclaimVsLeakSlope returns (leakySlope - reclaimSlope): the per-element
// allocation reclaimed by inlining a curated HOF. reclaimCall passes the callback
// as a capture-safe symbol (cb) so the call inlines and the loop self-tail-
// reclaims; leakyCall passes the SAME callback computed as (car (list cb)) so
// CallbackIsCaptureSafe fails (not a symbol/lambda literal) and the real, leaking
// HOF runs. Both arms call cb identically per element, so any per-element accessor
// cost that does not depend on inlining — string-ref/string-set! rebuild the
// immutable backing string on every access — is identical in both and cancels;
// only the ~2 env frames/element reclaimed by inlining survive the subtraction.
// Without the template both arms leak and the difference is ~0 (a correct RED).
func reclaimVsLeakSlope(t *testing.T, setup func(int) string, reclaimCall, leakyCall string) float64 {
	t.Helper()
	r1 := allocsForRun(t, setup(1000), reclaimCall)
	r2 := allocsForRun(t, setup(2000), reclaimCall)
	l1 := allocsForRun(t, setup(1000), leakyCall)
	l2 := allocsForRun(t, setup(2000), leakyCall)
	reclaimSlope := (r2 - r1) / 1000.0
	leakySlope := (l2 - l1) / 1000.0
	return leakySlope - reclaimSlope
}

// TestInlineHOFStringMapReclaims is the P6 reclaim gate for string-map. Inlining
// string-map's single-string index loop with a capture-safe callback must
// self-tail-reclaim ~2 env frames/character relative to the leaking real HOF.
// Measured differentially because string-ref/string-set! allocate per character
// regardless of inlining (the immutable Go-string backing is rebuilt each set!),
// which an absolute slope cannot separate from the env-frame reclaim.
func TestInlineHOFStringMapReclaims(t *testing.T) {
	delta := reclaimVsLeakSlope(t, stringReclaimSetup, "(string-map cb s)", "(string-map (car (list cb)) s)")
	if delta < 1.5 {
		t.Errorf("string-map does not reclaim: inlining frees only %.3f allocs/element vs the "+
			"leaking (car (list cb)) callback; want ~2 (the self-tail loop should reclaim its env frame)", delta)
	}
}

// TestInlineHOFStringMapCorrect pins that inlining preserves string-map
// semantics: the inlined index loop must build the same result string as the
// real string-map, in order.
func TestInlineHOFStringMapCorrect(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	const program = `(string-map (lambda (c) (char-upcase c)) "hello")`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != `"HELLO"` {
		t.Errorf("inlined string-map result = %s, want \"HELLO\"", got)
	}
}

// TestInlineHOFStringMapCapturingCallbackCorrect is the soundness boundary for
// the result-building string index loop: a call/cc callback is NOT capture-safe,
// so the dispatch must fall through to the real string-map (no frame release
// across the capture while writing into the result buffer). Result intact.
func TestInlineHOFStringMapCapturingCallbackCorrect(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	const program = `(string-map (lambda (c) (call/cc (lambda (k) (char-upcase c)))) "abc")`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != `"ABC"` {
		t.Errorf("string-map with a call/cc callback = %s, want \"ABC\" "+
			"(must fall through to the real string-map, result intact)", got)
	}
}

// TestInlineHOFStringForEachReclaims is the P6 reclaim gate for string-for-each
// (the side-effecting index loop). Differential measurement as for string-map:
// the capture-safe-callback call must reclaim ~2 env frames/character relative to
// the leaking (car (list cb)) callback.
func TestInlineHOFStringForEachReclaims(t *testing.T) {
	delta := reclaimVsLeakSlope(t, stringReclaimSetup, "(string-for-each cb s)", "(string-for-each (car (list cb)) s)")
	if delta < 1.5 {
		t.Errorf("string-for-each does not reclaim: inlining frees only %.3f allocs/element vs the "+
			"leaking (car (list cb)) callback; want ~2 (the self-tail loop should reclaim its env frame)", delta)
	}
}

// TestInlineHOFStringForEachCorrect pins that inlining preserves
// string-for-each semantics: identical side effects (and order) to the real
// string-for-each. Accumulates each visited character; the result must match a
// left-to-right traversal.
func TestInlineHOFStringForEachCorrect(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	const program = `(begin
(define acc '())
(define (cb c) (set! acc (cons c acc)))
(string-for-each cb "abc")
acc)`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != `(#\c #\b #\a)` {
		t.Errorf("inlined string-for-each result = %s, want (#\\c #\\b #\\a)", got)
	}
}
