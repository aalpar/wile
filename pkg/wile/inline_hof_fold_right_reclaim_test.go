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
	"github.com/aalpar/wile/pkg/values"
)

// foldRightReclaimSetup imports srfi/1 (fold-right is import-gated, so the import
// is what stamps its InlineHOF capability), builds a list of n cached small
// integers, and a capture-safe two-argument kons that returns the accumulator
// unchanged (no per-element allocation). The template's one (reverse ls) is a
// single block allocation independent of n, so the env-frame reclaim shows in an
// absolute allocation slope. The measured run is only the (fold-right cb 0 lst)
// call.
func foldRightReclaimSetup(n int) string {
	return fmt.Sprintf(`(begin
(import (srfi 1))
(define (cb x acc) acc)
(define (build i acc) (if (= i 0) acc (build (- i 1) (cons i acc))))
(define lst (build %d '())))`, n)
}

// TestInlineHOFFoldRightReclaims is the reclaim gate for fold-right. fold-right's
// real single-list clause recurses in non-tail position
// ((kons (car ls) (lp (cdr ls)))), leaking ~2 frames/element. The inline template
// is a TAIL rewrite (reverse once, then fold left), so the loop self-tail-reclaims
// its env frame. The single (reverse ls) is one block allocation independent of n,
// so an absolute allocation slope across two list sizes isolates the reclaim: ~0
// when reclaimed, ~2 when the non-tail real fold-right runs (the RED).
func TestInlineHOFFoldRightReclaims(t *testing.T) {
	a1 := allocsForRun(t, foldRightReclaimSetup(1000), "(fold-right cb 0 lst)")
	a2 := allocsForRun(t, foldRightReclaimSetup(2000), "(fold-right cb 0 lst)")
	slope := (a2 - a1) / 1000.0
	if slope > 0.5 {
		t.Errorf("fold-right does not reclaim: %.3f allocs/element (a1=%.0f@1000, a2=%.0f@2000); "+
			"want ~0 — the inlined tail-rewrite loop should self-tail-reclaim", slope, a1, a2)
	}
}

// TestInlineHOFFoldRightCorrect pins that inlining preserves fold-right semantics
// for a list-building (cons) and a non-accumulating-shape (sum) kons, across the
// boundary inputs (empty, single) and the multi-list arity that must NOT inline
// (the template is single-list; a 4-arg call falls through to the real zipping
// clause). Both cons and + are capture-safe primitives, so the single-list calls
// take the inline path; the results must match the real fold-right.
func TestInlineHOFFoldRightCorrect(t *testing.T) {
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
		{"cons", `(begin (import (srfi 1)) (fold-right cons '() '(1 2 3)))`, "(1 2 3)"},
		{"sum", `(begin (import (srfi 1)) (fold-right + 0 '(1 2 3 4 5)))`, "15"},
		{"empty", `(begin (import (srfi 1)) (fold-right cons '() '()))`, "()"},
		{"single", `(begin (import (srfi 1)) (fold-right cons '() '(7)))`, "(7)"},
		{"multi-list fall-through", `(begin (import (srfi 1)) (fold-right + 0 '(1 2 3) '(10 20 30)))`, "66"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := eng.EvalMultiple(ctx, tc.code)
			if err != nil {
				t.Fatalf("eval: %v", err)
			}
			got := result.SchemeString()
			if got != tc.want {
				t.Errorf("inlined fold-right = %s, want %s", got, tc.want)
			}
		})
	}
}

// TestInlineHOFFoldRightOrder pins that the tail rewrite preserves fold-right's
// right-to-left kons application order. The real fold-right applies kons
// innermost-first (the last element first); the reverse-then-fold-left rewrite
// visits the reversed list, so kons still sees elements last-to-first.
func TestInlineHOFFoldRightOrder(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	const program = `(begin
(import (srfi 1))
(define seen '())
(define (kk x acc) (set! seen (cons x seen)) (cons x acc))
(define r (fold-right kk '() '(1 2 3 4)))
(list r (reverse seen)))`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != "((1 2 3 4) (4 3 2 1))" {
		t.Errorf("inlined fold-right application order = %s, want ((1 2 3 4) (4 3 2 1)) "+
			"(result is (1 2 3 4); kons must fire right-to-left, so seen is (4 3 2 1))", got)
	}
}

// TestInlineHOFFoldRightHygiene is the cross-env soundness gate for the tail-
// rewrite shape: the inlined fold-right loop calls reverse/pair?/car/cdr/cons as
// free identifiers; a call site that locally rebinds one MUST NOT capture the
// inlined loop's — it must use the sealed-base global. Shadows car with a function
// that would corrupt the result if it leaked in.
func TestInlineHOFFoldRightHygiene(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	const program = `(begin
(import (srfi 1))
(let ((car (lambda (p) 999)))
  (fold-right cons '() '(1 2 3))))`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != "(1 2 3)" {
		t.Errorf("hygiene leak: inlined fold-right = %s, want (1 2 3) "+
			"(a call-site local car must not capture the inlined fold-right loop's car)", got)
	}
}

// TestInlineHOFFoldRightImportedIsStable pins the rebind-stability foundation that
// makes inlining fold-right's body sound: like fold, fold-right lives in the
// potentially mutable (srfi 1) library, not the always-immutable sealed base.
// (import (srfi 1)) marks it Imported, and IsStable() ORs in Imported (R7RS
// forbids set! on imports), so tryInlineHOFCall only ever inlines a binding the
// arc's IsStable() contract guarantees will not be rebound. If a future change
// stopped marking imported fold-right stable, this fails before an unsound inline
// could ship.
func TestInlineHOFFoldRightImportedIsStable(t *testing.T) {
	cases := []struct {
		name string
		opt  EngineOption
	}{
		{"immutable-default", WithImmutableTopLevel()},
		{"mutable-top-level", WithMutableTopLevel()},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			ctx := context.Background()
			eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
				WithLibraryPaths(), tc.opt)
			if err != nil {
				t.Fatal(err)
			}
			_, err = eng.EvalMultiple(ctx, "(import (srfi 1))")
			if err != nil {
				t.Fatalf("import: %v", err)
			}
			b := eng.Environment().GetBinding(values.NewSymbol("fold-right"), values.AllScopes())
			if b == nil {
				t.Fatal("fold-right unbound after (import (srfi 1))")
			}
			if b.InlineHOFParam() != 0 {
				t.Errorf("imported fold-right InlineHOFParam = %d, want 0 (stamped curated HOF)", b.InlineHOFParam())
			}
			if !b.IsStable() {
				t.Error("imported fold-right IsStable = false, want true — inlining its body is sound only " +
					"because the binding cannot be rebound (Imported)")
			}
		})
	}
}

// TestInlineHOFFoldRightCapturingCallbackReentrant is the soundness boundary for
// fold-right, with teeth: the kons captures a continuation mid-fold and the test
// RE-ENTERS it. A passive call/cc test passes even if the gate is broken (a
// wrongly-reclaimed frame is never used-after-release when the continuation is
// never resumed); re-entry exposes the bug. The call/cc kons is not capture-safe,
// so the dispatch falls through to the real (capturable, non-tail) fold-right:
// (fold-right kons 0 '(1 2 3)) is 6, and resuming the continuation captured at the
// outermost (first-element) kons with 99 makes the final result 99. A wrongly-
// inlined reclaiming loop would corrupt the released frame the continuation needs.
func TestInlineHOFFoldRightCapturingCallbackReentrant(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithSourceFS(stdlib.FS),
		WithLibraryPaths(), WithImmutableTopLevel())
	if err != nil {
		t.Fatal(err)
	}
	const program = `(begin
(import (srfi 1))
(define saved #f)
(define done #f)
(define r (fold-right (lambda (x acc) (call/cc (lambda (k) (set! saved k) (+ x acc)))) 0 '(1 2 3)))
(if (not done) (begin (set! done #t) (saved 99)))
r)`
	result, err := eng.EvalMultiple(ctx, program)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	got := result.SchemeString()
	if got != "99" {
		t.Errorf("re-entrant call/cc through fold-right = %s, want 99 "+
			"(the kons captures a continuation and is re-entered with 99; the real capturable "+
			"fold-right must support the re-entry — a wrongly-inlined reclaiming loop would corrupt "+
			"the released frame)", got)
	}
}
